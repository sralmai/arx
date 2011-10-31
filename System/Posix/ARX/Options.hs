{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

module System.Posix.ARX.Options where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Char
import Data.String
import Data.Word

import qualified Data.Attoparsec
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances hiding (pSym)
import qualified Text.ParserCombinators.UU.BasicInstances

import System.Posix.ARX.CLTokens (Class(..))
import qualified System.Posix.ARX.CLTokens as CLTokens
import qualified System.Posix.ARX.Sh as Sh
import System.Posix.ARX.Tar


{-| Parses an array of 'ByteString', representing arguments to a commmand.
 -}
type ArgsParser t            =  Gram (P (Str ByteString [ByteString] ArgN)) t

newtype ArgN                 =  ArgN Word
deriving instance Eq ArgN
deriving instance Ord ArgN
instance Show ArgN where
  show (ArgN w)              =  "arg:" ++ show w
deriving instance Num ArgN
instance IsLocationUpdatedBy ArgN ByteString where
  advance n _                =  n + 1


{-| Options for an SHDAT subprogram.
 -}
data SHDAT                   =  SHDAT Word       -- ^ Block size.
                                      IOStream   -- ^ Output stream.
                                      [IOStream] -- ^ Input streams.
deriving instance Eq SHDAT
deriving instance Ord SHDAT
deriving instance Show SHDAT

shdat                       ::  ArgsParser SHDAT
shdat                        =  do
  pSym "shdat"
  shdatResolve <$> pMaybe dashB <||> pMaybe dashO <||> pMany ioStream
  --shdatResolve <$> pMany dashB <||> pMany dashO <||> pMany ioStream
  -- Multiple dashB and dashO leads parser to sometimes crash with "ambiguous
  -- parser?".

{-| Apply defaulting rules to resolve parsed options to a full set of options.
    The default block size is 4MiB and the default IO resources are the
    @STDIO@ streams.
 -}
shdatResolve size output inputs = SHDAT (maybe defaultSize id size)
                                        (maybe STDIO id output) $
                                         if inputs == [] then [STDIO]
                                                         else inputs
 where
  defaultSize                =  0x400000 -- 4MiB


data TMPX                    =  TMPX Word       -- ^ Block size.
                                     SomeBytes  -- ^ Code of task to run.
                                     IOStream   -- ^ Output stream.
                                     [(Tar, IOStream)] -- ^ Input streams.
                                     Bool -- ^ Destroy tmp on success?
                                     Bool -- ^ Destroy tmp on failure?
                                     [(Sh.Var, Sh.Val)] -- ^ Env mapping.
deriving instance Eq TMPX
deriving instance Ord TMPX
deriving instance Show TMPX

tmpx                        ::  ArgsParser SHDAT
tmpx                         =  do
  pSym "tmpx"
  () <$> pMaybe dashB <||> pMaybe dashO <||> pMany ioStream
 where
  flagsAndArgs = pMaybe dashB <||> pMaybe dashO <||> pMaybe dashR

{-| An 'IOStream' is a source or sink for bytes, allowing one to start at the
    beginning and read or write to the end.
 -}
data IOStream                =  STDIO | Path ByteString
deriving instance Eq IOStream
deriving instance Ord IOStream
deriving instance Show IOStream

{-| A 'SomeBytes' provides a way to read in some bytes but not necessarily any
    way to write or stream them.
 -}
data SomeBytes               =  IOStream | Literal ByteString
deriving instance Eq SomeBytes
deriving instance Ord SomeBytes
deriving instance Show SomeBytes


dashB                       ::  ArgsParser Word
dashB                        =  do
  pSym "-b"
  attoOver CLTokens.sizeBounded $ tok Size

dashO                       ::  ArgsParser IOStream
dashO                        =  pSym "-o" *> ioStream

dashR                       ::  ArgsParser IOStream
dashR                        =  pSym "-r" *> ioStream

ioStream                    ::  ArgsParser IOStream
ioStream = Path <$> tok QualifiedPath <|> STDIO <$ tok Dash

pSym = mkGram . Text.ParserCombinators.UU.BasicInstances.pSym

tok                         ::  CLTokens.Class -> ArgsParser ByteString
tok cls = mkGram $ pSatisfy (CLTokens.match cls) (Insertion msg exemplar 5)
 where
  exemplar                   =  CLTokens.exemplar cls
  msg                        =  Char8.unpack exemplar

attoOver atto uu             =  do
  res                       <-  Data.Attoparsec.parseOnly atto <$> uu
  case res of Left _        ->  pFail
              Right x       ->  pReturn x

