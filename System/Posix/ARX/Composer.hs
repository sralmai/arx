{-# LANGUAGE OverloadedStrings
           , PostfixOperators
           , TupleSections #-}

module System.Posix.ARX.Composer where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString, unwords)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String

import System.Posix.ARX.Strings
import qualified System.Posix.ARX.Sh as Sh


-- | Execution contexts for wrappers. For common wrappers, like @flock@ or
--   @screen@, there's no real context needed; this is the @external@ context;
--   but for shell functions that are used as wrappers we need a way to inline
--   their code, escaped in a such a way that the calling shell (always @sh@
--   in ARX) simply passes it on. Most shell wrappers will call @exec@ to run
--   the subordinate commands, so as not to give rise to massive process
--   hierarchies.
data ExecutionContext
  = InlineSh   Sh.VarVal -- ^ Sh code.
  | InlineBash Sh.VarVal -- ^ Bash code.
  | Plain
 deriving (Eq, Ord, Show)

-- | Render an abstract command into an array of values suitable for use as a
--   shell command line.
class Compile t where compile :: t -> [ByteString]

instance Compile (ExecutionContext, [Sh.VarVal]) where
  compile (ctx, texts) = Sh.sh <$> case ctx of
    Plain           -> texts
    InlineSh   defs -> "sh"  :"-c":(defs +@):text:texts
    InlineBash defs -> "bash":"-c":(defs +@):text:texts
   where
    (+@) (Sh.VarVal vs) = Sh.VarVal (vs ++ [(Right . Sh.Val) "\n\"$@\""])
    text | text:_ <- texts = text -- Try to set $0 to something useful.
         | otherwise       = "_"



data Stacked = Simple (ExecutionContext, [Sh.VarVal])
             | Conditional ByteString (ExecutionContext, [Sh.VarVal])
 deriving (Eq, Ord, Show)

instance Compile Stacked where
  compile (Simple tuple) = "set" : "--" : (compile tuple ++ ["\"$@\""])
  compile (Conditional expr tuple) = "!" : expr : "||" : compile (Simple tuple)


{-
  set -- sh -c "$work" sh "$@"
  set -- env ... "$@"
  ! $downloads || set -- sh -c '...' downloads ... "$@"
  ! $dm_tmpx   || set -- sh -c '...' sh trap_on "$dir" "$@"
  ! $dm        || set -- sh -c '...' screend ... "$@"
  ! $dm_tmpx   || set -- trap_off "$@"
  ! $sources   || set -- sources "$@"
  ! $archives  || set -- archives "$@"
  ! $tmpx      || set -- tmp ... "$@"
  ## ! $dirs      || set -- mkdir_ ... "$@"
  "$@"
  -}

