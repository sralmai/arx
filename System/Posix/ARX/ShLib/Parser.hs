{-# LANGUAGE OverloadedStrings
  #-}
module System.Posix.ARX.ShLib.Parser where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8

import Text.Regex.TDFA ( Regex, RegexContext(matchM),
                         makeRegexOpts, defaultExecOpt,
                         blankCompOpt,  lastStarGreedy )


{-| Kinds of lines, from the point of view of the template parser.
 -}
data Line                    =  FunctionOpen ByteString
                             |  FunctionClose
                             |  DefaultingVar ByteString
                             |  Body
deriving instance Eq Line
deriving instance Show Line

data Function                =  Function ByteString [ByteString]
deriving instance Eq Function
deriving instance Ord Function
deriving instance Show Function


functions                   ::  ByteString -> [Function]
functions                    =  foldr undefined undefined . scan

scan                        ::  ByteString -> [(Line, ByteString)]
scan s                       =  classify' <$> Data.ByteString.Char8.lines s
 where
  classify' s                =  (classify s, s)

{-| Classify line as a template line or as a plain, code line. The classifier
    marks function declarations and defaulting variables in a very simple
    manner: the function open and close must be on a line by themselves, as
    must all defaulting variable declarations that the classifier will
    recognize.
 -}
classify                    ::  ByteString -> Line
classify s =
  maybe Body id (defaultingVar s <|> functionOpen s <|> functionClose s)


defaultingVar s              =  do
  (_, _, _, [name])         <-  pattern =~ s
  Just (DefaultingVar name)
 where
  pattern = re "^ *: +[$][{]([_a-z0-9][_a-zA-Z0-9]*):=[^}]*[}]"

functionOpen s               =  do
  (_, _, _, [name])         <-  pattern =~ s
  Just (FunctionOpen name)
 where
  pattern                    =  re "^([_a-z0-9][_a-zA-Z0-9]*)[(][)] +[{]"

functionClose s              =  (pattern =~ s) >> Just FunctionClose
 where
  pattern                    =  re "^[}]"


-- Regex stuff.

(=~) :: Regex -> ByteString
     -> Maybe (ByteString, ByteString, ByteString, [ByteString])
(=~)                         =  matchM

re                          ::  ByteString -> Regex
re                           =  makeRegexOpts fullPosix defaultExecOpt
 where
  fullPosix                  =  blankCompOpt { lastStarGreedy=True }

