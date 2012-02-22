{-| Strips a shell script for its function definitions, making them available
    individually, and recognizes templated variables on a line by themselves.

    Comments and top-level statements -- including variable definitions
    outside of functions -- are removed.

    Turns a shell script in to a record with named variables drawn from the
    script (any variable assigned in the script can be assigned in the ADT).

 -}

module System.Posix.ARX.Shlib where

import Control.Applicative
import Data.ByteString
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.Attoparsec.ByteString.Char8 (Parser)


name                        ::  Parser ByteString
name                         =  do
  Atto.takeWhile Atto.isSpace
  mappend <$> Atto.takeWhile1 leading <*> Atto.takeWhile body
 where
  leading                    =  Atto.inClass "a-zA-Z_"
  body                       =  Atto.inClass "a-zA-Z_0-9"



-- A byte region can be a...
data Shell = Quotation | Assignment | FunctionDeclaration | Comment
-- Rules are: an assignment can be found in a declaration. A quotation can be
-- found in a either.


function = do
  name
  string "() {\n"
  ...
  string "}"


