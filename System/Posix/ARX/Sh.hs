{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
           , GeneralizedNewtypeDeriving #-}
-- | Very basic Bourne shell generation.
module System.Posix.ARX.Sh where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import Data.String
import Data.Maybe
import Data.Monoid

import qualified Text.ShellEscape as Esc

import System.Posix.ARX.Strings


setEU                       ::  ByteString
setEU                        =  "set -e -u\n"

-- | Valid shell string values contain any byte but null.
newtype Val                  =  Val CString
 deriving (Eq, Ord, Show, Monoid)
instance Norm Val where
  norm                       =  Val . norm
instance IsString Val where
  fromString                 =  Val . fromString
instance Bytes Val where
  bytes (Val c)              =  bytes c
instance Sh Val where
  sh                         =  Esc.bytes . Esc.sh . bytes


-- | Valid shell variable names consist of a leading letter or underscore and
--   then any number of letters, underscores or digits.
newtype Var                  =  Var ByteString
 deriving (Eq, Ord, Show)
instance Sh Var where
  sh (Var bytes)             =  bytes

var                         ::  ByteString -> Maybe Var
var ""                       =  Nothing
var bytes = guard (leading h && Bytes.all body t) >> Just (Var bytes)
 where
  (h, t)                     =  (Bytes.head bytes, Bytes.tail bytes)
  body c                     =  leading c || (c >= '0' && c <= '9')
  leading c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_'


-- | Shell strings with variable substitution.
newtype VarVal               =  VarVal [Either Var Val]
 deriving (Eq, Ord, Show)
instance Sh VarVal where
  sh (VarVal l)              =  (mconcat . map sh) l
instance IsString VarVal where
  fromString                 =  VarVal . (:[]) . Right . fromString


instance Sh (Either Var Val) where
  sh (Left var)              =  mconcat ["\"$", sh var, "\""]
  sh (Right val)             =  sh val

instance Sh [Val] where
  sh                         =  mconcat . map (mappend " " . sh)

class Sh t where sh         ::  t -> ByteString

