{-# LANGUAGE QuasiQuotes
  #-}

module System.Posix.ARX.ShLib where

import Data.ByteString (ByteString)

import System.Posix.ARX.QQ


-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as Bytes
-- import Data.List
-- import Data.Maybe
-- import Data.Monoid

-- import qualified Blaze.ByteString.Builder as Blaze
-- import Data.FileEmbed

-- import System.Posix.ARX.BlazeIsString


--   a : b : c : d : e : [] = findChunks $(embedFile "./model-scripts/tmpx.sh")


tmpx_create                 ::  ByteString
tmpx_create                  =  [bQQ|
tmpx_create() {
  : ${tmp:=/tmp}  # Templated.
  tag="`tag`"
  dir="$tmp"/"$tag"
  [ ! -e "$dir" ] || { echo 'ERR: tmp dir collision!' 1>&2 ; return 2 ;}
  mkdir -p "$dir"
  cd "$dir"
}
|]

