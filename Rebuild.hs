
module Rebuild where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.ST
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Internal as Bytes (c2w)
import qualified Data.List as List
import Data.Monoid
import Data.Ord
import Data.STRef
import Data.String
import Data.Word

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Vector
import qualified Data.Vector.Algorithms.Intro as Vector


rebuildAsVector bytes        =  byteVector
 where
  byteVector                ::  Vector Word8
  byteVector                 =  Vector.create $ do
    counter                 <-  newSTRef 0
    v                       <-  Vector.new (Bytes.length bytes * 2)
    let write                =  writeOneByte v counter
    for each b in bytes
      write b
    return (Vector.unsafeSlice 0 count v)
  writeOneByte v counter b   =  do Vector.unsafeWrite v counter b
                                   modifySTRef counter (+1)


