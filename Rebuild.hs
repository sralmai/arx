{-# LANGUAGE BangPatterns
  #-}
module Rebuild where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.ST
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as Bytes
import qualified Data.List as List
import Data.Monoid
import Data.Ord
import Data.STRef
import Data.String
import Data.Word

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector (create, length)
import qualified Data.Vector.Unboxed.Mutable as Vector hiding (length)
import qualified Data.Vector.Algorithms.Intro as Vector


main                         =  do
  bytes                     <-  Bytes.getContents
--let out                    =  simple bytes
--print (Bytes.length out)
  let out                    =  rebuildAsVector bytes
  print (Vector.length out)


simple                      ::  ByteString -> ByteString
simple                       =  Bytes.map (+1)


rebuildAsVector             ::  ByteString -> Vector Word8
rebuildAsVector bytes        =  byteVector
 where
  len                        =  fromIntegral (Bytes.length bytes * 2)
  byteVector                ::  Vector Word8
  byteVector                 =  Vector.create $ do
    counter                 <-  newSTRef 0
    v                       <-  Vector.new len
    let write                =  writeOneByte v counter
    mapM_ write (Bytes.unpack bytes)
    n                       <-  readSTRef counter
    return (Vector.unsafeSlice 0 n v)
  writeOneByte v counter b   =  do n <- readSTRef counter
                                   Vector.unsafeWrite v n b
                                   modifySTRef counter (+!1)
  (+!)                       =  (+)
  --(+!) a b                   =  ((+) $! a) $! b

