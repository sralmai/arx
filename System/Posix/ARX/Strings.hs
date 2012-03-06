{-# LANGUAGE OverloadedStrings
           , GeneralizedNewtypeDeriving #-}
module System.Posix.ARX.Strings where

import Control.Monad
import Data.ByteString.Char8
import Data.Maybe
import Data.Monoid
import Prelude hiding (elem, takeWhile)
import Data.String


-- | This type represents the value and not the layout of a C string: a
--   sequence of bytes not containing null.
newtype CString = CString ByteString deriving (Eq, Ord, Show, Monoid)
instance IsString CString  where fromString = fromString'
instance Bytes CString     where bytes (CString s) = s
instance Norm CString      where norm = CString . takeWhile (/= '\0')

-- | A C string containing an @=@, separating the variable and the value.
newtype Env = Env ByteString deriving (Eq, Ord, Show)
instance IsString Env      where fromString = fromString'
instance Bytes Env         where bytes (Env b) = b
instance Norm Env          where norm b | elem '=' c = Env c
                                        | otherwise  = Env (snoc c '=')
                                  where CString c = norm b

-- | A filename is a C string not containing @\/@.
newtype Filename = Filename ByteString deriving (Eq, Ord, Show, Monoid)
instance IsString Filename where fromString = fromString'
instance Bytes Filename    where bytes (Filename s) = s
instance Norm Filename     where norm = Filename . takeWhile condition
                                  where condition = not . (`elem` "/\0")

class Norm t              where norm :: ByteString -> t

class Bytes t             where bytes :: t -> ByteString

maybeNorm                   ::  (Bytes t, Norm t) => ByteString -> Maybe t
maybeNorm b                  =  guard (bytes normed == b) >> Just normed
 where normed                =  norm b

fromString'                 ::  (Bytes t, Norm t) => String -> t
fromString'                  =  fromJust . maybeNorm . fromString

