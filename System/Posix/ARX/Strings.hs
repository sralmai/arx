{-# LANGUAGE OverloadedStrings
           , GeneralizedNewtypeDeriving #-}
module System.Posix.ARX.Strings where

import Control.Monad
import Data.ByteString.Char8
import Data.Maybe
import Data.Monoid
import Prelude hiding (elem, takeWhile, null)
import Data.String


-- | This type represents the value and not the layout of a C string: a
--   sequence of bytes not containing null.
newtype CString = CString ByteString deriving (Eq, Ord, Show, Monoid)
instance IsString CString  where fromString = fromJust . maybeFromString
instance Bytes CString     where bytes (CString s) = s
instance Norm CString      where norm = CString . takeWhile (/= '\0')

-- | A C string containing an @=@, separating the variable and the value.
newtype Env = Env ByteString deriving (Eq, Ord, Show)
instance IsString Env      where fromString = fromJust . maybeFromString
instance Bytes Env         where bytes (Env b) = b
instance Norm Env          where norm b | elem '=' c = Env c
                                        | otherwise  = Env (snoc c '=')
                                  where CString c = norm b

-- | A path segment is a non-empty C string not containing @\/@.
newtype PathSegment = PathSegment ByteString deriving (Eq, Ord, Show)
instance IsString PathSegment where fromString = fromJust . pathSeg. fromString
instance Bytes PathSegment    where bytes (PathSegment s) = s

pathSeg :: ByteString -> Maybe PathSegment
pathSeg b  |  null b || elem '\0' b || elem '/' b  =  Nothing
           |  otherwise                            =  Just (PathSegment b)


-- | A UNIX path is a non-empty C string.
newtype Path = Path ByteString deriving (Eq, Ord, Show)
instance IsString Path where fromString = fromJust . maybeFromString
instance Bytes Path    where bytes (Path s) = s
instance Norm Path     where norm b = if "" == c then Path "." else Path c
                              where CString c = norm b

class Norm t  where norm  :: ByteString -> t
class Bytes t where bytes :: t -> ByteString

maybeNorm                   ::  (Bytes t, Norm t) => ByteString -> Maybe t
maybeNorm b                  =  guard (bytes normed == b) >> Just normed
 where normed                =  norm b

maybeFromString             ::  (Bytes t, Norm t) => String -> Maybe t
maybeFromString              =  maybeNorm . fromString

