-- | URL "...refers to the subset of URIs that, in addition to identifying a
--   resource, provide a means of locating the resource by describing its
--   primary access mechanism".
--   http://tools.ietf.org/html/rfc3986
module System.Posix.ARX.URL where

import Data.ByteString.Char8 (ByteString)
import Data.Word

import System.Posix.ARX.Strings (CString)


data URL = URL { scheme    :: Scheme
               , authority :: Authority
               , path      :: ByteString
               , query     :: ByteString
               , fragment  :: ByteString }
 deriving (Eq, Ord, Show)

data Authority = Authority { userinfo :: ByteString
                           , host     :: ByteString
                           , port     :: Word64 }
 deriving (Eq, Ord, Show)

newtype Scheme               =  Scheme ByteString
 deriving (Eq, Ord, Show)

{-

         foo://example.com:8042/over/there?name=ferret#nose
         \_/   \______________/\_________/ \_________/ \__/
          |           |            |            |        |
       scheme     authority       path        query   fragment
          |   _____________________|__
         / \ /                        \
         urn:example:animal:ferret:nose

   Allowable bytes in URLs, according to RFC 3986:

    Paths are made of a few different productions, one of which is pchar,
    which includes percent encoded characters. Thus we can encode arbitrary
    data in the path component, if we use percent encoding. The query and
    fragment are also defined in terms of pchar; so the allowable data is
    again the space of all octet strings.

    The scheme is more limited in the allowable data:

      scheme                 =  ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )

    Note well that no percent encoding is allowed.

    The authority section, nominally denoting userinfo@host:port, is in fact
    quite flexible, allowing percent encoding for the hostname and and
    userinfo section; only the port has a byte range restriction, to digits.

 -}

