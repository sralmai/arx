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

-- | Names conforming to the "letter-digit-hyphen" rule commonly followed in
--   DNS naming. See <http://tools.ietf.org/html/rfc3467#page-3>. This
--   restricted subset of characters meshes well with restrictions imposed by
--   syslog, email systems, DOS filesystems, unquoted strings in YAML,
--   and variable names in many programming languages (espcially with case
--   normalization and allow substituting @_@ for @-@). The rules followed in
--   this implementation are drawn from RFC 1035.
newtype LDHName = LDHName ByteString deriving (Eq, Ord, Show)
{-

The following syntax will result in fewer problems with many
applications that use domain names (e.g., mail, TELNET).

<domain> ::= <subdomain> | " "

<subdomain> ::= <label> | <subdomain> "." <label>

<label> ::= <letter> [ [ <ldh-str> ] <let-dig> ]

<ldh-str> ::= <let-dig-hyp> | <let-dig-hyp> <ldh-str>

<let-dig-hyp> ::= <let-dig> | "-"

<let-dig> ::= <letter> | <digit>

<letter> ::= any one of the 52 alphabetic characters A through Z in
upper case and a through z in lower case

<digit> ::= any one of the ten digits 0 through 9

Note that while upper and lower case letters are allowed in domain
names, no significance is attached to the case.  That is, two names with
the same spelling but different case are to be treated as if identical.

The labels must follow the rules for ARPANET host names.  They must
start with a letter, end with a letter or digit, and have as interior
characters only letters, digits, and hyphen.  There are also some
restrictions on the length.  Labels must be 63 characters or less.

...

Various objects and parameters in the DNS have size limits.  They are
listed below.  Some could be easily changed, others are more
fundamental.

labels          63 octets or less

names           255 octets or less

 -- http://tools.ietf.org/html/rfc1035

 -}

class Norm t  where norm  :: ByteString -> t
class Bytes t where bytes :: t -> ByteString

maybeNorm                   ::  (Bytes t, Norm t) => ByteString -> Maybe t
maybeNorm b                  =  guard (bytes normed == b) >> Just normed
 where normed                =  norm b

maybeFromString             ::  (Bytes t, Norm t) => String -> Maybe t
maybeFromString              =  maybeNorm . fromString

