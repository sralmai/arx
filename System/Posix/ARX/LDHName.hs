{-# LANGUAGE OverloadedStrings #-}
-- | Names conforming to the \"letter-digit-hyphen\" rule commonly followed in
--   DNS naming. This restricted subset of characters meshes well with
--   restrictions imposed by syslog, email systems, DOS filesystems,
--   serialization formats and variable naming conventions in many
--   programming languages.
module System.Posix.ARX.LDHName where

import Control.Monad
import Data.ByteString.Char8
import Data.Maybe
import Data.Monoid
import Prelude hiding (length)
import Data.String

import System.Posix.ARX.RE


newtype LDHName = LDHName ByteString deriving (Eq, Ord, Show)
instance IsString LDHName where fromString = fromJust . ldh . fromString

-- ^ Recognizer for LDH names. The productions and guidelines for a "prudent"
--   subset of domain names as given in <http://tools.ietf.org/html/rfc1035>:
--
-- > The following syntax will result in fewer problems with many
-- > applications that use domain names (e.g., mail, TELNET).
-- >
-- > <domain> ::= <subdomain> | " "
-- >
-- > <subdomain> ::= <label> | <subdomain> "." <label>
-- >
-- > <label> ::= <letter> [ [ <ldh-str> ] <let-dig> ]
-- >
-- > <ldh-str> ::= <let-dig-hyp> | <let-dig-hyp> <ldh-str>
-- >
-- > <let-dig-hyp> ::= <let-dig> | "-"
-- >
-- > <let-dig> ::= <letter> | <digit>
-- >
-- > <letter> ::= any one of the 52 alphabetic characters A through Z in
-- > upper case and a through z in lower case
-- >
-- > <digit> ::= any one of the ten digits 0 through 9
-- >
-- > Note that while upper and lower case letters are allowed in domain
-- > names, no significance is attached to the case.  That is, two names with
-- > the same spelling but different case are to be treated as if identical.
-- >
-- > The labels must follow the rules for ARPANET host names.  They must
-- > start with a letter, end with a letter or digit, and have as interior
-- > characters only letters, digits, and hyphen.  There are also some
-- > restrictions on the length.  Labels must be 63 characters or less.
-- >
-- > ...
-- >
-- > Various objects and parameters in the DNS have size limits.  They are
-- > listed below.  Some could be easily changed, others are more
-- > fundamental.
-- >
-- > labels          63 octets or less
-- >
-- > names           255 octets or less
--
--   In the RFC 1035 productions, labels must begin with letters; but in RFC
--   1123 this was relaxed to a letter or a digit.
--
--   According to many sources on the internet, including Wikipedia, the
--   practical length of the character data in a domain name is 253
--   characters. The limit of 255 is then interpreted as the total storage
--   allocated to the domain name, including, presumably, a final @.@ and
--   @NUL@.
--
--   We vary from the RFC by requiring non-empty names.
--
--  In 
ldh  :: ByteString -> Maybe LDHName
ldh b = (ldhRE =~ b) >> guard (length b <= 253) >> Just (LDHName b)

ldhRE = "^[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\
     \([.][a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?){0,126}$"
  -- Haskell string line continuations are stupid. Neither backslash makes
  -- it's way through to the regular expression; you can ignore them.

