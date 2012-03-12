{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , PatternGuards #-}
-- | URL parser, following RFC 3986 (<http://tools.ietf.org/html/rfc3986>).
module System.Posix.ARX.URL where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.ByteString.Char8 (groupBy, pack)
import Data.ByteString hiding (groupBy, pack, take, takeWhile)
import Data.Either
import qualified Data.List
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Word
import Prelude hiding (concatMap, drop, null, take, takeWhile)

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (hexadecimal, decimal, char)


-- | URL \"...refers to the subset of URIs that, in addition to identifying a
--         resource, provide a means of locating the resource by describing its
--         primary access mechanism\".
--
--   A breakdown of URLs, per the diagram from RFC 3986:
--
-- >     foo://example.com:8042/over/there?name=ferret#nose
-- >     \_/   \______________/\_________/ \_________/ \__/
-- >      |           |            |            |        |
-- >   scheme     authority       path        query   fragment
-- >      |   _____________________|__
-- >     / \ /                        \
-- >     urn:example:animal:ferret:nose
--
--   For the most part, URL parts are made of strings with percent encoding
--   required of certain characters. The scheme is especially limited in the
--   allowable data:
--
-- >  scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
--
--   Note well that no percent encoding is allowed.
--
--   The authority section, nominally denoting @userinfo\@host:port@, is in
--   fact quite flexible, allowing percent encoding for the hostname and
--   userinfo section; only the port has a byte range restriction, to digits.
--
--   Since this datatype represents the /data/ in a URL and not its particular
--   encoded form, we use 'ByteString' liberally.
data URL = URL { scheme    :: Scheme
               , authority :: Maybe Authority
               , path      :: ByteString
               , query     :: ByteString
               , fragment  :: ByteString }
 deriving (Eq, Ord, Show)
instance IsString URL where fromString = fromRight . fromString'
instance Parse URL where parser = url <$> (parser <* string "://")
                                      <*> authorityPath
                                      <*> option "" (char '?' *> qf)
                                      <*> option "" (char '#' *> qf)
                                       where url a (b, c) d e = URL a b c d e
                                             qf = option "" queryFragmentP
instance Encode URL where
  encode URL{..} = mconcat
    [ encode scheme, "://"
    , maybe "" encode authority
    , "/" `concatNonEmpty` pathEncode path
    , "?" `concatNonEmpty` selectiveEncode queryFragmentOctet query
    , "#" `concatNonEmpty` selectiveEncode queryFragmentOctet fragment ]

-- | \"Many URI schemes include a hierarchical element for a naming authority
--     so that governance of the name space defined by the remainder of the URI
--     is delegated to that authority...\"
-- >  authority   = [ userinfo "@" ] host [ ":" port ]
-- >  userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )
-- >  host        = IP-literal / IPv4address / reg-name
-- >  reg-name    = *( unreserved / pct-encoded / sub-delims )
--   The @reg-name@ production overlaps with the IP-based ones; and in fact
--   allows host to contain all bytes.
data Authority = Authority { userinfo :: ByteString
                           , host     :: ByteString
                           , port     :: Maybe Word16 }
 deriving (Eq, Ord, Show)
instance IsString Authority where fromString = fromRight . fromString'
instance Parse Authority where parser  =  Authority
                                      <$> option "" (userinfoP <* char '@')
                                      <*> regNameP
                                      <*> optional (char ':' *> decimal)
instance Encode Authority where
  encode Authority{..} = mconcat
    [ selectiveEncode userinfoOctet userinfo `concatNonEmpty` "@"
    , selectiveEncode regNameOctet host
    , maybe "" (mappend ":" . pack . show) port ]

-- | \"Each URI begins with a scheme name that refers to a specification for
--     assigning identifiers within that scheme. As such, the URI syntax is a
--     federated and extensible naming system wherein each scheme's
--     specification may further restrict the syntax and semantics of
--     identifiers using that scheme.\"
-- >  scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
newtype Scheme = Scheme ByteString deriving (Eq, Ord, Show)
instance IsString Scheme where fromString = fromRight . fromString'
instance Encode Scheme where encode (Scheme b) = b
instance Parse Scheme where parser  =  (Scheme .) . cons
                                   <$> satisfy (inClass "a-zA-Z")
                                   <*> takeWhile (inClass "a-zA-Z0-9.+-")

-- | Class for encoding items from this module as URLs.
class Encode t where encode :: t -> ByteString
-- | Class for parsing URL-related datatypes.
class Parse t where parser :: Parser t


-- | > *( unreserved / pct-encoded / sub-delims / ":" )
userinfoOctet :: Word8 -> Bool
userinfoOctet  = inClass "-a-zA-Z0-9._~!$&'()*+,;=:"

userinfoP :: Parser ByteString
userinfoP  = withPercents userinfoOctet

-- | > *( unreserved / pct-encoded / sub-delims )
regNameOctet :: Word8 -> Bool
regNameOctet  = inClass "-a-zA-Z0-9._~!$&'()*+,;="

regNameP :: Parser ByteString
regNameP  = withPercents regNameOctet

percent :: Parser Word8
percent  = char '%' *> usingOnly 2 hexadecimal

-- | Paths are quite sophisticated, with 5 productions to handle the different
--   URI contexts in which they appear. However, for the purpose of URL
--   parsing, we can assume that paths are always separated from the authority
--   (even the empty authority) with a @/@ and thus can work with a relatively
--   simple subset of the productions in the RFC.
--
-- >  path-rootless = segment-nz *( "/" segment )
-- >
-- >  ...
-- >
-- >  segment-nz    = 1*pchar
-- >
-- >  ...
-- >
-- >  pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
--
-- Although literal slash runs are not permitted by the RFC, equivalent
-- content can be encoded with percent encoding.
pathRootlessP :: Parser ByteString
pathRootlessP  = mappend <$> segment <*> option "" next
 where segment = withPercents segmentOctet
       next    = char '/' *> (mappend "/" <$> pathRootlessP)

-- > unreserved / pct-encoded / sub-delims / ":" / "@"
segmentOctet :: Word8 -> Bool
segmentOctet  = inClass "-a-zA-Z0-9._~!$&'()*+,;=:@"

-- ^ To parse the authority and path:
--
-- * we parse an authority and then optionally a slash and a path or
--
-- * we parse a single slash and then optionally a path.
authorityPath :: Parser (Maybe Authority, ByteString)
authorityPath  =  (,) <$> (Just <$> parser) <*> option "" (char '/' *> pathP)
              <|> (,) <$> (char '/' *> pure Nothing) <*> pathP
 where pathP = option "" pathRootlessP

-- | The query and fragment have identical productions in the RFC.
-- > *( pchar / "/" / "?" )
queryFragmentOctet :: Word8 -> Bool
queryFragmentOctet  = inClass "-a-zA-Z0-9._~!$&'()*+,;=:@/?"

queryFragmentP :: Parser ByteString
queryFragmentP  = withPercents queryFragmentOctet

usingOnly    :: Int -> Parser t -> Parser t
usingOnly c p = either (const mzero) return . parseOnly p =<< take c

-- | Parse a bytestream, accepting either literal bytes matching the predicate
--   or any percent encoded characters.
withPercents :: (Word8 -> Bool) -> Parser ByteString
withPercents predicate = cons <$> one <*> (option "" more)
 where more = withPercents predicate
       one  = satisfy predicate <|> percent

{-

sub-delims  = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
pct-encoded = "%" HEXDIG HEXDIG
unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"

 -}

-- | Transform any octet to its percent encoded form.
percentEncode  :: Word8 -> ByteString
percentEncode w = "%" `snoc` mod' (w `shiftR` 4) `snoc` mod' w
  where mod' w' = "0123456789ABCDEF" `index` fromIntegral (w' `mod` 16)

-- | Percent encode a 'ByteString', ignoring octets that match the predicate.
selectiveEncode :: (Word8 -> Bool) -> ByteString -> ByteString
selectiveEncode predicate bytes = unfoldr f ("", bytes)
 where f (queued, remaining) = dequeue <|> next
        where
         dequeue = do (w, queued') <- uncons queued
                      Just (w, (queued', remaining))
         next = do (w, remaining') <- uncons remaining
                   if predicate w then Just (w, (queued, remaining'))
                                  else f (percentEncode w, remaining')

concatNonEmpty a b | null a || null b = ""
                   | otherwise        = mappend a b

-- | Slash runs are not allowed in encoded paths. Here, this is interpreted to
--   mean that the first slash in path data, which would come after the slash
--   separating the path and the scheme or authority, should be escaped.
pathEncode :: ByteString -> ByteString
pathEncode  = mconcat . fmap enc . groupBy neitherSlash
 where
  enc s | "/" == s          = "/"
        | isPrefixOf "//" s = "/" `mappend` concatMap percentEncode (drop 1 s)
        | otherwise         = selectiveEncode regNameOctet s
  neitherSlash a b          = (a /= '/' && b /= '/') || (a == '/' && b == '/')


fromString' s = parseOnly (parser <* endOfInput) (fromString s)

{-# WARNING fromRight "This function calls error on Left; not safe." #-}
fromRight (Right x) = x
fromRight (Left s) = error s

