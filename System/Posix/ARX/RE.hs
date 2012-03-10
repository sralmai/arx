-- | Wrappers for regexen to provide an 'IsString' instance and a monomorphic
--   match operator.
module System.Posix.ARX.RE where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.String

import Text.Regex.TDFA ( Regex, RegexContext(matchM),
                         makeRegexOpts, makeRegexOptsM,
                         defaultExecOpt, blankCompOpt, lastStarGreedy )


(=~) :: Regex -> ByteString
     -> Maybe (ByteString, ByteString, ByteString, [ByteString])
(=~)  = matchM

re :: ByteString -> Maybe Regex
re  = makeRegexOptsM fullPosix defaultExecOpt
 where fullPosix                  =  blankCompOpt { lastStarGreedy=True }

instance IsString Regex where
  fromString = fromJust . re . fromString

