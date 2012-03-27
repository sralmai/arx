{-# LANGUAGE OverloadedStrings
           , GeneralizedNewtypeDeriving #-}
module System.Posix.ARX where

import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Ord
import Data.Word

import Network.URI (URI())

import System.Posix.ARX.Composer
import System.Posix.ARX.Strings(CString(), Env(), Path())
import System.Posix.ARX.LDHName(LDHName())
import System.Posix.ARX.URI


-- | A task to run on a @UNIX@ system. The task specification combines:
--
-- * a program name and argument vector,
--
-- * an environment mapping,
--
-- * files to place in the working directory (or in dirs relative to it).
--
--   Collectively, these attributes capture much of what makes a running
--   @UNIX@ app what it is -- its code, configuration and environment.
data Task = Task { cmd   :: (Path, [CString])
                 , env   :: [Env]
                 , files :: Files }
 deriving (Eq, Ord, Show)

-- | A files spec combines files sources and directory paths, which are
--   interpreted relative to the task's working directory. The sources are
--   unpacked in order, with later sources overriding files laid down by
--   earlier ones if there is any conflict. File specs form a monoid, where
--   simple concatenation is the associative operation, since unpacking one
--   set of sources and then the other is just the same as unpacking all the
--   sources in order.
newtype Files = Files [(Path, FileSource)]
 deriving (Eq, Ord, Show, Monoid)

-- | A file source is a literal file archive annotated with its archive type;
--   or a URL which indicates a checkout via Git, a download via Curl or some
--   other extended mechanism for getting files.
data FileSource
  -- | A file archive. For @tar@, @tbz@ and @tgz@, the archive type is
  --   automatically detected by @ARX@.
  = Archive ByteString
  -- | A URI indicating a source for files via @HTTP@ (for example).
  | URI URI
 deriving (Eq, Show)
instance Ord FileSource where
  Archive a `compare` Archive b = compare a b
  Archive _ `compare` URI _     = LT
  URI _     `compare` Archive _ = GT
  URI a     `compare` URI b     = normalized a `compare` normalized b

