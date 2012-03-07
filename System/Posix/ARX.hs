
module System.Posix.ARX where

import qualified Data.ByteString.Lazy
import Data.Monoid

import System.Posix.ARX.Composer
import System.Posix.ARX.Strings(CString(), Env(), Filename(), Path())
import System.Posix.ARX.URL

-- | A task to run on a @UNIX@ system. The task specification combines:
--
-- * a program name and argument vector,
--
-- * an environment mapping,
--
-- * a directory to change to,
--
-- * files to place in that directory (or in dirs relative to it).
--
--   Collectively, these attributes capture much of what makes a running
--   @UNIX@ app what it is -- its code, configuration and environment.
data Task = Task { cmd :: (Path, [CString])
                 , env :: [Env]
                 , dir  :: Path
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
  -- | A file archive and a hint for how to decode it.
  = Archive ArchiveType Data.ByteString.Lazy.ByteString
  -- | A URL indicating a source for files via a particular transport.
  | URL URL
 deriving (Eq, Ord, Show)

-- | Supported archives.
data ArchiveType = Tar | TBZ | TGZ
 deriving (Eq, Ord, Show)

