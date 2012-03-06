
module System.Posix.ARX where

import qualified Data.ByteString.Lazy

import System.Posix.ARX.Composer
import System.Posix.ARX.Strings( CString(), Env(), Filename(),
                                 Bytes(..), Norm(..), maybeNorm )
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
data Task
  = Task { cmd   :: (CString, [CString]) -- ^ Command and its arguments.
         , env   :: [Env] -- ^ Environment mapping strings.
         , dir   :: CString -- ^ Desired working directory.
         , files :: [(CString, FileSource)] -- ^ File sources.
         }
 deriving (Eq, Ord, Show)

-- | A file source is a literal file archive annotated with its archive type;
--   or a URL which indicates a checkout via Git, a download via Curl or some
--   other extended mechanism for getting files.
data FileSource
  -- | A file archive and a hint for how to decode it.
  = Archive ArchiveType Data.ByteString.Lazy.ByteString
  -- | A URL indicating a source for files via a particular transport.
  | URL URL
 deriving (Eq, Ord, Show)

-- | Support archives.
data ArchiveType             =  Tar | TBZ | TGZ
 deriving (Eq, Ord, Show)

