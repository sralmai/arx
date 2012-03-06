
module System.Posix.ARX where

import Data.ByteString (ByteString)

import System.Posix.ARX.Composer


-- | A task to run on a @UNIX@ system. The task specification combines:
--
-- * a program name and argument vector,
--
-- * an environment mapping,
--
-- * a directory to change to,
--
-- * files to place in that directory.
data Task
  = Task { cmd :: (ByteString, [ByteString]) -- ^ Command and its arguments.
         , env :: [(ByteString, ByteString)] -- ^ Environment mapping.
         , dir :: ByteString -- ^ Desired working directory.
         , files :: [(ByteString, FileSource)] -- ^ File sources (git, tars).
         }
 deriving (Eq, Ord, Show)

data FileSource
  -- | A file archive to be inlined in to the shell script, using the @SHDAT@
  --   encoding.
  = Archive ArchiveType ByteSource
  -- | Used to call a shell library function for things like retrieving code
  --   via Git or downloading and unpacking a tarball over HTTP.
  | LibCall ExecV
 deriving (Eq, Ord, Show)

-- | Things that can be read for bytes -- either a literal 'ByteString' or a
--   path to a file.
data ByteSource              =  Bytes ByteString | Path ByteString
 deriving (Eq, Ord, Show)

-- | Types of supported archive.
data ArchiveType             =  Tar | TBZ | TGZ
 deriving (Eq, Ord, Show)

