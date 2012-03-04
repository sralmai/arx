
module System.Posix.ARX where

import Data.ByteString (ByteString)


-- | A task to run on a @UNIX@ system. The task specification combines:
-- * a program name and argument vector,
-- * an environment mapping,
-- * a directory to change to,
-- * files to place in that directory.
data Task                    =  Task ExecV Env Dir Files
deriving (Eq, Ord, Show)


-- | A command and its arguments.
data ExecV                   =  ExecV ByteString [ByteString]
deriving (Eq, Ord, Show)


-- | An environment mapping.
type Env                     =  [(ByteString, ByteString)]


-- | File archives or file retrieval functions -- using, for example, Git or
--   HTTP -- and where to place their contents. Archives are unpacked in the
--   order given.
type Files                   =  [(Dir, FileSource)]

data FileSource
  -- | A file archive to be inlined in to the shell script, using the @SHDAT@
  --   encoding.
  = Archive ArchiveType ByteSource
  -- | Used to call a shell library function for things like retrieving code
  --   via Git or downloading and unpacking a tarball from S3.
  | LibCall ExecV
deriving (Eq, Ord, Show)

-- | Things that can be read for bytes -- either a literal 'ByteString' or a
--   path to a file.
data ByteSource              =  Bytes ByteString | Path ByteString
deriving (Eq, Ord, Show)

-- | Types of supported archive.
data ArchiveType             =  Tar | TBZ | TGZ
deriving (Eq, Ord, Show)


type Dir                     =  ByteString



