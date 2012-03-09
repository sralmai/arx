
module System.Posix.ARX where

import Data.ByteString.Lazy (ByteString)
import Data.Monoid

import System.Posix.ARX.Composer
import System.Posix.ARX.Strings(CString(), Env(), PathSegment(), Path())
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
data Task = Task { cmd   :: (Path, [CString])
                 , env   :: [Env]
                 , dir   :: Path
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
  -- | A URL indicating a source for files via @git@, over @HTTP@ or other,
  --   extended mechanisms.
  | URL URL
 deriving (Eq, Ord, Show)


-- | The 'Executor' encapsulates all the "vendor specific" nonsense in @ARX@.
--   It's where we find all the "ARXiness". Once a task has been specified,
--   it's up the executor how we handle space allocation, timeouts,
--   daemonization and other matters.
data Executor = Executor { timeout :: Maybe Word32,
                           background :: Maybe Background,
                           redirect :: Maybe Redirect,
                           lxc :: Maybe LXC }

data Background = Screen  -- TODO: add  | TMUX | NoHUP
data LXC = LXC -- TODO: Set options.
data Redirect = Syslog

