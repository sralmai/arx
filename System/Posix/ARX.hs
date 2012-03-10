
module System.Posix.ARX where

import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Word

import System.Posix.ARX.Composer
import System.Posix.ARX.Strings(CString(), Env(), Path())
import System.Posix.ARX.LDHName(LDHName())
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


-- | The 'Executor' unpacks and runs a 'Task'. In @ARX@, the executor is a
--   shell script with the task compiled in to it. As a first step, the
--   script always creates a temporary directory to unpack its libraries; by
--   default, the task is executed in an empty directory below this directory.
--   The executor can also daemonize the task behind a screen and redirect its
--   output to syslog. Traits of how we run a task that are disctinct from
--   what to run -- for example, containerization, 
data Executor = Executor
  { tag :: LDHName -- ^ A short prefix used for screens, temporary directories
                   --   and other resources allocated by @ARX@. The default
                   --   is @arx@. The names are constrained by the
                   --   'letter-digit-hypen' rule common to DNS.
  , tmp :: Path -- ^ The directory where temporary directories are allocated.
                --   The default is @/tmp@.
  , redirect :: Maybe Redirect -- ^ Redirection of @STDERR@ and @STDOUT@. The
                               --   default is not to redirect.
  , detach :: Maybe Detach -- ^ Make it possible for the process to run with
                           --   the terminal detached (for example, with
                           --   screen or tmux). The default is not to detach.
  }
-- | Executor with defaults set.
executor = Executor { tag="arx", tmp="/tmp", redirect=Nothing, detach=Nothing }

data Detach = Screen  -- TODO: add  | TMUX | NoHUP
-- TODO: data LXC = LXC ...
data Redirect = Syslog

