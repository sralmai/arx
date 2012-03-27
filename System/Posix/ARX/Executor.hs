
module System.Posix.ARX.Executor where

-- | The 'Executor' unpacks and runs a 'Task'. In @ARX@, the executor is a
--   shell script with the task compiled in to it. As a first step, the
--   script always creates a temporary directory to unpack its libraries; a
--   task without a directory set will execute in this temporary directory.
--   Everything that represents an extension to the basic model of a
--   reproducible process hierarchy with files and environment variables is
--   captured by the 'Executor' data structure.
data Executor = Executor
  { tag :: LDHName -- ^ A short prefix used for screens, temporary directories
                   --   and other resources allocated by @ARX@. The default
                   --   is @arx@. The names are constrained by the
                   --   letter-digit-hypen rule common to DNS.
  , tmp :: Path -- ^ The directory where temporary directories are allocated.
                --   The default is @/tmp@.
  , dir :: Maybe Path -- ^ Directory to run task in, if a change is desired.
  , redirect :: Maybe Redirect -- ^ Redirection of @STDERR@ and @STDOUT@. The
                               --   default is not to redirect.
  , detach :: Maybe Detach -- ^ Make it possible for the process to run with
                           --   the terminal detached (for example, with
                           --   screen). The default is not to detach.
  }
-- | Executor with defaults set.
executor = Executor { tag="arx", tmp="/tmp", dir=Nothing
                    , redirect=Nothing, detach=Nothing }

data Detach = Screen  -- TODO: add  | TMUX | NoHUP
-- TODO: data LXC = LXC ...
data Redirect = Syslog


