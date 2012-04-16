{-# LANGUAGE RecordWildCards
           , TupleSections
  #-}
module System.Posix.ARX.Executor where

import Control.Applicative
import Control.Monad
import Data.Monoid

import System.Posix.ARX.Composer
import System.Posix.ARX.LDHName
import qualified System.Posix.ARX.Sh as Sh
import System.Posix.ARX.Strings


-- | The 'Executor' unpacks and runs a 'Task'. In @ARX@, the executor is a
--   shell script with the task compiled in to it. Everything that represents
--   an extension to the basic model of a reproducible process hierarchy with
--   files and environment Outerbles is captured by the 'Executor' data
--   structure.
data Executor = Executor
  { tag :: LDHName -- ^ A short prefix used for screens, temporary directories
                   --   and other resources allocated by @ARX@. The default
                   --   is @arx@. The names are constrained by the
                   --   letter-digit-hypen rule common to DNS.
  , dir :: Either TMP Path -- ^ Directory specification: either create a
                           --   temporary directory using the desired settings
                           --   or change to a directory in certain place in
                           --   the filesystem hierarchy.
  , redirect :: Maybe Redirect -- ^ Redirection of @STDERR@ and @STDOUT@. The
                               --   default is not to redirect.
  , detach :: Maybe Detach -- ^ Make it possible for the process to run with
                           --   the terminal detached (for example, with
                           --   screen). The default is not to detach.
  }
instance Vars Executor where
  variables Executor{..} = mconcat
    [ case dir of Right path -> [("work_dir", p2v path)]
                  Left tmp   -> variables tmp
    , maybe [] variables redirect
    ]
-- | Executor with defaults set.
executor = Executor { tag="arx", dir=(Left tmp)
                    , redirect=Nothing, detach=Nothing }

data Detach = Screen

setup :: Detach -> [TOK]
setup Screen = [libInner "screen_setup"]

enter :: Detach -> [TOK]
enter Screen = [libOuter "screen_run"]


data Redirect = Logger (Maybe CString)
instance Vars Redirect where
  variables (Logger syslogTag) =
    maybe [] ((:[]) . ("syslog_tag",) . Sh.Val) syslogTag

pipes :: Redirect -> [TOK]
pipes (Logger _) = [libInner "logger_"]


data TMP = TMP { path :: Path -- ^ Directory in which to create tmp dirs. The
                              --   default is @/tmp@.
               , rmOnSuccess :: Bool -- ^ Remove tmp dir on successful exit?
                                     --   The default is to do so.
               , rmOnFailure :: Bool -- ^ Remove tmp dir on failure?
                                     --   The default is to do so.
               }
instance Vars TMP where
  variables TMP{..} = [ ("tmp", p2v path)
                      , ("rm0", b2v rmOnSuccess)
                      , ("rm1", b2v rmOnFailure)]
tmp = TMP "/tmp" True True

-- TODO: data LXC = LXC ...

-- | Translate an Executor to tokens, forming a command line wrapper, which is
--   wrapped around a call to @env@ to launch the user command.
wrapper :: Executor -> [TOK]
wrapper Executor{..} = mconcat
  [ [libInner "tmp"]                                       --?  withTmp
  , [libInner "trap_on", dirVar]                           --?  withTmp
  , setup <$> detach                                       --|  []
  , [libInner "meta_archives"]                             --?  withTmp
  , [libInner "popd_", libInner "cd_p", cwdVar]
  , [libInner "archives"]
  , [libInner "interactive_sources"]
  , detach >> Just [libInner "trap_off"]           --| []  --?  withTmp
  , enter <$> detach                                       --|  []
    -- [ User wrapper and additional wrappers, like flock and LXC, go here. ]
    -- [ Below the wrappers, we reload the shell library and run the task. We
    --   write the shell library literally in to a shell command line with -c,
    --   so we aren't forced to drop a file if it's not necessary. ]
    -- [ Below the wrappers, we reload the shell library and run the task. ]
  , detach >> Just [libInner "trap_on", dirVar]            --|  []
  , pipes <$> redirect                                     --|  []
  , [libInner "background_sources"]
  ] -- All of this is wrapped around a call to env which is wrapped
    -- around a call to:
    --   sh -c 'exec "$@"' <first word in user command> <user command> "$@"
 where
  infixl 0 --|, --?
  (--|) m t = maybe t id m
  (--?) m bool = guard bool >> m
  dirVar = ARG (Sh.VarVal [Left "dir"])
  cwdVar = ARG (Sh.VarVal [Left "work_dir"])
  withTmp = case dir of Left _  -> True
                        Right _ -> False


p2v (Path c) = Sh.Val c
b2v b = if b then "true" else "false"


lib :: Bool -> Sh.VarVal -> TOK
lib b = CMD (Lib b libPath)

external :: Sh.VarVal -> TOK
external  = CMD External

libInner = lib False
libOuter = lib True

-- | The library is stored at @$dir/lib@, with @$dir@ set by the executor
--   dynamically.
libPath :: Sh.VarVal
libPath  = Sh.VarVal [Left "dir", Right "/lib"]


-- | Many datastructures in this module are translated to shell functions that
--   can be controlled through certain variables. This function creates shell
--   assignments that set those variables.
class Vars t where variables :: t -> [(Sh.Var, Sh.Val)]

