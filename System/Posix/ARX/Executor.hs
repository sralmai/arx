
module System.Posix.ARX.Executor where

import System.Posix.ARX.Composer
import System.Posix.ARX.LDHName
import qualified System.Posix.ARX.Sh as Sh
import System.Posix.ARX.Strings

-- | The 'Executor' unpacks and runs a 'Task'. In @ARX@, the executor is a
--   shell script with the task compiled in to it. As a first step, the
--   script always creates a temporary directory to unpack its libraries; a
--   task without a directory set will execute in this temporary directory.
--   Everything that represents an extension to the basic model of a
--   reproducible process hierarchy with files and environment Outerbles is
--   captured by the 'Executor' data structure.
data Executor = Executor
  { tag :: LDHName -- ^ A short prefix used for screens, temporary directories
                   --   and other resources allocated by @ARX@. The default
                   --   is @arx@. The names are constrained by the
                   --   letter-digit-hypen rule common to DNS.
  , tmp :: TMP -- ^ Temporary directory creation and removal settings.
  , dir :: Maybe Path -- ^ Directory to run task in, if a change is desired.
  , redirect :: Maybe Redirect -- ^ Redirection of @STDERR@ and @STDOUT@. The
                               --   default is not to redirect.
  , detach :: Maybe Detach -- ^ Make it possible for the process to run with
                           --   the terminal detached (for example, with
                           --   screen). The default is not to detach.
  }
-- | Executor with defaults set.
executorDefaults = Executor { tag="arx", tmp=tmpDefaults, dir=Nothing
                            , redirect=Nothing, detach=Nothing }

data Detach = Screen -- TODO: add  | TMUX | NoHUP
instance Compiled Detach where
  compile Screen = [CMD lib "screen_setup", CMD lib "screen_"]

data Redirect = Logger (Maybe CString)
instance Compiled Redirect where
  compile (Logger _) = [CMD lib "logger_"]

data TMP = TMP { path :: Path -- ^ Directory in which to create tmp dirs. The
                              --   default is @/tmp@.
               , rmOnSuccess :: Bool -- ^ Remove tmp dir on successful exit?
                                     --   The default is to do so.
               , rmOnFailure :: Bool -- ^ Remove tmp dir on failure?
                                     --   The default is to do so.
               }
tmpDefaults = TMP "/tmp" True True

-- TODO: data LXC = LXC ...

-- How to determine whether or not to use tmpx:
--  * If explicitly requested, use it.
--  * If a directory is not set, use it.
--  * Compile the executor without tmpx and if we have to call back in to the
--    library, add tmpx statements and recompile.

-- | Translate an Executor to tokens in preparation to joining it with env (if
--   used) and...
direct :: Executor -> [TOK]
direct Executor{..} = case detach,  in
  Just Screen -> [ libInner "screen_setup",
                   libInner "early_files",
  Nothing -> mconcat [ [CMD lib "tmpx"],
                       maybe [] ((++[CMD lib "trap_dot"]) . compile) detach,
                       maybe [] compile redirect ]
 where

  [ tmp_ ?> [libInner "tmp"]
  , tmp_ ?> [libInner "trap_on", Sh.VarVal [Left "dir"]]
  , [  ] |> (setup <$> detach)
  , tmp_ ?> [libInner "meta_archives"]
  ,         [libInner "popd_", libInner "cd_p", Sh.VarVal [Left "work_dir"]
  ,         [libInner "archives"]
  , tmp_ ?> [libInner "trap_off"] -- Remove trap since we are about to exit.
  , [  ] |> (enter <$> detach)
    -- User wrapper. Flock and other things.
  , [  ] |> (detach >> Just [libInner "trap_on", Sh.VarVal [Left "dir"]])
  , [  ] |> (pipes <$> redirect)
  ,         [libInner "interactive_sources"]
  , [  ] |> (reallyDetach <$> detach)
  ,         [libInner "background_sources"]
           -- env
           -- exec
  ]


|> :: t -> Maybe t -> t
(|>) t = maybe t id

?> :: (MonadPlus m) => Bool -> m t -> m t
bool ?> m = guard bool >> m

  --tag :: LDHName
 -- tmp :: TMP
-- dir :: Maybe Path
-- redirect :: Maybe Redirect
 -- detach :: Maybe Detach

tmpVars :: TMP -> [Sh.VarVal]
tmpVars TMP{..} = Sh.VarVal . (:[]) . Right . val <$>
  [ mappend "tmp=" (bytes path)
  , if rmOnSuccess then "rm0=true" else "rm0=false"
  , if rmOnFailure then "rm1=true" else "rm1=false" ]


class Compile t where compile :: t -> [TOK]

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


