{-# LANGUAGE OverloadedStrings
           , PatternGuards #-}

module System.Posix.ARX.Composer where

import Data.ByteString.Char8 (ByteString)
import Data.String

import qualified System.Posix.ARX.Sh as Sh


-- | An execution vector is a command and a list of arguments. Some of the
--   arguments may in fact be other commands; the implication is that the
--   vector is a sequence of wrappers, performing set up and calling the next
--   command with the remaining arguments. Programming with wrappers is a
--   kind of stack-based programming.
data ExecV                   =  ExecV [TOK]
 deriving (Eq, Ord, Show)


-- | Commands and the contexts in which they can be executed:
--
-- *  An 'Sh' command is a shell built-in that needs to be executed in a shell
--    context.
--
-- *  A 'Lib' command requires a certain shell library to be loaded.
--
-- *  An 'External' command is an executable file, resolvable with @which@.
--
--   When treated as wrappers, each command potentially changes the execution
--   context; so we may need to insert explicit calls to @sh@ or the library.
--   For example, a command like:
--
-- >  libfunc1 a b libfunc2 c d env x=y libfunc3
--
--   Needs to be rewritten to:
--
-- >  /lib/path libfunc1 -option libfunc2 --flag env x=y /lib/path libfunc3
--
--   The first call in to lib causes it be in-process when we make the second
--   call; but then the call to @env@ transfers control to a process where
--   @/usr/bin/env@ is in core so a shell must be started and the library
--   reloaded for the third call to a library function.
data CMD
  = Sh { external :: Bool
         -- ^ Some @sh@ built-ins, like @exec@, put us back in an external
         --   context. This may apply to lib calls, as well.
       }
  | Lib { external :: Bool
          -- ^ Some @sh@ built-ins, like @exec@, put us back in an external
          --   context. This may apply to lib calls, as well.
        , source :: Sh.VarVal }
  | External
 deriving (Eq, Ord, Show)

-- | A token in an execution vector is either a command (which we assume to be
--   wrapped by commands further up the chain) or a simple string argument.
--   From a stack programming point of view, an 'ARG' is an instruction to put
--   something on the stack while a 'CMD' performs some task and may take
--   arguments off the stack.
data TOK                     =  CMD CMD Sh.VarVal | ARG Sh.VarVal
 deriving (Eq, Ord, Show)
instance IsString TOK where
  fromString                 =  CMD External . fromString


compile (ExecV tokens)       =  snd (foldr f (Sh True, []) tokens)
 where
  f                         ::  TOK -> (CMD, [Sh.VarVal]) -> (CMD, [Sh.VarVal])
  f (ARG arg) (t, args)      =  (t, arg:args)
  f (CMD t arg) (t', args)   =  (t, args')
   where
    ba:ck:to:sh:[]           =  "/bin/sh":"-c":"\"$@\"":"sh":[]
    args' = case t of
      Sh _ | Sh False <- t'               -> arg:args
           | otherwise                    -> ba:ck:to:sh:arg:args
      Lib _ x | Sh False <- t'            -> "exec":x:arg:args
              | Lib False y <- t', x /= y -> "exec":x:arg:args
              | Lib False y <- t', x == y -> arg:args
              | otherwise                 -> x:arg:args
      External | Sh False <- t'           -> "exec":arg:args
               | Lib False _ <- t'        -> "exec":arg:args
               | otherwise                -> arg:args

