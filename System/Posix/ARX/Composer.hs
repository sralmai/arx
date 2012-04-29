{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
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
-- *  An 'Inline' command has its code -- a shell function definition -- made
--    available by way of wrapping it @sh -c '...'@. For example, a function
--    @const <something>@ that prints the first argument no matter what the
--    other arguments are might be called like this:
--
-- >  const a b c
--
--    It must be inlined like this:
--
-- >  sh -c 'const() { echo "$1" ;} ; "$@"' sh const a b c
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
data ExecutionContext
  = Sh { external :: Bool
         -- ^ Some @sh@ built-ins, like @exec@, put us back in an external
         --   context. This may apply to lib and inline calls, as well.
       }
  | Inline { external :: Bool
           , code :: [Sh.Val] -- ^ Code to inline, by wrapping with
                              --   @sh -c '...'@. All inlined segments will be
                              --   separated by newlines.
           }
  | Lib { external :: Bool
        , source :: Sh.VarVal -- ^ File to call into for this library.
        }
  | External
 deriving (Eq, Ord, Show)

-- | A token in an execution vector is either a command (which we assume to be
--   wrapped by commands further up the chain) or a simple string argument.
--   From a stack programming point of view, an 'ARG' is an instruction to put
--   something on the stack while a 'CMD' performs some task and may take
--   arguments off the stack.
data TOK                     =  CMD ExecutionContext Sh.VarVal | ARG Sh.VarVal
 deriving (Eq, Ord, Show)
instance IsString TOK where
  fromString                 =  CMD External . fromString


compile                     ::  ExecutionContext -> ExecV -> [Sh.VarVal]
compile ctx (ExecV tokens)   =  worker ctx tokens
 where
  worker _ []                =  []
  worker t (ARG arg:rest)    =  arg : worker t rest
  worker t (CMD t' arg:rest) =  case t' of
    Sh _     | Sh False     <- t         -> arg:recurse
             | otherwise                 -> ba:ck:to:sh:arg:recurse
    Lib _ x  | Sh False     <- t         -> "exec":x:arg:recurse
             | Lib False y  <- t, x /= y -> "exec":x:arg:recurse
             | Lib False y  <- t, x == y -> arg:recurse
             | otherwise                 -> x:arg:recurse
    External | Sh False     <- t         -> "exec":arg:recurse
             | Lib False _  <- t         -> "exec":arg:recurse
             | otherwise                 -> arg:recurse
   where recurse             =  worker t' rest
         ba:ck:to:sh:[]      =  "/bin/sh":"-c":"\"$@\"":"sh":[]

