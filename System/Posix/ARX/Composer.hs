
module System.Posix.ARX.Composer where

import Data.ByteString.Char8 (ByteString)
import Data.String

import System.Posix.ARX.Sh


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
-- *  An 'External' command is an executable file, resolvable with @which@.
--
-- *  An 'Amb' command, like @echo@ or @true@, could be treated as a shell
--    built-in or an external file, indifferently.
--
-- *  A 'Lib' command requires a certain shell library to be loaded.
--
--   When treated as wrappers, each command potentially changes the execution
--   context; so we may need to insert explicit calls to @sh@ or the library.
--   For example, a command like:
-- @
--    libfunc1 a b libfunc2 c d env x=y libfunc3
-- @
--   Needs to be rewritten to:
-- @
--    /lib/path libfunc1 -option libfunc2 --flag env x=y /lib/path libfunc3
-- @
--   The first call in to lib causes it be in-process when we make the second
--   call; but then the call to @env@ transfers control to a process where
--   @/usr/bin/env@ is in core so a shell must be started and the library
--   reloaded for the third call to a library function.
data CMD
  = Sh { external :: Bool
         -- ^ Some @sh@ built-ins, like @exec@, put us back in an external
         --   context. This may apply to lib calls, as well.
       }
  | External
  | Amb
  | Lib { external :: Bool
          -- ^ Some @sh@ built-ins, like @exec@, put us back in an external
          --   context. This may apply to lib calls, as well.
        , source :: StringWithSubs }
 deriving (Eq, Ord, Show)

-- | A token in an execution vector is either a command (which we assume to be
--   wrapped by commands further up the chain) or a simple string argument.
data TOK                     =  CMD CMD ByteString | ARG ByteString
 deriving (Eq, Ord, Show)
instance IsString TOK where
  fromString                 =  CMD Amb . fromString

