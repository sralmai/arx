
module System.Posix.ARX.Composer where

import Data.ByteString

{-| Some commands functions as wrappers for others, accepting a list of
    arguments and calling them as a command. For the sake of this tool, these
    wrappers are of two kinds: tools like screen, that use a variant of
    @execv@, and shell functions that use @exec@ or a subshell, on the one
    hand; and shell functions that simply execute the argument vector, fully
    escaped, on the other. The latter are 'Inner' wrappers, the former 'Outer'
    wrappers. For 'Inner' wrappers, commands that definitely must resolve to
    paths are wrapped with @which@; for 'Outer' wrappers, 'ExecV' arrays are
    prefixed with @./lib@ for library function commands or with @sh@ for
    built-ins.
 -}
data Wrapper                 =  Inner ExecV | Outer ExecV


-- | An execution vector is a command and a list of arguments.
data ExecV                   =  ExecV !CMD ![ByteString]


{-| A command could be something that only functions as a built-in, for
    example @set@, a shell function or alias dependent on the library being
    sourced, an external command that should be resolved to a filesystem path,
    or something ambivalent like @echo@ or @printf@ that can be a shell
    built-in or can be resolved to a filesystem path.
 -}
data CMD                     =  BuiltIn !ByteString  | Lib !ByteString
                             |  External !ByteString | Ambivalent !ByteString
instance IsString where
  fromString                 =  Ambivalent . fromString

