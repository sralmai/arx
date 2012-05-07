{-# LANGUAGE OverloadedStrings
           , TupleSections
           , PatternGuards #-}

module System.Posix.ARX.Composer where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString, intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String

import System.Posix.ARX.Strings
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
-- *  An 'External' command is an executable file, resolvable with @which@.
--
--   When treated as wrappers, each command potentially changes the execution
--   context; so we may need to insert explicit calls to @sh@ or the library.
data ExecutionContext
  = Sh { external :: Bool
         -- ^ Some @sh@ built-ins, like @exec@, put us back in an external
         --   context. This may apply to lib and inline calls, as well.
       }
  | Inline { external :: Bool
           , code :: Set Sh.Val
             -- ^ A set of declarations to inline, one of which is likely to
             --   be a definition of the command to be called. The declarations
             --   are inlined in /arbitrary order/ with a little postamble to
             --   call "$@". If they're not just function declarations and
             --   default settings, strange things may happen.
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


-- | Compile the execution vector to run in the given execution context.
compile :: ExecutionContext -> ExecV -> (ExecutionContext, [Sh.VarVal])
compile ctx' (ExecV tokens) = case foldr rollup ([], Nothing) tokens of
  (args, Nothing)               -> (ctx', args)
  (free, Just (ctx, following)) -> (final, free ++ transit ++ following)
   where (final, transit) = merge ctx' ctx

-- | Merge a token in to an execution vector that potentially has an execution
--   context already set. Takes care of flattening multiple invocations of the
--   shell and ensuring use of shelling @exec@ when calling external commands.
rollup :: TOK -> ([Sh.VarVal], Maybe (ExecutionContext, [Sh.VarVal]))
              -> ([Sh.VarVal], Maybe (ExecutionContext, [Sh.VarVal]))
rollup (ARG v) (vs, s) = (v:vs, s)
rollup (CMD ctx' v) r  = ([],) . Just $ case r of
  (vs, Nothing)        -> (ctx',  v:vs)
  (vs, Just (ctx, ys)) -> (ctx'', v:vs ++ transit ++ ys)
   where (ctx'', transit) = merge ctx' ctx

-- | Merges two execution contexts and potentially inserts code to transition
--   between them. The merge of two 'Inline' contexts, for example, is an
--   inline context with definitions from both of them and the transition code
--   is empty; but the merge of most contexts is simply the leftmost one and
--   the code inserted by 'transition'.
merge :: ExecutionContext -> ExecutionContext
      -> (ExecutionContext, [Sh.VarVal])
merge ctx' ctx = case ctx' of
  Sh False       | Sh b        <- ctx -> (Sh b                     , [ ]    )
                 | otherwise          -> (ctx'                     , transit)
  Inline False c | Inline b c' <- ctx -> (Inline b (Set.union c c'), [ ]    )
                 | otherwise          -> (ctx'                     , transit)
  _                                   -> (ctx'                     , transit)
 where transit = transition ctx' ctx

-- | Insert code that transitions from the first context to the second.
transition :: ExecutionContext -> ExecutionContext -> [Sh.VarVal]
transition ctx' ctx = case ctx of
  Sh _           | Sh False       <- ctx' -> []
                 | Inline False _ <- ctx' -> "exec" : backToSH
                 | otherwise              -> backToSH
  Inline _ codez | Sh False       <- ctx' -> "exec" : inlined codez
                 | Inline False _ <- ctx' -> "exec" : inlined codez
                 | otherwise              -> inlined codez
  External       | Sh False       <- ctx' -> ["exec"]
                 | Inline False _ <- ctx' -> ["exec"]
                 | otherwise              -> []
 where backToSH      = ["/bin/sh","-c","\"$@\"","sh"]
       inlined codez = ["/bin/sh","-c",Sh.VarVal [Right (inline codez)],"sh"]

inline :: Set Sh.Val -> Sh.Val
inline decls = norm code
 where
  code = (intercalate "\n" . (++["\"$@\""])) (bytes <$> Set.toList decls)

