{-# LANGUAGE OverloadedStrings
           , TupleSections
           , PatternGuards #-}

module System.Posix.ARX.Composer where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString, intercalate)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String

import System.Posix.ARX.Strings
import qualified System.Posix.ARX.Sh as Sh


-- | A sequence of labelled wrappers.
data Wrapped label = Wrapped ExecutionContext [(label, [Sh.VarVal])]
 deriving (Eq, Ord, Show)
instance Monoid (Wrapped t) where
  mempty = Wrapped (Inline False Set.empty) []
  Wrapped ctx defs `mappend` Wrapped _    []            = Wrapped ctx defs
  Wrapped ctx defs `mappend` Wrapped ctx_ ((l, code):t) = Wrapped ctx' defs'
   where (ctx', transit) = merge ctx ctx_
         defs' = defs ++ (l, transit ++ code):t


-- | Commands and the contexts in which they can be executed:
--
-- *  An 'Inline' command has its code -- a shell function definition -- made
--    available by way of wrapping it with @sh -c '...'@. For example, a
--    function @const <something>@ that prints the first argument no matter
--    what the other arguments are given might be called like this:
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
  = Inline { external :: Bool
             -- ^ A shell function might place us in an external context when
             --   it is done.
           , code :: Set Sh.Val
             -- ^ A set of declarations to inline, one of which is likely to
             --   be a definition of the command to be called. The declarations
             --   are inlined in /arbitrary order/ with a little postamble to
             --   call "$@". If they're not just function declarations and
             --   default settings, strange things may happen.
           }
  | External
 deriving (Eq, Ord, Show)

-- | Merges two execution contexts and potentially inserts code to transition
--   between them. The merge of two 'Inline' contexts, for example, is an
--   inline context with definitions from both of them and the transition code
--   is empty; but the merge of most contexts is simply the leftmost one and
--   the code inserted by 'transition'.
merge :: ExecutionContext -> ExecutionContext
      -> (ExecutionContext, [Sh.VarVal])
merge ctx' ctx = case ctx' of
  Inline False c | Inline b c' <- ctx -> (Inline b (Set.union c c'), [ ]    )
                 | otherwise          -> (ctx'                     , transit)
  _                                   -> (ctx'                     , transit)
 where transit = transition ctx' ctx

-- | Insert code that transitions from the first context to the second.
transition :: ExecutionContext -> ExecutionContext -> [Sh.VarVal]
transition ctx' ctx = case ctx of
  Inline _ codez | Inline False _ <- ctx' -> "exec" : inlined codez
                 | otherwise              -> inlined codez
  External       | Inline False _ <- ctx' -> ["exec"]
                 | otherwise              -> []
 where inlined codez = ["/bin/sh","-c",Sh.VarVal [Right (inline codez)],"sh"]

inline :: Set Sh.Val -> Sh.Val
inline decls = norm code
 where
  code = (intercalate "\n" . (++["\"$@\""])) (bytes <$> Set.toList decls)

