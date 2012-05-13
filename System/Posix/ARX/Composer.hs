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

-- | Execution contexts for wrappers. For common wrappers, like @flock@ or
--   @screen@, there's no real context needed; this is the @external@ context;
--   but for shell functions that are used as wrappers we need a way to inline
--   their code, escaped in a such a way that the calling shell (always @sh@
--   in ARX) simply passes it on.
data ExecutionContext
  = InlineSh   Sh.VarVal -- ^ Sh code.
  | InlineBash Sh.VarVal -- ^ Bash code.
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

