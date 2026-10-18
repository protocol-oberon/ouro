module Data.Ouro.Lisp.Canon.With
( withForms
) where

import           Data.Ouro.Lisp.Canon.Traverse (bottomUp)
import qualified Data.Ouro.Lisp.Surface        as S


-- withForms.
--
-- (with base item ...) is base with items written over it, layered as inlay layers:
--
--   (with base :k v)   ((inlay base) :k v)
--   (with base)        base
--
-- Written attribute pairs win over the base's, and an array base has its elements spliced.
-- Runs after 'threading', so a 'with' step of (-> x ...) has already received x as its base.
withForms :: S.Expr -> S.Expr
withForms = bottomUp $ \case
    S.Form pos (S.Symbol sPos "with" : base : items)
        -> case items of
               [] -> base
               _  -> S.Form pos (S.Form sPos [S.Symbol sPos "inlay", base] : items)
    other
        -> other
