module Data.Ouro.Lisp.Canon.Traverse
( bottomUp
, topDown
) where

import qualified Data.Ouro.Lisp.Surface as S


-- The two ways a desugaring rule walks the surface tree. Each pass uses one.


-- A rule that must see a form before its parts are rewritten, because it depends on the
-- form's own shape: the rule rewrites the form, then its parts are walked
topDown :: (S.Expr -> S.Expr) -> S.Expr -> S.Expr
topDown rule = go
    where
    go expr = case rule expr of
        S.Form     pos items    -> S.Form     pos (map go items)
        S.Asserted pos ty inner -> S.Asserted pos ty (go inner)
        leaf                    -> leaf


-- A rule that rewrites one form into another: a form's parts are rewritten first, then the
-- form itself
bottomUp :: (S.Expr -> S.Expr) -> S.Expr -> S.Expr
bottomUp rule = go
    where
    go = \case
        S.Form     pos items    -> rule (S.Form pos (map go items))
        S.Asserted pos ty inner -> S.Asserted pos ty (go inner)
        leaf                    -> leaf
