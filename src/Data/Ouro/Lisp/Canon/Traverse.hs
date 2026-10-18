module Data.Ouro.Lisp.Canon.Traverse
( bottomUp
, siblings
, topDown
) where

import qualified Data.Ouro.Lisp.Surface as S


-- The three ways a desugaring rule walks the surface tree. Each pass uses one.


-- A rule that must see a form before its parts are rewritten, because it depends on the
-- form's own shape: the rule rewrites the form, then its parts are walked
topDown :: (S.Expr -> S.Expr) -> S.Expr -> S.Expr
topDown rule = go
    where
    go expr = case rule expr of
        S.Form    pos items     -> S.Form    pos (map go items)
        S.Bracket pos items     -> S.Bracket pos (map go items)
        S.Tagged  pos tag inner -> S.Tagged  pos tag (go inner)
        leaf                    -> leaf


-- A rule that rewrites one form into another: a form's parts are rewritten first, then the
-- form itself
bottomUp :: (S.Expr -> S.Expr) -> S.Expr -> S.Expr
bottomUp rule = go
    where
    go = \case
        S.Form    pos items     -> rule (S.Form pos (map go items))
        S.Bracket pos items     -> S.Bracket pos (map go items)
        S.Tagged  pos tag inner -> S.Tagged  pos tag (go inner)
        leaf                    -> leaf


-- A rule over a list of siblings, which can turn one element into several: the elements of
-- every form and bracket, parts first. An expression that stands on its own (a graph's body,
-- a tag's payload, the whole tree) is a list of one; a rule that would turn it into several
-- has no meaning there, so it is left for the evaluator to report.
siblings :: ([S.Expr] -> [S.Expr]) -> S.Expr -> S.Expr
siblings rule = root
    where
    root expr = case rule [go expr] of
        [single]   -> single
        _expansion -> go expr

    go = \case
        S.Form    pos items     -> S.Form    pos (rule (map go items))
        S.Bracket pos items     -> S.Bracket pos (rule (map go items))
        S.Tagged  pos tag inner -> S.Tagged  pos tag (root inner)
        leaf                    -> leaf
