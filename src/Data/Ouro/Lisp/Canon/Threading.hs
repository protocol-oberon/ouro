
module Data.Ouro.Lisp.Canon.Threading
( threading
) where

import qualified Data.Ouro.Lisp.Surface as S


-- threading.
--
-- (-> x step ...) threads a value through its steps, each step receiving the value before:
--
--   (-> x (f a) (g b))   (g (f x a) b)       a step's head is called with the value first
--   (-> x f)             (f x)               a bare name is called with the value
--   (-> x (g b ?))       (%apply (fn (%1) (g b %1)) x)
--                                            a step that is a function, as a placeholder is,
--                                            is called with the value
--
-- Runs after 'placeholders', so (g b ?) is already a fn, and (-> ? step ...) is already a
-- function of the value it threads. Runs before 'construct', top-down, so a step is rewritten
-- before it is desugared: in (-> x (with :k v)), 'with' sees x as its base. '%apply' cannot be
-- written in Ouro: a form headed by a fn is an array, so the evaluator applies it instead.
threading :: S.Expr -> S.Expr
threading = \case
    S.Form _ (S.Symbol _ "->" : start : steps)
        -> threading (foldl thread start steps)

    S.Form    pos items     -> S.Form    pos (map threading items)
    S.Bracket pos items     -> S.Bracket pos (map threading items)
    S.Tagged  pos tag inner -> S.Tagged  pos tag (threading inner)
    leaf                    -> leaf

    where
    thread value step = case step of
        S.Form pos (S.Symbol _ "fn" : _)            -> S.Form pos [S.Symbol pos "%apply", step, value]
        S.Form pos (headExpr@(S.Symbol _ _) : args) -> S.Form pos (headExpr : value : args)
        S.Symbol pos _                              -> S.Form pos [step, value]
        other                                       -> S.Form (S.exprPos other) [S.Symbol (S.exprPos other) "%apply", other, value]
