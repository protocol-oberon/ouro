module Data.Ouro.Lisp.Canon.Bindings
( bindings
) where

import           Data.Ouro.Lisp.Canon.Traverse (topDown)
import qualified Data.Ouro.Lisp.Surface        as S


-- bindings.
--
-- Pairs each '$name' in a let's binding list with the value after it:
--
--   (let ($a 1 $b 2) body)   (let ((%bind "a" 1) (%bind "b" 2)) body)
--
-- Top-down, before 'thru', so a value that expands into several elements stays inside its
-- binding. A name with no value after it is left as (%bind "name"), which the evaluator
-- reports; a '$name' anywhere else stays (%binder "name"), reported where it is evaluated.
bindings :: S.Expr -> S.Expr
bindings = topDown $ \case
    S.Form pos (letSym@(S.Symbol _ "let") : S.Form lPos binders : rest)
        -> S.Form pos (letSym : S.Form lPos (pairUp binders) : rest)
    other
        -> other

    where
    pairUp = \case
        S.Form bPos [S.Symbol sPos "%binder", name] : value : more
            | not (isBinder value)
            -> S.Form bPos [S.Symbol sPos "%bind", name, value] : pairUp more
        S.Form bPos [S.Symbol sPos "%binder", name] : more
            -> S.Form bPos [S.Symbol sPos "%bind", name] : pairUp more
        item : more
            -> item : pairUp more
        []
            -> []

    isBinder = \case
        S.Form _ (S.Symbol _ "%binder" : _) -> True
        _notABinder                         -> False
