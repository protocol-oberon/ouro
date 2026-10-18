module Data.Ouro.Lisp.Canon.Guards
( guardOperators
, guards
) where

import           Data.Ouro.Lisp.Canon.Traverse (topDown)
import qualified Data.Ouro.Lisp.Surface        as S
import           Data.Text                     (Text)


-- guards.
--
-- Marks the guards of a case. A guard such as ((< 10 20) body) compares the case's target
-- with its operands, (< target 10 20), so it must not be desugared as a comparison of its own
-- operands. The head becomes (%guard < 10 20); '%' cannot be written in Ouro, so the marker
-- cannot be shadowed. Top-down, before 'comparisons' and 'alternatives', which leave a
-- marked guard alone.
guards :: S.Expr -> S.Expr
guards = topDown $ \case
    S.Form pos (cSym@(S.Symbol _ "case") : target : branches)
        -> S.Form pos (cSym : target : map markBranch branches)
    other
        -> other

    where
    markBranch = \case
        S.Form bPos [S.Form gPos (opNode@(S.Symbol sPos op) : operands), body]
            | op `elem` guardOperators
            -> S.Form bPos [S.Form gPos (S.Symbol sPos "%guard" : opNode : operands), body]
        branch
            -> branch


-- The comparisons that can head a case guard
guardOperators :: [Text]
guardOperators = [">=", ">", "<=", "<", "eq", "neq"]
