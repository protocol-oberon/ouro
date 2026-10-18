module Data.Ouro.Lisp.Canon.Comparisons
( comparisons
) where

import           Data.Ouro.Lisp.Canon.Traverse (bottomUp)
import qualified Data.Ouro.Lisp.Surface        as S


-- comparisons.
--
-- Unrolls variadic comparisons into nested binary 'and' checks:
--
--   (eq 1 2 3)   (%and (eq 1 2) (eq 2 3))
--
-- '%and' is an internal builtin: '%' cannot be lexed in user symbols or keys, so the
-- desugared form can never be shadowed by a user binding. After 'guards', so a case guard,
-- marked (%guard < 10 20), is not unrolled as a comparison of its own operands.
comparisons :: S.Expr -> S.Expr
comparisons = bottomUp unroll
    where
    unroll = \case
        -- Base Case: Exactly two arguments. Return as-is.
        S.Form pos (opNode@(S.Symbol _ opName) : a : b : [])
            | opName `elem` comparisonOperators
                -> S.Form pos [opNode, a, b]

        -- Recursive Case: Three or more arguments. Chain them.
        S.Form pos (opNode@(S.Symbol sPos opName) : a : b : xs)
            | opName `elem` comparisonOperators
                -> let leftCheck  = S.Form pos [opNode, a, b]
                       rightCheck = unroll (S.Form pos (opNode : b : xs))
                   in S.Form pos [S.Symbol sPos "%and", leftCheck, rightCheck]

        -- Pass-through for anything that isn't a comparison
        otherVal
            -> otherVal

    comparisonOperators = ["eq", ">", "<", ">=", "<="]
