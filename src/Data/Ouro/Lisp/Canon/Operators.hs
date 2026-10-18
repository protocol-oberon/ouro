module Data.Ouro.Lisp.Canon.Operators
( operators
) where

import           Data.Ouro.Lisp.Canon.Traverse (bottomUp)
import qualified Data.Ouro.Lisp.Surface        as S


-- operators.
--
-- Unrolls variadic arithmetic into nested, left-associative binary operations:
--
--   (+ 1 2 3)   (+ (+ 1 2) 3)
operators :: S.Expr -> S.Expr
operators = bottomUp unroll
    where
    unroll = \case
        -- Base Case: Exactly two arguments. Return as-is
        S.Form pos (opNode@(S.Symbol _ opName) : a : b : [])
            | opName `elem` arithmeticOperators
                -> S.Form pos [opNode, a, b]

        -- Recursive Case: Three or more arguments. Left-associative fold.
        S.Form pos (opNode@(S.Symbol _ opName) : a : b : xs)
            | opName `elem` arithmeticOperators
                -> let inner = S.Form pos [opNode, a, b]
                   in unroll (S.Form pos (opNode : inner : xs))

        -- Pass-through for anything that isn't a variadic math operation
        otherVal
            -> otherVal

    arithmeticOperators = ["+", "-", "*", "/"]
