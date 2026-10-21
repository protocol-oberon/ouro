module Data.Ouro.Lisp.Canon.Comparisons
( comparisons
) where

import           Data.Ouro.Lisp.Canon.Connectives (connectives)
import           Data.Ouro.Lisp.Canon.Traverse    (bottomUp)
import qualified Data.Ouro.Lisp.Surface           as S


-- comparisons.
--
-- Unrolls a variadic comparison into an 'and' of its neighbouring pairs:
--
--   (< a b c)    (and (< a b) (< b c))
--
-- so a chain stops at the first pair that is false, as and does. 'connectives' has already
-- run, so the and is desugared here, into the same chain of cases a written and becomes.
-- After 'guards', so a case guard, by then the condition (%when (< %target 10 20)), is
-- chained like any other comparison.
comparisons :: S.Expr -> S.Expr
comparisons = bottomUp unroll
    where
    unroll = \case
        -- Three or more operands: one comparison per neighbouring pair, all of which must hold
        S.Form pos (opNode@(S.Symbol sPos opName) : operands@(_ : _ : _ : _))
            | opName `elem` comparisonOperators
                -> let pairs = zipWith (\a b -> S.Form pos [opNode, a, b]) operands (drop 1 operands)
                   in connectives (S.Form pos (S.Symbol sPos "and" : pairs))

        -- Two operands, or anything that isn't a comparison
        otherVal
            -> otherVal

    comparisonOperators = ["eq", ">", "<", ">=", "<="]
