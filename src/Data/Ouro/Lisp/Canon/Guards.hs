module Data.Ouro.Lisp.Canon.Guards
( guardOperators
, guards
) where

import           Data.Ouro.Lisp.Canon.Connectives  (connectives)
import           Data.Ouro.Lisp.Canon.Placeholders (placeholders)
import           Data.Ouro.Lisp.Canon.Traverse     (topDown)
import qualified Data.Ouro.Lisp.Surface            as S
import           Data.Text                         (Text)


-- guards.
--
-- Turns the conditions of a case into (%when cond) heads, which the evaluator takes when
-- cond is true and skips when it is false:
--
--   ((when (has book ntp46)) e)   ((%when (has book ntp46)) e)
--   ((lt 10 20) e)                ((%when (lt %target 10 20)) e)
--   ((∈ uri) e)                   ((%when (%in %target #uri)) e)
--
-- A guard is about the case's target, which it takes first, as a '->' step takes the value
-- threaded into it: a comparison, or ∈, whose subject comes first. The case binds
-- its target once, to a name that cannot be written in Ouro, and the condition names it:
--
--   (case x ((lt 10) e) ...)   (let ((%bind "%target" x)) (case %target ((%when (lt %target 10)) e) ...))
--
-- A branch head is matched, not evaluated, so 'placeholders' and 'connectives' left it
-- alone; a condition is evaluated, so they are run on it here. A comparison takes two
-- operands, so a guard takes one: ((lt 10 20) e) is (lt %target 10 20), an arity error.
-- Top-down, after 'bindings', so the binding is written as bindings writes one; before
-- 'alternatives', which keeps a (%when ...) head.
guards :: S.Expr -> S.Expr
guards = topDown $ \case
    S.Form pos (cSym@(S.Symbol _ (S.formName -> "case")) : target : branches)
        | any comparesTarget branches
        -> S.Form pos [ S.Symbol pos "%let"
                      , S.Form pos [S.Form pos [S.Symbol pos "%bind", S.Literal pos (S.Str "%target"), target]]
                      , S.Form pos (cSym : S.Symbol pos "%target" : map condition branches)
                      ]
        | otherwise
        -> S.Form pos (cSym : target : map condition branches)
    other
        -> other

    where
    -- A branch whose head is a condition gets a (%when cond) head. A malformed (when ...)
    -- becomes the condition itself, which the evaluator reports as a misplaced when.
    condition = \case
        S.Form bPos [S.Form gPos [S.Symbol wPos "when", cond], body]
            -> S.Form bPos [S.Form gPos [S.Symbol wPos "%when", evaluated cond], body]
        S.Form bPos [whenForm@(S.Form gPos (S.Symbol wPos "when" : _)), body]
            -> S.Form bPos [S.Form gPos [S.Symbol wPos "%when", whenForm], body]
        S.Form bPos [S.Form gPos (opNode@(S.Symbol _ op) : operands), body]
            | S.formName op `elem` guardHeads
            -> S.Form bPos [ S.Form gPos [S.Symbol gPos "%when", evaluated (S.Form gPos (opNode : S.Symbol gPos "%target" : operands))]
                           , body ]
        branch
            -> branch

    comparesTarget = \case
        S.Form _ [S.Form _ (S.Symbol _ op : _), _] -> S.formName op `elem` guardHeads
        _notAGuard                                 -> False

    guardHeads = "in" : "satisfies" : guardOperators

    -- The passes a condition would have had, had it not been in a branch head
    evaluated = connectives . placeholders


-- The comparisons that can head a case guard
guardOperators :: [Text]
guardOperators = ["geq", "gt", "leq", "lt", "eq", "neq"]
