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
--   ((< 10 20) e)                 ((%when (< %target 10 20)) e)
--   ((satisfies #uri) e)          ((%when (satisfies #uri %target)) e)
--
-- A comparison guard, and a property on its own, are about the case's target, so the case
-- binds its target once, to a name that cannot be written in Ouro, and the condition names it:
--
--   (case x ((< 10) e) ...)   (let ((%bind "%target" x)) (case %target ((%when (< %target 10)) e) ...))
--
-- A branch head is matched, not evaluated, so 'placeholders' and 'connectives' left it
-- alone; a condition is evaluated, so they are run on it here. 'comparisons' runs later and
-- chains (< %target 10 20) into an and that stops at the first false pair. Top-down, after
-- 'bindings', so the binding is written as bindings writes one; before 'alternatives',
-- which keeps a (%when ...) head.
guards :: S.Expr -> S.Expr
guards = topDown $ \case
    S.Form pos (cSym@(S.Symbol _ "case") : target : branches)
        | any comparesTarget branches
        -> S.Form pos [ S.Symbol pos "let"
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
            | op `elem` guardOperators
            -> S.Form bPos [ S.Form gPos [S.Symbol gPos "%when", evaluated (S.Form gPos (opNode : S.Symbol gPos "%target" : operands))]
                           , body ]
        S.Form bPos [S.Form gPos [sSym@(S.Symbol _ "satisfies"), S.Tagged tPos tag (S.Hole _ "%subject")], body]
            -> S.Form bPos [ S.Form gPos [S.Symbol gPos "%when", S.Form gPos [sSym, S.Tagged tPos tag (S.Symbol tPos "%target")]]
                           , body ]
        branch
            -> branch

    comparesTarget = \case
        S.Form _ [S.Form _ [S.Symbol _ "satisfies", S.Tagged _ _ (S.Hole _ "%subject")], _] -> True
        S.Form _ [S.Form _ (S.Symbol _ op : _), _]                                          -> op `elem` guardOperators
        _notAGuard                                                                          -> False

    -- The passes a condition would have had, had it not been in a branch head
    evaluated = connectives . placeholders


-- The comparisons that can head a case guard
guardOperators :: [Text]
guardOperators = [">=", ">", "<=", "<", "eq", "neq"]
