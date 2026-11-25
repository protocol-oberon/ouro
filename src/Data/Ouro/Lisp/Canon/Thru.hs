module Data.Ouro.Lisp.Canon.Thru
( thru
) where

import           Data.Ouro.Lisp.Canon.Traverse (bottomUp)
import qualified Data.Ouro.Lisp.Surface        as S


-- thru.
--
-- The old spelling of a shift through the end of a unit, read as the operation that says it:
--
--   (+ date (thru u))       (shift/forward-through date u)
--   (+ (thru u) date)       (shift/forward-through date u)
--   (shift date (thru u))   (shift/forward-through date u)
--   (- date (thru u))       (shift/backward-through date u)
--
-- Forward, a date moves by the duration and on to the last second of its largest unit, as
-- end_of_the_end means; backward, to that unit's first second. A thru anywhere else has no
-- meaning, and is left for the evaluator to report. Each is warned of where it is written.
thru :: S.Expr -> S.Expr
thru = bottomUp through
    where
    through = \case
        S.Form pos [S.Symbol _ op, date, S.Form _ [S.Symbol _ "thru", duration]]
            | op `elem` ["+", "shift"]
            -> S.Form pos [S.Symbol pos "shift/forward-through", date, duration]
        S.Form pos [S.Symbol _ "+", S.Form _ [S.Symbol _ "thru", duration], date]
            -> S.Form pos [S.Symbol pos "shift/forward-through", date, duration]
        S.Form pos [S.Symbol _ "-", date, S.Form _ [S.Symbol _ "thru", duration]]
            -> S.Form pos [S.Symbol pos "shift/backward-through", date, duration]
        other
            -> other
