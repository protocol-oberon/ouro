module Data.Ouro.Lisp.Canon.Thru
( thru
) where

import           Data.Ouro.Lisp.Canon.Traverse (bottomUp, siblings)
import qualified Data.Ouro.Lisp.Surface        as S
import           Text.Megaparsec               (SourcePos)


-- thru.
--
-- Extends a duration to the end of its unit, as the durations it stands for:
--
--   (thru (days n))     (days n) (hours 23) (minutes 59) (seconds 59)
--   (thru (years n))    (years-end n)        resolved against the date at run time
--
-- shift, + and - take two operands, so a date shifted by a thru is shifted by each duration
-- in turn, the operation nested once for each:
--
--   (+ date (thru (days n)))   (+ (+ (+ (+ date (days n)) (hours 23)) (minutes 59)) (seconds 59))
--
-- Anywhere else one element becomes several siblings, the one pass over lists of siblings.
-- A bare (thru ...) standing on its own has no meaning, and is left for the evaluator to report.
thru :: S.Expr -> S.Expr
thru = siblings expand . bottomUp shift
    where
    expand = \case
        S.Form tPos [S.Symbol _ "thru", durationExpr] : rest -> thruOf tPos durationExpr ++ expand rest
        item : rest                                          -> item : expand rest
        []                                                   -> []

    -- A date and a thru, as the operands of shift, + or -: the duration added, or taken, one
    -- by one. A Duration may come first in +, so a thru written first is added from the right.
    -- shift takes a thru forwards only: a thru's parts are positive, and a (days n) of a
    -- negative n is not known to be one until it is evaluated.
    shift = \case
        S.Form pos [opSym@(S.Symbol _ op), date, S.Form tPos [S.Symbol _ "thru", durationExpr]]
            | op `elem` ["shift", "+", "-"]
            -> foldl (\shifted d -> S.Form pos [opSym, shifted, d]) date (thruOf tPos durationExpr)
        S.Form pos [opSym@(S.Symbol _ "+"), S.Form tPos [S.Symbol _ "thru", durationExpr], date]
            -> foldr (\d shifted -> S.Form pos [opSym, d, shifted]) date (thruOf tPos durationExpr)
        other
            -> other


thruOf :: SourcePos -> S.Expr -> [S.Expr]
thruOf pos target = case target of
    -- Calendar Units: Need Runtime Checks.
    S.Form _pos [S.Symbol _ "years", amt]  -> [ S.Form pos [S.Symbol pos "years-end", amt] ]
    S.Form _pos [S.Symbol _ "months", amt] -> [ S.Form pos [S.Symbol pos "months-end", amt] ]

    -- Absolute Units: Can be statically expanded.
    S.Form _pos [S.Symbol _ "days", _amt]
        -> [ target
           , mkDur "hours" 23
           , mkDur "minutes" 59
           , mkDur "seconds" 59
           ]

    S.Form _pos [S.Symbol _ "hours", _amt]
        -> [ target
           , mkDur "minutes" 59
           , mkDur "seconds" 59
           ]

    S.Form _pos [S.Symbol _ "minutes", _amt]
        -> [ target
           , mkDur "seconds" 59
           ]

    -- Catch malformed syntax and leave it for the runtime type-checker
    other -> [other]

    where
    mkDur unit val = S.Form pos [ S.Symbol pos unit, S.Literal pos (S.Num val) ]
