module Data.Ouro.Lisp.Canon.Thru
( thru
) where

import           Data.Ouro.Lisp.Canon.Traverse (siblings)
import qualified Data.Ouro.Lisp.Surface        as S
import           Text.Megaparsec               (SourcePos)


-- thru.
--
-- Extends a duration to the end of its unit, as the durations it stands for:
--
--   (thru (days n))     (days n) (hours 23) (minutes 59) (seconds 59)
--   (thru (years n))    (years-end n)        resolved against the date at run time
--
-- One element becomes several siblings, so this is the one pass over lists of siblings. A
-- bare (thru ...) standing on its own has no meaning, and is left for the evaluator to report.
thru :: S.Expr -> S.Expr
thru = siblings expand
    where
    expand = \case
        S.Form tPos [S.Symbol _ "thru", durationExpr] : rest -> thruOf tPos durationExpr ++ expand rest
        item : rest                                          -> item : expand rest
        []                                                   -> []


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
