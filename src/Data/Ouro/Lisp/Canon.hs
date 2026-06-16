
module Data.Ouro.Lisp.Canon where

import qualified Data.Ouro.Lisp.Surface as S
import Text.Megaparsec (SourcePos)


-- Main API entry point for the pipeline
construct :: S.Expr -> S.Expr
construct = \case
             S.Form    r exprs -> S.Form    r (desugar (map construct exprs))
             S.Bracket r exprs -> S.Bracket r (desugar (map construct exprs))
             leaf              -> leaf


-- Processes flat lists horizontally to rewrite syntax sugar
desugar :: [S.Expr] -> [S.Expr]
desugar = \case
           [] -> []

           -- Catch an explicit wrapper block whose only child is an inline context sugar token
           (S.Form _pos [S.Attr aPos "context", valExpr] : rest)
               -> let directive     = S.Form aPos [S.Symbol aPos "remote-context", valExpr]
                      dirWrapper    = S.Form aPos [directive]
                      canonicalForm = S.Form aPos [S.Symbol aPos "context", dirWrapper]
                  in canonicalForm : desugar rest

            -- Catch a raw standalone attribute context sugar sequence at this layout level
           (S.Attr pos "context" : valExpr : rest)
               -> let directive     = S.Form pos [S.Symbol pos "remote-context", valExpr]
                      dirWrapper    = S.Form pos [directive]
                      canonicalForm = S.Form pos [S.Symbol pos "context", dirWrapper]
                  in canonicalForm : desugar rest

            -- Match a Lisp sub-object block starting with an explicit context form
           (S.Form pos (S.Form cPos (S.Symbol sPos "context" : directives) : innerItems) : xs)
               -> let desugaredInner = S.Form cPos (S.Symbol sPos "context" : desugar directives) : desugar innerItems
                      canonicalBlock = S.Form pos desugaredInner
                  in canonicalBlock : desugar xs

           -- Desugar end-of to max temporal duration for given duration type
           (S.Form tPos [S.Symbol _ "thru", durationExpr] : rest)
               -> desugarThru tPos durationExpr ++ desugar rest

           -- Deep Recurse: Bottom-Up evaluation pass
           (S.Form pos items : xs)
               -> let desugaredItems = desugar items  -- 1. Expand 'thru' and desugar children FIRST
                      unrolled       = desugarBinOp (desugarComparison (S.Form pos desugaredItems)) -- 2. Unroll variadics
                  in unrolled : desugar xs

           -- Horizontal Pass-through (Safely passes over Tags, Symbols, and Literals)
           (x : xs) -> x : desugar xs


-- Unrolls variadic comparisons into nested binary 'and' checks.
-- E.g., (eq 1 2 3) -> (eq (eq 1 2) (eq 2 3))
desugarComparison :: S.Expr -> S.Expr
desugarComparison =
    \case
     -- Base Case: Exactly two arguments. Return as-is.
     S.Form pos (opNode@(S.Symbol _ opName) : a : b : [])
         | opName `elem` ["eq", ">", "<", ">=", "<="]
             -> S.Form pos [opNode, a, b]

     -- Recursive Case: Three or more arguments. Chain them.
     S.Form pos (opNode@(S.Symbol sPos opName) : a : b : xs)
         | opName `elem` ["eq", ">", "<", ">=", "<="]
             -> let leftCheck  = S.Form pos [opNode, a, b]
                    rightCheck = desugarComparison (S.Form pos (opNode : b : xs))
                in S.Form pos [S.Symbol sPos "eq", leftCheck, rightCheck]

     -- Pass-through for anything that isn't a comparison
     otherVal
         -> otherVal


-- Unrolls variadic arithmetic into nested, left-associative binary operations.
-- E.g., (+ 1 2 3) -> (+ (+ 1 2) 3)
desugarBinOp :: S.Expr -> S.Expr
desugarBinOp =
    \case
     -- Base Case: Exactly two arguments. Return as-is
     S.Form pos (opNode@(S.Symbol _ opName) : a : b : [])
         | opName `elem` ["+", "-", "*", "/"]
             -> S.Form pos [opNode, a, b]

     -- Recursive Case: Three or more arguments. Left-associative fold.
     S.Form pos (opNode@(S.Symbol _ opName) : a : b : xs)
         | opName `elem` ["+", "-", "*", "/"]
             -> let inner = S.Form pos [opNode, a, b]
                in desugarBinOp (S.Form pos (opNode : inner : xs))

     -- Pass-through for anything that isn't a variadic math operation
     otherVal
         -> otherVal


desugarThru :: SourcePos -> S.Expr -> [S.Expr]
desugarThru pos target = case target of
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
