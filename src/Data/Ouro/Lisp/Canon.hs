
module Data.Ouro.Lisp.Canon where

import qualified Data.Ouro.Lisp.Surface as S


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

           -- 1. Catch an explicit wrapper block whose only child is an inline context sugar token
           (S.Form fPos [S.Attr aPos "context", valExpr] : rest)
               -> let directive     = S.Form aPos [S.Symbol aPos "remote-context", valExpr]
                      dirWrapper    = S.Form aPos [directive]
                      canonicalForm = S.Form aPos [S.Symbol aPos "context", dirWrapper]
                  in canonicalForm : desugar rest

            -- 2. Catch a raw standalone attribute context sugar sequence at this layout level
           (S.Attr pos "context" : valExpr : rest)
               -> let directive     = S.Form pos [S.Symbol pos "remote-context", valExpr]
                      dirWrapper    = S.Form pos [directive]
                      canonicalForm = S.Form pos [S.Symbol pos "context", dirWrapper]
                  in canonicalForm : desugar rest

            -- 3. Match a Lisp sub-object block starting with an explicit context form
           (S.Form pos (S.Form cPos (S.Symbol sPos "context" : directives) : innerItems) : xs)
               -> let desugaredInner = S.Form cPos (S.Symbol sPos "context" : desugar directives) : desugar innerItems
                      canonicalBlock = S.Form pos desugaredInner
                  in canonicalBlock : desugar xs

            -- 4. Deep Recurse: Normal nested list traversal pass
           (S.Form pos items : xs) -> S.Form pos (desugar items) : desugar xs

            -- 5. Horizontal Pass-through
           (x : xs) -> x : desugar xs
