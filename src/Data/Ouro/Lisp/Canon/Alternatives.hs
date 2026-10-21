module Data.Ouro.Lisp.Canon.Alternatives
( alternatives
) where

import           Data.Ouro.Lisp.Canon.Traverse (bottomUp)
import qualified Data.Ouro.Lisp.Surface        as S


-- alternatives.
--
-- Expands a case branch headed by a form of literals into one branch per literal:
--
--   (case x (("Manet" "Proust") body) ...)   (case x ("Manet" body) ("Proust" body) ...)
--
-- After 'guards': a condition, (%when cond), is kept, and so are 'otherwise' and
-- destructuring patterns, which hold a '?' hole or a ':key pattern'.
alternatives :: S.Expr -> S.Expr
alternatives = bottomUp $ \case
    S.Form fPos (cSym@(S.Symbol _ "case") : target : branches)
        -> S.Form fPos (cSym : target : concatMap expandBranch branches)
    other
        -> other

    where
    expandBranch :: S.Expr -> [S.Expr]
    expandBranch = \case
        S.Form bPos [pattern, body]
            -> case pattern of
                   -- Keep conditions intact, written by 'guards'
                   S.Form _ (S.Symbol _ "%when" : _)
                       -> [S.Form bPos [pattern, body]]

                   -- Keep 'otherwise' intact
                   S.Symbol _ "otherwise"
                       -> [S.Form bPos [pattern, body]]

                   -- Keep destructuring patterns intact, e.g. ((?head ?..rest) body)
                   _ | S.isPatternForm pattern
                       -> [S.Form bPos [pattern, body]]

                   -- One branch per alternative: (("Manet" "Proust") body) -> ("Manet" body) ("Proust" body)
                   S.Form _ subPatterns
                       -> [S.Form bPos [subPat, body] | subPat <- subPatterns]

                   -- Keep single literals or undefined symbols intact
                   _singleLitOrSym
                       -> [S.Form bPos [pattern, body]]

        -- A malformed branch is passed through unchanged, so that 'evaluateGuards' can report it
        -- with its position
        malformed
            -> [malformed]
