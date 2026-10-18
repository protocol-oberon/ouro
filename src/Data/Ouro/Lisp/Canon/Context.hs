module Data.Ouro.Lisp.Canon.Context
( contexts
) where

import           Data.Ouro.Lisp.Canon.Traverse (bottomUp)
import qualified Data.Ouro.Lisp.Surface        as S


-- contexts.
--
-- A record's context, ':context value', is its metadata rather than an attribute pair:
--
--   :context e    (%context e)
--   :@context e   (%context e)     JSON-LD's own name for it
--
-- The value is read as a JSON-LD context when the record is built. '%' cannot be written in
-- Ouro, so the head cannot be confused with a user's function.
contexts :: S.Expr -> S.Expr
contexts = bottomUp $ \case
    S.Form pos [S.Symbol _ "attr", S.Literal _ (S.Str key), valExpr]
        | key `elem` ["context", "@context"]
        -> S.Form pos [S.Symbol pos "%context", valExpr]
    other
        -> other
