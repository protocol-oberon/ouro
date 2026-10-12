
module Data.Ouro.Error.Linter where

import           Data.Ouro.Error.Diagnostics (unanchoredLiteral)
import           Data.Ouro.Error.Types       (OuroWarning (..))
import qualified Data.Ouro.Lisp.Tokens       as Tkn


-- Recursive Check tokens for dead layout patterns
lintExpression :: [Tkn.Token] -> [OuroWarning]
lintExpression =
    \case
     -- MATCH: UNANCHORED OBJECT LITERALS
     (Tkn.Token pos Tkn.TagRecordEmpty : Tkn.Token _ (Tkn.String val) : rest)
         -> OuroWarning pos (unanchoredLiteral "#obj-empty" val) : lintExpression rest

     -- MATCH: UNANCHORED ARRAY LITERALS
     (Tkn.Token pos Tkn.TagArrEmpty : Tkn.Token _ (Tkn.String val) : rest)
         -> OuroWarning pos (unanchoredLiteral "#arr-empty" val) : lintExpression rest

     -- Structural recursion to keep scanning down the token stream
     (_ : rest) -> lintExpression rest
     []         -> []
