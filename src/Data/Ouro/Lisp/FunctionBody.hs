module Data.Ouro.Lisp.FunctionBody
( functionBody
) where

import           Data.Function               ((&))
import           Data.Ouro.Error.Diagnostics (lexicalError, withBlurb)
import           Data.Ouro.Error.Types       (OuroError (..))
import qualified Data.Ouro.Lisp.Surface      as S
import           Data.Text                   (Text)
import           Text.Megaparsec             (SourcePos)


-- functionBody.
--
-- Splits a function body into its declarations and its result. A body is any number
-- of 'define' and 'defun' declarations followed by exactly one expression, written
-- exactly as it would be anywhere else; its value is the function's result:
--
--   (defun getty! () "http://vocab.getty.edu/aat/")         ; a literal
--   (defun person! (who) (define :t "Person")                 ; a record
--                        (:type t :_label who))
--
-- Checked when a function is defined, so a malformed function is reported even if it
-- is never called.
functionBody :: Text -> SourcePos -> [S.Expr] -> Either OuroError ([S.Expr], S.Expr)
functionBody name pos body
    -- A ':key value' (or ':context') at the root of the body is a spliced record
    | splicedField : _ <- filter isSplicedField body
        = lexicalError ("The function '" <> name <> "' has a ':key value' field directly in its body.")
          & withBlurb ( "A function's body is one result expression, written as it would be anywhere else. "
                     <> "Perhaps wrap the record in parentheses: (defun " <> name <> " (args) (:key value ...))."
                      )
          & OuroError (S.exprPos splicedField)
          & Left

    | returnForm : _ <- filter isReturn body
        = lexicalError "'return' is no longer needed: a function's last expression is its result."
          & withBlurb "Perhaps write the value directly, e.g. (defun id! (x) x) instead of (defun id! (x) (return x))."
          & OuroError (S.exprPos returnForm)
          & Left

    | otherwise
        = case span isDeclaration body of
              (declarations, [result])
                  -> Right (declarations, result)

              (_, [])
                  -> lexicalError ("The function '" <> name <> "' has no result expression.")
                     & withBlurb "A function's body is any number of defines followed by exactly one expression: its result."
                     & OuroError pos
                     & Left

              (_, _result : extra : _)
                  | isDeclaration extra
                  -> lexicalError "Definitions must come before the function's result expression."
                     & withBlurb "A function's body is any number of defines followed by exactly one expression: its result."
                     & OuroError (S.exprPos extra)
                     & Left

                  | otherwise
                  -> lexicalError ("The function '" <> name <> "' has more than one result expression.")
                     & withBlurb "A function returns one value. Perhaps wrap the values in parentheses to return an Array: (1 2) instead of 1 2."
                     & OuroError (S.exprPos extra)
                     & Left

    where
    isDeclaration = \case
                     S.Form _ (S.Symbol _ "define" : _) -> True
                     S.Form _ (S.Symbol _ "defun"  : _) -> True
                     _notADeclaration                   -> False

    isSplicedField = \case
                      S.Form _ (S.Symbol _ "attr"    : _) -> True
                      S.Form _ (S.Symbol _ "context" : _) -> True
                      _notAField                          -> False

    isReturn = \case
                S.Form _ (S.Symbol _ "return" : _) -> True
                _notAReturn                        -> False
