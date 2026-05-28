
module Data.Ouro.Lisp.Eval.Schema where

import           Data.Function               ((&))
import           Data.Ouro.Error.Diagnostics (malformedTag, typeMismatch,
                                              typeMismatchBlurb, withBlurb)
import           Data.Ouro.Error.Types       (OuroError (..))
import qualified Data.Ouro.Internal.Expr     as I
import           Data.Ouro.Internal.Schema   (Schema (..), SchemaDirective (..))
import           Data.Ouro.Lisp.Eval.Types   (Value (..))
import qualified Data.Ouro.Lisp.Surface      as S
import           Data.Text                   (Text)
import qualified Text.URI                    as URI


-- parseContextDirectives.
--
-- Parses loose s-expression sequences found inside structural context macros.
-- Recursively unwraps structural blocks and accumulates separate directives into a combined state.
--
-- This function handles the initial metadata gathering pass for JSON-LD document construction.
-- Rather than acting as a generic evaluator, it strictly treats its input streams as semantic schema
-- configurations, sorting incoming tokens using distinct structural match passes:
--
--   1. Nested Form Unrolling: Enters unquoted inner sub-forms recursively, combining configurations
--      via their Monoid instances to support modular schema scoping.
--   2. Direct Property Mapping: Identifies raw keyword symbols and passes their associated values
--      to 'buildDirective', generating explicit 'SchemaDirective' entries.
--   3. Defensive Syntax Validation: Catch-all patterns isolate structural errors immediately via
--      'malformedTag', blocking uneven parameter streams or unexpected literals from propagating.
parseContextDirectives :: [S.Expr] -> Either OuroError Schema
parseContextDirectives =
    \case
     [] -> pure mempty
     (S.Form _ innerExprs : rest) -> do
                                     localSchema <- parseContextDirectives innerExprs
                                     nextSchema  <- parseContextDirectives rest
                                     pure (localSchema <> nextSchema)
     (S.Symbol _ key : valExpr : rest) -> do
                                          directive  <- buildDirective key valExpr
                                          nextSchema <- parseContextDirectives rest
                                          pure (Schema [directive] <> nextSchema)
     (badExpr : _)
         -> malformedTag "context" "Expected a Symbol-Value property pair layout structure."
            & withBlurb
                  ( "The structural schema parser found a malformed layout sequence inside the '#context' tag declaration.\n\n"
                 <> "A context block expects either an unquoted structural sub-form, or sequential key-value property pairs "
                 <> "written as pairs of bare symbols and values (e.g., :base \"https://schema.org/\").\n\n"
                 <> "Perhaps you have an uneven count of property attributes, or a loose literal object is sitting free within the payload structure?"
                  )
            & OuroError (S.exprPos badExpr)
            & Left


-- buildDirective.
--
-- Translates isolated keyword symbol names and values into specialized metadata directives.
-- Enforces type, shape, and syntax formatting rules on parameters mapped to core schema keys.
--
-- This worker provides atomic translation logic for the core vocabulary and base reference engines.
-- It isolates strings, tags, and symbols, ensuring they match explicit domain formats (like structured
-- URI schemes) before packing them into type-safe internal compiler instructions:
--
--   1. Semantic Vocab Mapping: Validates "vocab" keys, handling both explicit '#uri' tags with verified
--      schemes and raw fallback text strings.
--   2. Base Localization Setup: Unpacks and applies global text bases, checking constraints on raw forms.
--   3. Catch-All Constraint Checking: Validates language assertions and remote resources, rejecting
--      unknown directives with clear 'malformedTag' markers.
buildDirective :: Text -> S.Expr -> Either OuroError SchemaDirective
buildDirective key val =
    case key of
        "vocab" -> case val of
                       S.Tagged _ S.Uri (S.Literal _ (S.Str t)) ->
                           case URI.mkURI t of
                               Left _    -> typeMismatch "a String matching URI layout with a scheme included (https:)" "a String in an invalid URI format"
                                              & withBlurb (typeMismatchBlurb (Primitive (I.String t)))
                                              & OuroError (S.exprPos val)
                                              & Left
                               Right uri -> case URI.uriScheme uri of
                                                Just _  -> pure $ SetVocab (Left uri)
                                                Nothing -> typeMismatch "a String matching a URI layout with a scheme included (https:)" "a String in an invalid URI format"
                                                             & withBlurb (typeMismatchBlurb (Primitive (I.String t)))
                                                             & OuroError (S.exprPos val)
                                                             & Left

                       S.Literal _ (S.Str t) -> pure $ SetVocab (Right t)
                       _ -> typeMismatch "a text String or a resource #uri value" "an unsupported layout structure"
                              & withBlurb (typeMismatchBlurb (Primitive (I.String "")))
                              & OuroError (S.exprPos val)
                              & Left

        "base" -> case val of
                      S.Tagged _ S.Uri (S.Literal _ (S.Str t)) -> pure $ SetBase t
                      S.Literal _ (S.Str t)                    -> pure $ SetBase t
                      _ -> typeMismatch "a base path Text String or a structural #uri tag" "an unsupported value type"
                             & withBlurb (typeMismatchBlurb (Primitive (I.String "")))
                             & OuroError (S.exprPos val)
                             & Left

        "language" -> case val of
                          S.Literal _ (S.Str t) -> pure $ SetLanguage t
                          _                     -> typeMismatch "a language localization tag String (like \"en\" or \"fr\")" "an invalid node structure"
                                                     & withBlurb (typeMismatchBlurb (Primitive (I.String "")))
                                                     & OuroError (S.exprPos val)
                                                     & Left

        "remote-context" -> case val of
                                S.Tagged _ S.Uri (S.Literal _ (S.Str t)) ->
                                    case URI.mkURI t of
                                        Left _    -> typeMismatch "a String matching URI layout with a scheme included (https:)" "a String in an invalid URI format"
                                                       & withBlurb (typeMismatchBlurb (Primitive (I.String t)))
                                                       & OuroError (S.exprPos val)
                                                       & Left
                                        Right uri -> case URI.uriScheme uri of
                                                         Just _  -> pure $ RemoteContext uri
                                                         Nothing -> typeMismatch "a String matching a URI layout with a scheme included (https:)" "a String in an invalid URI format"
                                                                      & withBlurb (typeMismatchBlurb (Primitive (I.String t)))
                                                                      & OuroError (S.exprPos val)
                                                                      & Left

                                S.Literal _ (S.Str t) ->
                                    case URI.mkURI t of
                                        Left _    -> typeMismatch "a String matching URI layout with a scheme included (https:)" "a String in an invalid URI format"
                                                       & withBlurb (typeMismatchBlurb (Primitive (I.String t)))
                                                       & OuroError (S.exprPos val)
                                                       & Left
                                        Right uri -> case URI.uriScheme uri of
                                                         Just _  -> pure $ RemoteContext uri
                                                         Nothing -> typeMismatch "a String matching a URI layout with a scheme included (https:)" "a String in an invalid URI format"
                                                                      & withBlurb (typeMismatchBlurb (Primitive (I.String t)))
                                                                      & OuroError (S.exprPos val)
                                                                      & Left

                                _ -> typeMismatch "a direct metadata resource URL reference text" "an invalid structural expression shape"
                                       & OuroError (S.exprPos val)
                                       & Left

        _ -> malformedTag "context" ("Unknown or unsupported metadata declaration keyword: '" <> key <> "'.")
               & OuroError (S.exprPos val)
               & Left
