{-# OPTIONS_GHC -Wno-unused-imports #-}

module Data.Ouro.Lisp.Eval.Schema where

import           Control.Monad.Reader        (Reader)
import           Data.Function               ((&))
import           Data.Ouro.Error.Diagnostics (malformedTag, typeMismatch,
                                              typeMismatchBlurb, withBlurb)
import           Data.Ouro.Error.Types       (ErrorContext (..), OuroError (..),
                                              SyntaxError (..))
import qualified Data.Ouro.Internal.Expr     as I
import           Data.Ouro.Internal.Schema   (Schema (..), SchemaDirective (..))
import           Data.Ouro.Lisp.Eval.Types   (Env, Expr (..))
import qualified Data.Ouro.Lisp.Eval.Types   as L
import qualified Data.Ouro.Lisp.Surface      as S
import           Data.Text                   (Text)
import qualified Text.URI                    as URI


-- parseContextDirectives.
--
-- Parses structural sequences. Operates within the Reader monad, returning
-- a 'Schema' or an 'EvalError' wrapped in the resulting 'L.Expr' or
-- propagated via the monadic structure.
parseContextDirectives :: [S.Expr] -> Reader Env L.Expr
parseContextDirectives =
    \case
        [] -> pure $ SchemaVal mempty

        (S.Form pos innerExprs : rest)
            -> do
               localRes <- parseContextDirectives innerExprs
               nextRes  <- parseContextDirectives rest
               case (localRes, nextRes) of
                   (SchemaVal l, SchemaVal n) -> pure $ SchemaVal (l <> n)
                   (EvalError e, _)           -> pure $ EvalError e
                   (_, EvalError e)           -> pure $ EvalError e
                   _                          -> pure $ EvalError $ OuroError pos $ malformedTag "context" "Internal schema structure error."

        (S.Symbol _ key : valExpr : rest)
            -> do
               directiveRes <- buildDirective key valExpr
               nextRes      <- parseContextDirectives rest
               case (directiveRes, nextRes) of
                   (Directive d, SchemaVal n) -> pure $ SchemaVal (Schema [d] <> n)
                   (EvalError e, _)           -> pure $ EvalError e
                   (_, EvalError e)           -> pure $ EvalError e
                   _                          -> pure $ EvalError $ OuroError (S.exprPos valExpr) $ malformedTag "context" "Directive parsing error."

        (badExpr : _) ->
            pure $ EvalError $ OuroError (S.exprPos badExpr) $ malformedTag "context" "Expected a Symbol-L.Expr property pair layout structure."
                & withBlurb ( "The structural schema parser found a malformed layout sequence inside the '#context' tag declaration.\n\n"
                           <> "A context block expects either an unquoted structural sub-form, or sequential key-value property pairs "
                           <> "written as pairs of bare symbols and values (e.g., :base \"https://schema.org/\").\n\n"
                           <> "Perhaps you have an uneven count of property attributes, or a loose literal object is sitting free within the payload structure?"
                            )


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
-- Maps structural symbols to SchemaDirective entries.
-- Operates within the Reader monad to enable environment-aware directive construction.
buildDirective :: Text -> S.Expr -> Reader Env L.Expr
buildDirective key val = case key of
    "vocab" -> case val of
        S.Tagged  _ S.Uri (S.Literal _ (S.Str t)) -> validateUri t
        S.Literal _ (S.Str t)                     -> pure $ Directive $ SetVocab (Right t)
        _ -> pure $ EvalError $ typeMismatch "a text String or a resource #uri value"
                                             "an unsupported layout structure"
                                  & withBlurb (typeMismatchBlurb (Primitive (I.String "")))
                                  & OuroError (S.exprPos val)

    "base" -> case val of
        S.Tagged  _ S.Uri (S.Literal _ (S.Str t)) -> pure $ Directive $ SetBase t
        S.Literal _ (S.Str t)                     -> pure $ Directive $ SetBase t
        _ -> pure $ EvalError $ typeMismatch "a base path Text String or a structural #uri tag"
                                             "an unsupported value type"
                                  & withBlurb (typeMismatchBlurb (Primitive (I.String "")))
                                  & OuroError (S.exprPos val)

    "language" -> case val of
        S.Literal _ (S.Str t) -> pure $ Directive $ SetLanguage t
        _ -> pure $ EvalError $ typeMismatch "a language localization tag String (like \"en\" or \"fr\")"
                                             "an invalid node structure"
                                  & withBlurb (typeMismatchBlurb (Primitive (I.String "")))
                                  & OuroError (S.exprPos val)

    "remote-context" -> case val of
        S.Tagged  _ S.Uri (S.Literal _ (S.Str t)) -> validateRemote t
        S.Literal _ (S.Str t)                     -> validateRemote t
        _ -> pure $ EvalError $ typeMismatch "a direct metadata resource URL reference text"
                                             "an invalid structural expression shape"
                                  & OuroError (S.exprPos val)

    _ -> pure $ EvalError $ malformedTag "context" ("Unknown or unsupported metadata declaration keyword: '" <> key <> "'.")
                               & OuroError (S.exprPos val)

    where
    validateUri :: Text -> Reader Env L.Expr
    validateUri t = case URI.mkURI t of
        Right uri | Just _ <- URI.uriScheme uri -> pure $ Directive $ SetVocab (Left uri)
        _ -> pure $ EvalError $ typeMismatch "a String matching URI layout with a scheme included (https:)"
                                             "a String in an invalid URI format"
                                  & withBlurb (typeMismatchBlurb (Primitive (I.String t)))
                                  & OuroError (S.exprPos val)

    validateRemote :: Text -> Reader Env L.Expr
    validateRemote t = case URI.mkURI t of
        Right uri | Just _ <- URI.uriScheme uri -> pure $ Directive $ RemoteContext uri
        _ -> pure $ EvalError $ typeMismatch "a String matching URI layout with a scheme included (https:)"
                                             "a String in an invalid URI format"
                                  & withBlurb (typeMismatchBlurb (Primitive (I.String t)))
                                  & OuroError (S.exprPos val)
