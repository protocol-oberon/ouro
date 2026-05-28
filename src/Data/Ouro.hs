{-# LANGUAGE DataKinds #-}

module Data.Ouro
( -- * Core Compilation Pipeline
compile
, validate
, defaultOptions
, PrinterOptions(..)
, Ser.toJSON

-- * Core Execution Error Types
, OuroError(..)
, ErrorContext(..)
, SyntaxError(..)
, TypeError(..)
, PathError(..)
, ScopeError(..)
, InternalError(..)
, smartErrorCode

-- * Unified Diagnostic and Warning Subsystems
, OuroDiagnostic(..)
, OuroWarning(..)
, WarningContext(..)
, LinterWarning(..)
, SemanticWarning(..)
, VocabularyWarning(..)
, smartWarningCode
, warningBlurb
, warningSummary
) where

import           Data.Ouro.Error.Diagnostics (smartErrorCode, smartWarningCode,
                                              warningBlurb, warningSummary)
import qualified Data.Ouro.Error.Linter      as LN
import           Data.Ouro.Error.Types       (ErrorContext (..),
                                              InternalError (..),
                                              LinterWarning (..),
                                              OuroDiagnostic (..),
                                              OuroError (..), OuroWarning (..),
                                              PathError (..), ScopeError (..),
                                              SemanticWarning (..),
                                              SyntaxError (..), TypeError (..),
                                              VocabularyWarning (..),
                                              WarningContext (..))
import qualified Data.Ouro.Internal.Expr     as I
import qualified Data.Ouro.Internal.Kinds    as JLD
import qualified Data.Ouro.Json.Parser       as JP
import           Data.Ouro.Json.Serializer   (PrinterOptions (..),
                                              defaultOptions)
import qualified Data.Ouro.Json.Serializer   as Ser
import qualified Data.Ouro.Lisp.Canon        as Canon
import qualified Data.Ouro.Lisp.Eval.Engine  as EN
import qualified Data.Ouro.Lisp.Lexer        as LX
import qualified Data.Ouro.Lisp.Parser       as LP
import           Data.Text                   (Text)
import qualified Data.Text.Lazy              as TL
import           Text.Megaparsec             (errorBundlePretty)


-- Complete frontend pipeline compilation pass.
-- Returns either a fatal OuroError, or a successful tuple containing
-- all collected warnings along with the compiled expressions.
compile :: String -> Text -> Either OuroError ([OuroDiagnostic], I.Expr 'JLD.Primitive)
compile filename input = do
                         -- 1. Lexical Pass
                         tokens <- LX.tokenize filename input

                         -- 1.5 Pure Warning Harvesting Pass
                         -- We map our linter's [OuroWarning] variants into the unified wrapper array.
                         let warnings     = LN.lintExpression tokens
                             diagWarnings = map DiagnosticWarning warnings

                         -- 2. Syntactic Pass
                         surfaceAST <- LP.parse tokens

                         -- 2.5 Desugar syntax
                         let cleanAST = Canon.construct surfaceAST

                         -- 3. Semantic Pass (Routing, Lazy Env Bindings, and Type Verifications)
                         compiledExpr <- EN.evaluate cleanAST

                         -- 4. Complete Return Pass
                         -- Pair your accumulated non-fatal diagnostics with your final expression!
                         pure (diagWarnings, compiledExpr)



validate :: String -> Text -> PrinterOptions -> Either String TL.Text
validate filename input opts = case JP.go filename input of
                                   Left  err  -> Left (errorBundlePretty err)
                                   Right expr -> Right (Ser.toJSON opts expr)
