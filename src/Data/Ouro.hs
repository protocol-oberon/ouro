{-# LANGUAGE DataKinds #-}

module Data.Ouro
  -- * Core Compilation Pipeline
( compile
, CompilationResult(..)
, PrinterOptions(..)
, Ser.toJSON
, defaultOptions
, validate

-- * Core Execution Error Types
, ErrorContext(..)
, InternalError(..)
, OuroError(..)
, PathError(..)
, ScopeError(..)
, SyntaxError(..)
, TypeError(..)
, smartErrorCode

-- * Warning Subsystems
, LinterWarning(..)
, OuroWarning(..)
, SemanticWarning(..)
, VocabularyWarning(..)
, WarningContext(..)
, smartWarningCode
, warningBlurb
, warningSummary
) where

import           Control.Monad.Except        (ExceptT, runExceptT, throwError)
import           Control.Monad.Writer        (Writer, runWriter, tell)
import           Data.Ouro.Error.Diagnostics (smartErrorCode, smartWarningCode,
                                              warningBlurb, warningSummary)
import qualified Data.Ouro.Error.Linter      as LN
import           Data.Ouro.Error.Types       (ErrorContext (..),
                                              InternalError (..),
                                              LinterWarning (..),
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
import           Data.Ouro.Lisp.Eval.Types   (Expr (..), freeze)
import qualified Data.Ouro.Lisp.Eval.Types   as L
import qualified Data.Ouro.Lisp.Lexer        as LX
import qualified Data.Ouro.Lisp.Parser       as LP
import           Data.Text                   (Text)
import qualified Data.Text.Lazy              as TL
import           Text.Megaparsec             (errorBundlePretty)


data CompilationResult
    = Success [OuroWarning] (I.Expr 'JLD.Primitive)
    | Failure [OuroWarning] [OuroError]

-- Type alias representing the dual-track compiler sandbox.
-- Errors accumulate in the ExceptT track, Warnings accumulate in the Writer track.
type CompilerM = ExceptT [OuroError] (Writer [OuroWarning])

compile :: FilePath -> Text -> CompilationResult
compile filename content = compilationResult . runWriter . runExceptT $ compile'
    where
    compile' :: CompilerM (I.Expr 'JLD.Primitive)
    compile' = do
               -- Pass 1: Lexical Tokenization
               tokens <- case LX.tokenize filename content of
                             Left  lexErr -> throwError [lexErr]
                             Right tkns   -> return tkns

               -- Pass 2: Linting
               tell $ LN.lintExpression tokens

               -- Pass 3: Synatic Parsing
               surfaceAST <- case LP.parse tokens of
                                 Left  parseErr -> throwError [parseErr]
                                 Right ast      -> return ast

               -- Pass 4: Evaluation
               let evalTree = EN.evaluate (Canon.construct surfaceAST)
               case harvestErrors evalTree of
                   []   -> return (freeze evalTree)
                   errs -> throwError errs

    compilationResult :: (Either [OuroError] (I.Expr 'JLD.Primitive), [OuroWarning]) -> CompilationResult
    compilationResult = \case
                         (Right ast, warnings) -> Success warnings ast
                         (Left errs, warnings) -> Failure warnings errs


-- Walks the evaluated L.Expr tree to extract any embedded dynamic EvalErrors
harvestErrors :: L.Expr -> [OuroError]
harvestErrors valueGraph = case valueGraph of
                               EvalError err  -> [err]
                               -- Deeply traverse structural object field value branches
                               Object _ pairs -> concatMap (harvestErrors . snd) pairs
                               -- Deeply traverse open array value elements
                               Array elements -> concatMap harvestErrors elements
                               -- Pristine values, closures, and frozen GADTs have zero errors
                               _              -> []



validate :: String -> Text -> PrinterOptions -> Either String TL.Text
validate filename input opts = case JP.go filename input of
                                   Left  err  -> Left (errorBundlePretty err)
                                   Right expr -> Right (Ser.toJSON opts expr)
