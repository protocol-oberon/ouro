{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

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
import           Data.Function               ((&))
import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as Map
import           Data.Ouro.Error.Diagnostics (nonExistentGraph, smartErrorCode,
                                              smartWarningCode, typeMismatch,
                                              warningBlurb, warningSummary,
                                              withBlurb)
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
import           Data.Ouro.Internal.Utils    (rankBySimilarity)
import qualified Data.Ouro.Json.Parser       as JP
import           Data.Ouro.Json.Serializer   (PrinterOptions (..),
                                              defaultOptions)
import qualified Data.Ouro.Json.Serializer   as Ser
import qualified Data.Ouro.Lisp.Canon        as Canon
import qualified Data.Ouro.Lisp.Eval.Engine  as EN
import           Data.Ouro.Lisp.Eval.Types   (Expr (..), freeze)
import qualified Data.Ouro.Lisp.Eval.Types   as L
import qualified Data.Ouro.Lisp.Lexer        as LX
import           Data.Ouro.Lisp.Module.Types (HigherExpression (..),
                                              graphRegistry)
import qualified Data.Ouro.Lisp.Parser       as LP
import qualified Data.Ouro.Lisp.Surface      as S
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as TL
import           Lens.Micro.Platform         ((^.))
import qualified Text.Megaparsec             as M
import           Text.Megaparsec             (errorBundlePretty)


data CompilationResult
    = CompilationSuccess [OuroWarning] (I.Expr   'JLD.Primitive)
    | CompilationFailure [OuroWarning] (NonEmpty OuroError)

-- Type alias representing the dual-track compiler sandbox.
-- Errors accumulate in the ExceptT track, Warnings accumulate in the Writer track.
type CompilerM = ExceptT (NonEmpty OuroError) (Writer [OuroWarning])


compile :: String -> String -> Text -> CompilationResult
compile filename trgt content = compilationResult . runWriter . runExceptT $ compile'
    where
    compile' :: CompilerM (I.Expr 'JLD.Primitive)
    compile' = do
               -- Pass 1: Lexical Tokenization
               tokens <- case LX.tokenize filename  content of
                             Left  lexErr -> throwError $ pure lexErr
                             Right tkns   -> return tkns

               -- Pass 2: Linting
               tell $ LN.lintExpression tokens

               -- Pass 3: Synatic Parsing
               env <- case LP.parseModule tokens of
                          Left  parseErr  -> throwError $ pure parseErr
                          Right moduleEnv -> return moduleEnv

               -- Pass 3.1: Find target graph
               graph <- case Map.lookup (T.pack trgt) (env ^. graphRegistry) of
                            Just    g -> return g
                            Nothing
                                -> do
                                   let keys = Map.keys (env ^. graphRegistry)
                                   case rankBySimilarity (T.pack trgt) keys of
                                       ((bestMatch, _) : _)
                                           -> let _graphSuggestion@(Graph pos _ _) = (env ^. graphRegistry) Map.! bestMatch
                                              in nonExistentGraph (T.pack trgt) bestMatch
                                                 & OuroError pos
                                                 & pure
                                                 & throwError

                                       []  -> Scope (NoCompilationTarget (T.pack filename))
                                              & OuroError (M.initialPos filename)
                                              & pure
                                              & throwError
               -- Pass 4: Evaluation
               let (Graph _ _ gAst) = graph
                   evalTree         = EN.evaluate env (Canon.construct gAst)

               case validateAST evalTree of
                   Just    errs -> throwError errs
                   Nothing      -> return (freeze evalTree)

    compilationResult :: (Either(NonEmpty OuroError) (I.Expr 'JLD.Primitive), [OuroWarning]) -> CompilationResult
    compilationResult = \case
                         (Right ast, warnings) -> CompilationSuccess warnings ast
                         (Left errs, warnings) -> CompilationFailure warnings errs


validateAST :: L.Expr -> Maybe (NonEmpty OuroError)
validateAST root = NE.nonEmpty (Set.toList $ validateAST' root)

-- Walks the evaluated L.Expr tree to extract any embedded dynamic EvalErrors
validateAST' :: L.Expr -> Set OuroError
validateAST' valueGraph = case valueGraph of
                              EvalError err            -> Set.singleton err
                              -- Deeply traverse structural object field value branches
                              Record    _        pairs -> Set.unions (map (validateAST' . snd) pairs)
                              Array     elements       -> Set.unions (map validateAST' elements)
                              -- Unevaluated quotes cannot be present in the final AST
                              Quote     payload        -> typeMismatch "a resolved type" "an unevaluated Quoted expression"
                                                          & withBlurb ( "All expressions in Ouro must be evaluated before compilation can be finished."
                                                                     <> "\n\nPerhaps remove the quote (\') from this expression to evaluate it."
                                                                      )
                                                          & OuroError (S.exprPos payload)
                                                          & Set.singleton
                              -- Pristine values, closures, and frozen GADTs have zero errors
                              _primitve               -> Set.empty



validate :: String -> Text -> PrinterOptions -> Either String TL.Text
validate filename input opts = case JP.go filename input of
                                   Left  err  -> Left (errorBundlePretty err)
                                   Right expr -> Right (Ser.toJSON opts expr)
