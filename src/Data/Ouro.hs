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
import           Data.Containers.ListUtils   (nubOrd)
import           Data.Function               ((&))
import           Data.List                   (sortOn)
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
                                              graphRegistry, mapModuleExpr)
import qualified Data.Ouro.Lisp.Parser       as LP
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
               ouroModule <- case LP.parseModule tokens of
                          Left  parseErr  -> throwError $ pure parseErr
                          Right moduleEnv -> return moduleEnv

               -- Pass 3.0: Linting the parsed functions and graphs
               tell $ LN.lintModule ouroModule

               -- Pass 3.1 Desugar all ASTs, after turning calls with '?' holes into functions
               let desugaredModule = mapModuleExpr (Canon.construct . Canon.placeholders) ouroModule

               -- Pass 3.2: Find target graph
               graph <- case Map.lookup (T.pack trgt) (desugaredModule ^. graphRegistry) of
                            Just    g -> return g
                            Nothing
                                -> do
                                   let keys = Map.keys (desugaredModule ^. graphRegistry)
                                   case rankBySimilarity (T.pack trgt) keys of
                                       ((bestMatch, _) : _)
                                           -> let _graphSuggestion@(Graph pos _ _) = (desugaredModule ^. graphRegistry) Map.! bestMatch
                                              in nonExistentGraph (T.pack trgt) bestMatch
                                                 & OuroError pos
                                                 & pure
                                                 & throwError

                                       []  -> Scope (NoCompilationTarget (T.pack filename))
                                              & OuroError (M.initialPos filename)
                                              & pure
                                              & throwError
               -- Pass 4: Evaluation
               let (Graph gPos _ gAst)          = graph
                   (evalTree, bindingErrors) = EN.evaluate desugaredModule (Canon.constructRoot gAst)

               case validateAST gPos bindingErrors evalTree of
                   Just    errs -> throwError errs
                   Nothing      -> return (freeze evalTree)

    compilationResult :: (Either(NonEmpty OuroError) (I.Expr 'JLD.Primitive), [OuroWarning]) -> CompilationResult
    compilationResult = \case
                         (Right ast, warnings) -> CompilationSuccess warnings ast
                         (Left errs, warnings) -> CompilationFailure warnings errs


-- Errors in the graph's value, plus those found in bindings the output never used.
-- An error in a binding that is also in the output is reported once.
validateAST :: M.SourcePos -> [OuroError] -> L.Expr -> Maybe (NonEmpty OuroError)
validateAST gPos bindingErrors root = NE.nonEmpty (sortOn errPos . nubOrd $ validateAST' gPos "" root <> bindingErrors)

-- Walks the evaluated L.Expr tree to extract any embedded dynamic EvalErrors.
-- Accepts exactly the nodes 'freeze' can serialise; anything else becomes an error
-- reported at the graph's position, with the key path to the offending value.
-- Errors are returned in traversal (source) order, so the stable sort in
-- 'validateAST' keeps errors that share a position in the order they were written.
validateAST' :: M.SourcePos -> Text -> L.Expr -> [OuroError]
validateAST' gPos path valueGraph = case valueGraph of
                              EvalError err            -> [err]
                              -- Deeply traverse structural object field value branches
                              Record    _        pairs -> concat [ validateAST' gPos (keyPath k) v | (k, v) <- pairs ]
                              Array     elements       -> concat [ validateAST' gPos (indexPath i) v | (i, v) <- zip [0 :: Int ..] elements ]
                              -- JSON has no representation for Infinity or NaN
                              Primitive (I.Number n)
                                  | isInfinite n || isNaN n
                                                       -> typeMismatch "a finite Number" (if isNaN n then "NaN" else "Infinity")
                                                          & withBlurb ( "The value at " <> location <> " is not a finite number, which JSON cannot represent."
                                                                     <> "\n\nPerhaps an arithmetic expression overflowed, or a numeric literal is too large."
                                                                      )
                                                          & OuroError gPos
                                                          & pure
                              Primitive _              -> []
                              -- Compile-time only values (Durations, functions, schemas...) have no JSON form
                              unserialisable           -> typeMismatch
                                                              "a JSON value (String, Number, Boolean, Null, URI, Date, Record or Array)"
                                                              (L.humanReadableType unserialisable)
                                                          & withBlurb (unserialisableBlurb location unserialisable)
                                                          & OuroError gPos
                                                          & pure
    where
    keyPath   k = if T.null path then k else path <> "." <> k
    indexPath i = path <> "[" <> T.pack (show i) <> "]"
    location    = if T.null path then "the root of the graph" else "'" <> path <> "'"


-- Explains why a compile-time only value cannot be written to the JSON output.
unserialisableBlurb :: Text -> L.Expr -> Text
unserialisableBlurb location = \case
    Duration _ _          -> "The value at " <> location <> " is a Duration, which only has meaning when added to a date."
                          <> "\n\nPerhaps add it to a date, e.g. (+ #date \"1881-01-01T00:00:00Z\" (years 2))."
    PrimitiveOp _         -> functionHint
    FunctionClosure _ _ _ _ -> functionHint
    _compileTimeOnly      -> "The value at " <> location <> " only exists during compilation and has no JSON representation."
    where
    functionHint = "The value at " <> location <> " is a function, not a value."
                <> "\n\nPerhaps you meant to call it, e.g. (name! args)."



validate :: String -> Text -> PrinterOptions -> Either String TL.Text
validate filename input opts = case JP.go filename input of
                                   Left  err  -> Left (errorBundlePretty err)
                                   Right expr -> Right (Ser.toJSON opts expr)
