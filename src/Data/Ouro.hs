{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Data.Ouro
  -- * Core Compilation Pipeline
( compile
, compileUsing
, compileWith
, evaluateGraph
, CompilationResult(..)
, PrinterOptions(..)
, Ser.toJSON
, defaultOptions
, validate

-- * Decoding source files, and the bundled modules' sources
, bundledFile
, decodeSource

-- * Core Execution Error Types
, BuildError(..)
, ErrorContext(..)
, InternalError(..)
, OuroError(..)
, PathError(..)
, ScopeError(..)
, SyntaxError(..)
, TypeError(..)
, smartErrorCode
, SourceFrame(..)
, sourceFrame

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

import           Control.Monad.Validate      (refute, runValidateT)
import           Control.Monad.Writer        (runWriterT, tell)
import           Data.Containers.ListUtils   (nubOrd)
import           Data.Foldable               (traverse_)
import           Data.Function               ((&))
import           Data.Functor.Identity       (runIdentity)
import           Data.List                   (sortOn)
import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromMaybe)
import           Data.Ouro.Error.Diagnostics (SourceFrame (..), nonExistentGraph,
                                              smartErrorCode,
                                              smartWarningCode, sourceFrame,
                                              typeMismatch,
                                              warningBlurb, warningSummary,
                                              withBlurb)
import qualified Data.Ouro.Error.Linter      as LN
import           Data.Ouro.Error.Types       (BuildError (..), ErrorContext (..),
                                              InternalError (..),
                                              LinterWarning (..),
                                              OuroError (..), OuroWarning (..),
                                              PathError (..), ScopeError (..),
                                              SemanticWarning (..),
                                              SyntaxError (..), TypeError (..),
                                              VocabularyWarning (..),
                                              WarningContext (..))
import           Data.Ouro.Internal.Compiler (CompilerT)
import qualified Data.Ouro.Internal.Expr     as I
import qualified Data.Ouro.Internal.Kinds    as JLD
import           Data.Ouro.Internal.Source   (decodeSource)
import           Data.Ouro.Internal.Utils    (rankBySimilarity)
import qualified Data.Ouro.Json.Parser       as JP
import           Data.Ouro.Json.Serializer   (PrinterOptions (..),
                                              defaultOptions)
import qualified Data.Ouro.Json.Serializer   as Ser
import           Data.Ouro.Lisp.Analysis     (analyseModule)
import qualified Data.Ouro.Lisp.Canon        as Canon
import qualified Data.Ouro.Lisp.Eval.Engine  as EN
import           Data.Ouro.Lisp.Eval.Types   (Expr (..), freeze)
import qualified Data.Ouro.Lisp.Eval.Types   as L
import qualified Data.Ouro.Lisp.Lexer        as LX
import           Data.Ouro.Lisp.Module.Bundled (bundledFile)
import           Data.Ouro.Lisp.Module.Loader (LoadEnv, fromSources,
                                              loadImports, runLoader)
import           Data.Ouro.Lisp.Module.Types (Declaration (..),
                                              HigherExpression (..), Module,
                                              acceptedCalls, graphRegistry)
import qualified Data.Ouro.Lisp.Parser       as LP
import qualified Data.Ouro.Refinement        as R
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as TL
import           Lens.Micro.Platform         ((.~), (^.))
import qualified Text.Megaparsec             as M
import           Text.Megaparsec             (errorBundlePretty)


data CompilationResult
    = CompilationSuccess [OuroWarning] (I.Expr   'JLD.Primitive)
    | CompilationFailure [OuroWarning] (NonEmpty OuroError)


compile :: String -> String -> Text -> CompilationResult
compile = compileWith Map.empty


-- Compiles a file that can import the given modules, sources in memory by path, as well as
-- the bundled ones. Nothing is read from disk, so it is pure: the test suite's fake files.
compileWith :: Map.Map Text Text -> String -> String -> Text -> CompilationResult
compileWith sources filename trgt content = runIdentity (compileUsing (fromSources sources) filename trgt content)


-- Compiles a file whose imports are read through the given loader: from memory, or from disk
compileUsing :: forall m. Monad m => LoadEnv m -> String -> String -> Text -> m CompilationResult
compileUsing loader filename trgt content = compilationResult <$> runWriterT (runValidateT compile')
    where
    compile' :: CompilerT m (I.Expr 'JLD.Primitive)
    compile' = do
               -- Pass 1: Lexical Tokenization
               tokens <- case LX.tokenize filename  content of
                             Left  lexErr -> refute $ pure lexErr
                             Right tkns   -> return tkns

               -- Pass 2: Linting
               tell $ LN.lintExpression tokens

               -- Pass 3: Synatic Parsing
               ouroModule <- case LP.parseModule tokens of
                          Left  parseErr  -> refute $ pure parseErr
                          Right moduleEnv -> return moduleEnv

               -- Pass 3.0: Linting the parsed functions and graphs
               tell $ LN.lintModule ouroModule

               -- Pass 3.1 Desugar all ASTs, running Canon's passes in order (see 'Canon.passes')
               let desugaredModule = Canon.canonicalModule ouroModule

               -- Pass 3.1.1: Linting the types it writes, assertions in expressions among them
               tell $ LN.lintTypes desugaredModule

               -- Pass 3.2: Load the modules it imports, each linked into its own root frame.
               -- They are read through the loader: from memory, or from disk.
               imported <- runLoader loader (loadImports desugaredModule)

               -- Pass 3.3: Static analysis: every function and graph checked as written, before
               -- any is evaluated. Its errors end the compile, as the parser's do; the calls it
               -- accepted, which may call a function already being called, stay with the module.
               let (problems, accepted) = analyseModule imported desugaredModule
                   analysedModule       = desugaredModule & acceptedCalls .~ accepted
               traverse_ refute (NE.nonEmpty problems)

               -- Pass 3.4: Find target graph
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
                                                 & refute

                                       []  -> Scope (NoCompilationTarget (T.pack filename))
                                              & OuroError (M.initialPos filename)
                                              & pure
                                              & refute
               -- Pass 4: Evaluation
               either refute (pure . freeze) (evaluateGraph imported analysedModule graph)

    compilationResult :: (Either(NonEmpty OuroError) (I.Expr 'JLD.Primitive), [OuroWarning]) -> CompilationResult
    compilationResult = \case
                         (Right ast, warnings) -> CompilationSuccess warnings ast
                         (Left errs, warnings) -> CompilationFailure warnings errs


-- A graph of a module, evaluated with the modules it imports: its value, which has a JSON
-- form, or every error left in it
evaluateGraph :: Map.Map Text L.Imported -> Module -> HigherExpression 'GraphExpr -> Either (NonEmpty OuroError) L.Expr
evaluateGraph imported ouroModule (Graph gPos _ gAst) =
    let (evalTree, bindingErrors) = EN.evaluate imported ouroModule gAst
    in maybe (Right evalTree) Left (validateAST gPos bindingErrors evalTree)


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
                              -- Deeply traverse structural record attribute value branches
                              Record    _        pairs -> concat [ validateAST' gPos (keyPath k) v | (k, v) <- pairs ]
                              Array     elements       -> concat [ validateAST' gPos (indexPath i) v | (i, v) <- zip [0 :: Int ..] elements ]
                              -- JSON has no representation for Infinity or NaN
                              Primitive (I.Number n)
                                  | isInfinite n || isNaN n
                                                       -> typeMismatch "a finite Number" (case isNaN n of True -> "NaN"; False -> "Infinity")
                                                          & withBlurb ( "The value at " <> location <> " is not a finite number, which JSON cannot represent."
                                                                     <> "\n\nPerhaps an arithmetic expression overflowed, or a numeric literal is too large."
                                                                      )
                                                          & OuroError gPos
                                                          & pure
                              Primitive _              -> []
                              -- Compile-time only values (functions, schemas...) have no JSON form
                              unserialisable           -> typeMismatch
                                                              "data"
                                                              (L.humanReadableType unserialisable)
                                                          & withBlurb (unserialisableBlurb location unserialisable)
                                                          & OuroError gPos
                                                          & pure
    where
    keyPath   k = case T.null path of
                      True  -> k
                      False -> path <> "." <> k
    indexPath i = path <> "[" <> T.pack (show i) <> "]"
    location    = case T.null path of
                      True  -> "the root of the document"
                      False -> "'" <> path <> "'"


-- Explains why a compile-time only value cannot be written to the JSON output: a function is
-- drawn in the tree of built-in refinements, data required and its kind given
unserialisableBlurb :: Text -> L.Expr -> Text
unserialisableBlurb location = \case
    f@(PrimitiveOp _)     -> functionHint f
    f@FunctionClosure {}  -> functionHint f
    _compileTimeOnly      -> "The value at " <> location <> " only exists during compilation and has no JSON representation."
    where
    functionHint f = T.intercalate "\n\n" (filter (not . T.null)
                         [ "The value at " <> location <> " is a function, not data."
                         , fromMaybe "" (R.refinementTree R.RData f)
                         , maybe "Perhaps call it with its arguments." (\call -> "Perhaps call it: " <> call <> ".") (L.callOf f) ])



validate :: String -> Text -> PrinterOptions -> Either String TL.Text
validate filename input opts = case JP.go filename input of
                                   Left  err  -> Left (errorBundlePretty err)
                                   Right expr -> Right (Ser.toJSON opts expr)
