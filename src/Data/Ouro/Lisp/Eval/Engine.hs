{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Data.Ouro.Lisp.Eval.Engine where

import           Control.Monad.Reader           (MonadReader (..), Reader,
                                                 runReader)
import           Data.Function                  ((&))
import qualified Data.Map.Strict                as Map
import           Data.Ouro.Error.Diagnostics    (internalValueLeak,
                                                 targetMismatch, typeMismatch,
                                                 typeMismatchBlurb,
                                                 unbalancedDelimiter,
                                                 unboundIdentifier, withBlurb)
import           Data.Ouro.Error.Types          (ErrorContext (..),
                                                 OuroError (..),
                                                 SyntaxError (..),
                                                 TypeError (..))
import qualified Data.Ouro.Internal.Expr        as I
import qualified Data.Ouro.Internal.Kinds       as JLD
import           Data.Ouro.Lisp.Eval.Builtins   (parseISO8601)
import           Data.Ouro.Lisp.Eval.Schema     (parseContextDirectives)
import           Data.Ouro.Lisp.Eval.Scope      (lookupVar)
import           Data.Ouro.Lisp.Eval.Structural (BlockTarget (..), compileArray,
                                                 compileScope,
                                                 determineBlockTarget,
                                                 resolvePath)
import           Data.Ouro.Lisp.Eval.Types      (Env (..), Expr (..),
                                                 humanReadableType)
import qualified Data.Ouro.Lisp.Eval.Types      as L
import qualified Data.Ouro.Lisp.Surface         as S
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Text.Megaparsec                (SourcePos)
import qualified Text.URI                       as URI


-- No State monad is required because errors are handled as Data in the L.Expr tree.
type EvalM = Reader Env

-- High-level engine entry point.
-- Inspects a parsed AST node, handles routing, and returns the result as a L.Expr.
-- Errors are now contained within the returned L.Expr graph, not lifted to Either.
evaluate :: S.Expr -> L.Expr
evaluate rootExpr = runReader (evalExpr rootExpr) baseEnv
    where
    baseEnv = L.defaultEnv

-- Helper to extract the primitive tree or the error from the result.
-- This bridges the lazy engine to the typed static target.
evaluateToPrimitive :: S.Expr -> Either OuroError (I.Expr 'JLD.Primitive)
evaluateToPrimitive rootExpr =
    case evaluate rootExpr of
        Primitive finalGadtTree -> Right finalGadtTree
        EvalError err           -> Left err
        otherVal                -> Left $ OuroError (S.exprPos rootExpr) $
                                     typeMismatch
                                       "a top-level data Object or a plain value configuration"
                                       (humanReadableType otherVal)
                                     & withBlurb (typeMismatchBlurb otherVal)


-- evalExpr.
--
-- The core structural evaluation engine driving the Lisp interpreter's AST transformation pass.
-- Operates as a deep recursive semantic walker that lowers untyped surface syntax trees into typed runtime values.
--
-- This function serves as the central orchestration hub for execution semantics, handling variable lookups,
-- lexical block creation, type assertions, and functional application. It maps each surface language construct
-- to its exact operational behavior while dynamically tracking scoping constraints:
--
--   * Literal Normalization: Translates raw, un-indexed syntax leaf tokens directly into their matching
--     type-safe internal GADT representations, mapping primitives (Strings, Numbers, Booleans) and empty collections.
--   * Type Assertion Redirection: Flags tagged nodes and offloads validation logic directly to 'assertTag'
--     to preserve clean separation between AST routing layers and deep textual serialization checks.
--   * Intercepting Special Forms: Catches explicit keyword structures (like contextual schemas or metadata lookups)
--     to compute lazy properties or isolated navigation coordinates before evaluation loops spin up.
--   * Function Application Mechanics: Delegates composite call structures down to the 'applyFunction' handler,
--     resolving operational heads into native platform hooks and forcing left-to-right argument processing.
evalExpr :: S.Expr -> EvalM L.Expr
evalExpr expr = do
    env <- ask
    case expr of
        S.Literal _ (S.Str txt) -> pure $ Primitive (I.String txt)
        S.Literal _ (S.Num val) -> pure $ Primitive (I.Number val)
        S.Literal _ (S.Bool b)  -> pure $ Primitive (I.Boolean b)
        S.Literal _ S.Null      -> pure $ Primitive I.Null
        S.Literal _ S.EmptyArr  -> pure $ Primitive I.EmptyArr
        S.Literal _ S.EmptyObj  -> pure $ Primitive I.EmptyObj

        S.Tagged _   tag  payload -> assertTag tag payload
        S.Symbol pos name         -> lookupVar evalExpr pos name env

        --- INTERCEPT SPECIAL FORMS ---
        S.Form pos [S.Symbol _ "context", S.Form _ directives]
            -> do
               val <- parseContextDirectives directives
               case val of
                   SchemaVal s -> pure $ Metadata (I.Context s)
                   EvalError e -> pure $ EvalError e
                   actual      -> pure $ EvalError $ OuroError pos
                                        $ typeMismatch "Schema" (humanReadableType actual)

        S.Form _ (S.Symbol _ "get" : rootTarget : pathExpressions)
            -> do
               case validatePathKeys pathExpressions of
                   EvalError err     -> pure (EvalError err)
                   -- Match on the open ArrayVal superset node instead of the old frozen GADT variant
                   Array     pathVal -> case resolvePath evalExpr env rootTarget pathVal of
                                              EvalError err -> pure (EvalError err)
                                              -- The path resolved cleanly to a final L.Expr, pass it forward
                                              resolvedVal   -> pure resolvedVal
                   _ -> internalValueLeak "Path validation returned an unexpected L.Expr variant."
                        & OuroError (S.exprPos rootTarget)
                        & EvalError
                        & pure

        S.Form pos allFields -> do
            let wrappedEvaluator currentEnv expr' = runReader (evalExpr expr') currentEnv
            case determineBlockTarget allFields of
                TargetObject      -> pure (compileScope wrappedEvaluator env allFields)
                TargetList        -> pure (compileArray wrappedEvaluator env allFields)
                TargetFunctionApp -> applyFunction pos allFields

        otherNode
            -> let pos     = S.exprPos otherNode
                   context = Typing $ TypeMismatch
                               { expectedType = "a valid runtime configuration primitive value"
                               , actualType   = "an unrecognized compiler macro token shape"
                               }
               in pure $ EvalError $ OuroError pos context


-- assertTag.
--
-- Validates, coerces, and casts runtime primitive values against strict structural type assertions.
-- Isolates external text serialization and parsing dependencies away from the core execution engine loop.
assertTag :: S.ReaderTag -> S.Expr -> EvalM L.Expr
assertTag tag payload = do
    evaluatedVal <- evalExpr payload
    let mkErr pos context = pure $ EvalError (OuroError pos context)

    case tag of
        S.Uri -> case evaluatedVal of
            Primitive (I.String rawText)
                -> case URI.mkURI rawText of
                       Left _    -> mkErr (S.exprPos payload)
                                       (typeMismatch "a String matching URI layout" "invalid URI format"
                                           & withBlurb (typeMismatchBlurb evaluatedVal))
                       Right uri -> case URI.uriScheme uri of
                           Just _  -> pure $ Primitive (I.URI uri)
                           Nothing -> mkErr (S.exprPos payload)
                                           (typeMismatch "a String matching a URI layout with a scheme included (https:)"
                                                       "a String in an invalid URI format"
                                           & withBlurb (typeMismatchBlurb (Primitive (I.String rawText))))
            otherVal -> mkErr (S.exprPos payload)
                                (typeMismatch "a String matching a URI layout with a scheme included (https:)"
                                              (humanReadableType otherVal)
                                 & withBlurb (typeMismatchBlurb otherVal))

        S.Date -> case evaluatedVal of
            Primitive (I.String rawText) ->
                case parseISO8601 (T.unpack rawText) of
                    Just utcTime -> pure $ Primitive (I.Date utcTime)
                    Nothing      -> mkErr (S.exprPos payload)
                                          (typeMismatch "a String matching ISO-8601 layout (YYYY-MM-DDTHH:mm:ssZ)"
                                                        "a String in an invalid Date format"
                                           & withBlurb (typeMismatchBlurb (Primitive (I.String rawText))))
            otherVal -> mkErr (S.exprPos payload)
                              (typeMismatch "a String matching ISO-8601 layout (YYYY-MM-DDTHH:mm:ssZ)"
                                            (humanReadableType otherVal)
                               & withBlurb (typeMismatchBlurb otherVal))

        S.StrTag  -> case evaluatedVal of
            Primitive (I.String _) -> pure evaluatedVal
            otherVal               -> mkErr (S.exprPos payload)
                                            (typeMismatch "a String" (humanReadableType otherVal)
                                             & withBlurb (typeMismatchBlurb otherVal))

        S.NumTag  -> case evaluatedVal of
            Primitive (I.Number _) -> pure evaluatedVal
            otherVal               -> mkErr (S.exprPos payload)
                                            (typeMismatch "a Number" (humanReadableType otherVal)
                                             & withBlurb (typeMismatchBlurb otherVal))

        S.BoolTag -> case evaluatedVal of
            Primitive (I.Boolean _) -> pure evaluatedVal
            otherVal                -> mkErr (S.exprPos payload)
                                             (typeMismatch "a Boolean" (humanReadableType otherVal)
                                              & withBlurb (typeMismatchBlurb otherVal))

        _ -> mkErr (S.exprPos payload)
                   (Syntax $ MalformedTagPayload
                     { activeTag      = T.pack (show tag)
                     , foundNodeShape = "You cannot type assert an empty object or array"
                     })


-- applyFunction.
--
-- Unpacks evaluated argument vectors and triggers platform-native executors.
-- Operates entirely within the pure Reader monad (EvalM).
applyFunction :: SourcePos -> [S.Expr] -> EvalM L.Expr
applyFunction pos fields =
    case fields of
        (operatorExpr : argumentExprs) -> do
            resolvedOp <- evalExpr operatorExpr
            case resolvedOp of
                PrimitiveOp nativeFunc -> do
                    evaledArgs <- mapM evalExpr argumentExprs
                    env        <- ask
                    pure $ runReader (nativeFunc pos evaledArgs) env

                err@(EvalError _) -> pure err

                otherVal -> pure $ EvalError $
                    unboundIdentifier (humanReadableType otherVal)
                    & withBlurb ("The evaluator attempted to invoke the form head as a callable function handle, "
                              <> "but the identifier resolved to an immutable "
                              <> humanReadableType otherVal
                              <> " primitive instead.\n\n"
                              <> "Perhaps check that target value is in scope.")
                    & OuroError pos

        [] -> pure $ EvalError $
            unbalancedDelimiter "an active form operator symbol" "Empty Brackets"
            & withBlurb ("Empty structural framing brackets are invalid executable values in Ouro. "
                      <> "An execution group must contain at least a primary invocation symbol or operator key.")
            & OuroError pos


-- validatePathKeys.
--
-- Unpacks and sanitizes a sequence of tail surface expressions into bare navigation keys.
-- Enforces strict token constraints to prevent macro shapes or value objects from entering paths.
--
-- This structural gatekeeper sits directly inside the main execution loop to validate inputs
-- for the 'get' path navigation routine. It acts as an early semantic check, verifying that all
-- trailing arguments are plain symbols before handing control off to the deep path resolver:
--
--   1. Valid Symbol Unpacking: Clean 'S.Symbol' targets are instantly unwrapped into raw 'Text' keys.
--   2. Attribute Guard Isolation: Throws a 'targetMismatch' if a colon-prefixed property binder
--      (:key) sneaks into a parameter slot, offering localized syntax correction hints.
--   3. Form Fallthrough Protection: Blocks complex nested s-expression sub-trees or macros from
--      corrupting horizontal layout lookups by intercepting structural nodes immediately.
validatePathKeys :: [S.Expr] -> L.Expr
validatePathKeys exprs = go exprs []
    where
    go :: [S.Expr] -> [Text] -> L.Expr
    go es acc = case es of
        -- Build an open, resilient ArrayVal list of dynamic L.Exprs to support the new superset layout
        [] -> Array (map (Primitive . I.String) (reverse acc))

        (S.Symbol _ k : xs) -> go xs (k : acc)

        (S.Attr aPos rawAttr : _) ->
            EvalError $ targetMismatch "a lookup Symbol path component" (":" <> rawAttr)
                        & withBlurb ( "The 'get' path operator expects bare lookup symbols (e.g., properties) "
                                   <> "to traverse target graph layers. You provided a colon-prefixed attribute identifier.\n\n"
                                   <> "Perhaps remove the leading colon operator from '" <> ":" <> rawAttr <> "' to "
                                   <> "transition the token from a property key to an active navigation handle."
                                    )
                        & OuroError aPos

        (badNode : _)
            -> EvalError $ targetMismatch "a lookup Symbol property identifier" "a structural node form"
                           & withBlurb ( "Path navigation parameters following the target node must evaluate "
                                      <> "strictly to atomic path components. Compound S-Expression trees, macro tags, "
                                      <> "and loose primitive objects cannot be read as horizontal layout lookup slots."
                                       )
                           & OuroError (S.exprPos badNode)
