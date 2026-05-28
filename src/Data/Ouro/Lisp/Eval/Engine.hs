{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Data.Ouro.Lisp.Eval.Engine where

import           Data.Function                  ((&))
import qualified Data.Map.Strict                as Map
import           Data.Ouro.Error.Diagnostics    (targetMismatch, typeMismatch,
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
import           Data.Ouro.Lisp.Eval.Structural (BlockTarget (..), compileArry,
                                                 compileScope,
                                                 determineBlockTarget,
                                                 resolvePath)
import           Data.Ouro.Lisp.Eval.Types      (Env (..), Value (..),
                                                 humanReadableType)
import qualified Data.Ouro.Lisp.Eval.Types      as Types
import qualified Data.Ouro.Lisp.Surface         as S
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Text.URI                       as URI


-- High-level engine entry point. Inspects a parsed AST node, handles routing,
-- and unboxes the underlying GADT primitive value directly.
evaluate :: S.Expr -> Either OuroError (I.Expr 'JLD.Primitive)
evaluate rootExpr = do
                    let baseEnv = Env { localScope = Map.empty, parentEnv = Nothing }

                    -- 1. Route the evaluation based on the structural shape of the root expression
                    evaluatedVal <- case rootExpr of
                                        S.Form _ allFields -> case determineBlockTarget allFields of
                                                                  TargetList        -> compileArry  evalExpr baseEnv allFields
                                                                  TargetObject      -> compileScope evalExpr baseEnv allFields
                                                                  TargetFunctionApp -> evalExpr baseEnv rootExpr
                                        otherExpr -> evalExpr baseEnv otherExpr

                    -- 2. Enforce that the output successfully reduced down to a valid primitive schema graph
                    case evaluatedVal of
                        Primitive finalGadtTree -> pure finalGadtTree
                        otherVal                -> let pos     = S.exprPos rootExpr
                                                       context = Typing TypeMismatch
                                                                 { expectedType = "a top-level data Object or a plain value configuration"
                                                                 , actualType   = Types.humanReadableType otherVal
                                                                 }
                                                    in Left (OuroError pos context)


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
--   * Type Assertion & Schema Coercion: Intercepts 'Tagged' constructors to enforce runtime validation boundaries.
--     It evaluates nested payloads and strictly checks or parses them into specific GADT leaf indices—such as
--     running external Megaparsec routines to cast strings into structural URLs (#uri), or verifying date formats (#date).
--   * Lexical Block Grouping: Processes classic Lisp '(let [bindings...] body)' syntax by sweeping assignments lazily,
--     generating an isolated environment frame linked to the current scope, and evaluating the inner body block within that frame.
--   * Function Application Mechanics: Evaluates compound structural forms by treating the head expression as an invokable operator.
--     It resolves the operator down to an executable handle (like a native platform 'PrimitiveOp'), eagerly evaluates trailing
--     arguments from left to right, and applies the parameters directly to compute the final result block.
evalExpr :: Env -> S.Expr -> Either OuroError Value
evalExpr env expr =
    case expr of
        S.Literal _ (S.Str txt)  -> pure $ Primitive (I.String txt)
        S.Literal _ (S.Num val)  -> pure $ Primitive (I.Number val)
        S.Literal _ (S.Bool b)   -> pure $ Primitive (I.Boolean b)
        S.Literal _ S.Null       -> pure $ Primitive I.Null
        S.Literal _ S.EmptyArr   -> pure $ Primitive I.EmptyArr
        S.Literal _ S.EmptyObj   -> pure $ Primitive I.EmptyObj
        S.Tagged  _ tag payload
            -> case tag of
                   -- Explicit URL Type Assertion
                   S.Uri  -> do
                             evaluatedVal <- evalExpr env payload
                             case evaluatedVal of
                                 Primitive (I.String rawText)
                                     -> case URI.mkURI rawText of
                                            Left _ -> typeMismatch
                                                        "a String matching URI layout with a scheme included (https:)"
                                                        "a String in an invalid URI format"
                                                    & withBlurb (typeMismatchBlurb (Primitive (I.String rawText)))
                                                    & OuroError (S.exprPos payload)
                                                    & Left

                                            Right uri -> case URI.uriScheme uri of
                                                             Just _  -> pure $ Primitive (I.URI uri)
                                                             Nothing -> typeMismatch
                                                                            "a String matching a URI layout with a scheme included (https:)"
                                                                            "a String in an invalid URI format"
                                                                         & withBlurb (typeMismatchBlurb (Primitive (I.String rawText)))
                                                                         & OuroError (S.exprPos payload)
                                                                         &Left


                                 otherVal
                                     -> typeMismatch
                                            "a String matching a URI layout with a scheme included (https:)"
                                            (humanReadableType otherVal)
                                        & withBlurb (typeMismatchBlurb otherVal)
                                        & OuroError (S.exprPos payload)
                                        & Left

                   -- Explicit Date Type Assertion
                   S.Date -> do
                             evaluatedVal <- evalExpr env payload
                             case evaluatedVal of
                                 Primitive (I.String rawText) ->
                                     case parseISO8601 (T.unpack rawText) of
                                         Just utcTime -> pure $ Primitive (I.Date utcTime)
                                         Nothing
                                             -> typeMismatch
                                                    "a String matching ISO-8601 layout (YYYY-MM-DDTHH:mm:ssZ)"
                                                    "a String in an invalid Date format"
                                                & withBlurb (typeMismatchBlurb (Primitive (I.String rawText)))
                                                & OuroError (S.exprPos payload)
                                                & Left

                                 otherVal -> typeMismatch
                                                 "a String matching ISO-8601 layout (YYYY-MM-DDTHH:mm:ssZ)"
                                                 (humanReadableType otherVal)
                                             & withBlurb (typeMismatchBlurb otherVal)
                                             & OuroError (S.exprPos payload)
                                             & Left

                   -- Explicit String Type Assertion
                   S.StrTag -> do
                               evaluatedVal <- evalExpr env payload
                               case evaluatedVal of
                                   Primitive (I.String _) -> pure evaluatedVal
                                   otherVal               -> typeMismatch
                                                                 "a String"
                                                                 (humanReadableType otherVal)
                                                             & withBlurb (typeMismatchBlurb otherVal)
                                                             & OuroError (S.exprPos payload)
                                                             & Left

                   -- Explicit Numeric Type Assertion
                   S.NumTag -> do
                               evaluatedVal <- evalExpr env payload
                               case evaluatedVal of
                                   Primitive (I.Number _) -> pure evaluatedVal
                                   otherVal               -> typeMismatch
                                                                 "a Number"
                                                                 (humanReadableType otherVal)
                                                             & withBlurb (typeMismatchBlurb otherVal)
                                                             & OuroError (S.exprPos payload)
                                                             & Left

                   -- Explicit Boolean Type Assertion
                   S.BoolTag -> do
                                evaluatedVal <- evalExpr env payload
                                case evaluatedVal of
                                    Primitive (I.Boolean _) -> pure evaluatedVal
                                    otherVal                -> typeMismatch
                                                                   "a Boolean"
                                                                   (humanReadableType otherVal)
                                                               & withBlurb (typeMismatchBlurb otherVal)
                                                               & OuroError (S.exprPos payload)
                                                               & Left

                   _ -> let context = Syntax $ MalformedTagPayload
                                      { activeTag      = T.pack (show tag)
                                      , foundNodeShape = "You cannot type assert that an expression evaluates to an emtyp object or array"
                                      }
                        in Left (OuroError (S.exprPos payload) context)

        -- Variables (Now handles variable properties AND dynamic function resolution fallback)
        S.Symbol pos name -> lookupVar evalExpr pos name env

        -- --- INTERCEPT SPECIAL FORMS ---
        S.Form _ [S.Symbol _ "context", S.Form _ directives] -> do
                                                                localSchema <- parseContextDirectives directives
                                                                pure $ Metadata (I.Context localSchema)

        S.Form _ (S.Symbol _ "get" : rootTarget : pathExpressions)
            -> do
               -- Convert trailing arguments into a clean lookup stack of Text tokens inline
               pathKeys <- validatePathKeys pathExpressions

               -- Navigate down through the un-evaluated syntax blocks inside the Env
               leafExpr <- resolvePath env rootTarget pathKeys

               -- Eagerly evaluate only the selected leaf target node
               evalExpr env leafExpr

        -- Idiomatic Lisp Scoping Form: (context (directives...) scopedFields...)
        -- Complex Form Sequences: Evaluates structural routing targets based on nested depth indicators
        S.Form pos allFields
            -> case determineBlockTarget allFields of
                   -- Structural
                   TargetObject -> compileScope evalExpr env allFields
                   TargetList   -> compileArry  evalExpr env allFields
                   -- Functions
                   TargetFunctionApp
                       -> case allFields of
                              (operatorExpr : argumentExprs)
                                  -> do
                                     resolvedOp <- evalExpr env operatorExpr
                                     case resolvedOp of
                                         PrimitiveOp nativeFunc -> do
                                                                   evaledArgs <- mapM (evalExpr env) argumentExprs
                                                                   nativeFunc pos evaledArgs
                                         otherVal
                                             -- SCOPE/EXECUTION VIOLATION: The lookup handle resolved to a non-callable term
                                             -> unboundIdentifier (humanReadableType otherVal)
                                                & withBlurb
                                                      ( "The evaluator attempted to invoke the form head as a callable function handle, "
                                                     <> "but the identifier resolved to an immutable "
                                                         <> humanReadableType otherVal
                                                         <> " primitive instead.\n\n"
                                                     <> "Perhaps check that target value is in scope."
                                                      )
                                                & OuroError (S.exprPos operatorExpr)
                                                & Left

                              [] -> unbalancedDelimiter
                                        "an active form operator symbol"
                                        "Empty Brackets"
                                    & withBlurb
                                          ( "Empty structural framing brackets are invalid executable values in Ouro."
                                         <> "An execution group must contain at least a primary invocation symbol or operator key."
                                          )
                                    & OuroError pos
                                    & Left

        otherNode
            -> let pos     = S.exprPos otherNode
                   context = Typing $ TypeMismatch
                             { expectedType = "a valid runtime configuration primitive value"
                             , actualType   = "an unrecognized compiler macro token shape"
                             }
               in Left (OuroError pos context)


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
validatePathKeys :: [S.Expr] -> Either OuroError [Text]
validatePathKeys = mapM (\case
    S.Symbol _ k -> pure k

    -- SYNTAX VIOLATION: An active attribute binder was passed where a raw path identifier belongs
    S.Attr aPos rawAttr
        -> targetMismatch "a lookup Symbol path component" (":" <> rawAttr)
            & withBlurb
                (  "The 'get' path operator expects bare lookup symbols (e.g., properties) "
                <> "to traverse target graph layers. You provided a colon-prefixed attribute identifier.\n\n"
                <> "Perhaps remove the leading colon operator from '" <> ":" <> rawAttr <> "' to "
                <> "transition the token from a property key to an active navigation handle."
                )
            & OuroError aPos
            & Left

    -- SYNTAX VIOLATION: A compound structural form or primitive literal was passed instead of a symbol
    badNode
        -> targetMismatch "a lookup Symbol property identifier" "a structural node form"
            & withBlurb
                ( "Path navigation parameters following the target node must evaluate "
               <> "strictly to atomic path components. Compound S-Expression trees, macro tags, "
               <> "and loose primitive objects cannot be read as horizontal layout lookup slots."
                )
            & OuroError (S.exprPos badNode)
            & Left
    )
