{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}

module Data.Ouro.Lisp.Eval.Engine where

import           Control.Monad.Reader           (MonadReader (..), Reader,
                                                 runReader)
import           Data.Function                  ((&))
import qualified Data.Map                       as Map
import           Data.Ouro.Error.Diagnostics    (astCorruption,
                                                 indexOutOfBounds,
                                                 inexhaustiveCase,
                                                 inexhaustiveCaseBlurb,
                                                 internalValueLeak,
                                                 malformedCaseBranch,
                                                 malformedTag, missingOtherwise,
                                                 missingOtherwiseBlurb,
                                                 targetMismatch, typeMismatch,
                                                 typeMismatchBlurb,
                                                 unbalancedDelimiter,
                                                 unboundIdentifier, withBlurb)
import           Data.Ouro.Error.Types          (ErrorContext (..),
                                                 OuroError (..), TypeError (..))
import qualified Data.Ouro.Internal.Expr        as I
import qualified Data.Ouro.Internal.Kinds       as JLD
import           Data.Ouro.Lisp.Eval.Builtins   (builtinRegistry, parseISO8601)
import           Data.Ouro.Lisp.Eval.Schema     (parseContextDirectives)
import           Data.Ouro.Lisp.Eval.Scope      (lookupVar, quoteVar)
import           Data.Ouro.Lisp.Eval.Structural (BlockTarget (..), compileArray,
                                                 compileRecord, compileTemplate,
                                                 determineBlockTarget,
                                                 resolvePath)
import           Data.Ouro.Lisp.Eval.Types      (Env (..), Expr (..),
                                                 humanReadableType)
import qualified Data.Ouro.Lisp.Eval.Types      as L
import qualified Data.Ouro.Lisp.Surface         as S
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import           Text.Megaparsec                (SourcePos)
import qualified Text.URI                       as URI


-- No State monad is required because errors are handled as Data in the L.Expr tree.
type EvalM = Reader Env

-- High-level engine entry point.
-- Inspects a parsed AST node, handles routing, and returns the result as a L.Expr.
-- Errors are now contained within the returned L.Expr graph, not lifted to Either.
evaluate :: S.Expr -> L.Expr
evaluate rootExpr = runReader (evalExpr rootExpr) L.defaultEnv


-- Helper to extract the primitive tree or the error from the result.
-- This bridges the lazy engine to the typed static target.
evaluateToPrimitive :: S.Expr -> Either OuroError (I.Expr 'JLD.Primitive)
evaluateToPrimitive rootExpr =
    case evaluate rootExpr of
        Primitive finalGadtTree -> Right finalGadtTree
        EvalError err           -> Left err
        otherVal                -> typeMismatch
                                       "a top-level data Record or a plain value configuration"
                                       (humanReadableType otherVal)
                                   & withBlurb (typeMismatchBlurb otherVal)
                                   & OuroError (S.exprPos rootExpr)
                                   & Left


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
        S.Literal _ S.EmptyRec  -> pure $ Primitive I.EmptyRec

        S.Tagged _   tag   payload -> assertTag tag payload
        S.Quoted _   quote         -> pure $ Quote quote
        S.Symbol pos name          -> lookupVar evalExpr pos name env

        --- INTERCEPT SPECIAL FORMS ---
        S.Form _ [S.Symbol pos "context", S.Form _ directives]
            -> do
               val <- parseContextDirectives directives
               case val of
                   SchemaVal s -> pure $ Metadata (I.Context s)
                   EvalError e -> pure $ EvalError e
                   actual      -> pure $ EvalError $ OuroError pos
                                        $ typeMismatch "Schema" (humanReadableType actual)

        S.Form _ (S.Symbol _ "get'" : rootTarget : pathExpressions)
            -> case validatePathKeys pathExpressions of
                   EvalError err     -> pure (EvalError err)
                   -- Match on the open ArrayVal superset node instead of the old frozen GADT variant
                   Array     pathVal -> case resolvePath (Nothing) env rootTarget pathVal of
                                              EvalError err -> pure (EvalError err)
                                              -- The path resolved cleanly to a final Quote, pass it forward
                                              quotedVal   -> pure quotedVal
                   _unexpectedExpr -> internalValueLeak "Path validation returned an unexpected L.Expr variant."
                                      & OuroError (S.exprPos rootTarget)
                                      & EvalError
                                      & pure

        S.Form _ (S.Symbol _ "get" : rootTarget : pathExpressions)
            -> case validatePathKeys pathExpressions of
                   EvalError err     -> pure (EvalError err)
                   -- Match on the open ArrayVal superset node instead of the old frozen GADT variant
                   Array     pathVal -> case resolvePath (Just evalExpr) env rootTarget pathVal of
                                              EvalError err -> pure (EvalError err)
                                              -- The path resolved cleanly to a final L.Expr, pass it forward
                                              resolvedVal   -> pure resolvedVal
                   _unexpectedExpr -> internalValueLeak "Path validation returned an unexpected L.Expr variant."
                                      & OuroError (S.exprPos rootTarget)
                                      & EvalError
                                      & pure

        S.Form _ [S.Symbol _ "quote", S.Symbol pos name] -> quoteVar pos name env
        S.Form _ [S.Symbol _ "quote", payload]           -> pure $ Quote payload
        S.Form _ (S.Symbol _ "list" : items)             -> Array <$> mapM evalExpr items

        S.Form _ [S.Symbol _ "attr", nameExpr, aExpr]
            -> do
               nameVal <- evalExpr nameExpr
               case nameVal of
                   Primitive (I.String name) -> Attr name <$> evalExpr aExpr
                   notAStr                   -> typeMismatch
                                                    "an expression that evaluates to String"
                                                    (humanReadableType notAStr)
                                                & OuroError (S.exprPos nameExpr)
                                                & EvalError
                                                & pure



        S.Form _ [S.Symbol pos "eval", qExpr]
            -> case runReader (evalExpr qExpr) env of
                   EvalError err   -> pure $ EvalError err
                   Quote     quote -> evalExpr quote
                   notAQuote       -> typeMismatch "a quoted expression"
                                                   ("a non quoted expression that was evaluated to " <> (humanReadableType notAQuote))
                                      & withBlurb (typeMismatchBlurb notAQuote)
                                      & OuroError pos
                                      & EvalError
                                      & pure


        S.Form _ (S.Symbol pos "case" : target : patterns)
            -> case (hasValidOtherwise patterns) of
                   True -> case runReader (evalExpr target) env of
                               EvalError err   -> pure $ EvalError err
                               Quote     quote -> patternMatch   evalExpr env pos quote          0 patterns
                               resolvedTarget  -> evaluateGuards evalExpr env pos resolvedTarget 0 patterns

                   False -> missingOtherwise
                            & withBlurb missingOtherwiseBlurb
                            & OuroError pos
                            & EvalError
                            & pure

        S.Form _ [S.Symbol _ "nth", index, target]
            -> case runReader (evalExpr index) env of
                   EvalError err
                       -> pure $ EvalError err

                   Primitive (I.Number i)
                       -> case runReader (evalExpr target) env of
                              Record _ attrs -> do
                                              let len = (length attrs)
                                                  idx = case i < 0 of
                                                              True  -> len + (floor i)
                                                              False -> floor i

                                              case snd <$> (V.fromList attrs) V.!? idx of
                                                  Just    val -> pure val
                                                  Nothing     -> indexOutOfBounds "Record" (floor i) len
                                                                  & OuroError (S.exprPos target)
                                                                  & EvalError
                                                                  & pure
                              Array xs -> do
                                          let len = (length xs)
                                              idx = case i < 0 of
                                                        True  -> len + (floor i)
                                                        False -> floor i

                                          case (V.fromList xs) V.!? idx of
                                              Just    val  -> pure val
                                              Nothing      -> indexOutOfBounds "Array" (floor i) len
                                                              & OuroError (S.exprPos target)
                                                              & EvalError
                                                              & pure
                              wrongType -> typeMismatch "either a Record or an Array" (humanReadableType wrongType)
                                           & OuroError (S.exprPos target)
                                           & EvalError
                                           & pure
                   notANum -> typeMismatch "an expression which evaluates to a Number" (humanReadableType notANum)
                              & OuroError (S.exprPos index)
                              & EvalError
                              & pure

        S.Form _ [S.Symbol _ "insert", posExpr, payloadExpr, structExpr]
            -> do
               posVal     <- evalExpr posExpr
               payloadVal <- evalExpr payloadExpr
               structVal  <- evalExpr structExpr

               case (posVal, payloadVal, structVal) of
                   -- Error Propagation
                   (EvalError e, _, _) -> pure $ EvalError e
                   (_, EvalError e, _) -> pure $ EvalError e
                   (_, _, EvalError e) -> pure $ EvalError e

                   -- Array insertion
                   (Primitive (I.Number i), pVal, Array elems@(x : _))
                        | L.structuralEq pVal x
                       -> let len = length elems
                              idx = case i < 0 of
                                        True  -> len + (floor i)
                                        False -> floor i

                              (before, after) = splitAt idx elems

                          in pure $ Array (before <> [payloadVal] <> after)

                   (Primitive (I.Number _), _, Array [])
                       -> pure $ Array [payloadVal]

                   -- Record insertion
                   (Primitive (I.Number i), Attr key value, Record meta kvs)
                       -> let len = length kvs
                              idx = case i < 0 of
                                        True  -> len + (floor i)
                                        False -> floor i

                              (before, after) = splitAt idx kvs

                          in pure $ Record meta (before <> [(key, value)] <> after)

                   -- Type Errors
                   (Primitive (I.Number _), _, _)
                       -> typeMismatch
                              "a datatype for insertion matching the target data structure"
                              ((humanReadableType payloadVal) <> " and " <> (humanReadableType structVal))
                          & OuroError (S.exprPos payloadExpr)
                          & EvalError
                          & pure

                   (_, _, _)
                       -> typeMismatch
                              "an expression which evaluates into a Number"
                              (humanReadableType posVal)
                          & OuroError (S.exprPos posExpr)
                          & EvalError
                          & pure

        S.Form pos allFields
            -> let wrappedEvaluator currentEnv expr' = runReader (evalExpr expr') currentEnv
               in case determineBlockTarget allFields of
                      TargetRecord      -> pure (compileRecord wrappedEvaluator env allFields)
                      TargetList        -> pure (compileArray wrappedEvaluator env pos allFields)
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
    let pos = S.exprPos payload

    case evaluatedVal of
        err@(EvalError _) -> pure err
        _notError         -> case tag of
                                 S.Uri     -> assertUri  pos evaluatedVal
                                 S.Date    -> assertDate pos evaluatedVal
                                 S.StrTag  -> isString   pos evaluatedVal
                                 S.NumTag  -> isNumber   pos evaluatedVal
                                 S.BoolTag -> isBoolean  pos evaluatedVal
                                 _badTag   -> malformedTag (T.pack (show tag)) "You cannot type assert an empty object or array"
                                              & OuroError pos
                                              & EvalError
                                              & pure

    where
    -- Typed Primitive Helpers
    isString pos v = case v of
                         Primitive (I.String _) -> pure v
                         other                  -> failMismatch pos "a String" (humanReadableType other) other

    isNumber pos v = case v of
                         Primitive (I.Number _) -> pure v
                         other                  -> failMismatch pos "a Number" (humanReadableType other) other

    isBoolean pos v = case v of
                          Primitive (I.Boolean _) -> pure v
                          other                   -> failMismatch pos "a Boolean" (humanReadableType other) other

    -- Complex Parsers
    assertUri :: SourcePos -> L.Expr -> EvalM L.Expr
    assertUri pos = \case
        Primitive (I.String rawText)
            -> case URI.mkURI rawText of
                   Left _
                       -> failMismatch pos
                              "a String matching URI layout"
                              "invalid URI format"
                              (Primitive $ I.String rawText)

                   Right uri
                       -> case URI.uriScheme uri of
                              Just _
                                  -> pure $ Primitive (I.URI uri)
                              Nothing
                                  -> failMismatch pos
                                         "a String matching a URI layout with a scheme included (https:)"
                                         "a String in an invalid URI format"
                                         (Primitive $ I.String rawText)

        Primitive (I.URI uri)
            -> pure $ Primitive (I.URI uri)

        otherVal
            -> failMismatch pos
                   "a String matching a URI layout with a scheme included (https:)"
                   (humanReadableType otherVal)
                   otherVal

    assertDate :: SourcePos -> L.Expr -> EvalM L.Expr
    assertDate pos =
        \case
         Primitive (I.String rawText)
             -> case parseISO8601 (T.unpack rawText) of
                    Just    utcTime -> pure $ Primitive (I.Date utcTime)
                    Nothing         -> failMismatch pos
                                           "a String matching ISO-8601 layout (YYYY-MM-DDTHH:mm:ssZ)"
                                           "a String in an invalid Date format"
                                           (Primitive $ I.String rawText)

         Primitive (I.Date date)
             -> pure $ Primitive (I.Date date)

         otherVal
             -> failMismatch pos
                    "a String matching ISO-8601 layout (YYYY-MM-DDTHH:mm:ssZ)"
                    (humanReadableType otherVal)
                    otherVal

    -- Reusable Validation Helpers
    failMismatch :: SourcePos -> Text -> Text -> L.Expr -> EvalM L.Expr
    failMismatch pos expected actual valForBlurb =
        typeMismatch expected actual
        & withBlurb (typeMismatchBlurb valForBlurb)
        & OuroError pos
        & EvalError
        & pure


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
                TemplateClosure closureEnv name params bodyExprs
                    -> compileTemplate evalExpr closureEnv pos name params bodyExprs argumentExprs

                PrimitiveOp nativeFunc
                    -> do
                       evaledArgs <- mapM evalExpr argumentExprs
                       env        <- ask
                       pure $ runReader (nativeFunc pos evaledArgs) env

                err@(EvalError _) -> pure err
                otherVal          -> unboundIdentifier (humanReadableType otherVal)
                                     & withBlurb ("The evaluator attempted to invoke the form head as a callable function handle, "
                                                <> "but the identifier resolved to an immutable "
                                                <> humanReadableType otherVal
                                                <> " primitive instead.\n\n"
                                                <> "Perhaps check that target value is in scope.")
                                     & OuroError pos
                                     & EvalError
                                     & pure

        [] -> pure $ EvalError $
            unbalancedDelimiter "an active form operator symbol" "Empty Brackets"
            & withBlurb ("Empty structural framing brackets are invalid executable values in Ouro. "
                      <> "An execution group must contain at least a primary invocation symbol or operator key.")
            & OuroError pos


evaluateGuards
    :: (S.Expr -> EvalM L.Expr)
    -> Env
    -> SourcePos
    -> L.Expr
    -> Int
    -> [S.Expr]
    -> EvalM L.Expr
evaluateGuards eval env pos target attempts branches =
    case branches of
        [] -> inexhaustiveCase "Case statement fell through" attempts
              & withBlurb (inexhaustiveCaseBlurb attempts)
              & OuroError pos
              & EvalError
              & pure

        -- Deconstruct the next branch form
        (S.Form _ [pattern, body] : rest)
            -> do
               -- Catch the otherwise reserved keyword
               case pattern of
                   S.Symbol _ "otherwise" -> eval body

                   -- If the symbol is something that indecates a bool then
                   -- case operates as a guard statement
                   S.Form _ (S.Symbol opPos op : guardArgs) | op `elem` [">=", ">", "<=", "<", "eq", "neq"]
                        -> do
                           guardRes <- evalGuardCond eval env opPos op target guardArgs
                           case guardRes of
                               EvalError err              -> pure $ EvalError err
                               Primitive (I.Boolean True) -> eval body
                               _nextCase                  -> evaluateGuards eval env pos target (attempts + 1) rest

                   -- If just a literal is passed in we can default to eq
                   S.Literal lpos lvalue
                       -> do
                          guardRes <- evalGuardCond eval env lpos "eq" target [S.Literal lpos lvalue]
                          case guardRes of
                              EvalError err              -> pure $ EvalError err
                              Primitive (I.Boolean True) -> eval body
                              _nextCase                  -> evaluateGuards eval env pos target (attempts + 1) rest


                   invalidPattern
                       -> inexhaustiveCase "Guard condition operator missing from internal builtin registry." attempts
                          & OuroError (S.exprPos invalidPattern)
                          & EvalError
                          & pure

        (_malformed : _)
            -> malformedCaseBranch
               & OuroError pos
               & EvalError
               & pure


patternMatch
    :: (S.Expr -> EvalM L.Expr)
    -> Env
    -> SourcePos
    -> S.Expr
    -> Int
    -> [S.Expr]
    -> EvalM L.Expr
patternMatch eval env pos qTrgt attempts branches =
    case branches of
        [] -> inexhaustiveCase "Case statement fell through" attempts
              & withBlurb (inexhaustiveCaseBlurb attempts)
              & OuroError pos
              & EvalError
              & pure

        -- Deconstruct the next branch form
        (S.Form _ [pattern, body] : rest)
            -> do
               -- Catch the otherwise reserved keyword
               case pattern of
                   S.Symbol _ "otherwise"
                       -> eval body

                   S.Quoted _ qPattern
                       -> case qTrgt `S.structuralEq` qPattern of
                              True  -> eval body
                              False -> patternMatch eval env pos qTrgt (attempts + 1) rest

                   notAQuote
                       -> inexhaustiveCase "PatternMatching requires quoted patterns" attempts
                          & OuroError (S.exprPos notAQuote)
                          & EvalError
                          & pure

        (_malformed : _)
            -> malformedCaseBranch
               & OuroError pos
               & EvalError
               & pure


hasValidOtherwise :: [S.Expr] -> Bool
hasValidOtherwise branches =
    case reverse branches of
        -- Check the very last branch in the list (the first element of the reversed list)
        (S.Form _ [S.Symbol _ "otherwise", _] : _) -> True
        _noOtherwise                               -> False


evalGuardCond
    :: (S.Expr -> EvalM L.Expr)
    -> Env
    -> SourcePos
    -> Text
    -> L.Expr
    -> [S.Expr]
    -> EvalM L.Expr
evalGuardCond eval env pos op target runtimeArgs =
    case Map.lookup op builtinRegistry of
        Just handler -> do
                        evaledArgs <- mapM eval runtimeArgs

                        -- Inject the target as the implicity first argument of the guard
                        let allArgs = target : evaledArgs

                        -- Reslove the builtin
                        let res = runReader (handler pos allArgs) env

                        -- Verify that the builtin resolved fully
                        pure res

        Nothing      -> astCorruption op "Guard condition operator missing from internal builtin registry."
                        & OuroError pos
                        & EvalError
                        & pure


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
