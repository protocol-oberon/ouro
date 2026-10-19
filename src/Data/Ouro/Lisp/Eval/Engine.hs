{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}

module Data.Ouro.Lisp.Eval.Engine where

import           Control.Monad                  (unless)
import           Control.Monad.Reader           (MonadReader (..), runReader)
import           Control.Monad.State.Strict     (gets)
import qualified Data.IntMap.Strict             as IntMap
import           Data.Function                  ((&))
import qualified Data.Map                       as Map
import           Data.Ouro.Error.Diagnostics    (astCorruption,
                                                 indexOutOfBounds,
                                                 inexhaustiveCase,
                                                 inexhaustiveCaseBlurb,
                                                 lexicalError,
                                                 malformedCaseBranch,
                                                 malformedTag, missingOtherwise,
                                                 missingOtherwiseBlurb,
                                                 missingPathKey,
                                                 propertyMismatch, targetMismatch,
                                                 typeMismatch,
                                                 typeMismatchBlurb,
                                                 unbalancedDelimiter, withBlurb)
import           Data.Ouro.Error.Types          (ErrorContext (..),
                                                 OuroError (..), TypeError (..))
import qualified Data.Ouro.Internal.Expr        as I
import           Data.Ouro.Lisp.Eval.Builtins   (builtinRegistry, parseISO8601)
import           Data.Ouro.Lisp.Eval.Pattern    (matchPattern)
import           Data.Ouro.Lisp.Eval.Scope      (allocFrame, allocValues, force,
                                                 letBindings, lookupVar)
import           Data.Ouro.Lisp.Eval.Structural (BlockTarget (..), alreadyAField,
                                                 compileArray, compileRecord,
                                                 determineBlockTarget,
                                                 evalFunction)
import           Data.Ouro.Lisp.Eval.Types      (Env (..), EvalM, Expr (..),
                                                 humanReadableType, propertyOf)
import qualified Data.Ouro.Lisp.Eval.Types      as L
import           Data.Ouro.Lisp.Function        (checkParams, isFunctionKeyword,
                                                 misplacedBinding, patternParam,
                                                 removedAttrForm, removedContextForm,
                                                 removedDefine, removedLocalDefn,
                                                 strayHole, typedResult)
import           Data.Ouro.Lisp.Module.Types    (Module)
import qualified Data.Ouro.Lisp.Surface         as S
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import           Lens.Micro.Platform            ((.~), (^.))
import           Text.Megaparsec                (SourcePos)
import qualified Text.URI                       as URI


-- High-level engine entry point.
-- Evaluates the graph, then forces every promise its output never needed (unused
-- let bindings and arguments), so that errors in them are reported too. Returns the
-- graph's value, with its errors embedded, and the errors found in those bindings.
evaluate :: Module -> S.Expr -> (L.Expr, [OuroError])
evaluate moduleEnv rootExpr = L.runEvalM (L.defaultEnv moduleEnv) $ do
    graph <- evalExpr rootExpr
    forceRemaining
    promises <- gets IntMap.elems
    pure (graph, concat [ L.errorsIn value | L.Forced value <- map (^. L.promiseState) promises ])

    where
    -- Forcing a promise can allocate new ones (e.g. a call's arguments), so repeat until none are left
    forceRemaining :: EvalM ()
    forceRemaining = do
        unforced <- gets (IntMap.toList . IntMap.filter isUnforced)
        unless (null unforced) $ do
            mapM_ (\(addr, promise) -> force evalExpr (promise ^. L.promisePos) addr) unforced
            forceRemaining

    isUnforced promise = case promise ^. L.promiseState of
                             L.Unforced -> True
                             _forced    -> False


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
        S.Symbol pos name          -> lookupVar evalExpr pos name env

        -- Holes left by 'Canon.placeholders': outside a call's arguments and case patterns
        S.Hole   pos hole          -> pure (EvalError (strayHole hole pos))

        --- INTERCEPT SPECIAL FORMS ---
        -- The old (context (...)) form: a record's context is now ':context value'
        S.Form pos (S.Symbol _ "context" : _)
            -> pure (EvalError (removedContextForm pos))

        -- The removed (attr key value) form: an attribute pair is written ':key value'
        S.Form pos (S.Symbol _ "attr" : _)
            -> pure (EvalError (removedAttrForm pos))

        -- 'get' walks the evaluated value, so it works through functions, inlays and case
        S.Form _ (S.Symbol _ "get" : rootTarget : pathExpressions)
            -> case validatePathKeys pathExpressions of
                   Left  err  -> pure (EvalError err)
                   Right keys -> (`getPath` keys) <$> evalExpr rootTarget

        S.Form _ (S.Symbol _ "list" : items)             -> Array <$> mapM evalExpr items

        -- (map f array): calls f on each element. Each call is an ordinary call, written
        -- in a frame that binds the function and the element to names that cannot be
        -- written in Ouro, so arity, signatures and the recursion check apply as usual.
        S.Form pos [S.Symbol _ "map", fnExpr, arrExpr]
            -> do
               fnVal  <- evalExpr fnExpr
               arrVal <- evalExpr arrExpr
               case (fnVal, arrVal) of
                   (EvalError err, _) -> pure (EvalError err)
                   (_, EvalError err) -> pure (EvalError err)

                   (f, _)
                       | not (isFunction f)
                       -> typeMismatch "a function to map with" (humanReadableType f)
                          & withBlurb "map calls a function on each element of an Array: (map f array), e.g. (map (+ ? 1) (1 2 3))."
                          & OuroError (S.exprPos fnExpr)
                          & EvalError
                          & pure

                   (f, arr)
                       | Just elems <- L.asArray arr
                       -> Array <$> mapM (callOn f) elems

                   (_, arr)
                       -> typeMismatch "an Array to map over" (humanReadableType arr)
                          & withBlurb "map calls a function on each element of an Array: (map f (1 2 3))."
                          & OuroError (S.exprPos arrExpr)
                          & EvalError
                          & pure
            where
            isFunction = \case
                FunctionClosure {} -> True
                PrimitiveOp _      -> True
                _notAFunction      -> False

            -- An element has no position of its own, so it is reported at the Array
            callOn f element = do
                frame <- allocValues (\names -> env
                                              & L.localScope       .~ names
                                              & L.functionRegistry .~ Map.empty
                                              & L.parentEnv        .~ Just env)
                                     pos
                                     [("%map-fn", f), ("%map-element", element)]
                local (const frame) $
                    applyFunction pos [S.Symbol (S.exprPos fnExpr) "%map-fn", S.Symbol (S.exprPos arrExpr) "%map-element"]

        S.Form pos (S.Symbol _ "map" : _)
            -> lexicalError "map is written (map f array): a function, then the Array to call it on."
               & withBlurb "Perhaps write (map (+ ? 1) (1 2 3)), or (map f xs) with a function f."
               & OuroError pos
               & EvalError
               & pure

        -- (%apply f x): a '->' step that is itself a function, such as a placeholder, called
        -- with the value threaded into it. A form headed by a fn is an array, so the function is
        -- bound to a name that cannot be written in Ouro, and the call is an ordinary one:
        -- arity, signatures and the recursion check apply as they do anywhere else.
        S.Form pos [S.Symbol _ "%apply", fnExpr, argExpr]
            -> evalExpr fnExpr >>= \case
                   EvalError err -> pure (EvalError err)
                   f             -> do
                                    frame <- allocValues (\names -> env
                                                                  & L.localScope       .~ names
                                                                  & L.functionRegistry .~ Map.empty
                                                                  & L.parentEnv        .~ Just env)
                                                         pos
                                                         [("%apply-fn", f)]
                                    local (const frame) $
                                        applyFunction pos [S.Symbol (S.exprPos fnExpr) "%apply-fn", argExpr]

        -- '->' and 'with' are rewritten before evaluation, so what reaches here has no value
        S.Form pos [S.Symbol _ "->"]
            -> lexicalError "-> is written (-> value step ...): a value, then the steps it is threaded through."
               & withBlurb "Each step receives the value before it as its first argument: (-> (base) (with :_label \"Name\") (f arg))."
               & OuroError pos
               & EvalError
               & pure

        S.Form pos [S.Symbol _ "with"]
            -> lexicalError "with is written (with value :key value ...): a value, then what to write over it."
               & withBlurb "Written attribute pairs win over the value's own, as in an inlay: (with (base) :_label \"Name\")."
               & OuroError pos
               & EvalError
               & pure

        -- (let ($name expr ...) expr): private bindings, in any order, around one result
        S.Form pos (S.Symbol _ "let" : rest)
            -> case rest of
                   [S.Form _ bindings, body]
                       -> case letBindings bindings of
                              Left  err     -> pure (EvalError err)
                              Right entries -> do
                                               (frame, _) <- allocFrame (\names -> env
                                                                                   & L.localScope       .~ names
                                                                                   & L.functionRegistry .~ Map.empty
                                                                                   & L.parentEnv        .~ Just env)
                                                                        entries
                                               local (const frame) (evalExpr body)
                   _malformed
                       -> lexicalError "A let is written (let ($name value ...) expr), with exactly one result expression."
                          & withBlurb "Perhaps wrap several values in parentheses to make an Array or a Record."
                          & OuroError pos
                          & EvalError
                          & pure

        -- 'define' and a local 'defn' were replaced by 'let' and 'fn'
        S.Form pos (S.Symbol _ "define" : _)
            -> pure (EvalError (removedDefine pos))

        S.Form pos (S.Symbol _ keyword : _)
            | isFunctionKeyword keyword
            -> pure (EvalError (removedLocalDefn pos))

        -- A private binding belongs in a let
        S.Form pos [S.Symbol _ "%binder", S.Literal _ (S.Str name)]
            -> pure (EvalError (misplacedBinding name pos))

        -- (fn (args) expr): an anonymous function, closed over the scope it is written in.
        -- A return type is read as a tag on the parameter list: (fn #num (n) expr).
        S.Form pos [S.Symbol _ "fn", S.Form _ params, body]
            | Just typed <- traverse param (zip [1 ..] params)
            -> pure (either EvalError (const (FunctionClosure env ("fn", pos) typed [body])) (checkParams pos typed))

        S.Form pos [S.Symbol _ "fn", S.Tagged tPos tag (S.Form _ params), body]
            | Just typed <- traverse param (zip [1 ..] params)
            -> pure (either EvalError (const (FunctionClosure env ("fn", pos) typed [typedResult (Just (tPos, tag)) body])) (checkParams pos typed))

        S.Form pos (S.Symbol _ "fn" : _)
            -> lexicalError "A fn is written (fn (args) expr): argument names or patterns, each with an optional type, then exactly one result expression."
               & withBlurb "A signature's types are properties: (fn #num (#num n) (* n 2))."
               & OuroError pos
               & EvalError
               & pure

        -- An attribute pair, ':key value', read by the parser as (%attr "key" value): its key
        -- is always written, never computed
        S.Form _ [S.Symbol _ "%attr", S.Literal _ (S.Str name), aExpr]
            -> Attr name <$> evalExpr aExpr



        S.Form _ (S.Symbol pos "case" : target : patterns)
            -> case (hasValidOtherwise patterns) of
                   True -> evalExpr target >>= \case
                               EvalError err   -> pure $ EvalError err
                               resolvedTarget  -> evaluateGuards evalExpr env pos   resolvedTarget 0 patterns

                   False -> missingOtherwise
                            & withBlurb missingOtherwiseBlurb
                            & OuroError pos
                            & EvalError
                            & pure

        S.Form _ [S.Symbol _ "nth", index, target]
            -> evalExpr index >>= \case
                   EvalError err
                       -> pure $ EvalError err

                   Primitive (I.Number i)
                       -> evalExpr target >>= \case
                              Record _ attrs -> do
                                                let len = (length attrs)
                                                    idx = case i < 0 of
                                                              True  -> len + (floor i)
                                                              False -> floor i

                                                case snd <$> (V.fromList attrs) V.!? idx of
                                                    Just    val -> pure val
                                                    Nothing     -> indexOutOfBounds "nth" "Record" (floor i) len
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
                                              Nothing      -> indexOutOfBounds "nth" "Array" (floor i) len
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

        S.Form _ [S.Symbol _ opName, posExpr, payloadExpr, structExpr]
            | opName `elem` ["insert", "replace-at"]
            -> do
               posVal     <- evalExpr posExpr
               payloadVal <- evalExpr payloadExpr
               structVal  <- evalExpr structExpr

               case (posVal, payloadVal, structVal) of
                   -- Error Propagation
                   (EvalError e, _, _) -> pure $ EvalError e
                   (_, EvalError e, _) -> pure $ EvalError e
                   (_, _, EvalError e) -> pure $ EvalError e

                   -- Array manipulation: any value can be an element, but a ':key value'
                   -- attribute pair belongs in a record
                   (Primitive (I.Number i), pVal, target)
                        | Just elems <- L.asArray target
                        , notAField pVal
                       -> let len     = length elems
                              idx     = case i < 0 of
                                            True  -> len + (floor i)
                                            False -> floor i
                              isValid = case opName == "insert" of
                                            True  -> idx >= 0 && idx <= len
                                            False -> idx >= 0 && idx < len

                          in case isValid of
                                 False -> indexOutOfBounds opName "Array" (floor i) len
                                          & OuroError (S.exprPos structExpr)
                                          & EvalError
                                          & pure
                                 True  -> let (before, after) = splitAt idx elems
                                              actualAfter = case opName == "replace-at" of
                                                                True  -> drop 1 after
                                                                False -> after
                                          in pure $ Array (before <> [payloadVal] <> actualAfter)

                   -- Record manipulation
                   (Primitive (I.Number i), Attr key value, Record meta kvs)
                       -> let len = length kvs
                              idx = case i < 0 of
                                        True  -> len + (floor i :: Int)
                                        False -> floor i

                              isValid = case opName == "insert" of
                                            True  -> idx >= 0 && idx <= len
                                            False -> idx >= 0 && idx < len

                          in case isValid of
                                 False -> indexOutOfBounds opName "Record" (floor i) len
                                              & OuroError (S.exprPos structExpr)
                                              & EvalError
                                              & pure
                                 True  -> let (before, after) = splitAt idx kvs
                                              actualAfter = case opName == "replace-at" of
                                                                True  -> drop 1 after
                                                                False -> after
                                              -- The attribute pairs the new one is placed among: a key can appear once
                                              others      = before <> actualAfter
                                          in case lookup key others of
                                                 Just _  -> pure $ EvalError (alreadyAField key (S.exprPos payloadExpr))
                                                 Nothing -> pure $ Record meta (before <> [(key, value)] <> actualAfter)

                   -- Type Errors
                   (Primitive (I.Number _), _, _)
                       -> typeMismatch
                              "a datatype for insertion matching the target data structure"
                              (humanReadableType payloadVal <> " and " <> humanReadableType structVal)
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

        S.Form pos allItems
            -> let wrappedEvaluator currentEnv expr' = local (const currentEnv) (evalExpr expr')
               in case determineBlockTarget allItems of
                      TargetRecord      -> compileRecord wrappedEvaluator env allItems
                      TargetList        -> compileArray  wrappedEvaluator env pos allItems
                      TargetFunctionApp -> applyFunction pos allItems

        otherNode
            -> let pos     = S.exprPos otherNode
                   context = Typing $ TypeMismatch
                               { expectedType = "a valid runtime configuration primitive value"
                               , actualType   = "an unrecognized compiler macro token shape"
                               }
               in pure $ EvalError $ OuroError pos context


-- assertTag.
--
-- Checks a value against a built-in property, as a signature or an assertion such as #str x
-- names it. Only #uri and #date convert: a String that passes them becomes a URI or a Date.
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
                                 S.RecTag  -> isRecord   pos evaluatedVal
                                 S.ArrTag  -> isArray    pos evaluatedVal
                                 S.FnTag   -> isFn       pos evaluatedVal
                                 _badTag   -> malformedTag (T.pack (show tag)) "#rec-empty and #arr-empty are values, not properties, so nothing can be asserted with them"
                                              & OuroError pos
                                              & EvalError
                                              & pure

    where
    -- Typed Primitive Helpers
    isString pos v = case v of
                         Primitive (I.String _) -> pure v
                         other                  -> failProperty pos "#str" (propertyOf other) (typeMismatchBlurb other)

    isNumber pos v = case v of
                         Primitive (I.Number _) -> pure v
                         other                  -> failProperty pos "#num" (propertyOf other) (typeMismatchBlurb other)

    isRecord pos v = case v of
                         Record _ _           -> pure v
                         Primitive I.EmptyRec -> pure v
                         other                -> failProperty pos "#rec" (propertyOf other) (typeMismatchBlurb other)

    isArray pos v = case L.asArray v of
                        Just _  -> pure v
                        Nothing -> failProperty pos "#arr" (propertyOf v) (typeMismatchBlurb v)

    isFn pos v = case v of
                     FunctionClosure _ _ _ _ -> pure v
                     PrimitiveOp _           -> pure v
                     other                   -> failProperty pos "#fn" (propertyOf other) (typeMismatchBlurb other)

    isBoolean pos v = case v of
                          Primitive (I.Boolean _) -> pure v
                          other                   -> failProperty pos "#bool" (propertyOf other) (typeMismatchBlurb other)

    -- Complex Parsers
    assertUri :: SourcePos -> L.Expr -> EvalM L.Expr
    assertUri pos = \case
        Primitive (I.String rawText)
            -> case URI.mkURI rawText of
                   Left _
                       -> failProperty pos "#uri" "a #str that is not a URI" uriBlurb

                   Right uri
                       -> case URI.uriScheme uri of
                              Just _
                                  -> pure $ Primitive (I.URI uri)
                              Nothing
                                  -> failProperty pos "#uri" "a #str without a URI scheme" uriBlurb

        Primitive (I.URI uri)
            -> pure $ Primitive (I.URI uri)

        otherVal
            -> failProperty pos "#uri" (propertyOf otherVal) uriBlurb

    assertDate :: SourcePos -> L.Expr -> EvalM L.Expr
    assertDate pos =
        \case
         Primitive (I.String rawText)
             -> case parseISO8601 (T.unpack rawText) of
                    Just    utcTime -> pure $ Primitive (I.Date utcTime)
                    Nothing         -> failProperty pos "#date" "a #str that is not a date" dateBlurb

         Primitive (I.Date date)
             -> pure $ Primitive (I.Date date)

         otherVal
             -> failProperty pos "#date" (propertyOf otherVal) dateBlurb

    uriBlurb  = "A #uri is a #str with a scheme, such as \"https://linked.art/example/1\"."
    dateBlurb = "A #date is a #str in ISO-8601 form, YYYY-MM-DDTHH:mm:ssZ, such as \"1642-01-01T00:00:00Z\"."

    -- A value without the built-in property it is typed with
    failProperty :: SourcePos -> Text -> Text -> Text -> EvalM L.Expr
    failProperty pos expected actual blurb =
        propertyMismatch expected actual
        & withBlurb blurb
        & OuroError pos
        & EvalError
        & pure


-- applyFunction.
--
-- Unpacks evaluated argument vectors and triggers platform-native executors.
-- Operates entirely within the evaluator monad (EvalM).
applyFunction :: SourcePos -> [S.Expr] -> EvalM L.Expr
applyFunction pos exprs =
    case exprs of
        (operatorExpr : argumentExprs)
            -> do
               resolvedOp   <- evalExpr operatorExpr

               case resolvedOp of
                   FunctionClosure closureEnv name params bodyExprs
                       -> evalFunction evalExpr closureEnv pos name params bodyExprs argumentExprs

                   PrimitiveOp nativeFunc
                       -> do
                          evaledArgs <- mapM evalExpr argumentExprs
                          env        <- ask
                          pure $ runReader (nativeFunc pos evaledArgs) env

                   err@(EvalError _) -> pure err
                   otherVal          -> let what = case operatorExpr of
                                                       S.Symbol _ name -> "'" <> name <> "'"
                                                       _notAName       -> "This expression"
                                        in typeMismatch "a function to call" (humanReadableType otherVal)
                                           & withBlurb ( what <> " is " <> humanReadableType otherVal <> ", not a function, so it cannot be called."
                                                      <> "\n\nFunctions and values share one namespace: a value with the same name as a function shadows it."
                                                       )
                                           & OuroError pos
                                           & EvalError
                                           & pure

        [] -> unbalancedDelimiter "an active form operator symbol" "Empty Brackets"
              & withBlurb ( "Empty structural framing brackets are invalid executable values in Ouro. "
                         <> "An execution group must contain at least a primary invocation symbol or operator key."
                          )
              & OuroError pos
              & EvalError
              & pure


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

                   -- A guard, marked by the desugarer: (%guard op operand ...)
                   S.Form _ (S.Symbol _ "%guard" : S.Symbol opPos op : guardArgs)
                        -> do
                           guardRes <- evalGuardCond eval env opPos op target guardArgs
                           case guardRes of
                               EvalError err              -> pure $ EvalError err
                               Primitive (I.Boolean True) -> eval body
                               _nextCase                  -> evaluateGuards eval env pos target (attempts + 1) rest

                   -- Destructuring: a record form, or a form containing '?' holes
                   _ | S.isPatternForm pattern
                       -> do
                          matchRes <- matchPattern eval pattern target
                          case matchRes of
                              Left  err             -> pure $ EvalError err
                              Right Nothing         -> evaluateGuards eval env pos target (attempts + 1) rest
                              -- The holes are bound, as already-evaluated promises, in a new frame around the branch body
                              Right (Just bindings) -> do
                                                       scope        <- ask
                                                       patternFrame <- allocValues (\names -> scope
                                                                                              & L.localScope       .~ names
                                                                                              & L.functionRegistry .~ Map.empty
                                                                                              & L.parentEnv        .~ Just scope)
                                                                                   (S.exprPos pattern)
                                                                                   bindings
                                                       local (const patternFrame) (eval body)

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

                        -- The target is the guard's first operand: ((< 10) ...) asks (< target 10).
                        -- With more operands the comparison chains, like (< target 10 20).
                        -- 'neq' does not chain, so its handler reports the wrong operand count.
                        let operands    = target : evaledArgs
                            compareWith = \a b -> runReader (handler pos [a, b]) env

                        pure $ case op /= "neq" && length operands > 2 of
                                   True  -> allOf (zipWith compareWith operands (drop 1 operands))
                                   False -> runReader (handler pos operands) env

        Nothing      -> astCorruption op "Guard condition operator missing from internal builtin registry."
                        & OuroError pos
                        & EvalError
                        & pure


    where
    -- The pairwise results of a chained comparison: the first error, or whether all hold
    allOf results =
        case [ err | err@(EvalError _) <- results ] of
            err : _  -> err
            []       -> Primitive (I.Boolean (and [ b | Primitive (I.Boolean b) <- results ]))


-- validatePathKeys.
--
-- Checks that every path component after the 'get' target is a bare symbol, and
-- returns the keys with their positions. A colon-prefixed attribute or any other
-- form is rejected with a hint.
validatePathKeys :: [S.Expr] -> Either OuroError [(Text, SourcePos)]
validatePathKeys = traverse $ \case
    S.Symbol keyPos k
        -> Right (k, keyPos)

    S.Form aPos (S.Symbol _ "%attr" : S.Literal _ (S.Str rawAttr) : _)
        -> targetMismatch "a lookup Symbol path component" (":" <> rawAttr)
           & withBlurb ( "The 'get' path operator expects bare lookup symbols (e.g., properties) "
                      <> "to traverse target graph layers. You provided a colon-prefixed attribute identifier.\n\n"
                      <> "Perhaps remove the leading colon operator from '" <> ":" <> rawAttr <> "' to "
                      <> "transition the token from a property key to an active navigation handle."
                       )
           & OuroError aPos
           & Left

    badNode
        -> targetMismatch "a lookup Symbol property identifier" "a structural node form"
           & withBlurb ( "Path navigation parameters following the target node must evaluate "
                      <> "strictly to atomic path components. Compound S-Expression trees, properties, "
                      <> "and loose primitive values cannot be read as horizontal layout lookup slots."
                       )
           & OuroError (S.exprPos badNode)
           & Left


-- getPath.
--
-- Walks a path of keys through an evaluated value, one Record at a time. Only a
-- Record's attribute pairs can be reached: a 'define' is private to the block it is written in.
getPath :: L.Expr -> [(Text, SourcePos)] -> L.Expr
getPath value = \case
    [] -> value

    (key, keyPos) : rest
        -> case value of
               EvalError err        -> EvalError err
               Record _ pairs       -> lookupKey key keyPos (map fst pairs) (lookup key pairs) rest
               Primitive I.EmptyRec -> lookupKey key keyPos [] Nothing rest
               notARecord           -> typeMismatch ("a Record to look up '" <> key <> "' in") (humanReadableType notARecord)
                                       & withBlurb "Only the attribute pairs of a Record can be reached with 'get'. To index an Array, use (nth index array)."
                                       & OuroError keyPos
                                       & EvalError

    where
    lookupKey key keyPos available found rest =
        case found of
            Just attrValue -> getPath attrValue rest
            Nothing         -> missingPathKey key available
                               & withBlurb ( "The Record has no attribute '" <> key <> "'."
                                          <> "\n\nValues written in a 'define' are private to their block and cannot be reached with 'get'."
                                           )
                               & OuroError keyPos
                               & EvalError


-- A ':key value' attribute pair, as a value, belongs in a record, not an array
notAField :: L.Expr -> Bool
notAField = \case
    Attr _ _     -> False
    _notAField   -> True


-- A function parameter is a name or a pattern, optionally typed by the tag before it:
-- n, #num n, (?a ?b) or #arr (?a ?b). A pattern is named by its position among the parameters.
-- A '$name' is read as (%binder "name"), which is a binding, not a pattern.
param :: (Int, S.Expr) -> Maybe S.Param
param (index, expr) = case expr of
    S.Symbol _ name                       -> Just (S.Param name Nothing Nothing)
    S.Tagged pos tag (S.Symbol _ name)    -> Just (S.Param name (Just (pos, tag)) Nothing)
    S.Form _ (S.Symbol _ "%binder" : _)   -> Nothing
    pattern@(S.Form _ _)                  -> Just (patternParam index Nothing pattern)
    S.Tagged pos tag pattern@(S.Form _ _) -> Just (patternParam index (Just (pos, tag)) pattern)
    _notAParameter                        -> Nothing
