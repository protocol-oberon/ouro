{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}

module Data.Ouro.Lisp.Eval.Engine where

import           Control.Monad                  (foldM, unless)
import           Control.Monad.Reader           (MonadReader (..), runReader)
import           Control.Monad.State.Strict     (gets)
import qualified Data.IntMap.Strict             as IntMap
import           Data.Function                  ((&))
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe)
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
import           Data.Ouro.Lisp.Eval.Builtins   (builtinRegistry, distinctValues, parseISO8601)
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
                                                 removedDefine, removedLocalDefn, removedNth,
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
-- The graph is evaluated in its module's root frame, with the modules it imports.
evaluate :: Map.Map Text L.Imported -> Module -> S.Expr -> (L.Expr, [OuroError])
evaluate imported moduleEnv rootExpr = L.runEvalM (L.defaultEnv imported moduleEnv) $ do
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

        -- A property on its own, #str with nothing after it, is the subject-less form of
        -- (satisfies #str), which only a case branch head fills in
        S.Tagged pos _ (S.Hole _ "%subject")
            -> lexicalError "A property on its own has nothing to check: a property is written before the value it applies to, as in #str x."
               & withBlurb "In a case branch head, (satisfies #str) asks it of the case's target: (case x ((satisfies #str) \"text\") (otherwise \"other\"))."
               & OuroError pos
               & EvalError
               & pure

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
            -> case validatePath pathExpressions of
                   Left  err  -> pure (EvalError err)
                   Right keys -> (`getPath` keys) <$> evalExpr rootTarget

        -- (has record key ...): whether the path reaches a value. A missing key or an index
        -- past the end is false; anything else that stops the walk is reported as get reports it.
        S.Form _ (S.Symbol _ "has" : rootTarget : pathExpressions@(_ : _))
            -> case validatePath pathExpressions of
                   Left  err  -> pure (EvalError err)
                   Right keys -> (\root -> case findPath root keys of
                                               Nothing                -> Primitive (I.Boolean False)
                                               Just err@(EvalError _) -> err
                                               Just _found            -> Primitive (I.Boolean True))
                                 <$> evalExpr rootTarget

        -- (get-or default record key ...): the value at the path, or the default when a record
        -- on the way has no such key or an Array no such index. The default is evaluated only when it is used.
        S.Form _ (S.Symbol _ "get-or" : defaultExpr : rootTarget : pathExpressions@(_ : _))
            -> case validatePath pathExpressions of
                   Left  err  -> pure (EvalError err)
                   Right keys -> evalExpr rootTarget >>= \root -> case findPath root keys of
                                     Nothing    -> evalExpr defaultExpr
                                     Just value -> pure value

        S.Form pos (S.Symbol _ "has" : _)
            -> lexicalError "has is written (has record key ...): a Record, then the path of keys to look for."
               & withBlurb "Perhaps write (has book ntp46), which is true when the Record has an attribute ntp46."
               & OuroError pos
               & EvalError
               & pure

        S.Form pos (S.Symbol _ "get-or" : _)
            -> lexicalError "get-or is written (get-or default record key ...): the value to use when a key is missing, a Record, then the path of keys."
               & withBlurb "Perhaps write (get-or () book ntp46), which is the Record's ntp46, or () when it has none."
               & OuroError pos
               & EvalError
               & pure

        -- (image record key ...): the values a path reaches, read as a set, as JSON-LD
        -- reads a property's values. It goes through Arrays, a missing key adds nothing,
        -- and the result has no repeats, kept in the order they are reached.
        S.Form pos (S.Symbol _ "image" : rootTarget : pathExpressions@(_ : _))
            -> case validatePath pathExpressions of
                   Left  err  -> pure (EvalError err)
                   Right keys -> evalExpr rootTarget >>= \root -> case imageOf root keys of
                                     Left  err    -> pure err
                                     Right values -> runReader (distinctValues pos "image" values) <$> ask

        S.Form pos (S.Symbol _ "image" : _)
            -> lexicalError "image is written (image record key ...): a Record, or an Array of them, then the path of keys to follow."
               & withBlurb "Perhaps write (image book part classified_as), every classification of every part."
               & OuroError pos
               & EvalError
               & pure

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
                       | not (callable f)
                       -> typeMismatch "a function to map with" (humanReadableType f)
                          & withBlurb "map calls a function on each element of an Array: (map f array), e.g. (map (+ ? 1) (1 2 3))."
                          & OuroError (S.exprPos fnExpr)
                          & EvalError
                          & pure

                   (f, arr)
                       | Just elems <- L.asArray arr
                       -> Array <$> mapM (callOnElement pos fnExpr arrExpr f) elems

                   (_, arr)
                       -> typeMismatch "an Array to map over" (humanReadableType arr)
                          & withBlurb "map calls a function on each element of an Array: (map f (1 2 3))."
                          & OuroError (S.exprPos arrExpr)
                          & EvalError
                          & pure

        S.Form pos (S.Symbol _ "map" : _)
            -> lexicalError "map is written (map f array): a function, then the Array to call it on."
               & withBlurb "Perhaps write (map (+ ? 1) (1 2 3)), or (map f xs) with a function f."
               & OuroError pos
               & EvalError
               & pure

        -- (exists f array), (forall f array), (omits f array), (filter f array): f is called
        -- on each element, as map calls it, and must answer with a Boolean. exists, forall
        -- and omits stop at the element that decides the answer, as or and and stop at an
        -- operand; omits is exists the other way round.
        S.Form pos [S.Symbol _ form, fnExpr, arrExpr]
            | form `elem` ["exists", "forall", "omits", "filter"]
            -> do
               fnVal  <- evalExpr fnExpr
               arrVal <- evalExpr arrExpr
               case (fnVal, arrVal) of
                   (EvalError err, _) -> pure (EvalError err)
                   (_, EvalError err) -> pure (EvalError err)

                   (f, _)
                       | not (callable f)
                       -> typeMismatch ("a function for " <> form <> " to call") (humanReadableType f)
                          & withBlurb (predicateUsage form)
                          & OuroError (S.exprPos fnExpr)
                          & EvalError
                          & pure

                   (f, arr)
                       | Just elems <- L.asArray arr
                       -> let answer element = predicateVerdict form (S.exprPos arrExpr)
                                               <$> callOnElement pos fnExpr arrExpr f element
                          in case form of
                                 "exists" -> decideOn True  answer elems
                                 "forall" -> decideOn False answer elems
                                 "omits"  -> negateAnswer <$> decideOn True answer elems
                                 _filter  -> keepWhere answer elems

                   (_, arr)
                       -> typeMismatch ("an Array for " <> form <> " to go through") (humanReadableType arr)
                          & withBlurb (predicateUsage form)
                          & OuroError (S.exprPos arrExpr)
                          & EvalError
                          & pure

        S.Form pos (S.Symbol _ form : _)
            | form `elem` ["exists", "forall", "omits", "filter"]
            -> lexicalError (form <> " is written (" <> form <> " f array): a function that answers true or false, then the Array to call it on.")
               & withBlurb (predicateUsage form)
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

        -- (when cond) is the head of a case branch; anywhere else it is misplaced
        S.Form pos (S.Symbol _ "when" : _)
            -> lexicalError "when is written as the head of a case branch, with one condition: ((when cond) result)."
               & withBlurb "Perhaps write (case x ((when (has x key)) \"yes\") (otherwise \"no\"))."
               & OuroError pos
               & EvalError
               & pure

        -- (satisfies #p x): whether x has the property #p, as a Boolean. Only the property's
        -- check failing is false: an error in x, or a property that is not one, stays an error
        S.Form _ [S.Symbol _ "satisfies", S.Tagged tagPos tag subject]
            | not (isSubjectHole subject)
            -> evalExpr subject >>= \case
                   err@(EvalError _) -> pure err
                   value             -> checkProperty tag tagPos value >>= \case
                                            EvalError (OuroError _ ctx) | isPropertyMismatch ctx -> pure (Primitive (I.Boolean False))
                                            err@(EvalError _)                                    -> pure err
                                            _hasIt                                               -> pure (Primitive (I.Boolean True))

        S.Form pos (S.Symbol _ "satisfies" : _)
            -> lexicalError "satisfies is written (satisfies #property value), or as the head of a case branch, ((satisfies #property) result)."
               & withBlurb "Perhaps write (satisfies #str x), or (case x ((satisfies #str) \"text\") (otherwise \"other\"))."
               & OuroError pos
               & EvalError
               & pure

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

        -- The removed (nth index array) form: an index is written in get's path
        S.Form pos (S.Symbol _ "nth" : _)
            -> pure (EvalError (removedNth pos))

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
    case evaluatedVal of
        err@(EvalError _) -> pure err
        _notError         -> checkProperty tag (S.exprPos payload) evaluatedVal


-- Checks an evaluated value against a built-in property: the value, converted for #uri and
-- #date, or a PropertyMismatch error at the position given
checkProperty :: S.ReaderTag -> SourcePos -> L.Expr -> EvalM L.Expr
checkProperty tag pos evaluatedVal = case tag of
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

                   -- A condition, written by the desugarer from (when cond) or a guard: true
                   -- takes the branch, false tries the next, and anything else is an error
                   S.Form _ [S.Symbol _ "%when", cond]
                        -> eval cond >>= \case
                               Primitive (I.Boolean True)  -> eval body
                               Primitive (I.Boolean False) -> evaluateGuards eval env pos target (attempts + 1) rest
                               err@(EvalError _)           -> pure err
                               other                       -> propertyMismatch "#bool" (propertyOf other)
                                                              & withBlurb "A case condition answers true or false: ((when cond) result), or a comparison such as ((< 10) result)."
                                                              & OuroError (S.exprPos cond)
                                                              & EvalError
                                                              & pure

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
                   S.Literal lpos _
                       -> do
                          literal <- eval pattern
                          case literalMatch env lpos target literal of
                              err@(EvalError _)          -> pure err
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


-- The subject of a property written on its own, (satisfies #p), which a case fills in
isSubjectHole :: S.Expr -> Bool
isSubjectHole = \case
    S.Hole _ "%subject" -> True
    _notTheSubject      -> False


-- Whether an error is a value lacking a property, under any blurbs written on it
isPropertyMismatch :: ErrorContext -> Bool
isPropertyMismatch = \case
    Typing PropertyMismatch {} -> True
    AnnotatedContext _ ctx     -> isPropertyMismatch ctx
    _otherError                -> False


-- A literal branch compares the target with its literal, as eq compares them
literalMatch :: Env -> SourcePos -> L.Expr -> L.Expr -> L.Expr
literalMatch env pos target literal =
    case Map.lookup "eq" builtinRegistry of
        Just handler -> runReader (handler pos [target, literal]) env
        Nothing      -> astCorruption "eq" "The eq built-in is missing from the registry."
                        & OuroError pos
                        & EvalError


-- One step of a path: a key into a Record, or an index into an Array
data PathStep
    = Key   Text
    | Index Int


-- validatePath.
--
-- Checks that every path component after the 'get' target is a bare symbol or a whole
-- number, and returns the steps with their positions. A colon-prefixed attribute, a
-- fraction, a hole or any other form is rejected with a hint. A hole reaches here as the
-- name the placeholders pass gave it, which cannot be written: '%1' for '?', and '?k' for '?k'.
validatePath :: [S.Expr] -> Either OuroError [(PathStep, SourcePos)]
validatePath = traverse $ \case
    S.Symbol holePos k
        | isHoleName k
        -> holeInPath holePos

    S.Tagged holePos _ (S.Symbol _ k)
        | isHoleName k
        -> holeInPath holePos

    S.Symbol keyPos k
        -> Right (Key k, keyPos)

    S.Literal indexPos (S.Num n)
        | n == fromIntegral (floor n :: Int)
        -> Right (Index (floor n), indexPos)

        | otherwise
        -> targetMismatch "a whole number to index an Array with" (T.pack (show n))
           & withBlurb "An index in a path counts the elements of an Array: (get r part 0) is the first part."
           & OuroError indexPos
           & Left

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

    where
    isHoleName k = any (`T.isPrefixOf` k) ["%", "?"]

    -- Paths are written, so a hole cannot stand for a step: that would make a key or an
    -- index computed, which Ouro never allows
    holeInPath pos =
        lexicalError "A path is written: a hole cannot stand for a key or an index."
        & withBlurb ( "A hole can stand for the target: (get ? 0) is a function that takes the first element. "
                   <> "Keys and indexes are always written, so a path never depends on the values it is given."
                    )
        & OuroError pos
        & Left


-- getPath.
--
-- Walks a path through an evaluated value: a key into a Record, an index into an Array.
-- Only a Record's attribute pairs can be reached: a 'define' is private to the block it is written in.
getPath :: L.Expr -> [(PathStep, SourcePos)] -> L.Expr
getPath value = \case
    [] -> value

    (step, stepPos) : rest
        -> case (step, value) of
               (_, EvalError err)                -> EvalError err
               (Key key, Record _ pairs)         -> lookupKey key stepPos (map fst pairs) (lookup key pairs) rest
               (Key key, Primitive I.EmptyRec)   -> lookupKey key stepPos [] Nothing rest
               (Key key, notARecord)             -> typeMismatch ("a Record to look up '" <> key <> "' in") (humanReadableType notARecord)
                                                    & withBlurb (keyBlurb key notARecord)
                                                    & OuroError stepPos
                                                    & EvalError
               (Index i, other)
                   | Just xs <- L.asArray other  -> case elementAt i xs of
                                                        Just x  -> getPath x rest
                                                        Nothing -> indexOutOfBounds "get" "Array" i (length xs)
                                                                   & OuroError stepPos
                                                                   & EvalError
               (Index i, notAnArray)             -> typeMismatch ("an Array to take element " <> T.pack (show i) <> " of") (humanReadableType notAnArray)
                                                    & withBlurb "An index in a path counts the elements of an Array. A Record's attribute pairs are reached by their keys."
                                                    & OuroError stepPos
                                                    & EvalError

    where
    keyBlurb key = \case
        Array _              -> arrayBlurb key
        Primitive I.EmptyArr -> arrayBlurb key
        _notAnArray          -> "Only the attribute pairs of a Record can be reached by key, and the elements of an Array by index."

    arrayBlurb key = "An Array's elements are reached by index: write one before '" <> key <> "', as in (get r part 0 " <> key <> "), "
                  <> "or read '" <> key <> "' from every element with image."

    lookupKey key keyPos available found rest =
        case found of
            Just attrValue -> getPath attrValue rest
            Nothing         -> missingPathKey key available
                               & withBlurb ( "The Record has no attribute '" <> key <> "'."
                                          <> "\n\nValues written in a 'define' are private to their block and cannot be reached with 'get'."
                                           )
                               & OuroError keyPos
                               & EvalError


-- The value at a path, or Nothing when a record on the way has no such key or an Array no
-- such index. Anything else that stops the walk, an error or a value of the wrong kind, is
-- the value 'get' would give, so that has and get-or report it as get does.
findPath :: L.Expr -> [(PathStep, SourcePos)] -> Maybe L.Expr
findPath value = \case
    []                      -> Just value
    path@((step, _) : rest) -> case (step, value) of
        (Key key, Record _ pairs)        -> lookup key pairs >>= (`findPath` rest)
        (Key _, Primitive I.EmptyRec)    -> Nothing
        (Index i, other)
            | Just xs <- L.asArray other -> elementAt i xs >>= (`findPath` rest)
        _stopped                         -> Just (getPath value path)


-- The element at an index, counting from the end when it is negative
elementAt :: Int -> [a] -> Maybe a
elementAt i xs = V.fromList xs V.!? (if i < 0 then length xs + i else i)


-- imageOf.
--
-- The values a path reaches, read as JSON-LD reads a property's values: an Array is its
-- elements and any other value is one value. Each key is followed from every value reached
-- so far, and a Record without the key adds nothing. A value on the way that is not a
-- Record is an error, as it is for get, and so is the first error reached. A set has no
-- positions, so an index is an error too.
imageOf :: L.Expr -> [(PathStep, SourcePos)] -> Either L.Expr [L.Expr]
imageOf root = foldM step (valuesOf root)
    where
    step reached = \case
        (Key key, keyPos)  -> concat <$> traverse (follow key keyPos) reached
        (Index _, indexPos) -> typeMismatch "a key to follow" "an index"
                               & withBlurb "image reads a path as a set of values, and a set has no positions. To take one element, use get: (get r part 0)."
                               & OuroError indexPos
                               & EvalError
                               & Left

    follow key keyPos = \case
        err@(EvalError _)    -> Left err
        Record _ pairs       -> Right (maybe [] valuesOf (lookup key pairs))
        Primitive I.EmptyRec -> Right []
        notARecord           -> typeMismatch ("a Record to follow '" <> key <> "' from") (humanReadableType notARecord)
                                & withBlurb ( "image follows keys through Records, and through the elements of an Array. "
                                           <> "A value on the way that is neither has no attribute pairs to follow, "
                                           <> "and an Array inside an Array is not one of a property's values."
                                            )
                                & OuroError keyPos
                                & EvalError
                                & Left

    valuesOf value = fromMaybe [value] (L.asArray value)


-- A value map, exists, forall, omits and filter can call on each element
callable :: L.Expr -> Bool
callable = \case
    FunctionClosure {} -> True
    PrimitiveOp _      -> True
    _notAFunction      -> False


-- Calls a function on one element of an Array, for map, exists, forall, omits and filter. The call
-- is an ordinary one, made in a frame that binds the function and the element to names
-- that cannot be written in Ouro, so arity, signatures and the recursion check apply as
-- they do anywhere else. An element has no position of its own, so it is reported at the
-- Array.
callOnElement :: SourcePos -> S.Expr -> S.Expr -> L.Expr -> L.Expr -> EvalM L.Expr
callOnElement pos fnExpr arrExpr f element = do
    env   <- ask
    frame <- allocValues (\names -> env
                                  & L.localScope       .~ names
                                  & L.functionRegistry .~ Map.empty
                                  & L.parentEnv        .~ Just env)
                         pos
                         [("%map-fn", f), ("%map-element", element)]
    local (const frame) $
        applyFunction pos [S.Symbol (S.exprPos fnExpr) "%map-fn", S.Symbol (S.exprPos arrExpr) "%map-element"]


-- The answer of one call of exists', forall's, omits' or filter's function: a Boolean, or the
-- value to stop with, an error reported at the Array
predicateVerdict :: Text -> SourcePos -> L.Expr -> Either L.Expr Bool
predicateVerdict form arrPos = \case
    Primitive (I.Boolean b) -> Right b
    err@(EvalError _)       -> Left err
    other                   -> propertyMismatch "#bool" (propertyOf other)
                               & withBlurb (form <> "'s function must answer true or false for each element.")
                               & OuroError arrPos
                               & EvalError
                               & Left


-- exists, forall and omits: stop at the first element whose answer is the deciding one
decideOn :: Bool -> (L.Expr -> EvalM (Either L.Expr Bool)) -> [L.Expr] -> EvalM L.Expr
decideOn deciding answer = \case
    []             -> pure (Primitive (I.Boolean (not deciding)))
    element : rest -> answer element >>= \case
                          Left stop                -> pure stop
                          Right b | b == deciding  -> pure (Primitive (I.Boolean deciding))
                                  | otherwise      -> decideOn deciding answer rest


-- omits: the answer exists gives, the other way round. An error it stopped at is kept as
-- it is.
negateAnswer :: L.Expr -> L.Expr
negateAnswer = \case
    Primitive (I.Boolean b) -> Primitive (I.Boolean (not b))
    stop                    -> stop


-- filter: the elements whose answer is true, in order
keepWhere :: (L.Expr -> EvalM (Either L.Expr Bool)) -> [L.Expr] -> EvalM L.Expr
keepWhere answer = go []
    where
    go kept = \case
        []             -> pure (Array (reverse kept))
        element : rest -> answer element >>= \case
                              Left stop   -> pure stop
                              Right True  -> go (element : kept) rest
                              Right False -> go kept rest


predicateUsage :: Text -> Text
predicateUsage form =
    form <> " calls a function that answers true or false on each element of an Array: ("
    <> form <> " f array), e.g. (" <> form <> " (> ? 1) (1 2 3))."


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
