{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}

module Data.Ouro.Lisp.Eval.Engine where

import           Control.Monad                  (foldM, unless)
import           Control.Monad.Reader           (MonadReader (..), runReader)
import           Control.Monad.State.Strict     (gets, modify)
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
                                                 missingOtherwise,
                                                 missingOtherwiseBlurb,
                                                 missingPathKey, notARefinement,
                                                 refinementMismatch, targetMismatch,
                                                 typeMismatch,
                                                 unbalancedDelimiter,
                                                 unknownRefinement, withBlurb)
import           Data.Ouro.Error.Types          (OuroError (..))
import qualified Data.Ouro.Internal.Expr        as I
import           Data.Ouro.Lisp.Analysis.Forms  (BlockTarget (..),
                                                 determineBlockTarget,
                                                 hasValidOtherwise,
                                                 isSubjectHole, letBindings,
                                                 param, shadowsForm)
import           Data.Ouro.Lisp.Eval.Builtins   (builtinRegistry, distinctValues)
import           Data.Ouro.Lisp.Eval.Pattern    (matchPattern)
import           Data.Ouro.Lisp.Eval.Scope      (allocFrame, allocValues, boundIn,
                                                 force, lookupVar)
import           Data.Ouro.Lisp.Eval.Structural (alreadyAField, compileArray,
                                                 compileRecord, evalFunction)
import           Data.Ouro.Lisp.Eval.Types      (Env (..), EvalM, Expr (..),
                                                 humanReadableType)
import qualified Data.Ouro.Lisp.Eval.Types      as L
import           Data.Ouro.Lisp.Function        (checkParams, isFunctionKeyword,
                                                 misplacedBinding,
                                                 removedAttrForm, removedContextForm,
                                                 removedLocalDefn, removedNth,
                                                 specialForms, strayHole)
import           Data.Ouro.Lisp.Module.Types    (Module)
import qualified Data.Ouro.Lisp.Module.Types    as M
import qualified Data.Ouro.Lisp.Surface         as S
import           Data.Ouro.Refinement           (Evaluator (..), Role (..),
                                                 checkRefinement, isOutside,
                                                 refinementOf)
import qualified Data.Ouro.Refinement           as R
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import           Lens.Micro.Platform            ((%~), (.~), (^.))
import           Text.Megaparsec                (SourcePos)


-- High-level engine entry point.
-- Evaluates the graph, then forces every promise its output never needed (unused
-- let bindings and arguments), so that errors in them are reported too. Returns the
-- graph's value, with its errors embedded, and the errors found in those bindings.
-- The graph is evaluated in its module's root frame, with the modules it imports.
evaluate :: Map.Map Text L.Imported -> Module -> S.Expr -> (L.Expr, [OuroError])
evaluate imported moduleEnv rootExpr = L.runEvalM (L.defaultEnv imported moduleEnv) $ do
    graph <- evalExpr rootExpr
    forceRemaining
    promises <- gets (IntMap.elems . (^. L.promises))
    pure (graph, concat [ L.errorsIn value | L.Forced value <- map (^. L.promiseState) promises ])

    where
    -- Forcing a promise can allocate new ones (e.g. a call's arguments), so repeat until none are left
    forceRemaining :: EvalM ()
    forceRemaining = do
        unforced <- gets (IntMap.toList . IntMap.filter isUnforced . (^. L.promises))
        unless (null unforced) $ do
            mapM_ (\(addr, promise) -> force evalExpr (promise ^. L.promisePos) addr) unforced
            forceRemaining

    isUnforced promise = case promise ^. L.promiseState of
                             L.Unforced -> True
                             _forced    -> False


-- constant.
--
-- The value of a function of no parameters, named as a value. One that closes over its
-- module's root frame alone, as every definition does, is evaluated the first time it is named in
-- an evaluation, and that value is every later use's: nothing it can see varies, so it
-- cannot differ. A fn of no parameters written inside a function closes over a frame that
-- each call of that function binds afresh, so it is evaluated each time it is named.
constant :: Env -> SourcePos -> L.FunctionId -> Maybe (SourcePos, R.Type) -> [S.Expr] -> EvalM L.Expr
constant closureEnv pos fnId resultType bodyExprs =
    case closureEnv ^. L.parentEnv of
        Just _  -> call
        Nothing -> gets (Map.lookup fnId . (^. L.constants)) >>= \case
                       Just value -> pure value
                       Nothing    -> do
                                     value <- call
                                     modify (L.constants %~ Map.insert fnId value)
                                     pure value
    where
    call = evalFunction evalExpr closureEnv pos pos fnId [] resultType bodyExprs []


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
--   * Type Assertion Redirection: Hands asserted nodes to 'checkRefinement', which checks the value
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
        S.Literal _ S.EmptyArr  -> pure $ Primitive I.EmptyArr
        S.Literal _ S.EmptyRec  -> pure $ Primitive I.EmptyRec

        -- A refinement on its own, with nothing to check, is the subject-less form of
        -- (∈ string), which only a case branch head fills in
        S.Asserted pos _ (S.Hole _ "%subject")
            -> lexicalError "A refinement on its own has nothing to check: it is asserted of a value, <x ⊢ string>, or asked, <x ∈ string>."
               & withBlurb "In a case branch head, (∈ string) asks it of the case's target: (case x ((∈ string) \"text\") (otherwise \"other\"))."
               & OuroError pos
               & EvalError
               & pure

        S.Asserted typePos ty payload -> assertType Assertion typePos ty payload

        -- A signature's requirement, on an argument or a result, checked in its role
        S.Required typePos ty role payload -> assertType role typePos ty payload
        -- A name of a function of no parameters, written as a value, is its result: Ouro has no
        -- effects, so the function cannot be told from its value. It is called as (name) is,
        -- so arity, signatures, blame and the recursion check apply, and a definition's value is kept
        -- (see constant). At the head of a call a name is the function itself (see applyFunction)
        S.Symbol pos name          -> lookupVar evalExpr pos name env >>= \case
                                          FunctionClosure closureEnv _ fnId [] resultType bodyExprs
                                              -> constant closureEnv pos fnId resultType bodyExprs
                                          value
                                              -> pure value

        -- Holes left by 'Canon.placeholders': outside a call's arguments and case patterns
        S.Hole   pos hole          -> pure (EvalError (strayHole hole pos))

        -- A special form's name that a binding around the form has is the binding's: as in
        -- Scheme, a binding shadows a special form, so the form is an ordinary call of it
        S.Form pos items
            | shadowsForm (`boundIn` env) items
            -> applyFunction pos items

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

        -- (get-or record default key ...): the value at the path, or the default when a record
        -- on the way has no such key or an Array no such index. The record comes first, as in
        -- every path form, and the default before the path, whose end it would otherwise blur.
        -- The default is evaluated only when it is used.
        S.Form _ (S.Symbol _ "get-or" : rootTarget : defaultExpr : pathExpressions@(_ : _))
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
            -> lexicalError "get-or is written (get-or record default key ...): a Record, the value to use when a key is missing, then the path of keys."
               & withBlurb "Perhaps write (get-or book () ntp46), which is the Record's ntp46, or () when it has none."
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

        -- (array a b): an Array of its values, each evaluated
        S.Form _ (S.Symbol _ "array" : items)            -> Array <$> mapM evalExpr items

        -- (make record item ...) and (make array element ...): a record or an array of what
        -- follows, whatever its shape, as a literal of that kind is built. So a record of inlays
        -- alone, which its shape reads as an array, can be written (gap 14). The kind is a name
        -- in a type position, never evaluated.
        S.Form pos (S.Symbol _ "make" : S.Symbol _ kind : items)
            | kind == "record" -> compileRecord (\currentEnv e -> local (const currentEnv) (evalExpr e)) env items
            | kind == "array"  -> compileArray  (\currentEnv e -> local (const currentEnv) (evalExpr e)) env pos items
        S.Form pos (S.Symbol _ "make" : _)
            -> lexicalError "make makes a record or an array: (make record :k v), (make array x y)."
               & withBlurb "Its first operand is the kind, written by its name; (make array) is [] and (make record) is {}."
               & OuroError pos
               & EvalError
               & pure

        -- (map array f): calls f on each element. The Array comes first, as every form takes
        -- what it works on, so '->' threads through it. Each call is an ordinary call, written
        -- in a frame that binds the function and the element to names that cannot be
        -- written in Ouro, so arity, signatures and the recursion check apply as usual.
        S.Form pos [S.Symbol _ "map", arrExpr, fnExpr]
            -> do
               arrVal <- evalExpr arrExpr
               fnVal  <- evalExpr fnExpr
               case (arrVal, fnVal) of
                   (EvalError err, _) -> pure (EvalError err)
                   (_, EvalError err) -> pure (EvalError err)

                   (_, f)
                       | not (callable f)
                       -> typeMismatch "a function to map with" (humanReadableType f)
                          & withBlurb "map calls a function on each element of an Array: (map array f), e.g. (map (1 2 3) (+ ? 1))."
                          & OuroError (S.exprPos fnExpr)
                          & EvalError
                          & pure

                   (arr, f)
                       | Just elems <- L.asArray arr
                       -> Array <$> mapM (callOnElement pos fnExpr arrExpr f) elems

                   (arr, _)
                       -> typeMismatch "an Array to map over" (humanReadableType arr)
                          & withBlurb "map calls a function on each element of an Array: (map (1 2 3) f)."
                          & OuroError (S.exprPos arrExpr)
                          & EvalError
                          & pure

        S.Form pos (S.Symbol _ "map" : _)
            -> lexicalError "map is written (map array f): the Array, then a function to call on each element."
               & withBlurb "Perhaps write (map (1 2 3) (+ ? 1)), or (map xs f) with a function f."
               & OuroError pos
               & EvalError
               & pure

        -- (exists array f), (forall array f), (omits array f), (filter array f): f is called
        -- on each element, as map calls it, and must answer with a Boolean. The Array is the
        -- domain, written first as in ∀x ∈ xs. f(x). exists, forall and omits stop at the
        -- element that decides the answer, as or and and stop at an operand; omits is exists
        -- the other way round.
        S.Form pos [S.Symbol _ form, arrExpr, fnExpr]
            | form `elem` ["exists", "forall", "omits", "filter"]
            -> do
               arrVal <- evalExpr arrExpr
               fnVal  <- evalExpr fnExpr
               case (arrVal, fnVal) of
                   (EvalError err, _) -> pure (EvalError err)
                   (_, EvalError err) -> pure (EvalError err)

                   (_, f)
                       | not (callable f)
                       -> typeMismatch ("a function for " <> form <> " to call") (humanReadableType f)
                          & withBlurb (predicateUsage form)
                          & OuroError (S.exprPos fnExpr)
                          & EvalError
                          & pure

                   (arr, f)
                       | Just elems <- L.asArray arr
                       -> let answer element = predicateVerdict form (S.exprPos arrExpr)
                                               <$> callOnElement pos fnExpr arrExpr f element
                          in case form of
                                 "exists" -> decideOn True  answer elems
                                 "forall" -> decideOn False answer elems
                                 "omits"  -> negateAnswer <$> decideOn True answer elems
                                 _filter  -> keepWhere answer elems

                   (arr, _)
                       -> typeMismatch ("an Array for " <> form <> " to go through") (humanReadableType arr)
                          & withBlurb (predicateUsage form)
                          & OuroError (S.exprPos arrExpr)
                          & EvalError
                          & pure

        S.Form pos (S.Symbol _ form : _)
            | form `elem` ["exists", "forall", "omits", "filter"]
            -> lexicalError (form <> " is written (" <> form <> " array f): the Array, then a function that answers true or false for each element.")
               & withBlurb (predicateUsage form)
               & OuroError pos
               & EvalError
               & pure

        -- (all array) and (any array), ⋀ and ⋁: whether every element of an Array of Booleans
        -- is true, or one is. An array written in place is a chain of cases by now (see
        -- 'connectives'), so what reaches here is an Array given another way, evaluated
        -- whole; it is read in order, and stops at the element that decides the answer.
        S.Form _ [S.Symbol _ form, arrExpr]
            | form `elem` ["all", "any"]
            -> evalExpr arrExpr >>= \case
                   err@(EvalError _) -> pure err
                   arr
                       | Just elems <- L.asArray arr
                       -> let answer = \case
                                  Primitive (I.Boolean b) -> pure (Right b)
                                  other                   -> refinementMismatch "boolean" (refinementOf other)
                                                             & withBlurb (form <> " reads an Array of Booleans: each element must be true or false.")
                                                             & OuroError (S.exprPos arrExpr)
                                                             & EvalError
                                                             & Left
                                                             & pure
                          in decideOn (form == "any") answer elems
                   other
                       -> typeMismatch ("an Array of Booleans for " <> form <> " to read") (humanReadableType other)
                          & withBlurb (connectiveUsage form)
                          & OuroError (S.exprPos arrExpr)
                          & EvalError
                          & pure

        S.Form pos (S.Symbol _ form : _)
            | form `elem` ["all", "any"]
            -> lexicalError (form <> " is written (" <> form <> " array): one Array of Booleans.")
               & withBlurb (connectiveUsage form)
               & OuroError pos
               & EvalError
               & pure

        -- (and a b) and (or a b) take two operands, as every built-in takes a fixed number; a
        -- written one is a chain of cases by now (see 'connectives'), so what reaches here has
        -- another number of them. Many conditions are an Array: (all (a b c)).
        S.Form pos (S.Symbol _ form : operands)
            | form `elem` ["and", "or"]
            -> lexicalError (form <> " takes two operands, (" <> form <> " a b), and was given " <> T.pack (show (length operands)) <> ".")
               & withBlurb (connectiveUsage (case form of { "and" -> "all"; _or -> "any" }))
               & OuroError pos
               & EvalError
               & pure

        -- (fold array init f): f called on the running value and each element in turn, from
        -- init, a left fold: (fold (1 2 3) 0 f) is (f (f (f 0 1) 2) 3), and (fold () init f)
        -- is init. The Array comes first, as every form takes what it works on, then the data,
        -- then the function. Each call is an ordinary call, as map makes one, so arity,
        -- signatures and the recursion check apply. An error stops the fold, as its value.
        S.Form pos [S.Symbol _ "fold", arrExpr, initExpr, fnExpr]
            -> do
               arrVal  <- evalExpr arrExpr
               initVal <- evalExpr initExpr
               fnVal   <- evalExpr fnExpr
               case (arrVal, initVal, fnVal) of
                   (EvalError err, _, _) -> pure (EvalError err)
                   (_, EvalError err, _) -> pure (EvalError err)
                   (_, _, EvalError err) -> pure (EvalError err)

                   (_, _, f)
                       | not (callable f)
                       -> typeMismatch "a function to fold with" (humanReadableType f)
                          & withBlurb foldUsage
                          & OuroError (S.exprPos fnExpr)
                          & EvalError
                          & pure

                   (arr, start, f)
                       | Just elems <- L.asArray arr
                       -> let step running = \case
                                  []             -> pure running
                                  element : rest -> callOnPair pos fnExpr initExpr arrExpr f running element >>= \case
                                                        err@(EvalError _) -> pure err
                                                        next              -> step next rest
                          in step start elems

                   (arr, _, _)
                       -> typeMismatch "an Array to fold" (humanReadableType arr)
                          & withBlurb foldUsage
                          & OuroError (S.exprPos arrExpr)
                          & EvalError
                          & pure

        S.Form pos (S.Symbol _ "fold" : _)
            -> lexicalError "fold is written (fold array init f): the Array, the value to start from, then a function of the running value and an element."
               & withBlurb foldUsage
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

        -- '->', 'with' and 'adjoin' are rewritten before evaluation, so what reaches here has no value
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

        S.Form pos [S.Symbol _ "adjoin"]
            -> lexicalError "adjoin is written (adjoin value :key value ...): a value, then what to add to it."
               & withBlurb "Each value is added to its key's values unless it is already there: (adjoin (base) :classified_as (painting))."
               & OuroError pos
               & EvalError
               & pure

        -- (let ($name expr ...) expr): private bindings, in any order, around one result
        S.Form pos (S.Symbol _ (S.formName -> "let") : rest)
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

        -- (when cond) is the head of a case branch; anywhere else it is misplaced
        S.Form pos (S.Symbol _ "when" : _)
            -> lexicalError "when is written as the head of a case branch, with one condition: ((when cond) result)."
               & withBlurb "Perhaps write (case x ((when (has x key)) \"yes\") (otherwise \"no\"))."
               & OuroError pos
               & EvalError
               & pure

        -- <x ∈ p>: whether x is in the refinement p, as a Boolean, the value first, as
        -- <x ⊢ p> states it. Only the refinement's check failing is false: an
        -- error in x, or a refinement that is not one, stays an error. The refinement, written
        -- last, has nothing after it, so it is read as a refinement on its own.
        S.Form _ [S.Symbol _ (S.asks -> True), subject, S.Asserted typePos ty (S.Hole _ "%subject")]
            | not (isSubjectHole subject)
            -> evalExpr subject >>= \case
                   err@(EvalError _) -> pure err
                   value             -> checkRefinement evaluator Asked ty typePos typePos value >>= \case
                                            EvalError (OuroError _ ctx) | isOutside ctx            -> pure (Primitive (I.Boolean False))
                                            err@(EvalError _)                                    -> pure err
                                            _hasIt                                               -> pure (Primitive (I.Boolean True))

        -- An assert or in that 'Canon.Typing' did not read: its refinement is not a name, or it
        -- has the wrong number of operands
        S.Form pos (S.Symbol _ "assert" : _)
            -> lexicalError "⊢ is written (⊢ value refinement), or <value ⊢ refinement>, the refinement by its name."
               & withBlurb "Perhaps write (⊢ x string), or <r ⊢ titled> for a refinement of the module."
               & OuroError pos
               & EvalError
               & pure

        S.Form pos (S.Symbol _ "in" : _)
            -> lexicalError "∈ is written (∈ value refinement), or <value ∈ refinement>, the refinement by its name."
               & withBlurb "Perhaps write (∈ x string), or ((∈ titled) result) as the head of a case branch."
               & OuroError pos
               & EvalError
               & pure

        S.Form pos (S.Symbol _ (S.formName -> "satisfies") : _)
            -> lexicalError "satisfies is now ∈: (∈ value refinement), <x ∈ string>, or a case head, ((∈ refinement) result)."
               & withBlurb "Perhaps write <x ∈ string>, or (case x ((∈ string) \"text\") (otherwise \"other\"))."
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
        -- A return type is read as an assertion on the parameter list: (fn ⊢ number (n) expr).
        S.Form pos [S.Symbol _ (S.formName -> "fn"), S.Form _ params, body]
            | Just typed <- traverse param (zip [1 ..] params)
            -> pure (either EvalError (const (FunctionClosure env M.Procedure ("fn", pos) typed Nothing [body])) (checkParams pos typed))

        S.Form pos [S.Symbol _ (S.formName -> "fn"), S.Asserted tPos ty (S.Form _ params), body]
            | Just typed <- traverse param (zip [1 ..] params)
            -> pure (either EvalError (const (FunctionClosure env M.Procedure ("fn", pos) typed (Just (tPos, ty)) [body])) (checkParams pos typed))

        S.Form pos (S.Symbol _ (S.formName -> "fn") : _)
            -> lexicalError "A fn is written (fn (args) expr): argument names or patterns, each with an optional type, then exactly one result expression."
               & withBlurb "A signature's types are refinements: (fn ⊢ number (<n ⊢ number>) (* n 2))."
               & OuroError pos
               & EvalError
               & pure

        -- An attribute pair, ':key value', read by the parser as (%attr "key" value): its key
        -- is always written, never computed
        S.Form _ [S.Symbol _ "%attr", S.Literal _ (S.Str name), aExpr]
            -> Attr name <$> evalExpr aExpr



        S.Form _ (S.Symbol pos (S.formName -> "case") : target : patterns)
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

        -- (insert coll i x) and (replace-at coll i x): the collection first, as get takes it
        S.Form _ [S.Symbol _ opName, structExpr, posExpr, payloadExpr]
            | opName `elem` ["insert", "replace-at"]
            -> do
               structVal  <- evalExpr structExpr
               posVal     <- evalExpr posExpr
               payloadVal <- evalExpr payloadExpr

               case (posVal, payloadVal, structVal) of
                   -- Error Propagation, in the order the arguments are written
                   (_, _, EvalError e) -> pure $ EvalError e
                   (EvalError e, _, _) -> pure $ EvalError e
                   (_, EvalError e, _) -> pure $ EvalError e

                   -- An index is a whole number, as in a path: a fractional one is never floored
                   (Primitive (I.Number i), _, _)
                        | i /= fromIntegral (floor i :: Int)
                       -> targetMismatch "a whole number to index an Array with" (I.renderNumber i)
                          & withBlurb ("An index counts the elements: (" <> opName <> " (a b c) 1 x) puts x at the second place.")
                          & OuroError (S.exprPos posExpr)
                          & EvalError
                          & pure

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


-- assertType.
--
-- Checks a value against a type, as a signature or an assertion such as (⊢ x string) names
-- it, written at the position given, in the role it is required in: an argument, a result or
-- an assertion. The value passes on as itself, never converted.
assertType :: Role -> SourcePos -> R.Type -> S.Expr -> EvalM L.Expr
assertType role typePos ty payload = do
    evaluatedVal <- evalExpr payload
    case evaluatedVal of
        err@(EvalError _) -> pure err
        _notError         -> checkRefinement evaluator role ty typePos (S.exprPos payload) evaluatedVal


-- What the checker is given of the evaluator (Refinement.Check's Evaluator): a defined
-- refinement's domain, the type it asks, if that is all its body does, and running it
evaluator :: Evaluator
evaluator = Evaluator
    { domainOf    = refinementDomain
    , askedTypeOf = refinementAskedType
    , runDefined  = runRefinement
    }


-- The type a refinement defined in Ouro asks of its subject, when its body is that question and
-- nothing else, <x ∈ T> of its first parameter, as Canon leaves it, where T is written, and the
-- refinement's doc; Nothing for any other body, or a name that names no refinement
refinementAskedType :: Text -> EvalM (Maybe (SourcePos, R.Type, Maybe Text))
refinementAskedType name = do
    env <- ask
    case findFunction name env of
        Just (_, M.SomeFunction (M.Function M.RefinementKind _ _ doc (subject : _) _
                    (S.Form _ [S.Form _ [S.Symbol _ (S.asks -> True), S.Symbol _ asked, S.Asserted typePos ty (S.Hole _ "%subject")]])))
            | asked == S.paramName subject
            -> pure (Just (typePos, ty, doc))

        _otherwise
            -> pure Nothing


-- The type a refinement defined in Ouro declares its subject with, its first parameter's, and
-- where it is written; Nothing for a subject declared without one, or a name that names no
-- refinement, which running it reports
refinementDomain :: Text -> EvalM (Maybe (SourcePos, R.Type))
refinementDomain name = do
    env <- ask
    case findFunction name env of
        Just (_, M.SomeFunction (M.Function M.RefinementKind _ _ _ (subject : _) _ _)) -> pure (S.paramType subject)
        _otherwise                                                                   -> pure Nothing


-- Runs a refinement defined in Ouro, titled, on a value, for the checker: its answer, true or
-- false, with its doc. It is found by the frames around the assertion, as a function is, but
-- never among their bindings, since nothing bound by a let or a parameter is a refinement. An
-- error inside the refinement is given back as the error. It is called where its assertion is
-- written, as the analysis found the call, so a refinement asserting itself on a part of its
-- value is accepted. The analysis reports a name that names no refinement, so the errors here
-- are not reached from a compile.
runRefinement :: SourcePos -> Text -> L.Expr -> EvalM (Either L.Expr (Bool, Maybe Text))
runRefinement typePos name v = do
    env <- ask
    case findFunction name env of
        Just (fnEnv, M.SomeFunction (M.Function M.RefinementKind fnPos _ doc params result (S.Form _ bodyExprs)))
            -> do
               -- The refinement and the value are bound under names that cannot be
               -- written in Ouro, and the refinement is called as any function is, so
               -- arity, its signature and the recursion check apply
               frame  <- allocValues (\names -> env
                                              & L.localScope       .~ names
                                              & L.functionRegistry .~ Map.empty
                                              & L.parentEnv        .~ Just env)
                                     typePos
                                     [("%refinement", FunctionClosure fnEnv M.Refinement (name, fnPos) params result bodyExprs), ("%refinement-value", v)]
               answer <- local (const frame) $
                             applyFunction typePos [S.Symbol typePos "%refinement", S.Symbol typePos "%refinement-value"]
               pure $ case answer of
                   Primitive (I.Boolean held) -> Right (held, doc)
                   -- Its body is checked as a boolean, so anything else is an error
                   other                      -> Left other

        Just (_, M.SomeFunction (M.Function M.ProcedureKind _ _ _ _ _ _))
            -> notARefinement name
               & OuroError typePos
               & EvalError
               & Left
               & pure

        Just _
            -> astCorruption name "Corrupted function registry entry."
               & OuroError typePos
               & EvalError
               & Left
               & pure

        Nothing
            -> unknownRefinement name []
               & OuroError typePos
               & EvalError
               & Left
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
               -- A name at the head is the function itself, never a constant's value, so
               -- (c) calls a function of no parameters rather than its result
               resolvedOp   <- case operatorExpr of
                                   S.Symbol namePos name -> ask >>= lookupVar evalExpr namePos name
                                   _notAName             -> evalExpr operatorExpr

               case resolvedOp of
                   FunctionClosure closureEnv _ name params resultType bodyExprs
                       -> evalFunction evalExpr closureEnv pos (S.exprPos operatorExpr) name params resultType bodyExprs argumentExprs

                   PrimitiveOp nativeFunc
                       -> do
                          evaledArgs <- mapM evalExpr argumentExprs
                          env        <- ask
                          pure $ runReader (nativeFunc pos evaledArgs) env

                   err@(EvalError _) -> pure err
                   otherVal          -> let what = case operatorExpr of
                                                       S.Symbol _ name -> "'" <> name <> "'"
                                                       _notAName       -> "This expression"
                                            -- A binding named after a special form shadows it, so the call is of the binding
                                            shadowedForm = case operatorExpr of
                                                S.Symbol _ name
                                                    | name `elem` specialForms
                                                    -> "\n\n'" <> name <> "' is also a special form, which a binding of the same name shadows, as in Scheme, "
                                                    <> "so (" <> name <> " ...) here calls the binding. Perhaps rename the binding to use the form."
                                                _notAForm
                                                    -> ""
                                        in typeMismatch "a function to call" (humanReadableType otherVal)
                                           & withBlurb ( what <> " is " <> humanReadableType otherVal <> ", not a function, so it cannot be called."
                                                      <> "\n\nFunctions and values share one namespace: a value with the same name as a function shadows it."
                                                      <> shadowedForm
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
                               other                       -> refinementMismatch "boolean" (refinementOf other)
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


-- The function a name defines, unqualified, and the frame it is defined in: found in the
-- frames around the evaluation, as lookupFunction finds one, but never among their
-- bindings
findFunction :: Text -> Env -> Maybe (Env, M.SomeFunction)
findFunction name env =
    case Map.lookup name (env ^. L.functionRegistry) of
        Just fn -> Just (env, fn)
        Nothing -> env ^. L.parentEnv >>= findFunction name


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

    S.Asserted holePos _ (S.Symbol _ k)
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
                      <> "to walk into the value's records. You provided a colon-prefixed attribute identifier.\n\n"
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
-- Only a Record's attribute pairs can be reached by key, and an Array's elements by index.
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
                               & withBlurb ("The Record has no attribute '" <> key <> "'.")
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
elementAt i xs = V.fromList xs V.!? (case i < 0 of True -> length xs + i; False -> i)


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
    other                   -> refinementMismatch "boolean" (refinementOf other)
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
    <> form <> " array f), e.g. (" <> form <> " (1 2 3) (> ? 1))."

-- all and any, the n-ary and and or, over one Array of Booleans
connectiveUsage :: Text -> Text
connectiveUsage form =
    "(" <> form <> " (a b c)) is true when " <> (case form of { "all" -> "every condition is"; _any -> "one condition is" })
    <> "; written in place, it stops at the one that decides, as and and or do."

foldUsage :: Text
foldUsage = "fold calls a function on the running value and each element in turn: (fold (1 2 3) 0 +) is 6."


-- One call of fold's function, on the running value and an element, made as callOnElement
-- makes map's: in a frame that binds them to names that cannot be written in Ouro
callOnPair :: SourcePos -> S.Expr -> S.Expr -> S.Expr -> L.Expr -> L.Expr -> L.Expr -> EvalM L.Expr
callOnPair pos fnExpr initExpr arrExpr f running element = do
    env   <- ask
    frame <- allocValues (\names -> env
                                  & L.localScope       .~ names
                                  & L.functionRegistry .~ Map.empty
                                  & L.parentEnv        .~ Just env)
                         pos
                         [("%fold-fn", f), ("%fold-running", running), ("%fold-element", element)]
    local (const frame) $
        applyFunction pos [ S.Symbol (S.exprPos fnExpr) "%fold-fn"
                          , S.Symbol (S.exprPos initExpr) "%fold-running"
                          , S.Symbol (S.exprPos arrExpr) "%fold-element" ]


-- A ':key value' attribute pair, as a value, belongs in a record, not an array
notAField :: L.Expr -> Bool
notAField = \case
    Attr _ _     -> False
    _notAField   -> True
