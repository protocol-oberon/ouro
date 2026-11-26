{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Data.Ouro.Lisp.Eval.Structural where

import           Control.Monad.Reader        (ask, local)
import           Data.Functor                ((<&>))
import           Data.Function               ((&))
import qualified Data.Map                    as Map
import           Data.Maybe                  (listToMaybe)
import           Data.Ouro.Error.Diagnostics (incorrectArity, lexicalError,
                                              typeMismatch, typeMismatchBlurb,
                                              withBlurb)
import           Data.Ouro.Error.Types       (OuroError (..))
import qualified Data.Ouro.Internal.Expr     as I
import qualified Data.Ouro.Internal.Kinds    as JLD
import           Data.Ouro.Lisp.Analysis.Forms (BlockTarget (..), FrameEntry (..),
                                              bindableAttr, collectBindings,
                                              determineBlockTarget, shadowsForm)
import           Data.Ouro.Lisp.Eval.Pattern (matchPattern)
import           Data.Ouro.Lisp.Eval.Schema  (contextFromValue)
import           Data.Ouro.Lisp.Eval.Scope   (allocFrame, allocValues, boundIn, force)
import           Data.Ouro.Lisp.Eval.Types   (Binding (..), Env (..), EvalM, Expr (..),
                                              humanReadableType)
import qualified Data.Ouro.Lisp.Eval.Types   as L
import           Data.Ouro.Lisp.Function     (callee, functionBody,
                                              isFunctionKeyword,
                                              misplacedBinding, removedAttrForm,
                                              removedContextForm,
                                              removedLocalDefn, requiredArgument,
                                              requiredResult)
import qualified Data.Ouro.Lisp.Surface      as S
import qualified Data.Ouro.Refinement.Type   as R
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import           Lens.Micro.Platform         ((.~), (^.))
import           Text.Megaparsec             (SourcePos)


-- compileRecord.
--
-- Compiles a physical Lisp block into a self-contained, knot-tied lexical environment frame.
-- Manages local symbol mapping, dynamic property lazy evaluation, and JSON-LD schema context routing.
--
-- This worker serves as the critical transition boundary between surface form expressions and structural,
-- type-safe graph layers. Rather than applying standard top-down sequential evaluation, it operates in three distinct,
-- highly deliberate stages to enforce declarative order-independence within the local block:
--
--   1. Sweeping & Binding: It passes over the attribute pairs using 'collectBindings' to harvest their
--      un-evaluated expressions.
--   2. Environment Isolation: Every attribute pair becomes a promise in a fresh lexical 'Env' frame, whose parent
--      is the ambient env. The promises close over the frame itself, so attribute pairs can reference their
--      siblings in any order.
--   3. Semantic Extraction & Context Lowering: It evaluates the properties into a core GADT structural list.
compileRecord
    :: (Env -> S.Expr -> EvalM L.Expr)
    -> Env
    -> [S.Expr]
    -> EvalM L.Expr
compileRecord evaluator env items =
    case collectBindings items of
        -- Dynamic environment allocation failures represent a catastrophic scope break
        Left err
            -> pure (EvalError err)

        Right entries
            -> do
               -- Every binding becomes a promise in a new frame, whose parent is the ambient env
               (frame, addrs) <- allocFrame (\names -> env
                                                       & L.localScope       .~ names
                                                       & L.functionRegistry .~ Map.empty
                                                       & L.parentEnv        .~ Just env)
                                            entries
               -- Each output attribute pair is evaluated through its own promise, found by its position
               let attrAddrs = Map.fromList [ (entryPos entry, addr) | (entry, addr) <- zip entries addrs, entryIsAttr entry ]
               emitProps evaluator frame attrAddrs items


-- Iterates through a stream of tokens to filter and evaluate physical properties into a L.Expr superset tree.
-- Iterates through a stream of tokens to filter and evaluate physical properties into a L.Expr superset tree.
emitProps
    :: (Env -> S.Expr -> EvalM L.Expr)
    -> Env
    -> Map.Map SourcePos L.Addr     -- The promise of each output attribute pair, by the position of its ':key'
    -> [S.Expr]
    -> EvalM L.Expr
emitProps evaluator env attrAddrs expressions = go I.EmptyMeta expressions
    where
    -- The keys written in this record. A key can be written once: a later attribute pair with the
    -- same key is an error, and so is a second context. Inlays are layers: a later inlay
    -- overrides an earlier one, and written attribute pairs override both.
    writtenAttrs  = [ (key, attrPos) | Just (key, attrPos, _) <- map bindableAttr expressions ]
    writtenKeys   = Set.fromList (map fst writtenAttrs)

    writtenAgain :: Map.Map SourcePos OuroError
    writtenAgain  = Map.fromList
        [ (attrPos, writtenTwice key attrPos)
        | (i, (key, attrPos)) <- zip [0 :: Int ..] writtenAttrs
        , key `elem` map fst (take i writtenAttrs)
        ]

    laterContexts = drop 1 [ pos | S.Form _ (S.Symbol pos "%context" : _) <- expressions ]

    -- An error in the record's context is kept under its key
    contextError err = (<&> \case
                            Record finalMeta nextPairs -> Record finalMeta (("@context", EvalError err) : nextPairs)
                            otherVal                   -> otherVal)

    go :: I.Expr 'JLD.Meta -> [S.Expr] -> EvalM L.Expr
    go metaAcc = \case
                  [] -> pure (Record metaAcc [])

                  -- Case A: The record's context, ':context value'. Its value is evaluated like any
                  -- other, then read as a JSON-LD context and kept as the record's metadata.
                  S.Form _ [S.Symbol pos "%context", _] : remaining
                      | pos `elem` laterContexts
                      -> contextError (writtenTwice "context" pos) (go metaAcc remaining)

                  S.Form _ [S.Symbol _ "%context", contextExpr] : remaining
                      -> do
                         value <- evaluator env contextExpr
                         case contextFromValue (S.exprPos contextExpr) value of
                             Left  err    -> contextError err (go metaAcc remaining)
                             Right schema -> go (I.Context schema) remaining

                  -- The old (context (...)) form, replaced by ':context value'
                  S.Form _ (S.Symbol pos "context" : _) : remaining
                      -> contextError (removedContextForm pos) (go metaAcc remaining)

                  -- The removed (attr key value) form, replaced by ':key value'
                  S.Form _ (S.Symbol pos "attr" : _) : rest
                      -> go metaAcc rest <&> \case
                             Record m p -> Record m (("*err*", EvalError (removedAttrForm pos)) : p)
                             otherVal   -> otherVal

                  -- Case B: Inlay evaluated record pairs directly into the current record scope, unless a
                  -- binding of the name shadows the form, as in Scheme
                  S.Form _ inlayForm@[S.Symbol pos (S.formName -> "inlay"), iExpr] : rest
                      | not (shadowsForm (`boundIn` env) inlayForm)
                      -> let blockTarget = case iExpr of
                                               S.Form _ inner -> determineBlockTarget inner
                                               _              -> TargetFunctionApp -- Treat symbols/primitives as dynamic
                         in case blockTarget of
                                TargetList -> let err = typeMismatch
                                                            "a valid Record (or Function resolving to a Record) to inlay"
                                                            "a List/Array block target"
                                                        & OuroError pos
                                              in go metaAcc rest <&> \case
                                                     Record finalMeta nextPairs -> Record finalMeta (("*err*", EvalError err) : nextPairs)
                                                     otherVal                   -> otherVal

                                -- Catch both TargetRecord AND TargetFunctionApp
                                _ -> go metaAcc rest >>= \case
                                        Record finalMeta nextPairs
                                            -> evaluator env iExpr <&> \case
                                                    -- 1. If evaluation fails, embed the error so the tree retains it
                                                    EvalError err -> Record finalMeta (("*err*", EvalError err) : nextPairs)

                                                    -- 2. The successful path: Merge the evaluated pairs (p) into the current scope.
                                                    --    A key written in this record, or supplied by a later inlay
                                                    --    (already in nextPairs), overrides this inlay's attribute pair.
                                                    --    The inlay's context is layered under the record's in the same
                                                    --    order: finalMeta already holds the later inlays' contexts and
                                                    --    the written one, which are applied after it.
                                                    Record m p    -> let laterKeys = Set.fromList [ k | (k, _) <- nextPairs, k `notElem` ["*err*", "@context"] ]
                                                                         overridden k = Set.member k writtenKeys || Set.member k laterKeys
                                                                         spliced   = [ (k, v) | (k, v) <- p, not (overridden k) ]
                                                                     in Record (m <> finalMeta) (spliced <> nextPairs)

                                                    -- 3. If it evaluated to something other than a record, throw a type mismatch
                                                    otherVal      -> let err = typeMismatch
                                                                                   "a valid Record to inlay into the current Record scope"
                                                                                   (humanReadableType otherVal)
                                                                               & OuroError (S.exprPos iExpr)
                                                                     in Record finalMeta (("*err*", EvalError err) : nextPairs)

                                        otherVal -> pure otherVal

                  -- Case C: Extract valid body pairs using the new structured attribute form.
                  -- The value comes from the attribute pair's promise, which lookups of the same attribute share.
                  attrExpr : rest
                      | Just (key, attrPos, _) <- bindableAttr attrExpr
                      , Just err <- Map.lookup attrPos writtenAgain
                      -> go metaAcc rest <&> \case
                             Record finalMeta nextPairs -> Record finalMeta ((key, EvalError err) : nextPairs)
                             otherVal                   -> otherVal

                  attrExpr : rest
                      | Just (key, attrPos, valExpr) <- bindableAttr attrExpr
                      -> go metaAcc rest >>= \case
                             Record finalMeta nextPairs
                                 -> attrValue attrPos valExpr <&> \case
                                        -- 1. Catch error leaves completely independently
                                        EvalError err -> Record finalMeta ((key, EvalError err) : nextPairs)

                                        -- 2. Clean primitive values (frozen GADTs)
                                        Primitive prim -> Record finalMeta ((key, Primitive prim) : nextPairs)

                                        -- 3. Accept nested record configurations
                                        Record m p -> Record finalMeta ((key, Record m p) : nextPairs)

                                        -- 4. Accept array configurations
                                        Array elements -> Record finalMeta ((key, Array elements) : nextPairs)

                                        -- A value that is not data
                                        otherVal -> let err = typeMismatch
                                                                  "data"
                                                                  (humanReadableType otherVal)
                                                              & withBlurb (typeMismatchBlurb otherVal)
                                                              & OuroError (S.exprPos valExpr)
                                                    in Record finalMeta ((key, EvalError err) : nextPairs)

                             otherVal -> pure otherVal

                  -- Case D (Replaces Old B.25 and Old D): Safely drop malformed/loose attribute blocks
                  -- or attributes wrapping blocked structural layouts, and keep moving
                  S.Form _ (S.Symbol _ "%attr" : _) : rest -> go metaAcc rest

                  -- Case E: Catch dangling expressions instead of silently erasing them
                  expr : rest
                      -> let err = typeMismatch
                                       "an attribute pair, an inlay or a context"
                                       "a value without a key"
                                   & withBlurb ( "A form with an attribute pair is a record, so everything in it is an attribute pair, "
                                              <> "an inlay or a context, and this value has no key. Perhaps give it one, (:k value); "
                                              <> "or, if an array was meant, put the attribute pair in a record of its own, (1 (:a 2))." )
                                   & OuroError (S.exprPos expr)
                         in go metaAcc rest <&> \case
                                Record m p -> Record m (("*err*", EvalError err) : p)
                                otherVal   -> otherVal


    -- Forces the attribute pair's promise. Every bindable attribute pair is given one by compileRecord,
    -- so evaluating the expression directly is only a fallback.
    attrValue :: SourcePos -> S.Expr -> EvalM L.Expr
    attrValue attrPos valExpr =
        case Map.lookup attrPos attrAddrs of
            Just addr -> force inCurrentEnv attrPos addr
            Nothing   -> evaluator env valExpr

    -- 'force' switches to the promise's env; evaluate in whatever env is current
    inCurrentEnv :: S.Expr -> EvalM L.Expr
    inCurrentEnv expr' = ask >>= \current -> evaluator current expr'


-- A key can appear once in a record
writtenTwice :: Text -> SourcePos -> OuroError
writtenTwice key pos =
    lexicalError ("The key '" <> key <> "' is written twice in the same record.")
    & withBlurb "Each key can appear once in a record. Perhaps remove one of the attribute pairs, or rename it."
    & OuroError pos


-- 'insert' (or 'replace-at') would give a record the same key twice
alreadyAField :: Text -> SourcePos -> OuroError
alreadyAField key pos =
    lexicalError ("The record already has an attribute '" <> key <> "'.")
    & withBlurb "Each key can appear once in a record. Perhaps use replace-at to change the existing attribute pair."
    & OuroError pos


-- Compiles a collection of nested Lisp blocks into a uniform Array
compileArray
    :: (Env -> S.Expr -> EvalM L.Expr)
    -> Env
    -> SourcePos
    -> [S.Expr]
    -> EvalM L.Expr
compileArray evaluator env pos elements = Array <$> compileElements env elements

    where
    compileElements :: Env -> [S.Expr] -> EvalM [L.Expr]
    compileElements env' exprs =
        case exprs of
            [] -> pure []

            -- Case A: a local definition was replaced by a let-bound 'fn'
            S.Form dPos (S.Symbol _ keyword : _) : xs | isFunctionKeyword keyword
                -> (EvalError (removedLocalDefn dPos) :) <$> compileElements env' xs

            -- A private binding belongs in a let
            S.Form dPos [S.Symbol _ "%binder", S.Literal _ (S.Str name)] : xs
                -> (EvalError (misplacedBinding name dPos) :) <$> compileElements env' xs

            -- The old (context (...)) form, replaced by ':context value' in a record
            S.Form dPos (S.Symbol _ "context" : _) : xs
                -> (EvalError (removedContextForm dPos) :) <$> compileElements env' xs

            -- The removed (attr key value) form
            S.Form dPos (S.Symbol _ "attr" : _) : xs
                -> (EvalError (removedAttrForm dPos) :) <$> compileElements env' xs

            -- Case B.25 TRUE ERASURE: Skip ubound attrs from attr from
            S.Form _ [S.Symbol _ "%attr", _, _] : xs
                -> compileElements env' xs

            -- Case B.5: Inlay evaluated array elements directly into the current array scope, unless
            -- a binding of the name shadows the form, as in Scheme
            S.Form _ inlayForm@[S.Symbol sPos (S.formName -> "inlay"), iExpr] : xs
                | not (shadowsForm (`boundIn` env') inlayForm)
                -> let blockTarget = case iExpr of
                                         S.Form _ inner -> determineBlockTarget inner
                                         _              -> TargetFunctionApp -- Treat symbols/functions as dynamic
                   in case blockTarget of
                          TargetRecord -> let err = typeMismatch
                                                        "a valid Array (or Function resolving to an Array) to inlay"
                                                        "a Record block target"
                                                    & OuroError sPos
                                          in (EvalError err :) <$> compileElements env' xs

                          -- Catch both TargetList AND TargetFunctionApp (for functions/variables)
                          _ -> evaluator env' iExpr >>= \case
                                   -- 1. If evaluation fails, embed the error so the tree retains it
                                   EvalError err  -> (EvalError err :) <$> compileElements env' xs

                                   -- 2. The successful path: Flatten the evaluated elements into the stream
                                   Array elems -> (elems ++) <$> compileElements env' xs

                                   -- 3. If it evaluated to something other than an array, throw a type mismatch
                                   otherVal    -> let err = typeMismatch
                                                                "a valid Array to inlay into the current array scope"
                                                                (humanReadableType otherVal)
                                                            & OuroError (S.exprPos iExpr)
                                                  in (EvalError err :) <$> compileElements env' xs

            -- Case C: a form among the elements, a record or an array of its own
            ((S.Form _ items) : xs)
                -- 1. An array inside the array, compiled as one
                | TargetList <- determineBlockTarget items
                -> (:) <$> compileArray evaluator env' pos items <*> compileElements env' xs

                -- 2. Intercept Nested Records: compile using the record scope builder
                | TargetRecord <- determineBlockTarget items
                -> do
                   evaledItem <- compileRecord evaluator env' items
                   restL      <- compileElements env' xs
                   pure $ case evaledItem of
                          -- Retain independent error leaves found inside nested scopes safely
                          EvalError err -> EvalError err : restL

                          -- Seamlessly capture the open record superset node
                          Record    m p -> Record    m p : restL
                          Primitive p   -> Primitive p   : restL
                          otherVal      -> typeMismatch
                                               "data"
                                               (humanReadableType otherVal)
                                           & withBlurb (typeMismatchBlurb otherVal)
                                           & OuroError pos
                                           & (\e -> EvalError e : restL)

            -- Case D: Process flat scalar values or variables evaluated within the element stream
            (otherExpr : xs)
                -> do
                   evaledVal <- evaluator env' otherExpr
                   restL     <- compileElements env' xs
                   pure $ case evaledVal of
                        EvalError err  -> EvalError err  : restL
                        Primitive prim -> Primitive prim : restL

                        -- Accept nested layouts or expressions inside the stream
                        Record      m  p   -> Record      m  p   : restL
                        Array       ls     -> Array       ls     : restL

                        otherVal -> let err = typeMismatch
                                                    "data"
                                                    (humanReadableType otherVal)
                                                & withBlurb (typeMismatchBlurb otherVal)
                                                & OuroError (S.exprPos otherExpr)
                                    in EvalError err : restL


-- evalFunction
--
-- Calls a function. Each argument becomes a promise over the caller's environment,
-- so it means what it meant where it was written, and is evaluated at most once. A
-- typed parameter's argument is checked under its requirement, as that argument of the
-- function, when it is forced, and the result under the return type, as the function's
-- result. The body runs in the function's closure (the frame it was defined in) extended
-- with a frame for the parameters.
--
-- Recursion goes down into a value. The call stack comes from the caller and records each
-- function by identity, so nesting a call, like (f (f x)), is not recursion. A call of a
-- function already being called is let through only where the static analysis found it goes
-- down into a value, by where the function's name is written: its arguments are evaluated
-- first, and an error in one is the call's value, since an error is no part of a value. Each
-- argument is then a part, or no larger, as the analysis found, so the calls reach the
-- leaves. Anything else that calls a function already being called is an error.
evalFunction
    :: (S.Expr -> EvalM L.Expr)       -- Core evaluator function
    -> Env                            -- The function's closure: the frame it is defined in
    -> SourcePos                      -- Callsite position
    -> SourcePos                      -- Where the function's name, or what gives it, is written
    -> L.FunctionId                   -- The function's name and definition position
    -> [S.Param]                      -- The function's formal parameters (e.g., (name <year ⊢ number>))
    -> Maybe (SourcePos, R.Type) -- The requirement on its result, its return type
    -> [S.Expr]                       -- The un-evaluated body expressions of the function
    -> [S.Expr]                       -- The actual arguments passed at the call site
    -> EvalM L.Expr                   -- The returned evaluated expr
evalFunction evaluator closureEnv pos site fnId@(name, _) params resultType bodyExprs actualArgs = do
    callerEnv <- ask
    let parLen      = length params
        actLen      = length actualArgs
        callerStack = callerEnv ^. L.callStack
        again       = Set.member fnId callerStack

    case (parLen == actLen, again, Set.member site (callerEnv ^. L.acceptedCalls)) of
        (False, _, _)
            -> incorrectArity name parLen actLen
               & maybe id (\call -> withBlurb ("It is called " <> call <> ".")) (L.callTemplate name params)
               & OuroError pos
               & L.EvalError
               & pure

        (True, True, False)
            -> typeMismatch
                   "a terminating function call"
                   "a recursive function call"
               & withBlurb ( "The function '" <> name <> "' called itself through a value, such as a parameter or a function a call returned. "
                          <> "Recursion is checked to go down into a value only where a function calls itself by its name."
                           )
               & OuroError pos
               & EvalError
               & pure

        (True, _, _)
            -> do
               -- The arguments' promises close over the caller's environment. A typed
               -- parameter's argument is checked under its requirement, where the call is written.
               (_, argAddrs) <- allocFrame (const callerEnv)
                                    [ FrameEntry (S.paramName param) (S.exprPos arg) (requiredArgument (callee name) index param arg) False
                                    | (index, param, arg) <- zip3 [1 ..] params actualArgs ]

               -- A call of a function already being called evaluates its arguments first: an
               -- error in one ends the recursion, as its value
               stopped <- case again of
                              True  -> (\values -> listToMaybe [ err | err@(EvalError _) <- values ]) <$> traverse (force evaluator pos) argAddrs
                              False -> pure Nothing

               case stopped of
                   Just err -> pure err
                   Nothing  -> enter argAddrs

    where
    -- The body, in the closure with a frame of the parameters, this call on the stack
    enter argAddrs = do
               callerEnv <- ask
               let callerStack = callerEnv ^. L.callStack
                   functionEnv = closureEnv
                               & L.localScope       .~ Map.fromList (zip (map S.paramName params) (map Promised argAddrs))
                               & L.functionRegistry .~ Map.empty
                               & L.parentEnv        .~ Just closureEnv
                               & L.callStack        .~ Set.insert fnId callerStack

                   -- Evaluate an expression in an explicit environment
                   evaluator' currentEnv expr' = local (const currentEnv) (evaluator expr')

               -- The body is one expression: the function's result, under its return type
               case functionBody name pos bodyExprs of
                   Left  err    -> pure (EvalError err)
                   Right result -> destructure evaluator' closureEnv functionEnv pos name (zip params actualArgs) >>= \case
                                       Left  err     -> pure (EvalError err)
                                       Right bodyEnv -> evaluator' bodyEnv (requiredResult (callee name) resultType result)


-- destructure
--
-- Matches each pattern parameter's argument against its pattern when the function is
-- called, whether or not the body uses its holes, as a typed argument is checked. The
-- holes are bound, as already-evaluated values, in one frame around the body. A value
-- the pattern does not match is the caller's mistake, so it is reported at the argument.
--
-- The values written in a pattern are evaluated where the function is defined, so they
-- cannot see the function's parameters: a pattern describes its own argument, and a
-- check against another parameter is written in the body.
destructure
    :: (Env -> S.Expr -> EvalM L.Expr)  -- Evaluates an expression in an explicit environment
    -> Env                              -- The function's closure: the frame it is defined in
    -> Env                              -- The frame of the function's parameters
    -> SourcePos                        -- Callsite position
    -> Text                             -- The function's name
    -> [(S.Param, S.Expr)]              -- Each parameter with its argument
    -> EvalM (Either OuroError Env)
destructure evaluator' closureEnv functionEnv pos name = go []
    where
    -- The closure, with the call stack that includes this call, so the recursion check holds
    patternEnv = closureEnv & L.callStack .~ (functionEnv ^. L.callStack)

    go bound = \case
        []
            | null bound -> pure (Right functionEnv)
            | otherwise  -> Right <$> allocValues (\names -> functionEnv
                                                             & L.localScope       .~ names
                                                             & L.functionRegistry .~ Map.empty
                                                             & L.parentEnv        .~ Just functionEnv)
                                                  pos
                                                  bound

        (param, arg) : rest
            | Just pattern <- S.paramPattern param
            -> evaluator' functionEnv (S.Symbol (S.exprPos arg) (S.paramName param)) >>= \case
                   EvalError err -> pure (Left err)
                   value         -> matchPattern (evaluator' patternEnv) pattern value >>= \case
                                        Left  err             -> pure (Left err)
                                        Right (Just bindings) -> go (bound <> bindings) rest
                                        Right Nothing         -> typeMismatch "an argument with the shape of the parameter's pattern"
                                                                              (humanReadableType value)
                                                                 & withBlurb ( "The function '" <> name <> "' takes this argument apart with a pattern, "
                                                                            <> "as in (fn ((?a ?b)) ...), so the argument must have the pattern's shape."
                                                                             )
                                                                 & OuroError (S.exprPos arg)
                                                                 & Left
                                                                 & pure

            | otherwise
            -> go bound rest
