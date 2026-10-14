{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Data.Ouro.Lisp.Eval.Structural where

import           Control.Monad.Reader        (ask, local, runReader)
import           Data.Functor                ((<&>))
import           Data.Function               ((&))
import qualified Data.Map                    as Map
import           Data.Ouro.Error.Diagnostics (incorrectArity, internalValueLeak,
                                              lexicalError, typeMismatch,
                                              typeMismatchBlurb, withBlurb)
import           Data.Ouro.Error.Types       (OuroError (..))
import qualified Data.Ouro.Internal.Expr     as I
import qualified Data.Ouro.Internal.Kinds    as JLD
import           Data.Ouro.Lisp.Eval.Schema  (parseContextDirectives)
import           Data.Ouro.Lisp.Eval.Scope   (FrameEntry (..), allocFrame,
                                              bindableAttr, collectBindings,
                                              force)
import           Data.Ouro.Lisp.Eval.Types   (Binding (..), Env (..), EvalM, Expr (..),
                                              humanReadableType)
import qualified Data.Ouro.Lisp.Eval.Types   as L
import           Data.Ouro.Lisp.Function     (functionBody, isFunctionKeyword,
                                              misplacedBinding, removedDefine,
                                              removedLocalDefn)
import qualified Data.Ouro.Lisp.Surface      as S
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
--   1. Sweeping & Binding: It passes over the fields using 'collectBindings' to harvest their
--      un-evaluated expressions.
--   2. Environment Isolation: Every field becomes a promise in a fresh lexical 'Env' frame, whose parent
--      is the ambient env. The promises close over the frame itself, so fields can reference their
--      siblings in any order.
--   3. Semantic Extraction & Context Lowering: It evaluates the properties into a core GADT structural list.
compileRecord
    :: (Env -> S.Expr -> EvalM L.Expr)
    -> Env
    -> [S.Expr]
    -> EvalM L.Expr
compileRecord evaluator env fields =
    case collectBindings fields of
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
               -- Each output field is evaluated through its own promise, found by its position
               let fieldAddrs = Map.fromList [ (entryPos entry, addr) | (entry, addr) <- zip entries addrs, entryIsField entry ]
               emitProps evaluator frame fieldAddrs fields


-- Iterates through a stream of tokens to filter and evaluate physical properties into a L.Expr superset tree.
-- Iterates through a stream of tokens to filter and evaluate physical properties into a L.Expr superset tree.
emitProps
    :: (Env -> S.Expr -> EvalM L.Expr)
    -> Env
    -> Map.Map SourcePos L.Addr     -- The promise of each output field, by the position of its ':key'
    -> [S.Expr]
    -> EvalM L.Expr
emitProps evaluator env fieldAddrs expressions = go I.EmptyMeta expressions
    where
    -- The keys written in this record. A key can be written once: a later field with the
    -- same key is an error, and so is a second context. Inlays are layers: a later inlay
    -- overrides an earlier one, and written fields override both.
    writtenFields = [ (key, fieldPos) | Just (key, fieldPos, _) <- map bindableAttr expressions ]
    writtenKeys   = Set.fromList (map fst writtenFields)

    writtenAgain :: Map.Map SourcePos OuroError
    writtenAgain  = Map.fromList
        [ (fieldPos, writtenTwice key fieldPos)
        | (i, (key, fieldPos)) <- zip [0 :: Int ..] writtenFields
        , key `elem` map fst (take i writtenFields)
        ]

    laterContexts = drop 1 [ pos | S.Form _ (S.Symbol pos "context" : _) <- expressions ]

    go :: I.Expr 'JLD.Meta -> [S.Expr] -> EvalM L.Expr
    go metaAcc = \case
                  [] -> pure (Record metaAcc [])

                  -- Case A: Intercept ANY context form variant at the top-level and route to the schema engine
                  -- (Schema parsing only reads the environment, so it runs outside EvalM)
                  S.Form _ (S.Symbol pos "context" : _) : remaining
                      | pos `elem` laterContexts
                      -> go metaAcc remaining <&> \case
                             Record finalMeta nextPairs -> Record finalMeta (("@context", EvalError (writtenTwice "context" pos)) : nextPairs)
                             otherVal                   -> otherVal

                  S.Form _ (S.Symbol pos "context" : directives) : remaining
                      -> case runReader (parseContextDirectives directives) env of
                             EvalError err
                                 -> go metaAcc remaining <&> \case
                                        Record finalMeta nextPairs -> Record finalMeta (("@context", EvalError err) : nextPairs)
                                        otherVal                   -> otherVal

                             SchemaVal localSchema
                                 -> go (I.Context localSchema) remaining

                             otherVal
                                 -> let leakErr = internalValueLeak "emitProps context block resolution"
                                                  & withBlurb ("Expected a SchemaVal, but leaked: " <> humanReadableType otherVal)
                                                  & OuroError pos
                                    in go metaAcc remaining <&> \case
                                          Record finalMeta nextPairs -> Record finalMeta (("@context", EvalError leakErr) : nextPairs)
                                          ov                         -> ov

                  -- Case B: Inlay evaluated record pairs directly into the current record scope
                  S.Form _ [S.Symbol pos "inlay", iExpr] : rest
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
                                                    --    (already in nextPairs), overrides this inlay's field.
                                                    Record _ p    -> let laterKeys = Set.fromList [ k | (k, _) <- nextPairs, k `notElem` ["*err*", "@context"] ]
                                                                         overridden k = Set.member k writtenKeys || Set.member k laterKeys
                                                                         spliced   = [ (k, v) | (k, v) <- p, not (overridden k) ]
                                                                     in Record finalMeta (spliced <> nextPairs)

                                                    -- 3. If it evaluated to something other than a record, throw a type mismatch
                                                    otherVal      -> let err = typeMismatch
                                                                                   "a valid Record to inlay into the current Record scope"
                                                                                   (humanReadableType otherVal)
                                                                               & OuroError (S.exprPos iExpr)
                                                                     in Record finalMeta (("*err*", EvalError err) : nextPairs)

                                        otherVal -> pure otherVal

                  -- Case C: Extract valid body pairs using the new structured attribute form.
                  -- The value comes from the field's promise, which lookups of the same field share.
                  attrExpr : rest
                      | Just (key, fieldPos, _) <- bindableAttr attrExpr
                      , Just err <- Map.lookup fieldPos writtenAgain
                      -> go metaAcc rest <&> \case
                             Record finalMeta nextPairs -> Record finalMeta ((key, EvalError err) : nextPairs)
                             otherVal                   -> otherVal

                  attrExpr : rest
                      | Just (key, fieldPos, valExpr) <- bindableAttr attrExpr
                      -> go metaAcc rest >>= \case
                             Record finalMeta nextPairs
                                 -> fieldValue fieldPos valExpr <&> \case
                                        -- 1. Catch error leaves completely independently
                                        EvalError err -> Record finalMeta ((key, EvalError err) : nextPairs)

                                        -- 2. Clean primitive values (frozen GADTs)
                                        Primitive prim -> Record finalMeta ((key, Primitive prim) : nextPairs)

                                        -- 3. Accept nested object configurations
                                        Record m p -> Record finalMeta ((key, Record m p) : nextPairs)

                                        -- 4. Accept array configurations
                                        Array elements -> Record finalMeta ((key, Array elements) : nextPairs)

                                        -- 5. Pass-through for valid domain primitives (Durations, Schemas, builtins)
                                        Duration    u v   -> Record finalMeta ((key, Duration    u v)   : nextPairs)
                                        SchemaVal   s     -> Record finalMeta ((key, SchemaVal   s)     : nextPairs)
                                        Directive   d     -> Record finalMeta ((key, Directive   d)     : nextPairs)
                                        PrimitiveOp o     -> Record finalMeta ((key, PrimitiveOp o)     : nextPairs)

                                        -- Real type violations fall here (Metadata context blocks cannot be property values)
                                        otherVal -> let err = typeMismatch
                                                                  "a valid property value (like a Primitive or nested Record)"
                                                                  (humanReadableType otherVal)
                                                              & withBlurb (humanReadableType otherVal)
                                                              & OuroError (S.exprPos valExpr)
                                                    in Record finalMeta ((key, EvalError err) : nextPairs)

                             otherVal -> pure otherVal

                  -- Case D (Replaces Old B.25 and Old D): Safely drop malformed/loose attribute blocks
                  -- or attributes wrapping blocked structural layouts, and keep moving
                  S.Form _ (S.Symbol _ "attr" : _) : rest -> go metaAcc rest

                  -- Case E: Catch dangling expressions instead of silently erasing them
                  expr : rest
                      -> let err = typeMismatch
                                       "a structural record property (attr, inlay, context)"
                                       "an unbound expression"
                                   & withBlurb "Unbound expressions are not allowed inside Record blocks. They must be bound to an attribute."
                                   & OuroError (S.exprPos expr)
                         in go metaAcc rest <&> \case
                                Record m p -> Record m (("*err*", EvalError err) : p)
                                otherVal   -> otherVal


    -- Forces the field's promise. Every bindable field is given one by compileRecord,
    -- so evaluating the expression directly is only a fallback.
    fieldValue :: SourcePos -> S.Expr -> EvalM L.Expr
    fieldValue fieldPos valExpr =
        case Map.lookup fieldPos fieldAddrs of
            Just addr -> force inCurrentEnv fieldPos addr
            Nothing   -> evaluator env valExpr

    -- 'force' switches to the promise's env; evaluate in whatever env is current
    inCurrentEnv :: S.Expr -> EvalM L.Expr
    inCurrentEnv expr' = ask >>= \current -> evaluator current expr'


-- A key can appear once in a record
writtenTwice :: Text -> SourcePos -> OuroError
writtenTwice key pos =
    lexicalError ("The key '" <> key <> "' is written twice in the same record.")
    & withBlurb "Each key can appear once in a record. Perhaps remove one of the fields, or rename it."
    & OuroError pos


-- 'insert' (or 'replace-at') would give a record the same key twice
alreadyAField :: Text -> SourcePos -> OuroError
alreadyAField key pos =
    lexicalError ("The record already has a field '" <> key <> "'.")
    & withBlurb "Each key can appear once in a record. Perhaps use replace-at to change the existing field."
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

            -- Case A: 'define' and a local 'defn' were replaced by 'let' and 'fn'
            S.Form dPos (S.Symbol _ "define" : _) : xs
                -> (EvalError (removedDefine dPos) :) <$> compileElements env' xs

            S.Form dPos (S.Symbol _ keyword : _) : xs | isFunctionKeyword keyword
                -> (EvalError (removedLocalDefn dPos) :) <$> compileElements env' xs

            -- A private binding belongs in a let
            S.Form dPos (S.Symbol _ "%bind" : S.Literal _ (S.Str name) : _) : xs
                -> (EvalError (misplacedBinding name dPos) :) <$> compileElements env' xs

            -- Case B: TRUE ERASURE: Skip context blocks completely inside arrays
            S.Form _ [S.Symbol _ "context", S.Form _ _] : xs
                -> compileElements env' xs

            -- Case B.25 TRUE ERASURE: Skip ubound attrs from attr from
            S.Form _ [S.Symbol _ "attr", _, _] : xs
                -> compileElements env' xs

            -- Case B.5: Inlay evaluated array elements directly into the current array scope
            S.Form _ [S.Symbol sPos "inlay", iExpr] : xs
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

            -- Case C: Process structured nested forms (Records or trailing list matrices)
            ((S.Form _ fields) : xs)
                -- 1. Intercept Nested Arrays: recursively compile as a matrix
                | TargetList <- determineBlockTarget fields
                -> (:) <$> compileArray evaluator env' pos fields <*> compileElements env' xs

                -- 2. Intercept Nested Records: compile using the object scope builder
                | TargetRecord <- determineBlockTarget fields
                -> do
                   evaledItem <- compileRecord evaluator env' fields
                   restL      <- compileElements env' xs
                   pure $ case evaledItem of
                          -- Retain independent error leaves found inside nested scopes safely
                          EvalError err -> EvalError err : restL

                          -- Seamlessly capture the open object superset node
                          Record    m p -> Record    m p : restL
                          Primitive p   -> Primitive p   : restL
                          otherVal      -> typeMismatch
                                               "a valid nested Record block or a single value"
                                               (humanReadableType otherVal)
                                           & withBlurb (typeMismatchBlurb otherVal)
                                           & OuroError pos
                                           & (\e -> EvalError e : restL)

            -- Case D: Process flat scalar fields or variables evaluated within the element stream
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
                        Duration    u  v   -> Duration    u  v   : restL
                        SchemaVal   s      -> SchemaVal   s      : restL
                        Directive   d      -> Directive   d      : restL
                        PrimitiveOp o      -> PrimitiveOp o      : restL

                        otherVal -> let err = typeMismatch
                                                    "a plain data value (like a String, Number, or Boolean)"
                                                    (humanReadableType otherVal)
                                                & withBlurb (typeMismatchBlurb otherVal)
                                                & OuroError (S.exprPos otherExpr)
                                    in EvalError err : restL


-- Data type representing the structural target resolved by lookahead routing.
data BlockTarget
    = TargetRecord
    | TargetList
    | TargetFunctionApp
    deriving (Show, Eq)

-- Inspects incoming form tokens to determine if they compose an Record or a List.
determineBlockTarget :: [S.Expr] -> BlockTarget
determineBlockTarget fields =
    case fields of
        []       -> TargetList
        (x : xs) -> case isFunctionApplication (x : xs) of
                        True -> TargetFunctionApp
                        False -> case any isStructuralField fields of
                                         True  -> TargetRecord
                                         False -> TargetList

    where
    isFunctionApplication :: [S.Expr] -> Bool
    isFunctionApplication =
        \case
         (S.Symbol _ name : _) -> not (isFunctionKeyword name || name `elem` ["define", "context"])
         _NotaFunc             -> False

    isStructuralField :: S.Expr -> Bool
    isStructuralField =
        \case
         S.Form _ (S.Symbol _ "attr"    : _) -> True
         S.Form _ (S.Symbol _ "context" : _) -> True
         _other                              -> False


-- evalFunction
--
-- Calls a function. Each argument becomes a promise over the caller's environment,
-- so it means what it meant where it was written, and is evaluated at most once (and
-- only if used). The body runs in the function's closure (the frame it was defined
-- in) extended with a frame for the parameters.
--
-- Recursion is forbidden: the call stack comes from the caller and records each
-- function by identity, so nesting a call, like (f! (f! x)), is not recursion.
evalFunction
    :: (S.Expr -> EvalM L.Expr)       -- Core evaluator function
    -> Env                            -- The function's closure: the frame it is defined in
    -> SourcePos                      -- Callsite position
    -> L.FunctionId                   -- The function's name and definition position
    -> [Text]                         -- The function's formal parameters (e.g., (name year))
    -> [S.Expr]                       -- The un-evaluated body expressions of the function
    -> [S.Expr]                       -- The actual arguments passed at the call site
    -> EvalM L.Expr                   -- The returned evaluated expr
evalFunction evaluator closureEnv pos fnId@(name, _) params bodyExprs actualArgs = do
    callerEnv <- ask
    let parLen      = length params
        actLen      = length actualArgs
        callerStack = callerEnv ^. L.callStack

    case (parLen == actLen, Set.member fnId callerStack) of
        (False, _)
            -> incorrectArity name parLen actLen
               & OuroError pos
               & L.EvalError
               & pure

        (True, True)
            -> typeMismatch
                   "a terminating function call"
                   "a recursive function call"
               & withBlurb ("Recursion is strictly forbidden in Ouro. The function '" <> name <> "' attempted to call itself.")
               & OuroError pos
               & EvalError
               & pure

        (True, False)
            -> do
               -- The arguments' promises close over the caller's environment
               (_, argAddrs) <- allocFrame (const callerEnv)
                                    [ FrameEntry param (S.exprPos arg) arg False | (param, arg) <- zip params actualArgs ]

               let functionEnv = closureEnv
                               & L.localScope       .~ Map.fromList (zip params (map Promised argAddrs))
                               & L.functionRegistry .~ Map.empty
                               & L.parentEnv        .~ Just closureEnv
                               & L.callStack        .~ Set.insert fnId callerStack

                   -- Evaluate an expression in an explicit environment
                   evaluator' currentEnv expr' = local (const currentEnv) (evaluator expr')

               -- The body is one expression: the function's result
               case functionBody name pos bodyExprs of
                   Left  err    -> pure (EvalError err)
                   Right result -> evaluator' functionEnv result
