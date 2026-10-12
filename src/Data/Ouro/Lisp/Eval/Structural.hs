{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Data.Ouro.Lisp.Eval.Structural where

import           Control.Monad.Reader        (ask, local, runReader)
import           Control.Monad.State.Strict  (gets)
import qualified Data.IntMap.Strict          as IntMap
import           Data.Functor                ((<&>))
import           Data.Function               ((&))
import qualified Data.Map                    as Map
import           Data.Ouro.Error.Diagnostics (incorrectArity, internalValueLeak,
                                              missingPathKey, typeMismatch,
                                              typeMismatchBlurb,
                                              unboundIdentifier, withBlurb)
import           Data.Ouro.Error.Types       (OuroError (..))
import qualified Data.Ouro.Internal.Expr     as I
import qualified Data.Ouro.Internal.Kinds    as JLD
import           Data.Ouro.Internal.Utils    (rankBySimilarity)
import           Data.Ouro.Lisp.Eval.Schema  (parseContextDirectives)
import           Data.Ouro.Lisp.Eval.Scope   (FrameEntry (..), allocFrame,
                                              bindableAttr, buildLocalFunction,
                                              collectBindings, force)
import           Data.Ouro.Lisp.Eval.Types   (Binding (..), Env (..), EvalM, Expr (..),
                                              allEnvKeys, humanReadableType,
                                              localScope)
import qualified Data.Ouro.Lisp.Eval.Types   as L
import qualified Data.Ouro.Lisp.Surface      as S
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import           Lens.Micro.Platform         ((%~), (.~), (^.))
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
--   1. Sweeping & Binding: It passes over the fields using both 'collectBindings' and 'buildLocalFunction'
--      to harvest un-evaluated attributes and macro-blueprints into isolated dictionaries.
--   2. Environment Isolation & Knot-Tying: It constructs a fresh lexical 'Env' frame containing both scopes.
--      By linking this frame as its own parent and passing it downward, variables and functions inside the block
--      can lazily reference sibling properties seamlessly without triggering early-evaluation crashes.
--   3. Semantic Extraction & Context Lowering: It evaluates the properties into a core GADT structural list.
compileRecord
    :: (Env -> S.Expr -> EvalM L.Expr)
    -> Env
    -> [S.Expr]
    -> EvalM L.Expr
compileRecord evaluator env fields =
    case buildLocalFunction env fields of
        Left err
            -> pure (EvalError err)

        Right envWithFuncs
            -> case collectBindings fields of
                   -- Dynamic environment allocation failures represent a catastrophic scope break
                   Left err
                       -> pure (EvalError err)

                   Right entries
                       -> do
                          -- Every binding becomes a promise in a new frame, whose parent is the ambient env
                          (frame, addrs) <- allocFrame (\names -> envWithFuncs
                                                                  & L.localScope .~ names
                                                                  & L.parentEnv  .~ Just env)
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
    go :: I.Expr 'JLD.Meta -> [S.Expr] -> EvalM L.Expr
    go metaAcc = \case
                  [] -> pure (Record metaAcc [])

                  -- Case A: Intercept ANY context form variant at the top-level and route to the schema engine
                  -- (Schema parsing only reads the environment, so it runs outside EvalM)
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

                  -- Case B: Define Blocks are explicitly erased from the output JSON graph at comptime
                  S.Form _ (S.Symbol _ "define" : _) : rest -> go metaAcc rest

                  -- Case B.5 Function Blocks are also explicity erased from output JSON graph at comptime
                  S.Form _ (S.Symbol _ "defun" : _) : rest -> go metaAcc rest

                  -- Case B.75: Inlay evaluated record pairs directly into the current record scope
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

                                                    -- 2. The successful path: Merge the evaluated pairs (p) into the current scope
                                                    Record _ p    -> Record finalMeta (p <> nextPairs)

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

                                        -- 5. Pass-through for valid domain primitives (Durations, Closures, Schemas, etc.)
                                        Duration    u v   -> Record finalMeta ((key, Duration    u v)   : nextPairs)
                                        SchemaVal   s     -> Record finalMeta ((key, SchemaVal   s)     : nextPairs)
                                        Directive   d     -> Record finalMeta ((key, Directive   d)     : nextPairs)
                                        PrimitiveOp o     -> Record finalMeta ((key, PrimitiveOp o)     : nextPairs)
                                        Closure     e n x -> Record finalMeta ((key, Closure     e n x) : nextPairs)

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
                                       "a structural record property (attr, inlay, define, defun, context)"
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


-- Compiles a collection of nested Lisp blocks into a uniform Array
compileArray
    :: (Env -> S.Expr -> EvalM L.Expr)
    -> Env
    -> SourcePos
    -> [S.Expr]
    -> EvalM L.Expr
compileArray evaluator env pos elements =
    case buildLocalFunction env elements of
        Right envWithTmplts -> Array <$> compileElements envWithTmplts elements
        Left  err           -> pure (EvalError err)

    where
    compileElements :: Env -> [S.Expr] -> EvalM [L.Expr]
    compileElements env' exprs =
        case exprs of
            [] -> pure []

            -- Case A: TRUE ERASURE: Skip define blocks completely inside arrays
            -- Still inject variables into scope
            S.Form _ (S.Symbol _ "define" : variables) : xs
                -> case collectBindings variables of
                       Right entries -> do
                                        (withDefines, _) <- allocFrame (\names -> env' & localScope %~ Map.union names) entries
                                        compileElements withDefines xs
                       Left  err     -> (EvalError err :) <$> compileElements env' xs

            -- Skip Functions
            S.Form _ (S.Symbol _ "defun" : _) : xs
                -> compileElements env' xs

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
                        Closure     e  n x -> Closure     e  n x : restL

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
         (S.Symbol _ name : _) -> not (name `elem` ["defun", "define", "context", "return"])
         _NotaFunc             -> False

    isStructuralField :: S.Expr -> Bool
    isStructuralField =
        \case
         S.Form _ (S.Symbol _ "attr"    : _) -> True
         S.Form _ (S.Symbol _ "context" : _) -> True
         _other                              -> False


-- resolvePath.
--
-- Recursively traverses raw surface syntax structural envelopes to isolate a specific node target.
-- Resolves path routes by matching structural keys against forms, lists, and variable scope bindings.
--
-- This function serves as the declarative execution engine underlying the macro 'get' subsystem.
-- Instead of resolving paths using pre-computed, flattened value objects, it walks un-evaluated
-- structural expressions directly. It operates across three distinct evaluation patterns to trace
-- data branches without accidentally forcing global block evaluation side effects:
--
--   1. Scope Resolution Pointer Hoisting: Encountering symbols triggers a dynamic lookUp walk. It checks
--      local maps and scales parent frames, updating the active environment context dynamically.
--   2. Stream Lookahead Piercing: When tracing key segments through structured blocks, it leverages
--      'matchTokenStream' to track assignments down and bypass metadata or compile-time wrappers.
--   3. Defensive Boundary Isolation: Terminal primitive leafs or missing path segments safely trigger
--      failures via 'missingPathKey' before deep internal evaluation can result in invalid mutations.
resolvePath
    :: (S.Expr -> EvalM L.Expr)
    -> Env
    -> S.Expr
    -> [L.Expr]
    -> EvalM L.Expr
resolvePath evaluator fullEnv originalExpr pathVals = go fullEnv originalExpr (extractKeys pathVals)
    where
    -- Extract bare Text strings from the newly provisioned Array superset layout
    extractKeys :: [L.Expr] -> [Text]
    extractKeys elements = concatMap (
                             \case
                             Primitive (I.String k) -> [k]
                             _                      -> []
                           ) elements

    go :: Env -> S.Expr -> [Text] -> EvalM L.Expr
    go env currentExpr keys = case keys of
        [] -> local (const env) (evaluator currentExpr)

        (targetKey : remainingKeys)
            -> case currentExpr of
                   -- Strategy 1: Resolve symbol pointers out of environment frames
                   S.Symbol pos varName -> case Map.lookup varName (env ^. L.localScope) of
                       -- Follow a binding into its code, in the environment it was written in
                       Just (Unshared linkedExpr) -> go env linkedExpr (targetKey : remainingKeys)
                       Just (Promised addr)
                           -> do
                              promise <- gets (IntMap.! addr)
                              go (promise ^. L.promiseEnv) (promise ^. L.promiseExpr) (targetKey : remainingKeys)
                       Nothing
                           -> case env ^. L.parentEnv of
                               Just pEnv -> go pEnv currentExpr (targetKey : remainingKeys)
                               Nothing   -> let allKeys    = Set.toList $ allEnvKeys fullEnv
                                                suggestion = case rankBySimilarity varName allKeys of
                                                               ((bestMatch, score) : _) | score <= 3
                                                                  -> "\n\nPerhaps you meant: '"
                                                                  <> bestMatch
                                                                  <> "'?"
                                                               _  -> ""
                                            in unboundIdentifier varName
                                               & withBlurb ( "The evaluator attempted to lookup the value for '"
                                                          <> varName <> "', "
                                                          <> "but the identifier failed to resolve within any active scope chain."
                                                          <> suggestion
                                                           )
                                               & OuroError pos
                                               & EvalError
                                               & pure

                   -- Strategy 2: Scan Form wrappers using our strict token matching rules
                   S.Form pos elements
                       -> scanEnv env targetKey remainingKeys elements
                                  (missingPathKey targetKey [k | S.Form _ [S.Symbol _ "attr", S.Literal _ (S.Str k), _] <- elements]
                                  & withBlurb ( "The path resolution engine could not locate the key '"
                                              <> targetKey
                                              <> "' inside the active form structure."
                                              <> "\n\nPerhaps you misspelled the property handle?"
                                              )
                                  & OuroError pos
                                  )

                   -- Strategy 3: Scan Bracket wrappers identically
                   S.Bracket pos elements
                       -> scanEnv env targetKey remainingKeys elements
                               (missingPathKey targetKey [k | S.Form _ [S.Symbol _ "attr", S.Literal _ (S.Str k), _] <- elements]
                               & withBlurb ( "The path resolution engine could not locate the key '"
                                           <> targetKey
                                           <> "' inside the active bracket matrix."
                                           <> "\n\nPerhaps you misspelled the property handle?"
                                           )
                               & OuroError pos
                               )

                   -- Catch-all for premature scalar leaf nodes
                   other -> missingPathKey targetKey []
                           & withBlurb ( "The path resolution engine attempted to dig into the key '"
                                       <> targetKey
                                       <> "', but hit a terminal primitive scalar leaf value instead.\n\n"
                                       <> "Perhaps the schema mapping layout has changed?"
                                       )
                           & OuroError (S.exprPos other)
                           & EvalError
                           & pure

    scanEnv :: Env -> Text -> [Text] -> [S.Expr] -> OuroError -> EvalM L.Expr
    scanEnv env trgt rest exprs err =
        case matchTokenStream trgt exprs of
            Just matchedVal -> go env matchedVal rest
            Nothing         -> pure (EvalError err)


matchTokenStream :: Text -> [S.Expr] -> Maybe S.Expr
matchTokenStream targetKey stream =
    case stream of
        [] -> Nothing

        -- Match the new structured attribute form
        (S.Form _ [S.Symbol _ "attr", S.Literal _ (S.Str k), valExpr] : _)
            | k == targetKey
            -> Just valExpr

        -- Match the specific 'define' scope piercing pattern using the new attribute structure
        (S.Symbol _ "define" : S.Form _ [S.Symbol _ "attr", S.Literal _ (S.Str varName), S.Form _ innerBody] : _)
            | varName == targetKey
            -> matchTokenStream targetKey innerBody

        (_ : rest)
            -> matchTokenStream targetKey rest


-- evalFunction
--
-- Executes an AST blueprint within an isolated lexical bubble.
-- By mapping formal parameters directly to un-evaluated argument expressions,
-- this achieves macro-style lazy expansion without the messy string-substitution logic.
evalFunction
    :: (S.Expr -> EvalM L.Expr)       -- Core evaluator function
    -> Env                            -- The current ambient environment (Call site)
    -> SourcePos                      -- Callsite position
    -> Text                           -- Function name
    -> [Text]                         -- The function's formal parameters (e.g., (name year))
    -> [S.Expr]                       -- The un-evaluated body expressions of the function
    -> [S.Expr]                       -- The actual arguments passed at the call site
    -> EvalM L.Expr                   -- The returned evaluated expr
evalFunction evaluator parentEnv pos name params bodyExprs actualArgs =
    let parLen       = length params
        actLen       = length actualArgs
        currentStack = parentEnv ^. L.callStack
    in case parLen == actLen of
           True  -> do
                    case Set.member name currentStack of
                        True  -> typeMismatch
                                     "a terminating function call"
                                     "a recursive function call"
                                 & withBlurb ("Recursion is strictly forbidden in Ouro. The function '" <> name <> "' attempted to call itself.")
                                 & OuroError pos
                                 & EvalError
                                 & pure
                        False
                            -> do
                               -- Arguments stay unevaluated expressions until step 4 of the scoping redesign
                               let argMap      = Map.fromList (zip params (map Unshared actualArgs))
                                   functionEnv = parentEnv
                                               & L.localScope       .~ argMap
                                               & L.parentEnv        .~ Just parentEnv
                                               & L.activeLookups    .~ Set.empty
                                               & L.functionRegistry .~ (parentEnv ^. L.functionRegistry)
                                               & L.callStack        .~ (Set.insert name currentStack)

                                   -- Evaluate an expression in an explicit environment
                                   evaluator' currentEnv expr' = local (const currentEnv) (evaluator expr')

                               case returnExpr bodyExprs of
                                   Just (S.Form _ (_ : val : _))
                                       -> do
                                           case collectBindings bodyExprs of
                                               Right entries -> do
                                                                (bodyFrame, _) <- allocFrame (\names -> functionEnv
                                                                                                       & L.localScope .~ names
                                                                                                       & L.parentEnv  .~ Just functionEnv)
                                                                                             entries
                                                                evaluator' bodyFrame val
                                               Left  err     -> pure $ EvalError err

                                   _noReturnExpression
                                       -> case determineBlockTarget bodyExprs of
                                               TargetRecord      -> compileRecord evaluator' functionEnv bodyExprs
                                               TargetList        -> compileArray  evaluator' functionEnv pos bodyExprs
                                               TargetFunctionApp -> typeMismatch
                                                                       "a function with a concrete return type (Array, Record, Primitive or another Function)"
                                                                       "a function with a dangling expression"
                                                                   & withBlurb
                                                                       ("Perhaps wrap the body of the function: '" <> name <> "' in a (return) expresion.")
                                                                   & OuroError pos
                                                                   & L.EvalError
                                                                   & pure

           False -> incorrectArity name parLen actLen
                    & OuroError pos
                    & L.EvalError
                    & pure

    where
    returnExpr :: [S.Expr] -> Maybe S.Expr
    returnExpr =
        \case
         []                                                -> Nothing
         (S.Form fPos (S.Symbol sPos "return" : rest) : _) -> Just (S.Form fPos (S.Symbol sPos "return" : rest))
         (_ : xs)                                          -> returnExpr xs
