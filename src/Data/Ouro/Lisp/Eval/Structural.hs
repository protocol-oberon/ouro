{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Data.Ouro.Lisp.Eval.Structural where

import           Control.Monad.Reader        (Reader, runReader)
import           Data.Function               ((&))
import qualified Data.Map                    as Map
import           Data.Ouro.Error.Diagnostics (internalValueLeak, missingPathKey,
                                              typeMismatch, typeMismatchBlurb,
                                              unboundIdentifier, withBlurb)
import           Data.Ouro.Error.Types       (OuroError (..))
import qualified Data.Ouro.Internal.Expr     as I
import qualified Data.Ouro.Internal.Kinds    as JLD
import           Data.Ouro.Internal.Utils    (rankBySimilarity)
import           Data.Ouro.Lisp.Eval.Schema  (parseContextDirectives)
import           Data.Ouro.Lisp.Eval.Scope   (buildLazyEnv)
import           Data.Ouro.Lisp.Eval.Types   (Env (..), Expr (..), allEnvKeys,
                                              humanReadableType)
import qualified Data.Ouro.Lisp.Eval.Types   as L
import qualified Data.Ouro.Lisp.Surface      as S
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import           Lens.Micro                  ((.~), (^.))


-- compileScope.
--
-- Compiles a physical Lisp block into a self-contained, knot-tied lexical environment frame.
-- Manages local symbol mapping, dynamic property lazy evaluation, and JSON-LD schema context routing.
--
-- This worker serves as the critical transition boundary between surface form expressions and structural,
-- type-safe graph layers. Rather than applying standard top-down sequential evaluation, it operates in three distinct,
-- highly deliberate stages to enforce declarative order-independence within the local block:
--
--   1. Sweeping & Binding: It first passes over the fields using 'buildLazyEnv' to pull out all un-evaluated
--      attributes and definitions, organizing them into a flat local dictionary.
--   2. Environment Isolation & Knot-Tying: It constructs a fresh lexical 'Env' frame. By linking this frame
--      as its own parent and passing it downward, variables inside the block can lazily reference sibling
--      properties or forward-declare definitions seamlessly without triggering early-evaluation crashes.
--   3. Semantic Extraction & Context Lowering: It evaluates the properties into a core GADT structural list.
compileScope
    :: (Env -> S.Expr -> L.Expr)
    -> Env
    -> [S.Expr]
    -> L.Expr
compileScope evaluator env fields =
    case buildLazyEnv env fields of
        -- Dynamic environment allocation failures still represent a catastrophic scope break
        Left  err    -> EvalError err
        Right rawMap -> let isolatedEnv = env
                                & L.localScope .~ rawMap
                                -- Knot-tying: the parent of the isolated scope is the ambient env
                                & L.parentEnv  .~ Just env
                        in emitProps evaluator isolatedEnv fields


-- Iterates through a stream of tokens to filter and evaluate physical properties into a L.Expr superset tree.
emitProps
    :: (Env -> S.Expr -> L.Expr)
    -> Env
    -> [S.Expr]
    -> L.Expr
emitProps evaluator env expressions = go I.EmptyMeta expressions
    where
    go :: I.Expr 'JLD.Meta -> [S.Expr] -> L.Expr
    go metaAcc = \case
                  [] -> Object metaAcc []

                  -- Case A: Intercept ANY context form variant at the top-level and route to the schema engine
                  S.Form _ (S.Symbol pos "context" : directives) : remaining
                      -> case runReader (parseContextDirectives directives) env of
                             EvalError err
                                 -> case go metaAcc remaining of
                                        Object finalMeta nextPairs -> Object finalMeta (("@context", EvalError err) : nextPairs)
                                        otherVal                   -> otherVal

                             SchemaVal localSchema
                                 -> go (I.Context localSchema) remaining

                             otherVal
                                 -> let leakErr = internalValueLeak "emitProps context block resolution"
                                                  & withBlurb ("Expected a SchemaVal, but leaked: " <> humanReadableType otherVal)
                                                  & OuroError pos
                                    in case go metaAcc remaining of
                                          Object finalMeta nextPairs -> Object finalMeta (("@context", EvalError leakErr) : nextPairs)
                                          ov                         -> ov

                  -- Case B: Define Blocks are explicitly erased from the output JSON graph at comptime
                  S.Form _ (S.Symbol _ "define" : _) : rest -> go metaAcc rest

                  -- Case C: Extract valid body pairs. Supports lazy nesting compilation inline.
                  (S.Attr _ key : valExpr : rest) | not (isStructuralExpr valExpr)
                      -> case go metaAcc rest of
                             Object finalMeta nextPairs
                                 -> case evaluator env valExpr of
                                        -- 1. Catch error leaves completely independently
                                        EvalError err -> Object finalMeta ((key, EvalError err) : nextPairs)

                                        -- 2. Clean primitive values (frozen GADTs)
                                        Primitive prim -> Object finalMeta ((key, Primitive prim) : nextPairs)

                                        -- 3. Accept nested object configurations
                                        Object m p -> Object finalMeta ((key, Object m p) : nextPairs)

                                        -- 4. Accept array configurations
                                        Array elements -> Object finalMeta ((key, Array elements) : nextPairs)

                                        -- 5. Pass-through for valid domain primitives (Durations, Closures, Schemas, etc.)
                                        Duration    u v   -> Object finalMeta ((key, Duration u v) : nextPairs)
                                        SchemaVal   s     -> Object finalMeta ((key, SchemaVal s) : nextPairs)
                                        Directive   d     -> Object finalMeta ((key, Directive d) : nextPairs)
                                        PrimitiveOp o     -> Object finalMeta ((key, PrimitiveOp o) : nextPairs)
                                        Closure     e n x -> Object finalMeta ((key, Closure e n x) : nextPairs)

                                        -- Real type violations fall here (Metadata context blocks cannot be property values)
                                        otherVal -> let err = typeMismatch
                                                                  "a valid property value (like a primitive or nested object)"
                                                                  (humanReadableType otherVal)
                                                                  & withBlurb (humanReadableType otherVal)
                                                                  & OuroError (S.exprPos valExpr)
                                                    in Object finalMeta ((key, EvalError err) : nextPairs)

                             otherVal -> otherVal

                  -- Case D: If it's a loose keyword modifier layout, safely drop it and keep moving
                  S.Attr {} : rest -> go metaAcc rest

                  -- Case E: Erase exactly ONE unbound item element sequence loop and keep moving
                  _ : rest -> go metaAcc rest

    -- Helper layout guard to prevent key-value snatching across macro envelopes
    isStructuralExpr :: S.Expr -> Bool
    isStructuralExpr = \case
                        S.Attr _ _                          -> True
                        S.Form _ (S.Symbol _ "context" : _) -> True
                        S.Form _ (S.Symbol _ "define"  : _) -> True
                        _otherForm                          -> False


-- Compiles a collection of nested Lisp blocks into a uniform sequence array of Objects
compileArray
    :: (Env -> S.Expr -> L.Expr)
    -> Env
    -> [S.Expr]
    -> L.Expr
compileArray evaluator env elements = Array (compileElements elements)
    where
    compileElements :: [S.Expr] -> [L.Expr]
    compileElements exprs =
        case exprs of
            [] -> []

            -- Case A: TRUE ERASURE: Skip define blocks completely inside arrays
            S.Form _ (S.Symbol _ "define" : _) : xs -> compileElements xs

            -- Case B: TRUE ERASURE: Skip context blocks completely inside arrays
            S.Form _ [S.Symbol _ "context", S.Form _ _] : xs -> compileElements xs

            -- Case C: Process structured nested forms (Objects or trailing list matrices)
            (_formExpr@(S.Form pos fields) : xs)
                -- 1. Intercept Nested Arrays: recursively compile as a matrix
                | TargetList <- determineBlockTarget fields
                -> compileArray evaluator env fields : compileElements xs

                -- 2. Intercept Nested Objects: compile using the object scope builder
                | TargetObject <- determineBlockTarget fields
                -> let evaledItem = compileScope evaluator env fields
                       restL      = compileElements xs
                   in case evaledItem of
                          -- Retain independent error leaves found inside nested scopes safely
                          EvalError err -> EvalError err : restL

                          -- Seamlessly capture the open object superset node
                          Object m p    -> Object m p : restL
                          Primitive p   -> Primitive p : restL
                          otherVal      -> typeMismatch
                                               "a valid nested Object block or a single value"
                                               (humanReadableType otherVal)
                                           & withBlurb (typeMismatchBlurb otherVal)
                                           & OuroError pos
                                           & (\e -> EvalError e : restL)

            -- Case D: Process flat scalar fields or variables evaluated within the element stream
            (otherExpr : xs)
                -> let evaledVal = evaluator env otherExpr
                       restL     = compileElements xs
                    in case evaledVal of
                        EvalError err  -> EvalError err  : restL
                        Primitive prim -> Primitive prim : restL

                        -- Accept nested layouts or expressions inside the stream
                        Object      m  p   -> Object      m  p   : restL
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
    = TargetObject
    | TargetList
    | TargetFunctionApp
    deriving (Show, Eq)

-- Inspects incoming form tokens to determine if they compose an Object or a List.
determineBlockTarget :: [S.Expr] -> BlockTarget
determineBlockTarget =
    \case
     -- Rules for Object Detection (Immediate)
     S.Attr {} : _                                   -> TargetObject
     S.Form _ [S.Symbol _ "context", S.Form _ _] : _ -> TargetObject
     S.Form _ (S.Symbol _ "define" : _) : _          -> TargetObject

     -- Rules for List/Array Detection (Immediate Scalars)
     S.Literal _ _ : _          -> TargetList
     -- Recursive structural inspection for nested blocks
     S.Form _ innerContents : _ -> case determineBlockTarget innerContents of
                                       TargetObject      -> TargetList  -- Array of Objects: ((:id 1) (:id 2))
                                       TargetList        -> TargetList  -- Array of Lists (Nested): ((1 2) (3 4))
                                       TargetFunctionApp -> TargetList  -- Array of Expressions: ((add 1 2) (sub 3 4))

     -- Fallback: Default to a standard function/operator invocation
     _otherForm -> TargetFunctionApp

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
    :: (S.Expr -> Reader Env L.Expr)
    -> Env
    -> S.Expr
    -> [L.Expr]
    -> L.Expr
resolvePath evaluator fullEnv originalExpr pathVals = go fullEnv originalExpr (extractKeys pathVals)
    where
    -- Extract bare Text strings from the newly provisioned Array superset layout
    extractKeys :: [L.Expr] -> [Text]
    extractKeys elements = concatMap (
                             \case
                             Primitive (I.String k) -> [k]
                             _                      -> []
                           ) elements

    go :: Env -> S.Expr -> [Text] -> L.Expr
    go env currentExpr keys = case keys of
        [] -> runReader (evaluator currentExpr) env

        (targetKey : remainingKeys)
            -> case currentExpr of
                   -- Strategy 1: Resolve symbol pointers out of environment frames
                   S.Symbol pos varName -> case Map.lookup varName (env ^. L.localScope) of
                       Just linkedExpr -> go env linkedExpr (targetKey : remainingKeys)
                       Nothing
                           -> case env ^. L.parentEnv of
                               Just pEnv -> go pEnv currentExpr (targetKey : remainingKeys)
                               Nothing   -> let allKeys    = Set.toList $ allEnvKeys fullEnv
                                                suggestion = case rankBySimilarity varName allKeys of
                                                               ((bestMatch, score) : _) | score <= 3
                                                                   -> "\n\nPerhaps you meant: '"
                                                                   <> bestMatch
                                                                   <> "'?"
                                                               _   -> ""
                                           in unboundIdentifier varName
                                               & withBlurb ( "The evaluator attempted to lookup the value for '"
                                                           <> varName <> "', "
                                                           <> "but the identifier failed to resolve"
                                                           <> " within any active scope chain."
                                                           <> suggestion
                                                           )
                                               & OuroError pos
                                               & EvalError

                   -- Strategy 2: Scan Form wrappers using our strict token matching rules
                   S.Form pos elements
                       -> scanEnv env targetKey remainingKeys elements
                                   (missingPathKey targetKey [k | S.Attr _ k <- elements]
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
                               (missingPathKey targetKey [k | S.Attr _ k <- elements]
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

    scanEnv :: Env -> Text -> [Text] -> [S.Expr] -> OuroError -> L.Expr
    scanEnv env trgt rest exprs err =
        case matchTokenStream trgt exprs of
            Just matchedVal -> go env matchedVal rest
            Nothing         -> EvalError $ err


matchTokenStream :: Text -> [S.Expr] -> Maybe S.Expr
matchTokenStream targetKey stream =
    case stream of
        [] -> Nothing
        (S.Attr _ k : valExpr : _)
            | k == targetKey -> Just valExpr

        (S.Symbol _ "define" : S.Attr _ varName : S.Form _ innerBody : _)
            | varName == targetKey -> matchTokenStream targetKey innerBody

        (_ : rest) -> matchTokenStream targetKey rest

