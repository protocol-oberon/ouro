{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Data.Ouro.Lisp.Eval.Structural where

import           Control.Monad.Reader        (Reader, runReader)
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
import           Data.Ouro.Lisp.Eval.Scope   (buildLazyEnv, buildNestedTemplate)
import           Data.Ouro.Lisp.Eval.Types   (Env (..), Expr (..), allEnvKeys,
                                              humanReadableType, localScope)
import qualified Data.Ouro.Lisp.Eval.Types   as L
import qualified Data.Ouro.Lisp.Surface      as S
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import           Lens.Micro                  ((.~), (^.))
import           Text.Megaparsec             (SourcePos)
import Lens.Micro.Platform ((%~))


-- compileRecord.
--
-- Compiles a physical Lisp block into a self-contained, knot-tied lexical environment frame.
-- Manages local symbol mapping, dynamic property lazy evaluation, and JSON-LD schema context routing.
--
-- This worker serves as the critical transition boundary between surface form expressions and structural,
-- type-safe graph layers. Rather than applying standard top-down sequential evaluation, it operates in three distinct,
-- highly deliberate stages to enforce declarative order-independence within the local block:
--
--   1. Sweeping & Binding: It passes over the fields using both 'buildLazyEnv' and 'buildTemplateRegistry'
--      to harvest un-evaluated attributes and macro-blueprints into isolated dictionaries.
--   2. Environment Isolation & Knot-Tying: It constructs a fresh lexical 'Env' frame containing both scopes.
--      By linking this frame as its own parent and passing it downward, variables and templates inside the block
--      can lazily reference sibling properties seamlessly without triggering early-evaluation crashes.
--   3. Semantic Extraction & Context Lowering: It evaluates the properties into a core GADT structural list.
compileRecord
    :: (Env -> S.Expr -> L.Expr)
    -> Env
    -> [S.Expr]
    -> L.Expr
compileRecord evaluator env fields =
    case buildNestedTemplate env fields of
        Left err
            -> EvalError err

        Right envWithTemplates
            -> case buildLazyEnv env fields of
                   -- Dynamic environment allocation failures represent a catastrophic scope break
                   Left err
                       -> EvalError err

                   Right rawMap
                       -> let isolatedEnv = envWithTemplates
                                            & L.localScope .~ rawMap
                                            -- Knot-tying: the parent of the isolated scope is the ambient env
                                            & L.parentEnv  .~ Just env
                          in emitProps evaluator isolatedEnv fields


-- Iterates through a stream of tokens to filter and evaluate physical properties into a L.Expr superset tree.
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
                  [] -> Record metaAcc []

                  -- Case A: Intercept ANY context form variant at the top-level and route to the schema engine
                  S.Form _ (S.Symbol pos "context" : directives) : remaining
                      -> case runReader (parseContextDirectives directives) env of
                             EvalError err
                                 -> case go metaAcc remaining of
                                        Record finalMeta nextPairs -> Record finalMeta (("@context", EvalError err) : nextPairs)
                                        otherVal                   -> otherVal

                             SchemaVal localSchema
                                 -> go (I.Context localSchema) remaining

                             otherVal
                                 -> let leakErr = internalValueLeak "emitProps context block resolution"
                                                  & withBlurb ("Expected a SchemaVal, but leaked: " <> humanReadableType otherVal)
                                                  & OuroError pos
                                    in case go metaAcc remaining of
                                          Record finalMeta nextPairs -> Record finalMeta (("@context", EvalError leakErr) : nextPairs)
                                          ov                         -> ov

                  -- Case B: Define Blocks are explicitly erased from the output JSON graph at comptime
                  S.Form _ (S.Symbol _ "define" : _) : rest -> go metaAcc rest

                  -- Case B.5 Template Blocks are also explicity erased from output JSON graph at comptime
                  S.Form _ (S.Symbol _ "template" : _) : rest -> go metaAcc rest

                  -- Case B.75: Inlay evaluated record pairs directly into the current record scope
                  S.Form _ [S.Symbol pos "inlay", iExpr] : rest
                      -> let blockTarget = case iExpr of
                                               S.Form _ inner -> determineBlockTarget inner
                                               _              -> TargetFunctionApp -- Treat symbols/primitives as dynamic
                         in case blockTarget of
                                TargetList -> let err = typeMismatch
                                                            "a valid Record (or Template resolving to a Record) to inlay"
                                                            "a List/Array block target"
                                                        & OuroError pos
                                              in case go metaAcc rest of
                                                     Record finalMeta nextPairs -> Record finalMeta (("*err*", EvalError err) : nextPairs)
                                                     otherVal                   -> otherVal

                                -- Catch both TargetRecord AND TargetFunctionApp (for templates)
                                _ -> case go metaAcc rest of
                                        Record finalMeta nextPairs
                                            -> case evaluator env iExpr of
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

                                        otherVal -> otherVal

                  -- Case C: Extract valid body pairs using the new structured attribute form.
                  -- Supports lazy nesting compilation inline.
                  S.Form _ [S.Symbol _ "attr", S.Literal _ (S.Str key), valExpr] : rest
                      | not (isStructuralExpr valExpr)
                      -> case go metaAcc rest of
                             Record finalMeta nextPairs
                                 -> case evaluator env valExpr of
                                        -- 1. Catch error leaves completely independently
                                        EvalError err -> Record finalMeta ((key, EvalError err) : nextPairs)

                                        -- 2. Clean primitive values (frozen GADTs)
                                        Primitive prim -> Record finalMeta ((key, Primitive prim) : nextPairs)

                                        -- 2.5 Quotes
                                        Quote q -> Record finalMeta ((key, Quote q) : nextPairs)

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

                             otherVal -> otherVal

                  -- Case D (Replaces Old B.25 and Old D): Safely drop malformed/loose attribute blocks
                  -- or attributes wrapping blocked structural layouts, and keep moving
                  S.Form _ (S.Symbol _ "attr" : _) : rest -> go metaAcc rest

                  -- Case E: Erase exactly ONE unbound item element sequence loop and keep moving
                  _ : rest -> go metaAcc rest

    -- Helper layout guard to prevent key-value snatching across macro envelopes
    isStructuralExpr :: S.Expr -> Bool
    isStructuralExpr = \case
                        S.Form _ (S.Symbol _ "attr"     : _) -> True
                        S.Form _ (S.Symbol _ "context"  : _) -> True
                        S.Form _ (S.Symbol _ "define"   : _) -> True
                        S.Form _ (S.Symbol _ "template" : _) -> True
                        S.Form _ (S.Symbol _ "return"   : _) -> True
                        _otherForm                           -> False


-- Compiles a collection of nested Lisp blocks into a uniform Array
compileArray
    :: (Env -> S.Expr -> L.Expr)
    -> Env
    -> SourcePos
    -> [S.Expr]
    -> L.Expr
compileArray evaluator env pos elements =
    case buildNestedTemplate env elements of
        Right envWithTmplts -> Array $ compileElements envWithTmplts elements
        Left  err           -> EvalError err

    where
    compileElements :: Env -> [S.Expr] -> [L.Expr]
    compileElements env' exprs =
        case exprs of
            [] -> []

            -- Case A: TRUE ERASURE: Skip define blocks completely inside arrays
            -- Still inject variables into scope
            S.Form _ (S.Symbol _ "define" : variables) : xs
                -> case buildLazyEnv env' variables of
                       Right newEnv -> compileElements (env' & localScope %~ Map.union newEnv) xs
                       Left  err    -> EvalError err : compileElements env' xs

            -- Skip Templates
            S.Form _ (S.Symbol _ "template" : _) : xs
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
                                         _              -> TargetFunctionApp -- Treat symbols/templates as dynamic
                   in case blockTarget of
                          TargetRecord -> let err = typeMismatch
                                                        "a valid Array (or Template resolving to an Array) to inlay"
                                                        "a Record block target"
                                                    & OuroError sPos
                                          in EvalError err : compileElements env' xs

                          -- Catch both TargetList AND TargetFunctionApp (for templates/variables)
                          _ -> case evaluator env' iExpr of
                                   -- 1. If evaluation fails, embed the error so the tree retains it
                                   EvalError err  -> EvalError err : compileElements env' xs

                                   -- 2. The successful path: Flatten the evaluated elements into the stream
                                   Array elems -> elems ++ compileElements env' xs

                                   -- 3. If it evaluated to something other than an array, throw a type mismatch
                                   otherVal    -> let err = typeMismatch
                                                                "a valid Array to inlay into the current array scope"
                                                                (humanReadableType otherVal)
                                                            & OuroError (S.exprPos iExpr)
                                                  in EvalError err : compileElements env' xs

            -- Case C: Process structured nested forms (Records or trailing list matrices)
            (_formExpr@(S.Form _ fields) : xs)
                -- 1. Intercept Nested Arrays: recursively compile as a matrix
                | TargetList <- determineBlockTarget fields
                -> compileArray evaluator env' pos fields : compileElements env' xs

                -- 2. Intercept Nested Records: compile using the object scope builder
                | TargetRecord <- determineBlockTarget fields
                -> let evaledItem = compileRecord evaluator env' fields
                       restL      = compileElements env' xs
                   in case evaledItem of
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
                -> let evaledVal = evaluator env' otherExpr
                       restL     = compileElements env' xs
                    in case evaledVal of
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
         (S.Symbol _ name : _) -> not (name `elem` ["template", "define", "context"])
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
    :: Maybe (S.Expr -> Reader Env L.Expr)
    -> Env
    -> S.Expr
    -> [L.Expr]
    -> L.Expr
resolvePath shouldEval fullEnv originalExpr pathVals = go fullEnv originalExpr (extractKeys pathVals)
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
        [] -> case shouldEval of
                  Just evaluator -> runReader (evaluator currentExpr) env
                  Nothing        -> L.Quote currentExpr

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
                                                               _  -> ""
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

    scanEnv :: Env -> Text -> [Text] -> [S.Expr] -> OuroError -> L.Expr
    scanEnv env trgt rest exprs err =
        case matchTokenStream trgt exprs of
            Just matchedVal -> go env matchedVal rest
            Nothing         -> EvalError $ err


matchTokenStream :: Text -> [S.Expr] -> Maybe S.Expr
matchTokenStream targetKey stream =
    case stream of
        [] -> Nothing

        -- Match the new structured attribute form
        (S.Form _ [S.Symbol _ "attr", S.Literal _ (S.Str k), valExpr] : _)
            | k == targetKey -> Just valExpr

        -- Match the specific 'define' scope piercing pattern using the new attribute structure
        (S.Symbol _ "define" : S.Form _ [S.Symbol _ "attr", S.Literal _ (S.Str varName), S.Form _ innerBody] : _)
            | varName == targetKey -> matchTokenStream targetKey innerBody

        (_ : rest) -> matchTokenStream targetKey rest


-- compileTemplate.
--
-- Executes an AST blueprint within an isolated lexical bubble.
-- By mapping formal parameters directly to un-evaluated argument expressions,
-- this achieves macro-style lazy expansion without the messy string-substitution logic.
compileTemplate
    :: (S.Expr -> Reader Env L.Expr)  -- Core evaluator function
    -> Env                            -- The current ambient environment (Call site)
    -> SourcePos                      -- Callsite position
    -> Text                           -- Template name
    -> [Text]                         -- The template's formal parameters (e.g., (name year))
    -> [S.Expr]                       -- The un-evaluated body expressions of the template
    -> [S.Expr]                       -- The actual arguments passed at the call site
    -> Reader Env L.Expr              -- The returned evaluated expr
compileTemplate evaluator parentEnv pos name params bodyExprs actualArgs =
    let parLen = length params
        actLen = length actualArgs
    in case parLen == actLen of
           True  -> do
                    let argMap      = Map.fromList (zip params actualArgs)
                        templateEnv = parentEnv
                                      & L.localScope       .~ argMap
                                      & L.parentEnv        .~ Just parentEnv
                                      & L.activeLookups    .~ Set.empty
                                      & L.templateRegistry .~ (parentEnv ^. L.templateRegistry)

                        -- Unwrap the Reader Monad to explict (Env -> S.Expr -> L.Expr)
                        evaluator' currentEnv expr' = runReader (evaluator expr') currentEnv

                    -- Pass the prepared environment down into the block compiler
                    case determineBlockTarget bodyExprs of
                        TargetRecord      -> pure $ compileRecord evaluator' templateEnv bodyExprs
                        TargetList        -> pure $ compileArray  evaluator' templateEnv pos bodyExprs
                        TargetFunctionApp -> typeMismatch
                                                 "a template that evaluate to either a Record or an Array"
                                                 "a ast shape which corresponds to function appliaction"
                                             & OuroError pos
                                             & L.EvalError
                                             & pure

           False -> incorrectArity name parLen actLen
                    & OuroError pos
                    & L.EvalError
                    & pure
