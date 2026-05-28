{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Data.Ouro.Lisp.Eval.Engine where

import qualified Data.Map.Strict              as Map
import qualified Data.Ouro.Internal.Expr      as I
import qualified Data.Ouro.Internal.Kinds     as JLD
import           Data.Ouro.Internal.Schema    (Schema (..),
                                               SchemaDirective (..))
import           Data.Ouro.Lisp.Eval.Builtins (builtinRegistry, parseISO8601)
import           Data.Ouro.Lisp.Eval.Types    (Env (..), Value (..))
import qualified Data.Ouro.Lisp.Surface       as S
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Void                    (Void)
import           Text.Megaparsec              (Parsec, errorBundlePretty,
                                               runParser)
import qualified Text.URI                     as URI


-- High-level engine entry point. Inspects a parsed AST node, handles routing,
-- and unboxes the underlying GADT primitive value directly.
evaluateRoot :: S.Expr -> Either String (I.Expr 'JLD.Primitive)
evaluateRoot rootExpr = do
                        let baseEnv = Env { localScope = Map.empty, parentEnv = Nothing }

                        -- 1. Route the evaluation based on the structural shape of the root expression
                        evaluatedVal <- case rootExpr of
                                            S.Form _ allFields -> case determineBlockTarget allFields of
                                                                      TargetList        -> evalArray baseEnv allFields
                                                                      TargetObject      -> compileScope baseEnv allFields
                                                                      TargetFunctionApp -> evalExpr baseEnv rootExpr
                                            otherExpr -> evalExpr baseEnv otherExpr

                        -- 2. Enforce that the output successfully reduced down to a valid primitive schema graph
                        case evaluatedVal of
                            Primitive finalGadtTree -> pure finalGadtTree
                            _                       -> Left $ "Compile Error: Root configuration file must "
                                                               ++ "reduce down to a concrete Object or Primitive structure."


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
compileScope :: Env -> [S.Expr] -> Either String Value
compileScope env fields = do
    -- Step 1: Gather raw, un-evaluated structural maps from the block fields
    rawMap <- buildLazyEnv env fields

    -- Step 2: Spin up the dynamic knot-tied environment using our completed map
    let isolatedEnv = Env { localScope = rawMap, parentEnv = Just env }

    -- Step 3: Evaluate properties inline, dynamically distilling both the
    -- metadata leaf layer and the physical object data body spine at once!
    (metadataLeaf, bodySpine) <- emitProps isolatedEnv fields

    -- Step 4: Construct the unified, type-safe JLD Object frame
    pure $ Primitive (I.Object metadataLeaf bodySpine)


-- Iterates through a stream of tokens to filter and evaluate physical properties into a GADT List.
emitProps :: Env -> [S.Expr] -> Either String (I.Expr 'JLD.Meta, I.Expr 'JLD.List)
emitProps env expressions = go I.EmptyMeta expressions
    where
    go :: I.Expr 'JLD.Meta -> [S.Expr] -> Either String (I.Expr 'JLD.Meta, I.Expr 'JLD.List)
    go metaAcc = \case
                  [] -> pure (metaAcc, I.Nil)

                  -- A. Intercept ANY context form variant at the top-level and route to the schema engine
                  S.Form _ (S.Symbol _ "context" : directives) : remaining -> do
                                                                        localSchema <- parseContextDirectives directives
                                                                        go (I.Context localSchema) remaining

                  -- B. Define Blocks are explicitly erased from the output JSON graph at comptime
                  S.Form _ (S.Symbol _ "define" : _) : rest -> go metaAcc rest

                  -- C. Extract valid body pairs. Supports lazy nesting compilation inline.
                  S.Attr _ key : valExpr : rest
                      | not (isStructuralExpr valExpr)
                          -> do
                             restVal            <- evalExpr env valExpr
                             (finalMeta, nextL) <- go metaAcc rest
                             case restVal of
                                 Primitive prim -> pure (finalMeta, I.Cons (I.Attr key prim) nextL)
                                  -- If it evaluates to an entire sub-object, pass it downstream safely!
                                 _              -> Left "Type Error: Object layout mismatch encountered during property assignment pass."

                  -- D. If it's a loose keyword modifier layout, safely drop it and keep moving
                  S.Attr {} : rest -> go metaAcc rest

                  -- E. Erase exactly ONE unbound item element sequence loop and keep moving
                  _ : rest -> go metaAcc rest

    -- Helper layout guard to prevent key-value snatching across macro envelopes
    isStructuralExpr :: S.Expr -> Bool
    isStructuralExpr = \case
                        S.Attr _ _                          -> True
                        S.Form _ (S.Symbol _ "context" : _) -> True
                        S.Form _ (S.Symbol _ "define" : _)  -> True
                        _                                   -> False


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
evalExpr :: Env -> S.Expr -> Either String Value
evalExpr env expr =
    case expr of
        S.Literal _ (S.Str txt)  -> pure $ Primitive (I.String txt)
        S.Literal _ (S.Num val)  -> pure $ Primitive (I.Number val)
        S.Literal _ (S.Bool b)   -> pure $ Primitive (I.Boolean b)
        S.Literal _ S.Null       -> pure $ Primitive I.Null
        S.Literal _ S.EmptyArr   -> pure $ Primitive I.EmptyArr
        S.Literal _ S.EmptyObj   -> pure $ Primitive I.EmptyObj
        S.Tagged  _ tag payload  -> case tag of
                                         -- Explicit URL Type Assertion
                                         S.Uri  -> do
                                                   evaluatedVal <- evalExpr env payload
                                                   case evaluatedVal of
                                                       Primitive (I.String rawText) ->
                                                           case runParser (URI.parser :: Parsec Void Text URI.URI) "#uri validation" rawText of
                                                               Right uri -> pure $ Primitive (I.URI uri)
                                                               Left  err -> Left $ "Type Error: String failed to satisfy URI specification layout.\n"
                                                                                ++ errorBundlePretty err
                                                       _ -> Left "Type Error: The #uri tag modifier must target a string literal or a string variable."

                                         -- Explicit Date Type Assertion
                                         S.Date -> do
                                                   evaluatedVal <- evalExpr env payload
                                                   case evaluatedVal of
                                                       Primitive (I.String rawText) ->
                                                           case parseISO8601 (T.unpack rawText) of
                                                               Just utcTime -> pure $ Primitive (I.Date utcTime)
                                                               Nothing      -> Left "Type Error: Malformed ISO Date string layout. Format must be YYYY-MM-DD or YYYY-MM-DDTHH:MM:SSZ."
                                                       _ -> Left "Type Error: The #date tag modifier must target a string literal or a string variable."

                                         -- Explicit String Type Assertion
                                         S.StrTag -> do
                                                     evaluatedVal <- evalExpr env payload
                                                     case evaluatedVal of
                                                         Primitive (I.String _) -> pure evaluatedVal
                                                         _ -> Left "Type Error: Assertion failed. Expression did not resolve to a String."

                                         -- Explicit Numeric Type Assertion
                                         S.NumTag -> do
                                                     evaluatedVal <- evalExpr env payload
                                                     case evaluatedVal of
                                                         Primitive (I.Number _) -> pure evaluatedVal
                                                         _ -> Left "Type Error: Assertion failed. Expression did not resolve to a Number."

                                         -- Explicit Boolean Type Assertion
                                         S.BoolTag -> do
                                                      evaluatedVal <- evalExpr env payload
                                                      case evaluatedVal of
                                                          Primitive (I.Boolean _) -> pure evaluatedVal
                                                          _ -> Left "Type Error: Assertion failed. Expression did not resolve to a Boolean."

                                         _  -> Left "Compile Error: Unsupported semantic tag payload type."

        -- Variables (Now handles variable properties AND dynamic function resolution fallback)
        S.Symbol _ name -> lookupVar name env

        -- --- INTERCEPT SPECIAL FORMS ---
        S.Form _ [S.Symbol _ "context", S.Form _ directives] -> do
                                                                localSchema <- parseContextDirectives directives
                                                                pure $ Metadata (I.Context localSchema)

        S.Form _ (S.Symbol _ "define" : _) -> Left "Compile Error: Definition blocks are erasure forms and cannot be used as terminal values."

        S.Form _ (S.Symbol _ "get" : rootTarget : pathExpressions)
            -> do
               -- Convert trailing arguments into a clean lookup stack of Text tokens inline
               pathKeys <- mapM (\case
                                  S.Symbol _ k -> pure k
                                  S.Attr   _ _ -> Left "Syntax Error: The root target of a 'get' operation must be a Symbol, not an Attr."
                                  _            -> Left "Path Error: Arguments to 'get' must be valid symbols or attributes."
                                ) pathExpressions

               -- Navigate down through the un-evaluated syntax blocks inside the Env
               leafExpr <- resolvePath env rootTarget pathKeys

               -- Eagerly evaluate only the selected leaf target node
               evalExpr env leafExpr

        -- Idiomatic Lisp Scoping Form: (context (directives...) scopedFields...)
        -- Complex Form Sequences: Evaluates structural routing targets based on nested depth indicators
        S.Form _ allFields -> case determineBlockTarget allFields of
                                  TargetObject      -> compileScope env allFields
                                  TargetList        -> evalArray env allFields
                                  TargetFunctionApp -> case allFields of
                                                           (operatorExpr : argumentExprs)
                                                               -> do
                                                                  resolvedOp <- evalExpr env operatorExpr
                                                                  case resolvedOp of
                                                                      PrimitiveOp nativeFunc -> do
                                                                                                evaledArgs <- mapM (evalExpr env) argumentExprs
                                                                                                nativeFunc evaledArgs
                                                                      _ -> Left $ "Type Error: The head element of a Form sequence must resolve "
                                                                               ++ "to an executable function handle. Found un-callable value."
                                                           [] -> Left "Compile Error: Empty structural forms are invalid expression leaves."

        other -> Left $ "Compile Error: Unrecognized expression type inside record field value slot.\n"
                      ++ "Found AST Node Shape: " ++ show other

-- Data type representing the structural target resolved by lookahead routing.
data BlockTarget
    = TargetObject
    | TargetList
    | TargetFunctionApp

-- Inspects incoming form tokens to determine if they compose an Object or a List.
determineBlockTarget :: [S.Expr] -> BlockTarget
determineBlockTarget = \case
                        -- Rules for Object Detection
                        S.Attr {} : _                                   -> TargetObject
                        S.Form _ [S.Symbol _ "context", S.Form _ _] : _ -> TargetObject
                        S.Form _ (S.Symbol _ "define" : _) : _          -> TargetObject

                        -- Rules for List/Array Detection
                        S.Form _ [S.Form _ [S.Symbol _ "context", S.Form _ _]] : _ -> TargetList
                        S.Form _ [S.Form _ (S.Symbol _ "define" : _)] : _          -> TargetList
                        S.Form _ (S.Attr {} : _) : _                               -> TargetList

                        -- Fallback: If it's a standard list starting with a function/operator symbol
                        _ -> TargetFunctionApp


-- Compiles a collection of nested Lisp blocks into a uniform sequence array of Objects
evalArray :: Env -> [S.Expr] -> Either String Value
evalArray env elements = do
                         gadtList <- compileElements elements
                         pure $ Primitive (I.Array gadtList)

    where
    compileElements :: [S.Expr] -> Either String (I.Expr 'JLD.List)
    compileElements = \case
                      []                     -> pure I.Nil
                      -- A. TRUE ERASURE: Skip define blocks completely inside arrays
                      S.Form _ (S.Symbol _ "define" : _) : xs -> compileElements xs

                      -- B. TRUE ERASURE: Skip context blocks completely inside arrays
                      S.Form _ [S.Symbol _ "context", S.Form _ _] : xs -> compileElements xs

                      S.Form _ fields : xs -> do
                                              evaledItem <- compileScope env fields
                                              restL      <- compileElements xs
                                              case evaledItem of
                                                  -- Object record graph node
                                                  Primitive (I.Object props body) -> pure $ I.Cons (I.Object props body) restL
                                                  -- Standard scalar primitive (String, Number, Date, etc.)
                                                  Primitive standardPrim          -> pure $ I.Cons standardPrim restL
                                                  _ -> Left "Type Error: All items inside an object array block must be valid records."
                      otherExpr : xs -> do
                                        evaledVal <- evalExpr env otherExpr
                                        restL     <- compileElements xs
                                        case evaledVal of
                                            Primitive standardPrim -> pure $ I.Cons standardPrim restL
                                            _                      -> Left "Type Error: Arrays cannot process dynamic metadata tokens sequentially."


-- buildLazyEnv.
--
-- Performs a structural lookahead sweep across a block's immediate expression stream to capture declarations.
-- Extracts attribute pairs and macro definitions, binding raw surface syntax trees lazily into an environment map.
--
-- This function serves as the declarative foundation for the interpreter's order-independent variable bindings.
-- Instead of immediately evaluating expressions from left to right, it isolates and catalogs identifier mappings
-- as un-evaluated syntax tokens ('S.Expr'). This enables the compilation engine to resolve cyclical or forward-declared
-- properties gracefully, delaying actual evaluation until a value slot is explicitly queried via 'lookupVar':
--
--   * Local Block Expansion: Processes nested macro blocks like '(define (:key val ...))' by recursively unrolling
--     and harvesting their inner assignments, then merging them cleanly into the local scope layer.
--   * Keyword-Value Pairing: Identifies 'S.Attr' structural keys, capturing the trailing expression and binding it
--     directly into the accumulating dictionary while skipping across structural delimiters.
--   * Stream Sanitization & Compaction: Acts as a compile-time filter that strips away hanging keywords or loose
--     unbound elements, ensuring only valid symbol-to-expression associations persist inside the generated frame.
buildLazyEnv :: Env -> [S.Expr] -> Either String (Map.Map Text S.Expr)
buildLazyEnv = curry $ \case
                        (_,   []) -> pure Map.empty

                        -- A. Ensure BOTH layout variants of context forms are safely ignored by lazy scoping passes
                        (env, S.Form _ (S.Symbol _ "context" : _) : xs) ->
                            buildLazyEnv env xs

                        -- B. Standard block scope variable group processing
                        (env, S.Form _ (S.Symbol _ "define" : rest) : xs) -> do
                                                                             innerVars <- buildLazyEnv env rest
                                                                             outerVars <- buildLazyEnv env xs
                                                                             pure $ Map.union innerVars outerVars

                        -- C. FIXED: Standard attribute mapping accumulation pass (with structural layout check guards)
                        (env, S.Attr _ key : valExpr : rest)
                            | not (isStructuralExpr valExpr) -> do
                                                                next <- buildLazyEnv env rest
                                                                pure $ Map.insert key valExpr next

                        -- D. FIXED: Safely drop lone attribute tokens without eating sibling expressions
                        (env, S.Attr {} : rest) -> buildLazyEnv env rest

                        -- E. Erase exactly ONE unbound element and keep moving
                        (env, (_ : rest)) -> buildLazyEnv env rest

    where
    isStructuralExpr :: S.Expr -> Bool
    isStructuralExpr = \case
        S.Attr {} -> True
        S.Form _ (S.Symbol _ "context" : _) -> True
        S.Form _ (S.Symbol _ "define" : _)  -> True
        _                                   -> False


parseContextDirectives :: [S.Expr] -> Either String Schema
parseContextDirectives = \case
                         [] -> pure mempty
                         (S.Form _ innerExprs : rest) -> do
                                                         localSchema <- parseContextDirectives innerExprs
                                                         nextSchema  <- parseContextDirectives rest
                                                         pure (localSchema <> nextSchema)
                         (S.Symbol _ key : valExpr : rest) -> do
                                                              directive  <- buildDirective key valExpr
                                                              nextSchema <- parseContextDirectives rest
                                                              pure (Schema [directive] <> nextSchema)
                         _ -> Left "Syntax Error: Context configurations must consist of symbol-value pairs."


buildDirective :: Text -> S.Expr -> Either String SchemaDirective
buildDirective key val =
    case key of
        "vocab" -> case val of
                       S.Tagged _ S.Uri (S.Literal _ (S.Str t)) ->
                           case runParser (URI.parser :: Parsec Void Text URI.URI) "#uri validation" t of
                               Right u   -> pure $ SetVocab (Left u)
                               Left  err -> Left $ "Type Error: String failed to satisfy URI specification layout.\n"
                                                ++ errorBundlePretty err
                       S.Literal _ (S.Str t) -> pure $ SetVocab (Right t)
                       _ -> Left "Type Error: 'vocab' requires a #uri tag or raw String literal."

        "base" -> case val of
                      S.Tagged _ S.Uri (S.Literal _ (S.Str t)) -> pure $ SetBase t
                      S.Literal _ (S.Str t)                    -> pure $ SetBase t
                      _ -> Left "Type Error: 'base' requires a #uri tag or raw String literal."

        "language" -> case val of
                          S.Literal _ (S.Str t) -> pure $ SetLanguage t
                          _                     -> Left "Type Error: 'language' requires a String literal."

        "remote-context"
            -> case val of
                   -- Scenario A: Value is explicitly tagged via the #uri macro
                   S.Tagged _ S.Uri (S.Literal _ (S.Str t))
                       -> case runParser (URI.parser :: Parsec Void Text URI.URI) "#uri validation" t of
                              Right u  -> pure $ RemoteContext u
                              Left err -> Left $ "Type Error: Explicit #uri failed to satisfy specification layout:\n"
                                              ++ errorBundlePretty err

                   -- Scenario B: Value is a raw String literal (parse it inline as a RemoteContext URI)
                   S.Literal _ (S.Str t)
                       -> case runParser (URI.parser :: Parsec Void Text URI.URI) "inline string context validation" t of
                              Right u  -> pure $ RemoteContext u
                              Left err -> Left $ "Type Error: Inline context string failed to satisfy URI specification layout:\n"
                                              ++ errorBundlePretty err

                   _ -> Left "Type Error: 'context' requires a valid #uri tag or raw String reference layout."

        _ -> Left $ "Unknown context directive: " ++ T.unpack key


-- Resolves dynamic lookups via local maps, builtins fallbacks, or stepping up into parent scopes.
lookupVar :: Text -> Env -> Either String Value
lookupVar name env =
    case Map.lookup name (localScope env) of
        -- Evaluate lazy surface expression
        Just surfaceExpr -> evalExpr env surfaceExpr

        -- Field lookup else check if it matches a native function handle
        Nothing -> case builtinRegistry name of
                       Just nativeOp -> pure nativeOp
                       -- Look up to the nesting parent environment if not a built in function
                       Nothing       -> case parentEnv env of
                                            Just pEnv -> lookupVar name pEnv
                                            Nothing   -> Left $ "Scope Error: Unbound variable " ++ T.unpack name


-- | Traverses raw surface syntax blocks recursively using direct case statements.
resolvePath :: Env -> S.Expr -> [Text] -> Either String S.Expr
resolvePath _   currentExpr [] = pure currentExpr
resolvePath env currentExpr (targetKey : remainingKeys) =
    case currentExpr of
        -- Strategy 1: Resolve symbol pointers out of environment frames
        S.Symbol pos varName ->
            case Map.lookup varName (localScope env) of
                Just linkedExpr -> resolvePath env linkedExpr (targetKey : remainingKeys)
                Nothing         -> case parentEnv env of
                                     Just pEnv -> resolvePath pEnv currentExpr (targetKey : remainingKeys)
                                     Nothing   -> Left $ "Scope Error at " ++ show pos
                                                    ++ ": Bound block '" ++ T.unpack varName ++ "' not found."

        -- Strategy 2: Scan Form wrappers using our strict token matching rules
        S.Form pos elements ->
            case matchTokenStream elements of
                Just matchedValue -> resolvePath env matchedValue remainingKeys
                Nothing           -> Left $ "Compile Error at " ++ show pos
                                       ++ ": Key path segment '" ++ T.unpack targetKey ++ "' does not exist in block layout."

        -- Strategy 3: Scan Bracket wrappers identically
        S.Bracket pos elements ->
            case matchTokenStream elements of
                Just matchedValue -> resolvePath env matchedValue remainingKeys
                Nothing           -> Left $ "Compile Error at " ++ show pos
                                       ++ ": Key path segment '" ++ T.unpack targetKey ++ "' does not exist in block layout."

        -- Catch-all for premature scalar leaf nodes
        other -> Left $ "Cannot navigate path component '" ++ T.unpack targetKey
                     ++ "' into a scalar literal leaf node."

  where
    -- Direct token stream lookup case runner
    matchTokenStream stream =
        case stream of
            [] -> Nothing

            -- Match A: Target text string (from our get symbol) matches the defined Attr key
            (S.Attr _ k : valExpr : _) | k == targetKey -> Just valExpr

            -- Match B: Bypass the define scaffolding intact if stored completely in the env
            (S.Symbol _ "define" : S.Attr _ varName : S.Form _ innerBody : _) | varName == targetKey ->
                matchTokenStream innerBody

            -- Fallback: Step forward sequentially
            (_ : rest) -> matchTokenStream rest
