
module Data.Ouro.Lisp.Eval.Scope where

import           Control.Monad.Reader         (Reader, local)
import           Data.Function                ((&))
import qualified Data.Map                     as Map
import           Data.Ouro.Error.Diagnostics  (cyclicDependency,
                                               unboundIdentifier, withBlurb, invalidTemplateName, astCorruption)
import           Data.Ouro.Error.Types        (OuroError (..))
import           Data.Ouro.Internal.Utils     (rankBySimilarity)
import           Data.Ouro.Lisp.Eval.Builtins (builtinRegistry, checkShadowing)
import           Data.Ouro.Lisp.Eval.Types    (Env (..), allEnvKeys)
import qualified Data.Ouro.Lisp.Eval.Types    as L
import qualified Data.Ouro.Lisp.Surface       as S
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import           Lens.Micro                   ((%~), (^.))
import           Text.Megaparsec              (SourcePos)
import qualified Data.Text as T


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
--   * Keyword-L.Expr Pairing: Identifies 'S.Attr' structural keys, capturing the trailing expression and binding it
--     directly into the accumulating dictionary while skipping across structural delimiters.
--   * Stream Sanitization & Compaction: Acts as a compile-time filter that strips away hanging keywords or loose
--     unbound elements, ensuring only valid symbol-to-expression associations persist inside the generated frame.
buildLazyEnv :: Env -> [S.Expr] -> Either OuroError (Map.Map Text S.Expr)
buildLazyEnv = curry $ \case
                        (_,   [])
                            -> pure Map.empty

                        -- Case A: Ensure BOTH layout variants of context forms are safely ignored by lazy scoping passes
                        (env, S.Form _ (S.Symbol _ "context" : _) : xs)
                            -> buildLazyEnv env xs

                        -- Case B: Standard block scope variable group processing
                        (env, S.Form _ (S.Symbol _ "define" : rest) : xs)
                            -> do
                               innerVars <- buildLazyEnv env rest
                               outerVars <- buildLazyEnv env xs
                               pure $ Map.union innerVars outerVars

                        -- Case C: Standard attribute mapping accumulation pass (with structural layout check guards)
                        (env, S.Attr pos key : valExpr : rest)
                            | not (isStructuralExpr valExpr)
                            -> do
                               checkShadowing pos key
                               next <- buildLazyEnv env rest
                               pure $ Map.insert key valExpr next

                        -- Case D: Safely drop lone attribute tokens without eating sibling expressions
                        (env, S.Attr {} : rest)
                            -> buildLazyEnv env rest

                        -- Case E: Erase exactly 1 unbound element and keep moving
                        (env, (_ : rest))
                            -> buildLazyEnv env rest

    where
    isStructuralExpr :: S.Expr -> Bool
    isStructuralExpr = \case
                        S.Attr {} -> True
                        S.Form _ (S.Symbol _ "context" : _)  -> True
                        S.Form _ (S.Symbol _ "define" : _)   -> True
                        S.Form _ (S.Symbol _ "template" : _) -> True
                        _otherForm                           -> False


buildTemplateRegistry :: Env -> [S.Expr] -> Either OuroError (Map.Map Text S.Expr)
buildTemplateRegistry env =
    \case
     [] -> pure Map.empty

     -- Case A: Intercept top-level template forms and index them by name
     rawAst@(S.Form _ (S.Symbol _ "template" : S.Symbol namePos name : _)) : xs
         -> do
            case "!" `T.isSuffixOf` name of
                True  -> Right ()
                False -> invalidTemplateName name
                         & OuroError namePos
                         & Left

            nextRegistry <- buildTemplateRegistry env xs
            pure $ Map.insert name rawAst nextRegistry

     -- Case B: Recurse into define blocks if they can contain local templates
     S.Form _ (S.Symbol _ "define" : rest) : xs
         -> do
            innerTemplates <- buildTemplateRegistry env rest
            outerTemplates <- buildTemplateRegistry env xs
            pure $ Map.union innerTemplates outerTemplates

     -- Case C: Safely ignore variables, contexts, and attributes
     _ : xs -> buildTemplateRegistry env xs

-- Resolves dynamic lookups via local maps, builtins fallbacks, or stepping up into parent scopes.
-- Now operates purely within the Reader monad to maintain consistency with the engine.
lookupVar
  :: (S.Expr -> Reader Env L.Expr)
  -> SourcePos
  -> Text
  -> Env
  -> Reader Env L.Expr
lookupVar evaluator pos name fullEnv =
    case Set.member name (fullEnv ^. L.activeLookups) of
        True  -> cyclicDependency name
                 & withBlurb ( "An infinite lookup loop was detected. The identifier '" <> name
                            <> "' directly or indirectly references itself during evaluation." )
                 & OuroError pos
                 & L.EvalError
                 & pure
        False -> lookupVar' (Just evaluator) pos name fullEnv

lookupVar'
    :: Maybe (S.Expr -> Reader Env L.Expr)
    -> SourcePos
    -> Text
    -> Env
    -> Reader Env L.Expr
lookupVar' mEvaluator pos name env =
    case Map.lookup name (env ^. L.localScope) of
        Just surfaceExpr -> case mEvaluator of
                                -- If we have an evaluator, run it with active cycle detection
                                Just evaluator -> local (L.activeLookups %~ Set.insert name) (evaluator surfaceExpr)
                                -- Safe fallback, though lookupVar' is always called with 'Just'
                                Nothing        -> pure $ L.Quote surfaceExpr
        Nothing          -> lookupBuiltin lookupVar' mEvaluator pos name env


-- Quote lookup the same as lookupVar but returns a thunk
quoteVar
  :: SourcePos
  -> Text
  -> Env
  -> Reader Env L.Expr
quoteVar pos name fullEnv =
    case Set.member name (fullEnv ^. L.activeLookups) of
        True  -> cyclicDependency name
                 & withBlurb ( "An infinite lookup loop was detected. The identifier '" <> name
                            <> "' directly or indirectly references itself during evaluation." )
                 & OuroError pos
                 & L.EvalError
                 & pure
        False -> quoteVar' Nothing pos name fullEnv

quoteVar'
    :: Maybe (S.Expr -> Reader Env L.Expr)
    -> SourcePos
    -> Text
    -> Env
    -> Reader Env L.Expr
quoteVar' mEvaluator pos name env =
    case Map.lookup name (env ^. L.localScope) of
        -- Directly wrap the surface AST, intentionally ignoring mEvaluator
        Just surfaceAst -> pure $ L.Quote surfaceAst
        -- Pass the walker and the polymorphic evaluator down the chain
        Nothing         -> lookupBuiltin quoteVar' mEvaluator pos name env


-- Field lookup else check if it matches a native function handle
lookupBuiltin
    :: (Maybe (S.Expr -> Reader Env L.Expr) -> SourcePos -> Text -> Env -> Reader Env L.Expr)
    -> Maybe (S.Expr -> Reader Env L.Expr)
    -> SourcePos
    -> Text
    -> Env
    -> Reader Env L.Expr
lookupBuiltin scopeWalker mEvaluator pos name env =
    case L.PrimitiveOp <$> Map.lookup name builtinRegistry of
        Just nativeOp -> pure nativeOp
        Nothing       -> lookupTemplate scopeWalker mEvaluator pos name env

-- Resolves structural template blueprints.
-- If found, wraps the AST in a first-class closure waiting for actual arguments.
-- Otherwise, delegates up the chain to lookupField.
lookupTemplate
    :: (Maybe (S.Expr -> Reader Env L.Expr) -> SourcePos -> Text -> Env -> Reader Env L.Expr)
    -> Maybe (S.Expr -> Reader Env L.Expr)
    -> SourcePos
    -> Text
    -> Env
    -> Reader Env L.Expr
lookupTemplate scopeWalker mEvaluator pos name env =
    case Map.lookup name (env ^. L.templateRegistry) of
        Just rawAst@(S.Form _ (S.Symbol _ "template" : S.Symbol _ _ : S.Form _ argNodes : bodyExprs))
            -> case mEvaluator of
                   -- Standard path: Return a pure data closure carrying the lexical state
                   Just _ -> do
                             let params = map (\case S.Symbol _ p -> p; _ -> "") argNodes
                             pure $ L.TemplateClosure env name params bodyExprs

                   -- Quote path: If called via quoteVar, return the raw template AST
                   Nothing -> pure $ L.Quote rawAst

        -- Registry corruption check
        Just _ -> astCorruption name "Corrupted template registry entry."
                  & OuroError pos
                  & L.EvalError
                  & pure

        -- Not found in local templates, continue the chain
        Nothing -> lookupField scopeWalker mEvaluator pos name env


-- Look up to the nesting parent environment if not a built in function
lookupField
    :: (Maybe (S.Expr -> Reader Env L.Expr) -> SourcePos -> Text -> Env -> Reader Env L.Expr)
    -> Maybe (S.Expr -> Reader Env L.Expr)
    -> SourcePos
    -> Text
    -> Env
    -> Reader Env L.Expr
lookupField scopeWalker mEvaluator pos name env =
    case env ^. L.parentEnv of
        Just pEnv -> scopeWalker mEvaluator pos name pEnv
        Nothing   -> let keys       = Set.toList $ allEnvKeys env
                         suggestion = case rankBySimilarity name keys of
                                        ((bestMatch, score) : _) | score <= 3
                                            -> "\n\nPerhaps you meant: '" <> bestMatch <> "'?"
                                        _   -> ""
                     in unboundIdentifier name
                        & withBlurb
                            ( "The evaluator attempted to lookup the value for '" <> name <> "', "
                            <> "but the identifier failed to resolve within any active scope chain."
                            <> suggestion
                            )
                        & OuroError pos
                        & L.EvalError
                        & pure
