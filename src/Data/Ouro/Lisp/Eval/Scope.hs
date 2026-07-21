{-# LANGUAGE GADTs #-}

module Data.Ouro.Lisp.Eval.Scope where

import           Control.Monad                (foldM)
import           Control.Monad.Reader         (Reader, local)
import           Data.Function                ((&))
import qualified Data.Map                     as Map
import           Data.Ouro.Error.Diagnostics  (astCorruption, cyclicDependency,
                                               lexicalError, shadowedVariable,
                                               shadowedVariableBlurb,
                                               unboundIdentifier, withBlurb, invalidFunctionName)
import           Data.Ouro.Error.Types        (OuroError (..))
import           Data.Ouro.Internal.Utils     (rankBySimilarity)
import           Data.Ouro.Lisp.Eval.Builtins (builtinRegistry)
import           Data.Ouro.Lisp.Eval.Types    (Env (..), allEnvKeys)
import qualified Data.Ouro.Lisp.Eval.Types    as L
import qualified Data.Ouro.Lisp.Module.Types  as M
import qualified Data.Ouro.Lisp.Surface       as S
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Lens.Micro                   ((%~), (^.))
import           Lens.Micro.Platform          (at, (?~))
import           Text.Megaparsec              (SourcePos)


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

                        -- Case C: Standard attribute mapping accumulation pass using the new parsed form
                        (env, S.Form pos [S.Symbol _ "attr", S.Literal _ (S.Str key), valExpr] : rest)
                            | not (isStructuralExpr valExpr)
                            -> case isReserved key of
                                 True  -> shadowedVariable key
                                          & withBlurb (shadowedVariableBlurb key)
                                          & OuroError pos
                                          & Left

                                 False -> do
                                          next <- buildLazyEnv env rest
                                          pure $ Map.insert key valExpr next

                        -- Case D: Safely drop malformed or lone attribute forms without eating sibling expressions
                        -- (Catches cases like `(attr "key")` with missing or extra arguments)
                        (env, S.Form _ (S.Symbol _ "attr" : _) : rest)
                            -> buildLazyEnv env rest

                        -- Case E: Erase exactly 1 unbound element and keep moving
                        (env, (_ : rest))
                            -> buildLazyEnv env rest

    where
    isStructuralExpr :: S.Expr -> Bool
    isStructuralExpr = \case
                        S.Form _ (S.Symbol _ "attr"     : _) -> True
                        S.Form _ (S.Symbol _ "context"  : _) -> True
                        S.Form _ (S.Symbol _ "define"   : _) -> True
                        S.Form _ (S.Symbol _ "return"   : _) -> True
                        _otherForm                           -> False

    -- Reserved special forms
    isReserved :: Text -> Bool
    isReserved name = name `elem` ["nth", "list", "quote", "eval", "case", "get", "get'", "context", "define", "inlay", "insert", "attr", "defun", "return"]


-- Scans a list of surface expressions for nested functions, lifting them into
-- HigherExpressions and binding them into the local Environment.
buildLocalFunction :: Env -> [S.Expr] -> Either OuroError Env
buildLocalFunction = foldM processNode
    where
    processNode :: Env -> S.Expr -> Either OuroError Env
    processNode env = \case
        -- Intercept top-level function forms and lift them
        S.Form pos (S.Symbol _ "defun" : S.Symbol namePos name : S.Form _ argNodes : bodyExprs)
            -> do
               case "!" `T.isSuffixOf` name of
                   True  -> Right ()
                   False -> invalidFunctionName name
                            & OuroError namePos
                            & Left

               args <- traverse (extractArg pos) argNodes

               let liftedFunction = M.Function pos name args (S.Form pos bodyExprs)

               env & L.functionRegistry . at name ?~ liftedFunction
                   & Right

        -- Catch malformed function definitions
        S.Form pos (S.Symbol _ "defun" : _)
            -> lexicalError "Malformed function declaration. Expected format: (defun name! (args...) body...)"
               & OuroError pos
               & Left

        -- Recurse into define blocks
        S.Form _ (S.Symbol _ "define" : rest)
            -> foldM processNode env rest

        -- Safely ignore variables, contexts, and attributes
        _other
            -> Right env


    -- Helper to safely extract Text from an argument node
    extractArg :: SourcePos -> S.Expr -> Either OuroError Text
    extractArg fallbackPos =
        \case
         S.Symbol _ argName -> Right argName
         _notASymbol        -> lexicalError "Function arguments must be bare identifiers."
                               & OuroError fallbackPos
                               & Left


-- Resolves dynamic lookups via local maps, builtins fallbacks, or stepping up into parent scopes.
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
                                Just evaluator -> local (L.activeLookups %~ Set.insert name) (evaluator surfaceExpr)
                                Nothing        -> pure $ L.Quote surfaceExpr
        -- Pass lookupVar' as the walker to check functions before moving to the parent
        Nothing -> lookupFunction lookupVar' mEvaluator pos name env


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
        Just surfaceAst -> pure $ L.Quote surfaceAst
        -- Pass quoteVar' as the walker to preserve the thunking state up the chain
        Nothing         -> lookupFunction quoteVar' mEvaluator pos name env


-- Resolves structural function blueprints at the CURRENT scope level, then steps up.
lookupFunction
    :: (Maybe (S.Expr -> Reader Env L.Expr) -> SourcePos -> Text -> Env -> Reader Env L.Expr)
    -> Maybe (S.Expr -> Reader Env L.Expr)
    -> SourcePos
    -> Text
    -> Env
    -> Reader Env L.Expr
lookupFunction scopeWalker mEvaluator pos name env =
    case Map.lookup name (env ^. L.functionRegistry) of
        Just (M.Function tPos tName tArgs (S.Form _ bodyExprs))
            -> let argNodes = map (S.Symbol tPos) tArgs -- Reconstruct the argument bindings
                   rawAst   = S.Form tPos $             -- Reassemble the raw AST into (defun name (args...) body...)
                            [ S.Symbol tPos "defun"
                            , S.Symbol tPos tName
                            , S.Form tPos argNodes
                            ] ++ bodyExprs

               in case mEvaluator of
                     Just _eval -> pure $ L.FunctionClosure env name tArgs bodyExprs
                     Nothing    -> pure $ L.Quote rawAst

        Just _ -> astCorruption name "Corrupted function registry entry."
                  & OuroError pos
                  & L.EvalError
                  & pure

        -- SCHEME SCOPING If not in local functions, look into parent env
        Nothing -> case env ^. L.parentEnv of
                       Just pEnv -> scopeWalker mEvaluator pos name pEnv
                       -- If no parent exists, we are at the root. Fall back to Builtins.
                       Nothing   -> lookupBuiltin pos name env


-- Field lookup else throw the final unbound error
lookupBuiltin
    :: SourcePos
    -> Text
    -> Env
    -> Reader Env L.Expr
lookupBuiltin pos name env =
    case L.PrimitiveOp <$> Map.lookup name builtinRegistry of
        Just nativeOp -> pure nativeOp
        Nothing       -> let keys       = Set.toList $ allEnvKeys env
                             suggestion = case rankBySimilarity name keys of
                                 ((bestMatch, score) : _) | score <=3
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
