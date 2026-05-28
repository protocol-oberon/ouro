
module Data.Ouro.Lisp.Eval.Scope where

import           Data.Function                ((&))
import qualified Data.Map                     as Map
import           Data.Ouro.Error.Diagnostics  (unboundIdentifier, withBlurb)
import           Data.Ouro.Error.Types        (OuroError (..))
import           Data.Ouro.Internal.Utils     (rankBySimilarity)
import           Data.Ouro.Lisp.Eval.Builtins (builtinRegistry)
import           Data.Ouro.Lisp.Eval.Types    (Env (..), Value, allEnvKeys)
import qualified Data.Ouro.Lisp.Surface       as S
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
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
--   * Keyword-Value Pairing: Identifies 'S.Attr' structural keys, capturing the trailing expression and binding it
--     directly into the accumulating dictionary while skipping across structural delimiters.
--   * Stream Sanitization & Compaction: Acts as a compile-time filter that strips away hanging keywords or loose
--     unbound elements, ensuring only valid symbol-to-expression associations persist inside the generated frame.
buildLazyEnv :: Env -> [S.Expr] -> Either OuroError (Map.Map Text S.Expr)
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


-- Resolves dynamic lookups via local maps, builtins fallbacks, or stepping up into parent scopes.
-- Takes an explicit runner function `(Env -> S.Expr -> Either OuroError Value)`
-- to decouple scoping lookup loops from the evaluation engine internals.
lookupVar
  :: (Env -> S.Expr -> Either OuroError Value)
  -> SourcePos
  -> Text
  -> Env
  -> Either OuroError Value
lookupVar evaluator pos name fullEnv = go fullEnv
  where
    go env = case Map.lookup name (localScope env) of
        -- Evaluate lazy surface expression
        Just surfaceExpr -> evaluator fullEnv surfaceExpr

        -- Field lookup else check if it matches a native function handle
        Nothing -> case builtinRegistry name of
                       Just nativeOp -> pure nativeOp
                       -- Look up to the nesting parent environment if not a built in function
                       Nothing       -> case parentEnv env of
                                            Just pEnv -> go pEnv
                                            Nothing   ->
                                                let keys = Set.toList $ allEnvKeys fullEnv
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
                                                      & Left
