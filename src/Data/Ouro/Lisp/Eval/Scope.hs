{-# LANGUAGE GADTs #-}

module Data.Ouro.Lisp.Eval.Scope where

import           Control.Monad                (foldM)
import           Control.Monad.Reader         (asks, local)
import           Control.Monad.State.Strict   (gets, modify)
import qualified Data.IntMap.Strict           as IntMap
import           Data.Function                ((&))
import qualified Data.Map                     as Map
import           Data.Ouro.Error.Diagnostics  (astCorruption, cyclicDependency,
                                               lexicalError, shadowedVariable,
                                               shadowedVariableBlurb,
                                               unboundIdentifier, withBlurb, invalidFunctionName)
import           Data.Ouro.Error.Types        (OuroError (..))
import           Data.Ouro.Internal.Utils     (rankBySimilarity)
import           Data.Ouro.Lisp.Eval.Builtins (builtinRegistry)
import           Data.Ouro.Lisp.Eval.Types    (Binding (..), Env (..), EvalM,
                                               allEnvKeys)
import qualified Data.Ouro.Lisp.Eval.Types    as L
import qualified Data.Ouro.Lisp.Module.Types  as M
import qualified Data.Ouro.Lisp.Surface       as S
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Lens.Micro                   ((%~), (.~), (^.))
import           Lens.Micro.Platform          (at, (?~))
import           Text.Megaparsec              (SourcePos)


-- FrameEntry.
--
-- One binding written in a block: a ':key value' field, or a key inside a 'define'.
-- Each entry becomes its own promise, so a block with duplicate keys still outputs
-- every field, while lookups of the name see the first entry.
data FrameEntry = FrameEntry
    { entryKey     :: Text
    , entryPos     :: SourcePos
    , entryExpr    :: S.Expr
    , entryIsField :: Bool       -- A top-level field of the block (part of its output), not a define
    }


-- collectBindings.
--
-- Sweeps a block's expressions for the bindings it declares, in source order: its
-- ':key value' fields and the contents of any 'define' blocks (recursively). Context
-- forms, malformed attributes and loose expressions declare nothing.
collectBindings :: [S.Expr] -> Either OuroError [FrameEntry]
collectBindings = go True
    where
    go :: Bool -> [S.Expr] -> Either OuroError [FrameEntry]
    go isField = \case
        [] -> pure []

        S.Form _ (S.Symbol _ "context" : _) : xs
            -> go isField xs

        S.Form _ (S.Symbol _ "define" : inner) : xs
            -> (<>) <$> go False inner <*> go isField xs

        attrExpr : xs
            | Just (key, pos, valExpr) <- bindableAttr attrExpr
            -> case isReserved key of
                   True  -> shadowedVariable key
                            & withBlurb (shadowedVariableBlurb key)
                            & OuroError pos
                            & Left
                   False -> (FrameEntry key pos valExpr isField :) <$> go isField xs

        _unbound : xs
            -> go isField xs

    -- Reserved special forms
    isReserved :: Text -> Bool
    isReserved name = name `elem` ["nth", "list", "case", "get", "context", "define", "inlay", "insert", "attr", "defun", "return"]


-- A ':key value' pair that binds a name. Pairs whose value is itself a structural
-- form (a nested attribute, context, define, defun or return) bind nothing.
bindableAttr :: S.Expr -> Maybe (Text, SourcePos, S.Expr)
bindableAttr = \case
    S.Form pos [S.Symbol _ "attr", S.Literal _ (S.Str key), valExpr]
        | not (isStructuralExpr valExpr) -> Just (key, pos, valExpr)
    _notBindable                         -> Nothing

    where
    isStructuralExpr :: S.Expr -> Bool
    isStructuralExpr = \case
                        S.Form _ (S.Symbol _ "attr"     : _) -> True
                        S.Form _ (S.Symbol _ "context"  : _) -> True
                        S.Form _ (S.Symbol _ "define"   : _) -> True
                        S.Form _ (S.Symbol _ "defun"    : _) -> True
                        S.Form _ (S.Symbol _ "return"   : _) -> True
                        _otherForm                           -> False


-- allocFrame.
--
-- Allocates one unforced promise per entry and builds the frame that binds their
-- names. Every promise closes over that same frame, so the bindings can refer to
-- each other in any order (Scheme's letrec). Lookups see the first entry for each
-- name. Returns the frame and the entries' addresses, in entry order.
allocFrame :: (Map.Map Text Binding -> Env) -> [FrameEntry] -> EvalM (Env, [L.Addr])
allocFrame mkFrame entries =
    allocate mkFrame [ (entryKey e, entryPos e, entryExpr e, L.Unforced) | e <- entries ]


-- Binds already-evaluated values (from a case pattern) in a new frame
allocValues :: (Map.Map Text Binding -> Env) -> SourcePos -> [(Text, L.Expr)] -> EvalM Env
allocValues mkFrame pos bindings =
    fst <$> allocate mkFrame [ (name, pos, S.Hole pos ("?" <> name), L.Forced value) | (name, value) <- bindings ]


allocate :: (Map.Map Text Binding -> Env) -> [(Text, SourcePos, S.Expr, L.PromiseState)] -> EvalM (Env, [L.Addr])
allocate mkFrame entries = do
    base <- gets IntMap.size
    let addrs  = take (length entries) [base ..]
        names  = Map.fromListWith (\_later first -> first)
                     [ (name, Promised addr) | ((name, _, _, _), addr) <- zip entries addrs ]
        frame  = mkFrame names
    modify $ IntMap.union $ IntMap.fromList
        [ (addr, L.Promise frame expr name pos st) | ((name, pos, expr, st), addr) <- zip entries addrs ]
    pure (frame, addrs)


-- force.
--
-- Evaluates a promise at most once, in the environment it was written in, and caches
-- the result. Reaching a promise that is still being evaluated means the binding
-- depends on itself.
force :: (S.Expr -> EvalM L.Expr) -> SourcePos -> L.Addr -> EvalM L.Expr
force evaluator refPos addr = do
    promise <- gets (IntMap.! addr)
    case promise ^. L.promiseState of
        L.Forced value -> pure value
        L.Forcing      -> pure (cyclicLookup refPos (promise ^. L.promiseName))
        L.Unforced     -> do
                          setState L.Forcing
                          value <- local (const (promise ^. L.promiseEnv)) (evaluator (promise ^. L.promiseExpr))
                          setState (L.Forced value)
                          pure value
    where
    setState st = modify (IntMap.adjust (L.promiseState .~ st) addr)


cyclicLookup :: SourcePos -> Text -> L.Expr
cyclicLookup pos name =
    cyclicDependency name
    & withBlurb ( "An infinite lookup loop was detected. The identifier '" <> name
               <> "' directly or indirectly references itself during evaluation." )
    & OuroError pos
    & L.EvalError


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


-- lookupVar.
--
-- Resolves a name by walking outward through the frames, then function definitions,
-- then builtins. Promised bindings are forced in their own environment. Function
-- arguments are still evaluated where they are referenced (until step 4), so loops
-- through them are tracked by name in 'activeLookups'.
lookupVar
  :: (S.Expr -> EvalM L.Expr)
  -> SourcePos
  -> Text
  -> Env
  -> EvalM L.Expr
lookupVar evaluator pos name env =
    case Map.lookup name (env ^. L.localScope) of
        Just (Promised addr)
            -> force evaluator pos addr

        Just (Unshared argExpr)
            -> do
               active <- asks (^. L.activeLookups)
               case Set.member name active of
                   True  -> pure (cyclicLookup pos name)
                   False -> local (L.activeLookups %~ Set.insert name) (evaluator argExpr)

        Nothing
            -> lookupFunction evaluator pos name env


-- Resolves structural function blueprints at the CURRENT scope level, then steps up.
lookupFunction
    :: (S.Expr -> EvalM L.Expr)
    -> SourcePos
    -> Text
    -> Env
    -> EvalM L.Expr
lookupFunction evaluator pos name env =
    case Map.lookup name (env ^. L.functionRegistry) of
        Just (M.Function _ _ tArgs (S.Form _ bodyExprs))
            -> pure $ L.FunctionClosure env name tArgs bodyExprs

        Just _ -> astCorruption name "Corrupted function registry entry."
                  & OuroError pos
                  & L.EvalError
                  & pure

        -- SCHEME SCOPING If not in local functions, look into parent env
        Nothing -> case env ^. L.parentEnv of
                       Just pEnv -> lookupVar evaluator pos name pEnv
                       -- If no parent exists, we are at the root. Fall back to Builtins.
                       Nothing   -> lookupBuiltin pos name env


-- Field lookup else throw the final unbound error
lookupBuiltin
    :: SourcePos
    -> Text
    -> Env
    -> EvalM L.Expr
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
