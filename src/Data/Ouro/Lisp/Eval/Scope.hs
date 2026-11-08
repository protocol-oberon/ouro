{-# LANGUAGE GADTs #-}

module Data.Ouro.Lisp.Eval.Scope where

import           Control.Monad.Reader         (local)
import           Control.Monad.State.Strict   (gets, modify)
import qualified Data.IntMap.Strict           as IntMap
import           Data.Function                ((&))
import qualified Data.Map                     as Map
import           Data.Ouro.Error.Diagnostics  (astCorruption, cyclicDependency,
                                               notExported, unboundName,
                                               unknownQualifier, withBlurb)
import           Data.Ouro.Error.Types        (OuroError (..))
import           Data.Ouro.Lisp.Analysis.Forms (FrameEntry (..))
import           Data.Ouro.Lisp.Eval.Builtins (builtinRegistry)
import           Data.Ouro.Lisp.Eval.Types    (Binding (..), Env (..), EvalM,
                                               allEnvKeys)
import qualified Data.Ouro.Lisp.Eval.Types    as L
import           Data.Ouro.Lisp.Module.Bundled (bundled)
import qualified Data.Ouro.Lisp.Module.Types  as M
import qualified Data.Ouro.Lisp.Surface       as S
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import           Lens.Micro                   ((%~), (.~), (^.))
import           Text.Megaparsec              (SourcePos)


-- allocFrame.
--
-- Allocates one unforced promise per entry and builds the frame that binds their
-- names. Every promise closes over that same frame, so the bindings can refer to
-- each other in any order (Scheme's letrec), but a binding never sees itself: inside
-- its own expression, its name refers to the enclosing scope, so (:id id) outputs the
-- 'id' around the record. Lookups see the first entry for each name. Returns the frame
-- and the entries' addresses, in entry order.
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

        -- A binding's own expression is evaluated without its name in scope. Only a
        -- name this frame binds to this very promise is hidden: an argument's promise
        -- closes over the caller's frame, where the same name is a different binding.
        ownEnv name addr =
            case Map.lookup name (frame ^. L.localScope) of
                Just (Promised bound) | bound == addr -> frame & L.localScope %~ Map.delete name
                _otherBinding                         -> frame

    modify $ IntMap.union $ IntMap.fromList
        [ (addr, L.Promise (ownEnv name addr) expr name pos st) | ((name, pos, expr, st), addr) <- zip entries addrs ]
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


-- lookupVar.
--
-- Resolves a name by walking outward through the frames, then function definitions,
-- then builtins. Every binding is a promise, forced in the environment it was written in.
-- A qualified name, base/core, is an imported function, and skips the frames: no binding
-- can have a '/'.
lookupVar
  :: (S.Expr -> EvalM L.Expr)
  -> SourcePos
  -> Text
  -> Env
  -> EvalM L.Expr
lookupVar evaluator pos name env
    | Just (qualifier, unqualified) <- M.splitQualified name
        = lookupQualified evaluator pos qualifier unqualified env
    | otherwise
        = case Map.lookup name (env ^. L.localScope) of
              Just (Promised addr)
                  -> force evaluator pos addr

              Nothing
                  -> lookupFunction evaluator pos name env


-- An imported function, qualifier/name. The qualifier names an import of the module being
-- evaluated, and the name one of that module's exports, which is found in the module's own
-- root frame, so that the function closes over its own module, never the importer.
lookupQualified
    :: (S.Expr -> EvalM L.Expr)
    -> SourcePos
    -> Text
    -> Text
    -> Env
    -> EvalM L.Expr
lookupQualified evaluator pos qualifier name env =
    case Map.lookup qualifier (env ^. L.moduleImports) of
        Nothing
            -> unknownQualifier qualifier (Map.keys (env ^. L.moduleImports)) (Map.keys bundled)
               & OuroError pos
               & L.EvalError
               & pure

        Just imported
            | name `Set.member` L.importedExports imported
                -> lookupFunction evaluator pos name (L.importedRoot imported)
            | otherwise
                -> notExported qualifier name (Set.toList (L.importedExports imported))
                   & OuroError pos
                   & L.EvalError
                   & pure


-- Resolves structural function blueprints at the CURRENT scope level, then steps up.
lookupFunction
    :: (S.Expr -> EvalM L.Expr)
    -> SourcePos
    -> Text
    -> Env
    -> EvalM L.Expr
lookupFunction evaluator pos name env =
    case Map.lookup name (env ^. L.functionRegistry) of
        -- The closure is this frame: the one the function is defined in
        Just (M.Function tPos _ _ tArgs (S.Form _ bodyExprs))
            -> pure $ L.FunctionClosure env (name, tPos) tArgs bodyExprs

        Just _ -> astCorruption name "Corrupted function registry entry."
                  & OuroError pos
                  & L.EvalError
                  & pure

        -- SCHEME SCOPING If not in local functions, look into parent env
        Nothing -> case env ^. L.parentEnv of
                       Just pEnv -> lookupVar evaluator pos name pEnv
                       -- If no parent exists, we are at the root. Fall back to Builtins.
                       Nothing   -> lookupBuiltin pos name env


-- Attribute lookup else throw the final unbound error
lookupBuiltin
    :: SourcePos
    -> Text
    -> Env
    -> EvalM L.Expr
lookupBuiltin pos name env =
    case L.PrimitiveOp <$> Map.lookup name builtinRegistry of
        Just nativeOp -> pure nativeOp
        Nothing       -> unboundName name (Set.toList (allEnvKeys env))
                         & OuroError pos
                         & L.EvalError
                         & pure
