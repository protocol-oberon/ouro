{-# LANGUAGE GADTs #-}

module Data.Ouro.Lisp.Eval.Scope where

import           Control.Monad                (when)
import           Control.Monad.Reader         (local)
import           Control.Monad.State.Strict   (gets, modify)
import qualified Data.IntMap.Strict           as IntMap
import           Data.Function                ((&))
import qualified Data.Map                     as Map
import           Data.Ouro.Error.Diagnostics  (astCorruption, cyclicDependency,
                                               lexicalError, shadowedVariable,
                                               shadowedVariableBlurb,
                                               unboundIdentifier, withBlurb)
import           Data.Ouro.Error.Types        (OuroError (..))
import           Data.Ouro.Internal.Utils     (rankBySimilarity)
import           Data.Ouro.Lisp.Eval.Builtins (builtinRegistry)
import           Data.Ouro.Lisp.Eval.Types    (Binding (..), Env (..), EvalM,
                                               allEnvKeys)
import qualified Data.Ouro.Lisp.Eval.Types    as L
import           Data.Ouro.Lisp.Function      (bindingWithoutValue,
                                               isFunctionKeyword, keyInLet,
                                               misplacedBinding, removedDefine,
                                               removedLocalDefn)
import qualified Data.Ouro.Lisp.Module.Types  as M
import qualified Data.Ouro.Lisp.Surface       as S
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import           Lens.Micro                   ((%~), (.~), (^.))
import           Text.Megaparsec              (SourcePos)


-- FrameEntry.
--
-- One binding: a Record's ':key value' attribute pair, a let binding, or a function argument.
-- Each entry becomes its own promise, so a Record with duplicate keys still outputs
-- every attribute pair, while lookups of the name see the first entry.
data FrameEntry = FrameEntry
    { entryKey     :: Text
    , entryPos     :: SourcePos
    , entryExpr    :: S.Expr
    , entryIsAttr  :: Bool       -- A Record attribute pair (part of its output), not a let binding or argument
    }


-- collectBindings.
--
-- Sweeps a Record's expressions for the attribute pairs it binds, in source order. Context
-- forms, malformed attributes and loose expressions bind nothing. 'define' and a local
-- 'defn' were replaced by 'let' and 'fn', and a private '$name value' binding belongs
-- in a let; all are reported where they appear.
collectBindings :: [S.Expr] -> Either OuroError [FrameEntry]
collectBindings = \case
    [] -> pure []

    S.Form _ (S.Symbol _ "context" : _) : xs
        -> collectBindings xs

    S.Form pos (S.Symbol _ "define" : _) : _
        -> Left (removedDefine pos)

    S.Form pos (S.Symbol _ keyword : _) : _ | isFunctionKeyword keyword
        -> Left (removedLocalDefn pos)

    S.Form pos [S.Symbol _ "%binder", S.Literal _ (S.Str name)] : _
        -> Left (misplacedBinding name pos)

    attrExpr : xs
        | Just (key, pos, valExpr) <- bindableAttr attrExpr
        -> do
           checkBindableName key pos
           (FrameEntry key pos valExpr True :) <$> collectBindings xs

    _unbound : xs
        -> collectBindings xs


-- letBindings.
--
-- The bindings of (let ($name value ...) expr): only '$name value' pairs. They are
-- not part of any output, and can refer to each other in any order.
letBindings :: [S.Expr] -> Either OuroError [FrameEntry]
letBindings = traverse $ \case
    S.Form pos [S.Symbol _ "%bind", S.Literal _ (S.Str name), valExpr]
        -> FrameEntry name pos valExpr False <$ checkBindableName name pos

    S.Form pos [S.Symbol _ "%bind", S.Literal _ (S.Str name)]
        -> Left (bindingWithoutValue name pos)

    S.Form pos (S.Symbol _ "%attr" : S.Literal _ (S.Str key) : _)
        -> Left (keyInLet key pos)

    binding
        -> lexicalError "A let binds private values with '$name value' pairs."
           & withBlurb "Perhaps write (let ($name value ...) expr)."
           & OuroError (S.exprPos binding)
           & Left


-- Special forms are recognised by their head symbol, so they cannot be bound as names
checkBindableName :: Text -> SourcePos -> Either OuroError ()
checkBindableName key pos =
    when (key `elem` reservedNames) $
        shadowedVariable key
        & withBlurb (shadowedVariableBlurb key)
        & OuroError pos
        & Left

reservedNames :: [Text]
reservedNames = ["nth", "list", "case", "get", "context", "define", "inlay", "insert", "attr", "defn", "defun", "let", "fn"]


-- A ':key value' attribute pair that binds a name. Pairs whose value is itself a structural
-- form (a nested attribute or context) bind nothing.
bindableAttr :: S.Expr -> Maybe (Text, SourcePos, S.Expr)
bindableAttr = \case
    S.Form pos [S.Symbol _ "%attr", S.Literal _ (S.Str key), valExpr]
        | not (isStructuralExpr valExpr) -> Just (key, pos, valExpr)
    _notBindable                         -> Nothing

    where
    isStructuralExpr :: S.Expr -> Bool
    isStructuralExpr = \case
                        S.Form _ (S.Symbol _ "%attr"    : _) -> True
                        S.Form _ (S.Symbol _ "context"  : _) -> True
                        _otherForm                           -> False


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
        -- The closure is this frame: the one the function is defined in
        Just (M.Function tPos _ tArgs (S.Form _ bodyExprs))
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
