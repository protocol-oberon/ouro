{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Ouro.Lisp.Eval.Types where

import           Control.Monad.Reader        (Reader, ReaderT, runReaderT)
import           Control.Monad.State.Strict  (State, evalState)
import           Data.IntMap.Strict          (IntMap)
import qualified Data.IntMap.Strict          as IntMap
import           Data.List                   (sortOn)
import qualified Data.Map.Strict             as Map
import           Data.Ouro.Error.Types       (OuroError)
import qualified Data.Ouro.Internal.Expr     as I
import qualified Data.Ouro.Internal.Kinds    as JLD
import qualified Data.Ouro.Lisp.Module.Types as M
import qualified Data.Ouro.Lisp.Surface      as S
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Lens.Micro                  ((^.))
import           Lens.Micro.TH               (makeLenses)
import           Text.Megaparsec             (SourcePos)
import           Unsafe.Coerce               (unsafeCoerce)

-- Env.
--
-- The runtime execution environment backbone tracking variable visibility and lexical boundaries.
-- Structurally engineered as an immutable, singly-linked stack frame ring to enforce strict lexical scoping.
--
-- The Env record facilitates nested, lexically isolated visibility bubbles across the interpreter. By combining
-- a strictly evaluated local symbol table with a structural link to an optional outer layer, it establishes
-- a scope chain that matches the physical layout of source blocks. Variable resolution uses an upside-down
-- traversal strategy: the engine searches the immediate 'localScope' layer first, and recursively steps up into
-- the 'parentEnv' pointer frame until it either hits a binding or touches the root layer fallback.
--
-- Crucially, this structural composition enables both knot-tied lazy evaluation and first-class closures.
-- When a record block is evaluated, its properties are bound into a fresh Env frame whose parent pointer closes over
-- the surrounding context. Because these frames are captured by value inside closures, sub-graphs can pass functions
-- and variables down-stream without leaking local data or accidentally mutating outer execution registers.
--
-- Default state boundaries are controlled explicitly:
--   * defaultEnv instantiates a clean, terminal root scope layer containing zero localized bindings and no
--     parent fallback link, anchoring the absolute bottom of the variable resolution ladder.
data Env = Env
    { _localScope       :: Map.Map Text Binding
    , _parentEnv        :: Maybe Env
    , _functionRegistry :: FunctionRegistry   -- Functions defined in this frame only
    , _callStack        :: Set FunctionId   -- The functions currently being called
    }


-- Default root environment.
defaultEnv :: M.Module -> Env
defaultEnv moduleEnv = Env
    { _localScope       = Map.empty
    , _parentEnv        = Nothing
    , _functionRegistry = (moduleEnv ^. M.functionRegistry)
    , _callStack        = Set.empty
    }


emptyEnv :: Env
emptyEnv = Env
    { _localScope       = Map.empty
    , _parentEnv        = Nothing
    , _functionRegistry = Map.empty
    , _callStack        = Set.empty
    }


-- Expr.
--
-- A resilient evaluation superset utilized across all Ouro compiler passes.
-- Acts as the primary execution surface for the interpreter, holding valid data
-- nodes, structural scaffolding, and isolated error leaves simultaneously.
--
-- Unlike traditional compilers that terminate on the first semantic mismatch,
-- Ouro's Expr superset allows for "partial evaluation." By wrapping every structural
-- branch and primitive leaf in a unified type space, the engine can continue
-- evaluating sibling nodes even when specific branches have collapsed into EvalError.
--
-- Architectural Role:
--   * Resilient Scaffolding: Record and Array represent the "open" structural
--     layout of the graph. These structures hold recursive 'Expr' branches,
--     maintaining the tree topology even if child nodes are invalid.
--   * Fault Isolation: The 'EvalError' constructor serves as a universal terminal
--     node that allows the harvester to sweep through a partially-failed tree and
--     aggregate all diagnostic issues in a single pass.
--   * Domain Modeling: Retains explicit handles for functional abstractions (Closure,
--     PrimitiveOp) and intermediate domain logic (Duration) that have
--     not yet been collapsed into the final, frozen JSON-LD GADT ('I.Expr').
--
-- Terminal Resolution:
--   The 'Primitive' constructor serves as the final transition anchor. Once
--   the evaluation loop completes and all errors are harvested, these nodes
--   verify that the evaluated 'Expr' tree satisfies the strict Internal GADT
--   constraints required for final serialization.
data Expr where
    -- Pristine Frozen Targets (The pure GADTs)
    Primitive   :: I.Expr 'JLD.Primitive -> Expr

    -- Resilient Compilation Scaffolding (The Superset Nodes)
    -- These maintain the open tree structure during evaluation, allowing
    -- errors to be embedded at any depth.
    Record      :: I.Expr 'JLD.Meta -> [(Text, Expr)] -> Expr
    Attr        :: Text             -> Expr           -> Expr
    Array       :: [Expr]                             -> Expr

    -- Dedicated Evaluation Leaves
    Duration    :: PeriodUnit -> Int -> Expr

    PrimitiveOp :: NativeFunction -> Expr

    -- A function, closed over the frame it is defined in
    FunctionClosure :: Env -> FunctionId -> [S.Param] -> [S.Expr] -> Expr

    -- The Universal Error Leaf: Allows the engine to bypass crashes
    -- and continue evaluating sibling nodes.
    EvalError   :: OuroError -> Expr

-- Builtins only read the environment, so they never need the promise store
type NativeFunction = SourcePos -> [Expr] -> Reader Env Expr


-- EvalM.
--
-- The evaluator monad: the lexical environment is read, and the promise store is
-- threaded through evaluation. Each binding becomes a promise in the store (from
-- step 3 of the scoping redesign), so it is evaluated once, in the environment it
-- was written in, and re-entering a promise that is still being forced is a cycle.
type EvalM = ReaderT Env (State Store)

-- Promises allocated during evaluation, keyed by address. Addresses are never
-- reused, so the next free address is always the size of the store.
type Store = IntMap Promise

data Promise = Promise
    { _promiseEnv   :: Env          -- The environment the binding was written in
    , _promiseExpr  :: S.Expr       -- The binding's code
    , _promiseName  :: Text         -- For cycle and error messages
    , _promisePos   :: SourcePos
    , _promiseState :: PromiseState
    }

data PromiseState
    = Unforced
    | Forcing                       -- Being evaluated: reaching it again is a cycle
    | Forced Expr

-- What a name in a frame is bound to: a promise for a record attribute pair, let
-- binding, function argument or pattern variable
newtype Binding = Promised Addr

type Addr = Int

-- A function's identity: its name and where it is defined. Every anonymous fn is named
-- 'fn', so the recursion check compares both.
type FunctionId = (Text, SourcePos)

type FunctionRegistry = Map.Map Text (M.HigherExpression 'M.FunctionExpr)


runEvalM :: Env -> EvalM a -> a
runEvalM env m = evalState (runReaderT m env) IntMap.empty


-- The errors embedded anywhere in a value. Only error leaves count: a binding that
-- holds a Duration or a function is fine, as long as it never reaches the output.
errorsIn :: Expr -> [OuroError]
errorsIn = \case
    EvalError err -> [err]
    Record _ kvs  -> concatMap (errorsIn . snd) kvs
    Array  elems  -> concatMap errorsIn elems
    Attr   _ v    -> errorsIn v
    _noErrors     -> []


instance Eq Expr where
    -- Frozen Targets
    Primitive a == Primitive b = a == b

    -- Compilation Scaffolding
    Record a1 b1 == Record a2 b2 = a1 == a2 && sortOn fst b1 == sortOn fst b2
    Attr   k1 v1 == Attr   k2 v2 = k1 == k2 && v1 == v2
    Array  a     == Array  b     = a == b

    -- Eval only leaves
    Duration    p1 i1   == Duration    p2 i2   = (p1 == p2) && (i1 == i2)
    PrimitiveOp _       == PrimitiveOp _       = False
    EvalError   a       == EvalError   b       = a  == b

    -- Function Macro
    FunctionClosure _ _ _ _ == FunctionClosure _ _ _ _ = False

    -- Catch-all for shape mismatches
    _ == _ = False

instance Show Expr where
  showsPrec d expr = showParen (d > 10) $ case expr of
    Primitive   p          -> showString "Primitive " . showsPrec 11 p
    Record      meta kvs   -> showString "Record "    . showsPrec 11 meta . showChar ' ' . showsPrec 11 kvs
    Attr        k    v     -> showString "Attr "      . showsPrec 11 k    . showChar ' ' . showsPrec 11 v
    Array       arr        -> showString "Array "     . showsPrec 11 arr
    Duration    pu   i     -> showString "Duration "  . showsPrec 11 pu . showChar ' ' . showsPrec 11 i
    PrimitiveOp _          -> showString "PrimitiveOp <native_function>"

    FunctionClosure _ (name, _) args bodies -> showString "FunctionClosure <env> " . showsPrec 11 name . showChar ' ' . showsPrec 11 args . showChar ' ' . showsPrec 11 bodies

    EvalError err -> showString "EvalError " . showsPrec 11 err


-- The elements of an array value. An array literal and '#arr-empty' are both arrays;
-- like JSON arrays, their elements can be of any type.
asArray :: Expr -> Maybe [Expr]
asArray = \case
    Array elements       -> Just elements
    Primitive I.EmptyArr -> Just []
    _notAnArray          -> Nothing


-- Type collapses L.Expr into internal I.Expr
freeze :: Expr -> I.Expr 'JLD.Primitive
freeze = \case
          Primitive   p -> p
          Record    m p -> (I.Record m (foldRecordToGADT p))
          Array       p -> (I.Array (foldArrayToGADT p))
          other         -> error $ "Invariant: Non-serializable node type: " ++ (T.unpack $ humanReadableType other)

    where
    -- Convert [(Text, Expr)] to I.Expr 'JLD.List
    foldRecordToGADT :: [(Text, Expr)] -> I.Expr 'JLD.List
    foldRecordToGADT = foldr (\(k, v) acc -> I.Cons (I.Attr k (foldExprToAny v)) acc) I.Nil

    -- Convert [Expr] to I.Expr 'JLD.List
    foldArrayToGADT :: [Expr] -> I.Expr 'JLD.List
    foldArrayToGADT = foldr (\v acc -> I.Cons (foldExprToAny v) acc) I.Nil

    -- Helper: Existential bridge to I.Expr any
    -- This promotes the primitive value to the existential type required by Attr.
    foldExprToAny :: Expr -> I.Expr any
    foldExprToAny expr =
        case expr of
            Primitive p    -> unsafeCoerce p  -- We know this is safe post-harvest
            Record m p     -> unsafeCoerce (I.Record m (foldRecordToGADT p))
            Array els      -> unsafeCoerce (I.Array (foldArrayToGADT els))

            -- Safety Guards
            EvalError err  -> error $ "Invariant: EvalError survived harvest: " ++ show err
            other          -> error $ "Invariant: Non-serializable node type: " ++ (T.unpack $ humanReadableType other)


-- Calendar tracking metrics for time-shift date math engine
data PeriodUnit
    = Years
    | Months
    | Days
    | Hours
    | Minutes
    | Seconds
    -- Rutime checks
    | YearsToEnd
    | MonthsToEnd
    deriving (Show, Eq)


humanReadableType :: Expr -> Text
humanReadableType =
    \case
     Primitive   p      -> "a " <> describePrimitive p
     Duration    _ _    -> "a Duration time period"
     Array       elems  -> describeArray elems
     PrimitiveOp _      -> "a Built-in function"
     EvalError   _      -> "an Error"
     Record      _ _    -> "a Record"
     Attr        _ _    -> "an unbound Attribute pair"
     FunctionClosure _ _ _ _ -> "an unexecuted function"

-- Evaluates the root Array
describeArray :: [Expr] -> Text
describeArray =
    \case
     []    -> "an empty Array"
     elems@(x:_)
         | any ((/= humanReadableType x) . humanReadableType) elems
                 -> "an Array of mixed values"
         | otherwise
                 -> case x of
                        Array inner -> "an Array of "         <> describeInnerArray inner
                        _innerArray -> "an Array containing " <> humanReadableType x

-- Recursively pluralizes nested Arrays until it hits the payload
describeInnerArray :: [Expr] -> Text
describeInnerArray =
    \case
     []    -> "empty Arrays"
     (x:_) -> case x of
                  Array inner -> "Arrays of " <> describeInnerArray inner
                  _notNested  -> "Arrays containing " <> humanReadableType x

-- Helper to describe the inner Primitive
describePrimitive :: I.Expr 'JLD.Primitive -> Text
describePrimitive =
    \case
     I.String    _    -> "String"
     I.Number    _    -> "Number"
     I.Boolean   _    -> "Boolean"
     I.URI       _    -> "URI"
     I.Date      _    -> "Date"
     I.Null           -> "Null value"
     I.BlankNode _    -> "BlankNode"
     I.EmptyArr       -> "Empty Array"
     I.EmptyRec       -> "Empty Record"
     I.Record    _ _  -> "Record"
     I.Array     _    -> "Array"


-- Generated last: Env refers to 'Binding' and 'Expr', and Expr to 'PeriodUnit'
makeLenses ''Env
makeLenses ''Promise


-- Traverses the entire environment scope chain to collect every active
-- bind handle currently available to the evaluator context.
allEnvKeys :: Env -> Set.Set Text
allEnvKeys env =
    let localKeys  = Map.keysSet (env ^. localScope)
        tmpltKeys  = Map.keysSet (env ^. functionRegistry)
        currentSet = Set.union localKeys tmpltKeys
    in case env ^. parentEnv of
           Nothing     -> currentSet
           Just parent -> Set.union currentSet (allEnvKeys parent)
