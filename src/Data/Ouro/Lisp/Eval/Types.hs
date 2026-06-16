{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Ouro.Lisp.Eval.Types where

import           Control.Monad.Reader      (Reader)
import           Data.List                 (sortOn)
import qualified Data.Map.Strict           as Map
import           Data.Ouro.Error.Types     (OuroError)
import qualified Data.Ouro.Internal.Expr   as I
import qualified Data.Ouro.Internal.Kinds  as JLD
import           Data.Ouro.Internal.Schema (Schema, SchemaDirective)
import qualified Data.Ouro.Lisp.Surface    as S
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Lens.Micro                ((^.))
import           Lens.Micro.TH             (makeLenses)
import           Text.Megaparsec           (SourcePos)
import           Unsafe.Coerce             (unsafeCoerce)


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
-- When an object block is evaluated, its properties are bound into a fresh Env frame whose parent pointer closes over
-- the surrounding context. Because these frames are captured by value inside closures, sub-graphs can pass functions
-- and variables down-stream without leaking local data or accidentally mutating outer execution registers.
--
-- Default state boundaries are controlled explicitly:
--   * defaultEnv instantiates a clean, terminal root scope layer containing zero localized bindings and no
--     parent fallback link, anchoring the absolute bottom of the variable resolution ladder.
data Env = Env
    { _localScope    :: Map.Map Text S.Expr
    , _parentEnv     :: Maybe Env
    , _activeLookups :: Set Text
    }

makeLenses ''Env


-- Default root environment.
defaultEnv :: Env
defaultEnv = Env
    { _localScope    = Map.empty
    , _parentEnv     = Nothing
    , _activeLookups = Set.empty
    }

-- Traverses the entire environment scope chain to collect every active
-- bind handle currently available to the evaluator context.
allEnvKeys :: Env -> Set.Set Text
allEnvKeys env = go env Set.empty
    where
    go current acc =
            -- Use ^. to extract the local map from the current environment context
            let localKeys  = Map.keysSet (current ^. localScope)
                updatedAcc = Set.union localKeys acc
            in case current ^. parentEnv of
                   Nothing     -> updatedAcc
                   Just parent -> go parent updatedAcc


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
--   * Resilient Scaffolding: Object and Array represent the "open" structural
--     layout of the graph. These structures hold recursive 'Expr' branches,
--     maintaining the tree topology even if child nodes are invalid.
--   * Fault Isolation: The 'EvalError' constructor serves as a universal terminal
--     node that allows the harvester to sweep through a partially-failed tree and
--     aggregate all diagnostic issues in a single pass.
--   * Domain Modeling: Retains explicit handles for functional abstractions (Closure,
--     PrimitiveOp) and intermediate domain logic (Duration, SchemaVal) that have
--     not yet been collapsed into the final, frozen JSON-LD GADT ('I.Expr').
--
-- Terminal Resolution:
--   The 'Primitive' and 'Metadata' constructors serve as the final transition
--   anchors. Once the evaluation loop completes and all errors are harvested,
--   these nodes verify that the evaluated 'Expr' tree satisfies the strict
--   Internal GADT constraints required for final serialization.
data Expr where
    -- Pristine Frozen Targets (The pure GADTs)
    Primitive   :: I.Expr 'JLD.Primitive -> Expr
    Metadata    :: I.Expr 'JLD.Meta      -> Expr

    -- Resilient Compilation Scaffolding (The Superset Nodes)
    -- These maintain the open tree structure during evaluation, allowing
    -- errors to be embedded at any depth.
    Object      :: I.Expr 'JLD.Meta -> [(Text, Expr)] -> Expr
    Array       :: [Expr]                             -> Expr

    -- Dedicated Evaluation Leaves
    Duration    :: PeriodUnit -> Int -> Expr

    SchemaVal   :: Schema          -> Expr
    Directive   :: SchemaDirective -> Expr

    PrimitiveOp :: NativeFunction -> Expr
    Closure     :: Env -> Text -> S.Expr -> Expr

    -- The Universal Error Leaf: Allows the engine to bypass crashes
    -- and continue evaluating sibling nodes.
    EvalError   :: OuroError -> Expr

type NativeFunction = SourcePos -> [Expr] -> Reader Env Expr


instance Eq Expr where
    -- Frozen Targets
    Primitive a == Primitive b = a == b
    Metadata  a == Metadata  b = a == b

    -- Compilation Scaffolding
    Object a1 b1 == Object a2 b2 = a1 == a2 && sortOn fst b1 == sortOn fst b2
    Array  a     == Array  b     = a == b

    -- Eval only leaves
    Duration    p1 i1   == Duration    p2 i2   = (p1 == p2) && (i1 == i2)
    SchemaVal   a       == SchemaVal   b       = a == b
    Directive   a       == Directive   b       = a == b
    PrimitiveOp _       == PrimitiveOp _       = False
    Closure     _  _  _ == Closure     _  _  _ = False
    EvalError   a       == EvalError   b       = a == b

    -- Catch-all for shape mismatches
    _ == _ = False


-- Type collapses L.Expr into internal I.Expr
freeze :: Expr -> I.Expr 'JLD.Primitive
freeze = \case
          Primitive   p -> p
          Object    m p -> (I.Object m (foldObjectToGADT p))
          Array       p -> (I.Array (foldArrayToGADT p))
          other         -> error $ "Invariant: Non-serializable node type: " ++ (T.unpack $ humanReadableType other)

    where
    -- Convert [(Text, Expr)] to I.Expr 'JLD.List
    foldObjectToGADT :: [(Text, Expr)] -> I.Expr 'JLD.List
    foldObjectToGADT = foldr (\(k, v) acc -> I.Cons (I.Attr k (foldExprToAny v)) acc) I.Nil

    -- Convert [Expr] to I.Expr 'JLD.List
    foldArrayToGADT :: [Expr] -> I.Expr 'JLD.List
    foldArrayToGADT = foldr (\v acc -> I.Cons (foldExprToAny v) acc) I.Nil

    -- Helper: Existential bridge to I.Expr any
    -- This promotes the primitive value to the existential type required by Attr.
    foldExprToAny :: Expr -> I.Expr any
    foldExprToAny expr = case expr of
        Primitive p    -> unsafeCoerce p  -- We know this is safe post-harvest
        Object m p     -> unsafeCoerce (I.Object m (foldObjectToGADT p))
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
     Primitive   p     -> "a " <> describePrimitive p
     SchemaVal   _     -> "Schema definition directive"
     Directive   _     -> "Schema configuration directive"
     Metadata    _     -> "Metadata block"
     Duration    _ _   -> "Duration time period"
     Array       _     -> "List layout"
     PrimitiveOp _     -> "Built-in function"
     Closure     _ _ _ -> "an unexecuted function (lambda)"
     EvalError   _     -> "an Error"
     Object      _ _   -> "an Object block"


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
     I.EmptyObj       -> "Empty Object"
     I.Object    _ _  -> "Object"
     I.Array     _    -> "Array"
