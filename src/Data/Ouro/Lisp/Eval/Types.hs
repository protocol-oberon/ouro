{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Data.Ouro.Lisp.Eval.Types where

import qualified Data.Map.Strict           as Map
import           Data.Ouro.Error.Types     (OuroError)
import qualified Data.Ouro.Internal.Expr   as I
import qualified Data.Ouro.Internal.Kinds  as JLD
import           Data.Ouro.Internal.Schema (Schema)
import qualified Data.Ouro.Lisp.Surface    as S
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import           Text.Megaparsec           (SourcePos)


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
    { localScope :: Map.Map Text S.Expr
    , parentEnv  :: Maybe Env
    }

-- Default root environment.
defaultEnv :: Env
defaultEnv = Env
    { localScope = Map.empty
    , parentEnv  = Nothing
    }

-- Traverses the entire environment scope chain to collect every active
-- bind handle currently available to the evaluator context.
allEnvKeys :: Env -> Set.Set Text
allEnvKeys env = go env Set.empty
    where
    go current acc =
        let localKeys  = Map.keysSet (localScope current)
            updatedAcc = Set.union localKeys acc
        in case parentEnv current of
               Nothing     -> updatedAcc
               Just parent -> go parent updatedAcc

-- Value.
--
-- A monomorphic runtime value wrapper utilized across all compiler evaluation passes.
-- Acts as the foundational bridge between dynamic Lisp runtime types and the strictly-typed Internal GADT.
--
-- While the compilation engine processes untyped surface syntax trees, it requires a unified,
-- type-erased representation to hold intermediate data, execution frames, and native hooks. The Value
-- sum type provides this uniform interface, wrapping deeply typed structures into a singular type space
-- so they can be passed, bound, and accumulated dynamically inside the interpreter's lexical environments.
--
-- Core terminal nodes transition directly into the permanent internal GADT layout:
--   * Primitive injects fully evaluated, strongly-typed JSON-LD leaf elements (like Strings and Numbers).
--   * Array packages compiled sequence blocks, ensuring structural lists are lifted cleanly into terminal positions.
--
-- Specialized evaluation metadata and temporal components are tracked explicitly:
--   * SchemaVal retains active, compiled schema directives and context layout markers used during graph resolution.
--   * Duration serves as an intermediate domain model for calendar math, tracking date-shifting deltas before
--     collapsing them onto concrete timeline objects via the built-in operators.
--
-- Functional abstraction and execution mechanics are treated as first-class primitives:
--   * PrimitiveOp exposes a direct handle to host-platform Haskell functions, serving as the execution vehicle
--     for the standard library registry (such as variadic addition or calendar offset transformations).
--   * Closure captures a user-defined lambda block, structurally closing over its parent environment frame
--     alongside its parameter signature and body. This guarantees full lexical scoping, ensuring variables
--     resolve according to where the function was declared rather than where it is ultimately applied.
data Value where
    Primitive   :: I.Expr 'JLD.Primitive -> Value
    SchemaVal   :: Schema -> Value
    Metadata    :: I.Expr 'JLD.Meta -> Value
    Duration    :: PeriodUnit -> Int -> Value
    Array       :: I.Expr 'JLD.List -> Value
    PrimitiveOp :: (SourcePos -> [Value] -> Either OuroError Value) -> Value
    Closure     :: Env -> Text -> S.Expr -> Value


-- Calendar tracking metrics for time-shift date math engine
data PeriodUnit
    = Years
    | Months
    | Days
    deriving (Show, Eq)


humanReadableType :: Value -> Text
humanReadableType =
    \case
     Primitive   p     -> "a " <> describePrimitive p
     SchemaVal   _     -> "Schema definition directive"
     Metadata    _     -> "Metadata block"
     Duration    _ _   -> "Duration time period"
     Array       _     -> "List layout"
     PrimitiveOp _     -> "Built-in function"
     Closure     _ _ _ -> "An unexecuted function (lambda)"


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
