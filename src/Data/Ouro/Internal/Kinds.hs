
module Data.Ouro.Internal.Kinds where

-- Type.
--
-- Structural kind indices used to enforce JSON-LD domain constraints at compile time.
-- Promoted via DataKinds to partition AST nodes into distinct, non-overlapping categories.
--
-- 'Primitive' represents atomic leaf literals (strings, numbers, booleans, dates)
-- and terminal object/array blocks that emit directly into the final serialization stream.
--
-- We use 'Context' to track active schema scopes and local directive wrappers, ensuring
-- that structural metadata modifications remain bounded during recursive tree descents.
--
-- List is used to construct the pure binary backbone chains (Cons, Attr, Nil) that safely
-- encapsulate sequential networks of child nodes or key-value object property pairs.
data Type
    -- JLD
    = Primitive
    | Context
    | List
    | Meta
    deriving (Show, Eq)
