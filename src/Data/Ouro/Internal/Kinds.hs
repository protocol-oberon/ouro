
module Data.Ouro.Internal.Kinds where

-- Type.
--
-- The kinds of node in a JSON-LD document (Internal.Expr), promoted with DataKinds to index
-- it. 'Primitive' is a value: a leaf, a record or an array. 'Meta' is a record's context, or
-- its absence. 'Context' indexes no node at present.
data Type
    -- JLD
    = Primitive
    | Context
    | Meta
    deriving (Show, Eq)
