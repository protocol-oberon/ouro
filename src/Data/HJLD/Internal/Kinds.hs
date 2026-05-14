
module Data.HJLD.Internal.Kinds where


data Type
    = Node
    | Primitive
    | Context
    | List
    deriving (Show, Eq)
