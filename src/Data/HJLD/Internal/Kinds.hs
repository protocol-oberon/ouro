
module Data.HJLD.Internal.Kinds where


data Type
    = Node
    | Value
    | Context
    | List
    deriving (Show, Eq)
