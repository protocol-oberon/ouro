{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Data.HJLD.Internal.Expr where

import qualified Data.HJLD.Internal.Kinds as JLD
import           Data.Text                (Text, unpack)


-- TYPED AST of JASON Linked Data
data Expr (t :: JLD.Type) where
    -- JLD Primitives
    String  :: Text                          -> Expr 'JLD.Primitive
    Number  :: Double                        -> Expr 'JLD.Primitive
    Boolean :: Bool                          -> Expr 'JLD.Primitive
    Object  :: [(Text, Expr 'JLD.Primitive)] -> Expr 'JLD.Primitive
    Array   :: [Expr 'JLD.Primitive]         -> Expr 'JLD.Primitive
    Null    ::                                  Expr 'JLD.Primitive




-- Tree-based Visualization for HJLD Expressions.
--
-- A manual Show instance that renders the GADT as an ASCII tree.
-- It distinguishes between leaf 'Primitive' nodes and branching 'Node'
-- structures, using prefix markers (├──, └──) to indicate depth.
instance Show (Expr t) where
    -- Initiates the recursive rendering with an empty indent.
    -- The root is always treated as the 'last' child of its level.
    show expr = "\n" ++ render "" True expr
        where
        -- Render Expr to string
        render :: String -> Bool -> Expr any -> String
        render indent _isLast = \case
                                -- Base cases MUST NOT have trailing newlines
                                String  txt  -> "String "  ++ show txt
                                Number  n    -> "Number "  ++ show n
                                Boolean b    -> "Boolean " ++ show b
                                Null         -> "Null"
                                -- Containers add a newline after the header, then delegate
                                Object pairs -> "Object\n" ++ renderChildren indent [ (unpack k ++ " -> ", v) | (k, v) <- pairs ]
                                Array items  -> "Array\n"  ++ renderChildren indent [ ("- ", v) | v <- items ]


        renderChildren :: String -> [(String, Expr 'JLD.Primitive)] -> String
        renderChildren indent xs =
            let flags = replicate (length xs - 1) False ++ [True]
                -- Generate the full block of children
                block = concat $ zipWith (renderChild indent) flags xs
            in if null block then "" else init block -- 'init' removes the final trailing '\n'

        renderChild :: String -> Bool -> (String, Expr 'JLD.Primitive) -> String
        renderChild indent isLast (binding, val) =
            let marker      = if isLast then "└── " else "├── "
                padding     = replicate (length binding) ' '
                bar         = if isLast then "    " else "│   "
                childIndent = indent ++ bar ++ padding
                -- Render the result and ensure there is exactly ONE newline at the end of this branch
            in indent ++ marker ++ binding ++ render childIndent True val ++ "\n"
