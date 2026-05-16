{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Data.HJLD.Internal.Expr where

import qualified Data.HJLD.Internal.Kinds  as JLD
import           Data.HJLD.Internal.Schema (Schema)
import           Data.Text                 (Text, unpack)
import           Data.Time                 (UTCTime)
import qualified Text.URI                  as MURI
import           Text.URI                  (URI)


-- TYPED AST of JASON Linked Data
data Expr (t :: JLD.Type) where
    -- Leaves
    String    :: Text    -> Expr 'JLD.Primitive
    Number    :: Double  -> Expr 'JLD.Primitive
    Boolean   :: Bool    -> Expr 'JLD.Primitive
    URI       :: URI     -> Expr 'JLD.Primitive
    Date      :: UTCTime -> Expr 'JLD.Primitive
    Null      ::            Expr 'JLD.Primitive
    BlankNode :: Text    -> Expr 'JLD.Primitive

    -- Structural Trees / Closures
    Context :: Schema  -> Expr t -> Expr t
    Reverse ::            Expr t -> Expr t

    -- The Pure Binary Backbones
    -- A Cons can take any data node or property as its head, and another list or Nil as its tail.
    Cons    :: Expr head -> Expr tail -> Expr 'JLD.List
    Attr    :: Text      -> Expr any  -> Expr 'JLD.List
    Nil     ::                           Expr 'JLD.List

    -- Boundary Tags
    Object  :: Expr 'JLD.List -> Expr any -> Expr 'JLD.Primitive
    Array   :: Expr 'JLD.List             -> Expr 'JLD.Primitive


-- An existential wrapper to securely erase GADT type indices solely for tree rendering.
data SomeExpr where
    SomeExpr :: Expr t -> SomeExpr


instance Show (Expr t) where
    show expr = "\n" ++ render [] True expr
        where
        -- `env` holds the layout tokens for all parent levels.
        -- Concrete structural components ("│   ", "    ") and label-offsets
        -- are preserved sequentially in the list to prevent drifting.
        render :: [String] -> Bool -> Expr any -> String
        render env isLast = \case
                             -- Primatives
                             String  txt  -> "String "  ++ show txt
                             Number  n    -> "Number "  ++ show n
                             Boolean b    -> "Boolean " ++ show b
                             URI     u    -> "URI "     ++ MURI.renderStr u
                             Date    d    -> "Date "    ++ show d
                             Null         -> "Null"
                             BlankNode n  -> "BLANK "   ++ (unpack n)

                             -- Closures
                             Context schema inner -> "Context "  ++ show schema ++ "\n" ++
                                                     concat env ++ "└── " ++
                                                     render (env ++ ["    " :: String]) True inner

                             Reverse inner -> "Reverse\n" ++
                                              concat env ++ "└── " ++
                                              render (env ++ ["    " :: String]) True inner

                             -- The Pure Binary Backbones
                             Cons h t -> "Cons\n" ++ renderBackbone env isLast h t

                             Attr k v -> let label = "Attr " ++ show k ++ " -> "
                                             pad   = replicate (length label) ' '
                                         in label ++ render (env ++ [pad]) isLast v

                             Nil  -> "Nil"

                             -- Boundary tags
                             Object props body -> "Object\n" ++ renderChildren env [("properties -> ", SomeExpr props), ("body -> ", SomeExpr body)]
                             Array  list       -> "Array\n"  ++ renderChildren env [("elements -> ", SomeExpr list)]


        renderBackbone :: [String] -> Bool -> Expr head -> Expr tail -> String
        renderBackbone env _isLast headExpr tailExpr =
            let markerH, markerT, barH, barT :: String
                markerH = "├── "
                markerT = "└── "
                barH    = "│   "
                barT    = "    "
                lineH   = concat env ++ markerH ++ render (env ++ [barH]) False headExpr
                lineT   = case tailExpr of
                              Cons nextH nextT -> concat env ++ markerT ++ "Cons\n" ++ renderBackbone (env ++ [barT]) True nextH nextT
                              _                -> concat env ++ markerT ++ render env True tailExpr
            in lineH ++ "\n" ++ lineT


        renderChildren :: [String] -> [(String, SomeExpr)] -> String
        renderChildren env xs =
            let flags = replicate (length xs - 1) False ++ [True]
                block = concat $ zipWith (renderChild env) flags xs
            in if null block then "" else init block


        renderChild :: [String] -> Bool -> (String, SomeExpr) -> String
        renderChild env isLast (binding, SomeExpr val) =
            let marker    = if isLast then "└── " :: String else "├── " :: String
                parentBar = if isLast then "    " :: String else "│   " :: String
                pad       = replicate (length binding) ' '
            in concat env ++ marker ++ binding ++ render (env ++ [parentBar, pad]) isLast val ++ "\n"
