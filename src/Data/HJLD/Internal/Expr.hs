{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Data.HJLD.Internal.Expr where

import qualified Data.HJLD.Internal.Kinds  as JLD
import           Data.HJLD.Internal.Schema (Schema)
import           Data.List                 (intercalate)
import           Data.Text                 (Text, unpack)
import           Data.Time                 (UTCTime)
import qualified Text.URI                  as MURI
import           Text.URI                  (URI)


-- Expr t.
--
-- A type-safe Abstract Syntax Tree for JSON Linked Data (JSON-LD).
-- Indexed by a promoted 'Type' kind to enforce structural validity at compile time.
--
-- The strict type indexing of this GADT serves as a mandatory compile-time
-- guardian for JSON-LD structural conformity. By anchoring the expression type to the
-- promoted 'JLD.Type' kind, the compiler statically prevents the generation of illegal
-- or corrupted JSON-LD documents before a single line of serialization code even runs.
--
-- We use explicit primitive indexing ('Expr 'JLD.Primitive') on leaves like String and
-- Number to enforce structural boundaries. This prevents atomic literal values from
-- masquerading as collections, stopping invalid compositions before code-gen can execute.
--
-- A rigid backbone constraint ('Expr 'JLD.List') to completely isolate internal
-- sequential networks. By forcing Cons, Attr, and Nil into an isolated type parameter,
-- we guarantee that collection mechanics never leak outward into atomic leaf operations.
--
-- Transparent identity loops ('Expr t -> Expr t') on Context and Reverse closures are used
-- to preserve types across metadata wrappers. This allows the printer to freely descend
-- deep structural scopes without altering or shifting the underlying node's type index.
--
-- We use Object and Array constructors as strict boundary gates to transition the AST.
-- They ingest an internal sequence list and lift it back into a terminal primitive type,
-- ensuring that only syntactically sound blocks are emitted to the final printer streamdata
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


-- Util
-- Recursively unrolls a binary Cons-chain backbone into a flat list of existentially wrapped expressions.
flattenArray :: Expr t -> [SomeExpr]
flattenArray = \case
                Cons h t -> SomeExpr h : flattenArray t
                Nil      -> []
                other    -> [SomeExpr other]


-- Recursively traverses an internal list structure to collect and flatten all nested key-value attribute pairs.
flattenProps :: Expr t -> [(Text, SomeExpr)]
flattenProps = \case
                Cons h t -> flattenProps h ++ flattenProps t
                Attr k v -> [(k, SomeExpr v)]
                _        -> []


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
                             Context schema inner -> let schemaStr      = show schema
                                                         -- Split the multi-line schema string by its newlines
                                                         schemaLines    = lines schemaStr
                                                         -- Re-join them, forcing the current `env` block
                                                         -- to prefix every line after the first one!
                                                         indentedSchema = intercalate ("\n" ++ concat env) schemaLines
                                                     in "Context  " ++ indentedSchema ++ "\n" ++
                                                        concat env  ++ "└── " ++
                                                        render (env ++ ["    " :: String]) True inner

                             Reverse inner -> "Reverse\n" ++
                                              concat env  ++ "└── " ++
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
