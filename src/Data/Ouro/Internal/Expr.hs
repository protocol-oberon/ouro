{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Data.Ouro.Internal.Expr where

import           Data.List                 (intercalate, sortOn)
import qualified Data.Ouro.Internal.Kinds  as JLD
import           Data.Ouro.Internal.Schema (Schema)
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
    EmptyArr  ::            Expr 'JLD.Primitive
    EmptyObj  ::            Expr 'JLD.Primitive

    -- Structural Metadata Trees (Explicitly index as 'JLD.Meta)
    -- This acts as a strict compile-time guardian preventing metadata leaks.
    Context   :: Schema -> Expr 'JLD.Meta
    EmptyMeta ::           Expr 'JLD.Meta

    -- The Pure Binary Backbones
    Cons    :: Expr head -> Expr tail -> Expr 'JLD.List
    Attr    :: Text      -> Expr any  -> Expr 'JLD.List
    Nil     ::                           Expr 'JLD.List

    -- Boundary Gates
    -- The first slot is strictly bound to a metadata type index.
    -- The second slot captures data sequence payload spine.
    Object  :: Expr 'JLD.Meta -> Expr 'JLD.List -> Expr 'JLD.Primitive
    Array   :: Expr 'JLD.List                   -> Expr 'JLD.Primitive


instance Eq (Expr (t :: JLD.Type)) where
    -- Leaves
    String    a == String    b = a == b
    Number    a == Number    b = a == b
    Boolean   a == Boolean   b = a == b
    URI       a == URI       b = a == b
    Date      a == Date      b = a == b
    Null        == Null        = True
    BlankNode a == BlankNode b = a == b
    EmptyArr    == EmptyArr    = True
    EmptyObj    == EmptyObj    = True

    -- Structural Metadata Trees
    Context   a == Context   b = a == b
    EmptyMeta   == EmptyMeta   = True

    -- Pure Binary Backbones (Wrap in SomeExpr to satisfy the type checker!)
    Cons h1 t1 == Cons h2 t2 = SomeExpr h1 == SomeExpr h2 && SomeExpr t1 == SomeExpr t2
    Attr k1 v1 == Attr k2 v2 = k1 == k2 && SomeExpr v1 == SomeExpr v2
    Nil        == Nil        = True

    -- Boundary Gates
    Object m1 l1 == Object m2 l2 =
        -- Metadata must match exactly.
        -- Properties are flattened, sorted alphabetically by key, and compared.
        m1 == m2 && sortOn fst (flattenProps l1) == sortOn fst (flattenProps l2)

    Array l1 == Array l2 =
        -- Arrays are flattened into lists to handle nested/irregular Cons shapes linearly.
        flattenArray l1 == flattenArray l2

    -- Catch-all for structural mismatches sharing the same type index
    -- (e.g., comparing an Attr to a Nil, both being 'JLD.List)
    _ == _ = False


-- An existential wrapper to securely erase GADT type indices solely for tree rendering.
data SomeExpr where
    SomeExpr :: Expr t -> SomeExpr


-- This allows for comparison between heterogeneous GADT branches (like existentials in Cons/Attr)
-- by dropping their phantom types to runtime checks.
instance Eq SomeExpr where
    SomeExpr (String a)    == SomeExpr (String b)    = a == b
    SomeExpr (Number a)    == SomeExpr (Number b)    = a == b
    SomeExpr (Boolean a)   == SomeExpr (Boolean b)   = a == b
    SomeExpr (URI a)       == SomeExpr (URI b)       = a == b
    SomeExpr (Date a)      == SomeExpr (Date b)      = a == b
    SomeExpr Null          == SomeExpr Null          = True
    SomeExpr (BlankNode a) == SomeExpr (BlankNode b) = a == b
    SomeExpr EmptyArr      == SomeExpr EmptyArr      = True
    SomeExpr EmptyObj      == SomeExpr EmptyObj      = True

    SomeExpr (Context a)   == SomeExpr (Context b)   = a == b
    SomeExpr EmptyMeta     == SomeExpr EmptyMeta     = True

    -- Existential unwrapping
    SomeExpr (Cons h1 t1)  == SomeExpr (Cons h2 t2)  = SomeExpr h1 == SomeExpr h2 && SomeExpr t1 == SomeExpr t2
    SomeExpr (Attr k1 v1)  == SomeExpr (Attr k2 v2)  = k1 == k2 && SomeExpr v1 == SomeExpr v2
    SomeExpr Nil           == SomeExpr Nil           = True

    -- Boundary delegation
    SomeExpr (Object m1 l1)== SomeExpr (Object m2 l2)= SomeExpr m1 == SomeExpr m2 && sortOn fst (flattenProps l1) == sortOn fst (flattenProps l2)
    SomeExpr (Array l1)    == SomeExpr (Array l2)    = flattenArray l1 == flattenArray l2

    -- Shape/Type mismatches
    _                      == _                      = False

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
                             EmptyArr     -> "[]"
                             EmptyObj     -> "{}"

                             -- Closures
                             Context schema -> let schemaStr      = show schema
                                                   schemaLines    = lines schemaStr
                                                   indentedSchema = intercalate ("\n" ++ concat env) schemaLines
                                               in indentedSchema

                             EmptyMeta -> "Null"

                             -- The Pure Binary Backbones
                             Cons h t -> "Cons\n" ++ renderBackbone env isLast h t

                             Attr k v -> let label = "Attr " ++ show k ++ " -> "
                                             pad   = replicate (length label) ' '
                                         in label ++ render (env ++ [pad]) isLast v

                             Nil  -> "Nil"

                             -- Boundary tags
                             Object ctx  body -> "Object\n" ++ renderChildren env [("context -> ", SomeExpr ctx), ("body -> ", SomeExpr body)]
                             Array  list      -> "Array\n"  ++ renderChildren env [("elements -> ", SomeExpr list)]


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
