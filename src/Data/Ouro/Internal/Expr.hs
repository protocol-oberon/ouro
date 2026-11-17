{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Data.Ouro.Internal.Expr where

import           Data.Char                 (isDigit)
import           Data.List                 (intercalate, sortOn)
import qualified Data.Ouro.Internal.Kinds  as JLD
import           Data.Ouro.Internal.Schema (Schema)
import           Data.Ouro.Internal.Uri    (Uri)
import           Data.Text                 (Text, unpack)
import qualified Data.Text                 as T
import           Data.Time                 (UTCTime, defaultTimeLocale,
                                            formatTime, parseTimeM)
import           Numeric                   (showFFloat)


-- Expr t.
--
-- A JSON-LD document, as the compiler builds it and the serialiser writes it.
--
-- Indexed by the promoted 'JLD.Type' kind, so that an ill-formed document cannot be built:
-- a value ('JLD.Primitive') is a leaf, a record or an array, and a context ('JLD.Meta') can
-- only be a record's first slot, never written as a value.
data Expr (t :: JLD.Type) where
    -- Leaves
    String    :: Text    -> Expr 'JLD.Primitive
    Number    :: Double  -> Expr 'JLD.Primitive
    Boolean   :: Bool    -> Expr 'JLD.Primitive
    URI       :: Uri     -> Expr 'JLD.Primitive
    Date      :: UTCTime -> Expr 'JLD.Primitive
    Null      ::            Expr 'JLD.Primitive
    BlankNode :: Text    -> Expr 'JLD.Primitive
    EmptyArr  ::            Expr 'JLD.Primitive
    EmptyRec  ::            Expr 'JLD.Primitive

    -- A record's context, or its absence
    Context   :: Schema -> Expr 'JLD.Meta
    EmptyMeta ::           Expr 'JLD.Meta

    -- Collections: a record's context and its attribute pairs, in order, and an array's
    -- values, in order. Every value in either is a primitive, a record or an array, so an
    -- attribute pair cannot stand in an array, nor a bare value in a record.
    Record  :: Expr 'JLD.Meta -> [(Text, Expr 'JLD.Primitive)] -> Expr 'JLD.Primitive
    Array   :: [Expr 'JLD.Primitive]                         -> Expr 'JLD.Primitive


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
    EmptyRec    == EmptyRec    = True

    -- Structural Metadata Trees
    Context   a == Context   b = a == b
    EmptyMeta   == EmptyMeta   = True

    -- Collections: a record's attribute pairs in any order, as JSON-LD reads them, and an
    -- array's values in theirs
    Record m1 l1 == Record m2 l2 = m1 == m2 && sortOn fst l1 == sortOn fst l2
    Array l1     == Array l2     = l1 == l2

    -- Catch-all for structural mismatches sharing the same type index
    _ == _ = False


-- A record's metadata is a Monoid, built on its context's: two contexts combine as their
-- 'Schema's do, in order, and a record with no context is the identity. Layering a record
-- with an inlay is then 'inlaid <> own': the inlaid record's context is applied first, so
-- the record's own context, applied after it, overrides any term they both define.
instance Semigroup (Expr 'JLD.Meta) where
    Context a <> Context b = Context (a <> b)
    Context a <> EmptyMeta = Context a
    EmptyMeta <> meta      = meta

instance Monoid (Expr 'JLD.Meta) where
    mempty = EmptyMeta

-- An existential wrapper to erase GADT type indices for tree rendering, where a record's
-- context and its values are drawn side by side
data SomeExpr where
    SomeExpr :: Expr t -> SomeExpr


instance Show (Expr t) where
    show expr = "\n" ++ render [] True expr
        where
        -- `env` holds the layout tokens for all parent levels.
        -- Concrete structural components ("│   ", "    ") and label-offsets
        -- are preserved sequentially in the list to prevent drifting.
        render :: [String] -> Bool -> Expr any -> String
        render env _isLast = \case
                             -- Primatives
                             String  txt  -> "String "  ++ show txt
                             Number  n    -> "Number "  ++ show n
                             Boolean b    -> "Boolean " ++ show b
                             URI     u    -> "URI "     ++ show u
                             Date    d    -> "Date "    ++ show d
                             Null         -> "Null"
                             BlankNode n  -> "BLANK "   ++ (unpack n)
                             EmptyArr     -> "[]"
                             EmptyRec     -> "{}"

                             -- Closures
                             Context schema -> let schemaStr      = show schema
                                                   schemaLines    = lines schemaStr
                                                   indentedSchema = intercalate ("\n" ++ concat env) schemaLines
                                               in indentedSchema

                             EmptyMeta -> "Null"

                             -- Collections: a child per attribute pair, or per value
                             Record ctx  body -> "Record\n" ++ renderChildren env (("context -> ", SomeExpr ctx) : [ (show k ++ " -> ", SomeExpr v) | (k, v) <- body ])
                             Array  []        -> "Array []"
                             Array  values    -> "Array\n"  ++ renderChildren env [ ("- ", SomeExpr v) | v <- values ]


        renderChildren :: [String] -> [(String, SomeExpr)] -> String
        renderChildren env xs =
            let flags = replicate (length xs - 1) False ++ [True]
                block = concat $ zipWith (renderChild env) flags xs
            in case null block of
                   True  -> ""
                   False -> init block


        renderChild :: [String] -> Bool -> (String, SomeExpr) -> String
        renderChild env isLast (binding, SomeExpr val) =
            let marker    = case isLast of
                                True  -> "└── " :: String
                                False -> "├── "
                parentBar = case isLast of
                                True  -> "    " :: String
                                False -> "│   "
                pad       = replicate (length binding) ' '
            in concat env ++ marker ++ binding ++ render (env ++ [parentBar, pad]) isLast val ++ "\n"


-- parseDate and renderDate.
--
-- A Date as Ouro reads and writes it, in the form ISO 8601 and xsd:dateTime share,
-- 1642-01-01T00:00:00Z. Both need at least four digits of year: a shorter year is not a
-- date (gap 24), and a year before 1000 is written with its leading zeros, 0079 (gap 53).
parseDate :: String -> Maybe UTCTime
parseDate s = case span isDigit s of
    (year, '-' : _) | length year >= 4 -> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" s
    _shortYear                         -> Nothing

renderDate :: UTCTime -> Text
renderDate = T.pack . formatTime defaultTimeLocale "%0Y-%m-%dT%H:%M:%SZ"


-- renderNumber.
--
-- A Number as Ouro writes it, in JSON and with to-string, as JavaScript writes numbers: a
-- whole number as an integer, and any other in plain decimal with the fewest digits that
-- read back as the same Double, between 10^-7 and 10^21; outside that range, in scientific
-- notation. So 123456789 is "123456789", 0.1 is "0.1" and 1.0e-8 stays "1.0e-8".
renderNumber :: Double -> Text
renderNumber n =
    case (plain, n == fromInteger (round n)) of
        (True,  True)  -> T.pack (show (round n :: Integer))
        (True,  False) -> T.pack (showFFloat Nothing n "")
        (False, _)     -> T.pack (show n)
    where
    plain = n == 0 || (abs n >= 1.0e-7 && abs n < 1.0e21)
