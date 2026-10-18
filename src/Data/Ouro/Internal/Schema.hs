module Data.Ouro.Internal.Schema where

import           Data.List  (intercalate)
import           Data.Maybe (fromMaybe)
import           Data.Text  (Text)
import qualified Data.Text  as T
import qualified Text.URI   as URI


-- Schema.
--
-- A JSON-LD context: its entries, processed in order. A context with one entry is written
-- as that entry, and one with several as an array, so
--
--   Schema [RemoteContext linkedArt, LocalContext terms]
--
-- serialises as ["https://linked.art/ns/v1/linked-art.json", {...}].
--
-- Contexts compose by concatenation, which is how JSON-LD processes a context array: each
-- entry is applied after the ones before it. Adjacent local contexts are not merged: a later
-- one is processed against the context the earlier one built, so keeping them apart keeps
-- their meaning.
--
-- Applying the same entry twice in a row is the same as applying it once, so where two
-- contexts meet, an entry equal to the one before it is dropped: [X] <> [X] is [X]. Only
-- neighbours collapse. In [X, Y, X] the second X is applied against what Y built, and moving
-- or dropping it would change the meaning. '<>' stays associative, and the empty context
-- (an empty array, which changes nothing) is its identity, for contexts built with '<>',
-- which never hold two equal neighbours.
newtype Schema = Schema [ContextEntry]
    deriving Eq

instance Semigroup Schema where
    Schema a <> Schema b = Schema (a <> dropRepeat b)
        where
        dropRepeat = case reverse a of
            lastEntry : _ -> \case
                first : rest | first == lastEntry -> rest
                entries'                           -> entries'
            []            -> id

instance Monoid Schema where
    mempty = Schema []

instance Show Schema where
    show = \case
            Schema [] -> "Schema []"
            Schema es -> "Schema [\n" ++ intercalate ",\n" (map (("  " ++) . show) es) ++ "\n]"


entries :: Schema -> [ContextEntry]
entries (Schema xs) = xs


-- One entry of a context: a remote context's URL, a local context, or null, which clears
-- the active context.
data ContextEntry
    = RemoteContext !URI.URI
    | LocalContext  Directives
    | ClearContext
    deriving Eq

instance Show ContextEntry where
    show = \case
            RemoteContext uri -> "RemoteContext " ++ URI.renderStr uri
            LocalContext  ds  -> "LocalContext "  ++ show ds
            ClearContext      -> "ClearContext"


-- Directives.
--
-- A local context: keywords (@vocab, @base, @language, ...) and term definitions, each key
-- once, in the order they were first written.
--
-- '<>' is a right-biased union: a key in both keeps the position of its first occurrence
-- and the value of its later one, as defining a term again in JSON-LD replaces it. It is
-- associative (the keys come out in first-occurrence order, and the last value wins,
-- however the union is bracketed), and the empty local context is its identity.
newtype Directives = Directives [(Text, ContextValue)]
    deriving Eq

instance Semigroup Directives where
    Directives a <> Directives b =
        Directives ( [ (key, fromMaybe value (lookup key b)) | (key, value) <- a ]
                  <> [ pair | pair@(key, _) <- b, key `notElem` map fst a ] )

instance Monoid Directives where
    mempty = Directives []

instance Show Directives where
    show (Directives ds) = "{" ++ intercalate ", " [ show k ++ ": " ++ show v | (k, v) <- ds ] ++ "}"


-- A single key of a local context. Built with 'directive' and combined with '<>', a local
-- context always has each key once.
directive :: Text -> ContextValue -> Directives
directive key value = Directives [(key, value)]

directiveList :: Directives -> [(Text, ContextValue)]
directiveList (Directives ds) = ds

-- A JSON-LD keyword, such as @vocab, rather than a term
isKeyword :: Text -> Bool
isKeyword = T.isPrefixOf "@"


-- The value of a key in a local context, in JSON's own terms: a term's IRI, an expanded
-- term definition, a keyword's setting, or null.
data ContextValue
    = CString Text
    | CNumber Double
    | CBool   Bool
    | CNull
    | CArray  [ContextValue]
    | CObject [(Text, ContextValue)]
    deriving (Show, Eq)
