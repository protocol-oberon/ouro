module Data.Ouro.Refinement.Type where

import           Data.Text    (Text)
import           GHC.Generics (Generic)


-- A requirement, as a signature or an assertion writes it: a refinement by its name, or one
-- built from them with ∪ ∩ ∖, (array p) and (record :k p …)
data Type
    = Named    Refinement
    | Union    Type Type        -- (∪ a b): in a, or in b
    | Inter    Type Type        -- (∩ a b): in a and in b
    | Minus    Type Type        -- (∖ a b): in a and not in b
    | ArrayOf  Type             -- (array p): an array whose every element is in p
    | RecordOf [(Text, Type)]   -- (record :k p …): a record with at least these keys, each in its type
    deriving (Show, Eq, Ord, Generic)


-- A refinement by its name: a built-in, or one defined in Ouro
data Refinement
    = RUri              -- the strings that parse as a URI
    | RDate             -- the strings that parse as a date
    | RString           -- the strings
    | RNumber           -- the numbers
    | RBoolean          -- true and false
    | RRecord           -- the records, the empty one included
    | RArray            -- the arrays, the empty one included
    | RFunction         -- procedures and refinements
    | RValue            -- every value: the root of the tree
    | RData             -- what can be written to JSON-LD: strings, numbers, booleans, records, arrays
    | RProcedure        -- the functions that build data: define/procedure, fn, the built-ins
    | RRefinement       -- the functions that judge data: define/refinement
    | RDefined Text     -- a refinement defined in Ouro, by its name as written: titled, people/titled
    deriving (Show, Eq, Ord, Generic)


-- The refinement a name names, as ⊢ and ∈ and a binder write it: a built-in by its word, any
-- other name one defined in Ouro
refinementNamed :: Text -> Refinement
refinementNamed = \case
    "string"     -> RString
    "number"     -> RNumber
    "boolean"    -> RBoolean
    "record"     -> RRecord
    "array"      -> RArray
    "function"   -> RFunction
    "value"      -> RValue
    "data"       -> RData
    "procedure"  -> RProcedure
    "refinement" -> RRefinement
    "uri"        -> RUri
    "date"       -> RDate
    name         -> RDefined name


-- A type as a message writes it, as it is written in Ouro
renderType :: Type -> Text
renderType = \case
    Named refinement -> renderRefinement refinement
    Union    a b     -> "(∪ " <> renderType a <> " " <> renderType b <> ")"
    Inter    a b     -> "(∩ " <> renderType a <> " " <> renderType b <> ")"
    Minus    a b     -> "(∖ " <> renderType a <> " " <> renderType b <> ")"
    ArrayOf  element -> "(array " <> renderType element <> ")"
    RecordOf pairs   -> "(record" <> foldMap (\(key, t) -> " :" <> key <> " " <> renderType t) pairs <> ")"


-- A refinement as a message writes it: by its name, as a type is written
renderRefinement :: Refinement -> Text
renderRefinement = \case
    RUri          -> "uri"
    RDate         -> "date"
    RString       -> "string"
    RNumber       -> "number"
    RBoolean      -> "boolean"
    RRecord       -> "record"
    RArray        -> "array"
    RFunction     -> "function"
    RValue        -> "value"
    RData         -> "data"
    RProcedure    -> "procedure"
    RRefinement   -> "refinement"
    RDefined name -> name
