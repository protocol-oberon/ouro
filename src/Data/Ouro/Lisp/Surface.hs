
module Data.Ouro.Lisp.Surface where

import           Data.Ouro.Error.Types (Role)
import           Data.Text             (Text)
import           Text.Megaparsec       (SourcePos)


-- The source as written, untyped, each node at its position. Canon rewrites it, the analysis
-- checks it, and the engine evaluates it into the core Expr. docs/spec.org says what it means.
data Expr
    = Symbol      SourcePos Text            -- a name: a variable, a function, a special form's head
    | Literal     SourcePos LiteralValue    -- a string, number or boolean, or an empty array or record
    | Asserted    SourcePos Type Expr       -- <x ⊢ p>, (⊢ x p), the old #p x: x, asserted to be in p
    | Required    SourcePos Type Role Expr  -- a signature's requirement in its role, made at a call, never written
    | Hole        SourcePos Text            -- ?, ?name or ?..name in a case pattern
    | Form        SourcePos [Expr]          -- (...): records, arrays, special forms and calls alike
    deriving (Show, Eq)


-- A literal as written: a scalar, or #array-empty and #record-empty until # retires for
-- (make array) and (make record)
data LiteralValue
    = Str  Text
    | Num  Double
    | Bool Bool
    | EmptyArr
    | EmptyRec
    deriving (Show, Eq)


-- A requirement, as a signature or an assertion writes it: a refinement by its name, or one
-- built from them with ∪ ∩ ∖, (array p) and (record :k p …)
data Type
    = Named    Refinement
    | Union    Type Type        -- (∪ a b): in a, or in b
    | Inter    Type Type        -- (∩ a b): in a and in b
    | Minus    Type Type        -- (∖ a b): in a and not in b
    | ArrayOf  Type             -- (array p): an array whose every element is in p
    | RecordOf [(Text, Type)]   -- (record :k p …): a record with at least these keys, each in its type
    deriving (Show, Eq)


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
    | RDefined Text     -- a refinement defined in Ouro, by its name as written: titled, people/titled
    deriving (Show, Eq)


-- The refinement a name names, as ⊢ and ∈ and a binder write it: a built-in by its word, any
-- other name one defined in Ouro
refinementNamed :: Text -> Refinement
refinementNamed = \case
    "string"   -> RString
    "number"   -> RNumber
    "boolean"  -> RBoolean
    "record"   -> RRecord
    "array"    -> RArray
    "function" -> RFunction
    "uri"      -> RUri
    "date"     -> RDate
    name       -> RDefined name


-- A type as it is written: a refinement's name, ∪ ∩ ∖ of two types, prefix or infix,
-- (array p), or (record :k p …). Infix is still (%infix a op b ...) where the parser reads a
-- binder, before Canon runs; its shape is the parser's to check, so a chain is folded left as
-- the infix pass folds one. (array p) and (record :k p …) are read only here, in a type
-- position, so they never meet (array a b) or a record, the values. A record type's key
-- written twice is given back at its second pair, and (record) is the set record. Anything
-- else is not a type, and is given back for its caller to report or leave.
readType :: Expr -> Either Expr Type
readType = \case
    -- A refinement's name: a built-in, or one defined in Ouro
    Symbol _ name
        -> Right (Named (refinementNamed name))

    -- (array p): the arrays whose every element is in p
    Form _ [Symbol _ "array", element]
        -> ArrayOf <$> readType element

    -- (record :k p …): the records with at least these keys, each read as a type, no key twice
    Form _ (Symbol _ "record" : written)
        -> RecordOf <$> (traverse keyed written >>= distinct)

    -- ∪ ∩ ∖ of two types, by their names as the infix pass writes them
    Form _ [Symbol _ op, a, b]
        | Just combine <- setOperation op
        -> combine <$> readType a <*> readType b

    -- An infix chain the parser has not yet handed to Canon, folded left
    Form pos (Symbol _ "%infix" : first : rest)
        -> readType (foldl (\acc (op, operand) -> Form pos [op, acc, operand]) first (pairs rest))

    -- Anything else is not a type
    other
        -> Left other

    where
    setOperation = \case
        "union"     -> Just Union
        "intersect" -> Just Inter
        "setminus"  -> Just Minus
        _notOne     -> Nothing

    pairs = \case
        op : operand : more -> (op, operand) : pairs more
        _rest               -> []

    -- A record type's pair, :k p, as the parser reads it, (%attr "k" p): its type read, and
    -- its form kept, so that a key written twice is reported where it is
    keyed = \case
        pair@(Form _ [Symbol _ "%attr", Literal _ (Str key), ty]) -> (\t -> (key, t, pair)) <$> readType ty
        other                                                     -> Left other

    -- No key written twice: the second is given back
    distinct = go []
        where
        go _ [] = Right []
        go seen ((key, ty, pair) : rest)
            | key `elem` seen = Left pair
            | otherwise       = ((key, ty) :) <$> go (key : seen) rest


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
    RDefined name -> name


-- A function's parameter: its name, its type, if it has one, and the pattern it destructures
-- its argument with, if any, as in
--
--   (define/procedure node (<id ⊢ uri> type) ...)
--   (fn ((?slug ?label)) ...)
--
-- A pattern parameter's name cannot be written in Ouro ('%arg1', ...), so its argument
-- is only reached through the holes of its pattern.
data Param = Param
    { paramName    :: Text
    , paramType    :: Maybe (SourcePos, Type)
    , paramPattern :: Maybe Expr
    } deriving (Show, Eq)


-- Where an expression was written
exprPos :: Expr -> SourcePos
exprPos = \case
    Symbol   pos _     -> pos
    Literal  pos _     -> pos
    Asserted pos _ _   -> pos
    Required pos _ _ _ -> pos
    Hole     pos _     -> pos
    Form     pos _     -> pos


-- A case branch head is a destructuring pattern when it is a record form, or when a ? hole
-- appears anywhere inside it; other forms keep their meaning as guards or alternatives
isPatternForm :: Expr -> Bool
isPatternForm = \case
    Form _ elems -> any isAttrForm elems || any containsHole elems
    _notAForm    -> False


-- Whether a ? hole appears anywhere inside
containsHole :: Expr -> Bool
containsHole = \case
    Hole     _ _       -> True
    Form     _ elems   -> any containsHole elems
    Asserted _ _ inner -> containsHole inner
    _leaf              -> False


-- Matches the parsed form of a :key value attribute pair
isAttrForm :: Expr -> Bool
isAttrForm = \case
    Form _ (Symbol _ "%attr" : _) -> True
    _notAnAttr                    -> False


-- The heads Canon writes for the forms it builds: %case for and and or, %let for a case's
-- guards, %fn for placeholders, %inlay for with and %in for ∈. No Ouro source can spell them,
-- so no binding can capture code the source did not write (hygiene, as a Scheme macro's if
-- means the core if wherever it is expanded). Each is read as the form it names; any other
-- head is itself.
formName :: Text -> Text
formName = \case
    "%case"  -> "case"
    "%let"   -> "let"
    "%fn"    -> "fn"
    "%inlay" -> "inlay"
    "%in"    -> "in"
    name     -> name


-- A head that asks whether a value has a refinement: in, as Canon writes ∈, or satisfies, its
-- old name, until that is an error
asks :: Text -> Bool
asks name = formName name `elem` ["in", "satisfies"]
