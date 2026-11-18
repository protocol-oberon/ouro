
module Data.Ouro.Lisp.Surface where

import           Data.Ouro.Error.Types (Role)
import           Data.Text             (Text)
import           Text.Megaparsec       (SourcePos)


-- Expr.
--
-- An untyped, ergonomically flexible Abstract Syntax Tree for the Ouro Lisp surface syntax.
-- Designed to prioritize lazy structural evaluation, recursive macro expansion, and developer ergonomics.
--
-- Unlike the rigid, type-indexed backbone of the internal core GADT, the surface AST is intentionally
-- untyped at the representation layer. This architectural omission of strict types allows the syntax
-- tree to support dynamic Lisp macros, forward variable referencing, and knot-tied lexical environment
-- mapping before any type verification or graph compilation passes occur.
--
-- We use uniform metadata anchoring (SourcePos) across all constructors to preserve physical text
-- coordinates from the lexer stream. This ensures that down-funnel type assertion failures and scope
-- lookup errors can map back to precise file locations during compilation.
--
-- Explicit token boundaries are maintained for identifiers and structural keys:
--   * Attr (:key) captures semantic keywords representing graph property transitions.
--   * Symbol (var) serves as an unbound lexical identifier, acting as a lookup hook for local variables,
--     parent frames, or native primitive operation registry fallbacks.
--
-- Atomic leaves are grouped homogeneously under Literal. By packaging raw values (Strings, Numbers,
-- Booleans, and empty structures) into an un-indexed payload wrapper, the parser can handle scalar
-- primitives flexibly as generic data tokens prior to evaluation.
--
-- Type assertions are injected via the Tagged constructor '#'. This allows for explicit validation of types
-- which may visually share identical lexical layouts under raw JSON-LD rules (such as Strings and URLs),
-- ensuring they can be contextualized and evaluated strictly according to intent by the compiler.
--
-- Sequence nesting relies on uniform, unrestricted collections:
--   * Form (...) is the one list container: records, arrays, special forms and calls share it.
--     The source is written as this tree, but a program cannot build or read code as a value:
--     Ouro has no quote, eval or macros (see Appendix A of the spec).
--
-- The surface AST is short-lived. Canon rewrites its derived forms into core ones, the static
-- analysis checks it, and the Engine evaluates it, resolving its lazy environments, into the
-- strict 'JLD.Type' core GADT.
data Expr
    = Symbol      SourcePos Text            -- variable names and functions
    | DefunSymbol SourcePos Text            -- function identifiers
    | Literal     SourcePos LiteralValue    -- Raw String, Number, Boolean, empty Array/Record
    | Tagged      SourcePos ReaderTag Expr  -- #uri "...", #date "..."
    | Required    SourcePos ReaderTag Role Expr  -- A signature's requirement on an argument or a result, in its role: made by the evaluator at a call, never written
    | Hole        SourcePos Text            -- ?, ?name or ?..name in a case pattern
    | Form        SourcePos [Expr]          -- (...) nested lists
    deriving (Show, Eq)


-- Represents standard scalar literals and empty JSON data shapes
-- directly parsed from the Lisp source text.
data LiteralValue
    = Str  Text
    | Bool Bool
    | EmptyArr
    | EmptyRec
    | Num Double
    deriving (Show, Eq)


-- Explicit semantic metadata tags used to enforce validation boundaries
-- or assert target GADT types during evaluation.
data ReaderTag
    = Uri
    | Date
    | StringTag   -- For string assertion   (#string variable)
    | NumberTag   -- For numeric assertion  (#number variable)
    | BooleanTag  -- For boolean assertion  (#boolean variable)
    | RecordTag   -- For record assertion   (#record variable)
    | ArrayTag    -- For array assertion    (#array variable)
    | FunctionTag -- For function assertion (#function variable)
    | RefinementTag Text -- For refinement assertion (#titled variable): a refinement defined in Ouro, by its name as written
    | RecEmpty
    | ArrEmpty
    deriving (Show, Eq)


-- A function's parameter: its name, the type tag written before it, if any, and the
-- pattern it destructures its argument with, if any, as in
--
--   (define/procedure node (#uri id type) ...)
--   (fn ((?slug ?label)) ...)
--
-- A pattern parameter's name cannot be written in Ouro ('%arg1', ...), so its argument
-- is only reached through the holes of its pattern.
data Param = Param
    { paramName    :: Text
    , paramType    :: Maybe (SourcePos, ReaderTag)
    , paramPattern :: Maybe Expr
    } deriving (Show, Eq)


exprPos :: Expr -> SourcePos
exprPos = \case
           Symbol      pos _   -> pos
           DefunSymbol pos _   -> pos
           Literal     pos _   -> pos
           Tagged      pos _ _ -> pos
           Required    pos _ _ _ -> pos
           Hole        pos _   -> pos
           Form        pos _   -> pos


-- A case branch head is a destructuring pattern when it is a record form,
-- or when a '?' hole appears anywhere inside it. Other forms keep their
-- meaning as guards or as the alternatives sugar.
isPatternForm :: Expr -> Bool
isPatternForm = \case
                 Form _ elems -> any isAttrForm elems || any containsHole elems
                 _notAForm    -> False


containsHole :: Expr -> Bool
containsHole = \case
                Hole   _ _       -> True
                Form   _ elems   -> any containsHole elems
                Tagged _ _ inner -> containsHole inner
                _leaf            -> False


-- Matches the parsed form of a ':key value' attribute pair
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
