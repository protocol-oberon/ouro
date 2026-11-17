
module Data.Ouro.Lisp.Tokens where

import           Data.Ouro.Internal.Expr (renderNumber)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Text.Megaparsec.Pos (SourcePos)


-- A single lexeme produced by the lexer, pairing physical source stream
-- coordinates with its underlying syntactic category.
data Token = Token
    { pos       :: !SourcePos
    , tokenType :: !TokenType
    } deriving (Show, Eq, Generic)

-- The complete inventory of terminal symbols recognized by the Ouro Lisp tokenizer.
data TokenType
    -- Structural Delims
    = OpenParen        -- '('
    | CloseParen       -- ')'
    | DotDot           -- '..', between the ends of a range: (1..500)
    -- Core lang symbols
    | Let              -- 'let' (used for binding context scopes)
    | Import           -- Module imports
    | Export           -- Module exports, can only be functions
    | DefineProcedure  -- (define/procedure name (args) body): a named function of the procedure kind
    | DefineConstant   -- (define/constant name value): a procedure of no parameters
    | DefineProperty   -- (define/property ...): read, but not yet a form
    | Graph            -- JSON-LD graph to compile
    -- Identifiers
    | Attr   !Text     -- ':id', ':type' (stores the string without the leading ':')
    | Binding !Text    -- '$name' in a let (stores the name without the leading '$')
    | Symbol !Text     -- 'lambda', variable names like 'node' or 'x'
    -- Literals
    | String  !Text    -- "Update", "Core aggregation update for Van Gogh..."
    | Number  !Double  -- 42, 3.14159
    | Boolean !Bool    -- true, false
    -- Type Assertions
    | TagUri           -- '#uri'          (asserts or enforces a valid URI string layout)
    | TagDate          -- '#date'         (asserts or enforces a valid UTC timestamp layout)
    | TagString        -- '#string'       (enforces expression resolves to a String)
    | TagNumber        -- '#number'       (enforces expression resolves to a Number)
    | TagBoolean       -- '#boolean'      (enforces expression resolves to a Boolean)
    | TagRecord        -- '#record'       (enforces expression resolves to a Record)
    | TagArray         -- '#array'        (enforces expression resolves to an Array)
    | TagFunction      -- '#function'     (enforces expression resolves to a function)
    | TagRecordEmpty   -- '#record-empty' (the empty record, {})
    | TagArrayEmpty    -- '#array-empty'  (the empty array, [])
    | TagProperty !Text -- '#titled', '#people/titled': a property defined in Ouro, by its name
    -- Hole
    | Hole !Text
    deriving (Show, Eq, Generic)


-- A token as it is written in a source, for messages: Symbol "foo" is foo, Attr "id" is :id
written :: TokenType -> Text
written = \case
    OpenParen       -> "("
    CloseParen      -> ")"
    DotDot          -> ".."
    Let             -> "let"
    Import          -> "import"
    Export          -> "export"
    DefineProcedure -> "define/procedure"
    DefineConstant  -> "define/constant"
    DefineProperty  -> "define/property"
    Graph           -> "graph"
    Attr k          -> ":" <> k
    Binding b       -> "$" <> b
    Symbol s        -> s
    String t        -> "\"" <> t <> "\""
    Number n        -> renderNumber n
    Boolean True    -> "true"
    Boolean False   -> "false"
    TagUri          -> "#uri"
    TagDate         -> "#date"
    TagString       -> "#string"
    TagNumber       -> "#number"
    TagBoolean      -> "#boolean"
    TagRecord       -> "#record"
    TagArray        -> "#array"
    TagFunction     -> "#function"
    TagRecordEmpty  -> "#record-empty"
    TagArrayEmpty   -> "#array-empty"
    TagProperty p   -> "#" <> p
    Hole h          -> h
