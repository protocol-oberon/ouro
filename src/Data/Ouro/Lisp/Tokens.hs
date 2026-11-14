
module Data.Ouro.Lisp.Tokens where

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
    | OpenBracket      -- '['
    | CloseBracket     -- ']'
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
    | TagRecordEmpty   -- '#record-empty' (instantiates Record Nil Nil)
    | TagArrayEmpty    -- '#array-empty'  (instantiates Array Nil)
    | TagProperty !Text -- '#titled', '#people/titled': a property defined in Ouro, by its name
    -- Hole
    | Hole !Text
    deriving (Show, Eq, Generic)
