
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
    | Defn             -- Named top-level functions
    | Defun            -- The old name for defn, kept so it can be reported
    | Graph            -- JSON-LD graph to compile
    -- Identifiers
    | Attr   !Text     -- ':id', ':type' (stores the string without the leading ':')
    | Binding !Text    -- '$name' in a let (stores the name without the leading '$')
    | Symbol !Text     -- 'lambda', variable names like 'node' or 'x'
    -- Literals
    | String  !Text    -- "Update", "Core aggregation update for Van Gogh..."
    | Number  !Double  -- 42, 3.14159
    | Boolean !Bool    -- true, false
    | Null             -- null
    -- Type Assertions
    | TagUri           -- '#uri'       (asserts or enforces a valid URI string layout)
    | TagDate          -- '#date'      (asserts or enforces a valid UTC timestamp layout)
    | TagStr           -- '#str'       (enforces expression resolves to a String)
    | TagNum           -- '#num'       (enforces expression resolves to a Number)
    | TagBool          -- '#bool'      (enforces expression resolves to a Boolean)
    | TagRec           -- '#rec'       (enforces expression resolves to a Record)
    | TagArr           -- '#arr'       (enforces expression resolves to an Array)
    | TagFn            -- '#fn'        (enforces expression resolves to a function)
    | TagRecordEmpty   -- '#rec-empty' (instantiates Record Nil Nil)
    | TagArrEmpty      -- '#arr-empty' (instantiates Array Nil)
    -- Hole
    | Hole !Text
    deriving (Show, Eq, Generic)
