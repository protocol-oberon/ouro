
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
    | Export           -- Module exports, can only be templates or functions
    | Defun            -- Named functions that return a primitve
    | Template         -- Functions that return graph nodes
    | Graph            -- JSON-LD graph to compile
    -- Schema Context Specific Flags
    | FlagVocab        -- ':vocab'
    | FlagLanguage     -- ':language'
    | FlagBase         -- ':base'
    | FlagTerms        -- ':terms'
    | FlagClear        -- ':clear'
    -- Term Type Mapping Flags
    | TypeMappingID    -- ':id'
    | TypeMappingIRI   -- ':iri'
    -- Identifiers
    | Attr   !Text     -- ':id', ':type' (stores the string without the leading ':')
    | Symbol !Text     -- 'lambda', variable names like 'node' or 'x'
    | TemplateSymbol !Text -- NEW: Variables/identifiers that explicitly end in '!'
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
    | TagRecordEmpty   -- '#rec-empty' (instantiates Record Nil Nil)
    | TagArrEmpty      -- '#arr-empty' (instantiates Array Nil)
    -- Quote
    | Quote
    -- Hole
    | Hole !Text
    deriving (Show, Eq, Generic)
