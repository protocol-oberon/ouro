
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
    = OpenParen                 -- '('
    | CloseParen                -- ')'
    | DotDot                    -- '..', between the ends of a range: (1..500)
    | OpenAngle                 -- '<', which opens an infix expression: <a lt b>
    | CloseAngle                -- '>', which closes one
    -- Core lang symbols
    | Let                       -- 'let' (used for binding context scopes)
    | Import                    -- Module imports
    | Export                    -- Module exports, can only be functions
    | DefineProcedure           -- (define/procedure name (args) body): a named function of the procedure kind
    | DefineConstant            -- (define/constant name value): a procedure of no parameters
    | DefineRefinement          -- (define/refinement name (<x ⊢ record>) body): a named function of the refinement kind
    | Graph                     -- (graph name body): a document to compile, written as one output file
    -- Identifiers
    | Attr              !Text   -- ':id', ':type' (stores the string without the leading ':')
    | Binding           !Text   -- '$name' in a let (stores the name without the leading '$')
    | Symbol            !Text   -- 'lambda', variable names like 'node' or 'x'
    | ProseLines        ![Text] -- '| …': consecutive bar lines, a line each, raw to the end of the
                                -- line, the bar, the space after it and trailing spaces dropped
    -- Literals
    | String            !Text   -- "Update", "Core aggregation update for Van Gogh..."
    | Number            !Double -- 42, 3.14159
    | Boolean           !Bool   -- true, false
    -- The old hash forms, still read until the cleanup that ends 0.7.0 makes them errors: a type
    -- is now written in a binder or with ⊢, and the empty values are (make record) and (make array)
    | RetiredRefinement !Text   -- a refinement with a hash before its name, built-in or defined, by its name
    | RetiredEmptyRecord        -- the old spelling of (make record)
    | RetiredEmptyArray         -- the old spelling of (make array)
    -- Hole
    | Hole              !Text   -- '?', '?name' or '?..name': a placeholder in a call, or a hole in a case pattern
    deriving (Show, Eq, Generic)


-- A token as it is written in a source, for messages: Symbol "foo" is foo, Attr "id" is :id.
-- Bar lines are shown by their first line, and an ellipsis when more follow, so a message
-- that names the token stays on one line. A retired form is quoted as it was written.
written :: TokenType -> Text
written = \case
    OpenParen           -> "("
    CloseParen          -> ")"
    DotDot              -> ".."
    OpenAngle           -> "<"
    CloseAngle          -> ">"
    Let                 -> "let"
    Import              -> "import"
    Export              -> "export"
    DefineProcedure     -> "define/procedure"
    DefineConstant      -> "define/constant"
    DefineRefinement    -> "define/refinement"
    Graph               -> "graph"
    Attr k              -> ":" <> k
    Binding b           -> "$" <> b
    Symbol s            -> s
    ProseLines ls       -> case ls of
                               []       -> "|"
                               [line]   -> bar line
                               line : _ -> bar line <> " …"
    String t            -> "\"" <> t <> "\""
    Number n            -> renderNumber n
    Boolean True        -> "true"
    Boolean False       -> "false"
    RetiredRefinement p -> "#" <> p
    RetiredEmptyRecord  -> "#record-empty"
    RetiredEmptyArray   -> "#array-empty"
    Hole h              -> h

    where
    -- A bar line as written: a bar alone, or a bar, a space and its text
    bar = \case
        ""   -> "|"
        line -> "| " <> line
