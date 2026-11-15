
module Data.Ouro.Lisp.Lexer where

import           Data.Char                  (isLetter)
import qualified Data.List.NonEmpty         as NE
import           Data.Ouro.Error.Types      (ErrorContext (..), OuroError (..),
                                             SyntaxError (..))
import qualified Data.Ouro.Lisp.Tokens      as Tkn
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import qualified Text.Megaparsec            as M
import           Text.Megaparsec            (MonadParsec (eof, lookAhead, notFollowedBy),
                                             ParseError (..), Parsec, choice,
                                             getSourcePos, many, manyTill,
                                             oneOf, option, optional, runParser,
                                             some, try, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, letterChar,
                                             space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error      (ErrorItem (Tokens),
                                             ParseErrorBundle)



type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
    space1                         -- Consume standard whitespace
    (L.skipLineComment ";")       -- Line comments start with ;;
    (L.skipBlockComment "#|" "|#") -- Optional: standard block comment syntax


-- Seals the structural starting coordinates right when the token matching begins
withPos :: Parser Tkn.TokenType -> Parser Tkn.Token
withPos p = do
            pos       <- getSourcePos
            tokenType <- p
            pure $ Tkn.Token pos tokenType


-- Lexeme wrapper ensuring spaces are skipped post-token
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


-- Rigid Structural Syntax Elements
pDelims :: Parser Tkn.Token
pDelims = lexeme . withPos $ choice
                             [ Tkn.OpenParen    <$ char '('
                             , Tkn.CloseParen   <$ char ')'
                             , Tkn.OpenBracket  <$ char '['
                             , Tkn.CloseBracket <$ char ']'
                             ]


-- Core Operational Keywords & Symbols. A keyword that only begins a longer name
-- ('graphs', 'important') backtracks, so the name is read as a symbol. A definition is
-- define with the kind it defines, each tried on its own, since each begins with define.
pCoreKeywords :: Parser Tkn.Token
pCoreKeywords = lexeme . withPos . try $ choice
    [ Tkn.Import   <$ string "import"   <* endOfWord
    , Tkn.Export   <$ string "export"   <* endOfWord
    , try (Tkn.DefineProcedure <$ string "define/procedure" <* endOfWord)
    , try (Tkn.DefineProperty  <$ string "define/property"  <* endOfWord)
    , try (Tkn.DefineConstant  <$ string "define/constant"  <* endOfWord)
    , Tkn.Graph    <$ string "graph"    <* endOfWord
    ]

    where
    endOfWord = choice [space1, () <$ lookAhead (oneOf ("()[]" :: String)), eof]


-- A definition says what it defines: define/procedure, define/property or define/constant.
-- define on its own, or with another kind, is an error where it is written, as are defn,
-- defn/const and defn/property, which the define forms replace. Any other name that begins
-- with either ('defined', 'defnode', 'defn/constant') is a name.
pDefinitionWords :: Parser Tkn.Token
pDefinitionWords = lexeme . withPos $ do
    startOffset <- M.getOffset
    written     <- try $ do
                   word    <- choice [string "define", string "defn"] <* (() <$ lookAhead (char '/') <|> notFollowedBy symbolChar)
                   variant <- T.pack <$> many symbolChar
                   case (word, variant) of
                       ("defn", other) | other `notElem` ["", "/const", "/property"]
                           -> fail "a name"
                       _definition
                           -> pure (word <> variant)
    M.region (\err -> M.setErrorOffset startOffset err)
             (fail (T.unpack (retired written)))

    where
    retired :: Text -> Text
    retired = \case
        "defn"          -> "defn is now define/procedure: (define/procedure name (args) body)"
        "defn/const"    -> "defn/const is now define/constant: (define/constant name value)"
        "defn/property" -> "defn/property is now define/property: (define/property name (#contract x) body)"
        written         -> "'" <> written <> "' is not a definition: say what is defined, with define/procedure, define/property or define/constant"


-- Attribute keys, ':key'. A key may start with '@', so JSON-LD's keywords can be written as
-- keys, as a local context's are: (:@vocab "https://example.org/"). After its first character
-- a key may hold ':', so a compact IRI is one key: :crm:P2_has_type.
pAttributes :: Parser Tkn.Token
pAttributes = lexeme . withPos $ do
                                 _       <- char ':'
                                 keyword <- option "" (string "@")
                                 first   <- keyChar
                                 rest    <- many (keyChar <|> char ':')
                                 pure (Tkn.Attr (keyword <> T.pack (first : rest)))
    where
    keyChar = alphaNumChar <|> oneOf ("-_" :: String)


-- Reader Macros starting with '#'. A name that is not a built-in property, but begins with a
-- letter, is a property defined in Ouro, #titled, or imported, #people/titled: it is read as
-- any name is.
pReaderTags :: Parser Tkn.Token
pReaderTags = lexeme . withPos $ do
                                 -- Grab the absolute stream offset BEFORE we consume any characters
                                 startOffset <- M.getOffset

                                 _      <- try (char '#')
                                 rawTag <- some symbolChar
                                 case rawTag of
                                     "uri"          -> pure Tkn.TagUri
                                     "date"         -> pure Tkn.TagDate
                                     "string"       -> pure Tkn.TagString
                                     "number"       -> pure Tkn.TagNumber
                                     "boolean"      -> pure Tkn.TagBoolean
                                     "record"       -> pure Tkn.TagRecord
                                     "array"        -> pure Tkn.TagArray
                                     "function"     -> pure Tkn.TagFunction
                                     "record-empty" -> pure Tkn.TagRecordEmpty
                                     "array-empty"  -> pure Tkn.TagArrayEmpty
                                     name@(first : _)
                                         | isLetter first
                                                    -> pure (Tkn.TagProperty (T.pack name))
                                     other          -> M.region (\err -> M.setErrorOffset startOffset err)
                                                             (fail ("Unknown property: #" ++ other))

-- Structural Holes for Pattern Matching (? or ?name)
pHole :: Parser Tkn.Token
pHole = lexeme . withPos $ do
                           _    <- char '?'

                           -- Look for exactly two dots for the variadic capture
                           dots <- optional (char '.' >> char '.')

                           rest <- many (alphaNumChar <|> oneOf ("_-+*/<>=!?&|~'" :: String))

                           -- Reconstruct the string based on whether the dots were found
                           let prefix = case dots of
                                            Just _  -> "?.."
                                            Nothing -> "?"

                           pure (Tkn.Hole (T.pack (prefix ++ rest)))

-- Standard Data Literals (Strings, Numbers, Bools). A word literal that only begins a longer
-- name ('truer') backtracks, so the name is read as a symbol. Ouro has no null: the word is an
-- error where it is written, and a name that only begins with it ('null-result') is a name.
pLiterals :: Parser Tkn.Token
pLiterals = lexeme . withPos $ choice
                               [ pString
                               , try pNumber
                               , try (pBoolean <* endOfName)
                               , noNull
                               ]

   where
   noNull :: Parser Tkn.TokenType
   noNull = do
            startOffset <- M.getOffset
            _           <- try (string "null" <* endOfName)
            M.region (\err -> M.setErrorOffset startOffset err)
                     (fail "Ouro has no null: leave the attribute out, or state why the value is absent")

   pString :: Parser Tkn.TokenType
   pString = do
             _   <- char '"'
             txt <- manyTill L.charLiteral (char '"')
             pure (Tkn.String (T.pack txt))

   pNumber :: Parser Tkn.TokenType
   pNumber = do
             -- We use pure () to strictly enforce no spaces between '-' and the digits
             let signHandler = L.signed (pure ())

             num <- signHandler ((try L.float) <|> (fromIntegral <$> L.decimal))
             pure (Tkn.Number num)

   pBoolean :: Parser Tkn.TokenType
   pBoolean = choice
              [ Tkn.Boolean True  <$ string "true"
              , Tkn.Boolean False <$ string "false"
              ]

   endOfName :: Parser ()
   endOfName = notFollowedBy symbolChar



-- Fallback General Symbols (Variables, functions, operations)
pSymbol :: Parser Tkn.Token
pSymbol = lexeme . withPos $ Tkn.Symbol <$> symbolName


-- Private bindings in a let, written '$name value'. The name is any symbol, so the
-- binding can be referred to by that symbol.
pBinding :: Parser Tkn.Token
pBinding = lexeme . withPos $ Tkn.Binding <$> (char '$' *> symbolName)


-- The characters of a symbol. 'λ' on its own is read as 'fn', so it is fn everywhere: as
-- the head of a function, and as a name that cannot be used for anything else. A longer
-- name that begins with it, such as 'λx', is an ordinary symbol. The symbols of logic and
-- of sets are read the same way, as the names they stand for, so messages use the names. After
-- its first character a symbol may hold ':', so a path can name a compact IRI key: (get r crm:P2).
symbolName :: Parser Text
symbolName = do
             first <- letterChar <|> oneOf ("_+-*/<>=!&|~" :: String) <|> oneOf mathSymbols
             rest  <- many symbolChar
             pure $ case first : rest of
                        "λ"   -> "fn"
                        "="   -> "eq"
                        "/="  -> "neq"
                        "≠"   -> "neq"
                        "¬"   -> "not"
                        "∧"   -> "and"
                        "∨"   -> "or"
                        "⋀"   -> "all"
                        "⋁"   -> "any"
                        "∀"   -> "forall"
                        "∃"   -> "exists"
                        "∄"   -> "omits"
                        "∪"   -> "union"
                        "∩"   -> "intersect"
                        "∖"   -> "setminus"
                        "⊆"   -> "subset"
                        "⋃"   -> "unions"
                        "⋂"   -> "intersections"
                        "⊨"   -> "satisfies"
                        name  -> T.pack name
    where
    -- They are not letters, so each is allowed to begin a symbol by name
    mathSymbols = "¬∧∨⋀⋁∀∃∄∪∩∖⊆⋃⋂⊨" :: String


-- A character a symbol may hold after its first
symbolChar :: Parser Char
symbolChar = alphaNumChar <|> oneOf ("_-+*/<>=!?&|~':" :: String)


pSingleToken :: Parser Tkn.Token
pSingleToken =  pDelims
            <|> pCoreKeywords
            <|> pDefinitionWords
            <|> pAttributes
            <|> pBinding
            <|> pReaderTags
            <|> pHole
            <|> pLiterals
            <|> pSymbol


-- Unified entry point for the lexical scanner pass.
-- Maps Megaparsec internal error data directly into our zero-allocation OuroError matrix.
tokenize :: String -> Text -> Either OuroError [Tkn.Token]
tokenize filename input =
    case runParser (sc *> many pSingleToken <* eof) filename input of
        Right tokens -> unchecked tokens
        Left bundle  -> Left (translateLexError bundle)

    where
    -- A property defined in Ouro is read, but nothing checks one yet, so the first is an
    -- error, as an unknown property always was. It goes when the parser reads them.
    unchecked :: [Tkn.Token] -> Either OuroError [Tkn.Token]
    unchecked tokens =
        case [ (pos, name) | Tkn.Token pos (Tkn.TagProperty name) <- tokens ] of
            (pos, name) : _ -> Left (OuroError pos (Syntax LexicalError { rawLexeme = "Unknown property: #" <> name }))
            []              -> Right tokens

    translateLexError :: ParseErrorBundle T.Text Void -> OuroError
    translateLexError bundle =
        let -- Extract the primary parsing error sequence
            firstErr NE.:| _ = M.bundleErrors bundle

            -- Traverse the bundle states to compute the exact SourcePos where the error hit.
            ((_, pos) NE.:| _, _) = M.attachSourcePos M.errorOffset (M.bundleErrors bundle) (M.bundlePosState bundle)

            -- Extract the exact un-lexable context or custom failure reason
            context = case firstErr of
                          -- Matches explicit 'fail "..."' calls from your tokenizers (e.g., #poo)
                          -- We map over the ErrorFancy Set cleanly using standard elements
                          FancyError _ fancySet ->
                              case foldr (\x _ -> Just x) Nothing fancySet of
                                  Just (M.ErrorFail msg) -> Syntax LexicalError { rawLexeme = T.pack msg }
                                  _                      -> Syntax LexicalError { rawLexeme = "Malformed lexical sequence." }

                          -- Matches unexpected literal tokens/characters caught automatically by Megaparsec
                          TrivialError _ (Just (Tokens unexpectedChars)) _
                              -> Syntax LexicalError { rawLexeme = "Unexpected token: '" <> T.pack (NE.toList unexpectedChars) <> "'" }

                          TrivialError _ _ _
                              -> Syntax LexicalError { rawLexeme = "Malformed or unrecognized lexical token sequence." }

        in OuroError pos context
