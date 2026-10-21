
module Data.Ouro.Lisp.Lexer where

import qualified Data.List.NonEmpty         as NE
import           Data.Ouro.Error.Types      (ErrorContext (..), OuroError (..),
                                             SyntaxError (..))
import qualified Data.Ouro.Lisp.Tokens      as Tkn
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import qualified Text.Megaparsec            as M
import           Text.Megaparsec            (MonadParsec (eof, lookAhead),
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
-- ('defnode', 'important') backtracks, so the name is read as a symbol.
pCoreKeywords :: Parser Tkn.Token
pCoreKeywords = lexeme . withPos . try $ choice
    [ Tkn.Import   <$ string "import"   <* endOfWord
    , Tkn.Export   <$ string "export"   <* endOfWord
    , Tkn.Defn     <$ string "defn"     <* endOfWord
    , Tkn.Defun    <$ string "defun"    <* endOfWord
    , Tkn.Graph    <$ string "graph"    <* endOfWord
    ]

    where
    endOfWord = choice [space1, () <$ lookAhead (oneOf ("()[]" :: String)), eof]


-- Attribute keys, ':key'. A key may start with '@', so JSON-LD's keywords can be written as
-- keys, as a local context's are: (:@vocab "https://example.org/").
pAttributes :: Parser Tkn.Token
pAttributes = lexeme . withPos $ do
                                 _       <- char ':'
                                 keyword <- option "" (string "@")
                                 rawName <- T.pack <$> some (alphaNumChar <|> oneOf ("-_" :: String))
                                 pure (Tkn.Attr (keyword <> rawName))


-- Reader Macros starting with '#'
pReaderTags :: Parser Tkn.Token
pReaderTags = lexeme . withPos $ do
                                 -- Grab the absolute stream offset BEFORE we consume any characters
                                 startOffset <- M.getOffset

                                 _      <- try (char '#')
                                 rawTag <- some (alphaNumChar <|> char '-')
                                 case rawTag of
                                     "uri"       -> pure Tkn.TagUri
                                     "date"      -> pure Tkn.TagDate
                                     "str"       -> pure Tkn.TagStr
                                     "num"       -> pure Tkn.TagNum
                                     "bool"      -> pure Tkn.TagBool
                                     "rec"       -> pure Tkn.TagRec
                                     "arr"       -> pure Tkn.TagArr
                                     "fn"        -> pure Tkn.TagFn
                                     "rec-empty" -> pure Tkn.TagRecordEmpty
                                     "arr-empty" -> pure Tkn.TagArrEmpty
                                     other       -> M.region (\err -> M.setErrorOffset startOffset err)
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

-- Standard Data Literals (Strings, Numbers, Bools, Null)
pLiterals :: Parser Tkn.Token
pLiterals = lexeme . withPos $ choice
                               [ pString
                               , try pNumber
                               , pBoolean
                               , Tkn.Null <$ string "null"
                               ]

   where
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



-- Fallback General Symbols (Variables, functions, operations)
pSymbol :: Parser Tkn.Token
pSymbol = lexeme . withPos $ Tkn.Symbol <$> symbolName


-- Private bindings in a let, written '$name value'. The name is any symbol, so the
-- binding can be referred to by that symbol.
pBinding :: Parser Tkn.Token
pBinding = lexeme . withPos $ Tkn.Binding <$> (char '$' *> symbolName)


-- The characters of a symbol. 'λ' on its own is read as 'fn', so it is fn everywhere: as
-- the head of a function, and as a name that cannot be used for anything else. A longer
-- name that begins with it, such as 'λx', is an ordinary symbol.
symbolName :: Parser Text
symbolName = do
             first <- letterChar <|> oneOf ("_+-*/<>=!&|~" :: String)
             rest  <- many (alphaNumChar <|> oneOf ("_-+*/<>=!?&|~'" :: String))
             pure $ case first : rest of
                        "λ"   -> "fn"
                        "∨"   -> "or"
                        "∀"   -> "all"
                        "∃"   -> "exists"
                        name  -> T.pack name


pSingleToken :: Parser Tkn.Token
pSingleToken =  pDelims
            <|> pCoreKeywords
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
        Right tokens -> Right tokens
        Left bundle  -> Left (translateLexError bundle)

    where
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
