
module Data.Ouro.Lisp.Lexer where

import qualified Data.Ouro.Lisp.Tokens      as Tkn
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec            (MonadParsec (eof, lookAhead),
                                             Parsec, choice, getSourcePos, many,
                                             manyTill, oneOf, runParser, some,
                                             try, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, letterChar,
                                             space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error      (ParseErrorBundle)


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


-- Core Operational Keywords & Symbols
pCoreKeywords :: Parser Tkn.Token
pCoreKeywords = lexeme . withPos $ choice
                                   [ Tkn.Let     <$ string "let"     <* choice [space1, () <$ lookAhead (oneOf ("()[]" :: String)), eof]
                                   , Tkn.Context <$ string "Context" <* choice [space1, () <$ lookAhead (oneOf ("()[]" :: String)), eof]
                                   ]


-- Attributed Keys and Context Flags starting with ':'
pAttributes :: Parser Tkn.Token
pAttributes = lexeme . withPos $ do
                                 _       <- char ':'
                                 rawName <- T.pack <$> some (alphaNumChar <|> oneOf ("-_" :: String))
                                 pure $ case rawName of
                                            "vocab"    -> Tkn.FlagVocab
                                            "language" -> Tkn.FlagLanguage
                                            "base"     -> Tkn.FlagBase
                                            "terms"    -> Tkn.FlagTerms
                                            "clear"    -> Tkn.FlagClear
                                            "id"       -> Tkn.TypeMappingID
                                            "iri"      -> Tkn.TypeMappingIRI
                                            other      -> Tkn.Attr other


-- Reader Macros starting with '#'
pReaderTags :: Parser Tkn.Token
pReaderTags = lexeme . withPos $ do
                                 _      <- char '#'
                                 rawTag <- some (alphaNumChar <|> char '-')
                                 case rawTag of
                                     "uri"       -> pure Tkn.TagUri
                                     "date"      -> pure Tkn.TagDate
                                     "str"       -> pure Tkn.TagStr
                                     "num"       -> pure Tkn.TagNum
                                     "bool"      -> pure Tkn.TagBool
                                     "obj-empty" -> pure Tkn.TagObjectEmpty
                                     "arr-empty" -> pure Tkn.TagArrEmpty
                                     other       -> fail ("Unknown reader tag: #" ++ other)


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
                                         num <- (try L.float) <|> (fromIntegral <$> L.decimal)
                                         pure (Tkn.Number num)

                               pBoolean :: Parser Tkn.TokenType
                               pBoolean = choice
                                          [ Tkn.Boolean True  <$ string "true"
                                          , Tkn.Boolean False <$ string "false"
                                          ]



-- Fallback General Symbols (Variables, functions, operations)
pSymbol :: Parser Tkn.Token
pSymbol = lexeme . withPos $ do
                             first <- letterChar <|> oneOf ("_+-*/<>=!?&|~" :: String)
                             rest  <- many (alphaNumChar <|> oneOf ("_-+*/<>=!?&|~" :: String))
                             pure (Tkn.Symbol (T.pack (first : rest)))


pSingleToken :: Parser Tkn.Token
pSingleToken =  pDelims
            <|> pCoreKeywords
            <|> pAttributes
            <|> pReaderTags
            <|> pLiterals
            <|> pSymbol


-- The top level entry point for Lexer.hs
tokenize :: String -> Text -> Either (ParseErrorBundle Text Void) [Tkn.Token]
tokenize filename input = runParser (sc *> many pSingleToken <* eof) filename input
