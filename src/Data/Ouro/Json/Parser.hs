{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ouro.Json.Parser where

import           Control.Applicative        (empty)
import           Data.Maybe                 (listToMaybe)
import           Data.Ouro.Internal.Expr    (Expr)
import qualified Data.Ouro.Internal.Expr    as Expr
import qualified Data.Ouro.Internal.Kinds   as JLD
import           Data.Ouro.Internal.Schema  (Schema (..))
import qualified Data.Ouro.Internal.Schema  as Schema
import           Data.Ouro.Internal.Uri     (Uri, uriParser)
import           Data.Scientific            (toRealFloat)
import           Data.Text                  (Text, pack)
import           Data.Void                  (Void)
import           Text.Megaparsec            (ParseErrorBundle, Parsec, between,
                                             choice, manyTill, parse, sepBy,
                                             try, (<|>))
import           Text.Megaparsec.Char       (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L



type Parser = Parsec Void Text

-- Space Consumer: handles whitespace
sc :: Parser ()
sc = L.space space1 empty empty


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: Text -> Parser Text
symbol = L.symbol sc


-- Parse primitives (without pURI, since URI keys belong to Record scopes)
pExpr :: Parser (Expr 'JLD.Primitive)
pExpr = lexeme $  pRecord
              <|> pArray
              <|> pString
              <|> pNumber
              <|> pBoolean
              <|> pNull


-- Intermediate parsing state updated to catch native explicit URI values
data KeyVal
    = ContextKV Schema
    | DataKV Text (Expr 'JLD.Primitive)


-- Parse an Record where Context acts as a clean, self-contained metadata leaf.
pRecord :: Parser (Expr 'JLD.Primitive)
pRecord = between (symbol "{") (symbol "}") $ do
    fields <- pRecordField `sepBy` symbol ","

    let mSchema   = listToMaybe [ s | ContextField s <- fields ]
    let dataLists = [ d | DataField d <- fields ]

    -- Resolve the metadata block as an atomic leaf asset
    let metadataBlock = case mSchema of
            Just schema -> Expr.Context schema
            Nothing     -> Expr.EmptyMeta

    -- The record: its context, and its attribute pairs in the order they were written
    return $ Expr.Record metadataBlock dataLists


-- An intermediate type to separate the structural processing
-- directives (@context) from the record's attribute pairs.
data RecordField
    = ContextField Schema
    | DataField    (Text, Expr 'JLD.Primitive)

pRecordField :: Parser RecordField
pRecordField = choice
    [ try (symbol "\"@context\"" *> symbol ":") *> (ContextField <$> pSchema)
    , try pURIAttr
    , pDateAttr
    , DataField <$> pAttr
    ]


pQuotedURI :: Parser Uri
pQuotedURI = between (char '"') (char '"') uriParser


-- A JSON-LD context: null, a remote context's URL, a local context, or an array of them,
-- which are combined in order, so the array's structure is kept
pSchema :: Parser Schema
pSchema = choice
    [ Schema [Schema.ClearContext] <$ symbol "null"
    , (\uri -> Schema [Schema.RemoteContext uri]) <$> try (lexeme pQuotedURI)
    , (\ds -> Schema [Schema.LocalContext ds]) <$> pLocalContext
    , mconcat <$> between (symbol "[") (symbol "]") (pSchema `sepBy` symbol ",")
    ]


-- A local context's keywords and term definitions, each key kept once
pLocalContext :: Parser Schema.Directives
pLocalContext = between (symbol "{") (symbol "}") $
    mconcat . map (uncurry Schema.directive) <$> (pContextPair `sepBy` symbol ",")


pContextPair :: Parser (Text, Schema.ContextValue)
pContextPair = do
    key   <- pKey
    _     <- symbol ":"
    value <- pContextValue
    pure (key, value)


-- A value in a local context, in JSON's own terms
pContextValue :: Parser Schema.ContextValue
pContextValue = choice
    [ Schema.CObject <$> between (symbol "{") (symbol "}") (pContextPair `sepBy` symbol ",")
    , Schema.CArray  <$> between (symbol "[") (symbol "]") (pContextValue `sepBy` symbol ",")
    , Schema.CString . pack <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))
    , Schema.CNumber . toRealFloat <$> lexeme L.scientific
    , Schema.CBool True  <$ symbol "true"
    , Schema.CBool False <$ symbol "false"
    , Schema.CNull       <$ symbol "null"
    ]


-- Parse Array using the structural primitive wrapper
pArray :: Parser (Expr 'JLD.Primitive)
pArray = between (symbol "[") (symbol "]") $ do
    elems <- pExpr `sepBy` symbol ","
    return $ Expr.Array elems


-- An attribute pair, its key and its value
pAttr :: Parser (Text, Expr 'JLD.Primitive)
pAttr = do
    key <- pKey
    _   <- symbol ":"
    val <- pExpr
    pure (key, val)


-- Directly parses the "id" key and a strict URI value into Attr spine constructor
pURIAttr :: Parser RecordField
pURIAttr = do
           _   <- symbol "\"id\""
           _   <- symbol ":"
           val <- choice
                  [ try pBlankNodeCase
                  , pURICase
                  ]
           pure (DataField ("id", val))

           where
           pBlankNodeCase :: Parser (Expr 'JLD.Primitive)
           pBlankNodeCase = do
                            _   <- char '"'
                            _   <- string "_:"
                            str <- manyTill L.charLiteral (char '"')
                            pure . Expr.BlankNode . pack $ "_:" ++ str

           pURICase :: Parser (Expr 'JLD.Primitive)
           pURICase = Expr.URI <$> pQuotedURI


-- A TimeSpan's dates. Once a date's key and its colon are read, the value must be a date:
-- the parser does not go back to read the pair as an ordinary one (gap 54)
pDateAttr :: Parser RecordField
pDateAttr = do
            key <- try (choice [ dateKey k | k <- dateKeys ] <* symbol ":")
            _   <- char '"'
            str <- lexeme $ manyTill L.charLiteral (char '"')

            -- Attempt to parse the timestamp value strictly
            case Expr.parseDate str of
                Just utcTime -> pure (DataField (key, Expr.Date utcTime))
                -- Halts validation immediately and raises a localized parse error
                Nothing      -> fail $ "Invalid ISO 8601 Timestamp format for "
                                    ++ show key
                                    ++ ". Expected format: \"YYYY-MM-DDTHH:MM:SSZ\" but got: "
                                    ++ show str

            where
            dateKeys = ["begin_of_the_begin", "end_of_the_begin", "begin_of_the_end", "end_of_the_end"]

            -- The key in its quotes, as the key itself
            dateKey k = k <$ try (symbol ("\"" <> k <> "\""))



pString :: Parser (Expr 'JLD.Primitive)
pString = Expr.String . pack <$> (char '"' *> manyTill L.charLiteral (char '"'))


pNumber :: Parser (Expr 'JLD.Primitive)
pNumber = lexeme $ do
    num <- L.scientific
    return $ Expr.Number (toRealFloat num)


pBoolean :: Parser (Expr 'JLD.Primitive)
pBoolean =  (Expr.Boolean True  <$ symbol "true")
        <|> (Expr.Boolean False <$ symbol "false")


pNull :: Parser (Expr 'JLD.Primitive)
pNull = Expr.Null <$ symbol "null"


pKey :: Parser Text
pKey = lexeme $ do
                _   <- char '"'
                str <- manyTill L.charLiteral (char '"')
                return $ pack str


go :: String -> Text -> Either (ParseErrorBundle Text Void) (Expr 'JLD.Primitive)
go = parse pExpr
