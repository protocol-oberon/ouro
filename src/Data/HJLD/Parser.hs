{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.HJLD.Parser where

import           Control.Applicative        (empty)
import           Data.HJLD.Internal.Expr    (Expr)
import qualified Data.HJLD.Internal.Expr    as Expr
import qualified Data.HJLD.Internal.Kinds   as JLD
import           Data.HJLD.Internal.Schema  (Schema (..), SchemaDirective)
import qualified Data.HJLD.Internal.Schema  as Schema
import           Data.Maybe                 (listToMaybe)
import           Data.Scientific            (toRealFloat)
import           Data.Text                  (Text, pack)
import           Data.Time.Format           (defaultTimeLocale, parseTimeM)
import           Data.Void                  (Void)
import           Text.Megaparsec            (ParseErrorBundle, Parsec, between,
                                             choice, manyTill, parse, sepBy,
                                             try, (<|>))
import           Text.Megaparsec.Char       (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.URI                   as URI



type Parser = Parsec Void Text

-- Space Consumer: handles whitespace
sc :: Parser ()
sc = L.space space1 empty empty


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: Text -> Parser Text
symbol = L.symbol sc


-- Parse primitives (without pURI, since URI keys belong to Object scopes)
pExpr :: Parser (Expr 'JLD.Primitive)
pExpr = lexeme $   pObject
              <|> pArray
              <|> pString
              <|> pNumber
              <|> pBoolean
              <|> pNull


-- Intermediate parsing state updated to catch native explicit URI values
data KeyVal
    = ContextKV Schema
    | DataKV Text (Expr 'JLD.Primitive)


-- Parse an Object where the Context closure executes outer precedence
pObject :: Parser (Expr 'JLD.Primitive)
pObject = between (symbol "{") (symbol "}") $ do
    fields <- pObjectField `sepBy` symbol ","

    let mSchema   = listToMaybe [ s | ContextField s <- fields ]
    let dataLists = [ d | DataField d <- fields ]

    -- Because each 'd' is already an (Expr.Attr key val),
    -- we just link the existing attributes together using Cons!
    let propSpine = foldr Expr.Cons Expr.Nil dataLists

    let coreObject = Expr.Object propSpine Expr.Null

    case mSchema of
        Just schema -> return $ Expr.Context schema coreObject
        Nothing     -> return coreObject


-- An intermediate type to separate the structural processing
-- directives (@context) from the underlying property data spine.
data ObjectField
    = ContextField Schema
    | DataField    (Expr 'JLD.List)

pObjectField :: Parser ObjectField
pObjectField = choice
    [ try (symbol "\"@context\"" *> symbol ":") *> (ContextField <$> pSchema)
    , try pURIAttr
    , try pDateAttr
    , DataField <$> pAttr
    ]


pQuotedURI :: Parser URI.URI
pQuotedURI = between (char '"') (char '"') URI.parser

pSchema :: Parser Schema
pSchema = choice
    [ -- Scenario 1: Null context (Clear Context)
      Schema [Schema.ClearContext] <$ symbol "null"

      -- Scenario 2: A single remote context strict URI
    , do uri <- lexeme pQuotedURI
         return $ Schema [Schema.RemoteContext uri]

      -- Scenario 3: An inline object mapping block e.g. {"crm": "..."}
    , between (symbol "{") (symbol "}") $ do
        directives <- pDirective `sepBy` symbol ","
        return $ Schema directives

      -- Scenario 4: A list/array of multiple contexts e.g. ["url1", {"map": "url2"}]
    , between (symbol "[") (symbol "]") $ do
        schemas <- pSchema `sepBy` symbol ","
        let flattenedDirectives = concatMap (\(Schema directives) -> directives) schemas
        return $ Schema flattenedDirectives
    ]


pDirective :: Parser SchemaDirective
pDirective = do
    key <- pKey
    _   <- symbol ":"
    case key of
        "@vocab" -> do
            txt <- pKeyString
            -- Try to parse as an absolute URI first, otherwise fallback to Text
            case URI.mkURI txt of
                Just validUri -> return $ Schema.SetVocab (Left validUri)
                Nothing       -> return $ Schema.SetVocab (Right txt)

        "@base" -> do
            txt <- pKeyString
            return $ Schema.SetBase txt

        -- Fallback case handles standard user-defined terms
        _ -> do
            iri <- pKeyString
            return $ Schema.DefineTerm key (Schema.TermDefinition iri Nothing Nothing)

  where
    pKeyString :: Parser Text
    pKeyString = lexeme $ do
        _   <- char '"'
        str <- manyTill L.charLiteral (char '"')
        return $ pack str


-- Parse Array using the structural primitive wrapper
pArray :: Parser (Expr 'JLD.Primitive)
pArray = between (symbol "[") (symbol "]") $ do
    elems <- pExpr `sepBy` symbol ","
    -- Perfectly folds uniform expressions into an open-tailed Cons spine terminated by Nil
    return $ Expr.Array (foldr Expr.Cons Expr.Nil elems)


-- Pairs cleanly reference top-level primitives
pAttr :: Parser (Expr 'JLD.List)
pAttr = do
    key <- pKey
    _   <- symbol ":"
    val <- pExpr
    pure $ Expr.Attr key val


-- Directly parses the "id" key and a strict URI value into your Attr spine constructor
pURIAttr :: Parser ObjectField
pURIAttr = do
    _   <- symbol "\"id\""
    _   <- symbol ":"
    val <- choice
           [ try pBlankNodeCase
           , pURICase
           ]
    pure . DataField $ Expr.Attr "id" val
  where
    pBlankNodeCase :: Parser (Expr 'JLD.Primitive)
    pBlankNodeCase = do
        _   <- char '"'
        _   <- string "_:"
        str <- manyTill L.charLiteral (char '"')
        pure . Expr.BlankNode . pack $ "_:" ++ str

    -- Reuses the exact same logic as your remote context validation!
    pURICase :: Parser (Expr 'JLD.Primitive)
    pURICase = Expr.URI <$> pQuotedURI


pDateAttr :: Parser ObjectField
pDateAttr = do
    -- Match either target key string within quotes cleanly
    rawKey <- try (symbol "\"begin_of_the_begin\"") <|> try (symbol "\"end_of_the_end\"")
    _   <- symbol ":"
    _   <- char '"'
    str <- lexeme $ manyTill L.charLiteral (char '"')

    let key = case rawKey of
                       "\"begin_of_the_begin\"" -> "begin_of_the_begin"
                       _                        -> "end_of_the_end"

    -- Attempt to parse the timestamp value strictly
    case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" str of
        Just utcTime -> pure . DataField $ Expr.Attr key (Expr.Date utcTime)
        -- Halts validation immediately and raises a localized parse error
        Nothing ->
            fail $ "Invalid ISO 8601 Timestamp format for "
                ++ show key
                ++ ". Expected format: \"YYYY-MM-DDTHH:MM:SSZ\" but got: "
                ++ show str



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
