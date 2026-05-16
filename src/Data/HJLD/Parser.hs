{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}

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
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, between, choice,
                                             errorBundlePretty, manyTill, parse,
                                             sepBy, try, (<|>))
import           Text.Megaparsec.Char       (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text

-- Space Consumer: handles whitespace
sc :: Parser ()
sc = L.space space1 empty empty


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: Text -> Parser Text
symbol = L.symbol sc


-- Parse primitives, matching your new unified primitive variants
pExpr :: Parser (Expr 'JLD.Primitive)
pExpr = lexeme $   pObject
              <|> pArray
              <|> pString
              <|> pNumber
              <|> pBoolean
              <|> pNull

-- Intermediate parsing state to clean context out from data properties
data KeyVal
    = ContextKV Schema
    | DataKV Text (Expr 'JLD.Primitive)

-- Parse an Object where the Context closure executes outer precedence
pObject :: Parser (Expr 'JLD.Primitive)
pObject = between (symbol "{") (symbol "}") $ do
    pairs <- pObjectField `sepBy` symbol ","

    let mSchema   = listToMaybe [ s | ContextKV s <- pairs ]
    let dataProps = [ (k, v) | DataKV k v <- pairs ]

    -- Clean spine list construction
    let propSpine = foldr (\(k, v) acc -> Expr.Cons (Expr.Attr k v) acc) Expr.Nil dataProps

    -- Core structural object configuration
    let coreObject = Expr.Object propSpine Expr.Null

    case mSchema of
        -- Context wraps Object, maintaining 'JLD.Primitive type status
        Just schema -> return $ Expr.Context schema coreObject
        Nothing     -> return coreObject


pObjectField :: Parser KeyVal
pObjectField = choice
    [ try (symbol "\"@context\"" *> symbol ":") *> (ContextKV <$> pSchema)
    , do (key, val) <- pPair
         return (DataKV key val)
    ]


pSchema :: Parser Schema
pSchema = choice
    [ Schema [Schema.ClearContext] <$ symbol "null"
    , do uri <- pRawStringLiteral
         return $ Schema [Schema.RemoteContext uri]
    , between (symbol "{") (symbol "}") $ do
        directives <- pDirective `sepBy` symbol ","
        return $ Schema directives
    ]
    where
    pRawStringLiteral :: Parser Text
    pRawStringLiteral = lexeme $ do
        _   <- char '"'
        str <- manyTill L.charLiteral (char '"')
        return $ pack str


pDirective :: Parser SchemaDirective
pDirective = do
    key <- pKey
    _   <- symbol ":"
    choice
        [ Schema.SetVocab <$> try pKeyString
        , Schema.SetBase  <$> try pKeyString
        , do iri <- pKeyString
             return $ Schema.DefineTerm key (Schema.TermDefinition iri Nothing Nothing)
        ]
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
pPair :: Parser (Text, Expr 'JLD.Primitive)
pPair = do
    key <- pKey
    _   <- symbol ":"
    val <- pExpr
    return (key, val)


pString :: Parser (Expr 'JLD.Primitive)
pString = Expr.String . pack <$> (char '"' *> manyTill L.charLiteral (char '"'))


pNumber :: Parser (Expr 'JLD.Primitive)
pNumber = lexeme $ do
    num <- L.scientific
    return $ Expr.Number (toRealFloat num)


pBoolean :: Parser (Expr 'JLD.Primitive)
pBoolean = (Expr.Boolean True  <$ symbol "true")
       <|> (Expr.Boolean False <$ symbol "false")


pNull :: Parser (Expr 'JLD.Primitive)
pNull = Expr.Null <$ symbol "null"


pKey :: Parser Text
pKey = lexeme $ do
    _   <- char '"'
    str <- manyTill L.charLiteral (char '"')
    return $ pack str


go :: Text -> IO ()
go = \input -> case parse pExpr "JSON-LD Source" input of
                   Left err -> do
                       putStrLn "Parsing Failed!"
                       putStrLn (errorBundlePretty err)
                   Right expr -> do
                       putStrLn "Parsing Succeeded! Tree Output:"
                       print expr
