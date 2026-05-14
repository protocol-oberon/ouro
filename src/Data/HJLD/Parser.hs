{-# LANGUAGE DataKinds #-}

module Data.HJLD.Parser where

import           Control.Applicative        (empty)
import           Data.HJLD.Internal.Expr    (Expr)
import qualified Data.HJLD.Internal.Expr    as Expr
import qualified Data.HJLD.Internal.Kinds   as JLD
import           Data.Scientific            (toRealFloat)
import           Data.Text                  (Text, pack)
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, between, manyTill,
                                             parseTest, sepBy, (<|>))
import           Text.Megaparsec.Char       (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text

-- Space Consumer: handles whitespace (but not comments for now)
sc :: Parser ()
sc = L.space space1 empty empty

-- Wrapper to consume trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Helper for fixed strings like "{" or ":"
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Parse primitives, including recursive objects.
pPrim :: Parser (Expr 'JLD.Primitive)
pPrim = lexeme $  pObject
              <|> pArray
              <|> pString
              <|> pNumber
              <|> pBoolean
              <|> pNull

-- A standard JSON object parser
pObject :: Parser (Expr 'JLD.Primitive)
pObject = between (symbol "{") (symbol "}") $ do
    pairs <- pPair `sepBy` symbol ","
    return $ Expr.Object pairs

-- Parse Array
pArray :: Parser (Expr 'JLD.Primitive)
pArray = between (symbol "[") (symbol "]") $ do
    -- Use sepBy to handle commas and empty arrays []
    elems <- pPrim `sepBy` symbol ","
    return $ Expr.Array elems

-- A pair now points back to the top-level pPrim
pPair :: Parser (Text, Expr 'JLD.Primitive)
pPair = do
    key <- pKey
    _   <- symbol ":"
    val <- pPrim  -- This allows the recursion!
    return (key, val)


pString :: Parser (Expr 'JLD.Primitive)
pString = Expr.String . pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

-- Handles both integers and floating point
pNumber :: Parser (Expr 'JLD.Primitive)
pNumber = lexeme $ do
    -- L.scientific handles integers, decimals, and scientific notation (1e10)
    num <- L.scientific
    return $ Expr.Number (toRealFloat num)

pBoolean :: Parser (Expr 'JLD.Primitive)
pBoolean = (Expr.Boolean True  <$ symbol "true")
       <|> (Expr.Boolean False <$ symbol "false")

pNull :: Parser (Expr 'JLD.Primitive)
pNull = Expr.Null <$ symbol "null"

-- Parses the key of an object: "key"
pKey :: Parser Text
pKey = lexeme $ do
    _   <- char '"'
    str <- manyTill L.charLiteral (char '"')
    return $ pack str




nestedInput :: Text
nestedInput = pack $ unlines
    [ "{"
    , "  \"name\": \"Alice\","
    , "  \"active\": true,"
    , "  \"address\": {"
    , "    \"city\": \"London\","
    , "    \"zip\": 10118"
    , "  }"
    , "}"
    ]

complexInput :: Text
complexInput = pack $ unlines
    [ "{"
    , "  \"id\": 1001,"
    , "  \"username\": \"haskell_fan\","
    , "  \"verified\": true,"
    , "  \"profile\": {"
    , "    \"bio\": \"Functional programming enthusiast\","
    , "    \"rating\": 4.9"
    , "  },"
    , "  \"tags\": [\"linked-data\", \"json-ld\", \"recursive\"],"
    , "  \"legacy_data\": null"
    , "}"
    ]

main :: IO ()
main = do
    parseTest pPrim "\"Hello HJLD\""
    parseTest pPrim nestedInput
    parseTest pPrim complexInput
