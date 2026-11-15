module Ouro.FixtureTest (tests) where

import qualified Data.ByteString            as BS
import           Data.Char                  (chr, digitToInt)
import           Data.List                  (sort)
import           Data.Ouro                  (CompilationResult (..), compile,
                                             decodeSource, defaultOptions, toJSON)
import           Data.String                (fromString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Data.Void                  (Void)
import           Hedgehog
import           System.Directory           (listDirectory)
import           System.FilePath            (takeBaseName, takeExtension, (</>))
import           Text.Megaparsec            (Parsec, anySingle, between, choice,
                                             count, eof, errorBundlePretty,
                                             manyTill, parse, sepBy, try, (<|>))
import           Text.Megaparsec.Char       (char, hexDigitChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as L


-- The fixtures.
--
-- Every fixture compiles to its JSON: each =test/fixtures/ouro/name.ouro= has a graph named
-- after its file, which compiles to =test/fixtures/json/name.json=. Most are Linked Art's
-- examples, every one on linked.art's model pages compiling to the JSON linked.art publishes
-- (see the README of test/fixtures/json). They are compared as JSON values, with keys in their
-- order: linked.art's files are written by Python, which escapes every non-ASCII character and
-- indents its own way, so their text differs from Ouro's where their values do not. The folder
-- is read when the tests start, so a fixture added to it is tested with no change here.
tests :: IO Group
tests = do
    files <- sort . filter ((== ".ouro") . takeExtension) <$> listDirectory ouroFolder
    pure $ Group "Fixtures: each compiles to its JSON"
        [ (fromString name, compilesToItsJson name) | file <- files, let name = takeBaseName file ]

ouroFolder, jsonFolder :: FilePath
ouroFolder = "test/fixtures/ouro"
jsonFolder = "test/fixtures/json"

compilesToItsJson :: String -> Property
compilesToItsJson name = withTests 1 . property $ do
    source   <- evalIO (readText (ouroFolder </> name <> ".ouro"))
    expected <- evalIO (readText (jsonFolder </> name <> ".json"))
    case compile (name <> ".ouro") name source of
        CompilationSuccess _ ast -> json (TL.toStrict (toJSON defaultOptions ast)) === json expected
        CompilationFailure _ err -> do
                                    annotate (show err)
                                    failure

readText :: FilePath -> IO Text
readText path = BS.readFile path >>= either (fail . T.unpack) pure . decodeSource path


-- A JSON value, its object's keys in their order, so that order is compared too
data Json
    = JObject [(Text, Json)]
    | JArray  [Json]
    | JString Text
    | JNumber Double
    | JBool   Bool
    | JNull
    deriving (Eq, Show)

type Parser = Parsec Void Text

json :: Text -> Either String Json
json text = either (Left . errorBundlePretty) Right (parse (space *> value <* eof) "json" text)

value :: Parser Json
value = choice
    [ JObject <$> between (symbol '{') (symbol '}') (pair `sepBy` symbol ',')
    , JArray  <$> between (symbol '[') (symbol ']') (value `sepBy` symbol ',')
    , JString <$> stringLiteral <* space
    , JBool True  <$ string "true"  <* space
    , JBool False <$ string "false" <* space
    , JNull       <$ string "null"  <* space
    , JNumber <$> number <* space
    ]
    where
    pair     = (,) <$> (stringLiteral <* space) <*> (symbol ':' *> value)
    symbol c = char c <* space
    number   = try (L.signed (pure ()) L.float) <|> (fromInteger <$> L.signed (pure ()) L.decimal)

-- A JSON string, its escapes read, a \u surrogate pair joined into one character
stringLiteral :: Parser Text
stringLiteral = char '"' *> (T.pack <$> manyTill character (char '"'))
    where
    character = (char '\\' *> escaped) <|> anySingle
    escaped   = choice
        [ '"'  <$ char '"', '\\' <$ char '\\', '/'  <$ char '/'
        , '\n' <$ char 'n', '\t' <$ char 't',  '\r' <$ char 'r'
        , '\b' <$ char 'b', '\f' <$ char 'f'
        , char 'u' *> unicode
        ]
    unicode = do
        high <- hex4
        case high >= 0xD800 && high <= 0xDBFF of
            True  -> do
                     low <- string "\\u" *> hex4
                     pure (chr (0x10000 + (high - 0xD800) * 0x400 + (low - 0xDC00)))
            False -> pure (chr high)
    hex4 = foldl (\n d -> n * 16 + digitToInt d) 0 <$> count 4 hexDigitChar
