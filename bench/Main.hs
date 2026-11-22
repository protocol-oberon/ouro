module Main (main) where

import qualified Data.ByteString    as BS
import           Data.Int           (Int64)
import           Data.List          (sort)
import           Data.Ouro          (CompilationResult (..), compile,
                                     decodeSource, defaultOptions, toJSON)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as TL
import           System.Directory   (listDirectory)
import           System.FilePath    (takeBaseName, takeExtension, (</>))
import           Test.Tasty.Bench   (bench, bgroup, defaultMain, whnf)


-- The benchmarks.
--
-- Each compiles a program in memory, as the test suite does, with the bundled modules and
-- nothing read from disk while it is timed, and renders the result as JSON, the length of
-- which forces every part of it. So a benchmark is the whole pipeline after reading the file:
-- lexing, parsing, Canon, the static analysis, evaluation and serialisation.
--
--   cabal bench                                      every benchmark
--   cabal bench --benchmark-options='-p hoard'       those whose name has hoard in it
--   cabal bench --benchmark-options='--csv b.csv'    the results as a baseline, and
--               --benchmark-options='--baseline b.csv --fail-if-slower 20'  compared with it
main :: IO ()
main = do
    files    <- sort . filter ((== ".ouro") . takeExtension) <$> listDirectory fixtureFolder
    fixtures <- traverse (\file -> (takeBaseName file,) <$> readFixture (fixtureFolder </> file)) files
    defaultMain
        [ bgroup "fixtures"
              [ bench name (whnf (rendered name) source) | (name, source) <- fixtures ]
        , bgroup "hoard"
              [ bench (show coins <> " coins") (whnf (rendered "hoard") (hoard coins)) | coins <- sizes ]
          -- Arrays at the hoard's sizes: how an operation over all the coins grows with them
        , bgroup "arrays"
              [ bgroup "forall"
                    [ bench (show coins <> " coins") (whnf (rendered "hoard") (hoardWith everyCoinChecked coins)) | coins <- sizes ]
              , bgroup "len in map"
                    [ bench (show coins <> " coins") (whnf (rendered "hoard") (hoardWith lengthPerCoin coins)) | coins <- sizes ]
              , bgroup "union"
                    [ bench (show coins <> " coins") (whnf (rendered "hoard") (hoardWith unionOfCoins coins)) | coins <- sizes ]
              ]
        ]
    where
    sizes = [100, 500, 2000]


-- The test suite's fixtures, each with a graph named after its file: Linked Art's examples
-- (see the README of test/fixtures/json) and Ouro's own
fixtureFolder :: FilePath
fixtureFolder = "test/fixtures/ouro"


-- The length of a graph's JSON, which forces all of it; a program that fails to compile is
-- an error, since a benchmark of an error message measures nothing useful
rendered :: String -> Text -> Int64
rendered graph source = case compile "bench.ouro" graph source of
    CompilationSuccess _ ast -> TL.length (toJSON defaultOptions ast)
    CompilationFailure _ err -> error ("The benchmark's program does not compile: " <> show err)


readFixture :: FilePath -> IO Text
readFixture path = do
    bytes <- BS.readFile path
    either (fail . T.unpack) pure (decodeSource path bytes)


-- A hoard of the given number of coins, as test.ouro's: each coin numbered, weighed, and a
-- member of the chest's contents, built through std's constructors and aat's terms. The
-- weights repeat through 3.00 to 4.19 g, so the output grows with the number of coins alone.
hoard :: Int -> Text
hoard = hoardWith
    [ "  (:context \"https://linked.art/ns/v1/linked-art.json\""
    , "   :@graph  coins))"
    ]

-- Every coin checked as a homogeneous array's refinement would check it: forall over all of
-- them, one has each
everyCoinChecked :: [Text]
everyCoinChecked =
    [ "  (:checked (forall coins (fn (c) (has c identified_by)))"
    , "   :@graph  coins))"
    ]

-- The array's length asked once for each of its elements
lengthPerCoin :: [Text]
lengthPerCoin =
    [ "  (:lengths (map coins (fn (c) (len coins)))))"
    ]

-- The coins as a set, joined with themselves: every coin compared with every other
unionOfCoins :: [Text]
unionOfCoins =
    [ "  (:distinct (len (∪ coins coins))))"
    ]

-- The hoard's coins, bound as coins, with the graph's body after them
hoardWith :: [Text] -> Int -> Text
hoardWith body coins = T.unlines $
    [ "(import std/base)"
    , "(import std/object)"
    , "(import aat/object-type)"
    , "(import aat/identifier-type)"
    , ""
    , "(define/procedure example #uri (#string path)"
    , "  (+ \"https://linked.art/example/\" path))"
    , ""
    , "(define/constant contents"
    , "  (:id (example \"set/chest-contents\") :type \"Set\" :_label \"Coins found in the chest\"))"
    , ""
    , "(define/procedure accession #string (#number n)"
    , "  (-> n (to-string) (pad-start 4 \"0\") (+ \"C-\" ?)))"
    , ""
    , "(define/procedure coin #record ((?n ?grams))"
    , "  (let ($number (accession n))"
    , "    (:id            (example (+ \"object/coin/\" number))"
    , "     :type          \"HumanMadeObject\""
    , "     :_label        (+ \"Coin \" number)"
    , "     :classified_as (list object-type/coin)"
    , "     :identified_by (list (base/identifier identifier-type/accession-number number))"
    , "     :dimension     (list (object/weight grams object/grams))"
    , "     :member_of     (list contents))))"
    , ""
    , "(define/constant weights #array"
    , "  (" <> T.unwords [ T.pack (show (3 + fromIntegral (i `mod` 120) / 100 :: Double)) | i <- [1 .. coins] ] <> "))"
    , ""
    , "(define/constant coins #array"
    , "  (map (zip (1..(len weights)) weights) coin))"
    , ""
    , "(document hoard"
    ] <> body
