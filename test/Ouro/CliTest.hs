module Ouro.CliTest
( builds
, compiles
, validates
) where

import           Control.Monad      (forM_, when)
import qualified Data.ByteString    as BS
import           Data.Char          (isAlpha, isSpace)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import           Hedgehog
import           System.Directory   (createDirectoryIfMissing,
                                     doesDirectoryExist, doesFileExist,
                                     getTemporaryDirectory,
                                     removeDirectoryRecursive)
import           System.Exit        (ExitCode (..))
import           System.FilePath    (takeDirectory, (</>))
import           System.IO          (IOMode (..), withFile)
import           System.Process     (CreateProcess (..), StdStream (..), proc,
                                     waitForProcess, withCreateProcess)


-- ouro build, run as a user runs it, in a project of its own
builds :: Group
builds = Group "CLI: ouro build"
    [ ("every graph is written, and counted",       withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-success" [ build, ("main.ouro", "(graph g (:r 1))") ])
                                                        run <- evalIO (ouro dir ["build"])
                                                        exitCode run === ExitSuccess
                                                        assert ("Build Success:       1 graphs" `T.isInfixOf` plain (out run))
                                                        json <- evalIO (readText (dir </> "out" </> "main" </> "g.json"))
                                                        squash json === "{\"r\":1}")
    , ("a failure writes nothing, its errors on stderr", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-failure" [ build, ("main.ouro", "(graph g (:r (get () 5)))") ])
                                                        run <- evalIO (ouro dir ["build"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("Build Failed: nothing was written, due to compiler errors." `T.isInfixOf` plain (out run))
                                                        assert ("Error [" `T.isInfixOf` plain (err run))
                                                        assert (not ("Error [" `T.isInfixOf` plain (out run)))
                                                        written <- evalIO (doesDirectoryExist (dir </> "out"))
                                                        written === False)
    , ("without a build file, it says so on stderr", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-no-build-file" [ ("main.ouro", "(graph g (:r 1))") ])
                                                        run <- evalIO (ouro dir ["build"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("No build.ouro in" `T.isInfixOf` plain (err run)))
    , ("gap 27: a renamed graph's old output is left", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-stale" [ build, ("main.ouro", "(graph g (:r 1))") ])
                                                        _   <- evalIO (ouro dir ["build"])
                                                        evalIO (BS.writeFile (dir </> "main.ouro") "(graph h (:r 1))")
                                                        run <- evalIO (ouro dir ["build"])
                                                        exitCode run === ExitSuccess
                                                        both <- evalIO (traverse (doesFileExist . (\graph -> dir </> "out" </> "main" </> graph)) ["g.json", "h.json"])
                                                        both === [True, True])
    , ("gap 35: a build writes until a write fails", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-partial" [ build, ("a.ouro", "(graph g (:r 1))"), ("b.ouro", "(graph g (:r 2))"), ("out/b", "a file where a folder goes") ])
                                                        run <- evalIO (ouro dir ["build"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("already exists" `T.isInfixOf` plain (err run))
                                                        assert (not (any (`T.isInfixOf` plain (out run)) ["Build Success", "Build Failed"]))
                                                        written <- evalIO (doesFileExist (dir </> "out" </> "a" </> "g.json"))
                                                        written === True)
    , ("gap 37: colours are written when the output is not a terminal", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-colours" [ build, ("main.ouro", "(graph g (:r (get () 5)))") ])
                                                        run <- evalIO (ouro dir ["build"])
                                                        assert ("\ESC[" `T.isInfixOf` err run))
    ]
    where
    build = ("build.ouro", "(graph build (:name \"p\"))")


-- ouro compile, run as a user runs it, in a folder of its own
compiles :: Group
compiles = Group "CLI: ouro compile"
    [ ("a graph is written to <graph>.json where ouro runs", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "compile-success" [ module' ])
                                                        run <- evalIO (ouro dir ["compile", "m.ouro", "-t", "g"])
                                                        exitCode run === ExitSuccess
                                                        assert ("Compilation Success: m.ouro [g] -> g.json" `T.isInfixOf` plain (out run))
                                                        json <- evalIO (readText (dir </> "g.json"))
                                                        squash json === "{\"r\":1}")
    , ("-o a folder writes <graph>.json in it, made", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "compile-folder" [ module' ])
                                                        run <- evalIO (ouro dir ["compile", "m.ouro", "-t", "g", "-o", "new/dir"])
                                                        exitCode run === ExitSuccess
                                                        json <- evalIO (readText (dir </> "new" </> "dir" </> "g.json"))
                                                        squash json === "{\"r\":1}")
    , ("-o a file writes it",                       withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "compile-file" [ module' ])
                                                        run <- evalIO (ouro dir ["compile", "m.ouro", "-t", "g", "-o", "x.json"])
                                                        exitCode run === ExitSuccess
                                                        json <- evalIO (readText (dir </> "x.json"))
                                                        squash json === "{\"r\":1}")
    , ("a failure writes nothing, its errors on stderr", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "compile-failure" [ module' ])
                                                        run <- evalIO (ouro dir ["compile", "m.ouro", "-t", "nope"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("Compilation Failed: m.ouro due to compiler errors." `T.isInfixOf` plain (out run))
                                                        assert ("Error [SCP0004]" `T.isInfixOf` plain (err run))
                                                        written <- evalIO (doesFileExist (dir </> "nope.json"))
                                                        written === False)
    ]
    where
    module' = ("m.ouro", "(graph g (:r 1))")


-- ouro validate, run as a user runs it, in a folder of its own
validates :: Group
validates = Group "CLI: ouro validate"
    [ ("-o writes the JSON, indented by two",       withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "validate-output" [ json ])
                                                        run <- evalIO (ouro dir ["validate", "in.json", "-o", "formatted.json"])
                                                        exitCode run === ExitSuccess
                                                        assert ("Validated JLD JSON for: in.json" `T.isInfixOf` plain (out run))
                                                        written <- evalIO (readText (dir </> "formatted.json"))
                                                        written === formatted)
    , ("gap 13: without -o it rewrites its input",  withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "validate-in-place" [ json ])
                                                        run <- evalIO (ouro dir ["validate", "in.json"])
                                                        exitCode run === ExitSuccess
                                                        written <- evalIO (readText (dir </> "in.json"))
                                                        written === formatted)
    , ("gap 38: invalid JSON is reported on stdout", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "validate-invalid" [ ("bad.json", "{\"a\":") ])
                                                        run <- evalIO (ouro dir ["validate", "bad.json"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("Compilation Error:" `T.isInfixOf` plain (out run))
                                                        err run === "")
    , ("gap 38: merge is not implemented, on stdout", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "validate-merge" [ json ])
                                                        run <- evalIO (ouro dir ["validate", "in.json", "merge", "in.json"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("Error: 'validate merge' is not implemented yet." `T.isInfixOf` plain (out run))
                                                        err run === "")
    ]
    where
    json      = ("in.json", "{\"a\":1,  \"b\":[1,2]}")
    formatted = "{\n  \"a\": 1,\n  \"b\": [\n    1,\n    2\n  ]\n}\n"


-- What a run of ouro did: its exit code, and what it wrote to stdout and stderr
data Run = Run
    { exitCode :: ExitCode
    , out      :: Text
    , err      :: Text
    }

-- Runs ouro, as built for the tests (build-tool-depends puts it on the PATH), in a folder.
-- Its output is captured in files beside the folder, and read as UTF-8, as ouro writes it.
ouro :: FilePath -> [String] -> IO Run
ouro dir args = do
    code <- withFile (dir <> ".stdout") WriteMode $ \o ->
                withFile (dir <> ".stderr") WriteMode $ \e ->
                    withCreateProcess (proc "ouro" args) { cwd = Just dir, std_out = UseHandle o, std_err = UseHandle e }
                                      (\_ _ _ -> waitForProcess)
    Run code <$> readText (dir <> ".stdout") <*> readText (dir <> ".stderr")

-- Writes the files into a folder of their own, named for the test
writeFolder :: FilePath -> [(FilePath, BS.ByteString)] -> IO FilePath
writeFolder name files = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "ouro-test" </> "cli" </> name
    exists <- doesDirectoryExist dir
    when exists (removeDirectoryRecursive dir)
    createDirectoryIfMissing True dir
    forM_ files $ \(file, bytes) -> do
        createDirectoryIfMissing True (takeDirectory (dir </> file))
        BS.writeFile (dir </> file) bytes
    pure dir

readText :: FilePath -> IO Text
readText file = TE.decodeUtf8 <$> BS.readFile file

-- Output as a user reads it: without its colours, or Windows' carriage returns
plain :: Text -> Text
plain text = case T.splitOn "\ESC[" (T.filter (/= '\r') text) of
                 first : codes -> T.concat (first : map (T.drop 1 . T.dropWhile (not . isAlpha)) codes)
                 []            -> ""

squash :: Text -> Text
squash = T.filter (not . isSpace)
