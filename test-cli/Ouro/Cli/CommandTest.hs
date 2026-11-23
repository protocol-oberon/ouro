module Ouro.Cli.CommandTest
( builds
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
import           Utils              (fixedGap)


-- ouro build, run as a user runs it, in a project of its own
builds :: Group
builds = Group "CLI: ouro build"
    [ ("every graph is written, and counted",       withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-success" [ ("main.ouro", "(document g (:r 1))") ])
                                                        run <- evalIO (ouro dir ["build"])
                                                        exitCode run === ExitSuccess
                                                        assert ("Build Success:       1 document" `T.isInfixOf` plain (out run))
                                                        json <- evalIO (readText (dir </> "out" </> "main" </> "g.json"))
                                                        squash json === "{\"r\":1}")
    , ("a failure writes nothing, its errors on stderr", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-failure" [ ("main.ouro", "(document g (:r (get () 5)))") ])
                                                        run <- evalIO (ouro dir ["build"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("Build Failed: nothing was written, due to compiler errors." `T.isInfixOf` plain (out run))
                                                        assert ("Error [" `T.isInfixOf` plain (err run))
                                                        assert (not ("Error [" `T.isInfixOf` plain (out run)))
                                                        written <- evalIO (doesDirectoryExist (dir </> "out"))
                                                        written === False)
    , ("package.ouro's settings are used",          withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-package" [ ("package.ouro", "(define/constant package (:sources \"src\" :outputs \"dist\"))"), ("src/main.ouro", "(document g (:r 1))") ])
                                                        run <- evalIO (ouro dir ["build"])
                                                        exitCode run === ExitSuccess
                                                        json <- evalIO (readText (dir </> "dist" </> "main" </> "g.json"))
                                                        squash json === "{\"r\":1}")
    , ("a folder that does not exist, on stderr",   withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-no-folder" [ ("main.ouro", "(document g (:r 1))") ])
                                                        run <- evalIO (ouro dir ["build", "typo"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("No folder typo" `T.isInfixOf` plain (err run))
                                                        written <- evalIO (doesDirectoryExist (dir </> "out"))
                                                        written === False)
    , ("-t builds only the graphs named",           withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-chosen" [ ("a.ouro", "(document x (:r 1))\n(document y (:r 2))"), ("b.ouro", "(document g (:r (get () 5)))") ])
                                                        run <- evalIO (ouro dir ["build", "-t", "a/y"])
                                                        exitCode run === ExitSuccess
                                                        assert ("Build Success:       1 document" `T.isInfixOf` plain (out run))
                                                        written <- evalIO (traverse doesFileExist [dir </> "out" </> "a" </> "y.json", dir </> "out" </> "a" </> "x.json"])
                                                        written === [True, False])
    , ("-g, the old -t, still chooses, with a warning", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-chosen-old" [ ("a.ouro", "(document x (:r 1))\n(document y (:r 2))") ])
                                                        run <- evalIO (ouro dir ["build", "-g", "a/y"])
                                                        exitCode run === ExitSuccess
                                                        assert ("'-g' is now written '-t'" `T.isInfixOf` plain (err run))
                                                        written <- evalIO (traverse doesFileExist [dir </> "out" </> "a" </> "y.json", dir </> "out" </> "a" </> "x.json"])
                                                        written === [True, False])
    , ("-t not written module/document is named, without a line", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-chosen-form" [ ("a.ouro", "(document x (:r 1))") ])
                                                        run <- evalIO (ouro dir ["build", "-t", "ouro-mode"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("Error [BLD0018]" `T.isInfixOf` plain (err run))
                                                        assert ("│-t ouro-mode│" `T.isInfixOf` plain (err run))
                                                        assert (not ("-t ouro-mode:1:1" `T.isInfixOf` plain (err run))))
    , ("-g is not in the help",                     withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-help" [])
                                                        run <- evalIO (ouro dir ["build", "-h"])
                                                        assert ("--target" `T.isInfixOf` plain (out run))
                                                        assert (not ("--graph" `T.isInfixOf` plain (out run))))
    , ("-t with a graph that does not exist suggests the closest", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-chosen-graph" [ ("a.ouro", "(document person (:r 1))") ])
                                                        run <- evalIO (ouro dir ["build", "-t", "a/persn"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("Error [SCP0004]" `T.isInfixOf` plain (err run))
                                                        assert ("'person'" `T.isInfixOf` plain (err run)))
    , ("-t with a module that does not exist suggests the closest", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-chosen-module" [ ("people.ouro", "(document person (:r 1))") ])
                                                        run <- evalIO (ouro dir ["build", "-t", "peopel/person"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("Error [BLD0017]" `T.isInfixOf` plain (err run))
                                                        assert ("-t peopel/person" `T.isInfixOf` plain (err run))
                                                        assert ("Perhaps you meant 'people'" `T.isInfixOf` plain (err run)))
    , ("ouro compile is no longer a command",       withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "compile-gone" [ ("m.ouro", "(document g (:r 1))") ])
                                                        run <- evalIO (ouro dir ["compile", "m.ouro", "-t", "g"])
                                                        exitCode run === ExitFailure 1
                                                        written <- evalIO (doesFileExist (dir </> "g.json"))
                                                        written === False)
    , ("gap 27: a renamed graph's old output is left", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-stale" [ ("main.ouro", "(document g (:r 1))") ])
                                                        _   <- evalIO (ouro dir ["build"])
                                                        evalIO (BS.writeFile (dir </> "main.ouro") "(document h (:r 1))")
                                                        run <- evalIO (ouro dir ["build"])
                                                        exitCode run === ExitSuccess
                                                        both <- evalIO (traverse (doesFileExist . (\graph -> dir </> "out" </> "main" </> graph)) ["g.json", "h.json"])
                                                        both === [True, True])
    , ("gap 35 (fixed): a build writes until a write fails", fixedGap . withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-partial" [ ("a.ouro", "(document g (:r 1))"), ("b.ouro", "(document g (:r 2))"), ("out/b", "a file where a folder goes") ])
                                                        run <- evalIO (ouro dir ["build"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("already exists" `T.isInfixOf` plain (err run))
                                                        assert (not (any (`T.isInfixOf` plain (out run)) ["Build Success", "Build Failed"]))
                                                        written <- evalIO (doesFileExist (dir </> "out" </> "a" </> "g.json"))
                                                        written === True)
    , ("a write that fails leaves nothing written", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-all-or-nothing" [ ("a.ouro", "(document g (:r 1))"), ("b.ouro", "(document g (:r 2))"), ("out/b", "a file where a folder goes") ])
                                                        run <- evalIO (ouro dir ["build"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("Could not write" `T.isInfixOf` plain (err run))
                                                        assert ("Build Failed: a document could not be written, so none was." `T.isInfixOf` plain (out run))
                                                        written <- evalIO (doesFileExist (dir </> "out" </> "a" </> "g.json"))
                                                        staged  <- evalIO (doesFileExist (dir </> "out" </> "a" </> "g.json.partial"))
                                                        (written, staged) === (False, False))
    , ("and a build that succeeds leaves no staged file", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-no-partial" [ ("a.ouro", "(document g (:r 1))") ])
                                                        run <- evalIO (ouro dir ["build"])
                                                        exitCode run === ExitSuccess
                                                        written <- evalIO (doesFileExist (dir </> "out" </> "a" </> "g.json"))
                                                        staged  <- evalIO (doesFileExist (dir </> "out" </> "a" </> "g.json.partial"))
                                                        (written, staged) === (True, False))
    , ("gap 37 (fixed): colours are written when the output is not a terminal", fixedGap . withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-colours" [ ("main.ouro", "(document g (:r (get () 5)))") ])
                                                        run <- evalIO (ouro dir ["build"])
                                                        assert ("\ESC[" `T.isInfixOf` err run))
    , ("a diagnostic written to a file is plain text", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "build-plain" [ ("main.ouro", "(document g (:r (get () 5)))") ])
                                                        run <- evalIO (ouro dir ["build"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("Error [" `T.isInfixOf` err run)
                                                        assert (not ("\ESC" `T.isInfixOf` err run)))
    ]


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
    , ("without -o it writes the JSON to stdout, and nothing else", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "validate-stdout" [ json ])
                                                        run <- evalIO (ouro dir ["validate", "in.json"])
                                                        exitCode run === ExitSuccess
                                                        out run === formatted
                                                        input <- evalIO (readText (dir </> "in.json"))
                                                        input === TE.decodeUtf8 (snd json))
    , ("gap 13 (fixed): without -o it rewrites its input", fixedGap . withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "validate-in-place" [ json ])
                                                        run <- evalIO (ouro dir ["validate", "in.json"])
                                                        exitCode run === ExitSuccess
                                                        written <- evalIO (readText (dir </> "in.json"))
                                                        written === formatted)
    , ("gap 38 (fixed): invalid JSON is reported on stdout", fixedGap . withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "validate-invalid" [ ("bad.json", "{\"a\":") ])
                                                        run <- evalIO (ouro dir ["validate", "bad.json"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("Compilation Error:" `T.isInfixOf` plain (out run))
                                                        err run === "")
    , ("gap 38 (fixed): merge is not implemented, on stdout", fixedGap . withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "validate-merge" [ json ])
                                                        run <- evalIO (ouro dir ["validate", "in.json", "merge", "in.json"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("Error: 'validate merge' is not implemented yet." `T.isInfixOf` plain (out run))
                                                        err run === "")
    , ("invalid JSON is reported on stderr, and stdout is empty", withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "validate-invalid-err" [ ("bad.json", "{\"a\":") ])
                                                        run <- evalIO (ouro dir ["validate", "bad.json"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("Compilation Error:" `T.isInfixOf` plain (err run))
                                                        out run === "")
    , ("and so is merge, not implemented",          withTests 1 . property $ do
                                                        dir <- evalIO (writeFolder "validate-merge-err" [ json ])
                                                        run <- evalIO (ouro dir ["validate", "in.json", "merge", "in.json"])
                                                        exitCode run === ExitFailure 1
                                                        assert ("Error: 'validate merge' is not implemented yet." `T.isInfixOf` plain (err run))
                                                        out run === "")
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
