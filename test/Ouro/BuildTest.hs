{-# LANGUAGE GADTs #-}

module Ouro.BuildTest
( buildFiles
, builds
, projectsOnDisk
, buildsTo
, projectBuildsTo
, projectFailsWith
, writeProject
) where

import           Control.Monad                (forM_, when)
import qualified Data.ByteString              as BS
import           Data.Char                    (isSpace)
import           Data.Functor.Identity        (runIdentity)
import qualified Data.Map.Strict              as Map
import           Data.Ouro                    (CompilationResult (..),
                                               ErrorContext (..), OuroError (..),
                                               defaultOptions, readSource,
                                               smartErrorCode, toJSON)
import           Data.Ouro.Build              (BuildFile (..), BuildResult (..),
                                               buildModules, buildProject, outputFile,
                                               compileInProject, findBuildFile,
                                               readBuildFile)
import           Data.Ouro.Lisp.Module.Loader (fromSources)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import           Hedgehog
import           System.Directory             (createDirectoryIfMissing,
                                               doesDirectoryExist,
                                               getTemporaryDirectory,
                                               removeDirectoryRecursive)
import           System.FilePath              (takeDirectory, (</>))
import           Text.Megaparsec              (initialPos)
import           Utils                        (warningText)


-- A build file, build.ouro, is data: one graph named build, checked against the build file's
-- schema, with every setting that is wrong reported
buildFiles :: Group
buildFiles = Group "Builds: the build file"
    [ ("a name is all a build file needs",          readsAs "(graph build (:name \"p\"))" ("p", ["."], "out"))
    , ("every setting it acts on",                  readsAs "(graph build (:name \"p\" :sources (\"src\" \"lib\") :out \"dist\"))" ("p", ["src", "lib"], "dist"))
    , ("settings are Ouro values",                  readsAs "(graph build (:name \"p\" :out (+ \"di\" \"st\")))" ("p", ["."], "dist"))
    , ("an unknown setting suggests the nearest",   buildFileFails "(graph build (:name \"p\" :sorces (\"src\")))" "BLD0000" ["unknownSetting = \"sorces\"", "Perhaps you meant ':sources'"])
    , ("a name is required",                        buildFileFails "(graph build (:out \"dist\"))" "BLD0001" ["missingSetting = \"name\""])
    , (":depends is not acted on yet",              buildFileFails "(graph build (:name \"p\" :depends ()))" "BLD0002" ["unsupportedSetting = \"depends\"", "copying it under one of the :sources"])
    , (":graphs is not acted on yet",               buildFileFails "(graph build (:name \"p\" :graphs ()))" "BLD0002" ["unsupportedSetting = \"graphs\"", "every graph of every module is built"])
    , ("an import is not data",                     buildFileFails "(import std/base)\n(graph build (:name \"p\"))" "BLD0003" ["an import"])
    , ("an export is not data",                     buildFileFails "(export f)\n(defn f () 1)\n(graph build (:name \"p\"))" "BLD0003" ["an export"])
    , ("a function is not data",                    buildFileFails "(defn f () 1)\n(graph build (:name \"p\"))" "BLD0003" ["a function"])
    , ("a second graph is not data",                buildFileFails "(graph build (:name \"p\"))\n(graph other (:a 1))" "BLD0003" ["a second graph"])
    , ("the graph is named build",                  buildFileFails "(graph biuld (:name \"p\"))" "BLD0004" ["foundGraph = Just \"biuld\"", "perhaps rename it to build"])
    , ("an empty build file has no graph",          buildFileFails "" "BLD0004" ["foundGraph = Nothing"])
    , ("a setting of the wrong type",               buildFileFails "(graph build (:name 1))" "TYP0000" ["The build setting :name is a String", "a Number"])
    , (":sources is an Array, even of one",         buildFileFails "(graph build (:name \"p\" :sources \"src\"))" "TYP0000" ["even when it names one"])
    , ("each of the sources is a String",           buildFileFails "(graph build (:name \"p\" :sources (\"src\" 2)))" "TYP0000" [":sources[1] is a String"])
    , ("the graph is a record of settings",         buildFileFails "(graph build (\"p\"))" "TYP0000" ["a record of settings"])
    , ("every wrong setting is reported",           buildFileFailsTimes 3 "(graph build (:sorces () :out 1))")
    , ("every wrong declaration is reported",       buildFileFailsTimes 3 "(import std/base)\n(defn f () 1)\n(graph build (:name \"p\"))\n(graph other (:a 1))")
    ]


-- Every graph of every module, written to out/<module>/<graph>.json, or nothing at all
builds :: Group
builds = Group "Builds: every graph of every module"
    [ ("every graph, by module and name",           buildsTo [("a", "(graph x (:r 1))\n(graph y (:r 2))"), ("lib/b", "(graph z (:r 3))")]
                                                             [("out" </> "a" </> "x.json", "{\"r\":1}"), ("out" </> "a" </> "y.json", "{\"r\":2}"), ("out" </> "lib" </> "b" </> "z.json", "{\"r\":3}")])
    , ("a module of functions writes nothing",      buildsTo [("helpers", "(export f)\n(defn f () 1)"), ("main", "(import helpers)\n(graph g (:r (helpers/f)))")]
                                                             [("out" </> "main" </> "g.json", "{\"r\":1}")])
    , ("one failing graph fails the build",         buildFailsWith [("good", "(graph g (:r 1))"), ("bad", "(graph g (:r (get () 5)))")] ["attemptedIndex = 5"])
    , ("every failing module is reported",          buildFailsTimes 2 [("one", "(graph g (:r (get () 5)))"), ("two", "(graph g (:r (get () 6)))"), ("fine", "(graph g (:r 1))")])
    , ("a module imported by many warns once",      buildWarnsTimes 1 "badName" [("badly-named", "(export badName)\n(defn badName () 1)"), ("one", "(import badly-named)\n(graph g (:r (badly-named/badName)))"), ("two", "(import badly-named)\n(graph g (:r (badly-named/badName)))")])
    , ("an import cycle among modules",             buildFailsWith [("a", "(import b)\n(export f)\n(defn f () 1)"), ("b", "(import a)\n(export f)\n(defn f () 2)")] ["ImportCycle"])
    ]


-- Projects on disk: the build file found from a folder or above it, its source roots, and
-- what is skipped
projectsOnDisk :: Group
projectsOnDisk = Group "Builds: projects on disk"
    [ ("a project with two source roots",           projectBuildsTo "roots" [ build "(:name \"p\" :sources (\"src\" \"lib\"))", helpers "lib"
                                                                            , ("src/main.ouro", "(import helpers)\n(graph g (:r (helpers/greet \"Ann\")))") ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":\"Hello,Ann\"}")])
    , ("without :sources, the build file's folder",  projectBuildsTo "default-root" [ build "(:name \"p\")", ("main.ouro", "(graph g (:r 1))") ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":1}")])
    , ("hidden folders and the out folder are not", projectBuildsTo "skipped" [ build "(:name \"p\")", ("main.ouro", "(graph g (:r 1))")
                                                                              , (".jj/broken.ouro", "(graph g (:r (get () 5)))"), ("out/broken.ouro", "(graph g (:r (get () 5)))") ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":1}")])
    , ("a module in two roots is ambiguous",        projectFailsWith "ambiguous" [ build "(:name \"p\" :sources (\"src\" \"lib\"))", helpers "src", helpers "lib" ]
                                                                     "SCP0011" ["ambiguousPath = \"helpers\""])
    , ("an import of it is too",                    projectFailsWith "ambiguous-import" [ build "(:name \"p\" :sources (\"src\" \"lib\" \"app\"))", helpers "src", helpers "lib"
                                                                                        , ("app/main.ouro", "(import helpers)\n(graph g (:r (helpers/greet \"Ann\")))") ]
                                                                     "SCP0011" ["The module 'helpers' is in more than one source root"])
    , ("a source folder that does not exist",       projectFailsWith "missing-root" [ build "(:name \"p\" :sources (\"src\" \"scr\"))", ("src/main.ouro", "(graph g (:r 1))") ]
                                                                     "BLD0005" ["sourceFolder = \"scr\""])
    , ("the build file is found from below it",     withTests 1 . property $ do
                                                        dir   <- evalIO (writeProject "below" [ build "(:name \"p\")", ("src/deep/x.ouro", "(graph g (:r 1))") ])
                                                        found <- evalIO (findBuildFile (dir </> "src" </> "deep"))
                                                        fmap takeDirectory found === Just dir)
    , ("a compile reads the build's :sources",      withTests 1 . property $ do
                                                        dir    <- evalIO (writeProject "compile" [ build "(:name \"p\" :sources (\"lib\"))", helpers "lib"
                                                                                                 , ("main.ouro", "(import helpers)\n(graph g (:r (helpers/greet \"Ann\")))") ])
                                                        source <- evalIO (either (error . T.unpack) id <$> readSource (dir </> "main.ouro"))
                                                        evalIO (compileInProject (dir </> "main.ouro") "g" source) >>= \case
                                                            Right (CompilationSuccess _ ast) -> squash (TL.toStrict (toJSON defaultOptions ast)) === "{\"r\":\"Hello,Ann\"}"
                                                            Right (CompilationFailure _ errs) -> annotate (show errs) >> failure
                                                            Left message -> annotate (T.unpack message) >> failure)
    , ("a build file is looked for, or it says so", withTests 1 . property $ do
                                                        dir <- evalIO (writeProject "no-build-file" [ ("main.ouro", "(graph g (:r 1))") ])
                                                        evalIO (buildProject dir) >>= \case
                                                            Left message -> assert ("No build.ouro in " `T.isPrefixOf` message)
                                                            Right _      -> failure)
    ]
    where
    build settings = ("build.ouro", "(graph build " <> settings <> ")")
    helpers root   = (root <> "/helpers.ouro", "(export greet)\n(defn greet (n) (+ \"Hello, \" n))")


-- The build file reads as the given name, sources and out folder
readsAs :: Text -> (Text, [FilePath], FilePath) -> Property
readsAs source (name, sources, out) = withTests 1 . property $
    case readBuildFile "build.ouro" source of
        (_, Right b)   -> (buildName b, buildSources b, buildOut b) === (name, sources, out)
        (_, Left errs) -> annotate (show errs) >> failure

-- The build file fails with an error of the given code mentioning every given text
buildFileFails :: Text -> Text -> [Text] -> Property
buildFileFails source code messages = withTests 1 . property $
    case readBuildFile "build.ouro" source of
        (_, Left errs) -> do
                          annotate (show errs)
                          assert (any (\err -> smartErrorCode (errContext err) == code && all (`T.isInfixOf` errorText err) messages) errs)
        (_, Right b)   -> annotate (show b) >> failure

buildFileFailsTimes :: Int -> Text -> Property
buildFileFailsTimes times source = withTests 1 . property $
    case readBuildFile "build.ouro" source of
        (_, Left errs) -> annotate (show errs) >> length errs === times
        (_, Right b)   -> annotate (show b) >> failure


-- Modules in memory, each built: (path, source). Each graph is named by the file a project
-- would write it to, in out.
buildInMemory :: [(Text, Text)] -> BuildResult FilePath
buildInMemory modules = uncurry (outputFile "out")
                        <$> runIdentity (buildModules (fromSources (Map.fromList modules)) [ (initialPos (T.unpack path <> ".ouro"), path) | (path, _) <- modules ])

buildsTo :: [(Text, Text)] -> [(FilePath, Text)] -> Property
buildsTo modules expected = withTests 1 . property $ outputsAre expected (buildInMemory modules)

buildFailsWith :: [(Text, Text)] -> [Text] -> Property
buildFailsWith modules messages = withTests 1 . property $
    case buildInMemory modules of
        BuildFailure _ errs -> annotate (show errs) >> assert (any (\err -> all (`T.isInfixOf` errorText err) messages) errs)
        BuildSuccess _ outs -> annotate (show (map fst outs)) >> failure

buildFailsTimes :: Int -> [(Text, Text)] -> Property
buildFailsTimes times modules = withTests 1 . property $
    case buildInMemory modules of
        BuildFailure _ errs -> annotate (show errs) >> length errs === times
        BuildSuccess _ outs -> annotate (show (map fst outs)) >> failure

buildWarnsTimes :: Int -> Text -> [(Text, Text)] -> Property
buildWarnsTimes times message modules = withTests 1 . property $
    case buildInMemory modules of
        BuildSuccess warnings _ -> annotate (show warnings) >> length (filter (T.isInfixOf message . warningText) warnings) === times
        BuildFailure _ errs     -> annotate (show errs) >> failure


-- Writes the files into a directory of their own, named for the test
writeProject :: FilePath -> [(FilePath, BS.ByteString)] -> IO FilePath
writeProject name files = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "ouro-test" </> "build" </> name
    exists <- doesDirectoryExist dir
    when exists (removeDirectoryRecursive dir)
    createDirectoryIfMissing True dir
    forM_ files $ \(file, bytes) -> do
        createDirectoryIfMissing True (takeDirectory (dir </> file))
        BS.writeFile (dir </> file) bytes
    pure dir

-- The project builds to the given files, named relative to the project's directory
projectBuildsTo :: FilePath -> [(FilePath, BS.ByteString)] -> [(FilePath, Text)] -> Property
projectBuildsTo name files expected = withTests 1 . property $ do
    dir <- evalIO (writeProject name files)
    evalIO (buildProject dir) >>= \case
        Right result -> outputsAre [ (dir </> file, json) | (file, json) <- expected ] result
        Left message -> annotate (T.unpack message) >> failure

projectFailsWith :: FilePath -> [(FilePath, BS.ByteString)] -> Text -> [Text] -> Property
projectFailsWith name files code messages = withTests 1 . property $ do
    dir <- evalIO (writeProject name files)
    evalIO (buildProject dir) >>= \case
        Right (BuildFailure _ errs) -> do
                                       annotate (show errs)
                                       assert (any (\err -> smartErrorCode (errContext err) == code && all (`T.isInfixOf` errorText err) messages) errs)
        Right (BuildSuccess _ outs) -> annotate (show (map fst outs)) >> failure
        Left message                -> annotate (T.unpack message) >> failure


-- The build's files and their JSON, whitespace ignored
outputsAre :: [(FilePath, Text)] -> BuildResult FilePath -> PropertyT IO ()
outputsAre expected = \case
    BuildSuccess _ outputs -> [ (file, squash (TL.toStrict (toJSON defaultOptions ast))) | (file, ast) <- outputs ] === [ (file, squash json) | (file, json) <- expected ]
    BuildFailure _ errs    -> annotate (show errs) >> failure

squash :: Text -> Text
squash = T.filter (not . isSpace)

-- An error's text, with its blurb, unescaped: show would double a Windows path's backslashes
errorText :: OuroError -> Text
errorText (OuroError _ context) = contextText context
    where
    contextText = \case
        AnnotatedContext blurb inner -> blurb <> "\n" <> contextText inner
        other                        -> T.pack (show other)
