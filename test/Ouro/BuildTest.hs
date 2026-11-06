{-# LANGUAGE GADTs #-}

module Ouro.BuildTest
( packageFiles
, builds
, projectsOnDisk
, buildsTo
) where

import           CLI.Project                  (Package (..), Project (..),
                                               buildProject, discoverModules,
                                               outputFile, readPackage)
import           Control.Monad                (forM_, when)
import qualified Data.ByteString              as BS
import           Data.Char                    (isSpace)
import           Data.Functor.Identity        (runIdentity)
import qualified Data.Map.Strict              as Map
import           Data.Ouro                    (ErrorContext (..), OuroError (..),
                                               defaultOptions, smartErrorCode,
                                               toJSON)
import           Data.Ouro.Build              (BuildResult (..), buildModules)
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


-- A package.ouro holds one function, (defn package () ...), whose value is checked against
-- the settings' schema, with every setting that is wrong reported
packageFiles :: Group
packageFiles = Group "Builds: package.ouro"
    [ ("no setting is needed",                      readsAs "(defn package () #record-empty)" (".", "out"))
    , ("every setting it acts on",                  readsAs "(defn package () (:sources \"src\" :outputs \"dist\"))" ("src", "dist"))
    , ("settings are Ouro values",                  readsAs "(defn package () (:outputs (+ \"di\" \"st\")))" (".", "dist"))
    , ("it can import the bundled modules",         readsAs "(import std/base)\n(defn package () (:sources \"src\"))" ("src", "out"))
    , ("and only them",                             packageFails "(import helpers)\n(defn package () (:sources \"src\"))" "SCP0006" ["missingModule = \"helpers\""])
    , ("an unknown setting suggests the nearest",   packageFails "(defn package () (:sorces \"src\"))" "BLD0000" ["unknownSetting = \"sorces\"", "Perhaps you meant ':sources'"])
    , (":name is not a setting",                    packageFails "(defn package () (:name \"p\"))" "BLD0000" ["unknownSetting = \"name\""])
    , (":graphs is not acted on yet",               packageFails "(defn package () (:graphs ()))" "BLD0002" ["unsupportedSetting = \"graphs\"", "every graph of every module is built"])
    , (":ouro is not acted on yet",                 packageFails "(defn package () (:ouro \"1.0\"))" "BLD0002" ["unsupportedSetting = \"ouro\""])
    , ("a graph is not package data",               packageFails "(defn package () (:sources \"src\"))\n(graph g (:a 1))" "BLD0006" ["a graph"])
    , ("an export is not package data",             packageFails "(export package)\n(defn package () (:sources \"src\"))" "BLD0006" ["an export"])
    , ("another function is not package data",      packageFails "(defn f () 1)\n(defn package () (:sources \"src\"))" "BLD0006" ["a function"])
    , ("the function is named package",             packageFails "(defn pakage () (:sources \"src\"))" "BLD0007" ["foundFunction = Just \"pakage\"", "perhaps rename it to package"])
    , ("an empty package.ouro is an error",         packageFails "" "BLD0007" ["foundFunction = Nothing", "perhaps delete it"])
    , ("the package function takes no parameters",  packageFails "(defn package (x) (:sources x))" "BLD0008" ["parameterCount = 1"])
    , ("inputs is reserved",                        packageFails "(defn inputs () 1)\n(defn package () (:sources \"src\"))" "BLD0010" ["reservedName = \"inputs\""])
    , ("so are build and exports",                  packageFailsTimes 2 "(defn build () 1)\n(defn exports () 1)\n(defn package () (:sources \"src\"))")
    , ("a setting of the wrong type",               packageFails "(defn package () (:sources 1))" "TYP0000" ["The package setting :sources is a String", "a Number"])
    , (":sources is one folder, not an Array",      packageFails "(defn package () (:sources (\"src\" \"lib\")))" "TYP0000" ["The package setting :sources is a String", "Array"])
    , ("the value is a record of settings",         packageFails "(defn package () \"src\")" "TYP0000" ["a record of settings"])
    , ("every wrong setting is reported",           packageFailsTimes 3 "(defn package () (:sorces \"src\" :outputs 1 :ouro \"1.0\"))")
    , ("every wrong declaration is reported",       packageFailsTimes 3 "(defn f () 1)\n(graph g (:a 1))\n(defn build () 1)\n(defn package () (:sources \"src\"))")
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


-- Projects on disk: a folder, with or without a package.ouro, its source folder, and what
-- is skipped
projectsOnDisk :: Group
projectsOnDisk = Group "Builds: projects on disk"
    [ ("without package.ouro, the folder is the project", projectBuildsTo "no-package" [ ("main.ouro", "(graph g (:r 1))") ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":1}")])
    , ("a project with a source folder",            projectBuildsTo "source-folder" [ package "(:sources \"src\")", helpers "src"
                                                                                    , ("src/main.ouro", "(import helpers)\n(graph g (:r (helpers/greet \"Ann\")))") ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":\"Hello,Ann\"}")])
    , (":outputs is where the graphs are written",  projectBuildsTo "outputs" [ package "(:outputs \"dist\")", ("main.ouro", "(graph g (:r 1))") ]
                                                                    [("dist" </> "main" </> "g.json", "{\"r\":1}")])
    , ("hidden folders and the outputs folder are not", projectBuildsTo "skipped" [ ("main.ouro", "(graph g (:r 1))")
                                                                              , (".jj/broken.ouro", broken), ("out/broken.ouro", broken) ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":1}")])
    , ("nor is a source folder through ..",         projectBuildsTo "dotdot" [ package "(:sources \"src/..\")", ("main.ouro", "(graph g (:r 1))")
                                                                            , ("src/keep.txt", "a folder to go through"), ("out/broken.ouro", broken) ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":1}")])
    , ("nor is package.ouro, as a module",          withTests 1 . property $ do
                                                        dir   <- evalIO (writeProject "dotdot-modules" [ package "(:sources \"src/..\")", ("main.ouro", "(graph g (:r 1))"), ("src/keep.txt", "a folder to go through") ])
                                                        found <- evalIO (discoverModules (Project dir (Package "src/.." "out" (initialPos "package.ouro"))))
                                                        map fst found === ["main"])
    , ("a source folder with a trailing slash is one folder", projectBuildsTo "slash" [ package "(:sources \"src/\")", ("src/main.ouro", "(graph g (:r 1))") ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":1}")])
    , ("a folder with a package.ouro is a project of its own", projectBuildsTo "nested" [ ("main.ouro", "(graph g (:r 1))")
                                                                                       , ("sub/package.ouro", "(defn package () (:sources \"src\"))"), ("sub/src/broken.ouro", broken) ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":1}")])
    , ("a source folder that does not exist",       projectFailsWith "missing-root" [ package "(:sources \"scr\")", ("src/main.ouro", "(graph g (:r 1))") ]
                                                                     "BLD0005" ["sourceFolder = \"scr\""])
    , ("a module under std is reserved",            projectFailsWith "reserved-std" [ ("std/base.ouro", "(graph g (:r 1))"), ("main.ouro", "(graph g (:r 1))") ]
                                                                     "BLD0009" ["reservedPath = \"std/base\"", "rename the folder"])
    , ("so is a module named aat",                  projectFailsWith "reserved-aat" [ ("aat.ouro", "(graph g (:r 1))"), ("main.ouro", "(graph g (:r 1))") ]
                                                                     "BLD0009" ["reservedPath = \"aat\"", "rename the file"])
    , ("a folder that does not exist is not built from above", withTests 1 . property $ do
                                                        dir <- evalIO (writeProject "no-folder" [ package "#record-empty", ("main.ouro", "(graph g (:r 1))") ])
                                                        evalIO (buildProject (dir </> "typo")) >>= \case
                                                            Left message -> assert ("No folder " `T.isPrefixOf` message)
                                                            Right _      -> failure)
    ]
    where
    package settings = ("package.ouro", "(defn package () " <> settings <> ")")
    helpers root     = (root <> "/helpers.ouro", "(export greet)\n(defn greet (n) (+ \"Hello, \" n))")
    broken           = "(graph g (:r (get () 5)))"


-- package.ouro reads as the given source folder and outputs folder
readsAs :: Text -> (FilePath, FilePath) -> Property
readsAs source (sources, outputs) = withTests 1 . property $
    case readPackage "package.ouro" source of
        (_, Right p)   -> (packageSources p, packageOutputs p) === (sources, outputs)
        (_, Left errs) -> annotate (show errs) >> failure

-- package.ouro fails with an error of the given code mentioning every given text
packageFails :: Text -> Text -> [Text] -> Property
packageFails source code messages = withTests 1 . property $
    case readPackage "package.ouro" source of
        (_, Left errs) -> do
                          annotate (show errs)
                          assert (any (\err -> smartErrorCode (errContext err) == code && all (`T.isInfixOf` errorText err) messages) errs)
        (_, Right p)   -> annotate (show p) >> failure

packageFailsTimes :: Int -> Text -> Property
packageFailsTimes times source = withTests 1 . property $
    case readPackage "package.ouro" source of
        (_, Left errs) -> annotate (show errs) >> length errs === times
        (_, Right p)   -> annotate (show p) >> failure


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
