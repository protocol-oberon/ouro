{-# LANGUAGE GADTs #-}

-- The command line's project code, tested from inside: package.ouro, projects, vendored
-- libraries and modules on disk, source files read from disk, and the gaps that live there
module Ouro.Cli.ProjectTest
( packageFiles
, projectsOnDisk
, vendoredLibraries
, chosenGraphs
, modulesOnDisk
, sourceFiles
, knownGaps
) where

import           CLI.Project                  (Package (..), Project (..),
                                               buildProject, compileFile,
                                               defaultPackage, discoverModules,
                                               ouroVersion, outputFile,
                                               readPackage)
import           CLI.Source                   (readSource)
import           Control.Monad                (forM_, when)
import qualified Data.ByteString              as BS
import           Data.Foldable                (toList)
import           Data.Functor.Identity        (runIdentity)
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.Map.Strict              as Map
import           Data.Ouro                    (CompilationResult (..),
                                               OuroError (..), defaultOptions,
                                               smartErrorCode, toJSON)
import           Data.Ouro.Build              (BuildResult (..), Graphs (..),
                                               buildModules)
import           Data.Ouro.Lisp.Module.Loader (fromSources)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import           Hedgehog
import           System.Directory             (createDirectoryIfMissing,
                                               doesDirectoryExist,
                                               getTemporaryDirectory,
                                               removeDirectoryRecursive)
import           System.FilePath              (normalise, takeDirectory, (</>))
import           System.Info                  (os)
import           Text.Megaparsec              (initialPos, sourceColumn, sourceLine,
                                               sourceName, unPos)
import           Utils                        (describe, errorText, fixedGap,
                                               fixture, outputsAre, squash,
                                               warningText)


-- A package.ouro holds one function, (define/constant package ...), whose value is checked against
-- the settings' schema, with every setting that is wrong reported
packageFiles :: Group
packageFiles = Group "Builds: package.ouro"
    [ ("no setting is needed",                      readsAs "(define/constant package #record-empty)" (".", "out"))
    , ("every setting it acts on",                  readsAs "(define/constant package (:sources \"src\" :outputs \"dist\"))" ("src", "dist"))
    , ("settings are Ouro values",                  readsAs "(define/constant package (:outputs (+ \"di\" \"st\")))" (".", "dist"))
    , ("it can import the bundled modules",         readsAs "(import std/base)\n(define/constant package (:sources \"src\"))" ("src", "out"))
    , ("and only them",                             packageFails "(import helpers)\n(define/constant package (:sources \"src\"))" "SCP0006" ["missingModule = \"helpers\""])
    , ("an unknown setting suggests the nearest",   packageFails "(define/constant package (:sorces \"src\"))" "BLD0000" ["unknownSetting = \"sorces\"", "Perhaps you meant ':sources'"])
    , (":name is not a setting",                    packageFails "(define/constant package (:name \"p\"))" "BLD0000" ["unknownSetting = \"name\""])
    , (":documents chooses the graphs built",          readsGraphs "(define/constant package (:documents (\"people/person\" \"places/tate\")))" (Just ["people/person", "places/tate"]))
    , ("without it, every graph is",                readsGraphs "(define/constant package (:sources \"src\"))" Nothing)
    , (":documents is an Array, even of one",          packageFails "(define/constant package (:documents \"people/person\"))" "TYP0000" ["even when it names one"])
    , (":ouro is the version needed",               readsAs "(define/constant package (:ouro \"0.5\"))" (".", "out"))
    , ("no newer than this compiler",               packageFails "(define/constant package (:ouro \"99.0\"))" "BLD0015" ["neededOuro = \"99.0\"", "compilerOuro = \"" <> ouroVersion <> "\""])
    , ("written as numbers apart by dots",          packageFails "(define/constant package (:ouro \"latest\"))" "BLD0016" ["writtenVersion = \"latest\""])
    , ("a document is not package data",              packageFails "(define/constant package (:sources \"src\"))\n(document g (:a 1))" "BLD0006" ["a document"])
    , ("an export is not package data",             packageFails "(export package)\n(define/constant package (:sources \"src\"))" "BLD0006" ["an export"])
    , ("another function is not package data",      packageFails "(define/procedure f () 1)\n(define/constant package (:sources \"src\"))" "BLD0006" ["a function"])
    , ("the function is named package",             packageFails "(define/procedure pakage () (:sources \"src\"))" "BLD0007" ["foundFunction = Just \"pakage\"", "perhaps rename it to package"])
    , ("an empty package.ouro is an error",         packageFails "" "BLD0007" ["foundFunction = Nothing", "perhaps delete it"])
    , ("the package function takes no parameters",  packageFails "(define/procedure package (x) (:sources x))" "BLD0008" ["parameterCount = 1"])
    , ("inputs is reserved",                        packageFails "(define/procedure inputs () 1)\n(define/constant package (:sources \"src\"))" "BLD0010" ["reservedName = \"inputs\""])
    , ("so are build and exports",                  packageFailsTimes 2 "(define/procedure build () 1)\n(define/procedure exports () 1)\n(define/constant package (:sources \"src\"))")
    , ("a setting of the wrong type",               packageFails "(define/constant package (:sources 1))" "TYP0000" ["The package setting :sources is a String", "a Number"])
    , (":sources is one folder, not an Array",      packageFails "(define/constant package (:sources (\"src\" \"lib\")))" "TYP0000" ["The package setting :sources is a String", "Array"])
    , ("the value is a record of settings",         packageFails "(define/constant package \"src\")" "TYP0000" ["a record of settings"])
    , ("every wrong setting is reported",           packageFailsTimes 3 "(define/constant package (:sorces \"src\" :outputs 1 :ouro \"1.0\"))")
    , ("every wrong declaration is reported",       packageFailsTimes 3 "(define/procedure f () 1)\n(document g (:a 1))\n(define/procedure build () 1)\n(define/constant package (:sources \"src\"))")
    ]


-- Projects on disk: a folder, with or without a package.ouro, its source folder, and what
-- is skipped
projectsOnDisk :: Group
projectsOnDisk = Group "Builds: projects on disk"
    [ ("without package.ouro, the folder is the project", projectBuildsTo "no-package" [ ("main.ouro", "(document g (:r 1))") ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":1}")])
    , ("a project with a source folder",            projectBuildsTo "source-folder" [ package "(:sources \"src\")", helpers "src"
                                                                                    , ("src/main.ouro", "(import helpers)\n(document g (:r (helpers/greet \"Ann\")))") ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":\"Hello,Ann\"}")])
    , (":outputs is where the graphs are written",  projectBuildsTo "outputs" [ package "(:outputs \"dist\")", ("main.ouro", "(document g (:r 1))") ]
                                                                    [("dist" </> "main" </> "g.json", "{\"r\":1}")])
    , ("hidden folders and the outputs folder are not", projectBuildsTo "skipped" [ ("main.ouro", "(document g (:r 1))")
                                                                              , (".jj/broken.ouro", broken), ("out/broken.ouro", broken) ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":1}")])
    , ("nor is a source folder through ..",         projectBuildsTo "dotdot" [ package "(:sources \"src/..\")", ("main.ouro", "(document g (:r 1))")
                                                                            , ("src/keep.txt", "a folder to go through"), ("out/broken.ouro", broken) ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":1}")])
    , ("nor is package.ouro, as a module",          withTests 1 . property $ do
                                                        dir   <- evalIO (writeProject "dotdot-modules" [ package "(:sources \"src/..\")", ("main.ouro", "(document g (:r 1))"), ("src/keep.txt", "a folder to go through") ])
                                                        found <- evalIO (discoverModules (Project dir (Package "src/.." "out" Nothing (initialPos "package.ouro"))))
                                                        map fst found === ["main"])
    , ("a source folder with a trailing slash is one folder", projectBuildsTo "slash" [ package "(:sources \"src/\")", ("src/main.ouro", "(document g (:r 1))") ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":1}")])
    , ("a folder with a package.ouro is a project of its own", projectBuildsTo "nested" [ ("main.ouro", "(document g (:r 1))")
                                                                                       , ("sub/package.ouro", "(define/constant package (:sources \"src\"))"), ("sub/src/broken.ouro", broken) ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":1}")])
    , ("a source folder that does not exist",       projectFailsWith "missing-root" [ package "(:sources \"scr\")", ("src/main.ouro", "(document g (:r 1))") ]
                                                                     "BLD0005" ["sourceFolder = \"scr\""])
    , ("a module under std is reserved",            projectFailsWith "reserved-std" [ ("std/base.ouro", "(document g (:r 1))"), ("main.ouro", "(document g (:r 1))") ]
                                                                     "BLD0009" ["reservedPath = \"std/base\"", "rename the folder"])
    , ("so is a module named aat",                  projectFailsWith "reserved-aat" [ ("aat.ouro", "(document g (:r 1))"), ("main.ouro", "(document g (:r 1))") ]
                                                                     "BLD0009" ["reservedPath = \"aat\"", "rename the file"])
    , ("a folder that does not exist is not built from above", withTests 1 . property $ do
                                                        dir <- evalIO (writeProject "no-folder" [ package "#record-empty", ("main.ouro", "(document g (:r 1))") ])
                                                        evalIO (buildProject (dir </> "typo") []) >>= \case
                                                            Left message -> assert ("No folder " `T.isPrefixOf` message)
                                                            Right _      -> failure)

      -- A graph is written to a file named after it, so its name must be a file name everywhere
    , ("a graph's name cannot hold '?'",           projectFailsWith "graph-name-char" [("main.ouro", "(document a? (:r 1))")] "BLD0019" ["holds '?'"])
    , ("nor ':'",                                  projectFailsWith "graph-name-colon" [("main.ouro", "(document ex:g (:r 1))")] "BLD0019" ["holds ':'"])
    , ("nor be a device's name, in any case",      projectFailsWith "graph-name-device" [("main.ouro", "(document Con (:r 1))")] "BLD0019" ["Con", "device"])
    , ("a name that only begins so is a name",     projectBuildsTo "graph-name-console" [("main.ouro", "(document console (:r 1))")] [("out" </> "main" </> "console.json", "{\"r\": 1}")])
    , ("graphs apart only by case share a file",   projectFailsWith "graph-name-case" [("main.ouro", "(document G (:r 1))\n(document g (:r 2))")] "BLD0020" ["g", "G"])
    , ("but graphs of two modules do not",         projectBuildsTo "graph-name-modules" [("a.ouro", "(document g (:r 1))"), ("b.ouro", "(document G (:r 2))")]
                                                                   [("out" </> "a" </> "g.json", "{\"r\": 1}"), ("out" </> "b" </> "G.json", "{\"r\": 2}")])
    , ("nothing is written when one cannot be",    withTests 1 . property $ do
                                                        dir <- evalIO (writeProject "graph-name-nothing" [("main.ouro", "(document good (:r 1))\n(document con (:r 2))")])
                                                        _   <- evalIO (buildProject dir [])
                                                        written <- evalIO (doesDirectoryExist (dir </> "out"))
                                                        written === False)
    ]
    where
    package settings = ("package.ouro", "(define/constant package " <> settings <> ")")
    helpers root     = (root <> "/helpers.ouro", "(export greet)\n(define/procedure greet (n) (+ \"Hello, \" n))")
    broken           = "(document g (:r (get () 5)))"


-- Vendored libraries: each folder of vendor/ a collection, imported by its name, its modules
-- read from its own source folder and compiled only when imported
vendoredLibraries :: Group
vendoredLibraries = Group "Builds: vendored libraries"
    [ ("a collection's module is imported by its name", projectBuildsTo "vendor-import" [ person "vendor/tate/people.ouro", ("main.ouro", "(import tate/people)\n(document g (:r (people/person \"A\")))") ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":\"Person:A\"}")])
    , ("a library's own imports are its own",       projectBuildsTo "vendor-own" [ ("vendor/tate/people.ouro", "(import helpers)\n(export person)\n(define/procedure person () (helpers/who))")
                                                                                 , ("vendor/tate/helpers.ouro", "(export who)\n(define/procedure who () \"tate\")")
                                                                                 , ("helpers.ouro", "(export who)\n(define/procedure who () \"project\")")
                                                                                 , ("main.ouro", "(import tate/people)\n(import helpers)\n(document g (:r (people/person) :s (helpers/who)))") ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":\"tate\",\"s\":\"project\"}")])
    , ("its package.ouro gives its source folder",  projectBuildsTo "vendor-sources" [ ("vendor/tate/package.ouro", "(define/constant package (:sources \"src\"))"), person "vendor/tate/src/people.ouro"
                                                                                     , ("main.ouro", "(import tate/people)\n(document g (:r (people/person \"A\")))") ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":\"Person:A\"}")])
    , ("a library imports another collection",      projectBuildsTo "vendor-other" [ ("vendor/a/x.ouro", "(import b/y)\n(export f)\n(define/procedure f () (y/g))"), ("vendor/b/y.ouro", "(export g)\n(define/procedure g () \"from b\")")
                                                                                   , ("main.ouro", "(import a/x)\n(document g (:r (x/f)))") ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":\"fromb\"}")])
    , ("its modules are compiled only when imported", projectBuildsTo "vendor-lazy" [ ("vendor/tate/people.ouro", "(export person)\n(define/procedure person (n) n)\n(document g (:r 1))"), ("vendor/tate/broken.ouro", broken)
                                                                                   , ("main.ouro", "(import tate/people)\n(document g (:r (people/person \"A\")))") ]
                                                                    [("out" </> "main" </> "g.json", "{\"r\":\"A\"}")])
    , ("a module reached two ways is loaded once",  projectWarnsTimes "vendor-once" 1 "badName" [ ("vendor/tate/badly.ouro", "(export badName)\n(define/constant badName 1)")
                                                                                              , ("one.ouro", "(import tate/badly)\n(document g (:r (badly/badName)))")
                                                                                              , ("two.ouro", "(import tate/badly)\n(document g (:r (badly/badName)))") ])
    , ("a missing module says where it looked",     projectFailsWith "vendor-missing" [ person "vendor/tate/people.ouro", ("main.ouro", "(import tate/nope)\n(document g (:r 1))") ]
                                                                     "SCP0006" ["missingModule = \"tate/nope\"", "nope.ouro"])
    , ("a cycle through collections names its files", projectFailsWith "vendor-cycle" [ ("vendor/a/x.ouro", "(import b/y)\n(export f)\n(define/procedure f () (y/g))"), ("vendor/b/y.ouro", "(import a/x)\n(export g)\n(define/procedure g () (x/f))")
                                                                                    , ("main.ouro", "(import a/x)\n(document g (:r (x/f)))") ]
                                                                     "SCP0009" ["vendor", "x.ouro", "y.ouro"])
    , ("a collection cannot be named std",          projectFailsWith "vendor-std" [ person "vendor/std/people.ouro", ("main.ouro", "(document g (:r 1))") ]
                                                                     "BLD0011" ["reservedFolder = \"std\"", "Perhaps rename the folder"])
    , ("nor aat",                                   projectFailsWith "vendor-aat" [ person "vendor/aat/people.ouro", ("main.ouro", "(document g (:r 1))") ]
                                                                     "BLD0011" ["reservedFolder = \"aat\""])
    , ("a library's own vendor/ is an error",        projectFailsWith "vendor-nested" [ person "vendor/tate/people.ouro", person "vendor/tate/vendor/inner/people.ouro", ("main.ouro", "(document g (:r 1))") ]
                                                                     "BLD0012" ["nestedCollection = \"tate\"", "vendoring is flat"])
    , ("a collection's name as a folder of the project", projectFailsWith "vendor-overlap" [ person "vendor/tate/people.ouro", person "tate/people.ouro", ("main.ouro", "(document g (:r 1))") ]
                                                                     "BLD0013" ["overlapName = \"tate\"", "could mean either"])
    , ("its modules are not also loaded from the collection", projectFailsTimes "vendor-overlap-once" 1 [ person "vendor/tate/people.ouro", person "tate/places.ouro", ("main.ouro", "(document g (:r 1))") ])
    , ("or as a module of it",                      projectFailsWith "vendor-overlap-module" [ person "vendor/tate/people.ouro", ("tate.ouro", "(document g (:r 1))") ]
                                                                     "BLD0013" ["overlapName = \"tate\""])
    , ("or of another library",                     projectFailsWith "vendor-overlap-library" [ person "vendor/a/b/people.ouro", person "vendor/b/people.ouro", ("main.ouro", "(document g (:r 1))") ]
                                                                     "BLD0013" ["overlapName = \"b\""])
    , ("a library's package.ouro is checked",       projectFailsWith "vendor-package" [ ("vendor/tate/package.ouro", "(define/constant package (:sources 1))"), ("main.ouro", "(document g (:r 1))") ]
                                                                     "TYP0000" ["The package setting :sources is a String"])
    , ("and its source folder must exist",          projectFailsWith "vendor-no-sources" [ ("vendor/tate/package.ouro", "(define/constant package (:sources \"src\"))"), ("main.ouro", "(document g (:r 1))") ]
                                                                     "BLD0005" ["sourceFolder = \"src\""])
    , ("an import spelt in another case",           importCase "vendor-case" [ person "people.ouro", ("main.ouro", "(import People)\n(document g (:r (People/person \"A\")))") ]
                                                               "fileImport = \"people\"" "missingModule = \"People\"")
    , ("in a collection too",                       importCase "vendor-case-collection" [ person "vendor/tate/people.ouro", ("main.ouro", "(import tate/People)\n(document g (:r (People/person \"A\")))") ]
                                                               "fileImport = \"tate/people\"" "missingModule = \"tate/People\"")
    ]
    where
    person file = (file, "(export person)\n(define/procedure person (n) (+ \"Person: \" n))")
    broken      = "(document g (:r (get () 5)))"

    -- Only a file system that ignores case, as Windows' and macOS's do, finds the file, which
    -- is then refused (BLD0014); any other does not find it (SCP0006)
    importCase name files refused missing = case os `elem` ["mingw32", "darwin"] of
        True  -> projectFailsWith name files "BLD0014" [refused]
        False -> projectFailsWith name files "SCP0006" [missing]


-- The graphs a project builds: those named by -g, else by :documents, else every one; only their
-- modules are compiled
chosenGraphs :: Group
chosenGraphs = Group "Builds: the graphs chosen"
    [ (":documents builds only those graphs",         projectBuildsTo "chosen" [ package "(:documents (\"a/x\"))", two, broken ]
                                                                    [("out" </> "a" </> "x.json", "{\"r\":1}")])
    , ("-t replaces :documents",                       projectBuildsWith "chosen-g" ["a/y"] [ package "(:documents (\"a/x\"))", two, broken ]
                                                                      [("out" </> "a" </> "y.json", "{\"r\":2}")])
    , ("-t may be given more than once",            projectBuildsWith "chosen-twice" ["a/y", "a/x"] [ two ]
                                                                      [("out" </> "a" </> "y.json", "{\"r\":2}"), ("out" </> "a" </> "x.json", "{\"r\":1}")])
    , (":documents () builds nothing",                 projectBuildsTo "chosen-none" [ package "(:documents ())", two, broken ] [])
    , ("a graph is written module/graph",          projectFailsWith "chosen-form" [ package "(:documents (\"x\"))", two ]
                                                                     "BLD0018" ["writtenSelection = \"x\"", "people/person"])
    , ("a module the project lacks suggests the closest", projectFailsWith "chosen-module" [ package "(:documents (\"peopel/person\"))", ("people.ouro", "(document person (:r 1))") ]
                                                                     "BLD0017" ["unknownSelection = \"peopel/person\"", "Perhaps you meant 'people'"])
    , ("a graph its module lacks suggests the closest", projectFailsWith "chosen-graph" [ package "(:documents (\"a/persn\"))", ("a.ouro", "(document person (:r 1))") ]
                                                                     "SCP0004" ["trgtGraph = \"persn\"", "suggestedGraph = \"person\""])
    , ("-g's error is shown at the argument",     withTests 1 . property $ do
                                                        dir <- evalIO (writeProject "chosen-at" [ two ])
                                                        evalIO (buildProject dir ["b/x"]) >>= \case
                                                            Right (BuildFailure _ errs) -> map (sourceName . errPos) (toList errs) === ["-t b/x"]
                                                            Right (BuildSuccess _ outs) -> annotate (show (map fst outs)) >> failure
                                                            Left message                -> annotate (T.unpack message) >> failure)
    ]
    where
    package settings = ("package.ouro", "(define/constant package " <> settings <> ")")
    two              = ("a.ouro", "(document x (:r 1))\n(document y (:r 2))")
    broken           = ("b.ouro", "(document g (:r (get () 5)))")


-- Modules in files on disk, read as the command line reads them: from the directory of the
-- file compiled, before the bundled ones
modulesOnDisk :: Group
modulesOnDisk = Group "Modules: imported from files on disk"
    [ ("a module beside the file",                 onDiskToJSON "beside" [helpers, main' "(import helpers)\n(document g (:r (helpers/greet \"Ann\")))"]
                                                                "{\"r\": \"Hello, Ann\"}")
    , ("a module in a folder",                     onDiskToJSON "folder" [("linked-art/people.ouro", "(export person)\n(define/procedure person (n) (+ \"Person: \" n))"), main' "(import linked-art/people)\n(document g (:r (people/person \"A\")))"]
                                                                "{\"r\": \"Person: A\"}")
    , ("a module importing another",               onDiskToJSON "chain" [("lower.ouro", "(export shout)\n(define/procedure shout (s) (+ s \"!\"))"), ("middle.ouro", "(import lower)\n(export call)\n(define/procedure call (s) (lower/shout s))"), main' "(import middle)\n(document g (:r (middle/call \"hi\")))"]
                                                                "{\"r\": \"hi!\"}")
    , ("a module with CRLF lines and a BOM",       onDiskToJSON "crlf" [("helpers.ouro", "\xEF\xBB\xBF(export greet)\r\n(define/procedure greet (n) (+ \"Hello, \" n))\r\n"), main' "(import helpers)\n(document g (:r (helpers/greet \"Ann\")))"]
                                                                "{\"r\": \"Hello, Ann\"}")
    , ("the bundled std, with nothing beside it",  onDiskToJSON "bundled" [main' "(import std/base)\n(document g (base/aat \"1\" \"A\"))"]
                                                                "{\"id\": \"http://vocab.getty.edu/aat/1\", \"type\": \"Type\", \"_label\": \"A\"}")
    , ("a std/base.ouro beside it is not imported", onDiskToJSON "reserved" [("std/base.ouro", "(export core)\n(define/procedure core (a b c) \"mine\")"), main' "(import std/base)\n(document g (base/aat \"1\" \"A\"))"]
                                                                "{\"id\": \"http://vocab.getty.edu/aat/1\", \"type\": \"Type\", \"_label\": \"A\"}")
    , ("a missing module says where it looked",    onDiskFails "missing" [main' "(import nope)\n(document g (:r 1))"]
                                                               (\dir -> ("SCP0006", dir </> "main.ouro", 1, 1, ["Looked for " <> T.pack (dir </> "nope.ouro")])))
    , ("an error in a module is in its file",      onDiskFails "broken" [("broken.ouro", "(export bad)\n(define/procedure bad () (get () 5))"), main' "(import broken)\n(document g (:r (broken/bad)))"]
                                                               (\dir -> ("SCP0003", dir </> "broken.ouro", 2, 34, ["attemptedIndex = 5"])))
    , ("a cycle on disk",                          onDiskFails "cycle" [("a.ouro", "(import b)\n(export f)\n(define/procedure f () 1)"), ("b.ouro", "(import a)\n(export f)\n(define/procedure f () 2)"), main' "(import a)\n(document g (:r (a/f)))"]
                                                               (\dir -> ("SCP0009", dir </> "b.ouro", 1, 1, ["cyclePath = [\"a\",\"b\",\"a\"]"])))
    , ("a module that is not UTF-8",               onDiskFails "not-utf8" [("latin.ouro", "(export f)\n(define/procedure f () \"caf\xE9\")"), main' "(import latin)\n(document g (:r (latin/f)))"]
                                                               (\dir -> ("SCP0010", dir </> "main.ouro", 1, 1, ["unreadableModule = \"latin\"", "is not valid UTF-8"])))
      -- A module that fails is reported once, however many modules of the project reach it (gap 40, fixed)
    , ("a project's cycle is reported once",       projectFailsTimes "cycle-once" 1 [ ("main.ouro", "(import a)\n(document g (:r (a/f)))"), ("a.ouro", "(import b)\n(export f)\n(define/procedure f () 1)"), ("b.ouro", "(import a)\n(export f)\n(define/procedure f () 2)") ])
    , ("and an unreadable module",                 projectFailsTimes "unreadable-once" 1 [ ("main.ouro", "(import latin)\n(document g (:r (latin/f)))"), ("latin.ouro", "(export f)\n(define/procedure f () \"caf\xE9\")") ])
    , ("a path written with / has one separator",  slashedPath)
    ]

    where
    helpers = ("helpers.ouro", "(export greet)\n(define/procedure greet (n) (+ \"Hello, \" n))")
    main' source = ("main.ouro", source)


-- A file compiled by a path written with '/' names its modules with one kind of separator. On
-- Windows the modules were once named half with '/' and half with '\', as in
-- Temp/cli-mods\broken.ouro, because the module's name was joined to the path as written.
slashedPath :: Property
slashedPath = withTests 1 . property $ do
    (dir, _) <- evalIO (compileOnDisk "slashed" [("broken.ouro", "(export bad)\n(define/procedure bad () (get () 5))"), ("main.ouro", "(import broken)\n(document g (:r (broken/bad)))")])
    let written = map (\case '\\' -> '/'; c -> c) (dir </> "main.ouro")
    source <- evalIO (either (error . T.unpack) id <$> readSource written)
    result <- evalIO (compileFile written "g" source)
    case result of
        CompilationFailure _ (err :| _) -> sourceName (errPos err) === normalise (dir </> "broken.ouro")
        compiled                        -> annotate (describe compiled) >> failure


-- Source files, read from disk as the command line reads them: a file that cannot be read is
-- an error, never an exception
sourceFiles :: Group
sourceFiles = Group "Source files: read from disk"
    [ ("a file that does not exist is an error, not an exception", withTests 1 . property $ do
                                                  content <- evalIO (readSource (fixture "ouro/no-such-file.ouro"))
                                                  either (T.isInfixOf "cannot be read: it does not exist") (const False) content === True)
    ]


-- The project's gaps: pinned as they are today, and fixed gaps' old pins, run negated
knownGaps :: Group
knownGaps = Group "Known gaps: the project, current and fixed"
    [ ("gap 32 (fixed): overlapping source roots build a module twice", fixedGap $ projectBuildsTo "gap-32-overlap" [ ("package.ouro", "(define/constant package (:sources (\".\" \"src\")))"), ("src/main.ouro", "(document g (:r 1))") ]
                                                                  [("out" </> "main" </> "g.json", "{\"r\":1}"), ("out" </> "src" </> "main" </> "g.json", "{\"r\":1}")])
    , ("gap 32 (fixed): a root with a trailing slash is a second root", fixedGap $ projectFailsWith "gap-32-slash" [ ("package.ouro", "(define/constant package (:sources (\"src\" \"src/\")))"), ("src/main.ouro", "(document g (:r 1))") ]
                                                                   "SCP0011" ["ambiguousPath = \"main\""])
    , ("gap 32 (fixed): a root through .. builds the project's file", fixedGap . withTests 1 . property $ do
                                                    dir   <- evalIO (writeProject "gap-32-dotdot" [ ("package.ouro", "(define/constant package (:sources \"src/..\"))"), ("main.ouro", "(document g (:r 1))"), ("src/keep.txt", "a folder to go through") ])
                                                    found <- evalIO (discoverModules (Project dir (Package "src/.." "out" Nothing (initialPos "package.ouro"))))
                                                    map fst found === ["main", "package"])
    , ("gap 33 (fixed): a nested project's files are modules", fixedGap . withTests 1 . property $ do
                                                    dir   <- evalIO (writeProject "gap-33" [ ("main.ouro", "(document g (:r 1))"), ("sub/package.ouro", "(define/constant package (:sources \"src\"))"), ("sub/src/x.ouro", "(document g (:r 2))") ])
                                                    found <- evalIO (discoverModules (Project dir (defaultPackage dir)))
                                                    assert (any (`elem` map fst found) ["sub/package", "sub/src/x"]))
    , ("gap 34 (fixed): a graph's name is not checked as a file name", fixedGap $ filesTo [("main", "(document a? (:r 1))\n(document con (:r 2))\n(document ex:g (:r 3))")]
                                                                  [("out" </> "main" </> "a?.json", "{\"r\":1}"), ("out" </> "main" </> "con.json", "{\"r\":2}"), ("out" </> "main" </> "ex:g.json", "{\"r\":3}")])
    , ("gap 34 (fixed): nor are names apart only by case", fixedGap $ filesTo [("main", "(document G (:r 1))\n(document g (:r 2))")]
                                                             [("out" </> "main" </> "G.json", "{\"r\":1}"), ("out" </> "main" </> "g.json", "{\"r\":2}")])
    , ("gap 36 (fixed): a folder that does not exist builds the project above it", fixedGap . withTests 1 . property $ do
                                                    dir <- evalIO (writeProject "gap-36" [ ("package.ouro", "(define/constant package #record-empty)"), ("main.ouro", "(document g (:r 1))") ])
                                                    evalIO (buildProject (dir </> "typo") []) >>= \case
                                                        Right _ -> success
                                                        Left _  -> failure)
    , ("gap 40 (fixed): one import cycle is reported three times", fixedGap $ projectFailsTimes "gap-40-cycle" 3 [ ("main.ouro", "(import a)\n(document g (:r (a/f)))"), ("a.ouro", "(import b)\n(export f)\n(define/procedure f () 1)"), ("b.ouro", "(import a)\n(export f)\n(define/procedure f () 2)") ])
    , ("gap 40 (fixed): an unreadable module is reported twice", fixedGap $ projectFailsTimes "gap-40-unreadable" 2 [ ("main.ouro", "(import latin)\n(document g (:r (latin/f)))"), ("latin.ouro", "(export f)\n(define/procedure f () \"caf\xE9\")") ])
    ]


-- package.ouro reads as the given source folder and outputs folder
readsAs :: Text -> (FilePath, FilePath) -> Property
readsAs source (sources, outputs) = withTests 1 . property $
    case readPackage "package.ouro" source of
        (_, Right p)   -> (packageSources p, packageOutputs p) === (sources, outputs)
        (_, Left errs) -> annotate (show errs) >> failure

-- package.ouro reads as the given graphs chosen
readsGraphs :: Text -> Maybe [Text] -> Property
readsGraphs source graphs = withTests 1 . property $
    case readPackage "package.ouro" source of
        (_, Right p)   -> packageGraphs p === graphs
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


-- Modules in memory, each built, each graph named by the file a project writes it to, in out
filesTo :: [(Text, Text)] -> [(FilePath, Text)] -> Property
filesTo modules expected = withTests 1 . property $
    outputsAre expected (uncurry (outputFile "out") <$> runIdentity (buildModules (fromSources (Map.fromList modules)) [ (initialPos (T.unpack path <> ".ouro"), path, AllGraphs) | (path, _) <- modules ]))

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
    evalIO (buildProject dir []) >>= \case
        Right result -> outputsAre [ (dir </> file, json) | (file, json) <- expected ] result
        Left message -> annotate (T.unpack message) >> failure

-- The project builds the graphs chosen by -g to the given files
projectBuildsWith :: FilePath -> [Text] -> [(FilePath, BS.ByteString)] -> [(FilePath, Text)] -> Property
projectBuildsWith name graphs files expected = withTests 1 . property $ do
    dir <- evalIO (writeProject name files)
    evalIO (buildProject dir graphs) >>= \case
        Right result -> outputsAre [ (dir </> file, json) | (file, json) <- expected ] result
        Left message -> annotate (T.unpack message) >> failure

projectFailsWith :: FilePath -> [(FilePath, BS.ByteString)] -> Text -> [Text] -> Property
projectFailsWith name files code messages = withTests 1 . property $ do
    dir <- evalIO (writeProject name files)
    evalIO (buildProject dir []) >>= \case
        Right (BuildFailure _ errs) -> do
                                       annotate (show errs)
                                       assert (any (\err -> smartErrorCode (errContext err) == code && all (`T.isInfixOf` errorText err) messages) errs)
        Right (BuildSuccess _ outs) -> annotate (show (map fst outs)) >> failure
        Left message                -> annotate (T.unpack message) >> failure

-- The project fails with the given number of errors
projectFailsTimes :: FilePath -> Int -> [(FilePath, BS.ByteString)] -> Property
projectFailsTimes name times files = withTests 1 . property $ do
    dir <- evalIO (writeProject name files)
    evalIO (buildProject dir []) >>= \case
        Right (BuildFailure _ errs) -> annotate (show errs) >> length errs === times
        Right (BuildSuccess _ outs) -> annotate (show (map fst outs)) >> failure
        Left message                -> annotate (T.unpack message) >> failure

-- The project builds, warning the given number of times with the given text
projectWarnsTimes :: FilePath -> Int -> Text -> [(FilePath, BS.ByteString)] -> Property
projectWarnsTimes name times message files = withTests 1 . property $ do
    dir <- evalIO (writeProject name files)
    evalIO (buildProject dir []) >>= \case
        Right (BuildSuccess warnings _) -> annotate (show warnings) >> length (filter (T.isInfixOf message . warningText) warnings) === times
        Right (BuildFailure _ errs)     -> annotate (show errs) >> failure
        Left message'                   -> annotate (T.unpack message') >> failure


-- Writes the files into a directory of their own, named for the test, and compiles its
-- main.ouro as the command line would: its imports are read from the files beside it
compileOnDisk :: FilePath -> [(FilePath, BS.ByteString)] -> IO (FilePath, CompilationResult)
compileOnDisk name files = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "ouro-test" </> name
    exists <- doesDirectoryExist dir
    when exists (removeDirectoryRecursive dir)
    forM_ files $ \(file, bytes) -> do
        createDirectoryIfMissing True (takeDirectory (dir </> file))
        BS.writeFile (dir </> file) bytes
    let mainFile = dir </> "main.ouro"
    source <- either (error . T.unpack) id <$> readSource mainFile
    (,) dir <$> compileFile mainFile "g" source

onDiskToJSON :: FilePath -> [(FilePath, BS.ByteString)] -> Text -> Property
onDiskToJSON name files expected = withTests 1 . property $ do
    (_, result) <- evalIO (compileOnDisk name files)
    case result of
        CompilationSuccess _ ast -> squash (TL.toStrict (toJSON defaultOptions ast)) === squash expected
        failed                   -> annotate (describe failed) >> failure

-- The compile fails with an error of the given code, file, line and column, mentioning every
-- given text; the file is a path in the test's directory, which is only known once it is made
onDiskFails :: FilePath -> [(FilePath, BS.ByteString)] -> (FilePath -> (Text, FilePath, Int, Int, [Text])) -> Property
onDiskFails name files expectation = withTests 1 . property $ do
    (dir, result) <- evalIO (compileOnDisk name files)
    let (code, file, line, column, messages) = expectation dir
    case result of
        CompilationFailure _ errs
            -> do
               annotate (unlines [ T.unpack (smartErrorCode (errContext err)) <> " at " <> sourceName (errPos err) <> ":" <> show (unPos (sourceLine (errPos err))) <> ":" <> show (unPos (sourceColumn (errPos err))) <> ": " <> show err | err <- toList errs ])
               assert (any (\err -> smartErrorCode (errContext err) == code
                                 && sourceName (errPos err) == file
                                 && unPos (sourceLine (errPos err)) == line
                                 && unPos (sourceColumn (errPos err)) == column
                                 && all (`T.isInfixOf` errorText err) messages) errs)
        compiled
            -> annotate (describe compiled) >> failure
