{-# LANGUAGE GADTs #-}

module Ouro.BuildTest
( builds
, chosenGraphs
) where

import           Data.Functor.Identity        (runIdentity)
import qualified Data.Map.Strict              as Map
import           Data.Ouro.Build              (BuildResult (..), Graphs (..),
                                               buildModules)
import           Data.Ouro.Lisp.Module.Loader (fromSources)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Hedgehog
import           Text.Megaparsec              (initialPos)
import           Utils                        (errorText, outputsAre, warningText)


-- Every graph of every module, keyed by its module and name, or nothing at all. Where a graph
-- is written is the host's (the CLI's tests).
builds :: Group
builds = Group "Builds: every graph of every module"
    [ ("every graph, by module and name",           buildsTo [("a", "(graph x (:r 1))\n(graph y (:r 2))"), ("lib/b", "(graph z (:r 3))")]
                                                             [(("a", "x"), "{\"r\":1}"), (("a", "y"), "{\"r\":2}"), (("lib/b", "z"), "{\"r\":3}")])
    , ("a module of functions writes nothing",      buildsTo [("helpers", "(export f)\n(define/procedure f () 1)"), ("main", "(import helpers)\n(graph g (:r (helpers/f)))")]
                                                             [(("main", "g"), "{\"r\":1}")])
    , ("one failing graph fails the build",         buildFailsWith [("good", "(graph g (:r 1))"), ("bad", "(graph g (:r (get () 5)))")] ["attemptedIndex = 5"])
    , ("every failing module is reported",          buildFailsTimes 2 [("one", "(graph g (:r (get () 5)))"), ("two", "(graph g (:r (get () 6)))"), ("fine", "(graph g (:r 1))")])
    , ("a module imported by many warns once",      buildWarnsTimes 1 "badName" [("badly-named", "(export badName)\n(define/procedure badName () 1)"), ("one", "(import badly-named)\n(graph g (:r (badly-named/badName)))"), ("two", "(import badly-named)\n(graph g (:r (badly-named/badName)))")])
    , ("an import cycle among modules",             buildFailsWith [("a", "(import b)\n(export f)\n(define/procedure f () 1)"), ("b", "(import a)\n(export f)\n(define/procedure f () 2)")] ["ImportCycle"])
      -- A module that fails is reported once, however many modules reach it (gap 40, fixed)
    , ("is reported once",                          buildFailsTimes 1 [("main", "(import a)\n(graph g (:r (a/f)))"), ("a", "(import b)\n(export f)\n(define/procedure f () 1)"), ("b", "(import a)\n(export f)\n(define/procedure f () 2)")])
    , ("named by its own modules, not the one built", chosenFailsWith [("main", "(import a)\n(graph g (:r (a/f)))"), ("a", "(import b)\n(export f)\n(define/procedure f () 1)"), ("b", "(import a)\n(export f)\n(define/procedure f () 2)")] [("main", AllGraphs)]
                                                             ["cyclePath = [\"a\",\"b\",\"a\"]"])
    , ("a module imported by many fails once",      buildFailsTimes 1 [("main", "(import bad)\n(import other)\n(graph g (:r (other/f)))"), ("other", "(import bad)\n(export f)\n(define/procedure f () 1)"), ("bad", "(graph g (:r \"open))")])
      -- Every module built is analysed, its functions no graph calls too
    , ("a function no graph calls is checked",      buildFailsWith [("a", "(define/procedure f () nope)\n(graph g (:r 1))")] ["variableName = \"nope\""])
    ]


-- The graphs a build compiles, by module: every one, or those named
chosenGraphs :: Group
chosenGraphs = Group "Builds: the graphs chosen"
    [ ("only the graphs named",                     chosenTo [("a", "(graph x (:r 1))\n(graph y (:r 2))")] [("a", OnlyGraphs ["y"])]
                                                             [(("a", "y"), "{\"r\":2}")])
    , ("in the order named",                        chosenTo [("a", "(graph x (:r 1))\n(graph y (:r 2))")] [("a", OnlyGraphs ["y", "x"])]
                                                             [(("a", "y"), "{\"r\":2}"), (("a", "x"), "{\"r\":1}")])
    , ("a graph not named is not compiled",         chosenTo [("a", "(graph x (:r 1))\n(graph bad (:r (get () 5)))")] [("a", OnlyGraphs ["x"])]
                                                             [(("a", "x"), "{\"r\":1}")])
    , ("a graph its module lacks suggests the closest", chosenFailsWith [("a", "(graph x (:r 1))\n(graph person (:r 2))")] [("a", OnlyGraphs ["persn"])]
                                                             ["trgtGraph = \"persn\"", "suggestedGraph = \"person\""])
    , ("a module with no graphs has none to name",  chosenFailsWith [("a", "(export f)\n(define/procedure f () 1)")] [("a", OnlyGraphs ["g"])] ["NoCompilationTarget"])
    ]


-- Modules in memory, each built: (path, source). Each graph is keyed by its module and name.
buildInMemory :: [(Text, Text)] -> BuildResult (Text, Text)
buildInMemory modules = runIdentity (buildModules (fromSources (Map.fromList modules)) [ (initialPos (T.unpack path <> ".ouro"), path, AllGraphs) | (path, _) <- modules ])

buildsTo :: [(Text, Text)] -> [((Text, Text), Text)] -> Property
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

-- Modules in memory, the graphs named of each built
chosenInMemory :: [(Text, Text)] -> [(Text, Graphs)] -> BuildResult (Text, Text)
chosenInMemory modules chosen = runIdentity (buildModules (fromSources (Map.fromList modules)) [ (initialPos (T.unpack path <> ".ouro"), path, graphs) | (path, graphs) <- chosen ])

chosenTo :: [(Text, Text)] -> [(Text, Graphs)] -> [((Text, Text), Text)] -> Property
chosenTo modules chosen expected = withTests 1 . property $ outputsAre expected (chosenInMemory modules chosen)

chosenFailsWith :: [(Text, Text)] -> [(Text, Graphs)] -> [Text] -> Property
chosenFailsWith modules chosen messages = withTests 1 . property $
    case chosenInMemory modules chosen of
        BuildFailure _ errs -> annotate (show errs) >> assert (any (\err -> all (`T.isInfixOf` errorText err) messages) errs)
        BuildSuccess _ outs -> annotate (show (map fst outs)) >> failure

buildWarnsTimes :: Int -> Text -> [(Text, Text)] -> Property
buildWarnsTimes times message modules = withTests 1 . property $
    case buildInMemory modules of
        BuildSuccess warnings _ -> annotate (show warnings) >> length (filter (T.isInfixOf message . warningText) warnings) === times
        BuildFailure _ errs     -> annotate (show errs) >> failure
