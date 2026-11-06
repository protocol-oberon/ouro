{-# LANGUAGE GADTs #-}

module Ouro.BuildTest
( builds
) where

import           Data.Functor.Identity        (runIdentity)
import qualified Data.Map.Strict              as Map
import           Data.Ouro.Build              (BuildResult (..), buildModules)
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
    , ("a module of functions writes nothing",      buildsTo [("helpers", "(export f)\n(defn f () 1)"), ("main", "(import helpers)\n(graph g (:r (helpers/f)))")]
                                                             [(("main", "g"), "{\"r\":1}")])
    , ("one failing graph fails the build",         buildFailsWith [("good", "(graph g (:r 1))"), ("bad", "(graph g (:r (get () 5)))")] ["attemptedIndex = 5"])
    , ("every failing module is reported",          buildFailsTimes 2 [("one", "(graph g (:r (get () 5)))"), ("two", "(graph g (:r (get () 6)))"), ("fine", "(graph g (:r 1))")])
    , ("a module imported by many warns once",      buildWarnsTimes 1 "badName" [("badly-named", "(export badName)\n(defn badName () 1)"), ("one", "(import badly-named)\n(graph g (:r (badly-named/badName)))"), ("two", "(import badly-named)\n(graph g (:r (badly-named/badName)))")])
    , ("an import cycle among modules",             buildFailsWith [("a", "(import b)\n(export f)\n(defn f () 1)"), ("b", "(import a)\n(export f)\n(defn f () 2)")] ["ImportCycle"])
    ]


-- Modules in memory, each built: (path, source). Each graph is keyed by its module and name.
buildInMemory :: [(Text, Text)] -> BuildResult (Text, Text)
buildInMemory modules = runIdentity (buildModules (fromSources (Map.fromList modules)) [ (initialPos (T.unpack path <> ".ouro"), path) | (path, _) <- modules ])

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

buildWarnsTimes :: Int -> Text -> [(Text, Text)] -> Property
buildWarnsTimes times message modules = withTests 1 . property $
    case buildInMemory modules of
        BuildSuccess warnings _ -> annotate (show warnings) >> length (filter (T.isInfixOf message . warningText) warnings) === times
        BuildFailure _ errs     -> annotate (show errs) >> failure
