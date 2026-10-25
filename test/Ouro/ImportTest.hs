{-# LANGUAGE GADTs #-}

module Ouro.ImportTest
( importedModules
, imports
, moduleErrors
, modulesOnDisk
, sourceFrames
) where

import           Control.Monad                 (forM_, when)
import qualified Data.ByteString               as BS
import           Data.Char                     (isSpace)
import           Data.Foldable                 (toList)
import           Data.List                     (findIndex)
import           Data.List.NonEmpty            (NonEmpty (..))
import qualified Data.Map.Strict               as Map
import           Data.Ouro                     (CompilationResult (..),
                                                ErrorContext (..),
                                                OuroError (..), OuroWarning (..),
                                                SourceFrame (..), bundledFile,
                                                compileFile, defaultOptions,
                                                readSource, smartErrorCode,
                                                smartWarningCode, sourceFrame,
                                                toJSON)
import           Data.Ouro.Lisp.Lexer          (tokenize)
import           Data.Ouro.Lisp.Module.Bundled (bundled)
import qualified Data.Ouro.Lisp.Module.Types   as M
import           Data.Ouro.Lisp.Parser         (parseModule)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Hedgehog
import           System.Directory              (createDirectoryIfMissing,
                                                doesDirectoryExist,
                                                getTemporaryDirectory,
                                                removeDirectoryRecursive)
import           System.FilePath               (normalise, takeDirectory, (</>))
import           Text.Megaparsec               (SourcePos (..), mkPos,
                                                sourceColumn, sourceLine,
                                                sourceName, unPos)
import           Utils                         (compileG, compileWithModules,
                                                compilesToJSON,
                                                compilesWithoutWarnings,
                                                describe, failsWith,
                                                importsFailAt, importsFailIn,
                                                importsFailTimes,
                                                importsFailWith,
                                                importsFailWithEach,
                                                importsToJSON, importsWarnAt,
                                                importsWarnIn, importsWarnTimes,
                                                warningText)


-- A module imports others with (import path), and is qualified by the path's last segment
-- or an alias; it exports functions with one (export f g). Both come first in a file. std is
-- bundled with the compiler, and holds Linked Art's own patterns.
imports :: Group
imports = Group "Modules: import, export and the bundled std"
    [ ("std's aat builds a Type",                  compilesToJSON "(import std/base) (graph g (base/aat \"1\" \"A\"))"
                                                                  "{\"id\": \"http://vocab.getty.edu/aat/1\", \"type\": \"Type\", \"_label\": \"A\"}")
    , ("std's name builds a Name",                 compilesToJSON "(import std/base) (graph g (:identified_by ((base/name (:type \"T\") \"A\"))))"
                                                                  "{\"identified_by\": [{\"type\": \"Name\", \"classified_as\": [{\"type\": \"T\"}], \"content\": \"A\"}]}")
    , ("an alias qualifies the module",            compilesToJSON "(import std/base as la) (graph g (la/aat \"1\" \"A\"))"
                                                                  "{\"id\": \"http://vocab.getty.edu/aat/1\", \"type\": \"Type\", \"_label\": \"A\"}")
    , ("std compiles without warnings",            compilesWithoutWarnings "(import std/base) (graph g (base/aat \"1\" \"A\"))")
    , ("a local function and an import's apart",   compilesToJSON "(import std/base) (defn aat (x) \"mine\") (graph g (:a (aat 1) :b (base/aat \"1\" \"A\")))"
                                                                  "{\"a\": \"mine\", \"b\": {\"id\": \"http://vocab.getty.edu/aat/1\", \"type\": \"Type\", \"_label\": \"A\"}}")
    , ("/ and /= are not qualified names",         compilesToJSON "(graph g (:a (/ 6 3) :b (/= 1 2)))"
                                                                  "{\"a\": 2, \"b\": true}")

      -- Finding modules and names
    , ("the module must exist",                    failsWith "missingModule = \"nope\""
                                                             "(import nope) (graph g (:r 1))")
    , ("every missing module is reported",         everyMissingModule)
    , ("a qualifier must be imported",             failsWith "perhaps add (import std/base)"
                                                             "(graph g (base/name \"A\"))")
    , ("an unknown qualifier suggests one",        failsWith "Perhaps you meant 'base'"
                                                             "(import std/base) (graph g (basee/name \"A\"))")
    , ("a name must be exported",                  failsWith "unexportedName = \"nope\""
                                                             "(import std/base) (graph g (base/nope 1))")

      -- Writing imports and exports
    , ("imports come first",                       failsWith "Imports and exports come first"
                                                             "(defn f () 1) (import std/base) (graph g (:r 1))")
    , ("two imports need two qualifiers",          failsWith "Perhaps name one with as"
                                                             "(import std/base) (import std/base) (graph g (:r 1))")
    , ("an alias is one name",                     failsWith "has a '/' in it"
                                                             "(import std/base as a/b) (graph g (:r 1))")
    , ("an export names a defn of the module",     failsWith "defines no function by that name"
                                                             "(export nope) (graph g (:r 1))")
    , ("and suggests a close one",                 failsWith "Perhaps you meant 'mine'"
                                                             "(export mien) (defn mine () 1) (graph g (:r 1))")
    , ("a module has one export",                  failsWith "A module has one export"
                                                             "(export f) (export h) (defn f () 1) (defn h () 2) (graph g (:r 1))")
    , ("an export names a function once",          failsWith "exported twice"
                                                             "(export f f) (defn f () 1) (graph g (:r 1))")
    , ("an export names at least one",             failsWith "An export names at least one function"
                                                             "(export) (graph g (:r 1))")

      -- A '/' qualifies an imported name, so no other name has one
    , ("a defn's name has no /",                   failsWith "has a '/' in its name"
                                                             "(defn std/inc (n) (+ n 1)) (graph g (:r (std/inc 1)))")
    , ("nor a let binding's",                      failsWith "has a '/' in it"
                                                             "(graph g (let ($std/x 1) (:r 1)))")
    ]


-- Imports are independent, so two missing modules are both reported
everyMissingModule :: Property
everyMissingModule = withTests 1 . property $
    case compileG "(import nope) (import nada) (graph g (:r 1))" of
        CompilationFailure _ errs
            -> do
               let shown = T.pack (show (toList errs))
               annotate (T.unpack shown)
               assert ("\"nope\"" `T.isInfixOf` shown && "\"nada\"" `T.isInfixOf` shown)
        _compiled
            -> failure


-- Modules held in memory, imported as files would be: closures, privacy, the graph of
-- imports, cycles, using imported functions, and errors and warnings from inside a module
importedModules :: Group
importedModules = Group "Modules: imported modules, in memory"
    [ -- Each module is its own: its functions close over its own functions and imports
      ("an imported function uses its own helper", importsToJSON [helpers] "(import helpers) (graph g (:r (helpers/greet \"Ann\")))"
                                                                          "{\"r\": \"Hello, Ann\"}")
    , ("not the importer's of the same name",      importsToJSON [helpers] "(import helpers) (defn salutation () \"Bye, \") (graph g (:a (helpers/greet \"Ann\") :b (salutation)))"
                                                                          "{\"a\": \"Hello, Ann\", \"b\": \"Bye, \"}")
    , ("a module's own imports resolve in it",     importsToJSON [lower, middle] "(import middle) (graph g (:r (middle/call \"hi\")))"
                                                                                "{\"r\": \"hi!\"}")
    , ("imports are not passed on to importers",   importsFailWith ["qualifierName = \"lower\""] [lower, middle]
                                                                   "(import middle) (graph g (:r (lower/shout \"hi\")))")
    , ("nor an importer's imports to a module",    importsFailIn "unaware.ouro" ["qualifierName = \"lower\""] [lower, unaware]
                                                                   "(import lower) (import unaware) (graph g (:r (unaware/call \"hi\")))")

      -- Privacy
    , ("an unexported helper is private",          importsFailWith ["unexportedName = \"salutation\""] [helpers]
                                                                   "(import helpers) (graph g (:r (helpers/salutation)))")
    , ("a module's graphs are never imported",     importsFailWith ["unexportedName = \"secret\""] [withGraph]
                                                                   "(import with-graph) (graph g (:r (with-graph/secret)))")
    , ("a module with no export offers nothing",   importsFailWith ["unexportedName = \"f\""] [("quiet", "(defn f () 1)")]
                                                                   "(import quiet) (graph g (:r (quiet/f)))")

      -- The graph of imports
    , ("a module under two names",                 importsToJSON [helpers] "(import helpers) (import helpers as h) (graph g (:a (helpers/greet \"A\") :b (h/greet \"B\")))"
                                                                          "{\"a\": \"Hello, A\", \"b\": \"Hello, B\"}")
    , ("a qualifier is the path's last part",      importsToJSON [("linked-art/people", "(export person)\n(defn person (n) (+ \"Person: \" n))")]
                                                                 "(import linked-art/people) (graph g (:r (people/person \"A\")))"
                                                                 "{\"r\": \"Person: A\"}")
    , ("two last parts the same need as",          importsFailWith ["Perhaps name one with as"] [peopleA, peopleB]
                                                                   "(import a/people) (import b/people) (graph g (:r 1))")
    , ("and with as they are apart",               importsToJSON [peopleA, peopleB]
                                                                 "(import a/people) (import b/people as others) (graph g (:a (people/f) :b (others/f)))"
                                                                 "{\"a\": 1, \"b\": 2}")
    , ("a diamond shares its base",                importsToJSON [base, left, right] "(import left) (import right) (graph g (:r (+ (left/f) (right/f))))"
                                                                                     "{\"r\": 32}")
    , ("a module imported twice is loaded once",   importsWarnTimes 1 "badName" [badlyNamed, usesBadly "one", usesBadly "two"]
                                                                    "(import one) (import two) (graph g (:r (+ (one/f) (two/f))))")

      -- Cycles: Ouro has no recursion, across files as within one
    , ("a module cannot import itself",            importsFailWith ["cyclePath = [\"a\",\"a\"]"] [("a", "(import a)\n(export f)\n(defn f () 1)")]
                                                                   "(import a) (graph g (:r (a/f)))")
    , ("nor two each other",                       importsFailIn "b.ouro" ["cyclePath = [\"a\",\"b\",\"a\"]"]
                                                                   [("a", "(import b)\n(export f)\n(defn f () 1)"), ("b", "(import a)\n(export f)\n(defn f () 2)")]
                                                                   "(import a) (graph g (:r (a/f)))")
    , ("nor three in a circle",                    importsFailWith ["cyclePath = [\"a\",\"b\",\"c\",\"a\"]"]
                                                                   [("a", "(import b)\n(export f)\n(defn f () 1)"), ("b", "(import c)\n(export f)\n(defn f () 2)"), ("c", "(import a)\n(export f)\n(defn f () 3)")]
                                                                   "(import a) (graph g (:r (a/f)))")

      -- Imported functions are values like any other
    , ("an imported function as a value",          compilesToJSON "(import std/base) (graph g (let ($f base/aat) (f \"1\" \"A\")))"
                                                                  "{\"id\": \"http://vocab.getty.edu/aat/1\", \"type\": \"Type\", \"_label\": \"A\"}")
    , ("with a hole",                              compilesToJSON "(import std/base) (graph g (:r (map (base/aat ? \"A\") (\"1\" \"2\"))))"
                                                                  "{\"r\": [{\"id\": \"http://vocab.getty.edu/aat/1\", \"type\": \"Type\", \"_label\": \"A\"}, {\"id\": \"http://vocab.getty.edu/aat/2\", \"type\": \"Type\", \"_label\": \"A\"}]}")
    , ("passed to map",                            compilesToJSON "(import std/base) (graph g (:r (len (map base/description (\"A\" \"B\")))))"
                                                                  "{\"r\": 2}")
    , ("as a step of ->",                          importsToJSON [helpers] "(import helpers) (graph g (:r (-> \"Ann\" (helpers/greet))))"
                                                                          "{\"r\": \"Hello, Ann\"}")
    , ("in a case condition",                      importsToJSON [("checks", "(export big)\n(defn big (n) (> n 10))")]
                                                                 "(import checks) (graph g (:r (case 20 ((when (checks/big 20)) \"big\") (otherwise \"small\"))))"
                                                                 "{\"r\": \"big\"}")
    , ("a wrong number of arguments",              failsWith "nameOfFunc = \"aat\""
                                                             "(import std/base) (graph g (base/aat 1 2 3))")
    , ("a bad argument is blamed at the call",     importsFailIn "inline-test" ["expectedProperty = \"#str\""] []
                                                                   "(import std/base) (graph g (base/aat 5 \"A\"))")
    , ("an error in its body is in its module",    importsFailIn "broken-body.ouro" ["Index"] [("broken-body", "(export bad)\n(defn bad () (get () 5))")]
                                                                   "(import broken-body) (graph g (:r (broken-body/bad)))")

      -- Errors and warnings inside an imported module are reported, in its file
    , ("a lexical error in a module",              importsFailIn "unterminated.ouro" ["LexicalError"] [("unterminated", "(export f)\n(defn f () \"open)")]
                                                                   "(import unterminated) (graph g (:r 1))")
    , ("a parse error in a module",                importsFailIn "unparsed.ouro" ["LexicalError"] [unparsed]
                                                                   "(import unparsed) (graph g (:r 1))")
    , ("a bad export in a module",                 importsFailIn "misexported.ouro" ["defines no function by that name"] [("misexported", "(export g)\n(defn f () 1)")]
                                                                   "(import misexported) (graph g (:r 1))")
    , ("a missing import in a module",             importsFailIn "needy.ouro" ["missingModule = \"nope\""] [("needy", "(import nope)\n(export f)\n(defn f () 1)")]
                                                                   "(import needy) (graph g (:r 1))")
    , ("every missing import in a module",         importsFailWithEach ["\"nope\"", "\"nada\""] [("needy", "(import nope)\n(import nada)\n(export f)\n(defn f () 1)")]
                                                                       "(import needy) (graph g (:r 1))")
    , ("a missing module and a broken one",        importsFailWithEach ["missingModule = \"nope\"", "Expected argument list"] [unparsed]
                                                                       "(import nope) (import unparsed) (graph g (:r 1))")
    , ("a module's warnings reach the importer",   importsWarnTimes 1 "The function 'badName' is not named in kebab-case" [badlyNamed]
                                                                    "(import badly-named) (graph g (:r (badly-named/badName)))")

      -- Names
    , ("a binding named like a qualifier",         compilesToJSON "(import std/base) (graph g (let ($base 1) (:a base :b (base/aat \"1\" \"A\"))))"
                                                                  "{\"a\": 1, \"b\": {\"id\": \"http://vocab.getty.edu/aat/1\", \"type\": \"Type\", \"_label\": \"A\"}}")
    , ("a parameter's name has no /",              failsWith "The parameter 'a/b' has a '/' in it"
                                                             "(defn f (a/b) 1) (graph g (:r (f 1)))")
    , ("nor a fn's",                               failsWith "The parameter 'a/b' has a '/' in it"
                                                             "(graph g (:r ((fn (a/b) 1) 1)))")

      -- Writing imports and exports
    , ("an import names a module",                 failsWith "An import is written (import path) or (import path as name)"
                                                             "(import) (graph g (:r 1))")
    , ("as needs a name after it",                 failsWith "An import is written (import path) or (import path as name)"
                                                             "(import std/base as) (graph g (:r 1))")
    , ("an import names one module",               failsWith "An import is written (import path) or (import path as name)"
                                                             "(import std/base extra) (graph g (:r 1))")
    , ("a path has no empty part",                 failsWith "has an empty part"
                                                             "(import a//b) (graph g (:r 1))")
    , ("an export names functions",                failsWith "An export names the module's functions"
                                                             "(export 1) (graph g (:r 1))")
    , ("an export may come before imports",        compilesToJSON "(export f) (import std/base) (defn f () 1) (graph g (:r (f)))"
                                                                  "{\"r\": 1}")

      -- The bundled std
    , ("std/base exports exactly its patterns",    failsWith "exports: aat, description, identifier, language, name, nationality, occupation, shape, statement, statement-kind, style, tgn, ulan, work-type."
                                                             "(import std/base) (graph g (base/nope 1))")
    , ("every bundled function is documented",     stdIsDocumented)
    , ("bundled modules are named with /",        bundledNamesUseSlash)
    ]

    where
    helpers     = ("helpers", "(export greet)\n(defn salutation () \"Hello, \")\n(defn greet (n) (+ (salutation) n))")
    lower       = ("lower", "(export shout)\n(defn shout (s) (+ s \"!\"))")
    middle      = ("middle", "(import lower)\n(export call)\n(defn call (s) (lower/shout s))")
    unaware     = ("unaware", "(export call)\n(defn call (s) (lower/shout s))")
    withGraph   = ("with-graph", "(export f)\n(defn f () 1)\n(graph secret (:x 1))")
    peopleA     = ("a/people", "(export f)\n(defn f () 1)")
    peopleB     = ("b/people", "(export f)\n(defn f () 2)")
    base        = ("base", "(export one)\n(defn one () 1)")
    left        = ("left", "(import base)\n(export f)\n(defn f () (+ (base/one) 10))")
    right       = ("right", "(import base)\n(export f)\n(defn f () (+ (base/one) 20))")
    badlyNamed  = ("badly-named", "(export badName)\n(defn badName () 1)")
    usesBadly n = (n, "(import badly-named)\n(export f)\n(defn f () (badly-named/badName))")
    unparsed    = ("unparsed", "(export f)\n(defn f)")


-- Every function a bundled module defines has a (doc "..."), since the bundled modules are
-- where curators look things up
stdIsDocumented :: Property
stdIsDocumented = withTests 1 . property $ do
    assert (not (Map.null bundled))
    undocumented <- fmap concat . traverse undocumentedIn $ Map.elems bundled
    undocumented === []
    where
    undocumentedIn (file, source) =
        case tokenize file source >>= parseModule of
            Left err -> annotate (show err) >> failure
            Right m  -> pure [ (file, name) | M.Function _ name Nothing _ _ <- Map.elems (M._functionRegistry m) ]


-- Bundled modules are named with '/' on every platform: file-embed joins a folder's files
-- with the platform's separator, '\' on Windows, which would make std/base unreachable there
bundledNamesUseSlash :: Property
bundledNamesUseSlash = withTests 1 . property $ do
    annotate (show (Map.keys bundled))
    assert (Map.member "std/base" bundled)
    assert (not (any (T.isInfixOf "\\") (Map.keys bundled)))
    assert (not (any (T.isInfixOf "\\" . T.pack . fst) (Map.elems bundled)))


-- Every error modules can raise, each with its code, the file, line and column it is
-- reported at, and what its message and hint tell the reader
moduleErrors :: Group
moduleErrors = Group "Modules: every error, with its code, place and hint"
    [ -- Imports and exports come first
      ("an import after a defn",                   importsFailAt "SYN0000" ("inline-test", 1, 15) ["Imports and exports come first", "Perhaps move it to the top of the file"] []
                                                                 "(defn f () 1) (import std/base) (graph g (:r 1))")
    , ("an export after a graph",                  importsFailAt "SYN0000" ("inline-test", 1, 18) ["Imports and exports come first"] []
                                                                 "(graph g (:r 1)) (export f) (defn f () 1)")

      -- Writing an import
    , ("an import with no module",                 importsFailAt "SYN0000" ("inline-test", 1, 8) ["An import is written (import path) or (import path as name)"] []
                                                                 "(import) (graph g (:r 1))")
    , ("an import with as and no name",            importsFailAt "SYN0000" ("inline-test", 1, 18) ["An import is written (import path) or (import path as name)"] []
                                                                 "(import std/base as) (graph g (:r 1))")
    , ("an import of two modules",                 importsFailAt "SYN0000" ("inline-test", 1, 18) ["An import is written (import path) or (import path as name)"] []
                                                                 "(import std/base extra) (graph g (:r 1))")
    , ("a path with an empty part inside",         importsFailAt "SYN0000" ("inline-test", 1, 1) ["The module path 'a//b' has an empty part", "names joined by '/'"] []
                                                                 "(import a//b) (graph g (:r 1))")
    , ("a path ending in /",                       importsFailAt "SYN0000" ("inline-test", 1, 1) ["The module path 'a/' has an empty part"] []
                                                                 "(import a/) (graph g (:r 1))")
    , ("a path starting with /",                   importsFailAt "SYN0000" ("inline-test", 1, 1) ["The module path '/a' has an empty part"] []
                                                                 "(import /a) (graph g (:r 1))")
    , ("a path is written with /, never \\",       importsFailAt "SYN0000" ("inline-test", 1, 12) ["Unexpected token"] []
                                                                 "(import std\\base) (graph g (:r 1))")
    , ("an alias with a /",                        importsFailAt "SYN0000" ("inline-test", 1, 21) ["The import's name 'a/b' has a '/' in it", "so it is one name"] []
                                                                 "(import std/base as a/b) (graph g (:r 1))")
    , ("one module imported twice",                importsFailAt "SYN0000" ("inline-test", 1, 19) ["Two imports are both qualified 'base': std/base and std/base.", "(import std/base as another-name)"] []
                                                                 "(import std/base) (import std/base) (graph g (:r 1))")
    , ("two modules with one last part",           importsFailAt "SYN0000" ("inline-test", 1, 19) ["Two imports are both qualified 'people': a/people and b/people."] [peopleA, peopleB]
                                                                 "(import a/people) (import b/people) (graph g (:r 1))")

      -- Writing an export
    , ("a second export",                          importsFailAt "SYN0000" ("inline-test", 1, 12) ["A module has one export", "Perhaps join them into one: (export f g)"] []
                                                                 "(export f) (export h) (defn f () 1) (defn h () 2) (graph g (:r 1))")
    , ("an empty export",                          importsFailAt "SYN0000" ("inline-test", 1, 1) ["An export names at least one function"] []
                                                                 "(export) (graph g (:r 1))")
    , ("an export of a value",                     importsFailAt "SYN0000" ("inline-test", 1, 9) ["An export names the module's functions"] []
                                                                 "(export 1) (graph g (:r 1))")
    , ("an export of nothing defined",             importsFailAt "SYN0000" ("inline-test", 1, 9) ["The module exports 'nope', but defines no function by that name", "A module exports its own functions only"] []
                                                                 "(export nope) (graph g (:r 1))")
    , ("an export of a near name",                 importsFailAt "SYN0000" ("inline-test", 1, 9) ["The module exports 'mien'", "Perhaps you meant 'mine'"] []
                                                                 "(export mien) (defn mine () 1) (graph g (:r 1))")
    , ("an export of a graph",                     importsFailAt "SYN0000" ("inline-test", 1, 9) ["The module exports 'g', but defines no function by that name"] []
                                                                 "(export g) (graph g (:r 1))")
    , ("a function exported twice",                importsFailAt "SYN0000" ("inline-test", 1, 11) ["The function 'f' is exported twice", "Perhaps remove one of them"] []
                                                                 "(export f f) (defn f () 1) (graph g (:r 1))")

      -- A '/' qualifies an imported name, and nothing else
    , ("a defn named with a /",                    importsFailAt "SYN0000" ("inline-test", 1, 1) ["The function 'std/inc' has a '/' in its name", "such as 'std-inc'"] []
                                                                 "(defn std/inc (n) n) (graph g (:r 1))")
    , ("a let binding named with a /",             importsFailAt "SYN0000" ("inline-test", 1, 16) ["The name 'std/x' has a '/' in it", "Perhaps use '-' instead"] []
                                                                 "(graph g (let ($std/x 1) (:r 1)))")
    , ("a parameter named with a /",               importsFailAt "SYN0000" ("inline-test", 1, 1) ["The parameter 'a/b' has a '/' in it", "Perhaps use '-' instead"] []
                                                                 "(defn f (a/b) 1) (graph g (:r (f 1)))")
    , ("a fn's parameter named with a /",          importsFailAt "SYN0000" ("inline-test", 1, 15) ["The parameter 'a/b' has a '/' in it"] []
                                                                 "(graph g (:r ((fn (a/b) 1) 1)))")
    , ("a pattern's hole named with a /",          importsFailAt "SYN0000" ("inline-test", 1, 1) ["The parameter 'a/b' has a '/' in it"] []
                                                                 "(defn f ((?a/b)) 1) (graph g (:r 1))")

      -- Finding a module
    , ("a module that does not exist",             importsFailAt "SCP0006" ("inline-test", 1, 1) ["missingModule = \"nope\"", "The bundled modules are: std/base, std/time, std/vocab."] []
                                                                 "(import nope) (graph g (:r 1))")
    , ("a module near a bundled one",              importsFailAt "SCP0006" ("inline-test", 1, 1) ["missingModule = \"std/bsae\"", "Perhaps you meant 'std/base'"] []
                                                                 "(import std/bsae) (graph g (:r 1))")
    , ("a folder of bundled modules is not one",   importsFailAt "SCP0006" ("inline-test", 1, 1) ["missingModule = \"std\"", "'std' is a folder of modules, not a module: perhaps (import std/base)", "Its modules are: std/base, std/time, std/vocab."] []
                                                                 "(import std) (graph g (:r 1))")
    , ("a missing module, imported second",        importsFailAt "SCP0006" ("inline-test", 1, 19) ["missingModule = \"nope\""] []
                                                                 "(import std/base) (import nope) (graph g (:r 1))")
    , ("a missing module, inside a module",        importsFailAt "SCP0006" ("needy.ouro", 2, 1) ["missingModule = \"nope\""] [("needy", "(export f)\n(import nope)\n(defn f () 1)")]
                                                                 "(import needy) (graph g (:r 1))")

      -- Cycles
    , ("a module importing itself",                importsFailAt "SCP0009" ("a.ouro", 1, 1) ["cyclePath = [\"a\",\"a\"]", "Modules cannot import each other in a circle", "Ouro has no recursion"] [("a", "(import a)\n(export f)\n(defn f () 1)")]
                                                                 "(import a) (graph g (:r (a/f)))")
    , ("two modules importing each other",         importsFailAt "SCP0009" ("b.ouro", 1, 1) ["cyclePath = [\"a\",\"b\",\"a\"]", "Perhaps move what they share into a module of its own"] [("a", "(import b)\n(export f)\n(defn f () 1)"), ("b", "(import a)\n(export f)\n(defn f () 2)")]
                                                                 "(import a) (graph g (:r (a/f)))")
    , ("three modules in a circle",                importsFailAt "SCP0009" ("c.ouro", 2, 1) ["cyclePath = [\"a\",\"b\",\"c\",\"a\"]"] [("a", "(import b)\n(export f)\n(defn f () 1)"), ("b", "(import c)\n(export f)\n(defn f () 2)"), ("c", "(export f)\n(import a)\n(defn f () 3)")]
                                                                 "(import a) (graph g (:r (a/f)))")

      -- Errors inside a module are reported at their own line and column. An unterminated
      -- string is reported where the input ends, not at its opening quote: pinned as it is
    , ("a lexical error inside a module",          importsFailAt "SYN0000" ("unterminated.ouro", 2, 18) ["Malformed or unrecognized lexical token sequence"] [("unterminated", "(export f)\n(defn f () \"open)")]
                                                                 "(import unterminated) (graph g (:r 1))")
    , ("a parse error inside a module",            importsFailAt "SYN0000" ("unparsed.ouro", 2, 8) ["Expected argument list"] [unparsed]
                                                                 "(import unparsed) (graph g (:r 1))")
    , ("a bad export inside a module",             importsFailAt "SYN0000" ("misexported.ouro", 1, 9) ["The module exports 'g'"] [("misexported", "(export g)\n(defn f () 1)")]
                                                                 "(import misexported) (graph g (:r 1))")
    , ("an error in an imported function's body",  importsFailAt "SCP0003" ("broken-body.ouro", 2, 22) ["attemptedIndex = 5"] [("broken-body", "(export bad)\n(defn bad () (get () 5))")]
                                                                 "(import broken-body) (graph g (:r (broken-body/bad)))")

      -- Qualified names
    , ("a bundled module that is not imported",    importsFailAt "SCP0007" ("inline-test", 1, 11) ["qualifierName = \"base\"", "perhaps add (import std/base) at the top of the file"] []
                                                                 "(graph g (base/name \"A\"))")
    , ("a qualifier near an import",               importsFailAt "SCP0007" ("inline-test", 1, 29) ["qualifierName = \"basee\"", "Perhaps you meant 'base'"] []
                                                                 "(import std/base) (graph g (basee/name \"A\"))")
    , ("a qualifier in a file with no imports",    importsFailAt "SCP0007" ("inline-test", 1, 11) ["qualifierName = \"other\"", "This file imports nothing"] []
                                                                 "(graph g (other/f 1))")
    , ("a qualifier far from every import",        importsFailAt "SCP0007" ("inline-test", 1, 29) ["qualifierName = \"zzz\"", "This file imports: base."] []
                                                                 "(import std/base) (graph g (zzz/f 1))")
    , ("a name the module does not export",        importsFailAt "SCP0008" ("inline-test", 1, 29) ["unexportedName = \"nam\"", "exports: aat, description, identifier,", "Perhaps you meant 'name'"] []
                                                                 "(import std/base) (graph g (base/nam \"A\"))")
    , ("a name not exported, inside a module",     importsFailAt "SCP0008" ("uses.ouro", 3, 13) ["unexportedName = \"nope\""] [("uses", "(import std/base)\n(export f)\n(defn f () (base/nope))")]
                                                                 "(import uses) (graph g (:r (uses/f)))")
    , ("a qualifier unknown inside a module",      importsFailAt "SCP0007" ("uses.ouro", 2, 13) ["qualifierName = \"base\"", "perhaps add (import std/base)"] [("uses", "(export f)\n(defn f () (base/name \"A\"))")]
                                                                 "(import uses) (graph g (:r (uses/f)))")

      -- A failed import stops the compile: nothing after it is evaluated, so nothing echoes it
    , ("a missing module is the only error",       importsFailTimes 1 [] "(import nope) (graph g (:a (nope/f) :b (base/name 1 2)))")
    , ("as is a broken module",                    importsFailTimes 1 [unparsed] "(import unparsed) (graph g (:a (unparsed/f) :b (get () 5)))")

      -- Warnings inside a module are reported in its file
    , ("a warning inside a module",                importsWarnIn "badly-named.ouro" "The function 'badName' is not named in kebab-case" [("badly-named", "(export badName)\n(defn badName () 1)")]
                                                                 "(import badly-named) (graph g (:r (badly-named/badName)))")

      -- A module says where it was looked for, and a project's module replaces a bundled one
    , ("a missing module says where it looked",    importsFailAt "SCP0006" ("inline-test", 1, 1) ["Looked for nope.ouro.", "The bundled modules are: std/base, std/time, std/vocab."] []
                                                                 "(import nope) (graph g (:r 1))")
    , ("a project module replaces a bundled one",  importsWarnAt "WRN0005" ("inline-test", 1, 1) ["The project's module 'std/base' replaces the bundled 'std/base'", "std/base.ouro replaces the bundled 'std/base'", "Perhaps rename the file"] [mineStd]
                                                                 "(import std/base) (graph g (:r (base/core 1 2 3)))")
    , ("and is the one imported",                  importsToJSON [mineStd] "(import std/base) (graph g (:r (base/core 1 2 3)))"
                                                                          "{\"r\": \"mine\"}")
    ]

    where
    peopleA  = ("a/people", "(export f)\n(defn f () 1)")
    peopleB  = ("b/people", "(export f)\n(defn f () 2)")
    unparsed = ("unparsed", "(export f)\n(defn f)")
    mineStd  = ("std/base", "(export core)\n(defn core (a b c) \"mine\")")


-- Where a diagnostic is shown: in the file it was reported in, with that file's line. The
-- command line once showed every diagnostic in the file compiled, so an error inside an
-- imported module was printed with the wrong file's name and the wrong line.
sourceFrames :: Group
sourceFrames = Group "Modules: each diagnostic is shown in its own file"
    [ ("an error in a module, in that module",     frameOfError [brokenBody] "(import broken-body) (graph g (:r (broken-body/bad)))"
                                                                (SourceFrame "broken-body.ouro" 2 22 "(defn bad () (get () 5))"))
    , ("an error in the file, in the file",        frameOfError [] "(graph g\n  (:r (get () 5)))"
                                                                (SourceFrame "inline-test" 2 15 "  (:r (get () 5)))"))
    , ("a cycle, in the module that closes it",    frameOfError [("a", "(import b)\n(export f)\n(defn f () 1)"), ("b", "(export f)\n(import a)\n(defn f () 2)")] "(import a) (graph g (:r (a/f)))"
                                                                (SourceFrame "b.ouro" 2 1 "(import a)"))
    , ("a warning in a module, in that module",    frameOfWarning [("badly-named", "(export badName)\n(defn badName () 1)")] "(import badly-named) (graph g (:r (badly-named/badName)))"
                                                                  (SourceFrame "badly-named.ouro" 2 1 "(defn badName () 1)"))
    , ("a bundled module's line, from the compiler", withTests 1 . property $ do
                                                       -- core's line is found, not written, so reformatting base.ouro keeps the test
                                                       source <- maybe (fail "std/base is not bundled") pure (bundledFile "lib/std/base.ouro")
                                                       line   <- maybe (fail "std/base has no core") (pure . (+ 1)) (findIndex ("(defn core " `T.isPrefixOf`) (T.lines source))
                                                       sourceFrame bundledFile (at "lib/std/base.ouro" line 1) === SourceFrame "lib/std/base.ouro" line 1 "(defn core #rec (#uri id #str type #str label)")
    , ("a line past the end is empty",             withTests 1 . property $
                                                       sourceFrame (const (Just "one line")) (at "f.ouro" 3 1) === SourceFrame "f.ouro" 3 1 "")
    , ("an unknown file is empty",                 withTests 1 . property $
                                                       sourceFrame (const Nothing) (at "f.ouro" 1 1) === SourceFrame "f.ouro" 1 1 "")
    ]

    where
    brokenBody = ("broken-body", "(export bad)\n(defn bad () (get () 5))")

    at file line column = SourcePos file (mkPos line) (mkPos column)

    -- The frame of the program's first error, with the sources of the program and its modules
    frameOfError modules src expected = withTests 1 . property $
        case compileWithModules modules src of
            CompilationFailure _ (err :| _) -> sourceFrame (sourcesOf modules src) (errPos err) === expected
            compiled                        -> annotate (describe compiled) >> failure

    frameOfWarning modules src expected = withTests 1 . property $
        case compileWithModules modules src of
            CompilationSuccess (OuroWarning pos _ : _) _ -> sourceFrame (sourcesOf modules src) pos === expected
            compiled                                     -> annotate (describe compiled) >> failure

    sourcesOf modules src file
        | file == "inline-test" = Just src
        | otherwise             = lookup file [ (T.unpack path <> ".ouro", source) | (path, source) <- modules ]


-- Modules in files on disk, read as the command line reads them: from the directory of the
-- file compiled, before the bundled ones
modulesOnDisk :: Group
modulesOnDisk = Group "Modules: imported from files on disk"
    [ ("a module beside the file",                 onDiskToJSON "beside" [helpers, main' "(import helpers)\n(graph g (:r (helpers/greet \"Ann\")))"]
                                                                "{\"r\": \"Hello, Ann\"}")
    , ("a module in a folder",                     onDiskToJSON "folder" [("linked-art/people.ouro", "(export person)\n(defn person (n) (+ \"Person: \" n))"), main' "(import linked-art/people)\n(graph g (:r (people/person \"A\")))"]
                                                                "{\"r\": \"Person: A\"}")
    , ("a module importing another",               onDiskToJSON "chain" [("lower.ouro", "(export shout)\n(defn shout (s) (+ s \"!\"))"), ("middle.ouro", "(import lower)\n(export call)\n(defn call (s) (lower/shout s))"), main' "(import middle)\n(graph g (:r (middle/call \"hi\")))"]
                                                                "{\"r\": \"hi!\"}")
    , ("a module with CRLF lines and a BOM",       onDiskToJSON "crlf" [("helpers.ouro", "\xEF\xBB\xBF(export greet)\r\n(defn greet (n) (+ \"Hello, \" n))\r\n"), main' "(import helpers)\n(graph g (:r (helpers/greet \"Ann\")))"]
                                                                "{\"r\": \"Hello, Ann\"}")
    , ("the bundled std, with nothing beside it",  onDiskToJSON "bundled" [main' "(import std/base)\n(graph g (base/aat \"1\" \"A\"))"]
                                                                "{\"id\": \"http://vocab.getty.edu/aat/1\", \"type\": \"Type\", \"_label\": \"A\"}")
    , ("a project std replaces the bundled one",   onDiskToJSON "replaced" [("std/base.ouro", "(export core)\n(defn core (a b c) \"mine\")"), main' "(import std/base)\n(graph g (:r (base/core 1 2 3)))"]
                                                                "{\"r\": \"mine\"}")
    , ("and says so",                              onDiskWarns "replaced-warns" [("std/base.ouro", "(export core)\n(defn core (a b c) \"mine\")"), main' "(import std/base)\n(graph g (:r (base/core 1 2 3)))"]
                                                               (\_ -> "WRN0005") (\dir -> T.pack (dir </> "std" </> "base.ouro") <> " replaces the bundled 'std/base'"))
    , ("a missing module says where it looked",    onDiskFails "missing" [main' "(import nope)\n(graph g (:r 1))"]
                                                               (\dir -> ("SCP0006", dir </> "main.ouro", 1, 1, ["Looked for " <> T.pack (dir </> "nope.ouro")])))
    , ("an error in a module is in its file",      onDiskFails "broken" [("broken.ouro", "(export bad)\n(defn bad () (get () 5))"), main' "(import broken)\n(graph g (:r (broken/bad)))"]
                                                               (\dir -> ("SCP0003", dir </> "broken.ouro", 2, 22, ["attemptedIndex = 5"])))
    , ("a cycle on disk",                          onDiskFails "cycle" [("a.ouro", "(import b)\n(export f)\n(defn f () 1)"), ("b.ouro", "(import a)\n(export f)\n(defn f () 2)"), main' "(import a)\n(graph g (:r (a/f)))"]
                                                               (\dir -> ("SCP0009", dir </> "b.ouro", 1, 1, ["cyclePath = [\"a\",\"b\",\"a\"]"])))
    , ("a module that is not UTF-8",               onDiskFails "not-utf8" [("latin.ouro", "(export f)\n(defn f () \"caf\xE9\")"), main' "(import latin)\n(graph g (:r (latin/f)))"]
                                                               (\dir -> ("SCP0010", dir </> "main.ouro", 1, 1, ["unreadableModule = \"latin\"", "is not valid UTF-8"])))
    , ("a path written with / has one separator",  slashedPath)
    ]

    where
    helpers = ("helpers.ouro", "(export greet)\n(defn greet (n) (+ \"Hello, \" n))")
    main' source = ("main.ouro", source)


-- A file compiled by a path written with '/' names its modules with one kind of separator. On
-- Windows the modules were once named half with '/' and half with '\', as in
-- Temp/cli-mods\broken.ouro, because the module's name was joined to the path as written.
slashedPath :: Property
slashedPath = withTests 1 . property $ do
    (dir, _) <- evalIO (compileOnDisk "slashed" [("broken.ouro", "(export bad)\n(defn bad () (get () 5))"), ("main.ouro", "(import broken)\n(graph g (:r (broken/bad)))")])
    let written = map (\c -> if c == '\\' then '/' else c) (dir </> "main.ouro")
    source <- evalIO (either (error . T.unpack) id <$> readSource written)
    result <- evalIO (compileFile written "g" source)
    case result of
        CompilationFailure _ (err :| _) -> sourceName (errPos err) === normalise (dir </> "broken.ouro")
        compiled                        -> annotate (describe compiled) >> failure


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
    where
    squash = T.filter (not . isSpace)

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
    where
    -- An error's text, with its blurb, unescaped: show would double a Windows path's backslashes
    errorText (OuroError _ context) = contextText context
    contextText = \case
        AnnotatedContext blurb inner -> blurb <> "\n" <> contextText inner
        other                        -> T.pack (show other)

onDiskWarns :: FilePath -> [(FilePath, BS.ByteString)] -> (FilePath -> Text) -> (FilePath -> Text) -> Property
onDiskWarns name files code message = withTests 1 . property $ do
    (dir, result) <- evalIO (compileOnDisk name files)
    case result of
        CompilationSuccess warnings _
            -> do
               annotate (show [ (smartWarningCode context, warningText w) | w@(OuroWarning _ context) <- warnings ])
               assert (any (\w@(OuroWarning _ context) -> smartWarningCode context == code dir && message dir `T.isInfixOf` warningText w) warnings)
        failed
            -> annotate (describe failed) >> failure
