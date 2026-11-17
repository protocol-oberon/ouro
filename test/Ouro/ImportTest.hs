{-# LANGUAGE GADTs #-}

module Ouro.ImportTest
( importedModules
, imports
, moduleErrors
, sourceFrames
) where

import           Data.Char                     (isSpace)
import           Data.Foldable                 (toList)
import           Data.Functor.Identity         (runIdentity)
import           Data.List                     (findIndex, isSuffixOf, sort)
import           Data.List.NonEmpty            (NonEmpty (..))
import qualified Data.Map.Strict               as Map
import           Data.Ouro                     (CompilationResult (..),
                                                OuroError (..), OuroWarning (..),
                                                SourceFrame (..), bundledFile,
                                                compileUsing, defaultOptions,
                                                sourceFrame, toJSON)
import           Data.Ouro.Lisp.Lexer          (tokenize)
import           Data.Ouro.Lisp.Module.Bundled (bundled)
import           Data.Ouro.Lisp.Module.Loader  (LoadEnv (..), Lookup (..),
                                                Resolved (..))
import qualified Data.Ouro.Lisp.Module.Types   as M
import           Data.Ouro.Lisp.Parser         (parseModule)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Hedgehog
import           System.Directory              (doesDirectoryExist,
                                                listDirectory)
import           Text.Megaparsec               (SourcePos (..), mkPos)
import           Utils                         (compileG, compileWithModules,
                                                compilesToJSON,
                                                compilesWithoutWarnings,
                                                describe, failsWith,
                                                importsFailAt, importsFailIn,
                                                importsFailTimes,
                                                importsFailWith,
                                                importsFailWithEach,
                                                importsToJSON, importsWarnIn,
                                                importsWarnTimes)


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
    , ("a local function and an import's apart",   compilesToJSON "(import std/base) (define/procedure aat (x) \"mine\") (graph g (:a (aat 1) :b (base/aat \"1\" \"A\")))"
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
                                                             "(define/procedure f () 1) (import std/base) (graph g (:r 1))")
    , ("two imports need two qualifiers",          failsWith "Perhaps name one with as"
                                                             "(import std/base) (import std/base) (graph g (:r 1))")
    , ("an alias is one name",                     failsWith "has a '/' in it"
                                                             "(import std/base as a/b) (graph g (:r 1))")
    , ("no import is qualified defn",              failsWith "would be qualified 'defn', which begins the keywords defn/const and defn/property"
                                                             "(import lib/defn) (graph g (:r 1))")
    , ("nor is one qualified define",              failsWith "would be qualified 'define', which begins the keywords define/procedure, define/property and define/constant"
                                                             "(import lib/define) (graph g (:r 1))")
    , ("as defn is a retired keyword",             failsWith "defn is now define/procedure"
                                                             "(import std/base as defn) (graph g (:r 1))")
    , ("and as define is no definition",           failsWith "'define' is not a definition"
                                                             "(import std/base as define) (graph g (:r 1))")
    , ("an export names a defn of the module",     failsWith "defines no function by that name"
                                                             "(export nope) (graph g (:r 1))")
    , ("and suggests a close one",                 failsWith "Perhaps you meant 'mine'"
                                                             "(export mien) (define/procedure mine () 1) (graph g (:r 1))")
    , ("a module has one export",                  failsWith "A module has one export"
                                                             "(export f) (export h) (define/procedure f () 1) (define/procedure h () 2) (graph g (:r 1))")
    , ("an export names a function once",          failsWith "exported twice"
                                                             "(export f f) (define/procedure f () 1) (graph g (:r 1))")
    , ("an export names at least one",             failsWith "An export names at least one function"
                                                             "(export) (graph g (:r 1))")

      -- A '/' qualifies an imported name, so no other name has one
    , ("a defn's name has no /",                   failsWith "has a '/' in its name"
                                                             "(define/procedure std/inc (n) (+ n 1)) (graph g (:r (std/inc 1)))")
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
    , ("not the importer's of the same name",      importsToJSON [helpers] "(import helpers) (define/procedure salutation () \"Bye, \") (graph g (:a (helpers/greet \"Ann\") :b (salutation)))"
                                                                          "{\"a\": \"Hello, Ann\", \"b\": \"Bye, \"}")
    , ("a module's own imports resolve in it",     importsToJSON [lower, middle] "(import middle) (graph g (:r (middle/call \"hi\")))"
                                                                                "{\"r\": \"hi!\"}")
    , ("imports are not passed on to importers",   importsFailWith ["qualifierName = \"lower\""] [lower, middle]
                                                                   "(import middle) (graph g (:r (lower/shout \"hi\")))")
    , ("nor an importer's imports to a module",    importsFailIn "unaware.ouro" ["qualifierName = \"lower\""] [lower, unaware]
                                                                   "(import lower) (import unaware) (graph g (:r (unaware/call \"hi\")))")
    , ("one path, a module of each importer's own", hostKeys)

      -- Privacy
    , ("an unexported helper is private",          importsFailWith ["unexportedName = \"salutation\""] [helpers]
                                                                   "(import helpers) (graph g (:r (helpers/salutation)))")
    , ("a module's graphs are never imported",     importsFailWith ["unexportedName = \"secret\""] [withGraph]
                                                                   "(import with-graph) (graph g (:r (with-graph/secret)))")
    , ("a module with no export offers nothing",   importsFailWith ["unexportedName = \"f\""] [("quiet", "(define/procedure f () 1)")]
                                                                   "(import quiet) (graph g (:r (quiet/f)))")

      -- The graph of imports
    , ("a module under two names",                 importsToJSON [helpers] "(import helpers) (import helpers as h) (graph g (:a (helpers/greet \"A\") :b (h/greet \"B\")))"
                                                                          "{\"a\": \"Hello, A\", \"b\": \"Hello, B\"}")
    , ("a qualifier is the path's last part",      importsToJSON [("linked-art/people", "(export person)\n(define/procedure person (n) (+ \"Person: \" n))")]
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

      -- Cycles: imports go one way
    , ("a module cannot import itself",            importsFailWith ["cyclePath = [\"a\",\"a\"]"] [("a", "(import a)\n(export f)\n(define/procedure f () 1)")]
                                                                   "(import a) (graph g (:r (a/f)))")
    , ("nor two each other",                       importsFailIn "b.ouro" ["cyclePath = [\"a\",\"b\",\"a\"]"]
                                                                   [("a", "(import b)\n(export f)\n(define/procedure f () 1)"), ("b", "(import a)\n(export f)\n(define/procedure f () 2)")]
                                                                   "(import a) (graph g (:r (a/f)))")
    , ("nor three in a circle",                    importsFailWith ["cyclePath = [\"a\",\"b\",\"c\",\"a\"]"]
                                                                   [("a", "(import b)\n(export f)\n(define/procedure f () 1)"), ("b", "(import c)\n(export f)\n(define/procedure f () 2)"), ("c", "(import a)\n(export f)\n(define/procedure f () 3)")]
                                                                   "(import a) (graph g (:r (a/f)))")

      -- Imported functions are values like any other
    , ("an imported function as a value",          compilesToJSON "(import std/base) (graph g (let ($f base/aat) (f \"1\" \"A\")))"
                                                                  "{\"id\": \"http://vocab.getty.edu/aat/1\", \"type\": \"Type\", \"_label\": \"A\"}")
    , ("with a hole",                              compilesToJSON "(import std/base) (graph g (:r (map (\"1\" \"2\") (base/aat ? \"A\"))))"
                                                                  "{\"r\": [{\"id\": \"http://vocab.getty.edu/aat/1\", \"type\": \"Type\", \"_label\": \"A\"}, {\"id\": \"http://vocab.getty.edu/aat/2\", \"type\": \"Type\", \"_label\": \"A\"}]}")
    , ("passed to map",                            compilesToJSON "(import std/base) (graph g (:r (len (map (\"A\" \"B\") base/description))))"
                                                                  "{\"r\": 2}")
    , ("as a step of ->",                          importsToJSON [helpers] "(import helpers) (graph g (:r (-> \"Ann\" (helpers/greet))))"
                                                                          "{\"r\": \"Hello, Ann\"}")
    , ("in a case condition",                      importsToJSON [("checks", "(export big)\n(define/procedure big (n) (> n 10))")]
                                                                 "(import checks) (graph g (:r (case 20 ((when (checks/big 20)) \"big\") (otherwise \"small\"))))"
                                                                 "{\"r\": \"big\"}")
    , ("a wrong number of arguments",              failsWith "nameOfFunc = \"aat\""
                                                             "(import std/base) (graph g (base/aat 1 2 3))")
    , ("a bad argument is blamed at the call",     importsFailIn "inline-test" ["expectedProperty = \"#string\""] []
                                                                   "(import std/base) (graph g (base/aat 5 \"A\"))")
    , ("an error in its body is in its module",    importsFailIn "broken-body.ouro" ["Index"] [("broken-body", "(export bad)\n(define/procedure bad () (get () 5))")]
                                                                   "(import broken-body) (graph g (:r (broken-body/bad)))")

      -- Errors and warnings inside an imported module are reported, in its file
    , ("a lexical error in a module",              importsFailIn "unterminated.ouro" ["LexicalError"] [("unterminated", "(export f)\n(define/procedure f () \"open)")]
                                                                   "(import unterminated) (graph g (:r 1))")
    , ("a parse error in a module",                importsFailIn "unparsed.ouro" ["LexicalError"] [unparsed]
                                                                   "(import unparsed) (graph g (:r 1))")
    , ("a bad export in a module",                 importsFailIn "misexported.ouro" ["defines no function by that name"] [("misexported", "(export g)\n(define/procedure f () 1)")]
                                                                   "(import misexported) (graph g (:r 1))")
    , ("a missing import in a module",             importsFailIn "needy.ouro" ["missingModule = \"nope\""] [("needy", "(import nope)\n(export f)\n(define/procedure f () 1)")]
                                                                   "(import needy) (graph g (:r 1))")
    , ("every missing import in a module",         importsFailWithEach ["\"nope\"", "\"nada\""] [("needy", "(import nope)\n(import nada)\n(export f)\n(define/procedure f () 1)")]
                                                                       "(import needy) (graph g (:r 1))")
    , ("a missing module and a broken one",        importsFailWithEach ["missingModule = \"nope\"", "Expected argument list"] [unparsed]
                                                                       "(import nope) (import unparsed) (graph g (:r 1))")
    , ("a module's warnings reach the importer",   importsWarnTimes 1 "The function 'badName' is not named in kebab-case" [badlyNamed]
                                                                    "(import badly-named) (graph g (:r (badly-named/badName)))")

      -- Names
    , ("a binding named like a qualifier",         compilesToJSON "(import std/base) (graph g (let ($base 1) (:a base :b (base/aat \"1\" \"A\"))))"
                                                                  "{\"a\": 1, \"b\": {\"id\": \"http://vocab.getty.edu/aat/1\", \"type\": \"Type\", \"_label\": \"A\"}}")
    , ("a parameter's name has no /",              failsWith "The parameter 'a/b' has a '/' in it"
                                                             "(define/procedure f (a/b) 1) (graph g (:r (f 1)))")
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
    , ("an export may come before imports",        compilesToJSON "(export f) (import std/base) (define/procedure f () 1) (graph g (:r (f)))"
                                                                  "{\"r\": 1}")

      -- The bundled std
    , ("std/base exports exactly its patterns",    failsWith "exports: aat, core, description, identifier, language, name, nationality, occupation, shape, statement, statement-kind, style, tgn, ulan, work-type."
                                                             "(import std/base) (graph g (base/nope 1))")
    , ("every bundled function is documented",     stdIsDocumented)
    , ("bundled modules are named with /",        bundledNamesUseSlash)
    , ("every file in lib/ is bundled",            bundledMatchesLib)
    ]

    where
    helpers     = ("helpers", "(export greet)\n(define/procedure salutation () \"Hello, \")\n(define/procedure greet (n) (+ (salutation) n))")
    lower       = ("lower", "(export shout)\n(define/procedure shout (s) (+ s \"!\"))")
    middle      = ("middle", "(import lower)\n(export call)\n(define/procedure call (s) (lower/shout s))")
    unaware     = ("unaware", "(export call)\n(define/procedure call (s) (lower/shout s))")
    withGraph   = ("with-graph", "(export f)\n(define/procedure f () 1)\n(graph secret (:x 1))")
    peopleA     = ("a/people", "(export f)\n(define/procedure f () 1)")
    peopleB     = ("b/people", "(export f)\n(define/procedure f () 2)")
    base        = ("base", "(export one)\n(define/procedure one () 1)")
    left        = ("left", "(import base)\n(export f)\n(define/procedure f () (+ (base/one) 10))")
    right       = ("right", "(import base)\n(export f)\n(define/procedure f () (+ (base/one) 20))")
    badlyNamed  = ("badly-named", "(export badName)\n(define/constant badName 1)")
    usesBadly n = (n, "(import badly-named)\n(export f)\n(define/constant f (badly-named/badName))")
    unparsed    = ("unparsed", "(export f)\n(define/procedure f)")


-- The host keys each module: here a library's imports are its own, as a vendored library's
-- are, so lib/m's (import helpers) is lib/helpers, and the file's is helpers
hostKeys :: Property
hostKeys = withTests 1 . property $
    case runIdentity (compileUsing host "inline-test" "g" "(import lib/m) (import helpers) (graph g (:a (m/f) :b (helpers/v)))") of
        CompilationSuccess _ ast -> T.filter (not . isSpace) (TL.toStrict (toJSON defaultOptions ast)) === "{\"a\":1,\"b\":2}"
        failed                   -> annotate (describe failed) >> failure
    where
    sources = Map.fromList
        [ ("lib/m",       "(import helpers)\n(export f)\n(define/procedure f () (helpers/v))")
        , ("lib/helpers", "(export v)\n(define/procedure v () 1)")
        , ("helpers",     "(export v)\n(define/procedure v () 2)")
        ]
    host = LoadEnv
        { resolveModule = \importer path -> pure $
                              let key = case T.breakOn "/" <$> importer of
                                            Just (lib, rest) | not (T.null rest) -> lib <> "/" <> path
                                            _other                               -> path
                              in case Map.member key sources of
                                     True  -> Resolved key
                                     False -> Missing [T.unpack key <> ".ouro"]
        , readModule    = \key -> pure (maybe (Unreadable (T.unpack key) "No such module.") (Found (T.unpack key <> ".ouro")) (Map.lookup key sources))
        , importStack   = []
        }


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


-- The bundled modules are exactly the files in lib/. A file added there is not noticed by the
-- build: file-embed tracks the files it embedded, and neither GHC nor cabal watches the folder,
-- so a stale build would leave a new module out without a word. This test says which.
bundledMatchesLib :: Property
bundledMatchesLib = withTests 1 . property $ do
    onDisk <- evalIO (ouroFiles "lib")
    annotate "A module in lib/ is not in the build: run cabal clean, then cabal build all"
    sort onDisk === sort (map fst (Map.elems bundled))
    where
    ouroFiles dir = do
        entries <- listDirectory dir
        concat <$> traverse (inside dir) entries
    inside dir entry = do
        let path = dir <> "/" <> entry
        isFolder <- doesDirectoryExist path
        case isFolder of
            True  -> ouroFiles path
            False -> pure [ path | ".ouro" `isSuffixOf` entry ]


-- Every error modules can raise, each with its code, the file, line and column it is
-- reported at, and what its message and hint tell the reader
moduleErrors :: Group
moduleErrors = Group "Modules: every error, with its code, place and hint"
    [ -- Imports and exports come first
      ("an import after a definition",             importsFailAt "SYN0000" ("inline-test", 1, 27) ["Imports and exports come first", "Perhaps move it to the top of the file"] []
                                                                 "(define/procedure f () 1) (import std/base) (graph g (:r 1))")
    , ("an export after a graph",                  importsFailAt "SYN0000" ("inline-test", 1, 18) ["Imports and exports come first"] []
                                                                 "(graph g (:r 1)) (export f) (define/procedure f () 1)")

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
                                                                 "(export f) (export h) (define/procedure f () 1) (define/procedure h () 2) (graph g (:r 1))")
    , ("an empty export",                          importsFailAt "SYN0000" ("inline-test", 1, 1) ["An export names at least one function"] []
                                                                 "(export) (graph g (:r 1))")
    , ("an export of a value",                     importsFailAt "SYN0000" ("inline-test", 1, 9) ["An export names the module's functions"] []
                                                                 "(export 1) (graph g (:r 1))")
    , ("an export of nothing defined",             importsFailAt "SYN0000" ("inline-test", 1, 9) ["The module exports 'nope', but defines no function by that name", "A module exports its own functions only"] []
                                                                 "(export nope) (graph g (:r 1))")
    , ("an export of a near name",                 importsFailAt "SYN0000" ("inline-test", 1, 9) ["The module exports 'mien'", "Perhaps you meant 'mine'"] []
                                                                 "(export mien) (define/procedure mine () 1) (graph g (:r 1))")
    , ("an export of a graph",                     importsFailAt "SYN0000" ("inline-test", 1, 9) ["The module exports 'g', but defines no function by that name"] []
                                                                 "(export g) (graph g (:r 1))")
    , ("a function exported twice",                importsFailAt "SYN0000" ("inline-test", 1, 11) ["The function 'f' is exported twice", "Perhaps remove one of them"] []
                                                                 "(export f f) (define/procedure f () 1) (graph g (:r 1))")

      -- A '/' qualifies an imported name, and nothing else
    , ("a defn named with a /",                    importsFailAt "SYN0000" ("inline-test", 1, 1) ["The function 'std/inc' has a '/' in its name", "such as 'std-inc'"] []
                                                                 "(define/procedure std/inc (n) n) (graph g (:r 1))")
    , ("a let binding named with a /",             importsFailAt "SYN0000" ("inline-test", 1, 16) ["The name 'std/x' has a '/' in it", "Perhaps use '-' instead"] []
                                                                 "(graph g (let ($std/x 1) (:r 1)))")
    , ("a parameter named with a /",               importsFailAt "SYN0000" ("inline-test", 1, 1) ["The parameter 'a/b' has a '/' in it", "Perhaps use '-' instead"] []
                                                                 "(define/procedure f (a/b) 1) (graph g (:r (f 1)))")
    , ("a fn's parameter named with a /",          importsFailAt "SYN0000" ("inline-test", 1, 15) ["The parameter 'a/b' has a '/' in it"] []
                                                                 "(graph g (:r ((fn (a/b) 1) 1)))")
    , ("a pattern's hole named with a /",          importsFailAt "SYN0000" ("inline-test", 1, 1) ["The parameter 'a/b' has a '/' in it"] []
                                                                 "(define/procedure f ((?a/b)) 1) (graph g (:r 1))")

      -- Finding a module
    , ("a module that does not exist",             importsFailAt "SCP0006" ("inline-test", 1, 1) ["missingModule = \"nope\"", "The bundled modules are in the folders aat and std, and importing a folder, as (import std), lists its modules."] []
                                                                 "(import nope) (graph g (:r 1))")
    , ("a module near a bundled one",              importsFailAt "SCP0006" ("inline-test", 1, 1) ["missingModule = \"std/bsae\"", "Perhaps you meant 'std/base'"] []
                                                                 "(import std/bsae) (graph g (:r 1))")
    , ("a folder of bundled modules is not one",   importsFailAt "SCP0006" ("inline-test", 1, 1) ["missingModule = \"std\"", "'std' is a folder of modules, not a module: perhaps (import std/base)", "Its modules are: std/actor, std/base, std/digital, std/exhibition, std/object, std/provenance, std/time, std/vocab."] []
                                                                 "(import std) (graph g (:r 1))")
    , ("a missing module, imported second",        importsFailAt "SCP0006" ("inline-test", 1, 19) ["missingModule = \"nope\""] []
                                                                 "(import std/base) (import nope) (graph g (:r 1))")
    , ("a missing module, inside a module",        importsFailAt "SCP0006" ("needy.ouro", 2, 1) ["missingModule = \"nope\""] [("needy", "(export f)\n(import nope)\n(define/procedure f () 1)")]
                                                                 "(import needy) (graph g (:r 1))")

      -- Cycles
    , ("a module importing itself",                importsFailAt "SCP0009" ("a.ouro", 1, 1) ["cyclePath = [\"a\",\"a\"]", "Modules cannot import each other in a circle", "Imports go one way"] [("a", "(import a)\n(export f)\n(define/procedure f () 1)")]
                                                                 "(import a) (graph g (:r (a/f)))")
    , ("two modules importing each other",         importsFailAt "SCP0009" ("b.ouro", 1, 1) ["cyclePath = [\"a\",\"b\",\"a\"]", "Perhaps move what they share into a module of its own"] [("a", "(import b)\n(export f)\n(define/procedure f () 1)"), ("b", "(import a)\n(export f)\n(define/procedure f () 2)")]
                                                                 "(import a) (graph g (:r (a/f)))")
    , ("three modules in a circle",                importsFailAt "SCP0009" ("c.ouro", 2, 1) ["cyclePath = [\"a\",\"b\",\"c\",\"a\"]"] [("a", "(import b)\n(export f)\n(define/procedure f () 1)"), ("b", "(import c)\n(export f)\n(define/procedure f () 2)"), ("c", "(export f)\n(import a)\n(define/procedure f () 3)")]
                                                                 "(import a) (graph g (:r (a/f)))")

      -- Errors inside a module are reported at their own line and column. An unterminated
      -- string is reported where the input ends, not at its opening quote: pinned as it is
    , ("a lexical error inside a module",          importsFailAt "SYN0000" ("unterminated.ouro", 2, 24) ["This string is never closed"] [("unterminated", "(export f)\n(define/procedure f () \"open)")]
                                                                 "(import unterminated) (graph g (:r 1))")
    , ("a parse error inside a module",            importsFailAt "SYN0000" ("unparsed.ouro", 2, 20) ["Expected argument list"] [unparsed]
                                                                 "(import unparsed) (graph g (:r 1))")
    , ("a bad export inside a module",             importsFailAt "SYN0000" ("misexported.ouro", 1, 9) ["The module exports 'g'"] [("misexported", "(export g)\n(define/procedure f () 1)")]
                                                                 "(import misexported) (graph g (:r 1))")
    , ("an error in an imported function's body",  importsFailAt "SCP0003" ("broken-body.ouro", 2, 34) ["attemptedIndex = 5"] [("broken-body", "(export bad)\n(define/procedure bad () (get () 5))")]
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
    , ("a name the module does not export",        importsFailAt "SCP0008" ("inline-test", 1, 29) ["unexportedName = \"nam\"", "exports: aat, core, description, identifier,", "Perhaps you meant 'name'"] []
                                                                 "(import std/base) (graph g (base/nam \"A\"))")
    , ("a name not exported, inside a module",     importsFailAt "SCP0008" ("uses.ouro", 3, 25) ["unexportedName = \"nope\""] [("uses", "(import std/base)\n(export f)\n(define/procedure f () (base/nope))")]
                                                                 "(import uses) (graph g (:r (uses/f)))")
    , ("a qualifier unknown inside a module",      importsFailAt "SCP0007" ("uses.ouro", 2, 25) ["qualifierName = \"base\"", "perhaps add (import std/base)"] [("uses", "(export f)\n(define/procedure f () (base/name \"A\"))")]
                                                                 "(import uses) (graph g (:r (uses/f)))")

      -- A failed import stops the compile: nothing after it is evaluated, so nothing echoes it
    , ("a missing module is the only error",       importsFailTimes 1 [] "(import nope) (graph g (:a (nope/f) :b (base/name 1 2)))")
    , ("as is a broken module",                    importsFailTimes 1 [unparsed] "(import unparsed) (graph g (:a (unparsed/f) :b (get () 5)))")

      -- Warnings inside a module are reported in its file
    , ("a warning inside a module",                importsWarnIn "badly-named.ouro" "The function 'badName' is not named in kebab-case" [("badly-named", "(export badName)\n(define/procedure badName () 1)")]
                                                                 "(import badly-named) (graph g (:r (badly-named/badName)))")

      -- A module says where it was looked for, and std/… is always the bundled module
    , ("a missing module says where it looked",    importsFailAt "SCP0006" ("inline-test", 1, 1) ["Looked for nope.ouro.", "The bundled modules are in the folders aat and std, and importing a folder, as (import std), lists its modules."] []
                                                                 "(import nope) (graph g (:r 1))")
    , ("a project's std/base is not imported",     importsToJSON [mineStd] "(import std/base) (graph g (base/aat \"1\" \"A\"))"
                                                                          "{\"id\": \"http://vocab.getty.edu/aat/1\", \"type\": \"Type\", \"_label\": \"A\"}")
    , ("nor a std/ module only the project has",   importsFailAt "SCP0006" ("inline-test", 1, 1) ["missingModule = \"std/nope\""] [("std/nope", "(export f)\n(define/procedure f () 1)")]
                                                                 "(import std/nope) (graph g (:r 1))")
    ]

    where
    peopleA  = ("a/people", "(export f)\n(define/procedure f () 1)")
    peopleB  = ("b/people", "(export f)\n(define/procedure f () 2)")
    unparsed = ("unparsed", "(export f)\n(define/procedure f)")
    mineStd  = ("std/base", "(export core)\n(define/procedure core (a b c) \"mine\")")


-- Where a diagnostic is shown: in the file it was reported in, with that file's line. The
-- command line once showed every diagnostic in the file compiled, so an error inside an
-- imported module was printed with the wrong file's name and the wrong line.
sourceFrames :: Group
sourceFrames = Group "Modules: each diagnostic is shown in its own file"
    [ ("an error in a module, in that module",     frameOfError [brokenBody] "(import broken-body) (graph g (:r (broken-body/bad)))"
                                                                (SourceFrame "broken-body.ouro" 2 34 "(define/procedure bad () (get () 5))"))
    , ("an error in the file, in the file",        frameOfError [] "(graph g\n  (:r (get () 5)))"
                                                                (SourceFrame "inline-test" 2 15 "  (:r (get () 5)))"))
    , ("a cycle, in the module that closes it",    frameOfError [("a", "(import b)\n(export f)\n(define/procedure f () 1)"), ("b", "(export f)\n(import a)\n(define/procedure f () 2)")] "(import a) (graph g (:r (a/f)))"
                                                                (SourceFrame "b.ouro" 2 1 "(import a)"))
    , ("a warning in a module, in that module",    frameOfWarning [("badly-named", "(export badName)\n(define/procedure badName () 1)")] "(import badly-named) (graph g (:r (badly-named/badName)))"
                                                                  (SourceFrame "badly-named.ouro" 2 1 "(define/procedure badName () 1)"))
    , ("a bundled module's line, from the compiler", withTests 1 . property $ do
                                                       -- core's line is found, not written, so reformatting base.ouro keeps the test
                                                       source <- maybe (fail "std/base is not bundled") pure (bundledFile "lib/std/base.ouro")
                                                       line   <- maybe (fail "std/base has no core") (pure . (+ 1)) (findIndex ("(define/procedure core " `T.isPrefixOf`) (T.lines source))
                                                       sourceFrame bundledFile (at "lib/std/base.ouro" line 1) === SourceFrame "lib/std/base.ouro" line 1 "(define/procedure core #record (#uri id #string type #string label)")
    , ("a line past the end is empty",             withTests 1 . property $
                                                       sourceFrame (const (Just "one line")) (at "f.ouro" 3 1) === SourceFrame "f.ouro" 3 1 "")
    , ("an unknown file is empty",                 withTests 1 . property $
                                                       sourceFrame (const Nothing) (at "f.ouro" 1 1) === SourceFrame "f.ouro" 1 1 "")
    ]

    where
    brokenBody = ("broken-body", "(export bad)\n(define/procedure bad () (get () 5))")

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
