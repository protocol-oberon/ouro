{-# LANGUAGE GADTs #-}

module Ouro.ImportTest
( importedModules
, imports
, moduleErrors
) where

import           Data.Foldable                 (toList)
import qualified Data.Map.Strict               as Map
import           Data.Ouro                     (CompilationResult (..))
import           Data.Ouro.Lisp.Lexer          (tokenize)
import           Data.Ouro.Lisp.Module.Bundled (bundled)
import qualified Data.Ouro.Lisp.Module.Types   as M
import           Data.Ouro.Lisp.Parser         (parseModule)
import qualified Data.Text                     as T
import           Hedgehog
import           Utils                         (compileG, compilesToJSON,
                                                compilesWithoutWarnings,
                                                failsWith, importsFailAt,
                                                importsFailIn, importsFailTimes,
                                                importsFailWith,
                                                importsFailWithEach,
                                                importsToJSON, importsWarnIn,
                                                importsWarnTimes)


-- A module imports others with (import path), and is qualified by the path's last segment
-- or an alias; it exports functions with one (export f g). Both come first in a file. std is
-- bundled with the compiler, and holds Linked Art's own patterns.
imports :: Group
imports = Group "Modules: import, export and the bundled std"
    [ ("std's core builds a node",                 compilesToJSON "(import std) (graph g (std/core #uri \"https://a.org/1\" \"Type\" \"A\"))"
                                                                  "{\"id\": \"https://a.org/1\", \"type\": \"Type\", \"_label\": \"A\"}")
    , ("std's name builds a Name",                 compilesToJSON "(import std) (graph g (:identified_by ((std/name \"A\"))))"
                                                                  "{\"identified_by\": [{\"type\": \"Name\", \"content\": \"A\"}]}")
    , ("an alias qualifies the module",            compilesToJSON "(import std as la) (graph g (la/name \"A\"))"
                                                                  "{\"type\": \"Name\", \"content\": \"A\"}")
    , ("std compiles without warnings",            compilesWithoutWarnings "(import std) (graph g (std/name \"A\"))")
    , ("a local function and an import's apart",   compilesToJSON "(import std) (defn name (x) \"mine\") (graph g (:a (name 1) :b (std/name \"A\")))"
                                                                  "{\"a\": \"mine\", \"b\": {\"type\": \"Name\", \"content\": \"A\"}}")
    , ("/ and /= are not qualified names",         compilesToJSON "(graph g (:a (/ 6 3) :b (/= 1 2)))"
                                                                  "{\"a\": 2, \"b\": true}")

      -- Finding modules and names
    , ("the module must exist",                    failsWith "missingModule = \"nope\""
                                                             "(import nope) (graph g (:r 1))")
    , ("every missing module is reported",         everyMissingModule)
    , ("a qualifier must be imported",             failsWith "perhaps add (import std)"
                                                             "(graph g (std/name \"A\"))")
    , ("an unknown qualifier suggests one",        failsWith "Perhaps you meant 'std'"
                                                             "(import std) (graph g (stdd/name \"A\"))")
    , ("a name must be exported",                  failsWith "unexportedName = \"nope\""
                                                             "(import std) (graph g (std/nope 1))")

      -- Writing imports and exports
    , ("imports come first",                       failsWith "Imports and exports come first"
                                                             "(defn f () 1) (import std) (graph g (:r 1))")
    , ("two imports need two qualifiers",          failsWith "Perhaps name one with as"
                                                             "(import std) (import std) (graph g (:r 1))")
    , ("an alias is one name",                     failsWith "has a '/' in it"
                                                             "(import std as a/b) (graph g (:r 1))")
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
    , ("an imported function as a value",          compilesToJSON "(import std) (graph g (let ($f std/name) (f \"A\")))"
                                                                  "{\"type\": \"Name\", \"content\": \"A\"}")
    , ("with a hole",                              compilesToJSON "(import std) (graph g (:r (map (std/name ?) (\"A\" \"B\"))))"
                                                                  "{\"r\": [{\"type\": \"Name\", \"content\": \"A\"}, {\"type\": \"Name\", \"content\": \"B\"}]}")
    , ("passed to map",                            compilesToJSON "(import std) (graph g (:r (map std/name (\"A\"))))"
                                                                  "{\"r\": [{\"type\": \"Name\", \"content\": \"A\"}]}")
    , ("as a step of ->",                          importsToJSON [helpers] "(import helpers) (graph g (:r (-> \"Ann\" (helpers/greet))))"
                                                                          "{\"r\": \"Hello, Ann\"}")
    , ("in a case condition",                      importsToJSON [("checks", "(export big)\n(defn big (n) (> n 10))")]
                                                                 "(import checks) (graph g (:r (case 20 ((when (checks/big 20)) \"big\") (otherwise \"small\"))))"
                                                                 "{\"r\": \"big\"}")
    , ("a wrong number of arguments",              failsWith "nameOfFunc = \"name\""
                                                             "(import std) (graph g (std/name 1 2))")
    , ("a bad argument is blamed at the call",     importsFailIn "inline-test" ["expectedProperty = \"#str\""] []
                                                                   "(import std) (graph g (std/name 5))")
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
    , ("a binding named like a qualifier",         compilesToJSON "(import std) (graph g (let ($std 1) (:a std :b (std/name \"A\"))))"
                                                                  "{\"a\": 1, \"b\": {\"type\": \"Name\", \"content\": \"A\"}}")
    , ("a parameter's name has no /",              failsWith "The parameter 'a/b' has a '/' in it"
                                                             "(defn f (a/b) 1) (graph g (:r (f 1)))")
    , ("nor a fn's",                               failsWith "The parameter 'a/b' has a '/' in it"
                                                             "(graph g (:r ((fn (a/b) 1) 1)))")

      -- Writing imports and exports
    , ("an import names a module",                 failsWith "An import is written (import path) or (import path as name)"
                                                             "(import) (graph g (:r 1))")
    , ("as needs a name after it",                 failsWith "An import is written (import path) or (import path as name)"
                                                             "(import std as) (graph g (:r 1))")
    , ("an import names one module",               failsWith "An import is written (import path) or (import path as name)"
                                                             "(import std extra) (graph g (:r 1))")
    , ("a path has no empty part",                 failsWith "has an empty part"
                                                             "(import a//b) (graph g (:r 1))")
    , ("an export names functions",                failsWith "An export names the module's functions"
                                                             "(export 1) (graph g (:r 1))")
    , ("an export may come before imports",        compilesToJSON "(export f) (import std) (defn f () 1) (graph g (:r (f)))"
                                                                  "{\"r\": 1}")

      -- The bundled std
    , ("std exports exactly core and name",        failsWith "exports: core, name"
                                                             "(import std) (graph g (std/nope 1))")
    , ("every std function is documented",         stdIsDocumented)
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


-- Every function std defines has a (doc "..."), since std is where curators look things up
stdIsDocumented :: Property
stdIsDocumented = withTests 1 . property $
    case Map.lookup "std" bundled of
        Nothing
            -> failure
        Just (file, source)
            -> case tokenize file source >>= parseModule of
                   Left err
                       -> annotate (show err) >> failure
                   Right m
                       -> [ name | M.Function _ name Nothing _ _ <- Map.elems (M._functionRegistry m) ] === []


-- Every error modules can raise, each with its code, the file, line and column it is
-- reported at, and what its message and hint tell the reader
moduleErrors :: Group
moduleErrors = Group "Modules: every error, with its code, place and hint"
    [ -- Imports and exports come first
      ("an import after a defn",                   importsFailAt "SYN0000" ("inline-test", 1, 15) ["Imports and exports come first", "Perhaps move it to the top of the file"] []
                                                                 "(defn f () 1) (import std) (graph g (:r 1))")
    , ("an export after a graph",                  importsFailAt "SYN0000" ("inline-test", 1, 18) ["Imports and exports come first"] []
                                                                 "(graph g (:r 1)) (export f) (defn f () 1)")

      -- Writing an import
    , ("an import with no module",                 importsFailAt "SYN0000" ("inline-test", 1, 8) ["An import is written (import path) or (import path as name)"] []
                                                                 "(import) (graph g (:r 1))")
    , ("an import with as and no name",            importsFailAt "SYN0000" ("inline-test", 1, 13) ["An import is written (import path) or (import path as name)"] []
                                                                 "(import std as) (graph g (:r 1))")
    , ("an import of two modules",                 importsFailAt "SYN0000" ("inline-test", 1, 13) ["An import is written (import path) or (import path as name)"] []
                                                                 "(import std extra) (graph g (:r 1))")
    , ("a path with an empty part inside",         importsFailAt "SYN0000" ("inline-test", 1, 1) ["The module path 'a//b' has an empty part", "names joined by '/'"] []
                                                                 "(import a//b) (graph g (:r 1))")
    , ("a path ending in /",                       importsFailAt "SYN0000" ("inline-test", 1, 1) ["The module path 'a/' has an empty part"] []
                                                                 "(import a/) (graph g (:r 1))")
    , ("a path starting with /",                   importsFailAt "SYN0000" ("inline-test", 1, 1) ["The module path '/a' has an empty part"] []
                                                                 "(import /a) (graph g (:r 1))")
    , ("an alias with a /",                        importsFailAt "SYN0000" ("inline-test", 1, 16) ["The import's name 'a/b' has a '/' in it", "so it is one name"] []
                                                                 "(import std as a/b) (graph g (:r 1))")
    , ("one module imported twice",                importsFailAt "SYN0000" ("inline-test", 1, 14) ["Two imports are both qualified 'std': std and std.", "(import std as another-name)"] []
                                                                 "(import std) (import std) (graph g (:r 1))")
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
    , ("a module that does not exist",             importsFailAt "SCP0006" ("inline-test", 1, 1) ["missingModule = \"nope\"", "The bundled modules are: std."] []
                                                                 "(import nope) (graph g (:r 1))")
    , ("a module near a bundled one",              importsFailAt "SCP0006" ("inline-test", 1, 1) ["missingModule = \"sdt\"", "Perhaps you meant 'std'"] []
                                                                 "(import sdt) (graph g (:r 1))")
    , ("a missing module, imported second",        importsFailAt "SCP0006" ("inline-test", 1, 14) ["missingModule = \"nope\""] []
                                                                 "(import std) (import nope) (graph g (:r 1))")
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
    , ("a bundled module that is not imported",    importsFailAt "SCP0007" ("inline-test", 1, 11) ["qualifierName = \"std\"", "perhaps add (import std) at the top of the file"] []
                                                                 "(graph g (std/name \"A\"))")
    , ("a qualifier near an import",               importsFailAt "SCP0007" ("inline-test", 1, 24) ["qualifierName = \"stdd\"", "Perhaps you meant 'std'"] []
                                                                 "(import std) (graph g (stdd/name \"A\"))")
    , ("a qualifier in a file with no imports",    importsFailAt "SCP0007" ("inline-test", 1, 11) ["qualifierName = \"other\"", "This file imports nothing"] []
                                                                 "(graph g (other/f 1))")
    , ("a qualifier far from every import",        importsFailAt "SCP0007" ("inline-test", 1, 24) ["qualifierName = \"zzz\"", "This file imports: std."] []
                                                                 "(import std) (graph g (zzz/f 1))")
    , ("a name the module does not export",        importsFailAt "SCP0008" ("inline-test", 1, 24) ["unexportedName = \"nam\"", "exports: core, name.", "Perhaps you meant 'name'"] []
                                                                 "(import std) (graph g (std/nam \"A\"))")
    , ("a name not exported, inside a module",     importsFailAt "SCP0008" ("uses.ouro", 3, 13) ["unexportedName = \"nope\""] [("uses", "(import std)\n(export f)\n(defn f () (std/nope))")]
                                                                 "(import uses) (graph g (:r (uses/f)))")
    , ("a qualifier unknown inside a module",      importsFailAt "SCP0007" ("uses.ouro", 2, 13) ["qualifierName = \"std\"", "perhaps add (import std)"] [("uses", "(export f)\n(defn f () (std/name \"A\"))")]
                                                                 "(import uses) (graph g (:r (uses/f)))")

      -- A failed import stops the compile: nothing after it is evaluated, so nothing echoes it
    , ("a missing module is the only error",       importsFailTimes 1 [] "(import nope) (graph g (:a (nope/f) :b (std/name 1 2)))")
    , ("as is a broken module",                    importsFailTimes 1 [unparsed] "(import unparsed) (graph g (:a (unparsed/f) :b (get () 5)))")

      -- Warnings inside a module are reported in its file
    , ("a warning inside a module",                importsWarnIn "badly-named.ouro" "The function 'badName' is not named in kebab-case" [("badly-named", "(export badName)\n(defn badName () 1)")]
                                                                 "(import badly-named) (graph g (:r (badly-named/badName)))")
    ]

    where
    peopleA  = ("a/people", "(export f)\n(defn f () 1)")
    peopleB  = ("b/people", "(export f)\n(defn f () 2)")
    unparsed = ("unparsed", "(export f)\n(defn f)")
