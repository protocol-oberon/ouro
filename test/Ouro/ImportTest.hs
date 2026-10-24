module Ouro.ImportTest
( imports
) where

import           Data.Foldable (toList)
import           Data.Ouro     (CompilationResult (..))
import qualified Data.Text     as T
import           Hedgehog
import           Utils         (compileG, compilesToJSON,
                                compilesWithoutWarnings, failsWith)


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
