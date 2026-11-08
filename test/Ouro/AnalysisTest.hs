module Ouro.AnalysisTest
( names
) where

import           Hedgehog
import           Utils    (compilesToJSON, failsWith, failsWithout,
                           importsFailAt, importsFailTimes)


-- The static analysis checks every module as written, before anything is evaluated, so a
-- name is checked where it is written, whether or not that code is ever evaluated. Its scope
-- rules are the evaluator's: what compiles today still compiles.
names :: Group
names = Group "Static analysis: names are checked where they are written"
    [ ("an uncalled function's unbound name",     importsFailAt "SCP0000" ("inline-test", 1, 12) ["variableName = \"nope\""] []
                                                                "(defn f () nope) (graph g (:r 1))")
    , ("a branch not taken",                      importsFailAt "SCP0000" ("inline-test", 1, 45) ["variableName = \"nope\""] []
                                                                "(graph g (:r (case 1 ((= 1) \"a\") (otherwise nope))))")
    , ("an uncalled fn's body",                   importsFailAt "SCP0000" ("inline-test", 1, 27) ["variableName = \"nope\""] []
                                                                "(graph g (let ($f (fn (x) nope)) (:r 1)))")
    , ("a qualifier never imported",              importsFailAt "SCP0007" ("inline-test", 1, 12) ["people"] []
                                                                "(defn f () people/person) (graph g (:r 1))")
    , ("a name its import does not export",       importsFailAt "SCP0008" ("inline-test", 2, 12) ["hidden"]
                                                                [("lib", "(export ok)\n(defn ok () 1)\n(defn hidden () 2)")]
                                                                "(import lib)\n(defn f () lib/hidden)\n(graph g (:r 1))")
    , ("an imported module's uncalled function",  importsFailAt "SCP0000" ("lib.ouro", 3, 17) ["variableName = \"nope\""]
                                                                [("lib", "(export ok)\n(defn ok () 1)\n(defn broken () nope)")]
                                                                "(import lib)\n(graph g (:r (lib/ok)))")
    , ("every unbound name is reported",          importsFailTimes 2 []
                                                                "(defn f () x) (defn h () y) (graph g (:r 1))")
    , ("its errors end the compile",              failsWithout "TypeMismatch"
                                                                "(defn f () nope) (graph g (:r (+ 1 \"a\")))")
    , ("a local name is suggested",               failsWith "Perhaps you meant: 'artist'"
                                                                "(graph g (let ($artist 1) (:r artst)))")

      -- The scope rules, as the evaluator has them
    , ("a parameter's pattern binds its holes",   compilesToJSON "(defn f ((?a ?b)) a) (graph g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("a pattern's values cannot see the parameters", importsFailAt "SCP0000" ("inline-test", 1, 16) ["variableName = \"x\""] []
                                                                "(defn f (x (:k x)) 1) (graph g (:r 1))")
    , ("a case pattern binds its holes",          compilesToJSON "(defn f (x) (case x ((?a ?b) a) (otherwise 0))) (graph g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("an attribute pair never sees itself",     importsFailAt "SCP0000" ("inline-test", 1, 20) ["variableName = \"label\""] []
                                                                "(defn f () (:label label)) (graph g (:r 1))")
    , ("but sees the names around its record",    compilesToJSON "(defn f (label) (:label label)) (graph g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("and its siblings, in any order",          compilesToJSON "(defn f () (:a b :b 1)) (graph g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("a let binding never sees itself",         importsFailAt "SCP0000" ("inline-test", 1, 21) ["variableName = \"x\""] []
                                                                "(defn f () (let ($x x) x)) (graph g (:r 1))")
    , ("a path's keys are not names",             compilesToJSON "(defn f (r) (get r nope)) (graph g (:r 1))"
                                                                 "{\"r\": 1}")
    ]
