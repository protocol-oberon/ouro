module Ouro.AnalysisTest
( calls
, names
, recursion
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


-- A call written with a function's name, of the module or exported by an import, has as many
-- arguments as the function has parameters, wherever it is written
calls :: Group
calls = Group "Static analysis: a call of a known function has its arity"
    [ ("an uncalled function's wrong arity",      importsFailAt "SCP0002" ("inline-test", 1, 27) ["nameOfFunc = \"f\"", "It is called (f x)."] []
                                                                "(defn f (x) x) (defn h () (f 1 2)) (graph g (:r 1))")
    , ("an imported function's, by its own name", importsFailAt "SCP0002" ("inline-test", 2, 12) ["nameOfFunc = \"f\"", "It is called (f x)."]
                                                                [("lib", "(export f)\n(defn f (x) x)")]
                                                                "(import lib)\n(defn h () (lib/f 1 2))\n(graph g (:r 1))")
    , ("in a branch not taken",                   failsWith "IncorrectArity"
                                                                "(defn f (x) x) (graph g (:r (case 1 ((= 1) 1) (otherwise (f)))))")
    , ("a constant called with an argument",      failsWith "IncorrectArity"
                                                                "(defn c () 1) (defn h () (c 1)) (graph g (:r 1))")
    , ("a name bound around the call is its own", compilesToJSON "(defn f (x) x) (graph g (let ($f (fn (a b) a)) (:r (f 1 2))))"
                                                                 "{\"r\": 1}")
    ]


-- A function that reaches itself by its name, a defn or a fn bound by name, is recursion,
-- reported once for each circle of functions, at the name that closes it, whether or not
-- anything calls it. A function reached through a value is the evaluator's to check.
recursion :: Group
recursion = Group "Static analysis: no function reaches itself by its name"
    [ ("an uncalled function's recursion",        importsFailAt "TYP0000" ("inline-test", 1, 13) ["Recursion is strictly forbidden in Ouro: 'f' calls itself."] []
                                                                "(defn f () (f)) (graph g (:r 1))")
    , ("mutual recursion, at the name that closes it", importsFailAt "TYP0000" ("inline-test", 1, 33) ["'f' calls itself, through f "] []
                                                                "(defn f (n) (h n)) (defn h (n) (f n)) (graph g (:r 1))")
    , ("once for each circle",                    importsFailTimes 1 []
                                                                "(defn f (n) (h n)) (defn h (n) (f n)) (graph g (:r 1))")
    , ("a constant naming itself",                importsFailAt "TYP0000" ("inline-test", 1, 12) ["'a' calls itself."] []
                                                                "(defn a () a) (graph g (:r 1))")
    , ("a function passed to map by its name",    failsWith "'f' calls itself."
                                                                "(defn f (x) (map f x)) (graph g (:r 1))")
    , ("a function in its own pattern",           failsWith "'f' calls itself."
                                                                "(defn f (((f 1) ?x)) x) (graph g (:r 1))")
    , ("let-bound fns naming each other",         importsFailAt "TYP0000" ("inline-test", 1, 46) ["'a' calls itself, through a "] []
                                                                "(graph g (let ($a (fn (x) (b x)) $b (fn (x) (a x))) (:r 1)))")
    , ("a function and the fn it binds",          failsWith "'g' calls itself, through g "
                                                                "(defn f (x) (let ($g (fn (y) (f y))) (g x))) (graph g (:r 1))")
    , ("a fn it returns may not name it either",  failsWith "'f' calls itself."
                                                                "(defn f () (fn () (f))) (graph g (:r 1))")

      -- Only the function itself: names are resolved as the evaluator resolves them
    , ("a parameter of its name is not the function", compilesToJSON "(defn f (f) (f 1)) (graph g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("a binding never sees itself, so names the function", failsWith "'f' calls itself, through f "
                                                                "(defn f (x) (let ($f (fn (y) (f y))) (f x))) (graph g (:r 1))")
    , ("nesting a call is not recursion",         compilesToJSON "(defn id (a) a) (graph g (:r (id (id 1))))"
                                                                 "{\"r\": 1}")
    , ("a self-application is the evaluator's",   failsWith "attempted to call itself"
                                                                "(defn w (x) (x x)) (graph g (:r (w w)))")
    ]
