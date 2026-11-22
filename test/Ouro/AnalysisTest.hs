module Ouro.AnalysisTest
( calls
, descending
, names
, recursion
) where

import qualified Data.Map                     as Map
import           Data.Ouro.Lisp.Eval.Builtins (builtinArity, builtinRegistry)
import qualified Data.Text                    as T
import           Hedgehog
import           Utils                        (compilesToJSON, failsWith,
                                               failsWithout, importsFailAt,
                                               importsFailTimes)


-- The static analysis checks every module as written, before anything is evaluated, so a
-- name is checked where it is written, whether or not that code is ever evaluated. Its scope
-- rules are the evaluator's: what compiles today still compiles.
names :: Group
names = Group "Static analysis: names are checked where they are written"
    [ ("an uncalled function's unbound name",     importsFailAt "SCP0000" ("inline-test", 1, 24) ["variableName = \"nope\""] []
                                                                "(define/procedure f () nope) (document g (:r 1))")
    , ("a branch not taken",                      importsFailAt "SCP0000" ("inline-test", 1, 49) ["variableName = \"nope\""] []
                                                                "(document g (:r (case 1 ((eq 1) \"a\") (otherwise nope))))")
    , ("an uncalled fn's body",                   importsFailAt "SCP0000" ("inline-test", 1, 30) ["variableName = \"nope\""] []
                                                                "(document g (let ($f (fn (x) nope)) (:r 1)))")
    , ("a qualifier never imported",              importsFailAt "SCP0007" ("inline-test", 1, 24) ["people"] []
                                                                "(define/procedure f () people/person) (document g (:r 1))")
    , ("a name its import does not export",       importsFailAt "SCP0008" ("inline-test", 2, 24) ["hidden"]
                                                                [("lib", "(export ok)\n(define/procedure ok () 1)\n(define/procedure hidden () 2)")]
                                                                "(import lib)\n(define/procedure f () lib/hidden)\n(document g (:r 1))")
    , ("an imported module's uncalled function",  importsFailAt "SCP0000" ("lib.ouro", 3, 29) ["variableName = \"nope\""]
                                                                [("lib", "(export ok)\n(define/procedure ok () 1)\n(define/procedure broken () nope)")]
                                                                "(import lib)\n(document g (:r (lib/ok)))")
    , ("every unbound name is reported",          importsFailTimes 2 []
                                                                "(define/procedure f () x) (define/procedure h () y) (document g (:r 1))")
    , ("its errors end the compile",              failsWithout "TypeMismatch"
                                                                "(define/procedure f () nope) (document g (:r (+ 1 \"a\")))")
    , ("a local name is suggested",               failsWith "Perhaps you meant: 'artist'"
                                                                "(document g (let ($artist 1) (:r artst)))")

      -- The scope rules, as the evaluator has them
    , ("a parameter's pattern binds its holes",   compilesToJSON "(define/procedure f ((?a ?b)) a) (document g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("a pattern's values cannot see the parameters", importsFailAt "SCP0000" ("inline-test", 1, 28) ["variableName = \"x\""] []
                                                                "(define/procedure f (x (:k x)) 1) (document g (:r 1))")
    , ("a case pattern binds its holes",          compilesToJSON "(define/procedure f (x) (case x ((?a ?b) a) (otherwise 0))) (document g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("an attribute pair never sees itself",     importsFailAt "SCP0000" ("inline-test", 1, 32) ["variableName = \"label\""] []
                                                                "(define/procedure f () (:label label)) (document g (:r 1))")
    , ("but sees the names around its record",    compilesToJSON "(define/procedure f (label) (:label label)) (document g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("and its siblings, in any order",          compilesToJSON "(define/procedure f () (:a b :b 1)) (document g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("a let binding never sees itself",         importsFailAt "SCP0000" ("inline-test", 1, 33) ["variableName = \"x\""] []
                                                                "(define/procedure f () (let ($x x) x)) (document g (:r 1))")
    , ("a path's keys are not names",             compilesToJSON "(define/procedure f (r) (get r nope)) (document g (:r 1))"
                                                                 "{\"r\": 1}")
    ]


-- A call written with a function's name, of the module or exported by an import, has as many
-- arguments as the function has parameters, wherever it is written
calls :: Group
calls = Group "Static analysis: a call of a known function has its arity"
    [ ("an uncalled function's wrong arity",      importsFailAt "SCP0002" ("inline-test", 1, 51) ["nameOfFunc = \"f\"", "It is called (f x)."] []
                                                                "(define/procedure f (x) x) (define/procedure h () (f 1 2)) (document g (:r 1))")
    , ("an imported function's, by its own name", importsFailAt "SCP0002" ("inline-test", 2, 24) ["nameOfFunc = \"f\"", "It is called (f x)."]
                                                                [("lib", "(export f)\n(define/procedure f (x) x)")]
                                                                "(import lib)\n(define/procedure h () (lib/f 1 2))\n(document g (:r 1))")
    , ("in a branch not taken",                   failsWith "IncorrectArity"
                                                                "(define/procedure f (x) x) (document g (:r (case 1 ((eq 1) 1) (otherwise (f)))))")
    , ("a constant called with an argument",      failsWith "IncorrectArity"
                                                                "(define/procedure c () 1) (define/procedure h () (c 1)) (document g (:r 1))")
    , ("a name bound around the call is its own", compilesToJSON "(define/procedure f (x) x) (document g (let ($f (fn (a b) a)) (:r (f 1 2))))"
                                                                 "{\"r\": 1}")

      -- A built-in takes a fixed number of arguments, checked as a function's are
    , ("a built-in's wrong arity, uncalled",      importsFailAt "SCP0002" ("inline-test", 1, 24) ["nameOfFunc = \"+\"", "+ takes two operands, (+ a b). For more, fold them: (fold (a b c) start +)."] []
                                                                "(define/procedure h () (+ 1 2 3)) (document g (:r 1))")
    , ("in a branch not taken",                   importsFailAt "SCP0002" ("inline-test", 1, 47) ["nameOfFunc = \"len\"", "len takes one Array or String: (len xs)."] []
                                                                "(document g (:r (case 1 ((eq 1) 1) (otherwise (len (1) (2))))))")
    , ("with the hint for many sets",             importsFailAt "SCP0002" ("inline-test", 1, 24) ["For more, unions takes one Array of them: (unions (a b c))."] []
                                                                "(define/procedure h () (union (1) (2) (3))) (document g (:r 1))")
    , ("and in a guard, with the target",         importsFailAt "SCP0002" ("inline-test", 1, 26) ["nameOfFunc = \"lt\""] []
                                                                "(document g (:r (case 5 ((lt 1 9) \"a\") (otherwise \"b\"))))")
    , ("a binding of a built-in's name is its own", compilesToJSON "(document g (let ($len (fn (a b) a)) (:r (len 1 2))))"
                                                                   "{\"r\": 1}")
    , ("and so is a function of the module",      compilesToJSON "(define/procedure len (a b) a) (document g (:r (len 1 2)))"
                                                                 "{\"r\": 1}")
    , ("every built-in Ouro can write has an arity", withTests 1 . property $
                                                      [ name | name <- Map.keys builtinRegistry, not ("%" `T.isPrefixOf` name), not (Map.member name builtinArity) ] === [])
    ]


-- A function that reaches itself by its name, a define/procedure or a fn bound by name, must go down into
-- a value each time round, so it finishes: some call on every way round passes a part of one
-- of its parameters. One that might not is reported once for each circle of functions, at the
-- name that closes it, whether or not anything calls it. A function reached through a value
-- is the evaluator's to check.
recursion :: Group
recursion = Group "Static analysis: recursion goes down into a value"
    [ ("an uncalled function's recursion",        importsFailAt "TYP0000" ("inline-test", 1, 25) ["'f' calls itself, and might never finish."] []
                                                                "(define/procedure f () (f)) (document g (:r 1))")
    , ("mutual recursion, at the name that closes it", importsFailAt "TYP0000" ("inline-test", 1, 57) ["'f' calls itself, through f "] []
                                                                "(define/procedure f (n) (h n)) (define/procedure h (n) (f n)) (document g (:r 1))")
    , ("once for each circle",                    importsFailTimes 1 []
                                                                "(define/procedure f (n) (h n)) (define/procedure h (n) (f n)) (document g (:r 1))")
    , ("a constant naming itself",                importsFailAt "TYP0000" ("inline-test", 1, 24) ["'a' calls itself, and might never finish."] []
                                                                "(define/procedure a () a) (document g (:r 1))")
    , ("a function in its own pattern",           failsWith "'f' calls itself, and might never finish."
                                                                "(define/procedure f (((f 1) ?x)) x) (document g (:r 1))")
    , ("let-bound fns naming each other",         importsFailAt "TYP0000" ("inline-test", 1, 49) ["'a' calls itself, through a "] []
                                                                "(document g (let ($a (fn (x) (b x)) $b (fn (x) (a x))) (:r 1)))")
    , ("a function and the fn it binds",          failsWith "'g' calls itself, through g "
                                                                "(define/procedure f (x) (let ($g (fn (y) (f y))) (g x))) (document g (:r 1))")
    , ("a fn it returns may not name it either",  failsWith "'f' calls itself, and might never finish."
                                                                "(define/procedure f () (fn () (f))) (document g (:r 1))")

      -- Only the function itself: names are resolved as the evaluator resolves them
    , ("a parameter of its name is not the function", compilesToJSON "(define/procedure f (f) (f 1)) (document g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("a binding never sees itself, so names the function", failsWith "'f' calls itself, through f "
                                                                "(define/procedure f (x) (let ($f (fn (y) (f y))) (f x))) (document g (:r 1))")
    , ("nesting a call is not recursion",         compilesToJSON "(define/procedure id (a) a) (document g (:r (id (id 1))))"
                                                                 "{\"r\": 1}")
    , ("a self-application is the evaluator's",   failsWith "called itself through a value"
                                                                "(define/procedure w (x) (x x)) (document g (:r (w w)))")

      -- What goes down: a part of a parameter, passed on
    , ("a hole of a case pattern",                compilesToJSON "(define/procedure f (n) (case n ((?h ?..t) (f t)) (otherwise 0))) (document g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("a hole of a parameter's pattern",         compilesToJSON "(define/procedure f ((?h ?..t)) (f t)) (document g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("a path into it",                          compilesToJSON "(define/procedure f (n) (f (get n part))) (document g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("a let binding of a part",                 compilesToJSON "(define/procedure f (n) (let ($p (get n part)) (f p))) (document g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("an element, through map by name",         compilesToJSON "(define/procedure f (x) (map x f)) (document g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("an element of get-or, through a fn",      compilesToJSON "(define/procedure walk (n) (map (get-or n () part) (fn (p) (walk p)))) (document g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("an element of image, through forall",     compilesToJSON "(define/procedure named (n) (forall (image n part) named)) (document g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("mutual, one call going down is enough",   compilesToJSON "(define/procedure f (n) (h n)) (define/procedure h (n) (f (get n b))) (document g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("one parameter going down is enough",      compilesToJSON "(define/procedure f (n acc) (f (get n part) (+ acc 1))) (document g (:r 1))"
                                                                 "{\"r\": 1}")
    , ("arguments that change places",            compilesToJSON "(define/procedure f (a b) (f b (get a k))) (document g (:r 1))"
                                                                 "{\"r\": 1}")

      -- What does not: the same value, a value computed, or one no part of a parameter
    , ("the same argument again",                 failsWith "'f' calls itself, and might never finish."
                                                                "(define/procedure f (n) (f n)) (document g (:r 1))")
    , ("a computed argument",                     failsWith "'f' calls itself, and might never finish."
                                                                "(define/procedure f (n) (f (+ n 1))) (document g (:r 1))")
    , ("a number counting down",                  failsWith "A number counting down is not a part."
                                                                "(define/procedure f (n) (case n (0 0) (otherwise (f (- n 1))))) (document g (:r 1))")
    , ("get-or may pass its fallback, again",     failsWith "'f' calls itself, and might never finish."
                                                                "(define/procedure f (n) (f (get-or n () part))) (document g (:r 1))")
    , ("a rest hole alone may be all of it",      failsWith "'f' calls itself, and might never finish."
                                                                "(define/procedure f ((?..t)) (f t)) (document g (:r 1))")
    , ("a let binding of the whole",              failsWith "'f' calls itself, and might never finish."
                                                                "(define/procedure f (n) (let ($m n) (f m))) (document g (:r 1))")
    , ("a sibling of the parameter's name is not it", failsWith "'f' calls itself, and might never finish."
                                                                "(define/procedure f (n) (let ($n 1 $m (get n k)) (f m))) (document g (:r 1))")
    , ("one way round that does not go down",     failsWith "'f' calls itself, and might never finish."
                                                                "(define/procedure f (n) (case n ((?h ?..t) (f t)) (otherwise (f n)))) (document g (:r 1))")
    , ("arguments that change places, neither going down", failsWith "'f' calls itself, and might never finish."
                                                                "(define/procedure f (a b) (f b a)) (document g (:r 1))")
    , ("a part captured by a fn is not the fn's parameter", failsWith "'g' calls itself, through g "
                                                                "(define/procedure f (n) (let ($g (fn (y) (f (get n k)))) (g 1))) (document g (:r 1))")
    ]


-- A call the analysis found to go down into a value runs, though its function is already
-- being called. Its arguments are evaluated first, and an error in one ends the recursion, as
-- its value. A function calling itself any other way is still stopped when it does.
descending :: Group
descending = Group "Recursion: a call that goes down into a value runs"
    [ ("a tree, at every depth",                  compilesToJSON "(define/procedure outline (n) (:label (get n _label) :parts (map (get-or n () part) outline))) (document g (:r (outline (:_label \"Altarpiece\" :part ((:_label \"Panel\") (:_label \"Predella\" :part ((:_label \"Scene\"))))))))"
                                                                 "{\"r\": {\"label\": \"Altarpiece\", \"parts\": [{\"label\": \"Panel\", \"parts\": []}, {\"label\": \"Predella\", \"parts\": [{\"label\": \"Scene\", \"parts\": []}]}]}}")
    , ("the rest of an Array",                    compilesToJSON "(define/procedure final (xs) (case xs ((?x) x) ((?h ?..t) (final t)) (otherwise 0))) (document g (:r (final (1 2 3))))"
                                                                 "{\"r\": 3}")
    , ("through a fn given to map",               compilesToJSON "(define/procedure depth (n) (map (get-or n () part) (fn (p) (depth p)))) (document g (:r (depth (:part ((:part ()))))))"
                                                                 "{\"r\": [[]]}")
    , ("mutual recursion",                        compilesToJSON "(define/procedure a (n) (case n ((?h ?..t) (b t)) (otherwise \"a\"))) (define/procedure b (n) (case n ((?h ?..t) (a t)) (otherwise \"b\"))) (document g (:r (a (1 2 3))))"
                                                                 "{\"r\": \"b\"}")
    , ("arguments that change places",            compilesToJSON "(define/procedure f (a b) (case a ((?h ?..t) (f b t)) (otherwise b))) (document g (:r (f (1 2) (3 4 5))))"
                                                                 "{\"r\": [5]}")
    , ("a let-bound fn through its sibling",      compilesToJSON "(document g (let ($a (fn (n) (case n ((?h ?..t) (b t)) (otherwise 1))) $b (fn (n) (a n))) (:r (a (1 2)))))"
                                                                 "{\"r\": 1}")

      -- An error is no part of a value, so it ends the recursion
    , ("a path past a leaf ends at its error",    failsWithout "a recursive function call"
                                                                "(define/procedure f (n) (f (get n part))) (document g (:r (f (:part (:part 1)))))")
    , ("an error in an argument ends it",         failsWith "a Number + a String"
                                                                "(define/procedure f (n junk) (case n ((?h ?..t) (f t (+ 1 \"a\"))) (otherwise 0))) (document g (:r (f (1 2) 0)))")

      -- Only the calls the analysis accepted
    , ("a descending function given itself is stopped", failsWith "called itself through a value"
                                                                "(define/procedure walk (n k) (case n ((?h ?..t) (walk t k)) (otherwise (k n k)))) (document g (:r (walk (1 2) walk)))")
    ]
