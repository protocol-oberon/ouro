{-# LANGUAGE GADTs #-}

module Ouro.FunctionTest
( destructuring
, documentation
, expressionBodies
, mapping
, placeholders
, plainNames
, signatures
, threading
, adjoining
, constants
, properties
, assertions
) where

import qualified Data.Map.Strict             as Map
import           Data.Ouro.Lisp.Lexer        (tokenize)
import qualified Data.Ouro.Lisp.Module.Types as M
import           Data.Ouro.Lisp.Parser       (parseModule)
import qualified Data.Text                   as T
import           Hedgehog
import           Utils                       (compilesLike, compilesToJSON,
                                              compilesWithoutWarnings,
                                              failsToCompile, failsWith,
                                              importsFailAt,
                                              warnsWith)


-- A function body is exactly one expression: its result, written exactly as it would be
-- anywhere else. Private values are bound around it with let.
expressionBodies :: Group
expressionBodies = Group "Functions: a body is one result expression"
    [ ("a literal result",                        compilesLike "(define/procedure getty! () \"http://vocab.getty.edu/aat/\") (graph g (:u (getty!)))"
                                                               "(graph g (:u \"http://vocab.getty.edu/aat/\"))")
    , ("a call's result",                         compilesLike "(define/procedure add-one! (n) (+ 1 n)) (graph g (:r (add-one! 1)))"
                                                               "(graph g (:r 2))")
    , ("an array of one",                         compilesLike "(define/procedure one! () (\"lit\")) (graph g (:r (one!)))"
                                                               "(graph g (:r (\"lit\")))")
    , ("an array",                                compilesLike "(define/procedure ids! () (1 2 3)) (graph g (:r (ids!)))"
                                                               "(graph g (:r (1 2 3)))")
    , ("a record",                                compilesLike "(define/procedure person! (who) (:type \"Person\" :_label who)) (graph g (:r (person! \"Manet\")))"
                                                               "(graph g (:r (:type \"Person\" :_label \"Manet\")))")
    , ("a let around a record",                   compilesLike (T.unlines
                                                                 [ "(define/procedure node! (x)"
                                                                 , "  (let ($next (+ x 1))"
                                                                 , "    (:value next)))"
                                                                 , "(graph g (:r (node! 1)))" ])
                                                               "(graph g (:r (:value 2)))")
    , ("a let around a primitive",               compilesLike "(define/procedure twice-next! (x) (let ($next (+ x 1)) (* next 2))) (graph g (:r (twice-next! 1)))"
                                                               "(graph g (:r 4))")
    , ("a let-bound fn in the body",             compilesLike "(define/procedure outer! () (let ($inner (fn () 7)) (inner))) (graph g (:r (outer!)))"
                                                               "(graph g (:r 7))")

    -- The old forms are errors
    , ("a spliced record body is an error",       failsToCompile "(define/procedure person! (who) :type \"Person\" :_label who) (graph g (:r (person! \"Manet\")))")
    , ("return is no longer a special form",      failsToCompile "(define/procedure id! (x) (return x)) (graph g (:r (id! 1)))")
    , ("two result expressions are an error",     failsToCompile "(define/procedure two! () 1 2) (graph g (:r (two!)))")
    , ("a define in a body is an unbound call",   failsToCompile "(define/procedure none! () (define :a 1)) (graph g (:r (none!)))")
    ]


-- Function names are plain symbols, sharing one namespace with values: whichever
-- binding is innermost wins. A trailing '!' is still allowed, but no longer required.
plainNames :: Group
plainNames = Group "Functions: names are plain symbols, in one namespace"
    [ ("a function with a plain name",             compilesLike "(define/procedure twice (x) (* 2 x)) (graph g (:r (twice 2)))"
                                                                "(graph g (:r 4))")
    , ("a constant with a plain name",             compilesLike "(define/procedure getty () \"http://vocab.getty.edu/aat/\") (graph g (:u (getty)))"
                                                                "(graph g (:u \"http://vocab.getty.edu/aat/\"))")
    , ("a local function with a plain name",       compilesLike "(define/procedure outer () (let ($inner (fn () 7)) (inner))) (graph g (:r (outer)))"
                                                                "(graph g (:r 7))")
    , ("functions passed as values",               compilesLike "(define/procedure apply-to (f x) (f x)) (define/procedure inc (n) (+ n 1)) (graph g (:r (apply-to inc 1)))"
                                                                "(graph g (:r 2))")
    , ("an inner value shadows an outer function", compilesLike "(define/procedure label () \"function\") (graph g (:label \"value\" :r label))"
                                                                "(graph g (:label \"value\" :r \"value\"))")

    -- Compiles today, and must keep compiling
    , ("a trailing ! is still allowed",            compilesLike "(define/procedure legacy! () 1) (graph g (:r (legacy!)))"
                                                                "(graph g (:r 1))")

    -- Checked by message: today every program here already fails, but for another reason
    , ("a let value shadows a top-level function", compilesLike "(define/procedure label () \"function\") (graph g (let ($label \"value\") (:r label)))"
                                                                "(graph g (:r \"value\"))")
    , ("a function defined twice",                 failsWith "defined more than once"
                                                             "(define/procedure f () 1) (define/procedure f () 2) (graph g (:r (f)))")
    , ("a function named after a special form shadows it", compilesToJSON "(define/procedure get (x) x) (graph g (:r (get 1)))"
                                                             "{\"r\": 1}")
    , ("calling a value that shadows a function",  failsWith "not a function"
                                                             "(define/procedure label () \"function\") (graph g (:label \"value\" :r (label)))")

      -- A define/procedure may take a built-in's name, and replaces it everywhere, with a warning
    , ("a defn named after a built-in warns",      warnsWith "The function 'eq' replaces the built-in 'eq'"
                                                             "(define/procedure eq (a b) true) (graph g (:r (eq 1 2)))")
    , ("= is a name like any other, not eq",       compilesToJSON "(define/procedure = (a b) true) (graph g (:r (= 1 2) :s (eq 1 2)))"
                                                                  "{\"r\": true, \"s\": false}")
    , ("it replaces calls and guards alike",       compilesToJSON "(define/procedure eq (a b) true) (graph g (:r (eq 1 2) :s (case 1 ((eq 2) \"guard\") (otherwise \"no\"))))"
                                                                  "{\"r\": true, \"s\": \"guard\"}")
    , ("other names do not warn",                  compilesWithoutWarnings "(define/procedure mine (a b) true) (graph g (:r (mine 1 2)))")

      -- Functions and graphs are named in kebab-case, with a warning otherwise
    , ("a camelCase defn warns",                   warnsWith "Perhaps rename it 'make-place'"
                                                             "(define/procedure makePlace () 1) (graph g (:r (makePlace)))")
    , ("so does a snake_case one",                 warnsWith "Perhaps rename it 'make-place'"
                                                             "(define/procedure make_place () 1) (graph g (:r (make_place)))")
    , ("and a graph",                              warnsWith "The graph 'myGraph' is not named in kebab-case"
                                                             "(graph g (:r 1)) (graph myGraph (:r 2))")
    , ("keys and local bindings are exempt",       compilesWithoutWarnings "(graph g (let ($myValue 1) (:_label \"x\" :myKey myValue)))")
    ]


-- A signature's types are built-in properties in a function's header: one between the
-- name and the parameters for the result, and one before a parameter for that one. A
-- property checks (and converts to #uri or #date) exactly as it would anywhere else. An
-- argument is checked where the call is written, and the result where the body is.
signatures :: Group
signatures = Group "Functions: signatures are properties in the header"
    [ ("a typed defn",                            compilesToJSON "(define/procedure node #record (#uri id type) (:id id :type type)) (graph g (:r (node \"https://a.org/x\" \"Type\")))"
                                                                 "{\"r\": {\"id\": \"https://a.org/x\", \"type\": \"Type\"}}")
    , ("a typed fn",                              compilesToJSON "(graph g (let ($double (fn #number (#number n) (* n 2))) (:r (double 21))))"
                                                                 "{\"r\": 42}")
    , ("an untyped parameter beside a typed one", compilesToJSON "(define/procedure pair (#number a b) (array a b)) (graph g (:r (pair 1 \"two\")))"
                                                                 "{\"r\": [1, \"two\"]}")
    , ("a typed parameter converts to a date",    compilesToJSON "(define/procedure year-end #date (#date d) (+ d (thru (years 1)))) (graph g (:r (year-end \"2020-01-01T00:00:00Z\")))"
                                                                 "{\"r\": \"2021-12-31T23:59:59Z\"}")
    , ("#record and #array in a signature",       compilesToJSON "(define/procedure wrap #array (#record r) (array r)) (graph g (:r (wrap (:a 1))))"
                                                                 "{\"r\": [{\"a\": 1}]}")
    , ("#function in a signature",                compilesToJSON "(define/procedure apply #number (#function f x) (f x)) (graph g (:r (apply (fn (n) (+ n 1)) 1)))"
                                                                 "{\"r\": 2}")
    , ("#record and #array are properties anywhere", compilesToJSON "(graph g (:r #record (:a 1) :s #array (1 2)))"
                                                                 "{\"r\": {\"a\": 1}, \"s\": [1, 2]}")

    -- Mismatches, checked by message and by where they are reported
    , ("an argument of the wrong type",           failsWith "expectedRefinement = \"#number\", actualRefinement = \"#string\""
                                                            "(define/procedure double (#number n) (* n 2)) (graph g (:r (double \"x\")))")
    , ("it is reported at the argument",          failsWith "line 1, col 68"
                                                            "(define/procedure double (#number n) (* n 2)) (graph g (:r (double \"x\")))")
    , ("an unused argument is still checked",     failsWith "expectedRefinement = \"#number\""
                                                            "(define/procedure second (#number a b) b) (graph g (:r (second \"x\" 2)))")
    , ("a result of the wrong type",              failsWith "expectedRefinement = \"#number\", actualRefinement = \"#string\""
                                                            "(define/procedure label #number (who) (+ who \"!\")) (graph g (:r (label \"a\")))")
    , ("it is reported at the body",              failsWith "line 1, col 39"
                                                            "(define/procedure label #number (who) (+ who \"!\")) (graph g (:r (label \"a\")))")
    , ("a fn result of the wrong type",           failsWith "expectedRefinement = \"#string\""
                                                            "(graph g (let ($f (fn #string (n) n)) (:r (f 1))))")
    , ("a value that is not a function",          failsWith "expectedRefinement = \"#function\""
                                                            "(define/procedure apply (#function f x) (f x)) (graph g (:r (apply 1 2)))")
    , ("a type needs a parameter after it",       failsWith "written before its name"
                                                            "(define/procedure f (#number) 1) (graph g (:r (f)))")

    -- A return type says what the result is, so a typed one-value array is not a mistake
    , ("#array silences the one-value warning",   compilesWithoutWarnings "(define/constant f #array (\"x\")) (graph g (:r (f) :s (let ($h (fn #array () (\"y\"))) (h))))")

      -- A binder, <n ⊢ number>, types a parameter, and ⊢ after the name the result: the same
      -- signatures, the requirement a refinement's name
    , ("a binder types a parameter",              compilesToJSON "(define/procedure node ⊢ record (<id ⊢ uri> type) (:id id :type type)) (graph g (:r (node \"https://a.org/x\" \"Type\")))"
                                                                 "{\"r\": {\"id\": \"https://a.org/x\", \"type\": \"Type\"}}")
    , ("and converts as a tag does",              compilesToJSON "(define/procedure year-end ⊢ date (<d ⊢ date>) (+ d (thru (years 1)))) (graph g (:r (year-end \"2020-01-01T00:00:00Z\")))"
                                                                 "{\"r\": \"2021-12-31T23:59:59Z\"}")
    , ("a pattern binder",                        compilesToJSON "(define/procedure first-of (<(?a ?b) ⊢ array>) a) (graph g (:r (first-of (1 2))))"
                                                                 "{\"r\": 1}")
    , ("a binder in a fn, and its return type",   compilesToJSON "(graph g (let ($double (fn ⊢ number (<n ⊢ number>) (* n 2))) (:r (double 21))))"
                                                                 "{\"r\": 42}")
    , ("a constant's return type",                compilesToJSON "(define/constant c ⊢ string \"a\") (graph g (:r c))"
                                                                 "{\"r\": \"a\"}")
    , ("old and new together",                    compilesToJSON "(define/procedure pair (#number a <b ⊢ string>) (array a b)) (graph g (:r (pair 1 \"two\")))"
                                                                 "{\"r\": [1, \"two\"]}")
    , ("a binder's argument is checked",          failsWith "expectedRefinement = \"#number\", actualRefinement = \"#string\""
                                                            "(define/procedure double (<n ⊢ number>) (* n 2)) (graph g (:r (double \"x\")))")
    , ("at the argument",                         failsWith "line 1, col 71"
                                                            "(define/procedure double (<n ⊢ number>) (* n 2)) (graph g (:r (double \"x\")))")
    , ("and ⊢'s result",                          failsWith "expectedRefinement = \"#number\", actualRefinement = \"#string\""
                                                            "(define/procedure label ⊢ number (who) (+ who \"!\")) (graph g (:r (label \"a\")))")
    , ("a fn's ⊢ result",                         failsWith "expectedRefinement = \"#string\""
                                                            "(graph g (let ($f (fn ⊢ string (n) n)) (:r (f 1))))")
    , ("a binder asserts, so ∈ is no binder",     failsWith "A parameter's binder is a name or a pattern, then"
                                                            "(define/procedure f (<n ∈ number>) n) (graph g (:r (f 1)))")
    , ("nor is a chain",                          failsWith "A parameter's binder is a name or a pattern, then"
                                                            "(define/procedure f (<n ⊢ number ⊢ number>) n) (graph g (:r (f 1)))")
    , ("a requirement is a name",                 failsWith "A refinement in a signature is written by its name"
                                                            "(define/procedure f (<n ⊢ (array)>) n) (graph g (:r (f 1)))")
    , ("as a return type's is",                   failsWith "A refinement in a signature is written by its name"
                                                            "(define/procedure f ⊢ 1 (n) n) (graph g (:r (f 1)))")
    , ("a constant of ⊢ string still warns",      warnsWith "The procedure 'c' takes no parameters"
                                                            "(define/procedure c ⊢ string () \"a\") (graph g (:r c))")
    ]


-- A call with '?' holes among its arguments is a function of its holes. Each '?' is a new
-- parameter, a named hole '?n' is one parameter however often it is written, and a hole
-- belongs to the call it is an argument of.
placeholders :: Group
placeholders = Group "Functions: a call with holes is a function of them"
    [ ("a call with a hole is a function",        compilesToJSON "(graph g (let ($inc (+ ? 1)) (:r (inc 41))))"
                                                                 "{\"r\": 42}")
    , ("a hole can stand for any argument",       compilesToJSON "(define/procedure node (id type) (:id id :type type)) (graph g (let ($typed (node ? \"Type\")) (:r (typed \"x\"))))"
                                                                 "{\"r\": {\"id\": \"x\", \"type\": \"Type\"}}")
    , ("each ? is a parameter, in order",         compilesToJSON "(graph g (let ($sub (- ? ?)) (:r (sub 10 3))))"
                                                                 "{\"r\": 7}")
    , ("a named hole is one parameter",           compilesToJSON "(graph g (let ($sq (* ?n ?n)) (:r (sq 7))))"
                                                                 "{\"r\": 49}")
    , ("parameters follow first appearance",      compilesToJSON "(graph g (let ($f (array ?a ?b ?a)) (:r (f 1 10))))"
                                                                 "{\"r\": [1, 10, 1]}")
    , ("an operator takes a hole for either operand", compilesToJSON "(graph g (let ($f (+ ? 1) $g (- 10 ?)) (:r (f 10) :s (g 3))))"
                                                                 "{\"r\": 11, \"s\": 7}")
    , ("special forms take holes",                compilesToJSON "(graph g (let ($second (get ? 1) $a (get ? a)) (:r (second (10 20)) :s (a (:a 5)))))"
                                                                 "{\"r\": 20, \"s\": 5}")
    , ("insert, replace-at and array take holes",  compilesToJSON "(graph g (let ($ins (insert (1 2) 0 ?) $rep (replace-at (1 2) 0 ?) $pair (array ? ?)) (:r (ins 0) :s (rep 9) :t (pair 1 2))))"
                                                                 "{\"r\": [0, 1, 2], \"s\": [9, 2], \"t\": [1, 2]}")
    , ("a placeholder is a value",                compilesToJSON "(define/procedure apply (f x) (f x)) (graph g (:r (apply (+ ? 1) 1)))"
                                                                 "{\"r\": 2}")
    , ("a placeholder closes over its scope",     compilesToJSON "(define/procedure adder (n) (+ ? n)) (graph g (let ($add10 (adder 10)) (:r (add10 5))))"
                                                                 "{\"r\": 15}")
    , ("a case body can be a placeholder",        compilesToJSON "(graph g (:r (case (1 2) ((?a ?b) (let ($add (+ ? b)) (add a))) (otherwise 0))))"
                                                                 "{\"r\": 3}")

    -- A typed hole is a typed parameter: it converts, and a mismatch is reported at the call
    , ("a typed hole converts",                   compilesToJSON "(graph g (let ($y (+ #date ? (years 1))) (:r (y \"2020-01-01T00:00:00Z\"))))"
                                                                 "{\"r\": \"2021-01-01T00:00:00Z\"}")
    , ("a named hole is typed where it is written", compilesToJSON "(graph g (let ($f (+ #number ?x ?x)) (:r (f 2))))"
                                                                 "{\"r\": 4}")
    , ("a typed hole is checked at the call",     failsWith "line 1, col 102"
                                                            "(define/procedure node (id type) (:id id :type type)) (graph g (let ($n (node #number ? \"T\")) (:r (n \"x\"))))")
    , ("an untyped hole is checked at the hole",  failsWith "line 1, col 73"
                                                            "(define/procedure double (#number n) (* n 2)) (graph g (let ($d (double ?)) (:r (d \"x\"))))")

    -- Errors
    , ("a hole belongs to the innermost call",    failsWith "a Number * an unexecuted function"
                                                            "(define/procedure apply (f x) (f x)) (graph g (:r (apply (* 2 (+ ? 1)) 3)))")
    , ("it takes one argument per parameter",     failsWith "IncorrectArity"
                                                            "(graph g (let ($inc (+ ? 1)) (:r (inc 1 2))))")
    , ("a hole outside a call is an error",       failsWith "can only be written as an argument of a call"
                                                            "(graph g (:r (1 ?)))")
    , ("a hole as a let's value is an error",     failsWith "can only be written as an argument of a call"
                                                            "(graph g (let ($b ?) (:r b)))")
    , ("a hole in a record argument is an error", failsWith "can only be written as an argument of a call"
                                                            "(define/procedure f (x) x) (graph g (:r (f (:a ?))))")
    , ("a rest hole belongs in a pattern",        failsWith "can only be written in a pattern"
                                                            "(graph g (:r (+ ?..xs 1)))")
    ]


-- (map f array) calls f on each element of an Array. Each call is an ordinary call, so
-- arity, signatures and the recursion check apply as they do anywhere else.
mapping :: Group
mapping = Group "Functions: map calls a function on each element"
    [ ("map with a placeholder",                  compilesToJSON "(graph g (:r (map (1 2 3) (+ ? 1))))"
                                                                 "{\"r\": [2, 3, 4]}")
    , ("map with a defn",                         compilesToJSON "(define/procedure label (who) (:_label who)) (graph g (:r (map (\"Manet\" \"Proust\") label)))"
                                                                 "{\"r\": [{\"_label\": \"Manet\"}, {\"_label\": \"Proust\"}]}")
    , ("map with a fn over records",              compilesToJSON "(graph g (:r (map ((:id 1) (:id 2)) (fn (r) (get r id)))))"
                                                                 "{\"r\": [1, 2]}")
    , ("map with a built-in",                     compilesToJSON "(graph g (:r (map (map (1 2) years) (+ #date \"2020-01-01T00:00:00Z\" ?))))"
                                                                 "{\"r\": [\"2021-01-01T00:00:00Z\", \"2022-01-01T00:00:00Z\"]}")
    , ("map over an empty array",                 compilesToJSON "(graph g (:r (map () (+ ? 1)) :s (map #array-empty (+ ? 1))))"
                                                                 "{\"r\": [], \"s\": []}")
    , ("map with a hole is a function",           compilesToJSON "(graph g (let ($inc-all (map ? (+ ? 1))) (:r (inc-all (1 2)))))"
                                                                 "{\"r\": [2, 3]}")
    , ("map is only reserved as a function name", compilesToJSON "(graph g (let ($map 5) (:map map :r map)))"
                                                                 "{\"map\": 5, \"r\": 5}")

    -- Each element is checked by the function's signature, and reported at the Array
    , ("a typed parameter checks each element",   failsWith "expectedRefinement = \"#number\", actualRefinement = \"#string\""
                                                            "(define/procedure double (#number n) (* n 2)) (graph g (:r (map (1 \"x\" 3) double)))")
    , ("it is reported at the array",             failsWith "line 1, col 65"
                                                            "(define/procedure double (#number n) (* n 2)) (graph g (:r (map (1 \"x\" 3) double)))")
    , ("a result is checked by the return type",  failsWith "expectedRefinement = \"#number\""
                                                            "(graph g (:r (map (\"a\") (fn #number (n) (+ n \"!\")))))")

    -- Errors
    , ("the function takes one argument",         failsWith "IncorrectArity"
                                                            "(graph g (:r (map (1 2) (fn (a b) a))))")
    , ("map needs a function",                    failsWith "a function to map with"
                                                            "(graph g (:r (map (1 2) 1)))")
    , ("map needs an array",                      failsWith "an Array to map over"
                                                            "(graph g (:r (map 5 (+ ? 1))))")
    , ("a record is not an array",                failsWith "an Array to map over"
                                                            "(graph g (:r (map (:a 1) (+ ? 1))))")
    , ("map takes an array and a function",       failsWith "map is written (map array f)"
                                                            "(graph g (:r (map (+ ? 1))))")
    , ("map can name a function, which shadows it", compilesToJSON "(define/procedure map (x) x) (graph g (:r (map 1)))"
                                                            "{\"r\": 1}")
    , ("a function maps itself until an element is no Array", failsWith "an Array to map over"
                                                            "(define/procedure f (x) (map x f)) (graph g (:r (f (1))))")
    ]


-- A parameter can be a pattern, written as a case pattern is. It takes one argument, which
-- is matched when the function is called, and its holes are bound around the body.
destructuring :: Group
destructuring = Group "Functions: a parameter can be a pattern"
    [ ("an array pattern parameter",              compilesToJSON "(graph g (let ($swap (fn ((?a ?b)) (array b a))) (:r (swap (1 2)))))"
                                                                 "{\"r\": [2, 1]}")
    , ("a record pattern parameter",              compilesToJSON "(define/procedure label-of ((:_label ?l)) l) (graph g (:r (label-of (:type \"Type\" :_label \"Beauty\"))))"
                                                                 "{\"r\": \"Beauty\"}")
    , ("a rest hole in a parameter",              compilesToJSON "(graph g (let ($tail (fn ((?h ?..t)) t)) (:r (tail (1 2 3)))))"
                                                                 "{\"r\": [2, 3]}")
      -- A head before the rest hole makes a parameter that takes at least one element, as a
      -- null result needs at least one source; the whole is rebuilt with ∪, as a set
    , ("so a parameter can take at least one element", failsWith "the shape of the parameter's pattern"
                                                            "(define/procedure sources (#string s (?first ?..rest)) s) (graph g (:r (sources \"a\" ())))")
    , ("and is rebuilt with ∪",                   compilesToJSON "(define/procedure sources ((?first ?..rest)) (∪ (array first) rest)) (graph g (:a (sources (1)) :b (sources (1 2 3)) :c (sources (1 2 1))))"
                                                                 "{\"a\": [1], \"b\": [1, 2, 3], \"c\": [1, 2]}")
    , ("a literal in a pattern is compared",      compilesToJSON "(graph g (let ($f (fn ((\"Person\" ?l)) l)) (:r (f (\"Person\" \"Manet\")))))"
                                                                 "{\"r\": \"Manet\"}")
    , ("a pattern beside a named parameter",      compilesToJSON "(define/procedure sum (base (?a ?b)) (+ base (+ a b))) (graph g (:r (sum 1 (2 3))))"
                                                                 "{\"r\": 6}")
    , ("a property before a pattern",             compilesToJSON "(graph g (let ($f (fn (#array (?a ?b)) (+ a b))) (:r (f (1 2)))))"
                                                                 "{\"r\": 3}")
    , ("map with a pattern parameter",            compilesToJSON "(graph g (:r (map ((\"allan\" \"Scott Allan\") (\"beeny\" \"Emily Beeny\")) (fn ((?slug ?label)) (:id slug :_label label)))))"
                                                                 "{\"r\": [{\"id\": \"allan\", \"_label\": \"Scott Allan\"}, {\"id\": \"beeny\", \"_label\": \"Emily Beeny\"}]}")
    , ("a defn's pattern is desugared",           compilesToJSON "(define/procedure f (((let ($n 6) n) ?x)) x) (graph g (:r (f (6 1))))"
                                                                 "{\"r\": 1}")
    , ("as a fn's pattern is",                    compilesToJSON "(graph g (let ($f (fn (((let ($n 6) n) ?x)) x)) (:r (f (6 1)))))"
                                                                 "{\"r\": 1}")

    -- A pattern's values are evaluated where the function is defined, not among its parameters
    , ("a defn's pattern sees the module",        compilesToJSON "(define/procedure kind () \"Type\") (define/procedure label-of ((:type (kind) :_label ?l)) l) (graph g (:r (label-of (:type \"Type\" :_label \"Beauty\"))))"
                                                                 "{\"r\": \"Beauty\"}")
    , ("a fn's pattern sees its scope",           compilesToJSON "(graph g (let ($k \"Type\" $f (fn ((:type k :_label ?l)) l)) (:r (f (:type \"Type\" :_label \"Beauty\")))))"
                                                                 "{\"r\": \"Beauty\"}")
    , ("but not the other parameters",            failsWith "variableName = \"kind\""
                                                            "(define/procedure label-if (kind (:type kind :_label ?l)) l) (graph g (:r (label-if \"Type\" (:type \"Type\" :_label \"Beauty\"))))")
    , ("a pattern cannot call its own function",  failsWith "a recursive function call"
                                                            "(define/procedure f (((f 1) ?x)) x) (graph g (:r (f (1 2))))")

    -- A pattern is one parameter, matched at the call, and a mismatch is reported at the argument
    , ("a pattern takes one argument",            failsWith "IncorrectArity"
                                                            "(graph g (let ($f (fn ((?a ?b)) a)) (:r (f 1 2))))")
    , ("an unused pattern is still matched",      failsWith "the shape of the parameter's pattern"
                                                            "(graph g (let ($f (fn ((?a ?b)) 1)) (:r (f 5))))")
    , ("it is reported at the argument",          failsWith "line 1, col 44"
                                                            "(graph g (let ($f (fn ((?a ?b)) 1)) (:r (f 5))))")
    , ("in map it is reported at the array",      failsWith "line 1, col 19"
                                                            "(graph g (:r (map ((1 2) (3)) (fn ((?a ?b)) a))))")
    , ("the refinement is checked before the pattern", failsWith "expectedRefinement = \"#record\""
                                                            "(graph g (let ($f (fn (#record (?a ?b)) a)) (:r (f (1 2)))))")

    -- Errors, reported where the function is defined
    , ("a pattern must bind something",           failsWith "binds nothing"
                                                            "(define/procedure f ((1 2)) 1) (graph g (:r 1))")
    , ("a name is bound once",                    failsWith "bound more than once"
                                                            "(define/procedure f (a a) a) (graph g (:r (f 1 2)))")
    , ("a hole cannot reuse a parameter's name",  failsWith "bound more than once"
                                                            "(graph g (let ($f (fn (a (?a)) a)) (:r 1)))")
    , ("a hole in a call is a malformed pattern", failsWith "MalformedPattern"
                                                            "(define/procedure f ((?a (g ?b))) a) (graph g (:r (f (1 2))))")
    , ("placeholders leave parameters alone",     failsWith "MalformedPattern"
                                                            "(graph g (let ($f (fn ((g ?b)) b)) (:r (f 1))))")
    , ("a $name is not a parameter",              failsWith "A fn is written"
                                                            "(graph g (let ($f (fn ($x) x)) (:r (f 1))))")
    ]


-- (with base item ...) is base with items written over it, layered as an inlay is, and
-- (-> x step ...) threads a value through steps, each receiving the value before it. Both are
-- rewritten by Canon before evaluation.
threading :: Group
threading = Group "Functions: with and -> build a value step by step"
    [ ("with adds attribute pairs",               compilesToJSON (base <> "(graph g (:r (with (base) :_label \"L\")))")
                                                                 "{\"r\": {\"id\": \"1\", \"type\": \"T\", \"_label\": \"L\"}}")
    , ("a written attribute pair wins",           compilesToJSON (base <> "(graph g (:r (with (base) :type \"Mine\")))")
                                                                 "{\"r\": {\"id\": \"1\", \"type\": \"Mine\"}}")
    , ("with extends an array",                   compilesToJSON "(graph g (:r (with (1 2) 3 4)))"
                                                                 "{\"r\": [1, 2, 3, 4]}")
    , ("with nothing to add is the value",        compilesToJSON (base <> "(graph g (:r (with (base))))")
                                                                 "{\"r\": {\"id\": \"1\", \"type\": \"T\"}}")
    , ("with can add a context",                  compilesToJSON (base <> "(graph g (:r (with (base) :context \"https://a.org/c.json\")))")
                                                                 "{\"r\": {\"@context\": \"https://a.org/c.json\", \"id\": \"1\", \"type\": \"T\"}}")

    -- Each step receives the value as its first argument, or where a hole marks it
    , ("-> threads with",                         compilesToJSON (base <> "(graph g (:r (-> (base) (with :type \"Mine\") (with :_label \"L\"))))")
                                                                 "{\"r\": {\"id\": \"1\", \"type\": \"Mine\", \"_label\": \"L\"}}")
    , ("a bare name is called with the value",    compilesToJSON "(define/procedure wrap (r) (array r)) (graph g (:r (-> 1 wrap)))"
                                                                 "{\"r\": [1]}")
    , ("the value goes first, or at the hole",    compilesToJSON "(graph g (:r (-> 1 (- 10 ?)) :s (-> 1 (- 10))))"
                                                                 "{\"r\": 9, \"s\": -9}")
    , ("a fn step is called with the value",      compilesToJSON "(graph g (:r (-> 1 (fn (x) (+ x 1)))))"
                                                                 "{\"r\": 2}")
    , ("(-> ? ...) is a function",                compilesToJSON "(graph g (let ($f (-> ? (+ 1) (* 2))) (:r (f 3))))"
                                                                 "{\"r\": 8}")
    , ("which can be mapped",                     compilesToJSON "(graph g (:r (map ((:a 1) (:a 2)) (-> ? (with :seen true)))))"
                                                                 "{\"r\": [{\"a\": 1, \"seen\": true}, {\"a\": 2, \"seen\": true}]}")
    , ("pipelines nest",                          compilesToJSON "(graph g (:r (-> (-> 1 (+ 1)) (* 10))))"
                                                                 "{\"r\": 20}")
    , ("special forms are steps too",             compilesToJSON "(graph g (:r (-> (:a 1) (get a)) :s (-> 5 (case ((gt 3) \"big\") (otherwise \"small\")))))"
                                                                 "{\"r\": 1, \"s\": \"big\"}")
    , ("a pipeline of no steps is its value",     compilesToJSON "(graph g (:r (-> 1)))"
                                                                 "{\"r\": 1}")
    , ("a pipeline builds a Linked Art record",   compilesToJSON ( "(define/procedure core (id type label) (:id #uri id :type type :_label label)) "
                                                                <> "(define/procedure aat (id label) (core (+ \"http://vocab.getty.edu/aat/\" id) \"Type\" label)) "
                                                                <> "(define/procedure classified (r t) (with r :classified_as (array t))) "
                                                                <> "(define/procedure named (r n) (with r :identified_by (array (:type \"Name\" :content n)))) "
                                                                <> "(graph g (-> (core \"https://linked.art/example/place/amsterdam/1\" \"Place\" \"Amsterdam\") "
                                                                <> "(with :context \"https://linked.art/ns/v1/linked-art.json\") "
                                                                <> "(classified (aat \"300008389\" \"City\")) (named \"Amsterdam\")))" )
                                                                 ( "{\"@context\": \"https://linked.art/ns/v1/linked-art.json\", \"id\": \"https://linked.art/example/place/amsterdam/1\", \"type\": \"Place\", \"_label\": \"Amsterdam\", "
                                                                <> "\"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300008389\", \"type\": \"Type\", \"_label\": \"City\"}], "
                                                                <> "\"identified_by\": [{\"type\": \"Name\", \"content\": \"Amsterdam\"}]}" ))

    -- Errors
    , ("a step receives one value",               failsWith "IncorrectArity"
                                                            "(graph g (:r (-> 1 (+ ? ?))))")
    , ("a step that is not a function",           failsWith "a function to call"
                                                            "(graph g (:r (-> 1 (:a 2))))")
    , ("-> needs a value",                        failsWith "-> is written (-> value step ...)"
                                                            "(graph g (:r (->)))")
    , ("with needs a value",                      failsWith "with is written (with value :key value ...)"
                                                            "(graph g (:r (with)))")
    , ("with cannot be a function name",          failsWith "'with' is read by its syntax before any name is bound"
                                                            "(define/procedure with (x) x) (graph g (:r 1))")
    , ("-> cannot be a function name",            failsWith "'->' is read by its syntax before any name is bound"
                                                            "(define/procedure -> (x) x) (graph g (:r 1))")
    ]
    where
    base = "(define/procedure base () (:id \"1\" :type \"T\")) "


-- (adjoin base item ...) is with's dual: where with sets a key, adjoin adds to the key's set
-- of values, a value unless it is already there. Rewritten by Canon into %adjoin, which
-- decides by the evaluated base whether it adds attribute pairs to a record or elements to
-- an array.
adjoining :: Group
adjoining = Group "Functions: adjoin adds to a property's set of values"
    [ ("a key not there holds the value",         compilesToJSON "(graph g (:r (adjoin (:t \"X\") :k 1)))"
                                                                 "{\"r\": {\"t\": \"X\", \"k\": [1]}}")
    , ("a single value becomes a set",            compilesToJSON "(graph g (:r (adjoin (:k 1) :k 2)))"
                                                                 "{\"r\": {\"k\": [1, 2]}}")
    , ("a list adds its elements",                compilesToJSON "(graph g (:r (adjoin (:k (1)) :k (array 2 3))))"
                                                                 "{\"r\": {\"k\": [1, 2, 3]}}")
    , ("a value already there is not added",      compilesToJSON "(graph g (:r (adjoin (:k (1 2)) :k 2)))"
                                                                 "{\"r\": {\"k\": [1, 2]}}")
    , ("the values there are kept as they are",   compilesToJSON "(graph g (:r (adjoin (:k (1 1)) :k (1 3))))"
                                                                 "{\"r\": {\"k\": [1, 1, 3]}}")
    , ("a key keeps its place, a new one is last", compilesToJSON "(graph g (:r (adjoin (:a 1 :k 2 :z 3) :k 4 :new 5)))"
                                                                 "{\"r\": {\"a\": 1, \"k\": [2, 4], \"z\": 3, \"new\": [5]}}")
    , ("records are compared by value",           compilesToJSON "(graph g (:r (adjoin (:k ((:id \"a\"))) :k (:id \"a\") :k2 1)))"
                                                                 "{\"r\": {\"k\": [{\"id\": \"a\"}], \"k2\": [1]}}")
    , ("an array adds elements not there",        compilesToJSON "(graph g (:r (adjoin (1 2) 2 3)))"
                                                                 "{\"r\": [1, 2, 3]}")
    , ("an inlay's attribute pairs are adjoined", compilesToJSON "(graph g (:r (adjoin (:k (1)) (inlay (:k 2 :j 3)))))"
                                                                 "{\"r\": {\"k\": [1, 2], \"j\": [3]}}")
    , ("a context is layered, as with's",         compilesToJSON "(graph g (adjoin (:k 1 :context \"https://linked.art/ns/v1/linked-art.json\") :k 2 :context \"https://e.org/c.json\"))"
                                                                 "{\"@context\": [\"https://linked.art/ns/v1/linked-art.json\", \"https://e.org/c.json\"], \"k\": [1, 2]}")
    , ("adjoin with nothing to add is the value", compilesToJSON "(graph g (:r (adjoin (:k 1))))"
                                                                 "{\"r\": {\"k\": 1}}")
    , ("adjoining twice is adjoining once",       compilesToJSON "(graph g (:r (adjoin (adjoin (:k 1) :k 2) :k 2)))"
                                                                 "{\"r\": {\"k\": [1, 2]}}")
    , ("with after adjoin sets the key",          compilesToJSON "(graph g (:r (with (adjoin (:k 1) :k 2) :k 3)))"
                                                                 "{\"r\": {\"k\": 3}}")
    , ("adjoin after with adds to it",            compilesToJSON "(graph g (:r (adjoin (with (:k 0) :k 1) :k 2)))"
                                                                 "{\"r\": {\"k\": [1, 2]}}")

    -- A step, and a function by the placeholder rule
    , ("-> threads adjoin with with",             compilesToJSON "(graph g (:r (-> (:t \"X\") (with :_label \"L\") (adjoin :k 1) (adjoin :k 1 :k2 2))))"
                                                                 "{\"r\": {\"t\": \"X\", \"_label\": \"L\", \"k\": [1], \"k2\": [2]}}")
    , ("(-> ? (adjoin ...)) can be mapped",       compilesToJSON "(graph g (:r (map ((:k 1) (:j 2)) (-> ? (adjoin :k 9)))))"
                                                                 "{\"r\": [{\"k\": [1, 9]}, {\"j\": 2, \"k\": [9]}]}")
    , ("a pipeline builds a Linked Art record",   compilesToJSON ( "(define/procedure aat (id label) (:id #uri (+ \"http://vocab.getty.edu/aat/\" id) :type \"Type\" :_label label)) "
                                                                <> "(graph g (-> (:id #uri \"https://linked.art/example/place/amsterdam/1\" :type \"Place\" :_label \"Amsterdam\") "
                                                                <> "(adjoin :classified_as (aat \"300008389\" \"City\")) "
                                                                <> "(adjoin :classified_as (array (aat \"300008389\" \"City\") (aat \"300008375\" \"Town\"))) "
                                                                <> "(adjoin :identified_by (:type \"Name\" :content \"Amsterdam\"))))" )
                                                                 ( "{\"id\": \"https://linked.art/example/place/amsterdam/1\", \"type\": \"Place\", \"_label\": \"Amsterdam\", "
                                                                <> "\"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300008389\", \"type\": \"Type\", \"_label\": \"City\"}, "
                                                                <> "{\"id\": \"http://vocab.getty.edu/aat/300008375\", \"type\": \"Type\", \"_label\": \"Town\"}], "
                                                                <> "\"identified_by\": [{\"type\": \"Name\", \"content\": \"Amsterdam\"}]}" ))

    -- Errors
    , ("an element cannot be adjoined to a record", failsWith "attribute pairs, written :key value, to adjoin to a Record"
                                                              "(graph g (:r (adjoin (:k 1) 5)))")
    , ("attribute pairs cannot be adjoined to an array", failsWith "elements to adjoin to an Array"
                                                                   "(graph g (:r (adjoin (1 2) :k 5)))")
    , ("the base is a record or an array",        failsWith "a Record or an Array to adjoin to"
                                                            "(graph g (:r (adjoin 5 :k 1)))")
    , ("a function cannot be compared",           failsWith "values adjoin can compare"
                                                            "(graph g (:r (len (adjoin (array (fn (x) x)) 1))))")
    , ("an error in a value is reported",         failsWith "a Number + a String"
                                                            "(graph g (:r (adjoin (:k 1) :k (+ 1 \"a\"))))")
    , ("a key is written once, as in with",       failsWith "The key 'k' is written twice"
                                                            "(graph g (:r (adjoin (:k 1) :k 2 :k 3)))")
    , ("adjoin needs a value",                    failsWith "adjoin is written (adjoin value :key value ...)"
                                                            "(graph g (:r (adjoin)))")
    , ("adjoin cannot be a function name",        failsWith "'adjoin' is read by its syntax before any name is bound"
                                                            "(define/procedure adjoin (x) x) (graph g (:r 1))")
    ]


-- A name of a function of no parameters, written as a value, is its result: Ouro has no
-- effects, so the function cannot be told from its value. At the head of a call it is the
-- function, so (c) still calls it.
constants :: Group
constants = Group "Functions: a function of no parameters, named, is its value"
    [ ("a constant is its value",                 compilesToJSON "(define/procedure c () 1) (graph g (:r c))"
                                                                 "{\"r\": 1}")
    , ("and (c) still calls it",                  compilesToJSON "(define/procedure c () 1) (graph g (:a c :b (c)))"
                                                                 "{\"a\": 1, \"b\": 1}")
    , ("in an array",                             compilesToJSON "(define/procedure c () 1) (graph g (:r (array c 2)))"
                                                                 "{\"r\": [1, 2]}")
    , ("as an argument",                          compilesToJSON "(define/procedure c () (:a 1)) (define/procedure f #record (#record r) (with r :b 2)) (graph g (:r (f c)))"
                                                                 "{\"r\": {\"a\": 1, \"b\": 2}}")
    , ("added by adjoin",                         compilesToJSON "(define/procedure c () 1) (graph g (adjoin (:k 0) :k c))"
                                                                 "{\"k\": [0, 1]}")
    , ("under a property",                        compilesToJSON "(define/procedure c () 1) (graph g (:r #number c))"
                                                                 "{\"r\": 1}")
    , ("a let-bound fn of no parameters",         compilesToJSON "(graph g (let ($c (fn () 2)) (:a c :b (c))))"
                                                                 "{\"a\": 2, \"b\": 2}")
    , ("one inside a function is evaluated for each call", compilesToJSON "(define/procedure make (x) (let ($k (fn () x)) k)) (graph g (:a (make 1) :b (make 2)))"
                                                                 "{\"a\": 1, \"b\": 2}")
    , ("a constant named again is the same value", compilesToJSON "(define/procedure c () (:n 1)) (define/procedure f (x) (:in x :again c)) (graph g (:a c :b (f c) :d (c)))"
                                                                 "{\"a\": {\"n\": 1}, \"b\": {\"in\": {\"n\": 1}, \"again\": {\"n\": 1}}, \"d\": {\"n\": 1}}")
    , ("define/constant writes one",              compilesToJSON "(define/constant c (:n 1)) (graph g (:a c :b (c)))"
                                                                 "{\"a\": {\"n\": 1}, \"b\": {\"n\": 1}}")
    , ("with a return type and a doc",            compilesToJSON "(define/constant c #string (doc \"A name.\") \"a\") (graph g (:r c))"
                                                                 "{\"r\": \"a\"}")
    , ("whose return type is checked",            failsWith "expectedRefinement = \"#string\""
                                                            "(define/constant c #string 1) (graph g (:r c))")
    , ("a define/procedure of no parameters warns", warnsWith "The procedure 'c' takes no parameters, so it is a constant"
                                                            "(define/procedure c () 1) (graph g (:r c))")
    , ("suggesting define/constant",              warnsWith "Perhaps define it as one: (define/constant c value)."
                                                            "(define/procedure c () 1) (graph g (:r c))")
    , ("as it does with a return type",           warnsWith "The procedure 'c' takes no parameters"
                                                            "(define/procedure c #string () \"a\") (graph g (:r c))")
    , ("a define/procedure of parameters does not", compilesWithoutWarnings "(define/procedure f (x) x) (graph g (:r (f 1)))")
    , ("nor does a fn of none",                   compilesWithoutWarnings "(graph g (:r (let ($h (fn () 1)) (h))))")
    , ("a name that begins with defn/const is a name", failsWith "'defn/constant' cannot begin a form at the top level"
                                                            "(defn/constant c 1) (graph g (:r 1))")

      -- define/procedure, define/refinement and define/constant replace defn, defn/property and
      -- defn/const; each old form is an error that names its new one
    , ("defn is now define/procedure",            failsWith "defn is now define/procedure: (define/procedure name (args) body)"
                                                            "(defn f (x) x) (graph g (:r 1))")
    , ("defn/const is now define/constant",       failsWith "defn/const is now define/constant: (define/constant name value)"
                                                            "(defn/const c 1) (graph g (:r 1))")
    , ("defn/property is now define/refinement",  failsWith "defn/property is now define/refinement: (define/refinement name (#contract x) body)"
                                                            "(defn/property p (#record r) true) (graph g (:r 1))")
    , ("at the old word",                         failsWith "line 1, col 2"
                                                            "(defn f (x) x) (graph g (:r 1))")
    , ("define on its own is not a definition",   failsWith "'define' is not a definition: say what is defined, with define/procedure, define/refinement or define/constant"
                                                            "(define f (x) x) (graph g (:r 1))")
    , ("nor is define of another kind",           failsWith "'define/graph' is not a definition"
                                                            "(define/graph g (:r 1))")
    , ("define/property is of another kind",      failsWith "'define/property' is not a definition: say what is defined, with define/procedure, define/refinement or define/constant"
                                                            "(define/property p (#record r) true) (graph g (:r 1))")
    , ("a name that begins with define is a name", compilesToJSON "(define/procedure defined-thing () 3) (graph g (:r defined-thing))"
                                                                 "{\"r\": 3}")
    , ("one constant from another",               compilesToJSON "(define/procedure a () 1) (define/procedure b () (+ a 1)) (graph g (:r b))"
                                                                 "{\"r\": 2}")
    , ("an imported constant",                    compilesToJSON "(import std/vocab) (graph g (:r vocab/artwork))"
                                                                 "{\"r\": {\"id\": \"http://vocab.getty.edu/aat/300133025\", \"type\": \"Type\", \"_label\": \"Artwork\"}}")
    , ("a fn of no parameters not named stays a function", failsWith "an unexecuted function"
                                                                     "(graph g (:r (case 1 (otherwise (fn () 3)))))")

      -- It is called as (c) is
    , ("its return type is checked",              failsWith "expectedRefinement = \"#string\""
                                                            "(define/procedure c #string () 1) (graph g (:r c))")
    , ("an error in its body is reported there",  importsFailAt "TYP0000" ("inline-test", 1, 24) ["a Number + a String"] []
                                                                 "(define/procedure c () (+ 1 \"a\")) (graph g (:r c))")
    , ("a constant naming itself is recursion",   failsWith "might never finish"
                                                            "(define/procedure a () a) (graph g (:r a))")
    , ("as are two naming each other",            failsWith "might never finish"
                                                            "(define/procedure a () b) (define/procedure b () a) (graph g (:r a))")

      -- What it costs: a function of no parameters cannot be passed as a function
    , ("arguments to a constant are an error",    failsWith "It is called (c)."
                                                            "(define/procedure c () 1) (graph g (:r (c 1)))")
    , ("-> calls a constant with the value",      failsWith "It is called (c)."
                                                            "(define/procedure c () 1) (graph g (:r (-> 5 c)))")
    , ("map receives its value",                  failsWith "a function to map with"
                                                            "(define/procedure c () 1) (graph g (:r (map (1 2) c)))")
    , ("a constant is not #function",             compilesToJSON "(define/procedure c () 1) (graph g (:a (satisfies c #function) :b (satisfies c #number)))"
                                                                 "{\"a\": false, \"b\": true}")
    , ("passed on, it is its value",              failsWith "is a Record, not a function"
                                                            "(define/procedure mk () (:v 1)) (define/procedure call (make) (make)) (graph g (:r (call mk)))")
    ]


-- A function is documented with one (doc "...") among the direct elements of its body,
-- before or after its result. It is kept with the function and never evaluated, and a
-- doc anywhere else is reported when the file is parsed.
documentation :: Group
documentation = Group "Functions: a body can hold one (doc ...)"
    [ ("a doc before the result",                 compilesLike "(define/procedure one! () (doc \"One.\") 1) (graph g (:r (one!)))"
                                                               "(graph g (:r 1))")
    , ("a doc after the result",                  compilesLike "(define/procedure one! () 1 (doc \"One.\")) (graph g (:r (one!)))"
                                                               "(graph g (:r 1))")
    , ("a doc with a signature",                  compilesLike "(define/procedure inc! #number (#number n) (doc \"Adds one.\") (+ n 1)) (graph g (:r (inc! 1)))"
                                                               "(graph g (:r 2))")
    , ("the doc is kept with the function",       documents "(define/procedure one! () (doc \"One.\") 1) (graph g (:r (one!)))" "one!" (Just "One."))
    , ("so is a doc after the result",            documents "(define/procedure one! () 1 (doc \"One.\")) (graph g (:r (one!)))" "one!" (Just "One."))
    , ("a function without one has none",         documents "(define/procedure one! () 1) (graph g (:r (one!)))" "one!" Nothing)

    -- One doc, a single string, directly in a definition's body
    , ("two docs are an error",                   failsWith "is documented more than once"
                                                            "(define/procedure one! () (doc \"One.\") (doc \"Uno.\") 1) (graph g (:r (one!)))")
    , ("a doc that is not a string is an error",  failsWith "is not a single string"
                                                            "(define/procedure one! () (doc 1) 1) (graph g (:r (one!)))")
    , ("a doc of two strings is an error",        failsWith "is not a single string"
                                                            "(define/procedure one! () (doc \"One\" \"Uno\") 1) (graph g (:r (one!)))")
    , ("a doc inside the result is an error",     failsWith "'doc' belongs directly in the body of a definition"
                                                            "(define/procedure one! () (:r 1 :d (doc \"One.\"))) (graph g (:r (one!)))")
    , ("even in a function never called",         failsWith "'doc' belongs directly in the body of a definition"
                                                            "(define/procedure one! () (:r 1 :d (doc \"One.\"))) (graph g (:r 1))")
    , ("a doc in a fn is an error",               failsWith "'doc' belongs directly in the body of a definition"
                                                            "(define/procedure one! () (let ($f (fn () (doc \"F.\") 1)) (f))) (graph g (:r (one!)))")
    , ("a doc in a graph is an error",            failsWith "'doc' belongs directly in the body of a definition"
                                                            "(graph g (:r 1 :d (doc \"A graph.\")))")
    , ("doc cannot be a function name",           failsWith "'doc' is read by its syntax before any name is bound"
                                                            "(define/procedure doc (x) x) (graph g (:r 1))")
    ]
    where
    documents src name expected = withTests 1 . property $
        either (const Nothing) docOf (tokenize "t.ouro" src >>= parseModule) === Just expected
        where
        docOf m = case Map.lookup name (M._functionRegistry m) of
                      Just (M.SomeFunction (M.Function _ _ _ doc _ _ _)) -> Just doc
                      Nothing                                            -> Nothing


-- (define/refinement name (#contract x) body) is a property: a function of the property kind,
-- about one value given with a contract, never a function, whose result is #boolean
properties :: Group
properties = Group "Properties: define/refinement"
    [ ("a property is defined and called",        compilesToJSON "(define/refinement titled (#record r) (has r _label)) (graph g (:a (titled (:_label \"A\")) :b (titled (:x 1))))"
                                                                 "{\"a\": true, \"b\": false}")
    , ("it is a function of the property kind",   withTests 1 . property $
                                                      fmap kindOf (tokenize "t.ouro" "(define/refinement titled (#record r) (has r _label)) (define/procedure f (x) x)" >>= parseModule)
                                                      === Right (Just "property", Just "procedure"))

      -- Its result is #boolean, never written
    , ("its body must be Boolean",                failsWith "expectedRefinement = \"#boolean\", actualRefinement = \"#number\""
                                                            "(define/refinement p (#number n) n) (graph g (:r (p 1)))")
    , ("it is reported at the body",              failsWith "line 1, col 34"
                                                            "(define/refinement p (#number n) n) (graph g (:r (p 1)))")
    , ("it has no return type",                   failsWith "A refinement always answers true or false, so it has no return type."
                                                            "(define/refinement p #boolean (#number n) true) (graph g (:r 1))")
    , ("which is reported at the type",           failsWith "line 1, col 22"
                                                            "(define/refinement p #boolean (#number n) true) (graph g (:r 1))")

      -- One parameter, with a contract that is not #function
    , ("a parameter has a contract",              failsWith "The parameter 'n' of the refinement 'p' has no contract."
                                                            "(define/refinement p (n) true) (graph g (:r 1))")
    , ("a pattern parameter too",                 failsWith "The pattern parameter of the refinement 'p' has no contract."
                                                            "(define/refinement p ((?a ?b)) true) (graph g (:r 1))")
    , ("which can be written before it",          compilesToJSON "(define/refinement same (#array (?a ?b)) (eq a b)) (graph g (:a (same (1 1)) :b (same (1 2))))"
                                                                 "{\"a\": true, \"b\": false}")
    , ("the contract is never #function",         failsWith "A refinement is about data, so its parameter cannot be #function."
                                                            "(define/refinement p (#function f) true) (graph g (:r 1))")
    , ("which is reported at the contract",       failsWith "line 1, col 23"
                                                            "(define/refinement p (#function f) true) (graph g (:r 1))")
    , ("a refinement takes one parameter",        failsWith "The refinement 'p' takes one parameter, the value it is about."
                                                            "(define/refinement p (#number a #number b) true) (graph g (:r 1))")
    , ("not none",                                failsWith "The refinement 'p' takes one parameter, the value it is about."
                                                            "(define/refinement p () true) (graph g (:r 1))")

      -- Otherwise it is a function as any other
    , ("it can call itself, going down",          compilesToJSON "(define/refinement labelled (#record r) (and (has r _label) (forall (get-or r () part) labelled))) (graph g (:a (labelled (:_label \"A\" :part ((:_label \"B\")))) :b (labelled (:_label \"A\" :part ((:x 1))))))"
                                                                 "{\"a\": true, \"b\": false}")
    , ("it shares the defn's namespace",          failsWith "defined more than once"
                                                            "(define/procedure titled (x) 1) (define/refinement titled (#record r) true) (graph g (:r 1))")
    , ("it keeps a doc",                          documented "(define/refinement titled (#record r) (doc \"Has a label.\") (has r _label))" "titled" (Just "Has a label."))
    , ("a warning names it a refinement",         warnsWith "The refinement 'Titled' is not named in kebab-case"
                                                            "(define/refinement Titled (#record r) true) (graph g (:r (Titled (:a 1))))")
    , ("as when it replaces a built-in",          warnsWith "The refinement 'len' replaces the built-in 'len'"
                                                            "(define/refinement len (#array xs) true) (graph g (:r 1))")
    , ("one without a doc is a warning",          warnsWith "The refinement 'titled' has no doc"
                                                            "(define/refinement titled (#record r) (has r _label)) (graph g (:r 1))")
    , ("one with a doc is not",                   compilesWithoutWarnings "(define/refinement titled (#record r) (doc \"Has a label.\") (has r _label)) (graph g (:r 1))")

      -- Written with a binder, a refinement keeps every rule
    , ("a binder gives it its contract",          compilesToJSON "(define/refinement titled (<r ⊢ record>) (has r _label)) (graph g (:a (titled (:_label \"A\")) :b (titled (:x 1))))"
                                                                 "{\"a\": true, \"b\": false}")
    , ("a pattern binder too",                    compilesToJSON "(define/refinement same (<(?a ?b) ⊢ array>) (eq a b)) (graph g (:a (same (1 1)) :b (same (1 2))))"
                                                                 "{\"a\": true, \"b\": false}")
    , ("⊢ gives it no return type",               failsWith "A refinement always answers true or false, so it has no return type."
                                                            "(define/refinement p ⊢ boolean (<n ⊢ number>) true) (graph g (:r 1))")
    , ("and a binder of function is refused",     failsWith "A refinement is about data, so its parameter cannot be #function."
                                                            "(define/refinement p (<f ⊢ function>) true) (graph g (:r 1))")
    ]
    where
    kindOf m = ( kind <$> Map.lookup "titled" (M._functionRegistry m)
               , kind <$> Map.lookup "f" (M._functionRegistry m) )
    kind :: M.SomeFunction -> T.Text
    kind = \case
        M.SomeFunction (M.Function M.RefinementKind _ _ _ _ _ _)  -> "property"
        M.SomeFunction (M.Function M.ProcedureKind _ _ _ _ _ _) -> "procedure"
    documented src name expected = withTests 1 . property $
        either (const Nothing) docOf (tokenize "t.ouro" src >>= parseModule) === Just expected
        where
        docOf m = case Map.lookup name (M._functionRegistry m) of
                      Just (M.SomeFunction (M.Function _ _ _ doc _ _ _)) -> Just doc
                      Nothing                                            -> Nothing


-- #titled x asserts a property: the property is called on the value, which passes on when it
-- answers true. The static analysis resolves every tag before anything is evaluated.
assertions :: Group
assertions = Group "Properties: assertions, #titled x"
    [ ("a value with it passes on",               compilesToJSON (titled <> "(graph g (:r #titled (:_label \"A\")))")
                                                                 "{\"r\": {\"_label\": \"A\"}}")
    , ("a value without it fails",                failsWith "expectedRefinement = \"#titled\", actualRefinement = \"a #record that is not #titled\""
                                                            (titled <> "(graph g (:r #titled (:x 1)))"))
    , ("at the value",                            failsWith "line 1, col 76"
                                                            (titled <> "(graph g (:r #titled (:x 1)))"))
    , ("with its doc as the rule it broke",       failsWith "#titled: Has a label."
                                                            "(define/refinement titled (#record r) (doc \"Has a label.\") (has r _label)) (graph g (:r #titled (:x 1)))")
    , ("or saying it has none",                   failsWith "#titled has no doc, so the rule it asserts cannot be shown."
                                                            (titled <> "(graph g (:r #titled (:x 1)))"))
    , ("an error inside it stays that error",     failsWith "expectedRefinement = \"#record\", actualRefinement = \"#string\""
                                                            (titled <> "(graph g (:r #titled \"a\"))"))
    , ("at the tag, where it is called",          failsWith "line 1, col 68"
                                                            (titled <> "(graph g (:r #titled \"a\"))"))

      -- Wherever a built-in property is written
    , ("satisfies asks it",                       compilesToJSON (titled <> "(graph g (:a (satisfies (:_label \"A\") #titled) :b (satisfies (:x 1) #titled)))")
                                                                 "{\"a\": true, \"b\": false}")
    , ("as a case branch's head",                 compilesToJSON (titled <> "(graph g (:r (case (:x 1) ((satisfies #titled) \"titled\") (otherwise \"untitled\"))))")
                                                                 "{\"r\": \"untitled\"}")
    , ("satisfies keeps its parameter's contract an error", failsWith "expectedRefinement = \"#record\", actualRefinement = \"#string\""
                                                            (titled <> "(graph g (:r (satisfies \"a\" #titled)))"))
    , ("a parameter's contract",                 compilesToJSON (titled <> "(define/procedure name-of (#titled r) (get r _label)) (graph g (:r (name-of (:_label \"A\"))))")
                                                                 "{\"r\": \"A\"}")
    , ("which an argument without it fails",      failsWith "line 1, col 131"
                                                            (titled <> "(define/procedure name-of (#titled r) (get r _label)) (graph g (:r (name-of (:x 1))))"))
    , ("a return type",                           failsWith "line 1, col 89"
                                                            (titled <> "(define/procedure make #titled () (:x 1)) (graph g (:r make))"))
    , ("a binding is never a property",           compilesToJSON (titled <> "(graph g (:r (let ($titled 1) #titled (:_label \"A\"))))")
                                                                 "{\"r\": {\"_label\": \"A\"}}")

      -- Every tag is resolved as written, before anything is evaluated
    , ("an unknown one is reported at the tag",   failsWith "line 1, col 68"
                                                            (titled <> "(graph g (:r #titeld (:_label \"A\")))"))
    , ("with the nearest property",               failsWith "Perhaps you meant: '#titled'?"
                                                            (titled <> "(graph g (:r #titeld (:_label \"A\")))"))
    , ("or built-in name",                        failsWith "Perhaps you meant: '#string'?"
                                                            "(graph g (:r #str \"a\"))")
    , ("in a function no graph calls",            failsWith "Unknown refinement: #titeld"
                                                            (titled <> "(define/procedure unused (r) #titeld r) (graph g (:r 1))"))
    , ("and in a parameter's contract",           failsWith "Unknown refinement: #titeld"
                                                            "(define/procedure f (#titeld r) r) (graph g (:r 1))")
    , ("a function is not a refinement",          failsWith "'f' is a function, not a refinement, so #f cannot assert it."
                                                            "(define/procedure f (x) x) (graph g (:r #f 1))")

      -- A tag is a call of its property, so recursion through it must go down
    , ("asserting itself on a part goes down",    compilesToJSON "(define/refinement labelled (#record r) (and (has r _label) (forall (get-or r () part) (fn (#record p) (satisfies p #labelled))))) (graph g (:a (labelled (:_label \"A\" :part ((:_label \"B\")))) :b (labelled (:_label \"A\" :part ((:x 1))))))"
                                                                 "{\"a\": true, \"b\": false}")
    , ("on the whole value it is recursion",      failsWith "'p' calls itself"
                                                            "(define/refinement p (#record r) (satisfies r #p)) (graph g (:r (p (:a 1))))")
    , ("as is being its own contract",            failsWith "'p' calls itself"
                                                            "(define/refinement p (#p x) true) (graph g (:r (p 1)))")
    ]
    where
    titled = "(define/refinement titled (#record r) (has r _label)) "
