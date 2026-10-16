module Ouro.FunctionTest
( expressionBodies
, mapping
, placeholders
, plainNames
, signatures
) where

import qualified Data.Text as T
import           Hedgehog
import           Utils     (compilesLike, compilesToJSON, compilesWithoutWarnings,
                            failsToCompile, failsWith)


-- A function body is exactly one expression: its result, written exactly as it would be
-- anywhere else. Private values are bound around it with let.
expressionBodies :: Group
expressionBodies = Group "Functions: a body is one result expression"
    [ ("a literal result",                        compilesLike "(defn getty! () \"http://vocab.getty.edu/aat/\") (graph g (:u (getty!)))"
                                                               "(graph g (:u \"http://vocab.getty.edu/aat/\"))")
    , ("a call's result",                         compilesLike "(defn add-one! (n) (+ 1 n)) (graph g (:r (add-one! 1)))"
                                                               "(graph g (:r 2))")
    , ("an array of one",                         compilesLike "(defn one! () (\"lit\")) (graph g (:r (one!)))"
                                                               "(graph g (:r (\"lit\")))")
    , ("an array",                                compilesLike "(defn ids! () (1 2 3)) (graph g (:r (ids!)))"
                                                               "(graph g (:r (1 2 3)))")
    , ("a record",                                compilesLike "(defn person! (who) (:type \"Person\" :_label who)) (graph g (:r (person! \"Manet\")))"
                                                               "(graph g (:r (:type \"Person\" :_label \"Manet\")))")
    , ("a let around a record",                   compilesLike (T.unlines
                                                                 [ "(defn node! (x)"
                                                                 , "  (let ($next (+ x 1))"
                                                                 , "    (:value next)))"
                                                                 , "(graph g (:r (node! 1)))" ])
                                                               "(graph g (:r (:value 2)))")
    , ("a let around a primitive",               compilesLike "(defn twice-next! (x) (let ($next (+ x 1)) (* next 2))) (graph g (:r (twice-next! 1)))"
                                                               "(graph g (:r 4))")
    , ("a let-bound fn in the body",             compilesLike "(defn outer! () (let ($inner (fn () 7)) (inner))) (graph g (:r (outer!)))"
                                                               "(graph g (:r 7))")

    -- The old forms are errors
    , ("a spliced record body is an error",       failsToCompile "(defn person! (who) :type \"Person\" :_label who) (graph g (:r (person! \"Manet\")))")
    , ("return is no longer a special form",      failsToCompile "(defn id! (x) (return x)) (graph g (:r (id! 1)))")
    , ("two result expressions are an error",     failsToCompile "(defn two! () 1 2) (graph g (:r (two!)))")
    , ("a define in a body is an error",          failsToCompile "(defn none! () (define :a 1)) (graph g (:r (none!)))")
    ]


-- Function names are plain symbols, sharing one namespace with values: whichever
-- binding is innermost wins. A trailing '!' is still allowed, but no longer required.
plainNames :: Group
plainNames = Group "Functions: names are plain symbols, in one namespace"
    [ ("a function with a plain name",             compilesLike "(defn twice (x) (* 2 x)) (graph g (:r (twice 2)))"
                                                                "(graph g (:r 4))")
    , ("a constant with a plain name",             compilesLike "(defn getty () \"http://vocab.getty.edu/aat/\") (graph g (:u (getty)))"
                                                                "(graph g (:u \"http://vocab.getty.edu/aat/\"))")
    , ("a local function with a plain name",       compilesLike "(defn outer () (let ($inner (fn () 7)) (inner))) (graph g (:r (outer)))"
                                                                "(graph g (:r 7))")
    , ("functions passed as values",               compilesLike "(defn apply-to (f x) (f x)) (defn inc (n) (+ n 1)) (graph g (:r (apply-to inc 1)))"
                                                                "(graph g (:r 2))")
    , ("an inner value shadows an outer function", compilesLike "(defn label () \"function\") (graph g (:label \"value\" :r label))"
                                                                "(graph g (:label \"value\" :r \"value\"))")

    -- Compiles today, and must keep compiling
    , ("a trailing ! is still allowed",            compilesLike "(defn legacy! () 1) (graph g (:r (legacy!)))"
                                                                "(graph g (:r 1))")

    -- Checked by message: today every program here already fails, but for another reason
    , ("a let value shadows a top-level function", compilesLike "(defn label () \"function\") (graph g (let ($label \"value\") (:r label)))"
                                                                "(graph g (:r \"value\"))")
    , ("a function defined twice",                 failsWith "defined more than once"
                                                             "(defn f () 1) (defn f () 2) (graph g (:r (f)))")
    , ("a function named after a special form",    failsWith "special form"
                                                             "(defn get (x) x) (graph g (:r 1))")
    , ("calling a value that shadows a function",  failsWith "not a function"
                                                             "(defn label () \"function\") (graph g (:label \"value\" :r (label)))")
    ]


-- A signature's types are tags in a function's header: one between the name and the
-- parameters for the result, and one before a parameter for that parameter. A tag checks
-- (and converts to #uri or #date) exactly as it would anywhere else. An argument is
-- checked where the call is written, and the result where the body is.
signatures :: Group
signatures = Group "Functions: signatures are tags in the header"
    [ ("a typed defn",                            compilesToJSON "(defn node #rec (#uri id type) (:id id :type type)) (graph g (:r (node \"https://a.org/x\" \"Type\")))"
                                                                 "{\"r\": {\"id\": \"https://a.org/x\", \"type\": \"Type\"}}")
    , ("a typed fn",                              compilesToJSON "(graph g (let ($double (fn #num (#num n) (* n 2))) (:r (double 21))))"
                                                                 "{\"r\": 42}")
    , ("an untyped parameter beside a typed one", compilesToJSON "(defn pair (#num a b) (list a b)) (graph g (:r (pair 1 \"two\")))"
                                                                 "{\"r\": [1, \"two\"]}")
    , ("a typed parameter converts to a date",    compilesToJSON "(defn year-end #date (#date d) (+ d (thru (years 1)))) (graph g (:r (year-end \"2020-01-01T00:00:00Z\")))"
                                                                 "{\"r\": \"2021-12-31T23:59:59Z\"}")
    , ("#rec and #arr in a signature",            compilesToJSON "(defn wrap #arr (#rec r) (list r)) (graph g (:r (wrap (:a 1))))"
                                                                 "{\"r\": [{\"a\": 1}]}")
    , ("#fn in a signature",                      compilesToJSON "(defn apply #num (#fn f x) (f x)) (graph g (:r (apply (fn (n) (+ n 1)) 1)))"
                                                                 "{\"r\": 2}")
    , ("#rec and #arr are tags anywhere",         compilesToJSON "(graph g (:r #rec (:a 1) :s #arr (1 2)))"
                                                                 "{\"r\": {\"a\": 1}, \"s\": [1, 2]}")

    -- Mismatches, checked by message and by where they are reported
    , ("an argument of the wrong type",           failsWith "expectedType = \"a Number\", actualType = \"a String\""
                                                            "(defn double (#num n) (* n 2)) (graph g (:r (double \"x\")))")
    , ("it is reported at the argument",          failsWith "line 1, col 53"
                                                            "(defn double (#num n) (* n 2)) (graph g (:r (double \"x\")))")
    , ("an unused argument is still checked",     failsWith "expectedType = \"a Number\""
                                                            "(defn second (#num a b) b) (graph g (:r (second \"x\" 2)))")
    , ("a result of the wrong type",              failsWith "expectedType = \"a Number\", actualType = \"a String\""
                                                            "(defn label #num (who) (+ who \"!\")) (graph g (:r (label \"a\")))")
    , ("it is reported at the body",              failsWith "line 1, col 24"
                                                            "(defn label #num (who) (+ who \"!\")) (graph g (:r (label \"a\")))")
    , ("a fn result of the wrong type",           failsWith "expectedType = \"a String\""
                                                            "(graph g (let ($f (fn #str (n) n)) (:r (f 1))))")
    , ("a value that is not a function",          failsWith "expectedType = \"a function\""
                                                            "(defn apply (#fn f x) (f x)) (graph g (:r (apply 1 2)))")
    , ("a type needs a parameter after it",       failsWith "written before its name"
                                                            "(defn f (#num) 1) (graph g (:r (f)))")

    -- A return type says what the result is, so a typed one-value array is not a mistake
    , ("#arr silences the one-value warning",     compilesWithoutWarnings "(defn f #arr () (\"x\")) (graph g (:r (f) :s (let ($h (fn #arr () (\"y\"))) (h))))")
    ]


-- A call with '?' holes among its arguments is a function of its holes. Each '?' is a new
-- parameter, a named hole '?n' is one parameter however often it is written, and a hole
-- belongs to the call it is an argument of.
placeholders :: Group
placeholders = Group "Functions: a call with holes is a function of them"
    [ ("a call with a hole is a function",        compilesToJSON "(graph g (let ($inc (+ ? 1)) (:r (inc 41))))"
                                                                 "{\"r\": 42}")
    , ("a hole can stand for any argument",       compilesToJSON "(defn node (id type) (:id id :type type)) (graph g (let ($typed (node ? \"Type\")) (:r (typed \"x\"))))"
                                                                 "{\"r\": {\"id\": \"x\", \"type\": \"Type\"}}")
    , ("each ? is a parameter, in order",         compilesToJSON "(graph g (let ($sub (- ? ?)) (:r (sub 10 3))))"
                                                                 "{\"r\": 7}")
    , ("a named hole is one parameter",           compilesToJSON "(graph g (let ($sq (* ?n ?n)) (:r (sq 7))))"
                                                                 "{\"r\": 49}")
    , ("parameters follow first appearance",      compilesToJSON "(graph g (let ($f (+ ?a ?b ?a)) (:r (f 1 10))))"
                                                                 "{\"r\": 12}")
    , ("variadic operators take holes",           compilesToJSON "(graph g (let ($f (+ ? 1 2) $between (< ? 10 20)) (:r (f 10) :s (between 15))))"
                                                                 "{\"r\": 13, \"s\": false}")
    , ("special forms take holes",                compilesToJSON "(graph g (let ($second (nth 1 ?) $a (get ? a)) (:r (second (10 20)) :s (a (:a 5)))))"
                                                                 "{\"r\": 20, \"s\": 5}")
    , ("insert, replace-at and list take holes",  compilesToJSON "(graph g (let ($ins (insert 0 ? (1 2)) $rep (replace-at 0 ? (1 2)) $pair (list ? ?)) (:r (ins 0) :s (rep 9) :t (pair 1 2))))"
                                                                 "{\"r\": [0, 1, 2], \"s\": [9, 2], \"t\": [1, 2]}")
    , ("a placeholder is a value",                compilesToJSON "(defn apply (f x) (f x)) (graph g (:r (apply (+ ? 1) 1)))"
                                                                 "{\"r\": 2}")
    , ("a placeholder closes over its scope",     compilesToJSON "(defn adder (n) (+ ? n)) (graph g (let ($add10 (adder 10)) (:r (add10 5))))"
                                                                 "{\"r\": 15}")
    , ("a case body can be a placeholder",        compilesToJSON "(graph g (:r (case (1 2) ((?a ?b) (let ($add (+ ? b)) (add a))) (otherwise 0))))"
                                                                 "{\"r\": 3}")

    -- A tagged hole is a typed parameter: it converts, and a mismatch is reported at the call
    , ("a tagged hole converts",                  compilesToJSON "(graph g (let ($y (+ #date ? (years 1))) (:r (y \"2020-01-01T00:00:00Z\"))))"
                                                                 "{\"r\": \"2021-01-01T00:00:00Z\"}")
    , ("a named hole is typed where it is tagged", compilesToJSON "(graph g (let ($f (+ #num ?x ?x)) (:r (f 2))))"
                                                                 "{\"r\": 4}")
    , ("a tagged hole is checked at the call",    failsWith "line 1, col 87"
                                                            "(defn node (id type) (:id id :type type)) (graph g (let ($n (node #num ? \"T\")) (:r (n \"x\"))))")
    , ("an untyped hole is checked at the hole",  failsWith "line 1, col 58"
                                                            "(defn double (#num n) (* n 2)) (graph g (let ($d (double ?)) (:r (d \"x\"))))")

    -- Errors
    , ("a hole belongs to the innermost call",    failsWith "a Number * an unexecuted function"
                                                            "(defn apply (f x) (f x)) (graph g (:r (apply (* 2 (+ ? 1)) 3)))")
    , ("it takes one argument per parameter",     failsWith "IncorrectArity"
                                                            "(graph g (let ($inc (+ ? 1)) (:r (inc 1 2))))")
    , ("a hole outside a call is an error",       failsWith "can only be written as an argument of a call"
                                                            "(graph g (:r (1 ?)))")
    , ("a hole as a let's value is an error",     failsWith "can only be written as an argument of a call"
                                                            "(graph g (let ($b ?) (:r b)))")
    , ("a hole in a record argument is an error", failsWith "can only be written as an argument of a call"
                                                            "(defn f (x) x) (graph g (:r (f (:a ?))))")
    , ("a rest hole belongs in a pattern",        failsWith "can only be written in a case pattern"
                                                            "(graph g (:r (+ ?..xs 1)))")
    ]


-- (map f array) calls f on each element of an Array. Each call is an ordinary call, so
-- arity, signatures and the recursion check apply as they do anywhere else.
mapping :: Group
mapping = Group "Functions: map calls a function on each element"
    [ ("map with a placeholder",                  compilesToJSON "(graph g (:r (map (+ ? 1) (1 2 3))))"
                                                                 "{\"r\": [2, 3, 4]}")
    , ("map with a defn",                         compilesToJSON "(defn label (who) (:_label who)) (graph g (:r (map label (\"Manet\" \"Proust\"))))"
                                                                 "{\"r\": [{\"_label\": \"Manet\"}, {\"_label\": \"Proust\"}]}")
    , ("map with a fn over records",              compilesToJSON "(graph g (:r (map (fn (r) (get r id)) ((:id 1) (:id 2)))))"
                                                                 "{\"r\": [1, 2]}")
    , ("map with a built-in",                     compilesToJSON "(graph g (:r (map (+ #date \"2020-01-01T00:00:00Z\" ?) (map years (1 2)))))"
                                                                 "{\"r\": [\"2021-01-01T00:00:00Z\", \"2022-01-01T00:00:00Z\"]}")
    , ("map over an empty array",                 compilesToJSON "(graph g (:r (map (+ ? 1) ()) :s (map (+ ? 1) #arr-empty)))"
                                                                 "{\"r\": [], \"s\": []}")
    , ("map with a hole is a function",           compilesToJSON "(graph g (let ($inc-all (map (+ ? 1) ?)) (:r (inc-all (1 2)))))"
                                                                 "{\"r\": [2, 3]}")
    , ("map is only reserved as a function name", compilesToJSON "(graph g (let ($map 5) (:map map :r map)))"
                                                                 "{\"map\": 5, \"r\": 5}")

    -- Each element is checked by the function's signature, and reported at the Array
    , ("a typed parameter checks each element",   failsWith "expectedType = \"a Number\", actualType = \"a String\""
                                                            "(defn double (#num n) (* n 2)) (graph g (:r (map double (1 \"x\" 3))))")
    , ("it is reported at the array",             failsWith "line 1, col 57"
                                                            "(defn double (#num n) (* n 2)) (graph g (:r (map double (1 \"x\" 3))))")
    , ("a result is checked by the return type",  failsWith "expectedType = \"a Number\""
                                                            "(graph g (:r (map (fn #num (n) (+ n \"!\")) (\"a\"))))")

    -- Errors
    , ("the function takes one argument",         failsWith "IncorrectArity"
                                                            "(graph g (:r (map (fn (a b) a) (1 2))))")
    , ("map needs a function",                    failsWith "a function to map with"
                                                            "(graph g (:r (map 1 (1 2))))")
    , ("map needs an array",                      failsWith "an Array to map over"
                                                            "(graph g (:r (map (+ ? 1) 5)))")
    , ("a record is not an array",                failsWith "an Array to map over"
                                                            "(graph g (:r (map (+ ? 1) (:a 1))))")
    , ("map takes a function and an array",       failsWith "map is written (map f array)"
                                                            "(graph g (:r (map (+ ? 1))))")
    , ("map cannot be a function name",           failsWith "'map' is a special form"
                                                            "(defn map (x) x) (graph g (:r 1))")
    , ("a function cannot map itself",            failsWith "a recursive function call"
                                                            "(defn f (x) (map f x)) (graph g (:r (f (1))))")
    ]
