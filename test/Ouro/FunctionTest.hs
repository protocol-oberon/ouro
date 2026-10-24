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
                                              warnsWith)


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
    , ("a name can contain /, as std/... does",    compilesLike "(defn std/inc (n) (+ n 1)) (graph g (:r (std/inc 1)))"
                                                                "(graph g (:r 2))")

    -- Checked by message: today every program here already fails, but for another reason
    , ("a let value shadows a top-level function", compilesLike "(defn label () \"function\") (graph g (let ($label \"value\") (:r label)))"
                                                                "(graph g (:r \"value\"))")
    , ("a function defined twice",                 failsWith "defined more than once"
                                                             "(defn f () 1) (defn f () 2) (graph g (:r (f)))")
    , ("a function named after a special form",    failsWith "special form"
                                                             "(defn get (x) x) (graph g (:r 1))")
    , ("calling a value that shadows a function",  failsWith "not a function"
                                                             "(defn label () \"function\") (graph g (:label \"value\" :r (label)))")

      -- A defn may take a built-in's name, and replaces it everywhere, with a warning
    , ("a defn named after a built-in warns",      warnsWith "The function 'eq' replaces the built-in 'eq'"
                                                             "(defn eq (a b) true) (graph g (:r (eq 1 2)))")
    , ("so does one named with its symbol",        warnsWith "replaces the built-in 'eq'"
                                                             "(defn = (a b) true) (graph g (:r (= 1 2)))")
    , ("it replaces calls and guards alike",       compilesToJSON "(defn eq (a b) true) (graph g (:r (eq 1 2) :s (case 1 ((eq 2) \"guard\") (otherwise \"no\"))))"
                                                                  "{\"r\": true, \"s\": \"guard\"}")
    , ("other names do not warn",                  compilesWithoutWarnings "(defn mine (a b) true) (graph g (:r (mine 1 2)))")

      -- Functions and graphs are named in kebab-case, with a warning otherwise
    , ("a camelCase defn warns",                   warnsWith "Perhaps rename it 'make-place'"
                                                             "(defn makePlace () 1) (graph g (:r (makePlace)))")
    , ("so does a snake_case one",                 warnsWith "Perhaps rename it 'make-place'"
                                                             "(defn make_place () 1) (graph g (:r (make_place)))")
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
    , ("#rec and #arr are properties anywhere",   compilesToJSON "(graph g (:r #rec (:a 1) :s #arr (1 2)))"
                                                                 "{\"r\": {\"a\": 1}, \"s\": [1, 2]}")

    -- Mismatches, checked by message and by where they are reported
    , ("an argument of the wrong type",           failsWith "expectedProperty = \"#num\", actualProperty = \"#str\""
                                                            "(defn double (#num n) (* n 2)) (graph g (:r (double \"x\")))")
    , ("it is reported at the argument",          failsWith "line 1, col 53"
                                                            "(defn double (#num n) (* n 2)) (graph g (:r (double \"x\")))")
    , ("an unused argument is still checked",     failsWith "expectedProperty = \"#num\""
                                                            "(defn second (#num a b) b) (graph g (:r (second \"x\" 2)))")
    , ("a result of the wrong type",              failsWith "expectedProperty = \"#num\", actualProperty = \"#str\""
                                                            "(defn label #num (who) (+ who \"!\")) (graph g (:r (label \"a\")))")
    , ("it is reported at the body",              failsWith "line 1, col 24"
                                                            "(defn label #num (who) (+ who \"!\")) (graph g (:r (label \"a\")))")
    , ("a fn result of the wrong type",           failsWith "expectedProperty = \"#str\""
                                                            "(graph g (let ($f (fn #str (n) n)) (:r (f 1))))")
    , ("a value that is not a function",          failsWith "expectedProperty = \"#fn\""
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
    , ("special forms take holes",                compilesToJSON "(graph g (let ($second (get ? 1) $a (get ? a)) (:r (second (10 20)) :s (a (:a 5)))))"
                                                                 "{\"r\": 20, \"s\": 5}")
    , ("insert, replace-at and list take holes",  compilesToJSON "(graph g (let ($ins (insert 0 ? (1 2)) $rep (replace-at 0 ? (1 2)) $pair (list ? ?)) (:r (ins 0) :s (rep 9) :t (pair 1 2))))"
                                                                 "{\"r\": [0, 1, 2], \"s\": [9, 2], \"t\": [1, 2]}")
    , ("a placeholder is a value",                compilesToJSON "(defn apply (f x) (f x)) (graph g (:r (apply (+ ? 1) 1)))"
                                                                 "{\"r\": 2}")
    , ("a placeholder closes over its scope",     compilesToJSON "(defn adder (n) (+ ? n)) (graph g (let ($add10 (adder 10)) (:r (add10 5))))"
                                                                 "{\"r\": 15}")
    , ("a case body can be a placeholder",        compilesToJSON "(graph g (:r (case (1 2) ((?a ?b) (let ($add (+ ? b)) (add a))) (otherwise 0))))"
                                                                 "{\"r\": 3}")

    -- A typed hole is a typed parameter: it converts, and a mismatch is reported at the call
    , ("a typed hole converts",                   compilesToJSON "(graph g (let ($y (+ #date ? (years 1))) (:r (y \"2020-01-01T00:00:00Z\"))))"
                                                                 "{\"r\": \"2021-01-01T00:00:00Z\"}")
    , ("a named hole is typed where it is written", compilesToJSON "(graph g (let ($f (+ #num ?x ?x)) (:r (f 2))))"
                                                                 "{\"r\": 4}")
    , ("a typed hole is checked at the call",     failsWith "line 1, col 87"
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
    , ("a rest hole belongs in a pattern",        failsWith "can only be written in a pattern"
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
    , ("a typed parameter checks each element",   failsWith "expectedProperty = \"#num\", actualProperty = \"#str\""
                                                            "(defn double (#num n) (* n 2)) (graph g (:r (map double (1 \"x\" 3))))")
    , ("it is reported at the array",             failsWith "line 1, col 57"
                                                            "(defn double (#num n) (* n 2)) (graph g (:r (map double (1 \"x\" 3))))")
    , ("a result is checked by the return type",  failsWith "expectedProperty = \"#num\""
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


-- A parameter can be a pattern, written as a case pattern is. It takes one argument, which
-- is matched when the function is called, and its holes are bound around the body.
destructuring :: Group
destructuring = Group "Functions: a parameter can be a pattern"
    [ ("an array pattern parameter",              compilesToJSON "(graph g (let ($swap (fn ((?a ?b)) (list b a))) (:r (swap (1 2)))))"
                                                                 "{\"r\": [2, 1]}")
    , ("a record pattern parameter",              compilesToJSON "(defn label-of ((:_label ?l)) l) (graph g (:r (label-of (:type \"Type\" :_label \"Beauty\"))))"
                                                                 "{\"r\": \"Beauty\"}")
    , ("a rest hole in a parameter",              compilesToJSON "(graph g (let ($tail (fn ((?h ?..t)) t)) (:r (tail (1 2 3)))))"
                                                                 "{\"r\": [2, 3]}")
    , ("a literal in a pattern is compared",      compilesToJSON "(graph g (let ($f (fn ((\"Person\" ?l)) l)) (:r (f (\"Person\" \"Manet\")))))"
                                                                 "{\"r\": \"Manet\"}")
    , ("a pattern beside a named parameter",      compilesToJSON "(defn sum (base (?a ?b)) (+ base a b)) (graph g (:r (sum 1 (2 3))))"
                                                                 "{\"r\": 6}")
    , ("a property before a pattern",             compilesToJSON "(graph g (let ($f (fn (#arr (?a ?b)) (+ a b))) (:r (f (1 2)))))"
                                                                 "{\"r\": 3}")
    , ("map with a pattern parameter",            compilesToJSON "(graph g (:r (map (fn ((?slug ?label)) (:id slug :_label label)) ((\"allan\" \"Scott Allan\") (\"beeny\" \"Emily Beeny\")))))"
                                                                 "{\"r\": [{\"id\": \"allan\", \"_label\": \"Scott Allan\"}, {\"id\": \"beeny\", \"_label\": \"Emily Beeny\"}]}")
    , ("a defn's pattern is desugared",           compilesToJSON "(defn f (((+ 1 2 3) ?x)) x) (graph g (:r (f (6 1))))"
                                                                 "{\"r\": 1}")
    , ("as a fn's pattern is",                    compilesToJSON "(graph g (let ($f (fn (((+ 1 2 3) ?x)) x)) (:r (f (6 1)))))"
                                                                 "{\"r\": 1}")

    -- A pattern's values are evaluated where the function is defined, not among its parameters
    , ("a defn's pattern sees the module",        compilesToJSON "(defn kind () \"Type\") (defn label-of ((:type (kind) :_label ?l)) l) (graph g (:r (label-of (:type \"Type\" :_label \"Beauty\"))))"
                                                                 "{\"r\": \"Beauty\"}")
    , ("a fn's pattern sees its scope",           compilesToJSON "(graph g (let ($k \"Type\" $f (fn ((:type k :_label ?l)) l)) (:r (f (:type \"Type\" :_label \"Beauty\")))))"
                                                                 "{\"r\": \"Beauty\"}")
    , ("but not the other parameters",            failsWith "variableName = \"kind\""
                                                            "(defn label-if (kind (:type kind :_label ?l)) l) (graph g (:r (label-if \"Type\" (:type \"Type\" :_label \"Beauty\"))))")
    , ("a pattern cannot call its own function",  failsWith "a recursive function call"
                                                            "(defn f (((f 1) ?x)) x) (graph g (:r (f (1 2))))")

    -- A pattern is one parameter, matched at the call, and a mismatch is reported at the argument
    , ("a pattern takes one argument",            failsWith "IncorrectArity"
                                                            "(graph g (let ($f (fn ((?a ?b)) a)) (:r (f 1 2))))")
    , ("an unused pattern is still matched",      failsWith "the shape of the parameter's pattern"
                                                            "(graph g (let ($f (fn ((?a ?b)) 1)) (:r (f 5))))")
    , ("it is reported at the argument",          failsWith "line 1, col 44"
                                                            "(graph g (let ($f (fn ((?a ?b)) 1)) (:r (f 5))))")
    , ("in map it is reported at the array",      failsWith "line 1, col 36"
                                                            "(graph g (:r (map (fn ((?a ?b)) a) ((1 2) (3)))))")
    , ("the property is checked before the pattern", failsWith "expectedProperty = \"#rec\""
                                                            "(graph g (let ($f (fn (#rec (?a ?b)) a)) (:r (f (1 2)))))")

    -- Errors, reported where the function is defined
    , ("a pattern must bind something",           failsWith "binds nothing"
                                                            "(defn f ((1 2)) 1) (graph g (:r 1))")
    , ("a name is bound once",                    failsWith "bound more than once"
                                                            "(defn f (a a) a) (graph g (:r (f 1 2)))")
    , ("a hole cannot reuse a parameter's name",  failsWith "bound more than once"
                                                            "(graph g (let ($f (fn (a (?a)) a)) (:r 1)))")
    , ("a hole in a call is a malformed pattern", failsWith "MalformedPattern"
                                                            "(defn f ((?a (g ?b))) a) (graph g (:r (f (1 2))))")
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
    , ("a bare name is called with the value",    compilesToJSON "(defn wrap (r) (list r)) (graph g (:r (-> 1 wrap)))"
                                                                 "{\"r\": [1]}")
    , ("the value goes first, or at the hole",    compilesToJSON "(graph g (:r (-> 1 (- 10 ?)) :s (-> 1 (- 10))))"
                                                                 "{\"r\": 9, \"s\": -9}")
    , ("a fn step is called with the value",      compilesToJSON "(graph g (:r (-> 1 (fn (x) (+ x 1)))))"
                                                                 "{\"r\": 2}")
    , ("(-> ? ...) is a function",                compilesToJSON "(graph g (let ($f (-> ? (+ 1) (* 2))) (:r (f 3))))"
                                                                 "{\"r\": 8}")
    , ("which can be mapped",                     compilesToJSON "(graph g (:r (map (-> ? (with :seen true)) ((:a 1) (:a 2)))))"
                                                                 "{\"r\": [{\"a\": 1, \"seen\": true}, {\"a\": 2, \"seen\": true}]}")
    , ("pipelines nest",                          compilesToJSON "(graph g (:r (-> (-> 1 (+ 1)) (* 10))))"
                                                                 "{\"r\": 20}")
    , ("special forms are steps too",             compilesToJSON "(graph g (:r (-> (:a 1) (get a)) :s (-> 5 (case ((> 3) \"big\") (otherwise \"small\")))))"
                                                                 "{\"r\": 1, \"s\": \"big\"}")
    , ("a pipeline of no steps is its value",     compilesToJSON "(graph g (:r (-> 1)))"
                                                                 "{\"r\": 1}")
    , ("a pipeline builds a Linked Art record",   compilesToJSON ( "(defn core (id type label) (:id #uri id :type type :_label label)) "
                                                                <> "(defn aat (id label) (core (+ \"http://vocab.getty.edu/aat/\" id) \"Type\" label)) "
                                                                <> "(defn classified (r t) (with r :classified_as (list t))) "
                                                                <> "(defn named (r n) (with r :identified_by (list (:type \"Name\" :content n)))) "
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
    , ("with cannot be a function name",          failsWith "'with' is a special form"
                                                            "(defn with (x) x) (graph g (:r 1))")
    , ("-> cannot be a function name",            failsWith "'->' is a special form"
                                                            "(defn -> (x) x) (graph g (:r 1))")
    ]
    where
    base = "(defn base () (:id \"1\" :type \"T\")) "


-- A function is documented with one (doc "...") among the direct elements of its body,
-- before or after its result. It is kept with the function and never evaluated, and a
-- doc anywhere else is reported when the file is parsed.
documentation :: Group
documentation = Group "Functions: a body can hold one (doc ...)"
    [ ("a doc before the result",                 compilesLike "(defn one! () (doc \"One.\") 1) (graph g (:r (one!)))"
                                                               "(graph g (:r 1))")
    , ("a doc after the result",                  compilesLike "(defn one! () 1 (doc \"One.\")) (graph g (:r (one!)))"
                                                               "(graph g (:r 1))")
    , ("a doc with a signature",                  compilesLike "(defn inc! #num (#num n) (doc \"Adds one.\") (+ n 1)) (graph g (:r (inc! 1)))"
                                                               "(graph g (:r 2))")
    , ("the doc is kept with the function",       documents "(defn one! () (doc \"One.\") 1) (graph g (:r (one!)))" "one!" (Just "One."))
    , ("so is a doc after the result",            documents "(defn one! () 1 (doc \"One.\")) (graph g (:r (one!)))" "one!" (Just "One."))
    , ("a function without one has none",         documents "(defn one! () 1) (graph g (:r (one!)))" "one!" Nothing)

    -- One doc, a single string, directly in a defn's body
    , ("two docs are an error",                   failsWith "is documented more than once"
                                                            "(defn one! () (doc \"One.\") (doc \"Uno.\") 1) (graph g (:r (one!)))")
    , ("a doc that is not a string is an error",  failsWith "is not a single string"
                                                            "(defn one! () (doc 1) 1) (graph g (:r (one!)))")
    , ("a doc of two strings is an error",        failsWith "is not a single string"
                                                            "(defn one! () (doc \"One\" \"Uno\") 1) (graph g (:r (one!)))")
    , ("a doc inside the result is an error",     failsWith "'doc' belongs directly in the body of a defn"
                                                            "(defn one! () (:r 1 :d (doc \"One.\"))) (graph g (:r (one!)))")
    , ("even in a function never called",         failsWith "'doc' belongs directly in the body of a defn"
                                                            "(defn one! () (:r 1 :d (doc \"One.\"))) (graph g (:r 1))")
    , ("a doc in a fn is an error",               failsWith "'doc' belongs directly in the body of a defn"
                                                            "(defn one! () (let ($f (fn () (doc \"F.\") 1)) (f))) (graph g (:r (one!)))")
    , ("a doc in a graph is an error",            failsWith "'doc' belongs directly in the body of a defn"
                                                            "(graph g (:r 1 :d (doc \"A graph.\")))")
    , ("doc cannot be a function name",           failsWith "'doc' is a special form"
                                                            "(defn doc (x) x) (graph g (:r 1))")
    ]
    where
    documents src name expected = withTests 1 . property $
        either (const Nothing) docOf (tokenize "t.ouro" src >>= parseModule) === Just expected
        where
        docOf m = case Map.lookup name (M._functionRegistry m) of
                      Just (M.Function _ _ doc _ _) -> Just doc
                      Nothing                       -> Nothing
