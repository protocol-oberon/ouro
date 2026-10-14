module Ouro.LanguageTest
( serialisation
, formsAndBuiltins
, returnedFunctions
, recordKeys
, types
, operators
, dates
, knownGaps
) where

import           Hedgehog
import           Utils    (compilesToJSON, failsWith, failsWithout)


-- Only fields and elements reach the output. Functions, built-ins and other
-- compile-time values must be used up before then, or the graph does not compile.
serialisation :: Group
serialisation = Group "Serialisation: only values reach the output"
    [ ("a let-bound fn is not output",              compilesToJSON "(graph g (let ($f (fn (x) x)) (:r (f 1))))"
                                                                   "{\"r\": 1}")
    , ("a fn as a field is an error",               failsWith "an unexecuted function"
                                                              "(graph g (:f (fn (x) x)))")
    , ("a fn bound as a field is still output",     failsWith "an unexecuted function"
                                                              "(graph g (:f (fn (x) x) :r (f 1)))")
    , ("a returned fn is an error",                 failsWith "an unexecuted function"
                                                              "(defn mk () (fn (x) x)) (graph g (:r (mk)))")
    , ("a named function as a value is an error",   failsWith "an unexecuted function"
                                                              "(defn inc (x) (+ x 1)) (graph g (:r inc))")
    , ("a fn inside an array is an error",          failsWith "a JSON value"
                                                              "(graph g (:r (list (fn (x) x))))")
    , ("a fn as the graph is an error",             failsWith "a JSON value"
                                                              "(graph g (fn (x) x))")
    , ("a built-in as a value is an error",         failsWith "a Built-in function"
                                                              "(graph g (:r +))")
    , ("a fn spliced by inlay is an error",         failsWith "an unexecuted function"
                                                              "(defn mk () (:f (fn (x) x))) (graph g (:a 1 (inlay (mk))))")
    , ("let bindings are not output",               compilesToJSON "(graph g (let ($hidden 1) (:shown hidden)))"
                                                                   "{\"shown\": 1}")
    ]


-- The behaviour of the special forms and built-ins, as documented in the specification
formsAndBuiltins :: Group
formsAndBuiltins = Group "Special forms and built-ins"
    [ ("insert a field into a record",              compilesToJSON "(graph g (:r (insert 0 :k 1 (:a 2))))"
                                                                   "{\"r\": {\"k\": 1, \"a\": 2}}")
    , ("insert counts negative indexes from the end", compilesToJSON "(graph g (:r (insert -1 4 (1 2 3))))"
                                                                   "{\"r\": [1, 2, 4, 3]}")
    , ("nth indexes arrays and records",            compilesToJSON "(graph g (:r (nth -1 (1 2 3)) :s (nth 0 (:a 9 :b 8))))"
                                                                   "{\"r\": 3, \"s\": 9}")
    , ("get walks a path of keys",                  compilesToJSON "(graph g (:p (:q (:r 5)) :v (get p q r)))"
                                                                   "{\"p\": {\"q\": {\"r\": 5}}, \"v\": 5}")
    , ("a record pattern binds the rest",           compilesToJSON "(graph g (:r (case (:a 1 :b 2 :c 3) ((:a ?first ?..others) (:x first :y others)) (otherwise 0))))"
                                                                   "{\"r\": {\"x\": 1, \"y\": {\"b\": 2, \"c\": 3}}}")
    , ("empty records and arrays",                  compilesToJSON "(graph g (:r #rec-empty :s #arr-empty :t ()))"
                                                                   "{\"r\": {}, \"s\": [], \"t\": []}")
    , ("list makes an array headed by a variable",  compilesToJSON "(graph g (let ($x 1) (:r (list x x))))"
                                                                   "{\"r\": [1, 1]}")
    , ("a form headed by a variable is a call",     failsWith "not a function"
                                                              "(graph g (let ($x 1) (:r (x x))))")
    , ("a remote context",                          compilesToJSON "(graph g (:context \"https://linked.art/ns/v1/linked-art.json\" :a 1))"
                                                                   "{\"@context\": \"https://linked.art/ns/v1/linked-art.json\", \"a\": 1}")
    , ("a local vocab directive",                   compilesToJSON "(graph g ((context (vocab \"https://example.org/\")) :a 1))"
                                                                   "{\"@context\": {\"@vocab\": \"https://example.org/\"}, \"a\": 1}")
    , ("thru extends days to their last second",    compilesToJSON "(graph g (let ($start #date \"2020-01-01T00:00:00Z\") (:end (+ start (thru (days 1))))))"
                                                                   "{\"end\": \"2020-01-02T23:59:59Z\"}")
    , ("thru (years 0) ends the current year",      compilesToJSON "(graph g (let ($start #date \"2020-03-01T00:00:00Z\") (:end (+ start (thru (years 0))))))"
                                                                   "{\"end\": \"2020-12-31T23:59:59Z\"}")
    , ("comparisons chain, and null is a value",    compilesToJSON "(graph g (:a (eq 1 1 1) :b (neq 1 2) :c null))"
                                                                   "{\"a\": true, \"b\": true, \"c\": null}")
    , ("#str asserts a String",                     failsWith "a String"
                                                              "(graph g (:a #str 1))")
    , ("recursion is an error",                     failsWith "a recursive function call"
                                                              "(defn f (n) (f n)) (graph g (:r (f 1)))")
    , ("brackets are reserved",                     failsWith "an unrecognized compiler macro token shape"
                                                              "(graph g (:a [1 2]))")
    ]


-- Functions are not curried: a call supplies exactly the declared arguments. A function
-- can return a fn, which is called by binding it with let; ((f ...) ...) is an array.
returnedFunctions :: Group
returnedFunctions = Group "Functions: arity and returned functions"
    [ ("functions are not curried",                 failsWith "IncorrectArity"
                                                              "(defn add (a b) (+ a b)) (graph g (:r (add 1)))")
    , ("a returned fn is called through let",       compilesToJSON "(defn greet () (fn () \"hello\")) (graph g (let ($f (greet)) (:r (f))))"
                                                                   "{\"r\": \"hello\"}")
    , ("currying by hand, through let",             compilesToJSON "(defn adder (n) (fn (x) (+ n x))) (graph g (let ($inc (adder 1)) (:r (inc 2))))"
                                                                   "{\"r\": 3}")
    , ("a form headed by a call is an array",       compilesToJSON "(defn one () 1) (graph g (:r ((one))))"
                                                                   "{\"r\": [1]}")
    , ("so ((f x) y) does not call the result",     failsWith "an unexecuted function"
                                                              "(defn adder (n) (fn (x) (+ n x))) (graph g (:r ((adder 1) 2)))")
    ]


-- A key appears once in a record. Inlays are layers: a later inlay overrides an earlier
-- one, and a field written in the record overrides both, whatever the order. insert and
-- replace-at cannot duplicate a key.
recordKeys :: Group
recordKeys = Group "Records: each key once"
    [ ("a key written twice is an error",           failsWith "is written twice"
                                                              "(graph g (:a 1 :a 2))")
    , ("also in a nested record",                   failsWith "is written twice"
                                                              "(graph g (:x (:a 1 :a 2)))")
    , ("two contexts are an error",                 failsWith "The key 'context' is written twice"
                                                              "(graph g (:context \"https://x.org/c.json\" :context \"https://y.org/c.json\" :a 1))")
    , ("a written field wins over an inlaid one",   compilesToJSON (base <> "(graph g (:type \"Mine\" (inlay (base))))")
                                                                   "{\"type\": \"Mine\", \"id\": \"1\"}")
    , ("whatever the order",                        compilesToJSON (base <> "(graph g ((inlay (base)) :type \"Mine\"))")
                                                                   "{\"id\": \"1\", \"type\": \"Mine\"}")
    , ("a lookup sees the written field",           compilesToJSON (base <> "(graph g (:type \"Mine\" :r type (inlay (base))))")
                                                                   "{\"type\": \"Mine\", \"r\": \"Mine\", \"id\": \"1\"}")
    , ("a later inlay overrides an earlier one",    compilesToJSON "(defn a () (:k 1 :j 1)) (defn b () (:k 2)) (graph g (:x 0 (inlay (a)) (inlay (b))))"
                                                                   "{\"x\": 0, \"j\": 1, \"k\": 2}")
    , ("a written field settles two inlays",        compilesToJSON "(defn a () (:k 1)) (defn b () (:k 2)) (graph g (:k 3 (inlay (a)) (inlay (b))))"
                                                                   "{\"k\": 3}")
    , ("insert cannot add an existing key",         failsWith "already has a field"
                                                              "(graph g (:r (insert 0 :a 9 (:a 1))))")
    , ("replace-at can keep the field's own key",   compilesToJSON "(graph g (:r (replace-at 0 :a 9 (:a 1 :b 2))))"
                                                                   "{\"r\": {\"a\": 9, \"b\": 2}}")
    , ("replace-at cannot duplicate another key",   failsWith "already has a field"
                                                              "(graph g (:r (replace-at 0 :b 9 (:a 1 :b 2))))")
    ]

    where
    base = "(defn base () (:type \"Base\" :id \"1\")) "


-- The type rules as they are: values carry their types, built-ins check them, and nothing
-- is converted implicitly. Arrays are like JSON arrays: their elements can be of any type.
-- Several rules here are under review (equality across types, ordering, extending URIs);
-- a change to one flips its test.
types :: Group
types = Group "Types: the rules as they are"
    [ -- Arrays
      ("an array may mix types",                    compilesToJSON "(graph g (:r (1 \"a\" true)))"
                                                                   "{\"r\": [1, \"a\", true]}")
    , ("+ joins any two arrays",                    compilesToJSON "(graph g (:r (+ (1 \"a\") (2 3)) :s (+ (1 2) (\"a\"))))"
                                                                   "{\"r\": [1, \"a\", 2, 3], \"s\": [1, 2, \"a\"]}")
    , ("+ joins empty arrays",                      compilesToJSON "(graph g (:r (+ () (1)) :s (+ () ()) :t (+ #arr-empty (1))))"
                                                                   "{\"r\": [1], \"s\": [], \"t\": [1]}")
    , ("insert takes any value into an array",      compilesToJSON "(graph g (:r (insert 0 \"x\" (1 2)) :s (insert 0 1 #arr-empty)))"
                                                                   "{\"r\": [\"x\", 1, 2], \"s\": [1]}")
    , ("a field cannot be inserted into an array",  failsWith "a datatype for insertion matching"
                                                              "(graph g (:r (insert 0 :k 1 (1 2))))")
    , ("a mixed array is described as mixed",       failsWith "an Array of mixed values"
                                                              "(graph g (:r (+ 1 (1 \"a\"))))")

      -- Equality
    , ("eq across types is false",                  compilesToJSON "(graph g (:r (eq 1 \"1\") :s (eq true 1)))"
                                                                   "{\"r\": false, \"s\": false}")
    , ("a URI is not equal to its String",          compilesToJSON "(graph g (:r (eq #uri \"https://a.org\" \"https://a.org\")))"
                                                                   "{\"r\": false}")
    , ("a case literal of another type never matches", compilesToJSON "(graph g (:r (case 5 (\"5\" \"string\") (otherwise \"number\"))))"
                                                                   "{\"r\": \"number\"}")
    , ("records are equal whatever their field order", compilesToJSON "(graph g (:r (eq (:a 1 :b 2) (:b 2 :a 1))))"
                                                                   "{\"r\": true}")

      -- Ordering and arithmetic
    , ("dates cannot be ordered",                   failsWith "a Date > a Date"
                                                              ("(graph g (:r (> " <> date <> " " <> date <> ")))"))
    , ("dates cannot be subtracted",                failsWith "a Date - a Date"
                                                              ("(graph g (:r (- " <> date <> " " <> date <> ")))"))
    , ("strings cannot be ordered",                 failsWith "a String < a String"
                                                              "(graph g (:r (< \"a\" \"b\")))")
    , ("a URI cannot be extended",                  failsWith "a URI + a String"
                                                              "(graph g (:r (+ #uri \"https://a.org/\" \"x\")))")
    , ("durations do not add",                      failsWith "a Duration time period + a Duration time period"
                                                              "(graph g (:r (+ (days 1) (days 2))))")
    , ("null has no arithmetic",                    failsWith "a Null value + a Number"
                                                              "(graph g (:r (+ null 1)))")

      -- Assertions check, and only #uri and #date convert a String
    , ("#num does not convert a String",            failsWith "a Number"
                                                              "(graph g (:r #num \"5\"))")
    , ("#uri does not accept a Date",               failsWith "a URI layout"
                                                              ("(graph g (:r #uri " <> date <> "))"))
    ]

    where
    date = "#date \"2020-01-01T00:00:00Z\""


-- Operators take two operands. Variadic arithmetic and comparisons are unrolled into pairs
-- wherever they are written, including under a type tag; a case guard compares the case's
-- target with its operands, chaining like a comparison. A count that is wrong as written
-- is an arity error.
operators :: Group
operators = Group "Operators: operands and desugaring"
    [ ("variadic + under a type tag",               compilesToJSON "(graph g (:u #uri (+ \"https://a.org/\" \"b/\" \"c\")))"
                                                                   "{\"u\": \"https://a.org/b/c\"}")
    , ("thru under a type tag",                     compilesToJSON "(graph g (:d #date (+ #date \"2020-01-01T00:00:00Z\" (thru (days 1)))))"
                                                                   "{\"d\": \"2020-01-02T23:59:59Z\"}")
    , ("a chained comparison under a type tag",     compilesToJSON "(graph g (:r #bool (< 1 2 3) :s #num (* 2 3 4)))"
                                                                   "{\"r\": true, \"s\": 24}")
    , ("a tagged function body",                    compilesToJSON "(defn f () #uri (+ \"https://a.org/\" \"b/\" \"c\")) (graph g (:r (f)))"
                                                                   "{\"r\": \"https://a.org/b/c\"}")
    , ("a guard compares the target",               compilesToJSON "(defn size (n) (case n ((> 100) \"large\") ((< 10) \"small\") (otherwise \"medium\"))) (graph g (:a (size 150) :b (size 5) :c (size 50)))"
                                                                   "{\"a\": \"large\", \"b\": \"small\", \"c\": \"medium\"}")
    , ("a guard with two operands chains",          compilesToJSON "(graph g (:r (case 5 ((< 10 20) \"a\") (otherwise \"b\")) :s (case 15 ((< 10 20) \"a\") (otherwise \"b\"))))"
                                                                   "{\"r\": \"a\", \"s\": \"b\"}")
    , ("a guard with three operands chains",        compilesToJSON "(graph g (:r (case 5 ((< 10 20 30) \"a\") (otherwise \"b\"))))"
                                                                   "{\"r\": \"a\"}")
    , ("a guard's operands are desugared",          compilesToJSON "(graph g (:r (case 5 ((< (+ 1 2 3) 10) \"a\") (otherwise \"b\"))))"
                                                                   "{\"r\": \"a\"}")
    , ("an operator needs two operands",            failsWith "IncorrectArity"
                                                              "(graph g (:r (+ 1)))")
    , ("a comparison needs two operands",           failsWith "IncorrectArity"
                                                              "(graph g (:r (< 1)))")
    , ("neq compares exactly two values",           failsWith "IncorrectArity"
                                                              "(graph g (:r (neq 1 2 3)))")
    , ("so a neq guard takes one operand",          failsWith "IncorrectArity"
                                                              "(graph g (:r (case 5 ((neq 1 2) \"a\") (otherwise \"b\"))))")
    , ("a bare thru is not a value",                failsWith "thru"
                                                              "(graph g (:r #date (thru (days 1))))")
    ]


-- Calendar arithmetic. Months and years clip to the end of a shorter month instead of
-- rolling over into the next one, so a shift never skips past the month it lands in.
dates :: Group
dates = Group "Dates: calendar arithmetic"
    [ ("a month after 31 January is 29 February",   compilesToJSON ("(graph g (:r (+ " <> date "2020-01-31" <> " (months 1))))")
                                                                   "{\"r\": \"2020-02-29T00:00:00Z\"}")
    , ("or 28 February outside a leap year",        compilesToJSON ("(graph g (:r (+ " <> date "2021-01-31" <> " (months 1))))")
                                                                   "{\"r\": \"2021-02-28T00:00:00Z\"}")
    , ("a month after 31 May is 30 June",           compilesToJSON ("(graph g (:r (+ " <> date "2020-05-31" <> " (months 1))))")
                                                                   "{\"r\": \"2020-06-30T00:00:00Z\"}")
    , ("a month before 31 March is 29 February",    compilesToJSON ("(graph g (:r (- " <> date "2020-03-31" <> " (months 1))))")
                                                                   "{\"r\": \"2020-02-29T00:00:00Z\"}")
    , ("a year after 29 February is 28 February",   compilesToJSON ("(graph g (:r (+ " <> date "2020-02-29" <> " (years 1)) :s (+ " <> date "2020-02-29" <> " (years 4))))")
                                                                   "{\"r\": \"2021-02-28T00:00:00Z\", \"s\": \"2024-02-29T00:00:00Z\"}")
    , ("thru a month ends that month",              compilesToJSON ("(graph g (:r (+ " <> date "2020-01-31" <> " (thru (months 1))) :s (+ " <> date "2020-01-31" <> " (thru (months 0)))))")
                                                                   "{\"r\": \"2020-02-29T23:59:59Z\", \"s\": \"2020-01-31T23:59:59Z\"}")
    , ("thru a year ends that year",                compilesToJSON ("(graph g (:r (+ " <> date "2020-02-29" <> " (thru (years 1)))))")
                                                                   "{\"r\": \"2021-12-31T23:59:59Z\"}")
    , ("days do not clip",                          compilesToJSON ("(graph g (:r (+ " <> date "2020-01-31" <> " (days 30))))")
                                                                   "{\"r\": \"2020-03-01T00:00:00Z\"}")
    ]

    where
    date day = "#date \"" <> day <> "T00:00:00Z\""


-- Known gaps, numbered as in the specification's Appendix B. Each test pins the current,
-- incorrect behaviour; the change that fixes a gap flips its test to the correct one.
knownGaps :: Group
knownGaps = Group "Known gaps: current, incorrect behaviour"
    [ ("gap 1: a remote context with local terms nests", compilesToJSON "(graph g ((context (remote-context \"https://x.org/c.json\" vocab \"https://e.org/\")) :a 1))"
                                                                   "{\"@context\": {\"@context\": \"https://x.org/c.json\", \"@vocab\": \"https://e.org/\"}, \"a\": 1}")
    , ("gap 5: computed keys are dropped",          compilesToJSON "(graph g (let ($k \"key\") (:a 1 (attr k 2))))"
                                                                   "{\"a\": 1}")
    , ("gap 7: a field cannot see its outer name",  failsWith "references itself"
                                                              "(graph g (let ($name \"outer\") (:name name)))")
    , ("gap 9: no suggestion for a local name",     failsWithout "Perhaps you meant"
                                                                 "(graph g (let ($artist 1) (:r artst)))")
    , ("gap 10: the function hint uses old syntax", failsWith "(name! args)"
                                                              "(defn inc (x) (+ x 1)) (graph g (:r (list inc)))")
    , ("gap 10: get's missing-key hint mentions define", failsWith "'define'"
                                                              "(graph g (:p (:a 1) :r (get p b)))")
    , ("gap 10: subtraction's hint names L.Duration", failsWith "L.Duration"
                                                              "(graph g (:r (- \"a\" 1)))")
    , ("gap 10: a reserved $ name is called an attribute key", failsWith "for your attribute"
                                                              "(graph g (let ($get 1) (:r get)))")
    , ("gap 11: a parameter named fn",              failsWith "A fn is written"
                                                              "(defn call (fn) (fn)) (graph g (:r (call 1)))")
    , ("gap 14: a record of only inlays is an array", failsWith "a valid Array to inlay"
                                                              "(defn a () (:k 1)) (defn b () (:j 2)) (graph g ((inlay (a)) (inlay (b))))")
    ]
