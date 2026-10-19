{-# LANGUAGE DataKinds #-}

module Ouro.LanguageTest
( serialisation
, formsAndBuiltins
, returnedFunctions
, recordKeys
, types
, operators
, dates
, classification
, encoding
, contexts
, contextAlgebra
, knownGaps
) where

import qualified Data.ByteString           as BS
import           Data.Maybe                (fromJust)
import           Data.Ouro                 (decodeSource, defaultOptions,
                                            readSource, validate)
import qualified Data.Ouro.Internal.Expr   as I
import qualified Data.Ouro.Internal.Kinds  as JLD
import           Data.Ouro.Internal.Schema (ContextEntry (..), ContextValue (..),
                                            Directives, Schema (..), directive)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           Hedgehog
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range
import qualified Text.URI                  as URI
import           Utils                     (compilesToJSON, emitsExactly,
                                            failsWith, failsWithout, fixture,
                                            shouldCompileTo)


-- Only attribute pairs and elements reach the output. Functions, built-ins and other
-- compile-time values must be used up before then, or the graph does not compile.
serialisation :: Group
serialisation = Group "Serialisation: only values reach the output"
    [ ("a let-bound fn is not output",              compilesToJSON "(graph g (let ($f (fn (x) x)) (:r (f 1))))"
                                                                   "{\"r\": 1}")
    , ("a fn as an attribute value is an error",    failsWith "an unexecuted function"
                                                              "(graph g (:f (fn (x) x)))")
    , ("a fn bound by an attribute pair is still output", failsWith "an unexecuted function"
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
    [ ("insert an attribute pair into a record",    compilesToJSON "(graph g (:r (insert 0 :k 1 (:a 2))))"
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
    , ("a local vocab directive",                   compilesToJSON "(graph g (:context (:@vocab \"https://example.org/\") :a 1))"
                                                                   "{\"@context\": {\"@vocab\": \"https://example.org/\"}, \"a\": 1}")
    , ("thru extends days to their last second",    compilesToJSON "(graph g (let ($start #date \"2020-01-01T00:00:00Z\") (:end (+ start (thru (days 1))))))"
                                                                   "{\"end\": \"2020-01-02T23:59:59Z\"}")
    , ("thru (years 0) ends the current year",      compilesToJSON "(graph g (let ($start #date \"2020-03-01T00:00:00Z\") (:end (+ start (thru (years 0))))))"
                                                                   "{\"end\": \"2020-12-31T23:59:59Z\"}")
    , ("len counts an array's elements",            compilesToJSON "(graph g (:a (len (1 (2 3) (:k 4))) :b (len ()) :c (len #arr-empty)))"
                                                                   "{\"a\": 3, \"b\": 0, \"c\": 0}")
    , ("and a string's characters",                 compilesToJSON "(graph g (:a (len \"Amsterdam\") :b (len \"Málaga\") :c (len \"\")))"
                                                                   "{\"a\": 9, \"b\": 6, \"c\": 0}")
    , ("len of a record is a type error",           failsWith "An Array or a String"
                                                              "(graph g (:r (len (:a 1))))")
    , ("so is len of a URI",                        failsWith "An Array or a String"
                                                              "(graph g (:r (len #uri \"https://a.org/x\")))")
    , ("len takes one argument",                    failsWith "IncorrectArity"
                                                              "(graph g (:r (len (1) (2))))")
    , ("len is an ordinary function value",         compilesToJSON "(graph g (:r (map len (\"ab\" (1 2 3)))))"
                                                                   "{\"r\": [2, 3]}")
    , ("comparisons chain, and null is a value",    compilesToJSON "(graph g (:a (eq 1 1 1) :b (neq 1 2) :c null))"
                                                                   "{\"a\": true, \"b\": true, \"c\": null}")
    , ("#str asserts a string",                     failsWith "expectedProperty = \"#str\", actualProperty = \"#num\""
                                                              "(graph g (:a #str 1))")
    , ("recursion is an error",                     failsWith "a recursive function call"
                                                              "(defn f (n) (f n)) (graph g (:r (f 1)))")
    , ("brackets are reserved",                     failsWith "an unrecognized compiler macro token shape"
                                                              "(graph g (:a [1 2]))")

    -- (attr key value) was removed: an attribute pair is written ':key value', and a key is
    -- never computed. This also removed gap 5, a computed key dropped inside a record.
    , ("attr is removed",                           failsWith "The (attr key value) form has been removed"
                                                              "(graph g (:r (attr \"k\" 1)))")
    , ("so is attr inside a record",                failsWith "The (attr key value) form has been removed"
                                                              "(graph g (let ($k \"key\") (:a 1 (attr k 2))))")
    , ("and inside an array",                       failsWith "The (attr key value) form has been removed"
                                                              "(graph g (:r ((attr \"k\" 1))))")
    , ("insert takes a written attribute pair",     compilesToJSON "(graph g (:r (insert 2 :_label \"Proust\" (:id \"1\" :type \"Person\"))))"
                                                                   "{\"r\": {\"id\": \"1\", \"type\": \"Person\", \"_label\": \"Proust\"}}")
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
-- one, and an attribute pair written in the record overrides both, whatever the order. insert and
-- replace-at cannot duplicate a key.
recordKeys :: Group
recordKeys = Group "Records: each key once"
    [ ("a key written twice is an error",           failsWith "is written twice"
                                                              "(graph g (:a 1 :a 2))")
    , ("also in a nested record",                   failsWith "is written twice"
                                                              "(graph g (:x (:a 1 :a 2)))")
    , ("two contexts are an error",                 failsWith "The key 'context' is written twice"
                                                              "(graph g (:context \"https://x.org/c.json\" :context \"https://y.org/c.json\" :a 1))")
    , ("a written attribute pair wins over an inlaid one", compilesToJSON (base <> "(graph g (:type \"Mine\" (inlay (base))))")
                                                                   "{\"type\": \"Mine\", \"id\": \"1\"}")
    , ("whatever the order",                        compilesToJSON (base <> "(graph g ((inlay (base)) :type \"Mine\"))")
                                                                   "{\"id\": \"1\", \"type\": \"Mine\"}")
    , ("a lookup sees the written attribute pair",  compilesToJSON (base <> "(graph g (:type \"Mine\" :r type (inlay (base))))")
                                                                   "{\"type\": \"Mine\", \"r\": \"Mine\", \"id\": \"1\"}")
    , ("a later inlay overrides an earlier one",    compilesToJSON "(defn a () (:k 1 :j 1)) (defn b () (:k 2)) (graph g (:x 0 (inlay (a)) (inlay (b))))"
                                                                   "{\"x\": 0, \"j\": 1, \"k\": 2}")
    , ("a written attribute pair settles two inlays", compilesToJSON "(defn a () (:k 1)) (defn b () (:k 2)) (graph g (:k 3 (inlay (a)) (inlay (b))))"
                                                                   "{\"k\": 3}")
    , ("insert cannot add an existing key",         failsWith "already has an attribute"
                                                              "(graph g (:r (insert 0 :a 9 (:a 1))))")
    , ("replace-at can keep the attribute pair's own key", compilesToJSON "(graph g (:r (replace-at 0 :a 9 (:a 1 :b 2))))"
                                                                   "{\"r\": {\"a\": 9, \"b\": 2}}")
    , ("replace-at cannot duplicate another key",   failsWith "already has an attribute"
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
    , ("an attribute pair cannot be inserted into an array", failsWith "a datatype for insertion matching"
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
    , ("records are equal whatever the order of their attribute pairs", compilesToJSON "(graph g (:r (eq (:a 1 :b 2) (:b 2 :a 1))))"
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
    , ("#num does not convert a string",            failsWith "expectedProperty = \"#num\", actualProperty = \"#str\""
                                                              "(graph g (:r #num \"5\"))")
    , ("#uri does not accept a date",               failsWith "expectedProperty = \"#uri\", actualProperty = \"#date\""
                                                              ("(graph g (:r #uri " <> date <> "))"))

      -- A failed property names both properties, and says why when the value's own is not enough
    , ("a URI needs a scheme",                      failsWith "actualProperty = \"a #str without a URI scheme\""
                                                              "(graph g (:r #uri \"nope\"))")
    , ("a date needs ISO-8601 form",                failsWith "actualProperty = \"a #str that is not a date\""
                                                              "(graph g (:r #date \"1642\"))")
    , ("null has no built-in property",             failsWith "expectedProperty = \"#str\", actualProperty = \"null\""
                                                              "(graph g (:r #str null))")
    ]

    where
    date = "#date \"2020-01-01T00:00:00Z\""


-- Operators take two operands. Variadic arithmetic and comparisons are unrolled into pairs
-- wherever they are written, including under a built-in property; a case guard compares
-- the case's target with its operands, chaining like a comparison. A count that is wrong
-- as written is an arity error.
operators :: Group
operators = Group "Operators: operands and desugaring"
    [ ("variadic + under a built-in property",      compilesToJSON "(graph g (:u #uri (+ \"https://a.org/\" \"b/\" \"c\")))"
                                                                   "{\"u\": \"https://a.org/b/c\"}")
    , ("thru under a built-in property",            compilesToJSON "(graph g (:d #date (+ #date \"2020-01-01T00:00:00Z\" (thru (days 1)))))"
                                                                   "{\"d\": \"2020-01-02T23:59:59Z\"}")
    , ("a chained comparison under a property",     compilesToJSON "(graph g (:r #bool (< 1 2 3) :s #num (* 2 3 4)))"
                                                                   "{\"r\": true, \"s\": 24}")
    , ("a typed function body",                     compilesToJSON "(defn f () #uri (+ \"https://a.org/\" \"b/\" \"c\")) (graph g (:r (f)))"
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


-- How a form is read, from its direct elements only, never their types: a call if it
-- starts with a symbol, a record if any element is a ':key value' attribute pair (or a context),
-- and an array otherwise. A record inside an array has its own parentheses.
classification :: Group
classification = Group "Classification: records, arrays and calls"
    [ ("an array holds values of any type",         compilesToJSON "(graph g (:r (1 \"a\" true (:k 1) (2 3) null)))"
                                                                   "{\"r\": [1, \"a\", true, {\"k\": 1}, [2, 3], null]}")
    , ("a record in an array has its own parentheses", compilesToJSON "(graph g (:r ((:k 1) \"x\" 3)))"
                                                                   "{\"r\": [{\"k\": 1}, \"x\", 3]}")
    , ("an array of one record",                    compilesToJSON "(graph g (:r ((:a 1))))"
                                                                   "{\"r\": [{\"a\": 1}]}")
    , ("an attribute pair makes a record",          compilesToJSON "(graph g (:r (:a 1)))"
                                                                   "{\"r\": {\"a\": 1}}")
    , ("a loose value in a record is an error",     failsWith "an unbound expression"
                                                              "(graph g (:r (:a 1 2)))")
    , ("one attribute pair among values makes a record", failsWith "an unbound expression"
                                                              "(graph g (:r (1 :a 2)))")
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


-- Source files are UTF-8 on every platform: a byte order mark is dropped, CRLF line endings
-- become LF, and bytes that are not UTF-8 are an error rather than garbled text.
encoding :: Group
encoding = Group "Encoding: source files are UTF-8"
    [ ("a UTF-8 file compiles",              withTests 1 . property $
                                                  shouldCompileTo (fixture "ouro/encoding.ouro") "encoding" (fixture "json/encoding.json"))
    , ("whatever the code page",             withTests 1 . property $ do
                                                  content <- evalIO (readSource (fixture "ouro/encoding.ouro"))
                                                  fmap (T.isInfixOf "\201douard Manet") content === Right True)
    , ("names and keys can be any letters",  compilesToJSON "(defn caf\233 () \"x\") (graph g (:r (caf\233) :na\239ve 1))"
                                                                 "{\"r\": \"x\", \"na\239ve\": 1}")
    , ("\955 is fn",                         compilesToJSON "(graph g (let ($inc (\955 (x) (+ x 1))) (:r (inc 1))))"
                                                                 "{\"r\": 2}")
    , ("with a signature and a pattern",     compilesToJSON "(graph g (:r (map (\955 #num ((?a ?b)) (+ a b)) ((1 2) (3 4)))))"
                                                                 "{\"r\": [3, 7]}")
    , ("\955 cannot name a function",        failsWith "'fn' is a special form"
                                                            "(defn \955 (x) x) (graph g (:r 1))")
    , ("\955x is an ordinary name",          compilesToJSON "(graph g (let ($\955x 1) (:r \955x)))"
                                                                 "{\"r\": 1}")
    , ("UTF-8 is decoded",                   decodesTo [0xC3, 0x89, 0x64] "\201d")
    , ("a byte order mark is dropped",       decodesTo [0xEF, 0xBB, 0xBF, 0x61] "a")
    , ("CRLF becomes LF",                    decodesTo [0x61, 0x0D, 0x0A, 0x62] "a\nb")
    , ("a lone CR is kept",                  decodesTo [0x61, 0x0D, 0x62] "a\rb")
    , ("other bytes are an error",           withTests 1 . property $
                                                  assert (either (T.isInfixOf "is not valid UTF-8") (const False)
                                                                 (decodeSource "t.ouro" (BS.pack [0x61, 0xE9]))))
    ]
    where
    decodesTo bytes expected = withTests 1 . property $
        decodeSource "t.ouro" (BS.pack bytes) === Right expected


-- A record's context is an ordinary value, written as JSON-LD writes it: a remote context's
-- URL, a local context record, null, or an array of them, applied in order. It is checked
-- where it is written.
contexts :: Group
contexts = Group "Contexts: a context is a value"
    [ ("a remote context with local terms is an array", compilesToJSON "(graph g (:context (\"https://linked.art/ns/v1/linked-art.json\" (:ex \"https://example.org/ns/\")) :a 1))"
                                                                    "{\"@context\": [\"https://linked.art/ns/v1/linked-art.json\", {\"ex\": \"https://example.org/ns/\"}], \"a\": 1}")
    , ("local keywords",                            compilesToJSON "(graph g (:context (:@vocab \"https://example.org/\" :@language \"en\") :a 1))"
                                                                   "{\"@context\": {\"@vocab\": \"https://example.org/\", \"@language\": \"en\"}, \"a\": 1}")
    , ("an expanded term definition",               compilesToJSON "(graph g (:context (:condition (:@id \"ex:condition\" :@type \"@id\" :@container (\"@set\"))) :a 1))"
                                                                   "{\"@context\": {\"condition\": {\"@id\": \"ex:condition\", \"@type\": \"@id\", \"@container\": [\"@set\"]}}, \"a\": 1}")
    , ("null clears the context",                   compilesToJSON "(graph g (:context null :a 1))"
                                                                   "{\"@context\": null, \"a\": 1}")
    , ("a record can be only a context",            compilesToJSON "(graph g (:r (:context \"https://a.org/c.json\")))"
                                                                   "{\"r\": {\"@context\": \"https://a.org/c.json\"}}")
    , ("a context can be computed",                 compilesToJSON "(defn linked-art () \"https://linked.art/ns/v1/linked-art.json\") (graph g (:context ((linked-art) (:ex \"https://e.org/\")) :a 1))"
                                                                   "{\"@context\": [\"https://linked.art/ns/v1/linked-art.json\", {\"ex\": \"https://e.org/\"}], \"a\": 1}")
    , ("a #uri is a remote context",                compilesToJSON "(graph g (:context #uri \"https://a.org/c.json\" :a 1))"
                                                                   "{\"@context\": \"https://a.org/c.json\", \"a\": 1}")
    , (":@context is :context",                     compilesToJSON "(graph g (:@context \"https://a.org/c.json\" :a 1))"
                                                                   "{\"@context\": \"https://a.org/c.json\", \"a\": 1}")
    , ("any key can be a keyword",                  compilesToJSON "(graph g (:@id \"https://a.org/1\" :a 1))"
                                                                   "{\"@id\": \"https://a.org/1\", \"a\": 1}")

    -- A context is the record's metadata, which is serialised first wherever it is written
    , ("a context written last is serialised first", compilesToJSON "(graph g (:a 1 :b 2 :context \"https://a.org/c.json\"))"
                                                                   "{\"@context\": \"https://a.org/c.json\", \"a\": 1, \"b\": 2}")
    , ("so is one written in the middle",           compilesToJSON "(graph g (:a 1 :context (\"https://a.org/c.json\" (:ex \"https://e.org/\")) :b 2))"
                                                                   "{\"@context\": [\"https://a.org/c.json\", {\"ex\": \"https://e.org/\"}], \"a\": 1, \"b\": 2}")
    , ("and one written after an inlay",            compilesToJSON "(defn base () (:id \"1\" :type \"T\")) (graph g ((inlay (base)) :a 1 :context \"https://a.org/c.json\"))"
                                                                   "{\"@context\": \"https://a.org/c.json\", \"id\": \"1\", \"type\": \"T\", \"a\": 1}")

    -- An inlay layers its record's context under the record's own, as it layers attribute pairs:
    -- the inlays' contexts in order, then the written one, applied last
    , ("an inlay keeps its record's context",       compilesToJSON (ctx <> "(graph g (:r ((inlay (c)) :b 2)))")
                                                                   "{\"r\": {\"@context\": \"https://a.org/c.json\", \"a\": 1, \"b\": 2}}")
    , ("a written context is applied after it",     compilesToJSON (ctx <> "(graph g (:r ((inlay (c)) :context \"https://w.org/c.json\" :b 2)))")
                                                                   "{\"r\": {\"@context\": [\"https://a.org/c.json\", \"https://w.org/c.json\"], \"a\": 1, \"b\": 2}}")
    , ("wherever it is written",                    compilesToJSON (ctx <> "(graph g (:r (:context \"https://w.org/c.json\" (inlay (c)) :b 2)))")
                                                                   "{\"r\": {\"@context\": [\"https://a.org/c.json\", \"https://w.org/c.json\"], \"a\": 1, \"b\": 2}}")
    , ("inlays' contexts keep their order",         compilesToJSON (ctx <> "(graph g (:r ((inlay (c)) (inlay (d)) :b 2)))")
                                                                   "{\"r\": {\"@context\": [\"https://a.org/c.json\", \"https://d.org/c.json\"], \"a\": 1, \"d\": 4, \"b\": 2}}")
    , ("a pipeline keeps its context",              compilesToJSON (ctx <> "(graph g (:r (-> (c) (with :b 2) (with :x 3))))")
                                                                   "{\"r\": {\"@context\": \"https://a.org/c.json\", \"a\": 1, \"b\": 2, \"x\": 3}}")
    , ("the same context twice is written once",   compilesToJSON (ctx <> "(graph g (:r ((inlay (c)) :context \"https://a.org/c.json\" :b 2)))")
                                                                   "{\"r\": {\"@context\": \"https://a.org/c.json\", \"a\": 1, \"b\": 2}}")
    , ("so is a repeat written in the array",       compilesToJSON "(graph g (:context (\"https://a.org/c.json\" \"https://a.org/c.json\") :a 1))"
                                                                   "{\"@context\": \"https://a.org/c.json\", \"a\": 1}")
    , ("but only next to itself",                   compilesToJSON "(graph g (:context (\"https://a.org/c.json\" (:ex \"https://e.org/\") \"https://a.org/c.json\") :a 1))"
                                                                   "{\"@context\": [\"https://a.org/c.json\", {\"ex\": \"https://e.org/\"}, \"https://a.org/c.json\"], \"a\": 1}")
    , ("validate collapses a repeat",               withTests 1 . property $
                                                       fmap (T.filter (/= ' ') . T.filter (/= '\n') . TL.toStrict)
                                                            (validate "t.json" "{\"@context\": [\"https://a.org/c.json\", \"https://a.org/c.json\"], \"a\": 1}" defaultOptions)
                                                       === Right "{\"@context\":\"https://a.org/c.json\",\"a\":1}")

    , ("validate keeps a context array",            withTests 1 . property $
                                                       fmap (T.filter (/= ' ') . T.filter (/= '\n') . TL.toStrict)
                                                            (validate "t.json" "{\"@context\": [\"https://a.org/c.json\", {\"ex\": \"https://e.org/\"}], \"a\": 1}" defaultOptions)
                                                       === Right "{\"@context\":[\"https://a.org/c.json\",{\"ex\":\"https://e.org/\"}],\"a\":1}")

    -- A malformed context is reported where it is written
    , ("an unknown keyword",                        failsWith "'@vocabb' is not a JSON-LD context keyword"
                                                              "(graph g (:context (:@vocabb \"https://e.org/\") :a 1))")
    , ("a term maps to an IRI",                     failsWith "The term 'ex' cannot be a Number"
                                                              "(graph g (:context (:ex 5) :a 1))")
    , ("a remote context is an absolute URL",       failsWith "is not an absolute URL"
                                                              "(graph g (:context \"not a url\" :a 1))")
    , ("a context array holds no arrays",           failsWith "cannot hold another array"
                                                              "(graph g (:context ((\"https://a.org/c.json\")) :a 1))")
    , ("an expanded definition's keys are keywords", failsWith "cannot be part of the definition of the term 't'"
                                                              "(graph g (:context (:t (:@id \"x\" :foo 1)) :a 1))")
    , ("a date has no place in a context",          failsWith "cannot hold a Date"
                                                              "(graph g (:context (:@vocab #date \"2020-01-01T00:00:00Z\") :a 1))")
    , ("it is reported at the context",             failsWith "line 1, col 20"
                                                              "(graph g (:context (:ex 5) :a 1))")
    , ("the (context (...)) form is replaced",      failsWith "has been replaced by ':context value'"
                                                              "(graph g ((context (vocab \"https://e.org/\")) :a 1))")
    ]
    where
    ctx = "(defn c () (:context \"https://a.org/c.json\" :a 1)) (defn d () (:context \"https://d.org/c.json\" :d 4)) "


-- Contexts compose: a context is a Monoid under concatenation, its entries applied in order,
-- and a local context is a Monoid under a right-biased union, where a key's later value wins
-- and its first position is kept.
contextAlgebra :: Group
contextAlgebra = Group "Contexts: combining them is a monoid"
    [ ("contexts combine associatively",        property $ do
                                                    (a, b, c) <- forAll ((,,) <$> genSchema <*> genSchema <*> genSchema)
                                                    (a <> b) <> c === a <> (b <> c))
    , ("the empty context changes nothing",     property $ do
                                                    a <- forAll genSchema
                                                    (mempty <> a, a <> mempty) === (a, a))
    , ("local contexts combine associatively",  property $ do
                                                    (a, b, c) <- forAll ((,,) <$> genDirectives <*> genDirectives <*> genDirectives)
                                                    (a <> b) <> c === a <> (b <> c))
    , ("the empty local context changes nothing", property $ do
                                                    a <- forAll genDirectives
                                                    (mempty <> a, a <> mempty) === (a, a))
    , ("a later definition wins",               withTests 1 . property $
                                                    (directive "ex" (CString "https://a.org/") <> directive "ex" (CString "https://b.org/"))
                                                    === directive "ex" (CString "https://b.org/"))
    , ("and keeps its first position",          withTests 1 . property $
                                                    (directive "a" CNull <> directive "b" CNull <> directive "a" (CBool True))
                                                    === (directive "a" (CBool True) <> directive "b" CNull))
    , ("adjacent local contexts are not merged", withTests 1 . property $
                                                    (Schema [LocalContext (directive "a" CNull)] <> Schema [LocalContext (directive "b" CNull)])
                                                    === Schema [LocalContext (directive "a" CNull), LocalContext (directive "b" CNull)])
    , ("an entry next to itself is applied once", withTests 1 . property $
                                                    (remote "https://a.org/c.json" <> remote "https://a.org/c.json")
                                                    === remote "https://a.org/c.json")
    , ("but an entry apart from itself is kept",  withTests 1 . property $
                                                    (remote "https://a.org/c.json" <> remote "https://b.org/c.json" <> remote "https://a.org/c.json")
                                                    === Schema (map remoteEntry ["https://a.org/c.json", "https://b.org/c.json", "https://a.org/c.json"]))

    -- A record's metadata is a Monoid over its context, with no context as the identity
    , ("metadata combines associatively",       property $ do
                                                    (a, b, c) <- forAll ((,,) <$> genMeta <*> genMeta <*> genMeta)
                                                    (a <> b) <> c === a <> (b <> c))
    , ("no context changes nothing",            property $ do
                                                    a <- forAll genMeta
                                                    (mempty <> a, a <> mempty) === (a, a))
    ]
    where
    -- A few keys, so that unions overlap
    genDirectives :: Gen Directives
    genDirectives = mconcat <$> Gen.list (Range.linear 0 5)
                        (directive <$> Gen.element ["@vocab", "@base", "ex", "label", "id"] <*> genValue)

    genValue :: Gen ContextValue
    genValue = Gen.choice [ CString <$> Gen.element ["https://a.org/", "ex:x", "@id"]
                          , CBool <$> Gen.bool
                          , pure CNull ]

    genMeta :: Gen (I.Expr 'JLD.Meta)
    genMeta = Gen.choice [ pure I.EmptyMeta, I.Context <$> genSchema ]

    -- Built with '<>', as contexts are, so a context never holds two equal neighbours
    genSchema :: Gen Schema
    genSchema = mconcat <$> Gen.list (Range.linear 0 4)
                    (Schema . pure <$> Gen.choice [ pure ClearContext
                                                   , LocalContext <$> genDirectives
                                                   , remoteEntry <$> Gen.element ["https://a.org/c.json", "https://b.org/c.json"] ])

    remoteEntry :: T.Text -> ContextEntry
    remoteEntry = RemoteContext . fromJust . URI.mkURI

    remote :: T.Text -> Schema
    remote = Schema . pure . remoteEntry


-- Known gaps, numbered as in the specification's Appendix B. Each test pins the current,
-- incorrect behaviour; the change that fixes a gap flips its test to the correct one.
knownGaps :: Group
knownGaps = Group "Known gaps: current, incorrect behaviour"
    [ ("gap 9: no suggestion for a local name",     failsWithout "Perhaps you meant"
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
    , ("gap 17: a URI's empty root path is dropped", compilesToJSON "(graph g (:u #uri \"https://a.org/\"))"
                                                                   "{\"u\": \"https://a.org\"}")
    , ("gap 18: an attribute pair among values has no hint", failsWithout "(1 (:a 2))"
                                                                 "(graph g (:r (1 :a 2)))")
    , ("gap 19: a quote in a string is not escaped", emitsExactly "(graph g (:quote \"say \\\"hi\\\"\"))"
                                                                 "\"quote\": \"say \"hi\"\"")
    , ("gap 19: nor is a backslash",                emitsExactly "(graph g (:slash \"a\\\\b\"))"
                                                                 "\"slash\": \"a\\b\"")
    , ("gap 19: nor is a newline",                  emitsExactly "(graph g (:newline \"a\\nb\"))"
                                                                 "\"newline\": \"a\nb\"")
    , ("gap 19: nor is a tab",                      emitsExactly "(graph g (:tab \"a\\tb\"))"
                                                                 "\"tab\": \"a\tb\"")
    , ("gap 19: validate unescapes a valid document", withTests 1 . property $
                                                       fmap (T.isInfixOf "\"k\": \"a\"b\"" . TL.toStrict)
                                                            (validate "t.json" "{\"k\": \"a\\\"b\"}" defaultOptions)
                                                       === Right True)
    ]
