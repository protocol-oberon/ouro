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
, endOfFile
, strings
, logicAndCollections
, conditions
, contexts
, contextAlgebra
, specialFormsAreBindings
, knownGaps
) where

import qualified Data.ByteString           as BS
import           Data.Maybe                (fromJust)
import           Data.Ouro                 (LinterWarning (..),
                                            SemanticWarning (..),
                                            VocabularyWarning (..),
                                            WarningContext (..), decodeSource,
                                            defaultOptions,
                                            smartWarningCode, validate)
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
                                            failsWith, failsWithout, fixedGap,
                                            fixture, importsFailAt,
                                            readSourceFile, shouldCompileTo,
                                            warnsWith)


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

      -- A function where a value belongs says how it is called, with its parameters' names.
      -- This fixed the function hint of gap 10, which suggested (name! args).
    , ("in an array, how to call it",               failsWith "Perhaps call it: (inc x)."
                                                              "(defn inc (x) (+ x 1)) (graph g (:r (list inc)))")
    , ("as an attribute value",                     failsWith "A function is not a value: perhaps call it, (inc x)."
                                                              "(defn inc (x) (+ x 1)) (graph g (:r inc))")
    , ("as an argument with a property",            failsWith "This value is a function: perhaps call it, (mk x)."
                                                              "(defn f #record (#record r) r) (defn mk (x) (:a x)) (graph g (:r (f mk)))")
    , ("a pattern parameter is shown as (...)",     failsWith "Perhaps call it: (f (...) c)."
                                                              "(defn f ((?a ?b) c) a) (graph g (:r (list f)))")
    , ("a built-in has no names to show",           failsWith "Perhaps call it with its arguments."
                                                              "(graph g (:r (list len)))")
    , ("nor has an anonymous fn",                   failsWith "Perhaps call it with its arguments."
                                                              "(graph g (:r (list (fn (x) x))))")
    , ("which has no call to show",                 failsWithout "(fn"
                                                                 "(graph g (:r (fn (x) x)))")
    , ("the call is shown on a wrong arity",        failsWith "It is called (aat id label)."
                                                              "(defn aat (id label) 1) (graph g (:r (aat 1)))")
    , ("and to a function of none",                 failsWith "It is called (c)."
                                                              "(defn c () 1) (graph g (:r (c 1)))")

      -- The rest of gap 10's stale wording, fixed
    , ("get's missing key: the record has no such attribute", failsWith "The Record has no attribute 'b'.\""
                                                              "(graph g (:p (:a 1) :r (get p b)))")
    , ("subtraction's hint names a Duration",       failsWith "or a Date minus a Duration"
                                                              "(graph g (:r (- \"a\" 1)))")
    , ("a $ name may be a special form's",          compilesToJSON "(graph g (let ($get 1) (:r get)))"
                                                                   "{\"r\": 1}")
    , ("and so may a key",                          compilesToJSON "(graph g (:case 1))"
                                                                   "{\"case\": 1}")

      -- A removed form's name is free, as a key or a binding (gap 42, fixed); a call of one is
      -- reported with its replacement, unless a binding of the name shadows it, as in Scheme
    , ("a removed form's name is a key",            compilesToJSON "(graph g (:a (:attr 1 :nth 2 :define 3 :defun 4)))"
                                                                   "{\"a\": {\"attr\": 1, \"nth\": 2, \"define\": 3, \"defun\": 4}}")
    , ("and a binding",                             compilesToJSON "(graph g (let ($attr 1 $defun 2) (:r (+ attr defun))))"
                                                                   "{\"r\": 3}")
    , ("whose call is the binding's",               failsWith "'nth' is also a special form, which a binding of the same name shadows"
                                                              "(graph g (let ($nth 1) (:r (nth 0 (1 2)))))")

      -- Pinned limits: the call is shown by the function's own name, not as it was qualified,
      -- and a function inside an array is reported at the graph, where its position is lost
    , ("the name is unqualified",                   failsWith "Perhaps call it: (aat id label)."
                                                              "(import std/base) (graph g (:r (list base/aat)))")
    , ("an array's function is placed at the graph", importsFailAt "TYP0000" ("inline-test", 1, 19) ["The value at 'r[0]' is a function"] []
                                                                   "(import std/base) (graph g (:r (list base/aat)))")
    ]


-- The behaviour of the special forms and built-ins, as documented in the specification
formsAndBuiltins :: Group
formsAndBuiltins = Group "Special forms and built-ins"
    [ ("insert an attribute pair into a record",    compilesToJSON "(graph g (:r (insert 0 :k 1 (:a 2))))"
                                                                   "{\"r\": {\"k\": 1, \"a\": 2}}")
    , ("insert counts negative indexes from the end", compilesToJSON "(graph g (:r (insert -1 4 (1 2 3))))"
                                                                   "{\"r\": [1, 2, 4, 3]}")
    , ("get indexes an Array",                      compilesToJSON "(graph g (:r (get (1 2 3) -1) :s (get (1 2 3) 0)))"
                                                                   "{\"r\": 3, \"s\": 1}")
    , ("nth is replaced by get",                    failsWith "has been replaced by get"
                                                              "(graph g (:r (nth 0 (1 2))))")
    , ("get walks a path of keys",                  compilesToJSON "(graph g (:p (:q (:r 5)) :v (get p q r)))"
                                                                   "{\"p\": {\"q\": {\"r\": 5}}, \"v\": 5}")
    , ("a record pattern binds the rest",           compilesToJSON "(graph g (:r (case (:a 1 :b 2 :c 3) ((:a ?first ?..others) (:x first :y others)) (otherwise 0))))"
                                                                   "{\"r\": {\"x\": 1, \"y\": {\"b\": 2, \"c\": 3}}}")
    , ("empty records and arrays",                  compilesToJSON "(graph g (:r #record-empty :s #array-empty :t ()))"
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
    , ("len counts an array's elements",            compilesToJSON "(graph g (:a (len (1 (2 3) (:k 4))) :b (len ()) :c (len #array-empty)))"
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
    , ("a pair whose value is null is there",       compilesToJSON "(graph g (:a (has (:k null) k) :b (eq null null)))"
                                                                   "{\"a\": true, \"b\": true}")
    , ("#string asserts a string",                  failsWith "expectedProperty = \"#string\", actualProperty = \"#number\""
                                                              "(graph g (:a #string 1))")
    , ("recursion is an error",                     failsWith "a recursive function call"
                                                              "(defn f (n) (f n)) (graph g (:r (f 1)))")
    , ("so is mutual recursion",                    failsWith "a recursive function call"
                                                              "(defn f (n) (h n)) (defn h (n) (f n)) (graph g (:r (f 1)))")
    , ("and a function applied to itself",          failsWith "a recursive function call"
                                                              "(defn w (x) (x x)) (graph g (:r (w w)))")
    , ("a fn too, with no name to recurse by",      failsWith "a recursive function call"
                                                              "(graph g (let ($w (fn (x) (x x))) (:r (w w))))")
    , ("and through map",                           failsWith "a recursive function call"
                                                              "(graph g (let ($w (fn (x) (map (fn (y) (y y)) (list x)))) (:r (w w))))")
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

    -- (and a ...) is true when every operand is, and (or a ...) when one is. Each stops
    -- at the operand that decides it, and checks every operand it reaches as a #boolean
    , ("and is true when every operand is",         compilesToJSON "(graph g (:a (and true true) :b (and true false) :c (and true) :d (and)))"
                                                                   "{\"a\": true, \"b\": false, \"c\": true, \"d\": true}")
    , ("or is true when one operand is",            compilesToJSON "(graph g (:a (or false true) :b (or false false) :c (or)))"
                                                                   "{\"a\": true, \"b\": false, \"c\": false}")
    , ("and stops at the first false",              compilesToJSON "(graph g (:r (and false (get () 5)) :s (and false 1)))"
                                                                   "{\"r\": false, \"s\": false}")
    , ("or stops at the first true",                compilesToJSON "(graph g (:r (or true (get () 5))))"
                                                                   "{\"r\": true}")
    , ("an operand must be a #boolean",             failsWith "expectedProperty = \"#boolean\", actualProperty = \"#number\""
                                                              "(graph g (:r (and true 1)))")
    , ("so must a lone operand",                    failsWith "expectedProperty = \"#boolean\", actualProperty = \"#number\""
                                                              "(graph g (:r (and 1)))")
    , ("or checks its operands too",                failsWith "expectedProperty = \"#boolean\", actualProperty = \"#number\""
                                                              "(graph g (:r (or 1 true)))")
    , ("and and or combine comparisons",            compilesToJSON "(graph g (:r (and (> 2 1) (or (eq \"a\" \"b\") (eq \"a\" \"a\")))))"
                                                                   "{\"r\": true}")
    , ("and and or take holes",                     compilesToJSON "(graph g (:r (let ($f (and ? true)) (f false)) :s (map (or ? false) (true false))))"
                                                                   "{\"r\": false, \"s\": [true, false]}")
    , ("and and or are threading steps",            compilesToJSON "(graph g (:r (-> true (and false)) :s (-> false (or true))))"
                                                                   "{\"r\": false, \"s\": true}")
    , ("and cannot name a function",                failsWith "'and' is read by its syntax before any name is bound"
                                                              "(defn and (x) x) (graph g (:r (and 1)))")
    , ("nor can or",                                failsWith "'or' is read by its syntax before any name is bound"
                                                              "(defn or (x) x) (graph g (:r 1))")
    , ("and and or guard an index",                 compilesToJSON "(graph g (let ($tags ()) (:featured (and (> (len tags) 0) (eq (get tags 0) \"featured\")) :listed (or (eq (len tags) 0) (eq (get tags 0) \"listed\")))))"
                                                                   "{\"featured\": false, \"listed\": true}")
    , ("a chained comparison stops at the first false", compilesToJSON "(graph g (:r (< 2 1 (get () 5))))"
                                                                       "{\"r\": false}")
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

      -- A key is its value's: a key with none is an error, and a compact IRI is one key
    , ("a key followed by a key has no value",      importsFailAt "SYN0011" ("inline-test", 1, 11) ["valuelessKey = \"a\"", "followedBy = \"the key ':b'\"", "The key ':a' has no value"] []
                                                                  "(graph g (:a :b 1 :y 2))")
    , ("so does a key at the end of its form",      importsFailAt "SYN0011" ("inline-test", 1, 16) ["valuelessKey = \"b\"", "the end of its form"] []
                                                                  "(graph g (:a 1 :b))")
    , ("a key alone in its form",                   importsFailAt "SYN0011" ("inline-test", 1, 11) ["valuelessKey = \"a\"", "the end of its form"] []
                                                                  "(graph g (:a))")
    , ("a compact IRI is one key",                  compilesToJSON "(graph g (:crm:P2_has_type \"x\" :y 2))"
                                                                   "{\"crm:P2_has_type\": \"x\", \"y\": 2}")
    , ("a compact IRI key is a binding too",        compilesToJSON "(graph g (:crm:P2_has_type \"x\" :v crm:P2_has_type))"
                                                                   "{\"crm:P2_has_type\": \"x\", \"v\": \"x\"}")
    , ("a path can name a compact IRI key",         compilesToJSON "(graph g (let ($r (:crm:P2_has_type \"x\")) (:v (get r crm:P2_has_type))))"
                                                                   "{\"v\": \"x\"}")
    , ("a keyword key still starts with @",         compilesToJSON "(graph g (:context (:@vocab \"https://example.org/\") :a 1))"
                                                                   "{\"@context\": {\"@vocab\": \"https://example.org/\"}, \"a\": 1}")
    ]

    where
    base = "(defn base () (:type \"Base\" :id \"1\")) "


-- The type rules as they are: values carry their types, built-ins check them, and nothing
-- is converted implicitly. Arrays are like JSON arrays: their elements can be of any type.
-- Several rules here are under review (equality across types, ordering, extending URIs);
-- a change to one flips its test.
types :: Group
types = Group "Types: the rules as they are"
    [ -- The built-in properties, by their full names
      ("every built-in property, by its full name", compilesToJSON "(defn twice #number (#function f #number n) (f (f n))) (graph g (:s #string \"a\" :n (twice (fn (#number x) (* x 2)) 1) :b #boolean true :r #record (:a 1) :a #array (1) :e #array-empty :o #record-empty))"
                                                                   "{\"s\": \"a\", \"n\": 4, \"b\": true, \"r\": {\"a\": 1}, \"a\": [1], \"e\": [], \"o\": {}}")
    , ("#str is not a property: #string is",        failsWith "Unknown property: #str"       "(graph g (:r #str \"a\"))")
    , ("#num is not a property: #number is",        failsWith "Unknown property: #num"       "(graph g (:r #num 1))")
    , ("#bool is not a property: #boolean is",      failsWith "Unknown property: #bool"      "(graph g (:r #bool true))")
    , ("#rec is not a property: #record is",        failsWith "Unknown property: #rec"       "(graph g (:r #rec (:a 1)))")
    , ("#arr is not a property: #array is",         failsWith "Unknown property: #arr"       "(graph g (:r #arr (1)))")
    , ("#fn is not a property: #function is",       failsWith "Unknown property: #fn"        "(graph g (:r #fn 1))")
    , ("#rec-empty is not a value: #record-empty is", failsWith "Unknown property: #rec-empty" "(graph g (:r #rec-empty))")
    , ("#arr-empty is not a value: #array-empty is", failsWith "Unknown property: #arr-empty" "(graph g (:r #arr-empty))")
      -- Arrays
    , ("an array may mix types",                    compilesToJSON "(graph g (:r (1 \"a\" true)))"
                                                                   "{\"r\": [1, \"a\", true]}")
    , ("+ joins any two arrays",                    compilesToJSON "(graph g (:r (+ (1 \"a\") (2 3)) :s (+ (1 2) (\"a\"))))"
                                                                   "{\"r\": [1, \"a\", 2, 3], \"s\": [1, 2, \"a\"]}")
    , ("+ joins empty arrays",                      compilesToJSON "(graph g (:r (+ () (1)) :s (+ () ()) :t (+ #array-empty (1))))"
                                                                   "{\"r\": [1], \"s\": [], \"t\": [1]}")
    , ("insert takes any value into an array",      compilesToJSON "(graph g (:r (insert 0 \"x\" (1 2)) :s (insert 0 1 #array-empty)))"
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
    , ("#number does not convert a string",         failsWith "expectedProperty = \"#number\", actualProperty = \"#string\""
                                                              "(graph g (:r #number \"5\"))")
    , ("#uri does not accept a date",               failsWith "expectedProperty = \"#uri\", actualProperty = \"#date\""
                                                              ("(graph g (:r #uri " <> date <> "))"))

      -- A failed property names both properties, and says why when the value's own is not enough
    , ("a URI needs a scheme",                      failsWith "actualProperty = \"a #string without a URI scheme\""
                                                              "(graph g (:r #uri \"nope\"))")
    , ("a date needs ISO-8601 form",                failsWith "actualProperty = \"a #string that is not a date\""
                                                              "(graph g (:r #date \"1642\"))")
    , ("null has no built-in property",             failsWith "expectedProperty = \"#string\", actualProperty = \"null\""
                                                              "(graph g (:r #string null))")

      -- A URI keeps the slashes of its path; gaps 17 and 22 are the exceptions
    , ("#uri keeps a trailing slash",               compilesToJSON "(graph g (:u #uri \"https://a.org/x/\"))"
                                                                   "{\"u\": \"https://a.org/x/\"}")
    , ("a string keeps a doubled slash",            compilesToJSON "(graph g (:u \"https://a.org/x//y\"))"
                                                                   "{\"u\": \"https://a.org/x//y\"}")
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
    , ("a chained comparison under a property",     compilesToJSON "(graph g (:r #boolean (< 1 2 3) :s #number (* 2 3 4)))"
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
                                                  content <- evalIO (readSourceFile (fixture "ouro/encoding.ouro"))
                                                  fmap (T.isInfixOf "\201douard Manet") content === Right True)
    , ("names and keys can be any letters",  compilesToJSON "(defn caf\233 () \"x\") (graph g (:r (caf\233) :na\239ve 1))"
                                                                 "{\"r\": \"x\", \"na\239ve\": 1}")
    , ("\955 is fn",                         compilesToJSON "(graph g (let ($inc (\955 (x) (+ x 1))) (:r (inc 1))))"
                                                                 "{\"r\": 2}")
    , ("with a signature and a pattern",     compilesToJSON "(graph g (:r (map (\955 #number ((?a ?b)) (+ a b)) ((1 2) (3 4)))))"
                                                                 "{\"r\": [3, 7]}")
    , ("\955 can name a function, as fn",     compilesToJSON "(defn \955 (x) x) (graph g (:r (fn 1)))"
                                                            "{\"r\": 1}")
    , ("\955x is an ordinary name",          compilesToJSON "(graph g (let ($\955x 1) (:r \955x)))"
                                                                 "{\"r\": 1}")

      -- The symbols of logic and sets are their names
    , ("\172 \8743 \8744 are not, and, or",  compilesToJSON "(graph g (:a (\172 true) :b (\8743 true false) :c (\8744 false true)))"
                                                                 "{\"a\": false, \"b\": false, \"c\": true}")
    , ("\8704 \8707 \8708 are the quantifiers", compilesToJSON "(graph g (:a (\8704 (> ? 0) (1 2)) :b (\8707 (> ? 1) (1 2)) :c (\8708 (> ? 5) (1 2))))"
                                                                    "{\"a\": true, \"b\": true, \"c\": true}")
    , ("\8746 \8745 \8726 \8838 are the set operations", compilesToJSON "(graph g (:a (\8746 (1) (2)) :b (\8745 (1 2) (2)) :c (\8726 (1 2) (2)) :d (\8838 (1) (1 2))))"
                                                                            "{\"a\": [1, 2], \"b\": [2], \"c\": [1], \"d\": true}")
    , ("messages name the word",             failsWith "'and' is read by its syntax before any name is bound"
                                                            "(defn \8743 (x) x) (graph g (:r 1))")
    , ("\8743x is an ordinary name",         compilesToJSON "(graph g (let ($\8743x 1) (:r \8743x)))"
                                                                 "{\"r\": 1}")
    , ("= and /= are eq and neq",            compilesToJSON "(graph g (:a (= 1 1) :b (/= 1 2) :c (= 1 1 2) :d (case 3 ((= 3) \"three\") (otherwise \"other\"))))"
                                                                 "{\"a\": true, \"b\": true, \"c\": false, \"d\": \"three\"}")
    , ("\8899 \8898 are unions and intersections", compilesToJSON "(graph g (:a (\8899 ((1) (2))) :b (\8898 ((1 2) (2)))))"
                                                                      "{\"a\": [1, 2], \"b\": [2]}")
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


-- A file that ends with a delimiter still open is reported at that delimiter, the innermost
-- one still open, in the file it is in. The token stream has no position past its end, and
-- once reported this as "unknown-source" 1:1, which the CLI then crashed trying to read.
endOfFile :: Group
endOfFile = Group "Unclosed delimiters: a file that ends early"
    [ ("a graph left open",                  unclosedAt (1, 1)  "')'" "(graph g (:r 1)")
    , ("the innermost delimiter still open", unclosedAt (1, 10) "')'" "(graph g (:r (list 1 2)")
    , ("an array left open",                 unclosedAt (1, 14) "']'" "(graph g (:r [1 2")
    , ("a key with no value",                unclosedAt (1, 10) "')'" "(graph g (:r")
    , ("a property with no subject",         unclosedAt (1, 10) "')'" "(graph g (:r #string")
    , ("a defn with no name",                unclosedAt (1, 1)  "')'" "(defn")
    , ("a parameter list left open",         unclosedAt (1, 9)  "')'" "(defn f (x y")
    , ("an import left open",                unclosedAt (1, 1)  "')'" "(import std/base")
    , ("on its line, in a later form",       unclosedAt (3, 3)  "')'" "(graph a (:r 1))\n(graph b\n  (:r (list 1)")
    , ("in an imported module, in its file", importsFailAt "SYN0001" ("open.ouro", 2, 1) ["')' to close this", "the end of the file"] [("open", "(export f)\n(defn f () 1")]
                                                           "(import open) (graph g (:r (open/f)))")
    ]
    where
    unclosedAt (line, column) closer = importsFailAt "SYN0001" ("inline-test", line, column) [closer <> " to close this", "the end of the file"] []


-- A string is written as a JSON string: quotes, backslashes and control characters are
-- escaped, and every other character is written as it is. This fixed gap 19.
strings :: Group
strings = Group "Strings: escaped in the output"
    [ ("a quote is escaped",                     emitsExactly "(graph g (:quote \"say \\\"hi\\\"\"))"
                                                              "\"quote\": \"say \\\"hi\\\"\"")
    , ("a backslash is escaped",                 emitsExactly "(graph g (:slash \"a\\\\b\"))"
                                                              "\"slash\": \"a\\\\b\"")
    , ("a newline is escaped",                   emitsExactly "(graph g (:newline \"a\\nb\"))"
                                                              "\"newline\": \"a\\nb\"")
    , ("a tab is escaped",                       emitsExactly "(graph g (:tab \"a\\tb\"))"
                                                              "\"tab\": \"a\\tb\"")
    , ("other control characters are \\u escapes", emitsExactly "(graph g (:bell \"a\\1b\"))"
                                                              "\"bell\": \"a\\u0001b\"")
    , ("other characters are written as they are", emitsExactly "(graph g (:r \"caf\233\"))"
                                                              "\"r\": \"caf\233\"")
    , ("validate keeps an escaped quote",        withTests 1 . property $
                                                  fmap (T.isInfixOf "\"k\": \"a\\\"b\"" . TL.toStrict)
                                                       (validate "t.json" "{\"k\": \"a\\\"b\"}" defaultOptions)
                                                  === Right True)
    ]


-- not negates; exists, forall and filter call a function on each element, as map does,
-- and exists and forall stop at the element that decides; contains and distinct compare as eq
-- does; has and get-or walk a path as get does, and tell a missing key from an error; image
-- reads a path as a set, through Arrays.
logicAndCollections :: Group
logicAndCollections = Group "Built-ins: logic, arrays and records"
    [ ("not negates",                            compilesToJSON "(graph g (:a (not true) :b (not false)))"
                                                                "{\"a\": false, \"b\": true}")
    , ("not needs a Boolean",                    failsWith "expectedProperty = \"#boolean\""
                                                           "(graph g (:r (not 1)))")
    , ("not takes one operand",                  failsWith "nameOfFunc = \"not\""
                                                           "(graph g (:r (not true false)))")

      -- exists, forall and filter
    , ("exists finds an element",                compilesToJSON "(graph g (:r (exists (> ? 2) (1 2 3))))"
                                                                "{\"r\": true}")
    , ("exists of nothing is false",             compilesToJSON "(graph g (:r (exists (> ? 2) ())))"
                                                                "{\"r\": false}")
    , ("forall checks each element",             compilesToJSON "(graph g (:a (forall (> ? 0) (1 2 3)) :b (forall (> ? 1) (1 2 3))))"
                                                                "{\"a\": true, \"b\": false}")
    , ("forall of nothing is true",              compilesToJSON "(graph g (:r (forall (> ? 2) ())))"
                                                                "{\"r\": true}")
    , ("exists stops at the deciding element",   compilesToJSON "(graph g (:r (exists (fn (x) (> (len x) 1)) (\"ab\" 5))))"
                                                                "{\"r\": true}")
    , ("forall stops at the deciding element",   compilesToJSON "(graph g (:r (forall (fn (x) (> (len x) 5)) (\"ab\" 5))))"
                                                                "{\"r\": false}")
    , ("none is not exists",                     compilesToJSON "(graph g (:r (not (exists (eq ? 3) (1 2)))))"
                                                                "{\"r\": true}")
    , ("omits is true when no element answers true", compilesToJSON "(graph g (:a (omits (> ? 2) (1 2)) :b (omits (> ? 2) (1 5))))"
                                                                    "{\"a\": true, \"b\": false}")
    , ("omits of nothing is true",               compilesToJSON "(graph g (:r (omits (> ? 2) ())))"
                                                                "{\"r\": true}")
    , ("omits stops at the deciding element",    compilesToJSON "(graph g (:r (omits (fn (x) (> (len x) 1)) (\"ab\" 5))))"
                                                                "{\"r\": false}")
    , ("omits finds no contradiction",           compilesToJSON "(graph g (let ($book (:tp46 (\"a\" \"b\") :ntp46 (\"c\"))) (:r (omits (contains (get-or () book ntp46) ?) (get-or () book tp46)))))"
                                                                "{\"r\": true}")
    , ("omits needs a function",                 failsWith "a function for omits to call"
                                                           "(graph g (:r (omits 1 (1 2))))")
    , ("omits can name a function",              compilesToJSON "(defn omits (x) x) (graph g (:r (omits 1)))"
                                                           "{\"r\": 1}")
    , ("filter keeps the matching elements",     compilesToJSON "(graph g (:r (filter (> ? 1) (1 2 3))))"
                                                                "{\"r\": [2, 3]}")
    , ("filter with has",                        compilesToJSON "(graph g (:r (filter (fn (p) (has p name)) ((:name \"a\") (:id 1)))))"
                                                                "{\"r\": [{\"name\": \"a\"}]}")
    , ("a hole for the array",                   compilesToJSON "(graph g (let ($big (exists (> ? 2) ?)) (:r (big (1 5)))))"
                                                                "{\"r\": true}")
    , ("the answer must be a Boolean",           failsWith "expectedProperty = \"#boolean\""
                                                           "(graph g (:r (exists (+ ? 1) (1 2))))")
    , ("exists needs a function",                failsWith "a function for exists to call"
                                                           "(graph g (:r (exists 1 (1 2))))")
    , ("forall needs an Array",                  failsWith "an Array for forall to go through"
                                                           "(graph g (:r (forall (> ? 1) 5)))")
    , ("a malformed filter is an error",         failsWith "filter is written (filter f array)"
                                                           "(graph g (:r (filter (> ? 1))))")
    , ("exists can name a function",             compilesToJSON "(defn exists (x) x) (graph g (:r (exists 1)))"
                                                           "{\"r\": 1}")

      -- contains and distinct
    , ("contains looks in an Array",             compilesToJSON "(graph g (:a (contains (1 2 3) 2) :b (contains (1 2) 5)))"
                                                                "{\"a\": true, \"b\": false}")
    , ("contains compares records by value",     compilesToJSON "(graph g (:r (contains ((:a 1)) (:a 1))))"
                                                                "{\"r\": true}")
    , ("contains looks in a String",             compilesToJSON "(graph g (:a (contains \"Amsterdam\" \"dam\") :b (contains \"Amsterdam\" \"x\")))"
                                                                "{\"a\": true, \"b\": false}")
    , ("contains cannot compare functions",      failsWith "functions cannot be compared"
                                                           "(graph g (:r (contains (list (fn (x) x)) 1)))")
    , ("contains needs an Array or a String",    failsWith "an Array or a String to search"
                                                           "(graph g (:r (contains 5 1)))")
    , ("distinct keeps the first of each",       compilesToJSON "(graph g (:r (distinct (1 2 1 3 2))))"
                                                                "{\"r\": [1, 2, 3]}")
    , ("distinct of nothing is nothing",         compilesToJSON "(graph g (:r (distinct ())))"
                                                                "{\"r\": []}")
    , ("distinct needs an Array",                failsWith "expectedType = \"an Array\""
                                                           "(graph g (:r (distinct \"aa\")))")

      -- has and get-or
    , ("has follows a path",                     compilesToJSON "(graph g (:a (has (:x (:y 1)) x y) :b (has (:x 1) z)))"
                                                                "{\"a\": true, \"b\": false}")
    , ("has reports a path through a value",     failsWith "a Record to look up"
                                                           "(graph g (:r (has (:x 1) x y)))")
    , ("get-or gives the value when it is there", compilesToJSON "(graph g (:r (get-or \"none\" (:x 1) x)))"
                                                                 "{\"r\": 1}")
    , ("get-or gives the default when it is not", compilesToJSON "(graph g (:a (get-or \"none\" (:x 1) z) :b (get-or 0 (:x (:y 1)) x z)))"
                                                                 "{\"a\": \"none\", \"b\": 0}")
    , ("get-or evaluates the default only when used", compilesToJSON "(graph g (:r (get-or (get () 5) (:x 1) x)))"
                                                                     "{\"r\": 1}")
    , ("a malformed get-or is an error",         failsWith "get-or is written (get-or default record key ...)"
                                                           "(graph g (:r (get-or 1 (:x 1))))")

      -- indexes in paths
    , ("get takes an index into an Array",       compilesToJSON "(graph g (:r (get (:k ((:a 1) (:a 2))) k 1 a)))"
                                                                "{\"r\": 2}")
    , ("a negative index counts from the end",   compilesToJSON "(graph g (:r (get (:k (1 2 3)) k -1)))"
                                                                "{\"r\": 3}")
    , ("an index past the end is an error",      failsWith "Index"
                                                           "(graph g (:r (get (:k (1 2)) k 5)))")
    , ("an index needs an Array",                failsWith "an Array to take element 0 of"
                                                           "(graph g (:r (get (:k (:a 1)) k 0)))")
    , ("an index is a whole number",             failsWith "a whole number to index an Array with"
                                                           "(graph g (:r (get (:k (1 2)) k 0.5)))")
    , ("a key on an Array suggests an index",    failsWith "or read 'a' from every element with image"
                                                           "(graph g (:r (get (:k ((:a 1))) k a)))")
    , ("has is false past the end",              compilesToJSON "(graph g (:a (has (:k (1 2)) k 1) :b (has (:k (1 2)) k 2)))"
                                                                "{\"a\": true, \"b\": false}")
    , ("get-or gives the default past the end",  compilesToJSON "(graph g (:r (get-or 0 (:k ()) k 0)))"
                                                                "{\"r\": 0}")
    , ("image takes no index",                   failsWith "a set has no positions"
                                                           "(graph g (:r (image (:k (1 2)) k 0)))")
    , ("a hole cannot stand for an index",       failsWith "a hole cannot stand for a key or an index"
                                                           "(graph g (let ($at (get (1 2) ?)) (:r (at 0))))")
    , ("nor a named hole for a key",             failsWith "a hole cannot stand for a key or an index"
                                                           "(graph g (let ($at (has (:a 1) ?k)) (:r (at 1))))")

      -- image reads a path as a set
    , ("image follows a path through Arrays",    compilesToJSON "(graph g (:r (image (:part ((:k (1 2)) (:k (2 3)))) part k)))"
                                                                "{\"r\": [1, 2, 3]}")
    , ("image of a single value is one element", compilesToJSON "(graph g (:r (image (:k 1) k)))"
                                                                "{\"r\": [1]}")
    , ("a missing key adds nothing",             compilesToJSON "(graph g (:a (image (:part ((:k 1) (:x 2))) part k) :b (image (:x 1) k)))"
                                                                "{\"a\": [1], \"b\": []}")
    , ("image starts from an Array",             compilesToJSON "(graph g (:r (image ((:k 1) (:k 2)) k)))"
                                                                "{\"r\": [1, 2]}")
    , ("image compares records by value",        compilesToJSON "(graph g (:r (image (:p ((:t (:id 1)) (:t (:id 1)))) p t)))"
                                                                "{\"r\": [{\"id\": 1}]}")
    , ("image compares kinds by id",             compilesToJSON "(graph g (:r (image (:p ((:t ((:id \"a\" :_label \"x\"))) (:t ((:id \"a\" :_label \"y\"))))) p t id)))"
                                                                "{\"r\": [\"a\"]}")
    , ("image takes a hole",                     compilesToJSON "(graph g (:r (map (image ? k) ((:k 1) (:k (2 3))))))"
                                                                "{\"r\": [[1], [2, 3]]}")
    , ("image reports a value on the way",       failsWith "a Record to follow 'j' from"
                                                           "(graph g (:r (image (:k 1) k j)))")
    , ("image keeps errors",                     failsWith "Index"
                                                           "(graph g (:r (image (:p ((:k (get () 5)))) p k)))")
    , ("a malformed image is an error",          failsWith "image is written (image record key ...)"
                                                           "(graph g (:r (image (:k 1))))")
    , ("image can name a function",              compilesToJSON "(defn image (x) x) (graph g (:r (image 1)))"
                                                           "{\"r\": 1}")

      -- union, intersect and setminus treat Arrays as sets
    , ("union joins without repeats",            compilesToJSON "(graph g (:a (union (1 2) (2 3)) :b (union (1 1)) :c (union (1) (2) (1 3))))"
                                                                "{\"a\": [1, 2, 3], \"b\": [1], \"c\": [1, 2, 3]}")
    , ("intersect keeps what all share",         compilesToJSON "(graph g (:a (intersect (1 2 3) (3 2 4)) :b (intersect (1 2) (3)) :c (intersect (1 2 3) (2 3) (3))))"
                                                                "{\"a\": [2, 3], \"b\": [], \"c\": [3]}")
    , ("setminus removes the rest",              compilesToJSON "(graph g (:a (setminus (1 2 3) (2)) :b (setminus (1 2 3) (1) (3)) :c (setminus (1 1 2) ())))"
                                                                "{\"a\": [1, 3], \"b\": [2], \"c\": [1, 2]}")
    , ("sets compare records by value",          compilesToJSON "(graph g (:r (intersect ((:a 1) (:b 2)) ((:a 1)))))"
                                                                "{\"r\": [{\"a\": 1}]}")
    , ("set operations take holes",              compilesToJSON "(graph g (:r (map (setminus ? (2)) ((1 2) (2 3)))))"
                                                                "{\"r\": [[1], [3]]}")
    , ("a set must be an Array",                 failsWith "an Array, as a set"
                                                           "(graph g (:r (union (1) 2)))")
    , ("a set cannot hold a function",           failsWith "functions cannot be compared"
                                                           "(graph g (:r (union (list (fn (x) x)))))")
    , ("a set operation needs an Array",         failsWith "nameOfFunc = \"intersect\""
                                                           "(graph g (:r (intersect)))")
    , ("subset is true when all are in the other", compilesToJSON "(graph g (:a (subset (1 2) (2 3 1)) :b (subset (1 4) (1 2)) :c (subset () (1)) :d (subset (1 1) (1))))"
                                                                  "{\"a\": true, \"b\": false, \"c\": true, \"d\": true}")
    , ("subset compares records by value",       compilesToJSON "(graph g (:r (subset ((:a 1)) ((:b 2) (:a 1)))))"
                                                                "{\"r\": true}")
    , ("subset takes two Arrays",                failsWith "nameOfFunc = \"subset\""
                                                           "(graph g (:r (subset (1) (1) (1))))")
    , ("unions joins an Array of Arrays",        compilesToJSON "(graph g (:a (unions ((1 2) (2 3) (4))) :b (unions ())))"
                                                                "{\"a\": [1, 2, 3, 4], \"b\": []}")
    , ("intersections of an Array of Arrays",    compilesToJSON "(graph g (:r (intersections ((1 2 3) (2 3) (3 2)))))"
                                                                "{\"r\": [2, 3]}")
    , ("unions of computed sets",                compilesToJSON "(graph g (:r (unions (map (get ? k) ((:k (1)) (:k (1 2)))))))"
                                                                "{\"r\": [1, 2]}")
    , ("intersections of nothing is an error",   failsWith "at least one Array to intersect"
                                                           "(graph g (:r (intersections ())))")
    , ("unions needs an Array of Arrays",        failsWith "an Array of Arrays"
                                                           "(graph g (:r (unions 5)))")
    , ("each must be an Array",                  failsWith "an Array, as a set"
                                                           "(graph g (:r (unions ((1) 2))))")
    ]


-- A case branch can be headed by a condition, (when cond), as well as by a pattern, a
-- literal or a comparison guard. A condition must be a Boolean; true takes the branch.
conditions :: Group
conditions = Group "Case: conditions"
    [ ("when takes a branch when true",          compilesToJSON "(graph g (:r (case 1 ((when false) \"a\") ((when (eq 1 1)) \"b\") (otherwise \"c\"))))"
                                                                "{\"r\": \"b\"}")
    , ("when can use other values",              compilesToJSON "(graph g (let ($strict true) (:r (case 5 ((when strict) \"strict\") (otherwise \"lenient\")))))"
                                                                "{\"r\": \"strict\"}")
    , ("when takes and and or",                  compilesToJSON "(graph g (:r (case 5 ((when (and (> 5 1) (< 5 9))) \"in\") (otherwise \"out\"))))"
                                                                "{\"r\": \"in\"}")
    , ("when sits beside patterns and literals", compilesToJSON "(graph g (:r (case (:k 1) (2 \"two\") ((when false) \"no\") ((:k ?v) v) (otherwise \"other\"))))"
                                                                "{\"r\": 1}")
    , ("a condition must be a Boolean",          failsWith "expectedProperty = \"#boolean\""
                                                           "(graph g (:r (case 5 ((when 1) \"a\") (otherwise \"b\"))))")
    , ("when outside a case is an error",        failsWith "when is written as the head of a case branch"
                                                           "(graph g (:r (when true)))")
    , ("when can name a function",               compilesToJSON "(defn when (x) x) (graph g (:r (when 1)))"
                                                           "{\"r\": 1}")

      -- Comparison guards become conditions on the case's target
    , ("a guard compares a computed target",     compilesToJSON "(graph g (:r (case (+ 2 3) ((> 4) \"big\") (otherwise \"small\"))))"
                                                                "{\"r\": \"big\"}")
    , ("a chained guard stops at the first false", compilesToJSON "(graph g (:r (case 5 ((< 1 (get () 9)) \"x\") (otherwise \"y\"))))"
                                                                  "{\"r\": \"y\"}")

      -- satisfies asks a property; in a branch head, of the case's target
    , ("satisfies asks a property",              compilesToJSON "(graph g (:a (satisfies #string \"x\") :b (satisfies #number \"x\") :c (satisfies #uri \"https://a.org/x\") :d (satisfies #uri \"nope\")))"
                                                                "{\"a\": true, \"b\": false, \"c\": true, \"d\": false}")
    , ("satisfies keeps errors",                 failsWith "Index"
                                                           "(graph g (:r (satisfies #string (get () 5))))")
    , ("satisfies heads a branch",               compilesToJSON "(graph g (:r (map (fn (v) (case v ((satisfies #uri) \"an identifier\") ((satisfies #number) \"a number\") (otherwise \"other\"))) (list #uri \"https://a.org/1\" 3 \"x\"))))"
                                                                "{\"r\": [\"an identifier\", \"a number\", \"other\"]}")
    , ("\8872 is satisfies",                     compilesToJSON "(graph g (:r (case 3 ((\8872 #number) \"n\") (otherwise \"o\"))))"
                                                                "{\"r\": \"n\"}")
    , ("a subject-less satisfies needs a case",  failsWith "satisfies is written"
                                                           "(graph g (:r (map (satisfies #string) (list 1))))")
    , ("a property on its own is misplaced",     failsWith "A property on its own"
                                                           "(graph g (:r (list #string)))")
    , ("satisfies can name a function",          compilesToJSON "(defn satisfies (x) x) (graph g (:r (satisfies 1)))"
                                                           "{\"r\": 1}")
    ]


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


-- A special form's name is bound like any other name, as in Scheme: a binding around a form,
-- whether an attribute key, a let binding, a parameter or a defn, shadows it, and the form is
-- then an ordinary call of the binding. The forms Canon writes are not the source's, so no
-- binding can capture them.
specialFormsAreBindings :: Group
specialFormsAreBindings = Group "Special forms are bindings, as in Scheme"
    [ ("a let binding shadows a special form",      compilesToJSON "(graph g (let ($list (fn (x) (+ x 1))) (:r (list 1))))"
                                                                   "{\"r\": 2}")
    , ("as does a bound map, which is called",      compilesToJSON "(graph g (let ($map (fn (x) x)) (:r (map 1))))"
                                                                   "{\"r\": 1}")
    , ("a parameter, even one named fn",            compilesToJSON "(defn call (fn) (fn)) (graph g (:r (call (fn () 1))))"
                                                                   "{\"r\": 1}")
    , ("a defn, throughout its module",             compilesToJSON "(defn list (x) (+ x 1)) (graph g (:r (list 1)))"
                                                                   "{\"r\": 2}")
    , ("with a warning, as for a built-in",         warnsWith "or a special form replaces it throughout the file"
                                                              "(defn list (x) (+ x 1)) (graph g (:r (list 1)))")
    , ("and its calls have its arity",              failsWith "IncorrectArity"
                                                              "(defn list (x) x) (defn h () (list 1 2)) (graph g (:r 1))")
    , ("a key, whose value is no function",         failsWith "'image' is also a special form, which a binding of the same name shadows"
                                                              "(graph g (:image \"x\" :r (image \"a\" 1)))")
    , ("only where the binding is",                 compilesToJSON "(graph g (:a (let ($list 1) list) :b (list 1 2)))"
                                                                   "{\"a\": 1, \"b\": [1, 2]}")
    , ("an inlay is shadowed too",                  compilesToJSON "(graph g (let ($inlay (fn (x) x)) (:r ((inlay 1) 2))))"
                                                                   "{\"r\": [1, 2]}")
    , ("Canon's forms are not captured",            compilesToJSON "(graph g (let ($case 1 $fn 2 $inlay 3) (:r (or false true) :s (map (+ ? 1) (1 2)) :t (with (:a 1) :b 2))))"
                                                                   "{\"r\": true, \"s\": [2, 3], \"t\": {\"a\": 1, \"b\": 2}}")
    , ("nor is a guard's",                          compilesToJSON "(graph g (let ($let 4) (:r (case 5 ((< 3) \"small\") (otherwise \"big\")))))"
                                                                   "{\"r\": \"big\"}")
    ]


-- Known gaps, numbered as in the specification's Appendix B. Each test pins the current,
-- incorrect behaviour; the change that fixes a gap adds a test of the correct behaviour and
-- keeps the pin, run negated (fixedGap), so the old behaviour coming back fails the suite.
knownGaps :: Group
knownGaps = Group "Known gaps: current and fixed"
    [ ("gap 9 (fixed): no suggestion for a local name", fixedGap $ failsWithout "Perhaps you meant"
                                                                 "(graph g (let ($artist 1) (:r artst)))")
    , ("gap 10 (fixed): get's missing-key hint mentions define", fixedGap $ failsWith "'define'"
                                                              "(graph g (:p (:a 1) :r (get p b)))")
    , ("gap 10 (fixed): subtraction's hint names L.Duration", fixedGap $ failsWith "L.Duration"
                                                              "(graph g (:r (- \"a\" 1)))")
    , ("gap 10 (fixed): a reserved $ name is called an attribute key", fixedGap $ failsWith "for your attribute"
                                                              "(graph g (let ($get 1) (:r get)))")
    , ("gap 11 (fixed): a parameter named fn",      fixedGap $ failsWith "A fn is written"
                                                              "(defn call (fn) (fn)) (graph g (:r (call 1)))")
    , ("gap 14: a record of only inlays is an array", failsWith "a valid Array to inlay"
                                                              "(defn a () (:k 1)) (defn b () (:j 2)) (graph g ((inlay (a)) (inlay (b))))")
    , ("gap 17: a URI's empty root path is dropped", compilesToJSON "(graph g (:u #uri \"https://a.org/\"))"
                                                                   "{\"u\": \"https://a.org\"}")
    , ("gap 18: an attribute pair among values has no hint", failsWithout "(1 (:a 2))"
                                                                 "(graph g (:r (1 :a 2)))")
    , ("gap 21: validate rejects a \\u escape",    withTests 1 . property $
                                                    assert (either (const True) (const False)
                                                                   (validate "t.json" "{\"k\": \"\\u00e9\"}" defaultOptions)))
    , ("gap 21: and an escaped slash",            withTests 1 . property $
                                                    assert (either (const True) (const False)
                                                                   (validate "t.json" "{\"k\": \"\\/\"}" defaultOptions)))
    , ("gap 21: but accepts a Haskell escape",    withTests 1 . property $
                                                    fmap (T.isInfixOf "\"k\": \"A\"" . TL.toStrict)
                                                         (validate "t.json" "{\"k\": \"\\x41\"}" defaultOptions)
                                                    === Right True)
    , ("gap 22: a URI's empty path segment is dropped", compilesToJSON "(graph g (:u #uri \"https://a.org/x//y\"))"
                                                                      "{\"u\": \"https://a.org/x/y\"}")
    , ("gap 22: so is a leading one",              compilesToJSON "(graph g (:u #uri \"https://a.org//x\"))"
                                                                   "{\"u\": \"https://a.org/x\"}")
    , ("gap 23: an unterminated string is reported where the input ends", importsFailAt "SYN0000" ("inline-test", 1, 21) ["Malformed or unrecognized lexical token sequence"] []
                                                                                "(graph g (:r \"open))")
    , ("gap 24: #date accepts a year of fewer than four digits", compilesToJSON "(graph g (:d #date \"81-01-01T00:00:00Z\"))"
                                                                                "{\"d\": \"81-01-01T00:00:00Z\"}")
    , ("gap 25: warning codes collide across categories", withTests 1 . property $
                                                    map smartWarningCode [ Lint (UnanchoredLiteral "a" "b")
                                                                         , Semantic (ElidedPropertyAssign "k" "define")
                                                                         , Vocab (UnboundTypeReference "T" "r") ]
                                                    === ["WRN0000", "WRN0000", "WRN0000"])
    , ("gap 26: an unknown export is a lexical error", importsFailAt "SYN0000" ("inline-test", 1, 9) ["The module exports 'f', but defines no function by that name"] []
                                                                    "(export f)\n(graph g (:r 1))")
    , ("gap 31: a key cannot hold a '.'",            importsFailAt "SYN0000" ("inline-test", 1, 16) ["Unexpected token: '.'"] []
                                                                     "(graph g (:ex:a.b 1 :y 2))")
    , ("gap 31: a '/' ends a key",                   importsFailAt "SCP0000" ("inline-test", 1, 16) ["variableName = \"/b\""] []
                                                                     "(graph g (:ex:a/b 1 :y 2))")
    , ("gap 31: a '#' ends a key",                   importsFailAt "SYN0000" ("inline-test", 1, 16) ["Unknown property: #b"] []
                                                                     "(graph g (:ex:a#b 1 :y 2))")
    , ("gap 39: insert floors a fractional index",   compilesToJSON "(graph g (:r (insert 1.5 \"x\" (1 2))))"
                                                                   "{\"r\": [1, \"x\", 2]}")
    , ("gap 41: WRN0000 says a value is erased",     warnsWith "will be completely erased"
                                                               "(graph g (:r (#record-empty \"x\")))")
    , ("gap 41: though it is kept",                  compilesToJSON "(graph g (:r (#record-empty \"x\")))"
                                                                   "{\"r\": [{}, \"x\"]}")
    , ("gap 42 (fixed): a removed form's name is reserved as a key", fixedGap $ failsWith "ShadowedVariableError"
                                                                   "(graph g (:a (:attr 1)))")
    , ("gap 42 (fixed): and as a binding",           fixedGap $ failsWith "ShadowedVariableError"
                                                              "(graph g (let ($defun 1) (:r defun)))")
    , ("gap 43: a binding may be named map",         compilesToJSON "(graph g (let ($map 1) (:r map)))"
                                                                   "{\"r\": 1}")
    , ("gap 43: and a key",                          compilesToJSON "(graph g (:map 1))"
                                                                   "{\"map\": 1}")
    , ("gap 43 (fixed): but not list",               fixedGap $ failsWith "ShadowedVariableError"
                                                              "(graph g (let ($list 1) (:r list)))")
    , ("gap 43 (fixed): and a bound map is not called", fixedGap $ failsWith "LexicalError"
                                                              "(graph g (let ($map (fn (x) x)) (:r (map 1))))")
    , ("gap 44 (fixed for names): an uncalled function's unbound name", fixedGap $ compilesToJSON "(defn f () nope) (graph g (:r 1))"
                                                                   "{\"r\": 1}")
    , ("gap 44 (fixed for arity): and its wrong arity", fixedGap $ compilesToJSON "(defn f (x) x) (defn h () (f 1 2)) (graph g (:r 1))"
                                                                   "{\"r\": 1}")
    , ("gap 44 (fixed for names): an untaken branch's unbound name", fixedGap $ compilesToJSON "(graph g (:r (case 1 ((= 1) \"a\") (otherwise nope))))"
                                                                   "{\"r\": \"a\"}")
    , ("gap 44 (fixed): an uncalled function's recursion", fixedGap $ compilesToJSON "(defn f () (f)) (graph g (:r 1))"
                                                                   "{\"r\": 1}")
    , ("gap 45: a binding of and is ignored at a call", compilesToJSON "(graph g (let ($and (fn (a b) 1)) (:r (and true false))))"
                                                                   "{\"r\": false}")
    , ("gap 45: and of with",                        compilesToJSON "(graph g (let ($with (fn (a b) 1)) (:r (with (:a 1) :b 2))))"
                                                                   "{\"r\": {\"a\": 1, \"b\": 2}}")
    , ("gap 46: an unknown top-level form is named by its Haskell constructor", importsFailAt "SYN0000" ("inline-test", 1, 2) ["Invalid top-level keyword: Symbol"] []
                                                                   "(foo f () 1)\n(graph g (:r 1))")
    , ("gap 47: a name that begins with null is read as null and a name", failsWith "Expected a function name"
                                                                   "(defn null-result (x) x) (graph g (:r (null-result 1)))")
    , ("gap 47: and one that begins with true",      failsWith "Expected a function name"
                                                                   "(defn truer (x) x) (graph g (:r (truer 1)))")
    , ("gap 47: and a graph named so",              failsWith "Expected graph name"
                                                                   "(graph null-results (:r 1))")
    , ("gap 47: and an attribute named so, read where it is used", failsWith "an unbound expression"
                                                                   "(graph g (:null-ish 1 :r null-ish))")
    ]
