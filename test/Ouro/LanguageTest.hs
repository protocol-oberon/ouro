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
, stringBuiltins
, logicAndCollections
, conditions
, contexts
, contextAlgebra
, specialFormsAreBindings
, knownGaps
) where

import qualified Data.ByteString           as BS
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
import           Data.Ouro.Internal.Uri    (mkUri)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           Hedgehog
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range
import           Utils                     (compilesToJSON,
                                            compilesWithoutWarnings, emitsExactly,
                                            failsWith, failsWithout, fixedGap,
                                            fixture, importsFailAt,
                                            readSourceFile, shouldCompileTo,
                                            warnsWith)


-- Only attribute pairs and elements reach the output. Functions, built-ins and other
-- compile-time values must be used up before then, or the graph does not compile.
serialisation :: Group
serialisation = Group "Serialisation: only values reach the output"
    [ -- A Number is written as JavaScript writes it: whole, or in plain decimal, between 10^-7 and 10^21
      ("a whole number is written whole",           compilesToJSON "(document g (:a 9999999 :b 10000000 :c 123456789 :d -42))"
                                                                   "{\"a\": 9999999, \"b\": 10000000, \"c\": 123456789, \"d\": -42}")
    , ("a fraction in plain decimal",               compilesToJSON "(document g (:a 0.00001 :b 0.1 :c (+ 0.1 0.2) :d 2.5))"
                                                                   "{\"a\": 0.00001, \"b\": 0.1, \"c\": 0.30000000000000004, \"d\": 2.5}")
    , ("and beyond that range in scientific notation", compilesToJSON "(document g (:a 1e21 :b 1e-8))"
                                                                   "{\"a\": 1.0e21, \"b\": 1.0e-8}")
    , ("a let-bound fn is not output",              compilesToJSON "(document g (let ($f (fn (x) x)) (:r (f 1))))"
                                                                   "{\"r\": 1}")
    , ("a fn as an attribute value is an error",    failsWith "an unexecuted function"
                                                              "(document g (:f (fn (x) x)))")
    , ("a fn bound by an attribute pair is still output", failsWith "an unexecuted function"
                                                              "(document g (:f (fn (x) x) :r (f 1)))")
    , ("a returned fn is an error",                 failsWith "an unexecuted function"
                                                              "(define/procedure mk () (fn (x) x)) (document g (:r (mk)))")
    , ("a named function as a value is an error",   failsWith "an unexecuted function"
                                                              "(define/procedure inc (x) (+ x 1)) (document g (:r inc))")
    , ("a fn inside an array is an error",          failsWith "a JSON value"
                                                              "(document g (:r (array (fn (x) x))))")
    , ("a fn as the graph is an error",             failsWith "a JSON value"
                                                              "(document g (fn (x) x))")
    , ("a built-in as a value is an error",         failsWith "a Built-in function"
                                                              "(document g (:r +))")
    , ("a fn spliced by inlay is an error",         failsWith "an unexecuted function"
                                                              "(define/procedure mk () (:f (fn (x) x))) (document g (:a 1 (inlay (mk))))")
    , ("let bindings are not output",               compilesToJSON "(document g (let ($hidden 1) (:shown hidden)))"
                                                                   "{\"shown\": 1}")

      -- A function where a value belongs says how it is called, with its parameters' names.
      -- This fixed the function hint of gap 10, which suggested (name! args).
    , ("in an array, how to call it",               failsWith "Perhaps call it: (inc x)."
                                                              "(define/procedure inc (x) (+ x 1)) (document g (:r (array inc)))")
    , ("as an attribute value",                     failsWith "A function is not a value: perhaps call it, (inc x)."
                                                              "(define/procedure inc (x) (+ x 1)) (document g (:r inc))")
    , ("as an argument with a property",            failsWith "This value is a function: perhaps call it, (mk x)."
                                                              "(define/procedure f #record (#record r) r) (define/procedure mk (x) (:a x)) (document g (:r (f mk)))")
    , ("a pattern parameter is shown as (...)",     failsWith "Perhaps call it: (f (...) c)."
                                                              "(define/procedure f ((?a ?b) c) a) (document g (:r (array f)))")
    , ("a built-in has no names to show",           failsWith "Perhaps call it with its arguments."
                                                              "(document g (:r (array len)))")
    , ("nor has an anonymous fn",                   failsWith "Perhaps call it with its arguments."
                                                              "(document g (:r (array (fn (x) x))))")
    , ("which has no call to show",                 failsWithout "(fn"
                                                                 "(document g (:r (fn (x) x)))")
    , ("the call is shown on a wrong arity",        failsWith "It is called (aat id label)."
                                                              "(define/procedure aat (id label) 1) (document g (:r (aat 1)))")
    , ("and to a function of none",                 failsWith "It is called (c)."
                                                              "(define/procedure c () 1) (document g (:r (c 1)))")

      -- The rest of gap 10's stale wording, fixed
    , ("get's missing key: the record has no such attribute", failsWith "The Record has no attribute 'b'.\""
                                                              "(document g (:p (:a 1) :r (get p b)))")
    , ("subtraction's hint names a Duration",       failsWith "or a Date minus a Duration"
                                                              "(document g (:r (- \"a\" 1)))")
    , ("a $ name may be a special form's",          compilesToJSON "(document g (let ($get 1) (:r get)))"
                                                                   "{\"r\": 1}")
    , ("and so may a key",                          compilesToJSON "(document g (:case 1))"
                                                                   "{\"case\": 1}")

      -- A removed form's name is free, as a key or a binding (gap 42, fixed); a call of one is
      -- reported with its replacement, unless a binding of the name shadows it, as in Scheme
    , ("a removed form's name is a key",            compilesToJSON "(document g (:a (:attr 1 :nth 2 :define 3 :defun 4)))"
                                                                   "{\"a\": {\"attr\": 1, \"nth\": 2, \"define\": 3, \"defun\": 4}}")
    , ("and a binding",                             compilesToJSON "(document g (let ($attr 1 $defun 2) (:r (+ attr defun))))"
                                                                   "{\"r\": 3}")
    , ("whose call is the binding's",               failsWith "'nth' is also a special form, which a binding of the same name shadows"
                                                              "(document g (let ($nth 1) (:r (nth 0 (1 2)))))")

      -- Pinned limits: the call is shown by the function's own name, not as it was qualified,
      -- and a function inside an array is reported at the graph, where its position is lost
    , ("the name is unqualified",                   failsWith "Perhaps call it: (aat id label)."
                                                              "(import std/base) (document g (:r (array base/aat)))")
    , ("an array's function is placed at the graph", importsFailAt "TYP0000" ("inline-test", 1, 19) ["The value at 'r[0]' is a function"] []
                                                                   "(import std/base) (document g (:r (array base/aat)))")
    ]


-- The behaviour of the special forms and built-ins, as documented in the specification
formsAndBuiltins :: Group
formsAndBuiltins = Group "Special forms and built-ins"
    [ ("insert an attribute pair into a record",    compilesToJSON "(document g (:r (insert (:a 2) 0 :k 1)))"
                                                                   "{\"r\": {\"k\": 1, \"a\": 2}}")
    , ("insert counts negative indexes from the end", compilesToJSON "(document g (:r (insert (1 2 3) -1 4)))"
                                                                   "{\"r\": [1, 2, 4, 3]}")
    , ("get indexes an Array",                      compilesToJSON "(document g (:r (get (1 2 3) -1) :s (get (1 2 3) 0)))"
                                                                   "{\"r\": 3, \"s\": 1}")
    , ("nth is replaced by get",                    failsWith "has been replaced by get"
                                                              "(document g (:r (nth 0 (1 2))))")
    , ("get walks a path of keys",                  compilesToJSON "(document g (:p (:q (:r 5)) :v (get p q r)))"
                                                                   "{\"p\": {\"q\": {\"r\": 5}}, \"v\": 5}")
    , ("a record pattern binds the rest",           compilesToJSON "(document g (:r (case (:a 1 :b 2 :c 3) ((:a ?first ?..others) (:x first :y others)) (otherwise 0))))"
                                                                   "{\"r\": {\"x\": 1, \"y\": {\"b\": 2, \"c\": 3}}}")
    , ("empty records and arrays",                  compilesToJSON "(document g (:r #record-empty :s #array-empty :t ()))"
                                                                   "{\"r\": {}, \"s\": [], \"t\": []}")
    , ("array makes an array headed by a variable",  compilesToJSON "(document g (let ($x 1) (:r (array x x))))"
                                                                   "{\"r\": [1, 1]}")
    , ("a form headed by a variable is a call",     failsWith "not a function"
                                                              "(document g (let ($x 1) (:r (x x))))")
    , ("a remote context",                          compilesToJSON "(document g (:context \"https://linked.art/ns/v1/linked-art.json\" :a 1))"
                                                                   "{\"@context\": \"https://linked.art/ns/v1/linked-art.json\", \"a\": 1}")
    , ("a local vocab directive",                   compilesToJSON "(document g (:context (:@vocab \"https://example.org/\") :a 1))"
                                                                   "{\"@context\": {\"@vocab\": \"https://example.org/\"}, \"a\": 1}")
    , ("thru extends days to their last second",    compilesToJSON "(document g (let ($start #date \"2020-01-01T00:00:00Z\") (:end (+ start (thru (days 1))))))"
                                                                   "{\"end\": \"2020-01-02T23:59:59Z\"}")
    , ("thru (years 0) ends the current year",      compilesToJSON "(document g (let ($start #date \"2020-03-01T00:00:00Z\") (:end (+ start (thru (years 0))))))"
                                                                   "{\"end\": \"2020-12-31T23:59:59Z\"}")
    , ("len counts an array's elements",            compilesToJSON "(document g (:a (len (1 (2 3) (:k 4))) :b (len ()) :c (len #array-empty)))"
                                                                   "{\"a\": 3, \"b\": 0, \"c\": 0}")
    , ("and a string's characters",                 compilesToJSON "(document g (:a (len \"Amsterdam\") :b (len \"Málaga\") :c (len \"\")))"
                                                                   "{\"a\": 9, \"b\": 6, \"c\": 0}")
    , ("len of a record is a type error",           failsWith "An Array or a String"
                                                              "(document g (:r (len (:a 1))))")
    , ("a URI is its string, so len counts it",     compilesToJSON "(document g (:r (len #uri \"https://a.org/x\")))"
                                                                   "{\"r\": 15}")
    , ("len takes one argument",                    failsWith "IncorrectArity"
                                                              "(document g (:r (len (1) (2))))")
    , ("len is an ordinary function value",         compilesToJSON "(document g (:r (map (\"ab\" (1 2 3)) len)))"
                                                                   "{\"r\": [2, 3]}")
    , ("a comparison takes two operands",           compilesToJSON "(document g (:a (eq 1 1) :b (neq 1 2)))"
                                                                   "{\"a\": true, \"b\": true}")
    , ("so three are an arity error",               failsWith "eq takes two operands, (eq a b). For more, fold them"
                                                              "(document g (:r (eq 1 1 1)))")
    , ("as for arithmetic",                         failsWith "+ takes two operands, (+ a b). For more, fold them: (fold (a b c) start +)."
                                                              "(document g (:r (+ 1 2 3)))")

      -- min and max, of one Array
    , ("min and max of an Array",                   compilesToJSON "(document g (let ($weights (3.72 3.36 4.19 3.12)) (:lower (min weights) :upper (max weights))))"
                                                                   "{\"lower\": 3.12, \"upper\": 4.19}")
    , ("several Numbers are written as one Array",  compilesToJSON "(document g (:a (min (3 1 2)) :b (max (3 1 2)) :d (max (7))))"
                                                                   "{\"a\": 1, \"b\": 3, \"d\": 7}")
    , ("not as the Numbers themselves",             failsWith "(min xs) takes one Array of Numbers"
                                                              "(document g (:r (min 3 1 2)))")
    , ("nor as one Number",                         failsWith "an Array of Numbers"
                                                              "(document g (:r (min 5)))")
    , ("there is no smallest of nothing",           failsWith "at least one Number"
                                                              "(document g (:r (min ())))")
    , ("min and max compare Numbers only",          failsWith "Numbers to compare"
                                                              "(document g (:r (max (1 \"2\"))))")
    , ("they are function values",                  compilesToJSON "(document g (:r (map ((1 2) (5 3)) max)))"
                                                                   "{\"r\": [2, 5]}")
    , ("#string asserts a string",                  failsWith "expectedRefinement = \"string\", actualRefinement = \"number\""
                                                              "(document g (:a #string 1))")
    , ("recursion is an error",                     failsWith "a recursive function call"
                                                              "(define/procedure f (n) (f n)) (document g (:r (f 1)))")
    , ("so is mutual recursion",                    failsWith "a recursive function call"
                                                              "(define/procedure f (n) (h n)) (define/procedure h (n) (f n)) (document g (:r (f 1)))")
    , ("and a function applied to itself",          failsWith "a recursive function call"
                                                              "(define/procedure w (x) (x x)) (document g (:r (w w)))")
    , ("a fn too, with no name to recurse by",      failsWith "a recursive function call"
                                                              "(document g (let ($w (fn (x) (x x))) (:r (w w))))")
    , ("and through map",                           failsWith "a recursive function call"
                                                              "(document g (let ($w (fn (x) (map (array x) (fn (y) (y y))))) (:r (w w))))")
    , ("Ouro has no brackets",                      failsWith "Ouro has no brackets: an array is written in parentheses"
                                                              "(document g (:a [1 2]))")
    , ("nor a closing one",                         failsWith "Ouro has no brackets: an array is written in parentheses"
                                                              "(document g (:a 1]))")

    -- (attr key value) was removed: an attribute pair is written ':key value', and a key is
    -- never computed. This also removed gap 5, a computed key dropped inside a record.
    , ("attr is removed",                           failsWith "The (attr key value) form has been removed"
                                                              "(document g (:r (attr \"k\" 1)))")
    , ("so is attr inside a record",                failsWith "The (attr key value) form has been removed"
                                                              "(document g (let ($k \"key\") (:a 1 (attr k 2))))")
    , ("and inside an array",                       failsWith "The (attr key value) form has been removed"
                                                              "(document g (:r ((attr \"k\" 1))))")
    , ("insert takes a written attribute pair",     compilesToJSON "(document g (:r (insert (:id \"1\" :type \"Person\") 2 :_label \"Proust\")))"
                                                                   "{\"r\": {\"id\": \"1\", \"type\": \"Person\", \"_label\": \"Proust\"}}")

    -- (and a b) is true when both operands are, and (or a b) when one is; (all array) and
    -- (any array) are the n-ary and and or. Each stops at the operand that decides it, and
    -- checks every operand it reaches as a #boolean
    , ("and is true when both operands are",        compilesToJSON "(document g (:a (and true true) :b (and true false)))"
                                                                   "{\"a\": true, \"b\": false}")
    , ("or is true when one operand is",            compilesToJSON "(document g (:a (or false true) :b (or false false)))"
                                                                   "{\"a\": true, \"b\": false}")
    , ("and takes two operands",                    failsWith "and takes two operands, (and a b), and was given 1."
                                                              "(document g (:r (and true)))")
    , ("with the n-ary form as the hint",           failsWith "(all (a b c)) is true when every condition is"
                                                              "(document g (:r (and true true true)))")
    , ("and so does or",                            failsWith "or takes two operands, (or a b), and was given 0."
                                                              "(document g (:r (or)))")
    , ("all is true when every element is",         compilesToJSON "(document g (:a (all (true true true)) :b (all (true false true)) :c (all ())))"
                                                                   "{\"a\": true, \"b\": false, \"c\": true}")
    , ("any is true when one element is",           compilesToJSON "(document g (:a (any (false true false)) :b (any (false false)) :c (any ())))"
                                                                   "{\"a\": true, \"b\": false, \"c\": false}")
    , ("all written in place stops at the first false", compilesToJSON "(document g (let ($r (:x 1)) (:r (all ((has r year) (gt (get r year) 1600))))))"
                                                                       "{\"r\": false}")
    , ("any written in place stops at the first true", compilesToJSON "(document g (:r (any (true (get () 5)))))"
                                                                      "{\"r\": true}")
    , ("a list is written in place",                compilesToJSON "(document g (let ($a false) (:r (all (array a (get () 5))))))"
                                                                   "{\"r\": false}")
    , ("an Array given another way is read whole",  compilesToJSON "(document g (let ($conds (true false)) (:a (all conds) :b (any conds) :c (any (map (1 2) (gt ? 1))))))"
                                                                   "{\"a\": false, \"b\": true, \"c\": true}")
    , ("its elements must be Booleans",             failsWith "expectedRefinement = \"boolean\", actualRefinement = \"number\""
                                                              "(document g (let ($conds (true 1)) (:r (all conds))))")
    , ("and so must a written element",             failsWith "expectedRefinement = \"boolean\", actualRefinement = \"number\""
                                                              "(document g (:r (all (1))))")
    , ("all takes one Array",                       failsWith "all is written (all array): one Array of Booleans."
                                                              "(document g (:r (all true false)))")
    , ("\8896 and \8897 are all and any",           compilesToJSON "(document g (:a (\8896 (true true)) :b (\8897 (false true))))"
                                                                   "{\"a\": true, \"b\": true}")
    , ("and stops at the first false",              compilesToJSON "(document g (:r (and false (get () 5)) :s (and false 1)))"
                                                                   "{\"r\": false, \"s\": false}")
    , ("or stops at the first true",                compilesToJSON "(document g (:r (or true (get () 5))))"
                                                                   "{\"r\": true}")
    , ("an operand must be a #boolean",             failsWith "expectedRefinement = \"boolean\", actualRefinement = \"number\""
                                                              "(document g (:r (and true 1)))")
    , ("or checks its operands too",                failsWith "expectedRefinement = \"boolean\", actualRefinement = \"number\""
                                                              "(document g (:r (or 1 true)))")
    , ("and and or combine comparisons",            compilesToJSON "(document g (:r (and (gt 2 1) (or (eq \"a\" \"b\") (eq \"a\" \"a\")))))"
                                                                   "{\"r\": true}")
    , ("and and or take holes",                     compilesToJSON "(document g (:r (let ($f (and ? true)) (f false)) :s (map (true false) (or ? false))))"
                                                                   "{\"r\": false, \"s\": [true, false]}")
    , ("and and or are threading steps",            compilesToJSON "(document g (:r (-> true (and false)) :s (-> false (or true))))"
                                                                   "{\"r\": false, \"s\": true}")
    , ("and cannot name a function",                failsWith "'and' is read by its syntax before any name is bound"
                                                              "(define/procedure and (x) x) (document g (:r (and 1)))")
    , ("nor can or",                                failsWith "'or' is read by its syntax before any name is bound"
                                                              "(define/procedure or (x) x) (document g (:r 1))")
    , ("and and or guard an index",                 compilesToJSON "(document g (let ($tags ()) (:featured (and (gt (len tags) 0) (eq (get tags 0) \"featured\")) :listed (or (eq (len tags) 0) (eq (get tags 0) \"listed\")))))"
                                                                   "{\"featured\": false, \"listed\": true}")
    , ("a chain of comparisons is all over its pairs", compilesToJSON "(document g (:r (all ((lt 2 1) (lt 1 (get () 5))))))"
                                                                       "{\"r\": false}")

      -- (fold array init f): a left fold, f called on the running value and each element
    , ("fold reduces an Array from a start",        compilesToJSON "(document g (:a (fold (1 2 3) 0 +) :b (fold (2 3 4) 1 *) :c (fold () 7 +)))"
                                                                   "{\"a\": 6, \"b\": 24, \"c\": 7}")
    , ("it goes from the left",                     compilesToJSON "(document g (:r (fold (1 2 3) 10 -)))"
                                                                   "{\"r\": 4}")
    , ("its function takes the running value first", compilesToJSON "(document g (:r (fold (\"a\" \"b\") \"\" (fn (s x) (+ s x)))))"
                                                                     "{\"r\": \"ab\"}")
    , ("it builds a record",                        compilesToJSON "(document g (:r (fold (1 2) (:type \"Set\") (fn (set n) (adjoin set :member n)))))"
                                                                   "{\"r\": {\"type\": \"Set\", \"member\": [1, 2]}}")
    , ("it is a threading step",                    compilesToJSON "(document g (:r (-> (1 2 3) (map (* ? 2)) (fold 0 +))))"
                                                                   "{\"r\": 12}")
    , ("it takes a hole",                           compilesToJSON "(document g (let ($sum (fold ? 0 +)) (:r (sum (4 5)))))"
                                                                   "{\"r\": 9}")
    , ("its function takes two arguments",          failsWith "IncorrectArity"
                                                              "(document g (:r (fold (1 2) 0 (fn (x) x))))")
    , ("fold needs a function",                     failsWith "a function to fold with"
                                                              "(document g (:r (fold (1 2) 0 1)))")
    , ("fold needs an Array",                       failsWith "an Array to fold"
                                                              "(document g (:r (fold 5 0 +)))")
    , ("fold is written with three",                failsWith "fold is written (fold array init f)"
                                                              "(document g (:r (fold (1 2) +)))")
    , ("an error stops it",                         failsWith "Index"
                                                              "(document g (:r (fold (1 2) 0 (fn (s x) (+ s (get () 5))))))")
    ]


-- Functions are not curried: a call supplies exactly the declared arguments. A function
-- can return a fn, which is called by binding it with let; ((f ...) ...) is an array.
returnedFunctions :: Group
returnedFunctions = Group "Functions: arity and returned functions"
    [ ("functions are not curried",                 failsWith "IncorrectArity"
                                                              "(define/procedure add (a b) (+ a b)) (document g (:r (add 1)))")
    , ("a returned fn is called through let",       compilesToJSON "(define/procedure greet () (fn () \"hello\")) (document g (let ($f (greet)) (:r (f))))"
                                                                   "{\"r\": \"hello\"}")
    , ("currying by hand, through let",             compilesToJSON "(define/procedure adder (n) (fn (x) (+ n x))) (document g (let ($inc (adder 1)) (:r (inc 2))))"
                                                                   "{\"r\": 3}")
    , ("a form headed by a call is an array",       compilesToJSON "(define/procedure one () 1) (document g (:r ((one))))"
                                                                   "{\"r\": [1]}")
    , ("so ((f x) y) does not call the result",     failsWith "an unexecuted function"
                                                              "(define/procedure adder (n) (fn (x) (+ n x))) (document g (:r ((adder 1) 2)))")
    ]


-- A key appears once in a record. Inlays are layers: a later inlay overrides an earlier
-- one, and an attribute pair written in the record overrides both, whatever the order. insert and
-- replace-at cannot duplicate a key.
recordKeys :: Group
recordKeys = Group "Records: each key once"
    [ ("a key written twice is an error",           failsWith "is written twice"
                                                              "(document g (:a 1 :a 2))")
    , ("also in a nested record",                   failsWith "is written twice"
                                                              "(document g (:x (:a 1 :a 2)))")
    , ("two contexts are an error",                 failsWith "The key 'context' is written twice"
                                                              "(document g (:context \"https://x.org/c.json\" :context \"https://y.org/c.json\" :a 1))")
    , ("a written attribute pair wins over an inlaid one", compilesToJSON (base <> "(document g (:type \"Mine\" (inlay (base))))")
                                                                   "{\"type\": \"Mine\", \"id\": \"1\"}")
    , ("whatever the order",                        compilesToJSON (base <> "(document g ((inlay (base)) :type \"Mine\"))")
                                                                   "{\"id\": \"1\", \"type\": \"Mine\"}")
    , ("a lookup sees the written attribute pair",  compilesToJSON (base <> "(document g (:type \"Mine\" :r type (inlay (base))))")
                                                                   "{\"type\": \"Mine\", \"r\": \"Mine\", \"id\": \"1\"}")
    , ("a later inlay overrides an earlier one",    compilesToJSON "(define/procedure a () (:k 1 :j 1)) (define/procedure b () (:k 2)) (document g (:x 0 (inlay (a)) (inlay (b))))"
                                                                   "{\"x\": 0, \"j\": 1, \"k\": 2}")
    , ("a written attribute pair settles two inlays", compilesToJSON "(define/procedure a () (:k 1)) (define/procedure b () (:k 2)) (document g (:k 3 (inlay (a)) (inlay (b))))"
                                                                   "{\"k\": 3}")
    , ("insert cannot add an existing key",         failsWith "already has an attribute"
                                                              "(document g (:r (insert (:a 1) 0 :a 9)))")
    , ("replace-at can keep the attribute pair's own key", compilesToJSON "(document g (:r (replace-at (:a 1 :b 2) 0 :a 9)))"
                                                                   "{\"r\": {\"a\": 9, \"b\": 2}}")
    , ("replace-at cannot duplicate another key",   failsWith "already has an attribute"
                                                              "(document g (:r (replace-at (:a 1 :b 2) 0 :b 9)))")

      -- A key is its value's: a key with none is an error, and a compact IRI is one key
    , ("a key followed by a key has no value",      importsFailAt "SYN0011" ("inline-test", 1, 14) ["valuelessKey = \"a\"", "followedBy = \"the key ':b'\"", "The key ':a' has no value"] []
                                                                  "(document g (:a :b 1 :y 2))")
    , ("so does a key at the end of its form",      importsFailAt "SYN0011" ("inline-test", 1, 19) ["valuelessKey = \"b\"", "the end of its form"] []
                                                                  "(document g (:a 1 :b))")
    , ("a key alone in its form",                   importsFailAt "SYN0011" ("inline-test", 1, 14) ["valuelessKey = \"a\"", "the end of its form"] []
                                                                  "(document g (:a))")
    , ("a compact IRI is one key",                  compilesToJSON "(document g (:crm:P2_has_type \"x\" :y 2))"
                                                                   "{\"crm:P2_has_type\": \"x\", \"y\": 2}")
    , ("a compact IRI key is a binding too",        compilesToJSON "(document g (:crm:P2_has_type \"x\" :v crm:P2_has_type))"
                                                                   "{\"crm:P2_has_type\": \"x\", \"v\": \"x\"}")
    , ("a path can name a compact IRI key",         compilesToJSON "(document g (let ($r (:crm:P2_has_type \"x\")) (:v (get r crm:P2_has_type))))"
                                                                   "{\"v\": \"x\"}")
    , ("a keyword key still starts with @",         compilesToJSON "(document g (:context (:@vocab \"https://example.org/\") :a 1))"
                                                                   "{\"@context\": {\"@vocab\": \"https://example.org/\"}, \"a\": 1}")
    ]

    where
    base = "(define/procedure base () (:type \"Base\" :id \"1\")) "


-- The type rules as they are: values carry their types, built-ins check them, and nothing
-- is converted implicitly. Arrays are like JSON arrays: their elements can be of any type.
-- Several rules here are under review (equality across types, ordering, extending URIs);
-- a change to one flips its test.
types :: Group
types = Group "Types: the rules as they are"
    [ -- The built-in properties, by their full names
      ("every built-in property, by its full name", compilesToJSON "(define/procedure twice #number (#function f #number n) (f (f n))) (document g (:s #string \"a\" :n (twice (fn (#number x) (* x 2)) 1) :b #boolean true :r #record (:a 1) :a #array (1) :e #array-empty :o #record-empty))"
                                                                   "{\"s\": \"a\", \"n\": 4, \"b\": true, \"r\": {\"a\": 1}, \"a\": [1], \"e\": [], \"o\": {}}")
    , ("#str is not a refinement: #string is",      failsWith "Unknown refinement: str"       "(document g (:r #str \"a\"))")
    , ("#num is not a refinement: #number is",      failsWith "Unknown refinement: num"       "(document g (:r #num 1))")
    , ("#bool is not a refinement: #boolean is",    failsWith "Unknown refinement: bool"      "(document g (:r #bool true))")
    , ("#rec is not a refinement: #record is",      failsWith "Unknown refinement: rec"       "(document g (:r #rec (:a 1)))")
    , ("#arr is not a refinement: #array is",       failsWith "Unknown refinement: arr"       "(document g (:r #arr (1)))")
    , ("#fn is not a refinement: #function is",     failsWith "Unknown refinement: fn"        "(document g (:r #fn 1))")
      -- Ouro has no null: absence is a key left out, or a statement of why
    , ("Ouro has no null",                          failsWith "Ouro has no null: leave the attribute out, or state why the value is absent"
                                                              "(document g (:r null))")
    , ("it is reported where it is written",        failsWith "line 1, col 17"
                                                              "(document g (:r null))")
    , ("in an array too",                           failsWith "Ouro has no null"
                                                              "(document g (:r (1 null)))")
    , ("and null is no name",                       failsWith "Ouro has no null"
                                                              "(define/procedure null (x) x) (document g (:r 1))")
    , ("#rec-empty is not a value: #record-empty is", failsWith "Unknown refinement: rec-empty" "(document g (:r #rec-empty))")
    , ("#arr-empty is not a value: #array-empty is", failsWith "Unknown refinement: arr-empty" "(document g (:r #arr-empty))")
      -- A property defined in Ouro is read by its whole name, qualified or not; one the module does not define is unknown, at its tag
    , ("a refinement the module does not define is unknown", importsFailAt "SYN0000" ("inline-test", 1, 17) ["Unknown refinement: titled"] []
                                                                     "(document g (:r #titled (:_label \"A\")))")
    , ("as is an imported one, not yet read", importsFailAt "SYN0000" ("inline-test", 1, 17) ["Unknown refinement: people/titled"] []
                                                                     "(document g (:r #people/titled (:_label \"A\")))")
    , ("a name that does not begin with a letter is no refinement", failsWith "Unknown refinement: #1st" "(document g (:r #1st 1))")
      -- Arrays
    , ("an array may mix types",                    compilesToJSON "(document g (:r (1 \"a\" true)))"
                                                                   "{\"r\": [1, \"a\", true]}")
    , ("+ joins any two arrays",                    compilesToJSON "(document g (:r (+ (1 \"a\") (2 3)) :s (+ (1 2) (\"a\"))))"
                                                                   "{\"r\": [1, \"a\", 2, 3], \"s\": [1, 2, \"a\"]}")
    , ("+ joins empty arrays",                      compilesToJSON "(document g (:r (+ () (1)) :s (+ () ()) :t (+ #array-empty (1))))"
                                                                   "{\"r\": [1], \"s\": [], \"t\": [1]}")
    , ("insert takes any value into an array",      compilesToJSON "(document g (:r (insert (1 2) 0 \"x\") :s (insert #array-empty 0 1)))"
                                                                   "{\"r\": [\"x\", 1, 2], \"s\": [1]}")
    , ("insert's index is a whole number",          importsFailAt "SYN0003" ("inline-test", 1, 31) ["a whole number to index an Array with", "1.5"] []
                                                                   "(document g (:r (insert (1 2) 1.5 \"x\")))")
    , ("and so is replace-at's",                    failsWith "a whole number to index an Array with"
                                                                   "(document g (:r (replace-at (1 2) 0.5 \"x\")))")
    , ("a whole number written with a point is one", compilesToJSON "(document g (:r (insert (1 2) 1.0 \"x\")))"
                                                                   "{\"r\": [1, \"x\", 2]}")
    , ("an attribute pair cannot be inserted into an array", failsWith "a datatype for insertion matching"
                                                              "(document g (:r (insert (1 2) 0 :k 1)))")
    , ("a mixed array is described as mixed",       failsWith "an Array of mixed values"
                                                              "(document g (:r (+ 1 (1 \"a\"))))")

      -- Equality
    , ("eq across types is false",                  compilesToJSON "(document g (:r (eq 1 \"1\") :s (eq true 1)))"
                                                                   "{\"r\": false, \"s\": false}")
    , ("a URI is equal to its string",              compilesToJSON "(document g (:r (eq #uri \"https://a.org\" \"https://a.org\")))"
                                                                   "{\"r\": true}")
    , ("a case literal of another type never matches", compilesToJSON "(document g (:r (case 5 (\"5\" \"string\") (otherwise \"number\"))))"
                                                                   "{\"r\": \"number\"}")
    , ("records are equal whatever the order of their attribute pairs", compilesToJSON "(document g (:r (eq (:a 1 :b 2) (:b 2 :a 1))))"
                                                                   "{\"r\": true}")

      -- Ordering and arithmetic
    , ("dates cannot be ordered: they are strings", failsWith "a String and a String do not support the requested operation"
                                                              ("(document g (:r (gt " <> date <> " " <> date <> ")))"))
    , ("nor subtracted",                            failsWith "a String and a String do not support the requested operation"
                                                              ("(document g (:r (- " <> date <> " " <> date <> ")))"))
    , ("strings cannot be ordered",                 failsWith "a String lt a String"
                                                              "(document g (:r (lt \"a\" \"b\")))")
    , ("a URI joins as a string does",              compilesToJSON "(document g (:r (+ #uri \"https://a.org/\" \"x\")))"
                                                                   "{\"r\": \"https://a.org/x\"}")
    , ("durations join as strings, as + joins",     compilesToJSON "(document g (:r (+ (days 1) (days 2))))"
                                                                   "{\"r\": \"P1DP2D\"}")

      -- Assertions check, and only #uri and #date convert a String
    , ("#number does not convert a string",         failsWith "expectedRefinement = \"number\", actualRefinement = \"string\""
                                                              "(document g (:r #number \"5\"))")
    , ("#uri does not accept a date",               failsWith "expectedRefinement = \"uri\", actualRefinement = \"a string"
                                                              ("(document g (:r #uri " <> date <> "))"))

      -- A failed property names both properties, and says why when the value's own is not enough
    , ("a URI needs a scheme",                      failsWith "actualRefinement = \"a string without a URI scheme\""
                                                              "(document g (:r #uri \"nope\"))")
    , ("a date needs ISO-8601 form",                failsWith "actualRefinement = \"a string that is not a date\""
                                                              "(document g (:r #date \"1642\"))")

      -- A URI is written as it was written: every slash of its path is kept
    , ("#uri keeps a trailing slash",               compilesToJSON "(document g (:u #uri \"https://a.org/x/\"))"
                                                                   "{\"u\": \"https://a.org/x/\"}")
    , ("and an empty root path",                    compilesToJSON "(document g (:u #uri \"https://a.org/\"))"
                                                                   "{\"u\": \"https://a.org/\"}")
    , ("and an empty path segment",                 compilesToJSON "(document g (:u #uri \"https://a.org/x//y\"))"
                                                                   "{\"u\": \"https://a.org/x//y\"}")
    , ("and a leading one",                         compilesToJSON "(document g (:u #uri \"https://a.org//x\"))"
                                                                   "{\"u\": \"https://a.org//x\"}")
    , ("and so does a remote context",              compilesToJSON "(document g (:context #uri \"https://a.org/\" :a 1))"
                                                                   "{\"@context\": \"https://a.org/\", \"a\": 1}")
    , ("a string keeps a doubled slash",            compilesToJSON "(document g (:u \"https://a.org/x//y\"))"
                                                                   "{\"u\": \"https://a.org/x//y\"}")
    ]

    where
    date = "#date \"2020-01-01T00:00:00Z\""


-- Operators take two operands, as every built-in takes a fixed number of arguments: nothing
-- is unrolled, and many values are an Array, folded. A case guard compares the case's
-- target with its one operand. A count that is wrong as written is an arity error.
operators :: Group
operators = Group "Operators: operands and desugaring"
    [ ("nested + under a built-in property",        compilesToJSON "(document g (:u #uri (+ \"https://a.org/\" (+ \"b/\" \"c\"))))"
                                                                   "{\"u\": \"https://a.org/b/c\"}")
    , ("thru under a built-in property",            compilesToJSON "(document g (:d #date (+ #date \"2020-01-01T00:00:00Z\" (thru (days 1)))))"
                                                                   "{\"d\": \"2020-01-02T23:59:59Z\"}")
    , ("thru shifts back with -",                   compilesToJSON "(document g (:d (- #date \"2020-01-03T00:00:00Z\" (thru (days 1)))))"
                                                                   "{\"d\": \"2020-01-02T00:00:00Z\"}")
    , ("all and fold under a property",             compilesToJSON "(document g (:r #boolean (all ((lt 1 2) (lt 2 3))) :s #number (fold (2 3 4) 1 *)))"
                                                                   "{\"r\": true, \"s\": 24}")
    , ("a typed function body",                     compilesToJSON "(define/procedure f () #uri (+ \"https://a.org/\" (+ \"b/\" \"c\"))) (document g (:r (f)))"
                                                                   "{\"r\": \"https://a.org/b/c\"}")
    , ("a guard compares the target",               compilesToJSON "(define/procedure size (n) (case n ((gt 100) \"large\") ((lt 10) \"small\") (otherwise \"medium\"))) (document g (:a (size 150) :b (size 5) :c (size 50)))"
                                                                   "{\"a\": \"large\", \"b\": \"small\", \"c\": \"medium\"}")
    , ("a guard takes one operand",                 failsWith "lt takes two operands, (lt a b)"
                                                              "(document g (:r (case 5 ((lt 10 20) \"a\") (otherwise \"b\"))))")
    , ("two bounds are a when over all",            compilesToJSON "(document g (:r (case 5 ((when (all ((gt 5 1) (lt 5 10)))) \"a\") (otherwise \"b\")) :s (case 15 ((when (all ((gt 15 1) (lt 15 10)))) \"a\") (otherwise \"b\"))))"
                                                                   "{\"r\": \"a\", \"s\": \"b\"}")
    , ("a guard's operand is desugared",            compilesToJSON "(document g (:r (case 5 ((lt (fold (1 2 3) 0 +)) \"a\") (otherwise \"b\"))))"
                                                                   "{\"r\": \"a\"}")
    , ("an operator needs two operands",            failsWith "IncorrectArity"
                                                              "(document g (:r (+ 1)))")
    , ("a comparison needs two operands",           failsWith "IncorrectArity"
                                                              "(document g (:r (lt 1)))")
    , ("neq compares exactly two values",           failsWith "IncorrectArity"
                                                              "(document g (:r (neq 1 2 3)))")
    , ("so a neq guard takes one operand",          failsWith "IncorrectArity"
                                                              "(document g (:r (case 5 ((neq 1 2) \"a\") (otherwise \"b\"))))")
      -- ⊢ asserts and ∈ asks, of a value and a refinement written by its name, never evaluated
    , ("⊢ asserts: the value passes on",            compilesToJSON "(document g (:r (⊢ 1 number) :s <\"https://a.org/x\" ⊢ uri>))"
                                                                   "{\"r\": 1, \"s\": \"https://a.org/x\"}")
    , ("its ASCII spelling is |-",                  compilesToJSON "(document g (:r (|- 1 number)))"
                                                                   "{\"r\": 1}")
    , ("|- infix, and in a signature",              compilesToJSON "(define/procedure twice |- number (<x |- number>) (+ x x)) (document g (:a <1 |- number> :b (twice 2)))"
                                                                   "{\"a\": 1, \"b\": 4}")
    , ("a value without it is a type error",        failsWith "expectedRefinement = \"number\", actualRefinement = \"string\""
                                                              "(document g (:r <\"a\" ⊢ number>))")
    , ("∈ asks",                                    compilesToJSON "(document g (:r (∈ 1 number) :s <\"a\" ∈ number>))"
                                                                   "{\"r\": true, \"s\": false}")
    , ("a refinement of the module",                compilesToJSON (titled <> "(document g (:r <(:_label \"A\") ⊢ titled> :s <(:x 1) ∈ titled>))")
                                                                   "{\"r\": {\"_label\": \"A\"}, \"s\": false}")
    , ("which a value without it fails",            failsWith "expectedRefinement = \"titled\""
                                                              (titled <> "(document g (:r <(:x 1) ⊢ titled>))"))
    , ("∈ heads a case branch",                     compilesToJSON "(document g (:r (case 1 ((∈ string) \"text\") (otherwise \"other\"))))"
                                                                   "{\"r\": \"other\"}")
    , ("a hole makes ∈ a function",                 compilesToJSON "(document g (:r (filter (1 \"a\") <? ∈ string>)))"
                                                                   "{\"r\": [\"a\"]}")
    , ("⊢ chains, asserting each",                  compilesToJSON (titled <> "(document g (:r <(:_label \"A\") ⊢ record ⊢ titled>))")
                                                                   "{\"r\": {\"_label\": \"A\"}}")
    , ("the refinement is a name, not a value",     compilesToJSON "(document g (let ($string 1) (:r <\"a\" ⊢ string>)))"
                                                                   "{\"r\": \"a\"}")
    , ("a binding of satisfies does not capture ∈", compilesToJSON "(document g (let ($satisfies (fn (a b) \"mine\")) (:r <1 ∈ number>)))"
                                                                   "{\"r\": true}")
    , ("one that is not a name is an error",        failsWith " value refinement), or <value"
                                                              "(document g (:r (⊢ 1 (array))))")
    , ("as is ∈ of one operand outside a case",     failsWith "as the head of a case branch"
                                                              "(document g (:r (∈ 1)))")
    , ("no function is named in",                   failsWith "'in' is read by its syntax"
                                                              "(define/procedure in (a b) a) (document g (:r 1))")

      -- ∪ ∩ ∖ of types, prefix or infix, wherever a type is written; the value never changes
    , ("⊢ a union, through either side",            compilesToJSON "(document g (:a <1 ⊢ (∪ string number)> :b <\"x\" ⊢ <string ∪ number>>))"
                                                                   "{\"a\": 1, \"b\": \"x\"}")
    , ("outside both is a type error",              failsWith "expectedRefinement = \"(\\8746 string number)\""
                                                              "(document g (:r <true ⊢ (∪ string number)>))")
    , ("an intersection holds both",                compilesToJSON "(document g (:r <\"https://a.org\" ⊢ (∩ string uri)>))"
                                                                   "{\"r\": \"https://a.org\"}")
    , ("and refuses a value in one only",           failsWith "RefinementFailed"
                                                              "(document g (:r <\"x\" ⊢ (∩ string uri)>))")
    , ("a difference refuses what it takes away",   compilesToJSON "(document g (:a <\"x\" ∈ (∖ string uri)> :b <\"https://a.org\" ∈ (∖ string uri)>))"
                                                                   "{\"a\": true, \"b\": false}")
    , ("a chain, and nesting",                      compilesToJSON "(document g (:a <1 ∈ <string ∪ number ∪ boolean>> :b <1 ∈ <(∪ string number) ∩ number>>))"
                                                                   "{\"a\": true, \"b\": true}")
    , ("in a binder and a return type",             compilesToJSON "(define/procedure f ⊢ (∪ string number) (<x ⊢ (∪ string number)>) x) (document g (:r (f 1)))"
                                                                   "{\"r\": 1}")
    , ("as a case head",                            compilesToJSON "(document g (:r (case 1 ((∈ (∪ uri date)) \"when\") (otherwise \"other\"))))"
                                                                   "{\"r\": \"other\"}")
    , ("a union takes two types",                   failsWith "A type is a refinement's name"
                                                              "(define/procedure f (<x ⊢ (∪ string number boolean)>) x) (document g (:r 1))")
    , ("each of them a type",                       failsWith "A type is a refinement's name"
                                                              "(define/procedure f (<x ⊢ (∪ string 1)>) x) (document g (:r 1))")
    , ("an unknown refinement inside is reported",  failsWith "Unknown refinement: titeld"
                                                              "(document g (:r <1 ⊢ (∪ number titeld)>))")
    , ("an error inside a side stays that error",   failsWith "expectedRefinement = \"string\""
                                                              (titled <> "(define/refinement broken (<x ⊢ number>) (eq <x ⊢ string> \"s\")) (document g (:r <1 ⊢ (∪ broken number)>))"))

      -- (array p): an array whose every element is in p
    , ("every element in p",                        compilesToJSON "(document g (:a <(1 2 3) ⊢ (array number)> :b <(make array) ⊢ (array string)>))"
                                                                   "{\"a\": [1, 2, 3], \"b\": []}")
    , ("a value that is no array is refused",       failsWith "expectedRefinement = \"(array number)\""
                                                              "(document g (:r <\"x\" ⊢ (array number)>))")
    , ("an element outside is named",               failsWith "an array whose element 3 is string"
                                                              "(document g (:r <(1 2 \"x\") ⊢ (array number)>))")
    , ("nested, and over a union",                  compilesToJSON "(document g (:a <((1) (2 3)) ⊢ (array (array number))> :b <(1 \"a\") ∈ (array (∪ string number))>))"
                                                                   "{\"a\": [[1], [2, 3]], \"b\": true}")
    , ("in a binder",                               compilesToJSON "(define/procedure total ⊢ number (<xs ⊢ (array number)>) (fold xs 0 +)) (document g (:r (total (1 2 3))))"
                                                                   "{\"r\": 6}")
    , ("as a case head",                            compilesToJSON "(document g (:r (case (1 2) ((∈ (array string)) \"words\") (otherwise \"other\"))))"
                                                                   "{\"r\": \"other\"}")
    , ("(array) is no type",                        failsWith "A type is a refinement's name"
                                                              "(define/procedure f (<x ⊢ (array)>) x) (document g (:r 1))")
    , ("nor is (array a b)",                        failsWith "A type is a refinement's name"
                                                              "(define/procedure f (<x ⊢ (array string number)>) x) (document g (:r 1))")
    , ("an error inside an element's check stays",  failsWith "expectedRefinement = \"string\""
                                                              (titled <> "(define/refinement broken (<x ⊢ number>) (eq <x ⊢ string> \"s\")) (document g (:r <(1) ⊢ (array broken)>))"))
    , ("a refinement over its own parts descends",  compilesToJSON "(define/refinement tree (<n ⊢ record>) (doc \"Its parts are trees.\") <(get-or n () part) ∈ (array tree)>) (document g (:r <(:part ((:part ()))) ∈ tree>))"
                                                                   "{\"r\": true}")

      -- (record :k p …): a record with at least these keys, each value in its type
    , ("a record with the keys",                    compilesToJSON "(document g (:r <(:label \"a\" :part ()) ⊢ (record :label string :part array)>))"
                                                                   "{\"r\": {\"label\": \"a\", \"part\": []}}")
    , ("other keys are left alone",                 compilesToJSON "(document g (:r <(:label \"a\" :note 1) ⊢ (record :label string)>))"
                                                                   "{\"r\": {\"label\": \"a\", \"note\": 1}}")
    , ("a value that is no record is refused",      failsWith "expectedRefinement = \"(record :label string)\""
                                                              "(document g (:r <\"x\" ⊢ (record :label string)>))")
    , ("a missing key is named",                    failsWith "a record without label"
                                                              "(document g (:r <(:note 1) ⊢ (record :label string)>))")
    , ("a value outside its key's type is named",   failsWith "a record whose label is number"
                                                              "(document g (:r <(:label 1) ⊢ (record :label string)>))")
    , ("the keys are asked in the order written",   failsWith "a record without label"
                                                              "(document g (:r <(:part 1) ⊢ (record :label string :part array)>))")
    , ("(record) is every record",                  compilesToJSON "(document g (:a <(make record) ⊢ (record)> :b <(:k 1) ∈ (record)> :c <1 ∈ (record)>))"
                                                                   "{\"a\": {}, \"b\": true, \"c\": false}")
    , ("nested, and in a union",                    compilesToJSON "(document g (:a <(:t (:b \"x\")) ∈ (record :t (record :b string))> :b <(:n 1) ∈ (∪ (record :s string) (record :n number))>))"
                                                                   "{\"a\": true, \"b\": true}")
    , ("in a binder",                               compilesToJSON "(define/procedure label-of ⊢ string (<r ⊢ (record :label string)>) (get r label)) (document g (:r (label-of (:label \"a\"))))"
                                                                   "{\"r\": \"a\"}")
    , ("as a case head",                            compilesToJSON "(document g (:r (case (:part ()) ((∈ (record :part array)) \"node\") (otherwise \"leaf\"))))"
                                                                   "{\"r\": \"node\"}")
    , ("an error inside a key's check stays",       failsWith "expectedRefinement = \"string\""
                                                              (titled <> "(define/refinement broken (<x ⊢ number>) (eq <x ⊢ string> \"s\")) (document g (:r <(:k 1) ⊢ (record :k broken)>))"))
    , ("a refinement through its own keys descends", compilesToJSON "(define/refinement tree (<n ⊢ record>) (doc \"A leaf, or parts that are trees.\") <n ∈ (∪ (record :leaf string) (record :part (array tree)))>) (document g (:r <(:part ((:leaf \"a\"))) ∈ tree>))"
                                                                   "{\"r\": true}")
    , ("a record type is read",                     compilesToJSON "(define/procedure f (<r ⊢ (record :label string :part (array record))>) r) (document g (:ok 1))"
                                                                   "{\"ok\": 1}")
    , ("(record) is read too",                      compilesToJSON "(define/procedure f (<r ⊢ (record)>) r) (document g (:ok 1))"
                                                                   "{\"ok\": 1}")
    , ("a key written twice is no type",            failsWith "A type is a refinement's name"
                                                              "(define/procedure f (<r ⊢ (record :k string :k number)>) r) (document g (:ok 1))")
    , ("reported at the second pair",               failsWith "line 1, col 45"
                                                              "(define/procedure f (<r ⊢ (record :k string :k number)>) r) (document g (:ok 1))")
    , ("a key's type is a type, not a value",       failsWith "A type is a refinement's name"
                                                              "(define/procedure f (<r ⊢ (record :k 1)>) r) (document g (:ok 1))")
    , ("and every item is a pair",                  failsWith "A type is a refinement's name"
                                                              "(define/procedure f (<r ⊢ (record :k string number)>) r) (document g (:ok 1))")

      -- (record) is every record, so it is a warning (WRN0008) that suggests record
    , ("(record) in a binder warns",                warnsWith "(record) is the type record"
                                                              "(define/procedure f (<r ⊢ (record)>) r) (document g (:ok 1))")
    , ("and as a return type",                      warnsWith "(record) is the type record"
                                                              "(define/procedure f ⊢ (record) () (make record)) (document g (:ok 1))")
    , ("and in an assertion",                       warnsWith "(record) is the type record"
                                                              "(document g (:r <(make record) ⊢ (record)>))")
    , ("and in a fn's binder",                      warnsWith "(record) is the type record"
                                                              "(document g (:r (map ((:k 1)) (fn (<r ⊢ (record)>) r))))")
    , ("and nested in a type",                      warnsWith "(record) is the type record"
                                                              "(define/procedure f (<xs ⊢ (array (record))>) xs) (document g (:ok 1))")
    , ("a record type with keys does not",          compilesWithoutWarnings "(define/procedure f (<r ⊢ (record :k string)>) r) (document g (:ok 1))")

      -- prose: bar lines, a bar and a space or a bar alone at its line's end beginning each line of
      -- raw text, folded into one string where it is written
    , ("prose folds its lines into a string",       compilesToJSON "(document g (:r (prose | a\n                    | b\n                    )))"
                                                                   "{\"r\": \"a b\"}")
    , ("a bar alone begins a paragraph",            compilesToJSON "(document g (:r (prose | a\n | b\n |\n | c\n )))"
                                                                   "{\"r\": \"a b\\n\\nc\"}")
    , ("each bare bar is a break, at the ends too",  compilesToJSON "(document g (:r (prose |\n | a\n |\n |\n | b\n |\n )))"
                                                                   "{\"r\": \"\\n\\na\\n\\n\\n\\nb\\n\\n\"}")
    , ("each line is trimmed",                      compilesToJSON "(document g (:r (prose |   x  \n | y\n )))"
                                                                   "{\"r\": \"x y\"}")
    , ("a bar line is raw text",                    compilesToJSON "(document g (:r (prose | a ; \"b\" (c \\d\n )))"
                                                                   "{\"r\": \"a ; \\\"b\\\" (c \\\\d\"}")
    , ("Windows line endings too",                  compilesToJSON "(document g (:r (prose | a\r\n | b\r\n )))"
                                                                   "{\"r\": \"a b\"}")
    , ("prose is a string where it is written, a doc too", compilesWithoutWarnings "(define/refinement titled (<r ⊢ record>) (doc (prose | Has\n | a label.\n )) (has r _label)) (document g (:r 1))")
    , ("bar lines are prose's alone",               failsWith "Bar lines are written only in prose"
                                                              "(document g (:r | a\n ))")
    , ("and prose takes nothing else",              failsWith "prose is written with bar lines"
                                                              "(document g (:r (prose \"a\")))")
    , ("|- still asserts",                          compilesToJSON "(document g (:r (|- 1 number)))"
                                                                   "{\"r\": 1}")
    , ("a bar in a string is text",                 compilesToJSON "(document g (:r \"| a\"))"
                                                                   "{\"r\": \"| a\"}")
    , ("a bar in a comment is a comment",           compilesToJSON "(document g ; | a\n (:r 1))"
                                                                   "{\"r\": 1}")
    , ("and in a block comment",                    compilesToJSON "(document g #| | a |# (:r 1))"
                                                                   "{\"r\": 1}")
    , ("a bar against text begins no bar line",     failsWith "|x"
                                                              "(document g (:r |x))")

      -- graph, the old name of document, still works, with a warning (WRN0001) that names document
    , ("graph still writes a document",            compilesToJSON "(graph g (:r 1))"
                                                                   "{\"r\": 1}")
    , ("with a warning that names document",        warnsWith "'graph' is now written 'document'"
                                                              "(graph g (:r 1))")
    , ("document does not warn",                    compilesWithoutWarnings "(document g (:r 1))")

      -- The old hash-marked spellings still work until 0.8.0.0, each with a warning (WRN0001)
      -- that names what replaces it where it is written
    , ("an old assertion warns",                    warnsWith "'#string' is now written '<x ⊢ string>'"
                                                              "(document g (:r #string \"a\"))")
    , ("and still asserts",                         compilesToJSON "(document g (:r #string \"a\"))"
                                                                   "{\"r\": \"a\"}")
    , ("an old parameter type warns",               warnsWith "'#number' is now written '<x ⊢ number>'"
                                                              "(define/procedure f (#number n) n) (document g (:r (f 1)))")
    , ("an old return type is now ⊢ p",             warnsWith "'#record' is now written '⊢ record'"
                                                              "(define/procedure f #record () (:a 1)) (document g (:r (f)))")
    , ("and a constant's",                          warnsWith "'#array' is now written '⊢ array'"
                                                              "(define/constant c #array (1 2)) (document g (:r c))")
    , ("and a fn's",                                warnsWith "'#number' is now written '⊢ number'"
                                                              "(document g (:r (let ($f (fn #number (n) n)) (f 1))))")
    , ("an old question is now ∈",                  warnsWith "'(satisfies x #number)' is now written '<x ∈ number>'"
                                                              "(document g (:r (satisfies 1 #number)))")
    , ("and an old case head (∈ p)",                warnsWith "'(satisfies #number)' is now written '(∈ number)'"
                                                              "(document g (:r (case 1 ((satisfies #number) \"n\") (otherwise \"o\"))))")
    , ("the old empty record is now (make record)", warnsWith "'#record-empty' is now written '(make record)'"
                                                              "(document g (:r #record-empty))")
    , ("and the old empty array (make array)",      warnsWith "'#array-empty' is now written '(make array)'"
                                                              "(document g (:r #array-empty))")
    , ("the current spellings do not warn",         compilesWithoutWarnings "(define/procedure f ⊢ record (<n ⊢ number>) (:n n)) (document g (:r (f 1) :q <1 ∈ number> :e (make record)))")

      -- Ouro has no triple-quoted strings: """Spring""" is three strings, which a warning (WRN0009) points to prose
    , ("\"\"\"text\"\"\" warns",                     warnsWith "Ouro has no triple-quoted strings"
                                                              "(document g (:r (\"\"\"Spring\"\"\")))")
    , ("and is three strings",                      compilesToJSON "(document g (:r (\"\"\"Spring\"\"\")))"
                                                                   "{\"r\": [\"\", \"Spring\", \"\"]}")
    , ("two strings written apart do not warn",     compilesWithoutWarnings "(document g (:r (\"\" \"x\")))")
    , ("nor does an empty string",                  compilesWithoutWarnings "(document g (:r \"\"))")

      -- Comparisons are words. '<' and '>' delimit an infix expression, its '<' written
      -- against the first operand, so each old name is an error that names its word
    , ("< is now lt",                               failsWith "'<' is now 'lt': (lt a b)"
                                                              "(document g (:r (< 1 2)))")
    , ("<= is now leq",                             failsWith "'<=' is now 'leq': (leq a b)"
                                                              "(document g (:r (<= 1 2)))")
    , (">= is now geq",                             failsWith "'>=' is now 'geq': (geq a b)"
                                                              "(document g (:r (>= 1 2)))")
    , ("> closes no <, and names gt",               failsWith "A comparison is a word: (gt a b)"
                                                              "(document g (:r (> 1 2)))")
    , ("= is now eq",                               failsWith "'=' is now 'eq': (eq a b)"
                                                              "(document g (:r (= 1 2)))")
    , ("/= is now neq",                             failsWith "'/=' is now 'neq': (neq a b)"
                                                              "(document g (:r (/= 1 2)))")
    , ("≠ is now neq",                              failsWith "' is now 'neq': (neq a b)"
                                                              "(document g (:r (≠ 1 2)))")

      -- <a op b> is (op a b), as SRFI 105's {a op b}: no precedence, nesting written, and a
      -- chain only where a left fold means something
    , ("<a op b> is (op a b)",                      compilesToJSON "(document g (:r <1 lt 2> :s <\"a\" eq \"b\">))"
                                                                   "{\"r\": true, \"s\": false}")
    , ("a chain folds left",                        compilesToJSON "(document g (:r <1 + 2 + 3> :s <10 - 2 - 3>))"
                                                                   "{\"r\": 6, \"s\": 5}")
    , ("nesting is written",                        compilesToJSON "(document g (:r <2 lt <1 + 2>> :s <<10 - 2> * 3>))"
                                                                   "{\"r\": true, \"s\": 24}")
    , ("any name of two arguments is an operator",  compilesToJSON "(define/procedure plus (a b) (+ a b)) (document g (:r <1 plus 2>))"
                                                                   "{\"r\": 3}")
    , ("a hole makes it a function",                compilesToJSON "(document g (:r (filter (1500 1700) <? gt 1600>)))"
                                                                   "{\"r\": [1700]}")
    , ("one element is not infix",                  failsWith "An infix expression has an operator between each two operands"
                                                              "(document g (:r <1>))")
    , ("nor are two",                               failsWith "An infix expression has an operator between each two operands"
                                                              "(document g (:r <not true>))")
    , ("nor an operand without its operator",       failsWith "An infix expression has an operator between each two operands"
                                                              "(document g (:r <1 + 2 3>))")
    , ("an operator is a name",                     failsWith "An infix operator is a name"
                                                              "(document g (:r <1 (array) 2>))")
    , ("mixed operators are nested by hand",        failsWith "'+' and '-' are mixed in one infix expression."
                                                              "(document g (:r <1 + 2 - 3>))")
    , ("a relation does not chain",                 failsWith "Write each pair: (all (<a lt b> <b lt c>))."
                                                              "(document g (:r <1 lt 2 lt 3>))")
    , ("nor does a function of the module",         failsWith "Nest it: <<a plus b> plus c>."
                                                              "(define/procedure plus (a b) (+ a b)) (document g (:r <1 plus 2 plus 3>))")
    , ("-> is a name of its own",                   compilesToJSON "(document g (:r (-> 1 (+ 2))))"
                                                                   "{\"r\": 3}")
    , ("with a variant after a /, not yet a form",  failsWith "A qualified name, module/function"
                                                              "(document g (:r (->/or 1 0 (+ 2))))")
    , ("written apart from what follows it",        failsWith "'->' is written apart from what follows it"
                                                              "(document g (:r (->x 1)))")
    , ("a bare thru is not a value",                failsWith "thru"
                                                              "(document g (:r #date (thru (days 1))))")
    ]
    where
    titled = "(define/refinement titled (#record r) (has r _label)) "


-- How a form is read, from its direct elements only, never their types: a call if it
-- starts with a symbol, a record if any element is a ':key value' attribute pair (or a context),
-- and an array otherwise. A record inside an array has its own parentheses.
classification :: Group
classification = Group "Classification: records, arrays and calls"
    [ ("an array holds values of any type",         compilesToJSON "(document g (:r (1 \"a\" true (:k 1) (2 3))))"
                                                                   "{\"r\": [1, \"a\", true, {\"k\": 1}, [2, 3]]}")
    , ("inlays alone are an array, spliced",        compilesToJSON "(document g (:r ((inlay (1 2)) (inlay (3)))))"
                                                                   "{\"r\": [1, 2, 3]}")
    , ("so a record's inlays alone need make",      failsWith "a valid Array to inlay"
                                                              "(define/procedure a () (:k 1)) (define/procedure b () (:j 2)) (document g ((inlay (a)) (inlay (b))))")

      -- make makes what its kind names, whatever the shape of what follows
    , ("make array begins with a name",             compilesToJSON "(define/constant k \"x\") (document g (:r (make array k 2)))"
                                                                   "{\"r\": [\"x\", 2]}")
    , ("its empty values",                          compilesToJSON "(document g (:a (make array) :b (make record)))"
                                                                   "{\"a\": [], \"b\": {}}")
    , ("make record takes a literal's items",       compilesToJSON "(document g (:r (make record :k 1 :j (+ 1 1))))"
                                                                   "{\"r\": {\"k\": 1, \"j\": 2}}")
    , ("and its inlays alone",                      compilesToJSON "(document g (:r (make record (inlay (:a 1)) (inlay (:b 2)))))"
                                                                   "{\"r\": {\"a\": 1, \"b\": 2}}")
    , ("the kind is a record or an array",          failsWith "make makes a record or an array"
                                                              "(document g (:r (make string \"a\")))")
    , ("written by its name",                       failsWith "make makes a record or an array"
                                                              "(document g (:r (make)))")
    , ("its names are checked",                     failsWith "'nope' is not bound here"
                                                              "(document g (:r (make array nope)))")
    , ("no function is named make",                 failsWith "'make' is read by its syntax"
                                                              "(define/procedure make (x) x) (document g (:r 1))")
    , ("a record in an array has its own parentheses", compilesToJSON "(document g (:r ((:k 1) \"x\" 3)))"
                                                                   "{\"r\": [{\"k\": 1}, \"x\", 3]}")
    , ("an array of one record",                    compilesToJSON "(document g (:r ((:a 1))))"
                                                                   "{\"r\": [{\"a\": 1}]}")
    , ("an attribute pair makes a record",          compilesToJSON "(document g (:r (:a 1)))"
                                                                   "{\"r\": {\"a\": 1}}")
    , ("a loose value in a record is an error",     failsWith "an unbound expression"
                                                              "(document g (:r (:a 1 2)))")
    , ("one attribute pair among values makes a record", failsWith "an unbound expression"
                                                              "(document g (:r (1 :a 2)))")
    , ("array builds an Array of computed values",  compilesToJSON "(document g (:r (array (+ 1 1) \"a\")))"
                                                                   "{\"r\": [2, \"a\"]}")
    , ("and of no values, an empty one",            compilesToJSON "(document g (:r (array)))"
                                                                   "{\"r\": []}")
    , ("and without a warning",                     compilesWithoutWarnings "(document g (:r (array 1 2)))")
    , ("list, its old name, is an error naming it", failsWith "'list' is now 'array': (array 1 2 3)."
                                                              "(document g (:r (list 1 2)))")
    , ("inside a procedure too",                    failsWith "'list' is now 'array'"
                                                              "(define/procedure f #array (#number n) (list n)) (document g (:r (f 1)))")
    , ("but a module may define its own list",      compilesToJSON "(define/procedure list #string (#string s) s) (document g (:r (list \"a\")))"
                                                                   "{\"r\": \"a\"}")
    , ("std builds its arrays with array",          compilesWithoutWarnings "(import std/base) (import std/actor) (import std/digital) (import std/exhibition) (import std/object) (import std/provenance) (document g (:r 1))")
    ]


-- Calendar arithmetic. Months and years clip to the end of a shorter month instead of
-- rolling over into the next one, so a shift never skips past the month it lands in.
dates :: Group
dates = Group "Dates: calendar arithmetic"
    [ ("a month after 31 January is 29 February",   compilesToJSON ("(document g (:r (+ " <> date "2020-01-31" <> " (months 1))))")
                                                                   "{\"r\": \"2020-02-29T00:00:00Z\"}")
    , ("or 28 February outside a leap year",        compilesToJSON ("(document g (:r (+ " <> date "2021-01-31" <> " (months 1))))")
                                                                   "{\"r\": \"2021-02-28T00:00:00Z\"}")
    , ("a month after 31 May is 30 June",           compilesToJSON ("(document g (:r (+ " <> date "2020-05-31" <> " (months 1))))")
                                                                   "{\"r\": \"2020-06-30T00:00:00Z\"}")
    , ("a month before 31 March is 29 February",    compilesToJSON ("(document g (:r (- " <> date "2020-03-31" <> " (months 1))))")
                                                                   "{\"r\": \"2020-02-29T00:00:00Z\"}")
    , ("a year after 29 February is 28 February",   compilesToJSON ("(document g (:r (+ " <> date "2020-02-29" <> " (years 1)) :s (+ " <> date "2020-02-29" <> " (years 4))))")
                                                                   "{\"r\": \"2021-02-28T00:00:00Z\", \"s\": \"2024-02-29T00:00:00Z\"}")
    , ("thru a month ends that month",              compilesToJSON ("(document g (:r (+ " <> date "2020-01-31" <> " (thru (months 1))) :s (+ " <> date "2020-01-31" <> " (thru (months 0)))))")
                                                                   "{\"r\": \"2020-02-29T23:59:59Z\", \"s\": \"2020-01-31T23:59:59Z\"}")

      -- A year has at least four digits, as ISO 8601 and xsd:dateTime require
    , ("a year before 1000 keeps its leading zeros", compilesToJSON "(document g (:d #date \"0079-08-24T12:00:00Z\"))"
                                                                   "{\"d\": \"0079-08-24T12:00:00Z\"}")
    , ("as its arithmetic and to-string do",        compilesToJSON "(document g (:d (+ #date \"0079-08-24T12:00:00Z\" (days 2)) :s (to-string #date \"0079-08-24T12:00:00Z\")))"
                                                                   "{\"d\": \"0079-08-26T12:00:00Z\", \"s\": \"0079-08-24T12:00:00Z\"}")
    , ("a year of fewer than four digits is not a date", failsWith "a string that is not a date"
                                                                   "(document g (:d #date \"81-01-01T00:00:00Z\"))")
    , ("nor through std/time",                      failsWith "a string that is not a date"
                                                                   "(import std/time) (document g (:d (time/year \"81\")))")
    , ("thru a year ends that year",                compilesToJSON ("(document g (:r (+ " <> date "2020-02-29" <> " (thru (years 1)))))")
                                                                   "{\"r\": \"2021-12-31T23:59:59Z\"}")
    , ("days do not clip",                          compilesToJSON ("(document g (:r (+ " <> date "2020-01-31" <> " (days 30))))")
                                                                   "{\"r\": \"2020-03-01T00:00:00Z\"}")
    , ("validate reads a timespan's four dates",    withTests 1 . property $
                                                    fmap (\out -> all (\k -> T.isInfixOf ("\"" <> k <> "\": \"1881-01-01T00:00:00Z\"") (TL.toStrict out)) dateKeys)
                                                         (validate "t.json" (timespan [ (k, "1881-01-01T00:00:00Z") | k <- dateKeys ]) defaultOptions)
                                                    === Right True)
    , ("and rejects one that is not a date",        withTests 1 . property $
                                                    either (T.isInfixOf "Invalid ISO 8601 Timestamp" . T.pack) (const False)
                                                           (validate "t.json" (timespan [("begin_of_the_begin", "hello")]) defaultOptions)
                                                    === True)
    , ("in each of the four keys",                  withTests 1 . property $
                                                    [ either (const True) (const False) (validate "t.json" (timespan [(k, "81-01-01T00:00:00Z")]) defaultOptions) | k <- dateKeys ]
                                                    === [True, True, True, True])

      -- A date is a string in date, never converted: asserted, it passes on as written, and
      -- shift moves it, reading it where it is used
    , ("a date passes on as written",               compilesToJSON "(document g (:r <\"2020-01-31T00:00:00Z\" ⊢ date>))"
                                                                   "{\"r\": \"2020-01-31T00:00:00Z\"}")
    , ("shift moves a date",                        compilesToJSON ("(document g (:a (shift " <> date "2020-01-31" <> " (days 1)) :b (shift " <> date "2020-01-31" <> " (months 1))))")
                                                                   "{\"a\": \"2020-02-01T00:00:00Z\", \"b\": \"2020-02-29T00:00:00Z\"}")
    , ("a negative duration moves it back",         compilesToJSON ("(document g (:r (shift " <> date "2020-03-31" <> " (months -1))))")
                                                                   "{\"r\": \"2020-02-29T00:00:00Z\"}")
    , ("through a thru, forwards",                  compilesToJSON ("(document g (:r (shift " <> date "2020-01-31" <> " (thru (days 1)))))")
                                                                   "{\"r\": \"2020-02-01T23:59:59Z\"}")
    , ("on a string, unasserted",                   compilesToJSON "(document g (:r (shift \"2020-01-31T00:00:00Z\" (days 1))))"
                                                                   "{\"r\": \"2020-02-01T00:00:00Z\"}")
    , ("a string that is no date is refused",       failsWith "actualRefinement = \"a string that is not a date\""
                                                              "(document g (:r (shift \"tomorrow\" (days 1))))")
    , ("as is a value that is no duration",         failsWith "expectedRefinement = \"duration\", actualRefinement = \"number\""
                                                              ("(document g (:r (shift " <> date "2020-01-31" <> " 1)))"))
    , ("shift takes two operands",                  failsWith "shift takes a date, then the duration"
                                                              ("(document g (:r (shift " <> date "2020-01-31" <> ")))"))
    ]

    where
    date day = "#date \"" <> day <> "T00:00:00Z\""

    dateKeys = ["begin_of_the_begin", "end_of_the_begin", "begin_of_the_end", "end_of_the_end"]

    -- A JSON record holding a timespan of the given dates
    timespan pairs = "{\"timespan\": {" <> T.intercalate ", " [ "\"" <> k <> "\": \"" <> v <> "\"" | (k, v) <- pairs ] <> "}}"


-- Source files are UTF-8 on every platform: a byte order mark is dropped, CRLF line endings
-- become LF, and bytes that are not UTF-8 are an error rather than garbled text.
encoding :: Group
encoding = Group "Encoding: source files are UTF-8"
    [ ("a UTF-8 file compiles",              withTests 1 . property $
                                                  shouldCompileTo (fixture "ouro/encoding.ouro") "encoding" (fixture "json/encoding.json"))
    , ("whatever the code page",             withTests 1 . property $ do
                                                  content <- evalIO (readSourceFile (fixture "ouro/encoding.ouro"))
                                                  fmap (T.isInfixOf "\201douard Manet") content === Right True)
    , ("names and keys can be any letters",  compilesToJSON "(define/procedure caf\233 () \"x\") (document g (:r (caf\233) :na\239ve 1))"
                                                                 "{\"r\": \"x\", \"na\239ve\": 1}")
    , ("\955 is fn",                         compilesToJSON "(document g (let ($inc (\955 (x) (+ x 1))) (:r (inc 1))))"
                                                                 "{\"r\": 2}")
    , ("with a signature and a pattern",     compilesToJSON "(document g (:r (map ((1 2) (3 4)) (\955 #number ((?a ?b)) (+ a b)))))"
                                                                 "{\"r\": [3, 7]}")
    , ("\955 can name a function, as fn",     compilesToJSON "(define/procedure \955 (x) x) (document g (:r (fn 1)))"
                                                            "{\"r\": 1}")
    , ("\955x is an ordinary name",          compilesToJSON "(document g (let ($\955x 1) (:r \955x)))"
                                                                 "{\"r\": 1}")

      -- The symbols of logic and sets are their names
    , ("\172 \8743 \8744 are not, and, or",  compilesToJSON "(document g (:a (\172 true) :b (\8743 true false) :c (\8744 false true)))"
                                                                 "{\"a\": false, \"b\": false, \"c\": true}")
    , ("\8704 \8707 \8708 are the quantifiers", compilesToJSON "(document g (:a (\8704 (1 2) (gt ? 0)) :b (\8707 (1 2) (gt ? 1)) :c (\8708 (1 2) (gt ? 5))))"
                                                                    "{\"a\": true, \"b\": true, \"c\": true}")
    , ("\8746 \8745 \8726 \8838 are the set operations", compilesToJSON "(document g (:a (\8746 (1) (2)) :b (\8745 (1 2) (2)) :c (\8726 (1 2) (2)) :d (\8838 (1) (1 2))))"
                                                                            "{\"a\": [1, 2], \"b\": [2], \"c\": [1], \"d\": true}")
    , ("messages name the word",             failsWith "'and' is read by its syntax before any name is bound"
                                                            "(define/procedure \8743 (x) x) (document g (:r 1))")
    , ("\8743x is an ordinary name",         compilesToJSON "(document g (let ($\8743x 1) (:r \8743x)))"
                                                                 "{\"r\": 1}")
    , ("= and /= are eq and neq",            compilesToJSON "(document g (:a (eq 1 1) :b (neq 1 2) :d (case 3 ((eq 3) \"three\") (otherwise \"other\"))))"
                                                                 "{\"a\": true, \"b\": true, \"d\": \"three\"}")
    , ("\8899 \8898 are unions and intersections", compilesToJSON "(document g (:a (\8899 ((1) (2))) :b (\8898 ((1 2) (2)))))"
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
    [ ("a graph left open",                  unclosedAt (1, 1)  "')'" "(document g (:r 1)")
    , ("the innermost delimiter still open", unclosedAt (1, 13) "')'" "(document g (:r (array 1 2)")
    , ("a key with no value",                unclosedAt (1, 13) "')'" "(document g (:r")
    , ("a property with no subject",         unclosedAt (1, 13) "')'" "(document g (:r #string")
    , ("a definition with no name",          unclosedAt (1, 1)  "')'" "(define/procedure")
    , ("a parameter list left open",         unclosedAt (1, 21) "')'" "(define/procedure f (x y")
    , ("an import left open",                unclosedAt (1, 1)  "')'" "(import std/base")
    , ("on its line, in a later form",       unclosedAt (3, 3)  "')'" "(document a (:r 1))\n(document b\n  (:r (array 1)")
    , ("in an imported module, in its file", importsFailAt "SYN0001" ("open.ouro", 2, 1) ["')' to close this", "the end of the file"] [("open", "(export f)\n(define/procedure f () 1")]
                                                           "(import open) (document g (:r (open/f)))")
    , ("a string left open, at its opening quote", importsFailAt "SYN0000" ("inline-test", 1, 17) ["This string is never closed"] []
                                                           "(document g (:r \"open))")
    , ("on its own line",                    importsFailAt "SYN0000" ("inline-test", 2, 7) ["the file ends before its closing"] []
                                                           "(document g\n  (:r \"open\n more))")
    , ("a closed string is not affected",    compilesToJSON "(document g (:r \"x\" :s \"y\"))"
                                                           "{\"r\": \"x\", \"s\": \"y\"}")
    ]
    where
    unclosedAt (line, column) closer = importsFailAt "SYN0001" ("inline-test", line, column) [closer <> " to close this", "the end of the file"] []


-- A string is written as a JSON string: quotes, backslashes and control characters are
-- escaped, and every other character is written as it is. This fixed gap 19.
strings :: Group
strings = Group "Strings: escaped in the output"
    [ ("a quote is escaped",                     emitsExactly "(document g (:quote \"say \\\"hi\\\"\"))"
                                                              "\"quote\": \"say \\\"hi\\\"\"")
    , ("a backslash is escaped",                 emitsExactly "(document g (:slash \"a\\\\b\"))"
                                                              "\"slash\": \"a\\\\b\"")
    , ("a newline is escaped",                   emitsExactly "(document g (:newline \"a\\nb\"))"
                                                              "\"newline\": \"a\\nb\"")
    , ("a tab is escaped",                       emitsExactly "(document g (:tab \"a\\tb\"))"
                                                              "\"tab\": \"a\\tb\"")
    , ("other control characters are \\u escapes", emitsExactly "(document g (:bell \"a\\1b\"))"
                                                              "\"bell\": \"a\\u0001b\"")
    , ("other characters are written as they are", emitsExactly "(document g (:r \"caf\233\"))"
                                                              "\"r\": \"caf\233\"")
    , ("validate keeps an escaped quote",        withTests 1 . property $
                                                  fmap (T.isInfixOf "\"k\": \"a\\\"b\"" . TL.toStrict)
                                                       (validate "t.json" "{\"k\": \"a\\\"b\"}" defaultOptions)
                                                  === Right True)
      -- validate reads JSON's escapes (gap 21)
    , ("validate reads a \\u escape",            validatesTo "{\"k\": \"caf\\u00e9\"}"       "\"k\": \"caf\233\"")
    , ("and an escaped slash",                   validatesTo "{\"k\": \"a\\/b\"}"            "\"k\": \"a/b\"")
    , ("and a surrogate pair, as one character", validatesTo "{\"k\": \"\\uD83D\\uDE00\"}"   "\"k\": \"\128512\"")
    , ("and an escaped newline, written escaped", validatesTo "{\"k\": \"a\\nb\"}"           "\"k\": \"a\\nb\"")
    , ("a Haskell escape is an error",           rejects "{\"k\": \"\\x41\"}"        "is not a JSON escape")
    , ("so is a lone low surrogate",             rejects "{\"k\": \"\\uDE00\"}"      "alone it is not a character")
    , ("and a high one without its low half",    rejects "{\"k\": \"\\uD83Dx\"}"     "followed by its low surrogate")
    , ("and a raw control character",            rejects "{\"k\": \"a\tb\"}"         "is written escaped, as \\u0009")
    ]

    where
    validatesTo json expected = withTests 1 . property $
                                fmap (T.isInfixOf expected . TL.toStrict) (validate "t.json" json defaultOptions)
                                === Right True

    rejects json message      = withTests 1 . property $
                                either (T.isInfixOf message . T.pack) (const False) (validate "t.json" json defaultOptions)
                                === True


-- The string built-ins each take the string first, so -> threads through them. to-string
-- writes a value as the JSON output does; the rest take Strings apart and put them together.
stringBuiltins :: Group
stringBuiltins = Group "Strings: built-ins, each taking the string first"
    [ -- to-string
      ("to-string writes a Number as the output does", compilesToJSON "(document g (:a (to-string 42) :b (to-string 4.9) :c (to-string 123456789) :d (to-string -0.5) :e (to-string (+ 0.1 0.2))))"
                                                                   "{\"a\": \"42\", \"b\": \"4.9\", \"c\": \"123456789\", \"d\": \"-0.5\", \"e\": \"0.30000000000000004\"}")
    , ("and a Boolean, a String, a URI and a Date",   compilesToJSON "(document g (:a (to-string true) :b (to-string \"x\") :c (to-string #uri \"https://a.org/1\") :d (to-string #date \"1642-01-01T00:00:00Z\")))"
                                                                   "{\"a\": \"true\", \"b\": \"x\", \"c\": \"https://a.org/1\", \"d\": \"1642-01-01T00:00:00Z\"}")
    , ("so a number can be part of an id",          compilesToJSON "(document g (:id (+ \"https://a.org/coin/\" (to-string 7))))"
                                                                   "{\"id\": \"https://a.org/coin/7\"}")
    , ("a Record has no single text",               failsWith "a String, a finite Number, a Boolean, a URI or a Date"
                                                              "(document g (:r (to-string (:a 1))))")
    , ("to-string takes one argument",              failsWith "IncorrectArity"
                                                              "(document g (:r (to-string 1 2)))")
    , ("an error in an argument is passed on",      failsWith "a Number + a String"
                                                              "(document g (:r (to-string (+ 1 \"a\"))))")

      -- split and join
    , ("split takes a String apart at a separator",  compilesToJSON "(document g (:a (split \"Manet, Monet\" \", \") :b (split \"Manet\" \",\") :c (split \"a,,b\" \",\")))"
                                                                   "{\"a\": [\"Manet\", \"Monet\"], \"b\": [\"Manet\"], \"c\": [\"a\", \"\", \"b\"]}")
    , ("a separator is at least one character",     failsWith "a separator of at least one character"
                                                              "(document g (:r (split \"abc\" \"\")))")
    , ("join puts Strings together with a separator", compilesToJSON "(document g (:a (join (\"a\" \"b\") \", \") :b (join () \"-\") :c (join (split \"a b\" \" \") \"-\")))"
                                                                   "{\"a\": \"a, b\", \"b\": \"\", \"c\": \"a-b\"}")
    , ("join takes only Strings",                   failsWith "an Array of Strings to join"
                                                              "(document g (:r (join (1 2) \",\")))")
    , ("so a Number is written with to-string first", compilesToJSON "(document g (:r (join (map (1 2 3) to-string) \"-\")))"
                                                                   "{\"r\": \"1-2-3\"}")

      -- case and white space
    , ("upper-case, lower-case and trim",           compilesToJSON "(document g (:a (upper-case \"dutch\") :b (lower-case \"MANET\") :c (trim \"  Manet  \")))"
                                                                   "{\"a\": \"DUTCH\", \"b\": \"manet\", \"c\": \"Manet\"}")
    , ("each takes a String",                       failsWith "expectedType = \"a String\""
                                                              "(document g (:r (upper-case 1)))")
    , ("they thread, the string first",             compilesToJSON "(document g (:r (-> \"  dutch \" (trim) (upper-case))))"
                                                                   "{\"r\": \"DUTCH\"}")

      -- starts-with and ends-with
    , ("starts-with and ends-with",                 compilesToJSON "(document g (:a (starts-with \"https://a.org\" \"https\") :b (ends-with \"C-0042\" \"42\") :c (starts-with \"a\" \"abc\")))"
                                                                   "{\"a\": true, \"b\": true, \"c\": false}")

      -- slice
    , ("slice takes characters from start up to end", compilesToJSON "(document g (:a (slice \"C-0042\" 2 6) :b (slice \"C-0042\" -4 6) :c (slice \"C-0042\" 2 -1) :d (slice \"C-0042\" 0 0)))"
                                                                   "{\"a\": \"0042\", \"b\": \"0042\", \"c\": \"004\", \"d\": \"\"}")
    , ("an index outside the String is an error",   failsWith "a start and an end within the String's 6 characters"
                                                              "(document g (:r (slice \"C-0042\" 2 9)))")
    , ("as is an end before the start",             failsWith "the start first"
                                                              "(document g (:r (slice \"C-0042\" 4 2)))")
    , ("an index is a whole number",                failsWith "a whole number for each of start and end"
                                                              "(document g (:r (slice \"abc\" 0.5 1)))")

      -- pad-start
    , ("pad-start pads to a width",                 compilesToJSON "(document g (:a (pad-start \"42\" 4 \"0\") :b (pad-start \"12345\" 4 \"0\")))"
                                                                   "{\"a\": \"0042\", \"b\": \"12345\"}")
    , ("with one character",                        failsWith "one character to pad with"
                                                              "(document g (:r (pad-start \"42\" 4 \"00\")))")
    , ("so an accession number can be built",       compilesToJSON "(document g (:r (+ \"C-\" (pad-start (to-string 7) 4 \"0\"))))"
                                                                   "{\"r\": \"C-0007\"}")

      -- Building strings: + joins two, -> threads the pieces in one by one
    , ("a string of many pieces is built with ->",  compilesToJSON "(document g (let ($kind \"place\" $slug \"amsterdam\") (:r (-> \"https://example.org/\" (+ kind) (+ \"/\") (+ slug)))))"
                                                                   "{\"r\": \"https://example.org/place/amsterdam\"}")
    , ("a hole puts the value second",              compilesToJSON "(document g (:r (-> 7 (to-string) (pad-start 4 \"0\") (+ \"C-\" ?))))"
                                                                   "{\"r\": \"C-0007\"}")
    , ("an Array of pieces is folded from \"\"",    compilesToJSON "(document g (let ($kind \"place\") (:r (-> (array \"https://example.org/\" kind) (fold \"\" +)))))"
                                                                   "{\"r\": \"https://example.org/place\"}")
    , ("or joined with a separator",                compilesToJSON "(document g (:r (-> (\" Manet\" \"Proust \") (map trim) (join \", \"))))"
                                                                   "{\"r\": \"Manet, Proust\"}")

      -- ranges, Haskell's, both ends included
    , ("a range counts from start to end",          compilesToJSON "(document g (:a (1..5) :b (0..0) :c (5..1) :d (-2..2)))"
                                                                   "{\"a\": [1, 2, 3, 4, 5], \"b\": [0], \"c\": [], \"d\": [-2, -1, 0, 1, 2]}")
    , ("its ends are expressions",                  compilesToJSON "(document g (let ($n 3 $xs (\"a\" \"b\")) (:a (1..n) :b (1..(len xs)) :c (1 .. 2))))"
                                                                   "{\"a\": [1, 2, 3], \"b\": [1, 2], \"c\": [1, 2]}")
    , ("so numbered ids need no list",              compilesToJSON "(document g (:r (map (1..3) (fn (n) (+ \"C-\" (pad-start (to-string n) 4 \"0\"))))))"
                                                                   "{\"r\": [\"C-0001\", \"C-0002\", \"C-0003\"]}")
    , ("an end is a whole number",                  failsWith "a whole number at each end of a range"
                                                              "(document g (:r (1..2.5)))")
    , ("a range is alone in its parentheses",       failsWith "A range is written (start..end)"
                                                              "(document g (:r (1..2..3)))")
    , ("with an end on each side",                  failsWith "A range is written (start..end)"
                                                              "(document g (:r (..5)))")
    , ("and is no item of a record",                failsWith "A range is written (start..end)"
                                                              "(document g (:r 1..5))")
    , ("a lone dot is still no token",              failsWith "Unexpected token: '.'"
                                                              "(document g (:r (1.5.)))")
    , ("two numbers before .. count in steps",      compilesToJSON "(document g (:a (2 4 .. 10) :b (5 4 .. 1) :c (1 3 .. 6) :d (1 2 .. 0)))"
                                                                   "{\"a\": [2, 4, 6, 8, 10], \"b\": [5, 4, 3, 2, 1], \"c\": [1, 3, 5], \"d\": []}")
    , ("a step of 0 would never end",               failsWith "a second number different from the first"
                                                              "(document g (:r (1 1 .. 5)))")

      -- zip
    , ("zip pairs two Arrays",                      compilesToJSON "(document g (:a (zip (1 2) (\"a\" \"b\")) :b (zip () ())))"
                                                                   "{\"a\": [[1, \"a\"], [2, \"b\"]], \"b\": []}")
    , ("so a range numbers a list",                 compilesToJSON "(document g (:r (map (zip (1..2) (3.72 3.36)) (fn ((?n ?grams)) (:n n :grams grams)))))"
                                                                   "{\"r\": [{\"n\": 1, \"grams\": 3.72}, {\"n\": 2, \"grams\": 3.36}]}")
    , ("Arrays of different lengths are an error",  failsWith "two Arrays of the same length"
                                                              "(document g (:r (zip (1..3) (\"a\" \"b\"))))")
    , ("zip takes Arrays",                          failsWith "an Array to zip"
                                                              "(document g (:r (zip (1 2) \"ab\")))")

      -- replace-all
    , ("replace-all replaces every one",            compilesToJSON "(document g (:r (replace-all \"a b c\" \" \" \"-\")))"
                                                                   "{\"r\": \"a-b-c\"}")
    , ("of at least one character",                 failsWith "a String of at least one character to replace"
                                                              "(document g (:r (replace-all \"abc\" \"\" \"-\")))")
    ]


-- not negates; exists, forall and filter call a function on each element, as map does,
-- and exists and forall stop at the element that decides; contains and distinct compare as eq
-- does; has and get-or walk a path as get does, and tell a missing key from an error; image
-- reads a path as a set, through Arrays.
logicAndCollections :: Group
logicAndCollections = Group "Built-ins: logic, arrays and records"
    [ ("not negates",                            compilesToJSON "(document g (:a (not true) :b (not false)))"
                                                                "{\"a\": false, \"b\": true}")
    , ("not needs a Boolean",                    failsWith "expectedRefinement = \"boolean\""
                                                           "(document g (:r (not 1)))")
    , ("not takes one operand",                  failsWith "nameOfFunc = \"not\""
                                                           "(document g (:r (not true false)))")

      -- exists, forall and filter
    , ("exists finds an element",                compilesToJSON "(document g (:r (exists (1 2 3) (gt ? 2))))"
                                                                "{\"r\": true}")
    , ("exists of nothing is false",             compilesToJSON "(document g (:r (exists () (gt ? 2))))"
                                                                "{\"r\": false}")
    , ("forall checks each element",             compilesToJSON "(document g (:a (forall (1 2 3) (gt ? 0)) :b (forall (1 2 3) (gt ? 1))))"
                                                                "{\"a\": true, \"b\": false}")
    , ("forall of nothing is true",              compilesToJSON "(document g (:r (forall () (gt ? 2))))"
                                                                "{\"r\": true}")
    , ("exists stops at the deciding element",   compilesToJSON "(document g (:r (exists (\"ab\" 5) (fn (x) (gt (len x) 1)))))"
                                                                "{\"r\": true}")
    , ("forall stops at the deciding element",   compilesToJSON "(document g (:r (forall (\"ab\" 5) (fn (x) (gt (len x) 5)))))"
                                                                "{\"r\": false}")
    , ("none is not exists",                     compilesToJSON "(document g (:r (not (exists (1 2) (eq ? 3)))))"
                                                                "{\"r\": true}")
    , ("omits is true when no element answers true", compilesToJSON "(document g (:a (omits (1 2) (gt ? 2)) :b (omits (1 5) (gt ? 2))))"
                                                                    "{\"a\": true, \"b\": false}")
    , ("omits of nothing is true",               compilesToJSON "(document g (:r (omits () (gt ? 2))))"
                                                                "{\"r\": true}")
    , ("omits stops at the deciding element",    compilesToJSON "(document g (:r (omits (\"ab\" 5) (fn (x) (gt (len x) 1)))))"
                                                                "{\"r\": false}")
    , ("omits finds no contradiction",           compilesToJSON "(document g (let ($book (:tp46 (\"a\" \"b\") :ntp46 (\"c\"))) (:r (omits (get-or book () tp46) (contains (get-or book () ntp46) ?)))))"
                                                                "{\"r\": true}")
    , ("omits needs a function",                 failsWith "a function for omits to call"
                                                           "(document g (:r (omits (1 2) 1)))")
    , ("omits can name a function",              compilesToJSON "(define/procedure omits (x) x) (document g (:r (omits 1)))"
                                                           "{\"r\": 1}")
    , ("filter keeps the matching elements",     compilesToJSON "(document g (:r (filter (1 2 3) (gt ? 1))))"
                                                                "{\"r\": [2, 3]}")
    , ("filter with has",                        compilesToJSON "(document g (:r (filter ((:name \"a\") (:id 1)) (fn (p) (has p name)))))"
                                                                "{\"r\": [{\"name\": \"a\"}]}")
    , ("a hole for the array",                   compilesToJSON "(document g (let ($big (exists ? (gt ? 2))) (:r (big (1 5)))))"
                                                                "{\"r\": true}")
    , ("the answer must be a Boolean",           failsWith "expectedRefinement = \"boolean\""
                                                           "(document g (:r (exists (1 2) (+ ? 1))))")
    , ("exists needs a function",                failsWith "a function for exists to call"
                                                           "(document g (:r (exists (1 2) 1)))")
    , ("forall needs an Array",                  failsWith "an Array for forall to go through"
                                                           "(document g (:r (forall 5 (gt ? 1))))")
    , ("a malformed filter is an error",         failsWith "filter is written (filter array f)"
                                                           "(document g (:r (filter (gt ? 1))))")
    , ("exists can name a function",             compilesToJSON "(define/procedure exists (x) x) (document g (:r (exists 1)))"
                                                           "{\"r\": 1}")

      -- contains and distinct
    , ("contains looks in an Array",             compilesToJSON "(document g (:a (contains (1 2 3) 2) :b (contains (1 2) 5)))"
                                                                "{\"a\": true, \"b\": false}")
    , ("contains compares records by value",     compilesToJSON "(document g (:r (contains ((:a 1)) (:a 1))))"
                                                                "{\"r\": true}")
    , ("contains looks in a String",             compilesToJSON "(document g (:a (contains \"Amsterdam\" \"dam\") :b (contains \"Amsterdam\" \"x\")))"
                                                                "{\"a\": true, \"b\": false}")
    , ("contains cannot compare functions",      failsWith "functions cannot be compared"
                                                           "(document g (:r (contains (array (fn (x) x)) 1)))")
    , ("contains needs an Array or a String",    failsWith "an Array or a String to search"
                                                           "(document g (:r (contains 5 1)))")
    , ("distinct keeps the first of each",       compilesToJSON "(document g (:r (distinct (1 2 1 3 2))))"
                                                                "{\"r\": [1, 2, 3]}")
    , ("distinct of nothing is nothing",         compilesToJSON "(document g (:r (distinct ())))"
                                                                "{\"r\": []}")
    , ("distinct needs an Array",                failsWith "expectedType = \"an Array\""
                                                           "(document g (:r (distinct \"aa\")))")

      -- has and get-or
    , ("has follows a path",                     compilesToJSON "(document g (:a (has (:x (:y 1)) x y) :b (has (:x 1) z)))"
                                                                "{\"a\": true, \"b\": false}")
    , ("has reports a path through a value",     failsWith "a Record to look up"
                                                           "(document g (:r (has (:x 1) x y)))")
    , ("get-or gives the value when it is there", compilesToJSON "(document g (:r (get-or (:x 1) \"none\" x)))"
                                                                 "{\"r\": 1}")
    , ("get-or gives the default when it is not", compilesToJSON "(document g (:a (get-or (:x 1) \"none\" z) :b (get-or (:x (:y 1)) 0 x z)))"
                                                                 "{\"a\": \"none\", \"b\": 0}")
    , ("get-or evaluates the default only when used", compilesToJSON "(document g (:r (get-or (:x 1) (get () 5) x)))"
                                                                     "{\"r\": 1}")
    , ("a malformed get-or is an error",         failsWith "get-or is written (get-or record default key ...)"
                                                           "(document g (:r (get-or (:x 1) 1)))")
      -- A default cannot tell a missing key from one that holds it, so a check that a value is
      -- empty reads a missing key as empty; has, under and, tells them apart without reading it
    , ("get-or reads a missing key as its default", compilesToJSON "(document g (:a (eq (get-or (:x 1) () found) ()) :b (eq (get-or (:found ()) () found) ())))"
                                                                 "{\"a\": true, \"b\": true}")
    , ("has under and tells them apart",         compilesToJSON "(define/procedure found-nothing #boolean (#record s) (and (has s found) (eq (get s found) ()))) (document g (:a (found-nothing (:x 1)) :b (found-nothing (:found ())) :c (found-nothing (:found (\"A. Reid\")))))"
                                                                "{\"a\": false, \"b\": true, \"c\": false}")

      -- indexes in paths
    , ("get takes an index into an Array",       compilesToJSON "(document g (:r (get (:k ((:a 1) (:a 2))) k 1 a)))"
                                                                "{\"r\": 2}")
    , ("a negative index counts from the end",   compilesToJSON "(document g (:r (get (:k (1 2 3)) k -1)))"
                                                                "{\"r\": 3}")
    , ("an index past the end is an error",      failsWith "Index"
                                                           "(document g (:r (get (:k (1 2)) k 5)))")
    , ("an index needs an Array",                failsWith "an Array to take element 0 of"
                                                           "(document g (:r (get (:k (:a 1)) k 0)))")
    , ("an index is a whole number",             failsWith "a whole number to index an Array with"
                                                           "(document g (:r (get (:k (1 2)) k 0.5)))")
    , ("a key on an Array suggests an index",    failsWith "or read 'a' from every element with image"
                                                           "(document g (:r (get (:k ((:a 1))) k a)))")
    , ("has is false past the end",              compilesToJSON "(document g (:a (has (:k (1 2)) k 1) :b (has (:k (1 2)) k 2)))"
                                                                "{\"a\": true, \"b\": false}")
    , ("get-or gives the default past the end",  compilesToJSON "(document g (:r (get-or (:k ()) 0 k 0)))"
                                                                "{\"r\": 0}")
    , ("image takes no index",                   failsWith "a set has no positions"
                                                           "(document g (:r (image (:k (1 2)) k 0)))")
    , ("a hole cannot stand for an index",       failsWith "a hole cannot stand for a key or an index"
                                                           "(document g (let ($at (get (1 2) ?)) (:r (at 0))))")
    , ("nor a named hole for a key",             failsWith "a hole cannot stand for a key or an index"
                                                           "(document g (let ($at (has (:a 1) ?k)) (:r (at 1))))")

      -- image reads a path as a set
    , ("image follows a path through Arrays",    compilesToJSON "(document g (:r (image (:part ((:k (1 2)) (:k (2 3)))) part k)))"
                                                                "{\"r\": [1, 2, 3]}")
    , ("image of a single value is one element", compilesToJSON "(document g (:r (image (:k 1) k)))"
                                                                "{\"r\": [1]}")
    , ("a missing key adds nothing",             compilesToJSON "(document g (:a (image (:part ((:k 1) (:x 2))) part k) :b (image (:x 1) k)))"
                                                                "{\"a\": [1], \"b\": []}")
    , ("image starts from an Array",             compilesToJSON "(document g (:r (image ((:k 1) (:k 2)) k)))"
                                                                "{\"r\": [1, 2]}")
    , ("image compares records by value",        compilesToJSON "(document g (:r (image (:p ((:t (:id 1)) (:t (:id 1)))) p t)))"
                                                                "{\"r\": [{\"id\": 1}]}")
    , ("image compares kinds by id",             compilesToJSON "(document g (:r (image (:p ((:t ((:id \"a\" :_label \"x\"))) (:t ((:id \"a\" :_label \"y\"))))) p t id)))"
                                                                "{\"r\": [\"a\"]}")
    , ("image takes a hole",                     compilesToJSON "(document g (:r (map ((:k 1) (:k (2 3))) (image ? k))))"
                                                                "{\"r\": [[1], [2, 3]]}")
    , ("image reports a value on the way",       failsWith "a Record to follow 'j' from"
                                                           "(document g (:r (image (:k 1) k j)))")
    , ("image keeps errors",                     failsWith "Index"
                                                           "(document g (:r (image (:p ((:k (get () 5)))) p k)))")
    , ("a malformed image is an error",          failsWith "image is written (image record key ...)"
                                                           "(document g (:r (image (:k 1))))")
    , ("image can name a function",              compilesToJSON "(define/procedure image (x) x) (document g (:r (image 1)))"
                                                           "{\"r\": 1}")

      -- union, intersect and setminus treat Arrays as sets
    , ("union joins without repeats",            compilesToJSON "(document g (:a (union (1 2) (2 3)) :b (union (1 1) ())))"
                                                                "{\"a\": [1, 2, 3], \"b\": [1]}")
    , ("intersect keeps what both share",        compilesToJSON "(document g (:a (intersect (1 2 3) (3 2 4)) :b (intersect (1 2) (3))))"
                                                                "{\"a\": [2, 3], \"b\": []}")
    , ("setminus removes the second",            compilesToJSON "(document g (:a (setminus (1 2 3) (2)) :c (setminus (1 1 2) ())))"
                                                                "{\"a\": [1, 3], \"c\": [1, 2]}")
    , ("a set operation takes two sets",         failsWith "union takes two Arrays: (union (1 2) (2 3)) is every value of either. For more, unions takes one Array of them: (unions (a b c))."
                                                           "(document g (:r (union (1) (2) (1 3))))")
    , ("many sets are unions over an Array",     compilesToJSON "(document g (:a (unions ((1) (2) (1 3))) :b (intersections ((1 2 3) (2 3) (3)))))"
                                                                "{\"a\": [1, 2, 3], \"b\": [3]}")
    , ("sets compare records by value",          compilesToJSON "(document g (:r (intersect ((:a 1) (:b 2)) ((:a 1)))))"
                                                                "{\"r\": [{\"a\": 1}]}")
    , ("set operations take holes",              compilesToJSON "(document g (:r (map ((1 2) (2 3)) (setminus ? (2)))))"
                                                                "{\"r\": [[1], [3]]}")
    , ("a set must be an Array",                 failsWith "an Array, as a set"
                                                           "(document g (:r (union (1) 2)))")
    , ("a set cannot hold a function",           failsWith "functions cannot be compared"
                                                           "(document g (:r (union (array (fn (x) x)) ())))")
    , ("a set operation needs an Array",         failsWith "nameOfFunc = \"intersect\""
                                                           "(document g (:r (intersect)))")
    , ("subset is true when all are in the other", compilesToJSON "(document g (:a (subset (1 2) (2 3 1)) :b (subset (1 4) (1 2)) :c (subset () (1)) :d (subset (1 1) (1))))"
                                                                  "{\"a\": true, \"b\": false, \"c\": true, \"d\": true}")
    , ("subset compares records by value",       compilesToJSON "(document g (:r (subset ((:a 1)) ((:b 2) (:a 1)))))"
                                                                "{\"r\": true}")
    , ("subset takes two Arrays",                failsWith "nameOfFunc = \"subset\""
                                                           "(document g (:r (subset (1) (1) (1))))")
    , ("unions joins an Array of Arrays",        compilesToJSON "(document g (:a (unions ((1 2) (2 3) (4))) :b (unions ())))"
                                                                "{\"a\": [1, 2, 3, 4], \"b\": []}")
    , ("intersections of an Array of Arrays",    compilesToJSON "(document g (:r (intersections ((1 2 3) (2 3) (3 2)))))"
                                                                "{\"r\": [2, 3]}")
    , ("unions of computed sets",                compilesToJSON "(document g (:r (unions (map ((:k (1)) (:k (1 2))) (get ? k)))))"
                                                                "{\"r\": [1, 2]}")
    , ("intersections of nothing is an error",   failsWith "at least one Array to intersect"
                                                           "(document g (:r (intersections ())))")
    , ("unions needs an Array of Arrays",        failsWith "an Array of Arrays"
                                                           "(document g (:r (unions 5)))")
    , ("each must be an Array",                  failsWith "an Array, as a set"
                                                           "(document g (:r (unions ((1) 2))))")
    ]


-- A case branch can be headed by a condition, (when cond), as well as by a pattern, a
-- literal or a comparison guard. A condition must be a Boolean; true takes the branch.
conditions :: Group
conditions = Group "Case: conditions"
    [ ("when takes a branch when true",          compilesToJSON "(document g (:r (case 1 ((when false) \"a\") ((when (eq 1 1)) \"b\") (otherwise \"c\"))))"
                                                                "{\"r\": \"b\"}")
    , ("when can use other values",              compilesToJSON "(document g (let ($strict true) (:r (case 5 ((when strict) \"strict\") (otherwise \"lenient\")))))"
                                                                "{\"r\": \"strict\"}")
    , ("when takes and and or",                  compilesToJSON "(document g (:r (case 5 ((when (and (gt 5 1) (lt 5 9))) \"in\") (otherwise \"out\"))))"
                                                                "{\"r\": \"in\"}")
    , ("when sits beside patterns and literals", compilesToJSON "(document g (:r (case (:k 1) (2 \"two\") ((when false) \"no\") ((:k ?v) v) (otherwise \"other\"))))"
                                                                "{\"r\": 1}")
    , ("a condition must be a Boolean",          failsWith "expectedRefinement = \"boolean\""
                                                           "(document g (:r (case 5 ((when 1) \"a\") (otherwise \"b\"))))")
    , ("when outside a case is an error",        failsWith "when is written as the head of a case branch"
                                                           "(document g (:r (when true)))")
    , ("when can name a function",               compilesToJSON "(define/procedure when (x) x) (document g (:r (when 1)))"
                                                           "{\"r\": 1}")

      -- Comparison guards become conditions on the case's target
    , ("a guard compares a computed target",     compilesToJSON "(document g (:r (case (+ 2 3) ((gt 4) \"big\") (otherwise \"small\"))))"
                                                                "{\"r\": \"big\"}")
    , ("a when over all stops at the first false", compilesToJSON "(document g (:r (case 5 ((when (all ((lt 5 1) (lt 5 (get () 9))))) \"x\") (otherwise \"y\"))))"
                                                                  "{\"r\": \"y\"}")

      -- satisfies asks a property; in a branch head, of the case's target
    , ("satisfies asks a property, the value first", compilesToJSON "(document g (:a (satisfies \"x\" #string) :b (satisfies \"x\" #number) :c (satisfies \"https://a.org/x\" #uri) :d (satisfies \"nope\" #uri)))"
                                                                "{\"a\": true, \"b\": false, \"c\": true, \"d\": false}")
    , ("satisfies keeps errors",                 failsWith "Index"
                                                           "(document g (:r (satisfies (get () 5) #string)))")
    , ("satisfies heads a branch",               compilesToJSON "(document g (:r (map (array #uri \"https://a.org/1\" 3 \"x\") (fn (v) (case v ((satisfies #uri) \"an identifier\") ((satisfies #number) \"a number\") (otherwise \"other\"))))))"
                                                                "{\"r\": [\"an identifier\", \"a number\", \"other\"]}")
    , ("\8872 is satisfies",                     compilesToJSON "(document g (:r (case 3 ((\8872 #number) \"n\") (otherwise \"o\"))))"
                                                                "{\"r\": \"n\"}")
    , ("a -> step can be satisfies",             compilesToJSON "(document g (:a (-> 3 (satisfies #number)) :b (-> \"x\" (satisfies #number))))"
                                                                "{\"a\": true, \"b\": false}")
    , ("satisfies takes a hole for the value",   compilesToJSON "(document g (:r (filter (1 \"x\" 2) (satisfies ? #number))))"
                                                                "{\"r\": [1, 2]}")
    , ("a subject-less satisfies needs a case",  failsWith "satisfies is now "
                                                           "(document g (:r (map (array 1) (satisfies #string))))")
    , ("a refinement on its own is misplaced",   failsWith "A refinement on its own"
                                                           "(document g (:r (array #string)))")
    , ("satisfies can name a function",          compilesToJSON "(define/procedure satisfies (x) x) (document g (:r (satisfies 1)))"
                                                           "{\"r\": 1}")
    ]


-- A record's context is an ordinary value, written as JSON-LD writes it: a remote context's
-- URL, a local context record, or an array of them, applied in order. It is checked
-- where it is written.
contexts :: Group
contexts = Group "Contexts: a context is a value"
    [ ("a remote context with local terms is an array", compilesToJSON "(document g (:context (\"https://linked.art/ns/v1/linked-art.json\" (:ex \"https://example.org/ns/\")) :a 1))"
                                                                    "{\"@context\": [\"https://linked.art/ns/v1/linked-art.json\", {\"ex\": \"https://example.org/ns/\"}], \"a\": 1}")
    , ("local keywords",                            compilesToJSON "(document g (:context (:@vocab \"https://example.org/\" :@language \"en\") :a 1))"
                                                                   "{\"@context\": {\"@vocab\": \"https://example.org/\", \"@language\": \"en\"}, \"a\": 1}")
    , ("an expanded term definition",               compilesToJSON "(document g (:context (:condition (:@id \"ex:condition\" :@type \"@id\" :@container (\"@set\"))) :a 1))"
                                                                   "{\"@context\": {\"condition\": {\"@id\": \"ex:condition\", \"@type\": \"@id\", \"@container\": [\"@set\"]}}, \"a\": 1}")
    , ("Ouro has no null, so no context clears another", failsWith "Ouro has no null"
                                                              "(document g (:context null :a 1))")
    , ("nor sets a keyword to null",                failsWith "Ouro has no null"
                                                              "(document g (:context (:@vocab null) :a 1))")
    , ("nor maps a term to it",                     failsWith "Ouro has no null"
                                                              "(document g (:context (:ex null) :a 1))")
    , ("a record can be only a context",            compilesToJSON "(document g (:r (:context \"https://a.org/c.json\")))"
                                                                   "{\"r\": {\"@context\": \"https://a.org/c.json\"}}")
    , ("a context can be computed",                 compilesToJSON "(define/procedure linked-art () \"https://linked.art/ns/v1/linked-art.json\") (document g (:context ((linked-art) (:ex \"https://e.org/\")) :a 1))"
                                                                   "{\"@context\": [\"https://linked.art/ns/v1/linked-art.json\", {\"ex\": \"https://e.org/\"}], \"a\": 1}")
    , ("a #uri is a remote context",                compilesToJSON "(document g (:context #uri \"https://a.org/c.json\" :a 1))"
                                                                   "{\"@context\": \"https://a.org/c.json\", \"a\": 1}")
    , (":@context is :context",                     compilesToJSON "(document g (:@context \"https://a.org/c.json\" :a 1))"
                                                                   "{\"@context\": \"https://a.org/c.json\", \"a\": 1}")
    , ("any key can be a keyword",                  compilesToJSON "(document g (:@id \"https://a.org/1\" :a 1))"
                                                                   "{\"@id\": \"https://a.org/1\", \"a\": 1}")

    -- A context is the record's metadata, which is serialised first wherever it is written
    , ("a context written last is serialised first", compilesToJSON "(document g (:a 1 :b 2 :context \"https://a.org/c.json\"))"
                                                                   "{\"@context\": \"https://a.org/c.json\", \"a\": 1, \"b\": 2}")
    , ("so is one written in the middle",           compilesToJSON "(document g (:a 1 :context (\"https://a.org/c.json\" (:ex \"https://e.org/\")) :b 2))"
                                                                   "{\"@context\": [\"https://a.org/c.json\", {\"ex\": \"https://e.org/\"}], \"a\": 1, \"b\": 2}")
    , ("and one written after an inlay",            compilesToJSON "(define/procedure base () (:id \"1\" :type \"T\")) (document g ((inlay (base)) :a 1 :context \"https://a.org/c.json\"))"
                                                                   "{\"@context\": \"https://a.org/c.json\", \"id\": \"1\", \"type\": \"T\", \"a\": 1}")

    -- An inlay layers its record's context under the record's own, as it layers attribute pairs:
    -- the inlays' contexts in order, then the written one, applied last
    , ("an inlay keeps its record's context",       compilesToJSON (ctx <> "(document g (:r ((inlay (c)) :b 2)))")
                                                                   "{\"r\": {\"@context\": \"https://a.org/c.json\", \"a\": 1, \"b\": 2}}")
    , ("a written context is applied after it",     compilesToJSON (ctx <> "(document g (:r ((inlay (c)) :context \"https://w.org/c.json\" :b 2)))")
                                                                   "{\"r\": {\"@context\": [\"https://a.org/c.json\", \"https://w.org/c.json\"], \"a\": 1, \"b\": 2}}")
    , ("wherever it is written",                    compilesToJSON (ctx <> "(document g (:r (:context \"https://w.org/c.json\" (inlay (c)) :b 2)))")
                                                                   "{\"r\": {\"@context\": [\"https://a.org/c.json\", \"https://w.org/c.json\"], \"a\": 1, \"b\": 2}}")
    , ("inlays' contexts keep their order",         compilesToJSON (ctx <> "(document g (:r ((inlay (c)) (inlay (d)) :b 2)))")
                                                                   "{\"r\": {\"@context\": [\"https://a.org/c.json\", \"https://d.org/c.json\"], \"a\": 1, \"d\": 4, \"b\": 2}}")
    , ("a pipeline keeps its context",              compilesToJSON (ctx <> "(document g (:r (-> (c) (with :b 2) (with :x 3))))")
                                                                   "{\"r\": {\"@context\": \"https://a.org/c.json\", \"a\": 1, \"b\": 2, \"x\": 3}}")
    , ("the same context twice is written once",   compilesToJSON (ctx <> "(document g (:r ((inlay (c)) :context \"https://a.org/c.json\" :b 2)))")
                                                                   "{\"r\": {\"@context\": \"https://a.org/c.json\", \"a\": 1, \"b\": 2}}")
    , ("so is a repeat written in the array",       compilesToJSON "(document g (:context (\"https://a.org/c.json\" \"https://a.org/c.json\") :a 1))"
                                                                   "{\"@context\": \"https://a.org/c.json\", \"a\": 1}")
    , ("but only next to itself",                   compilesToJSON "(document g (:context (\"https://a.org/c.json\" (:ex \"https://e.org/\") \"https://a.org/c.json\") :a 1))"
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
                                                              "(document g (:context (:@vocabb \"https://e.org/\") :a 1))")
    , ("a term maps to an IRI",                     failsWith "The term 'ex' cannot be a Number"
                                                              "(document g (:context (:ex 5) :a 1))")
    , ("a remote context is an absolute URL",       failsWith "is not an absolute URL"
                                                              "(document g (:context \"not a url\" :a 1))")
    , ("a context array holds no arrays",           failsWith "cannot hold another array"
                                                              "(document g (:context ((\"https://a.org/c.json\")) :a 1))")
    , ("an expanded definition's keys are keywords", failsWith "cannot be part of the definition of the term 't'"
                                                              "(document g (:context (:t (:@id \"x\" :foo 1)) :a 1))")
    , ("a date is a string in a context too",       compilesToJSON "(document g (:context (:@vocab #date \"2020-01-01T00:00:00Z\") :a 1))"
                                                                   "{\"@context\": {\"@vocab\": \"2020-01-01T00:00:00Z\"}, \"a\": 1}")
    , ("it is reported at the context",             failsWith "line 1, col 23"
                                                              "(document g (:context (:ex 5) :a 1))")
    , ("the (context (...)) form is replaced",      failsWith "has been replaced by ':context value'"
                                                              "(document g ((context (vocab \"https://e.org/\")) :a 1))")
    ]
    where
    ctx = "(define/procedure c () (:context \"https://a.org/c.json\" :a 1)) (define/procedure d () (:context \"https://d.org/c.json\" :d 4)) "


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
    remoteEntry = either (error . show) RemoteContext . mkUri

    remote :: T.Text -> Schema
    remote = Schema . pure . remoteEntry


-- A special form's name is bound like any other name, as in Scheme: a binding around a form,
-- whether an attribute key, a let binding, a parameter or a define/procedure, shadows it, and the form is
-- then an ordinary call of the binding. The forms Canon writes are not the source's, so no
-- binding can capture them.
specialFormsAreBindings :: Group
specialFormsAreBindings = Group "Special forms are bindings, as in Scheme"
    [ ("a let binding shadows a special form",      compilesToJSON "(document g (let ($array (fn (x) (+ x 1))) (:r (array 1))))"
                                                                   "{\"r\": 2}")
    , ("as does a bound map, which is called",      compilesToJSON "(document g (let ($map (fn (x) x)) (:r (map 1))))"
                                                                   "{\"r\": 1}")
    , ("a parameter, even one named fn",            compilesToJSON "(define/procedure call (fn) (fn)) (document g (:r (call (fn () 1))))"
                                                                   "{\"r\": 1}")
    , ("a defn, throughout its module",             compilesToJSON "(define/procedure array (x) (+ x 1)) (document g (:r (array 1)))"
                                                                   "{\"r\": 2}")
    , ("with a warning, as for a built-in",         warnsWith "or a special form replaces it throughout the file"
                                                              "(define/procedure array (x) (+ x 1)) (document g (:r (array 1)))")
    , ("and its calls have its arity",              failsWith "IncorrectArity"
                                                              "(define/procedure array (x) x) (define/procedure h () (array 1 2)) (document g (:r 1))")
    , ("a key, whose value is no function",         failsWith "'image' is also a special form, which a binding of the same name shadows"
                                                              "(document g (:image \"x\" :r (image \"a\" 1)))")
    , ("only where the binding is",                 compilesToJSON "(document g (:a (let ($array 1) array) :b (array 1 2)))"
                                                                   "{\"a\": 1, \"b\": [1, 2]}")
    , ("an inlay is shadowed too",                  compilesToJSON "(document g (let ($inlay (fn (x) x)) (:r ((inlay 1) 2))))"
                                                                   "{\"r\": [1, 2]}")
    , ("Canon's forms are not captured",            compilesToJSON "(document g (let ($case 1 $fn 2 $inlay 3) (:r (or false true) :s (map (1 2) (+ ? 1)) :t (with (:a 1) :b 2))))"
                                                                   "{\"r\": true, \"s\": [2, 3], \"t\": {\"a\": 1, \"b\": 2}}")
    , ("nor is a guard's",                          compilesToJSON "(document g (let ($let 4) (:r (case 5 ((lt 3) \"small\") (otherwise \"big\")))))"
                                                                   "{\"r\": \"big\"}")
    ]


-- Known gaps, numbered as in the specification's Appendix B. Each test pins the current,
-- incorrect behaviour; the change that fixes a gap adds a test of the correct behaviour and
-- keeps the pin, run negated (fixedGap), so the old behaviour coming back fails the suite.
knownGaps :: Group
knownGaps = Group "Known gaps: current and fixed"
    [ ("gap 9 (fixed): no suggestion for a local name", fixedGap $ failsWithout "Perhaps you meant"
                                                                 "(document g (let ($artist 1) (:r artst)))")
    , ("gap 10 (fixed): get's missing-key hint mentions define", fixedGap $ failsWith "'define'"
                                                              "(document g (:p (:a 1) :r (get p b)))")
    , ("gap 10 (fixed): subtraction's hint names L.Duration", fixedGap $ failsWith "L.Duration"
                                                              "(document g (:r (- \"a\" 1)))")
    , ("gap 10 (fixed): a reserved $ name is called an attribute key", fixedGap $ failsWith "for your attribute"
                                                              "(document g (let ($get 1) (:r get)))")
    , ("gap 11 (fixed): a parameter named fn",      fixedGap $ failsWith "A fn is written"
                                                              "(define/procedure call (fn) (fn)) (document g (:r (call 1)))")
    , ("gap 14 (fixed): a record of only inlays is made", compilesToJSON "(define/procedure a () (:k 1)) (define/procedure b () (:j 2)) (document g (make record (inlay (a)) (inlay (b))))"
                                                              "{\"k\": 1, \"j\": 2}")
    , ("gap 17 (fixed): a URI's empty root path is dropped", fixedGap $ compilesToJSON "(document g (:u #uri \"https://a.org/\"))"
                                                                   "{\"u\": \"https://a.org\"}")
    , ("gap 18: an attribute pair among values has no hint", failsWithout "(1 (:a 2))"
                                                                 "(document g (:r (1 :a 2)))")
    , ("gap 21 (fixed): validate rejects a \\u escape", fixedGap . withTests 1 . property $
                                                    assert (either (const True) (const False)
                                                                   (validate "t.json" "{\"k\": \"\\u00e9\"}" defaultOptions)))
    , ("gap 21 (fixed): and an escaped slash",    fixedGap . withTests 1 . property $
                                                    assert (either (const True) (const False)
                                                                   (validate "t.json" "{\"k\": \"\\/\"}" defaultOptions)))
    , ("gap 21 (fixed): but accepts a Haskell escape", fixedGap . withTests 1 . property $
                                                    fmap (T.isInfixOf "\"k\": \"A\"" . TL.toStrict)
                                                         (validate "t.json" "{\"k\": \"\\x41\"}" defaultOptions)
                                                    === Right True)
    , ("gap 22 (fixed): a URI's empty path segment is dropped", fixedGap $ compilesToJSON "(document g (:u #uri \"https://a.org/x//y\"))"
                                                                      "{\"u\": \"https://a.org/x/y\"}")
    , ("gap 22 (fixed): so is a leading one",      fixedGap $ compilesToJSON "(document g (:u #uri \"https://a.org//x\"))"
                                                                   "{\"u\": \"https://a.org/x\"}")
    , ("gap 23 (fixed): an unterminated string is reported where the input ends", fixedGap $ importsFailAt "SYN0000" ("inline-test", 1, 21) ["Malformed or unrecognized lexical token sequence"] []
                                                                                "(document g (:r \"open))")
    , ("gap 24 (fixed): #date accepts a year of fewer than four digits", fixedGap $ compilesToJSON "(document g (:d #date \"81-01-01T00:00:00Z\"))"
                                                                                "{\"d\": \"81-01-01T00:00:00Z\"}")
    , ("gap 25: warning codes collide across categories", withTests 1 . property $
                                                    map smartWarningCode [ Lint (UnanchoredLiteral "a" "b")
                                                                         , Semantic (ElidedPropertyAssign "k" "define")
                                                                         , Vocab (UnboundTypeReference "T" "r") ]
                                                    === ["WRN0000", "WRN0000", "WRN0000"])
    , ("gap 26: an unknown export is a lexical error", importsFailAt "SYN0000" ("inline-test", 1, 9) ["The module exports 'f', but defines no function by that name"] []
                                                                    "(export f)\n(document g (:r 1))")
    , ("gap 31: a key cannot hold a '.'",            importsFailAt "SYN0000" ("inline-test", 1, 19) ["Unexpected token: '.'"] []
                                                                     "(document g (:ex:a.b 1 :y 2))")
    , ("gap 31: a '/' ends a key",                   importsFailAt "SCP0000" ("inline-test", 1, 19) ["variableName = \"/b\""] []
                                                                     "(document g (:ex:a/b 1 :y 2))")
    , ("gap 31: a '#' ends a key",                   importsFailAt "SYN0000" ("inline-test", 1, 19) ["Unknown refinement: b"] []
                                                                     "(document g (:ex:a#b 1 :y 2))")
    , ("gap 39 (fixed): insert floors a fractional index", fixedGap $ compilesToJSON "(document g (:r (insert (1 2) 1.5 \"x\")))"
                                                                   "{\"r\": [1, \"x\", 2]}")
    , ("gap 41 (fixed): WRN0000 says a value is erased", fixedGap $ warnsWith "will be completely erased"
                                                               "(document g (:r (#record-empty \"x\")))")
    , ("a string after (make record) is kept, without a warning", compilesWithoutWarnings "(document g (:r ((make record) \"x\")))")
    , ("and is the array's second value",            compilesToJSON "(document g (:r ((make record) \"x\")))"
                                                                   "{\"r\": [{}, \"x\"]}")
    , ("gap 42 (fixed): a removed form's name is reserved as a key", fixedGap $ failsWith "ShadowedVariableError"
                                                                   "(document g (:a (:attr 1)))")
    , ("gap 42 (fixed): and as a binding",           fixedGap $ failsWith "ShadowedVariableError"
                                                              "(document g (let ($defun 1) (:r defun)))")
    , ("gap 43: a binding may be named map",         compilesToJSON "(document g (let ($map 1) (:r map)))"
                                                                   "{\"r\": 1}")
    , ("gap 43: and a key",                          compilesToJSON "(document g (:map 1))"
                                                                   "{\"map\": 1}")
    , ("gap 43 (fixed): but not list",               fixedGap $ failsWith "ShadowedVariableError"
                                                              "(document g (let ($list 1) (:r list)))")
    , ("gap 43 (fixed): and a bound map is not called", fixedGap $ failsWith "LexicalError"
                                                              "(document g (let ($map (fn (x) x)) (:r (map 1))))")
    , ("gap 44 (fixed for names): an uncalled function's unbound name", fixedGap $ compilesToJSON "(define/procedure f () nope) (document g (:r 1))"
                                                                   "{\"r\": 1}")
    , ("gap 44 (fixed for arity): and its wrong arity", fixedGap $ compilesToJSON "(define/procedure f (x) x) (define/procedure h () (f 1 2)) (document g (:r 1))"
                                                                   "{\"r\": 1}")
    , ("gap 44 (fixed for names): an untaken branch's unbound name", fixedGap $ compilesToJSON "(document g (:r (case 1 ((eq 1) \"a\") (otherwise nope))))"
                                                                   "{\"r\": \"a\"}")
    , ("gap 44 (fixed): an uncalled function's recursion", fixedGap $ compilesToJSON "(define/procedure f () (f)) (document g (:r 1))"
                                                                   "{\"r\": 1}")
    , ("gap 45: a binding of and is ignored at a call", compilesToJSON "(document g (let ($and (fn (a b) 1)) (:r (and true false))))"
                                                                   "{\"r\": false}")
    , ("gap 45: and of with",                        compilesToJSON "(document g (let ($with (fn (a b) 1)) (:r (with (:a 1) :b 2))))"
                                                                   "{\"r\": {\"a\": 1, \"b\": 2}}")
    , ("gap 46 (fixed): an unknown top-level form is named by its Haskell constructor", fixedGap $ importsFailAt "SYN0000" ("inline-test", 1, 2) ["Invalid top-level keyword: Symbol"] []
                                                                   "(foo f () 1)\n(document g (:r 1))")
    , ("an unknown top-level form is named as written", importsFailAt "SYN0000" ("inline-test", 1, 2) ["'foo' cannot begin a form at the top level", "A file's top level holds"] []
                                                                   "(foo f () 1)\n(document g (:r 1))")
    , ("and a top level that is not a form",       importsFailAt "SYN0000" ("inline-test", 1, 1) ["A top-level form begins with '(', not ':k'"] []
                                                                   ":k (document g (:r 1))")
    , ("gap 47 (fixed): a name that begins with null is read as null and a name", fixedGap $ failsWith "Expected a function name"
                                                                   "(define/procedure null-result (x) x) (document g (:r (null-result 1)))")
    , ("gap 47 (fixed): and one that begins with true", fixedGap $ failsWith "Expected a function name"
                                                                   "(define/procedure truer (x) x) (document g (:r (truer 1)))")
    , ("gap 47 (fixed): and a graph named so",      fixedGap $ failsWith "Expected graph name"
                                                                   "(document null-results (:r 1))")
    , ("gap 47 (fixed): and an attribute named so, read where it is used", fixedGap $ failsWith "an unbound expression"
                                                                   "(document g (:null-ish 1 :r null-ish))")
    , ("gap 48 (fixed): a whole number of ten million or more is written in scientific notation", fixedGap $ compilesToJSON "(document g (:a 9999999 :b 10000000 :c 123456789))"
                                                                   "{\"a\": 9999999, \"b\": 1.0e7, \"c\": 1.23456789e8}")
    , ("gap 48 (fixed): and so is a small fraction", fixedGap $ compilesToJSON "(document g (:r 0.00001))"
                                                                   "{\"r\": 1.0e-5}")
    , ("gap 49 (fixed): a define/constant in an expression is an unknown qualifier define", fixedGap $ failsWith "qualifierName = \"define\""
                                                                   "(document g (:r (define/constant q 1)))")
    , ("gap 49 (fixed): not the error a local definition is", fixedGap $ failsWithout "can only appear at the top level of a file"
                                                                   "(document g (:r (define/constant q 1)))")
    , ("gap 49 (fixed): a define/constant in an expression is a local definition", failsWith "A definition can only appear at the top level of a file."
                                                                   "(document g (:r (define/constant q 1)))")
    , ("gap 49 (fixed): and so is a define/refinement", failsWith "A definition can only appear at the top level of a file."
                                                                   "(document g (:r (define/refinement p (#record r) true)))")
    , ("gap 50 (fixed): satisfies is false when an assertion inside the refinement fails", fixedGap $ compilesToJSON "(define/refinement labelled (#record r) (eq #string (get r label) \"x\")) (document g (:r (satisfies (:label 1) #labelled)))"
                                                                   "{\"r\": false}")
    , ("gap 51: decimal arithmetic is binary floating point", compilesToJSON "(document g (:a (+ 0.1 0.2) :b (* 3 1.1) :c (fold (3.72 3.36 3.40) 0 +)))"
                                                                   "{\"a\": 0.30000000000000004, \"b\": 3.3000000000000003, \"c\": 10.48}")
    , ("gap 53 (fixed): a #date before the year 1000 loses its leading zeros", fixedGap $ compilesToJSON "(document g (:d #date \"0079-08-24T12:00:00Z\"))"
                                                                   "{\"d\": \"79-08-24T12:00:00Z\"}")
    , ("gap 54 (fixed): validate keeps a timespan's date that is not one", fixedGap . withTests 1 . property $
                                                    fmap (T.isInfixOf "\"begin_of_the_begin\": \"hello\"" . TL.toStrict)
                                                         (validate "t.json" "{\"timespan\": {\"begin_of_the_begin\": \"hello\"}}" defaultOptions)
                                                    === Right True)
    , ("gap 55 (fixed): array is unbound",          fixedGap $ importsFailAt "SCP0000" ("inline-test", 1, 15) ["array"] []
                                                                   "(document g (:r (array 1 2)))")
    , ("gap 57 (fixed): a misplaced (∈ p) is an unbound %in", fixedGap $ importsFailAt "SCP0000" ("inline-test", 1, 17) ["%in"] []
                                                                   "(document g (:a (∈ number)))")
    , ("gap 59 (fixed): an array that fails a refinement is 'a array'", fixedGap $ failsWith "a array that is not non-empty"
                                                                   "(define/refinement non-empty (<xs ⊢ array>) (doc \"d\") (gt (len xs) 0)) (document g (:a <(make array) ⊢ non-empty>))")
    ]
