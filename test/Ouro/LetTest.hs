module Ouro.LetTest
( letAndFn
) where

import           Hedgehog
import           Utils    (compilesToJSON, compilesWithoutWarnings, failsWith,
                           warnsWith)


-- 'let' and 'fn' replaced 'define' and a local 'defun', and 'defun' became 'defn'; 'define'
-- and 'defun' are now ordinary names:
--
--   (fn (x y) expr)              an anonymous function value
--   (let ($name expr ...) expr)  private bindings, then one result expression. The
--                                bindings can refer to each other in any order (Scheme's
--                                letrec); a cycle is an error. '$' marks a binding that is
--                                never output, as ':' marks a key that is.
--   (define/procedure name (x) expr)         a top-level function (was defun)
--   (document name expr)            a graph is one expression, usually a Record
--
-- Every body (function, graph, let) is then exactly one expression. These tests compare
-- against JSON rather than another program, so they keep their meaning while the syntax
-- around them changes.
letAndFn :: Group
letAndFn = Group "Bindings: let, fn and defn"
    [ ("a graph is one expression",                compilesToJSON "(document g (:a 1))"
                                                                  "{\"a\": 1}")
    , ("let binds values for one expression",      compilesToJSON "(document g (:r (let ($a 1 $b (+ a 1)) (+ a b))))"
                                                                  "{\"r\": 3}")
    , ("let bindings can be in any order",         compilesToJSON "(document g (:r (let ($b (+ a 1) $a 1) b)))"
                                                                  "{\"r\": 2}")
    , ("let keeps private values out of a Record", compilesToJSON "(document g (let ($getty \"http://vocab.getty.edu/aat/\") (:id (+ getty \"300033618\"))))"
                                                                  "{\"id\": \"http://vocab.getty.edu/aat/300033618\"}")
    , ("let bindings shadow outer ones",           compilesToJSON "(document g (:x \"outer\" :r (let ($x \"inner\") x)))"
                                                                  "{\"x\": \"outer\", \"r\": \"inner\"}")
    , ("a fn is a value",                          compilesToJSON "(document g (:r (let ($inc (fn (n) (+ n 1))) (inc 41))))"
                                                                  "{\"r\": 42}")
    , ("a fn closes over its let",                 compilesToJSON "(document g (:r (let ($base 10 $add (fn (n) (+ base n))) (add 5))))"
                                                                  "{\"r\": 15}")
    , ("a fn can be passed to a function",         compilesToJSON "(define/procedure apply-to (f x) (f x)) (document g (:r (apply-to (fn (n) (* n 2)) 21)))"
                                                                  "{\"r\": 42}")
    , ("a function body can use let",              compilesToJSON "(define/procedure person (who) (let ($t \"Person\") (:type t :_label who))) (document g (:p (person \"Manet\")))"
                                                                  "{\"p\": {\"type\": \"Person\", \"_label\": \"Manet\"}}")
    , ("a let in a function sees its parameters",  compilesToJSON "(define/procedure label-of (who) (let ($full (+ who \" (painter)\")) (:_label full))) (document g (:p (label-of \"Manet\")))"
                                                                  "{\"p\": {\"_label\": \"Manet (painter)\"}}")
    , ("a fn body can use let",                    compilesToJSON "(document g (:r (let ($twice (fn (n) (let ($m (* n 2)) m))) (twice 21))))"
                                                                  "{\"r\": 42}")
    , ("a let can be an attribute value in a function's record", compilesToJSON "(define/procedure node (id) (:id id :part (let ($sub (+ id \"/1\")) (:id sub)))) (document g (:r (node \"x\")))"
                                                                  "{\"r\": {\"id\": \"x\", \"part\": {\"id\": \"x/1\"}}}")
    , ("a returned fn keeps its function's let",   compilesToJSON "(define/procedure adder (base) (let ($add (fn (n) (+ base n))) add)) (document g (:r (let ($add10 (adder 10)) (add10 5))))"
                                                                  "{\"r\": 15}")
    , ("a function body is one let, not several",  failsWith "more than one result expression"
                                                             "(define/procedure f (x) (let ($a 1) a) (let ($b 2) b)) (document g (:r (f 1)))")

    -- A result is written as it would be anywhere else, so one value in parentheses is
    -- an array of that value. It compiles, with a warning.
    , ("a parenthesised result is an array",       compilesToJSON "(define/procedure f () (\"hello\")) (document g (:r (f)))"
                                                                  "{\"r\": [\"hello\"]}")
    , ("a defn result in parentheses warns",       warnsWith "The result of 'f' is an array of one value"
                                                             "(define/procedure f () (\"hello\")) (document g (:r (f)))")
    , ("a fn result in parentheses warns",         warnsWith "Perhaps remove the parentheses: 1"
                                                             "(document g (let ($f (fn () (1))) (:r (f))))")
    , ("a bare result does not warn",              compilesWithoutWarnings "(define/constant f \"hello\") (document g (:r (let ($h (fn () true)) (h)) :s (f)))")
    , ("a record or call result does not warn",    compilesWithoutWarnings "(define/constant one 1) (define/constant f (:a (one))) (document g (:r (f) :s ((one))))")
    , ("an array of several values does not warn", compilesWithoutWarnings "(define/constant f (1 2)) (document g (:r (f)))")
    , ("a parenthesised attribute value does not warn", compilesWithoutWarnings "(document g (:r (\"hello\")))")

    -- Errors, checked by message
    , ("a cycle in let is an error",               failsWith "references itself"
                                                             "(document g (:r (let ($a b $b a) a)))")
    , ("a let body is one expression",             failsWith "exactly one"
                                                             "(document g (:r (let ($a 1) a a)))")
    , ("a spliced graph is an error",              failsWith "one expression"
                                                             "(document g :a 1)")
    , ("define on its own is no name",             failsWith "'define' is not a definition"
                                                             "(document g (:a 1 :b (define :c 2)))")
    , ("so no function is named define",           failsWith "'define' is not a definition"
                                                             "(define/procedure define (x) x) (document g (:r (define 1)))")
    , ("a name may start with defn",               compilesToJSON "(define/procedure defnode (defnx) defnx) (document g (:r (defnode 1)))"
                                                                  "{\"r\": 1}")
    , ("a name may start with null",               compilesToJSON "(define/procedure null-result (x) x) (document g (:r (null-result 1)))"
                                                                  "{\"r\": 1}")
    , ("or with true or false",                    compilesToJSON "(define/procedure truer (x) x) (define/procedure falsey (x) x) (document g (:r (truer (falsey 1))))"
                                                                  "{\"r\": 1}")
    , ("a graph may start with null",              compilesToJSON "(document null-results (:r 2)) (document g (:r 1))"
                                                                  "{\"r\": 1}")
    , ("and so may a key, read where it is used",  compilesToJSON "(document g (:null-ish 1 :r null-ish))"
                                                                  "{\"null-ish\": 1, \"r\": 1}")
    , ("a literal still ends at a delimiter or a comment", compilesToJSON "(document g (:a (true false) :b true;c\n))"
                                                                  "{\"a\": [true, false], \"b\": true}")
    , ("defun is an ordinary name",                failsWith "'defun' cannot begin a form at the top level"
                                                             "(defun f () 1) (document g (:r (f)))")
    , ("a local defun is a call",                  failsWith "'defun' is not bound here"
                                                             "(define/procedure outer () (defun inner () 1)) (document g (:r (outer)))")
    , ("a function may be named defun",            compilesToJSON "(define/procedure defun (x) x) (document g (:r (defun 1)))"
                                                                  "{\"r\": 1}")
    , ("a local defn is replaced by fn",           failsWith "(fn "
                                                             "(define/procedure outer () (define/procedure inner () 1) (inner)) (document g (:r (outer)))")

    -- '$' marks a private binding, ':' a key in the output
    , ("a $ name can be any symbol",               compilesToJSON "(document g (let ($ok? true $count+1 2) (:r ok? :s count+1)))"
                                                                  "{\"r\": true, \"s\": 2}")
    , ("a $ binding can hold a record",            compilesToJSON "(document g (let ($node (:type \"Type\" :_label \"Painting\")) (:r node)))"
                                                                  "{\"r\": {\"type\": \"Type\", \"_label\": \"Painting\"}}")
    , ("array puts a bound record in an array",    compilesToJSON "(document g (let ($m (:id \"manet\")) (:r (array m))))"
                                                                  "{\"r\": [{\"id\": \"manet\"}]}")
    , ("a key in a let is an error",               failsWith "marks a key in the output"
                                                             "(document g (:r (let (:a 1) a)))")
    , ("a let binds only $ pairs",                 failsWith "A let binds private values"
                                                             "(document g (let (1 2) 3))")
    , ("a $ binding in a record is an error",      failsWith "is a private binding"
                                                             "(document g (:a 1 $b 2))")
    , ("a $ binding as a graph is an error",       failsWith "is a private binding"
                                                             "(document g ($b 2))")
    , ("a $ binding in an array is an error",      failsWith "is a private binding"
                                                             "(document g (:r (1 $b 2)))")
    , ("a $ binding as a value is an error",       failsWith "is a private binding"
                                                             "(document g (:r $b 2))")

    -- '$' only defines: a reference is the bare name, and a stray '$name' never takes
    -- the expression after it
    , ("$name in a call is reported, not paired",  failsWith "write its name without '$': base"
                                                             "(document g (let ($base \"https://a.org/\") (:r (+ $base \"x\"))))")
    , ("$name before a closing parenthesis",       failsWith "write its name without '$': base"
                                                             "(define/procedure f (x) x) (document g (let ($base 1) (:r (f $base))))")
    , ("$name as an attribute value",              failsWith "write its name without '$': base"
                                                             "(document g (let ($base 1) (:r $base)))")
    , ("a $ name needs a value",                   failsWith "has no value"
                                                             "(document g (let ($a 1 $b) a))")
    , ("a binding's value can desugar",            compilesToJSON "(document g (let ($end (+ #date \"2020-01-01T00:00:00Z\" (thru (days 1)))) (:r end)))"
                                                                  "{\"r\": \"2020-01-02T23:59:59Z\"}")
    , ("a binding's value can be typed",           compilesToJSON "(document g (let ($base #uri \"https://a.org/b\") (:r base)))"
                                                                  "{\"r\": \"https://a.org/b\"}")

    -- A binding never sees itself: inside its own expression, its name refers outward
    , ("an attribute pair outputs the binding of its name", compilesToJSON "(document g (let ($label \"Spring\") (:label label)))"
                                                                  "{\"label\": \"Spring\"}")
    , ("a template outputs its parameters by name", compilesToJSON "(define/procedure node (id type) (:id id :type type)) (document g (:r (node \"1\" \"Type\")))"
                                                                  "{\"r\": {\"id\": \"1\", \"type\": \"Type\"}}")
    , ("a nested attribute pair sees the outer one", compilesToJSON "(document g (:label \"outer\" :inner (:label label)))"
                                                                  "{\"label\": \"outer\", \"inner\": {\"label\": \"outer\"}}")
    , ("a let binding sees the outer one",         compilesToJSON "(document g (let ($x 1) (:r (let ($x (+ x 1)) x))))"
                                                                  "{\"r\": 2}")
    , ("with nothing outside, the name is unbound", failsWith "ScopeError"
                                                             "(document g (:a a))")
    , ("siblings still see each other",            compilesToJSON "(document g (:a b :b 1))"
                                                                  "{\"a\": 1, \"b\": 1}")
    , ("siblings in a cycle are still an error",   failsWith "references itself"
                                                             "(document g (:a b :b a))")
    , ("an argument is the caller's binding",      compilesToJSON "(define/procedure f (x) (:x x)) (document g (let ($x \"caller\") (:r (f x))))"
                                                                  "{\"r\": {\"x\": \"caller\"}}")
    ]
