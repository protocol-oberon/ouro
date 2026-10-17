module Ouro.LetTest
( letAndFn
) where

import           Hedgehog
import           Utils    (compilesToJSON, compilesWithoutWarnings, failsWith,
                           warnsWith)


-- 'let' and 'fn' replace 'define' and a local 'defun', and 'defun' is now 'defn':
--
--   (fn (x y) expr)              an anonymous function value
--   (let ($name expr ...) expr)  private bindings, then one result expression. The
--                                bindings can refer to each other in any order (Scheme's
--                                letrec); a cycle is an error. '$' marks a binding that is
--                                never output, as ':' marks a key that is.
--   (defn name (x) expr)         a top-level function (was defun)
--   (graph name expr)            a graph is one expression, usually a Record
--
-- Every body (function, graph, let) is then exactly one expression. These tests compare
-- against JSON rather than another program, so they keep their meaning while the syntax
-- around them changes.
letAndFn :: Group
letAndFn = Group "Bindings: let, fn and defn"
    [ ("a graph is one expression",                compilesToJSON "(graph g (:a 1))"
                                                                  "{\"a\": 1}")
    , ("let binds values for one expression",      compilesToJSON "(graph g (:r (let ($a 1 $b (+ a 1)) (+ a b))))"
                                                                  "{\"r\": 3}")
    , ("let bindings can be in any order",         compilesToJSON "(graph g (:r (let ($b (+ a 1) $a 1) b)))"
                                                                  "{\"r\": 2}")
    , ("let keeps private values out of a Record", compilesToJSON "(graph g (let ($getty \"http://vocab.getty.edu/aat/\") (:id (+ getty \"300033618\"))))"
                                                                  "{\"id\": \"http://vocab.getty.edu/aat/300033618\"}")
    , ("let bindings shadow outer ones",           compilesToJSON "(graph g (:x \"outer\" :r (let ($x \"inner\") x)))"
                                                                  "{\"x\": \"outer\", \"r\": \"inner\"}")
    , ("a fn is a value",                          compilesToJSON "(graph g (:r (let ($inc (fn (n) (+ n 1))) (inc 41))))"
                                                                  "{\"r\": 42}")
    , ("a fn closes over its let",                 compilesToJSON "(graph g (:r (let ($base 10 $add (fn (n) (+ base n))) (add 5))))"
                                                                  "{\"r\": 15}")
    , ("a fn can be passed to a function",         compilesToJSON "(defn apply-to (f x) (f x)) (graph g (:r (apply-to (fn (n) (* n 2)) 21)))"
                                                                  "{\"r\": 42}")
    , ("a function body can use let",              compilesToJSON "(defn person (who) (let ($t \"Person\") (:type t :_label who))) (graph g (:p (person \"Manet\")))"
                                                                  "{\"p\": {\"type\": \"Person\", \"_label\": \"Manet\"}}")
    , ("a let in a function sees its parameters",  compilesToJSON "(defn label-of (who) (let ($full (+ who \" (painter)\")) (:_label full))) (graph g (:p (label-of \"Manet\")))"
                                                                  "{\"p\": {\"_label\": \"Manet (painter)\"}}")
    , ("a fn body can use let",                    compilesToJSON "(graph g (:r (let ($twice (fn (n) (let ($m (* n 2)) m))) (twice 21))))"
                                                                  "{\"r\": 42}")
    , ("a let can be an attribute value in a function's record", compilesToJSON "(defn node (id) (:id id :part (let ($sub (+ id \"/1\")) (:id sub)))) (graph g (:r (node \"x\")))"
                                                                  "{\"r\": {\"id\": \"x\", \"part\": {\"id\": \"x/1\"}}}")
    , ("a returned fn keeps its function's let",   compilesToJSON "(defn adder (base) (let ($add (fn (n) (+ base n))) add)) (graph g (:r (let ($add10 (adder 10)) (add10 5))))"
                                                                  "{\"r\": 15}")
    , ("a function body is one let, not several",  failsWith "more than one result expression"
                                                             "(defn f (x) (let ($a 1) a) (let ($b 2) b)) (graph g (:r (f 1)))")

    -- A result is written as it would be anywhere else, so one value in parentheses is
    -- an array of that value. It compiles, with a warning.
    , ("a parenthesised result is an array",       compilesToJSON "(defn f () (\"hello\")) (graph g (:r (f)))"
                                                                  "{\"r\": [\"hello\"]}")
    , ("a defn result in parentheses warns",       warnsWith "The result of 'f' is an array of one value"
                                                             "(defn f () (\"hello\")) (graph g (:r (f)))")
    , ("a fn result in parentheses warns",         warnsWith "Perhaps remove the parentheses: 1"
                                                             "(graph g (let ($f (fn () (1))) (:r (f))))")
    , ("a bare result does not warn",              compilesWithoutWarnings "(defn f () \"hello\") (graph g (:r (let ($h (fn () true)) (h)) :s (f)))")
    , ("a record or call result does not warn",    compilesWithoutWarnings "(defn one () 1) (defn f () (:a (one))) (graph g (:r (f) :s ((one))))")
    , ("an array of several values does not warn", compilesWithoutWarnings "(defn f () (1 2)) (graph g (:r (f)))")
    , ("a parenthesised attribute value does not warn", compilesWithoutWarnings "(graph g (:r (\"hello\")))")

    -- Errors, checked by message
    , ("a cycle in let is an error",               failsWith "references itself"
                                                             "(graph g (:r (let ($a b $b a) a)))")
    , ("a let body is one expression",             failsWith "exactly one"
                                                             "(graph g (:r (let ($a 1) a a)))")
    , ("a spliced graph is an error",              failsWith "one expression"
                                                             "(graph g :a 1)")
    , ("define has been replaced by let",          failsWith "replaced by let"
                                                             "(graph g (:a 1 (define :b 2)))")
    , ("a define as a value is an error",          failsWith "replaced by let"
                                                             "(graph g (:a 1 :b (define :c 2)))")
    , ("a name may start with defn",               compilesToJSON "(defn defnode (defnx) defnx) (graph g (:r (defnode 1)))"
                                                                  "{\"r\": 1}")
    , ("defun has been renamed defn",              failsWith "renamed defn"
                                                             "(defun f () 1) (graph g (:r (f)))")
    , ("a local defn is replaced by fn",           failsWith "(fn "
                                                             "(defn outer () (defn inner () 1) (inner)) (graph g (:r (outer)))")

    -- '$' marks a private binding, ':' a key in the output
    , ("a $ name can be any symbol",               compilesToJSON "(graph g (let ($ok? true $count+1 2) (:r ok? :s count+1)))"
                                                                  "{\"r\": true, \"s\": 2}")
    , ("a $ binding can hold a record",            compilesToJSON "(graph g (let ($node (:type \"Type\" :_label \"Painting\")) (:r node)))"
                                                                  "{\"r\": {\"type\": \"Type\", \"_label\": \"Painting\"}}")
    , ("list puts a bound record in an array",    compilesToJSON "(graph g (let ($m (:id \"manet\")) (:r (list m))))"
                                                                  "{\"r\": [{\"id\": \"manet\"}]}")
    , ("a key in a let is an error",               failsWith "marks a key in the output"
                                                             "(graph g (:r (let (:a 1) a)))")
    , ("a let binds only $ pairs",                 failsWith "A let binds private values"
                                                             "(graph g (let (1 2) 3))")
    , ("a $ binding in a record is an error",      failsWith "is a private binding"
                                                             "(graph g (:a 1 $b 2))")
    , ("a $ binding as a graph is an error",       failsWith "is a private binding"
                                                             "(graph g ($b 2))")
    , ("a $ binding in an array is an error",      failsWith "is a private binding"
                                                             "(graph g (:r (1 $b 2)))")
    , ("a $ binding as a value is an error",       failsWith "is a private binding"
                                                             "(graph g (:r $b 2))")

    -- '$' only defines: a reference is the bare name, and a stray '$name' never takes
    -- the expression after it
    , ("$name in a call is reported, not paired",  failsWith "write its name without '$': base"
                                                             "(graph g (let ($base \"https://a.org/\") (:r (+ $base \"x\"))))")
    , ("$name before a closing parenthesis",       failsWith "write its name without '$': base"
                                                             "(defn f (x) x) (graph g (let ($base 1) (:r (f $base))))")
    , ("$name as an attribute value",              failsWith "write its name without '$': base"
                                                             "(graph g (let ($base 1) (:r $base)))")
    , ("a $ name needs a value",                   failsWith "has no value"
                                                             "(graph g (let ($a 1 $b) a))")
    , ("a binding's value can desugar",            compilesToJSON "(graph g (let ($end (+ #date \"2020-01-01T00:00:00Z\" (thru (days 1)))) (:r end)))"
                                                                  "{\"r\": \"2020-01-02T23:59:59Z\"}")
    , ("a binding's value can be typed",           compilesToJSON "(graph g (let ($base #uri \"https://a.org/b\") (:r base)))"
                                                                  "{\"r\": \"https://a.org/b\"}")

    -- A binding never sees itself: inside its own expression, its name refers outward
    , ("an attribute pair outputs the binding of its name", compilesToJSON "(graph g (let ($label \"Spring\") (:label label)))"
                                                                  "{\"label\": \"Spring\"}")
    , ("a template outputs its parameters by name", compilesToJSON "(defn node (id type) (:id id :type type)) (graph g (:r (node \"1\" \"Type\")))"
                                                                  "{\"r\": {\"id\": \"1\", \"type\": \"Type\"}}")
    , ("a nested attribute pair sees the outer one", compilesToJSON "(graph g (:label \"outer\" :inner (:label label)))"
                                                                  "{\"label\": \"outer\", \"inner\": {\"label\": \"outer\"}}")
    , ("a let binding sees the outer one",         compilesToJSON "(graph g (let ($x 1) (:r (let ($x (+ x 1)) x))))"
                                                                  "{\"r\": 2}")
    , ("with nothing outside, the name is unbound", failsWith "ScopeError"
                                                             "(graph g (:a a))")
    , ("siblings still see each other",            compilesToJSON "(graph g (:a b :b 1))"
                                                                  "{\"a\": 1, \"b\": 1}")
    , ("siblings in a cycle are still an error",   failsWith "references itself"
                                                             "(graph g (:a b :b a))")
    , ("an argument is the caller's binding",      compilesToJSON "(defn f (x) (:x x)) (graph g (let ($x \"caller\") (:r (f x))))"
                                                                  "{\"r\": {\"x\": \"caller\"}}")
    ]
