module Ouro.LetTest
( letAndFn
) where

import           Hedgehog
import           Utils    (compilesToJSON, failsWith)


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
    ]
