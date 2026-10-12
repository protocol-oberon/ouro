module Ouro.FunctionTest
( expressionBodies
, plainNames
) where

import qualified Data.Text as T
import           Hedgehog
import           Utils     (compilesLike, failsToCompile, failsWith)


-- A function body is any number of defines followed by exactly one expression: its
-- result, written exactly as it would be anywhere else.
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
                                                                 , "  (let (:next (+ x 1))"
                                                                 , "    (:value next)))"
                                                                 , "(graph g (:r (node! 1)))" ])
                                                               "(graph g (:r (:value 2)))")
    , ("a let around a primitive",               compilesLike "(defn twice-next! (x) (let (:next (+ x 1)) (* next 2))) (graph g (:r (twice-next! 1)))"
                                                               "(graph g (:r 4))")
    , ("a let-bound fn in the body",             compilesLike "(defn outer! () (let (:inner (fn () 7)) (inner))) (graph g (:r (outer!)))"
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
    , ("a local function with a plain name",       compilesLike "(defn outer () (let (:inner (fn () 7)) (inner))) (graph g (:r (outer)))"
                                                                "(graph g (:r 7))")
    , ("functions passed as values",               compilesLike "(defn apply-to (f x) (f x)) (defn inc (n) (+ n 1)) (graph g (:r (apply-to inc 1)))"
                                                                "(graph g (:r 2))")
    , ("an inner value shadows an outer function", compilesLike "(defn label () \"function\") (graph g (:label \"value\" :r label))"
                                                                "(graph g (:label \"value\" :r \"value\"))")

    -- Compiles today, and must keep compiling
    , ("a trailing ! is still allowed",            compilesLike "(defn legacy! () 1) (graph g (:r (legacy!)))"
                                                                "(graph g (:r 1))")

    -- Checked by message: today every program here already fails, but for another reason
    , ("a let value shadows a top-level function", compilesLike "(defn label () \"function\") (graph g (let (:label \"value\") (:r label)))"
                                                                "(graph g (:r \"value\"))")
    , ("a function defined twice",                 failsWith "defined more than once"
                                                             "(defn f () 1) (defn f () 2) (graph g (:r (f)))")
    , ("a function named after a special form",    failsWith "special form"
                                                             "(defn get (x) x) (graph g (:r 1))")
    , ("calling a value that shadows a function",  failsWith "not a function"
                                                             "(defn label () \"function\") (graph g (:label \"value\" :r (label)))")
    ]
