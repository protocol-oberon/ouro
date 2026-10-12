module Ouro.FunctionBodyTest
( expressionBodies
) where

import qualified Data.Text as T
import           Hedgehog
import           Utils     (compilesLike, failsToCompile)


-- A function body is any number of defines followed by exactly one expression: its
-- result, written exactly as it would be anywhere else. Registered in Main.hs once
-- the change lands.
expressionBodies :: Group
expressionBodies = Group "Functions: a body is one result expression"
    [ ("a literal result",                        compilesLike "(defun getty! () \"http://vocab.getty.edu/aat/\") (graph g :u (getty!))"
                                                               "(graph g :u \"http://vocab.getty.edu/aat/\")")
    , ("a call's result",                         compilesLike "(defun add-one! (n) (+ 1 n)) (graph g :r (add-one! 1))"
                                                               "(graph g :r 2)")
    , ("an array of one",                         compilesLike "(defun one! () (\"lit\")) (graph g :r (one!))"
                                                               "(graph g :r (\"lit\"))")
    , ("an array",                                compilesLike "(defun ids! () (1 2 3)) (graph g :r (ids!))"
                                                               "(graph g :r (1 2 3))")
    , ("a record",                                compilesLike "(defun person! (who) (:type \"Person\" :_label who)) (graph g :r (person! \"Manet\"))"
                                                               "(graph g :r (:type \"Person\" :_label \"Manet\"))")
    , ("defines before a record",                 compilesLike (T.unlines
                                                                 [ "(defun node! (x)"
                                                                 , "  (define :next (+ x 1))"
                                                                 , "  (:value next))"
                                                                 , "(graph g :r (node! 1))" ])
                                                               "(graph g :r (:value 2))")
    , ("defines before a primitive",              compilesLike "(defun twice-next! (x) (define :next (+ x 1)) (* next 2)) (graph g :r (twice-next! 1))"
                                                               "(graph g :r 4)")
    , ("a local function in the body",            compilesLike "(defun outer! () (defun inner! () 7) (inner!)) (graph g :r (outer!))"
                                                               "(graph g :r 7)")

    -- These compile today; each must fail once bodies are single expressions
    , ("a spliced record body is an error",       failsToCompile "(defun person! (who) :type \"Person\" :_label who) (graph g :r (person! \"Manet\"))")
    , ("return is no longer a special form",      failsToCompile "(defun id! (x) (return x)) (graph g :r (id! 1))")
    , ("two result expressions are an error",     failsToCompile "(defun two! () 1 2) (graph g :r (two!))")
    , ("a body of only defines is an error",      failsToCompile "(defun none! () (define :a 1)) (graph g :r (none!))")
    ]
