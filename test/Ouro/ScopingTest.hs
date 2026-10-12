module Ouro.ScopingTest
( baseline
, valuePatterns
, lexicalRecords
, callSiteArguments
, lexicalFunctions
, valueGetAndErrors
, cleanup
) where

import           Control.Exception (evaluate)
import           Data.Ouro         (CompilationResult (..), compile,
                                    defaultOptions, toJSON)
import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.Lazy    as TL
import           Hedgehog
import qualified Hedgehog.Gen      as Gen
import qualified Hedgehog.Range    as Range
import           System.Timeout    (timeout)


-- Each step of the scoping redesign registers its group in Main.hs once it lands.
-- 'baseline' holds behaviour that already works and must keep working.

baseline :: Group
baseline = Group "Scoping: baseline (must keep passing)"
    [ ("forward references within a record",      compilesLike "(graph g :a b :b 1)"
                                                               "(graph g :a 1 :b 1)")
    , ("nested records see outer bindings",       compilesLike "(graph g :x 1 :inner (:y x))"
                                                               "(graph g :x 1 :inner (:y 1))")
    , ("function defines shadow without leaking", compilesLike (T.unlines
                                                                 [ "(defun sandbox! () (define :env \"dev\") :fenv env)"
                                                                 , "(graph g (define :env \"prod\") :genv env (inlay (sandbox!)))" ])
                                                               "(graph g :genv \"prod\" :fenv \"dev\")")
    , ("functions are first-class values",        compilesLike (T.unlines
                                                                 [ "(defun mk! () :v 1)"
                                                                 , "(defun call! (fn) (inlay (fn)) :d 2)"
                                                                 , "(graph g :r (call! mk!))" ])
                                                               "(graph g :r (:v 1 :d 2))")
    , ("arguments from a local call-site scope",  compilesLike "(defun f! (a) :out a) (graph g :rec (:loc \"local\" :res (f! loc)))"
                                                               "(graph g :rec (:loc \"local\" :res (:out \"local\")))")
    , ("functions calling functions",             compilesLike "(defun inner! (b) (return b)) (defun outer! (a) (return (inner! a))) (graph g :r (outer! 7))"
                                                               "(graph g :r 7)")
    , ("unused arguments are not forced early",   compilesLike "(defun k! (a b) (return a)) (graph g :a (k! 1 b) :b a)"
                                                               "(graph g :a 1 :b 1)")
    , ("case alternatives sugar",                 compilesLike "(graph g :n \"Proust\" :r (case n ((\"Manet\" \"Proust\") \"person\") (otherwise \"other\")))"
                                                               "(graph g :n \"Proust\" :r \"person\")")
    , ("case guards",                             compilesLike "(graph g :n 150 :r (case n ((> 100) \"big\") (otherwise \"small\")))"
                                                               "(graph g :n 150 :r \"big\")")
    , ("cyclic bindings are an error",            failsToCompile "(graph g :a b :b a)")
    , ("wrong arity is an error",                 failsToCompile "(defun id! (a) (return a)) (graph g :r (id! 1 2))")
    , ("direct recursion is an error",            failsToCompile "(defun f! (n) (return (f! n))) (graph g :r (f! 1))")
    , ("defines stay private to get",             failsToCompile "(graph g :rec (:a 1 (define :hidden 5)) :r (get rec hidden))")
    , ("local functions shadow top-level ones",   compilesLike (T.unlines
                                                                 [ "(defun helper! () (return \"top\"))"
                                                                 , "(defun caller! () (defun helper! () (return \"local\")) :r (helper!))"
                                                                 , "(graph g :res (caller!))" ])
                                                               "(graph g :res (:r \"local\"))")
    , ("an expression can be replaced by its value", prop_substitution)
    ]

lexicalRecords :: Group
lexicalRecords = Group "Scoping: lexical records, shared bindings"
    [ ("bindings evaluate in their own scope",    compilesLike "(graph g :x \"outer\" :y x :inner (:x \"inner\" :z y))"
                                                               "(graph g :x \"outer\" :y \"outer\" :inner (:x \"inner\" :z \"outer\"))")
    , ("each binding is evaluated once",          prop_sharedChain)
    ]

callSiteArguments :: Group
callSiteArguments = Group "Scoping: arguments evaluate at the call site"
    [ ("callee defines do not capture arguments", compilesLike (T.unlines
                                                                 [ "(defun f! (a) (define :x \"inside\") :out a)"
                                                                 , "(graph g :x \"outside\" :res (f! x))" ])
                                                               "(graph g :x \"outside\" :res (:out \"outside\"))")
    , ("callee parameters do not capture arguments", compilesLike "(defun f! (a b) (return b)) (graph g :a \"caller\" :r (f! 1 a))"
                                                               "(graph g :a \"caller\" :r \"caller\")")
    , ("nesting a call is not recursion",         compilesLike "(defun id! (a) (return a)) (graph g :r (id! (id! 1)))"
                                                               "(graph g :r 1)")
    ]

lexicalFunctions :: Group
lexicalFunctions = Group "Scoping: functions resolve names where they are defined"
    [ ("a function uses the helper defined next to it", compilesLike (T.unlines
                                                                 [ "(defun helper! () (return \"defined next to uses-helper!\"))"
                                                                 , "(defun uses-helper! () (return (helper!)))"
                                                                 , "(defun caller! () (defun helper! () (return \"caller's own helper\")) :r (uses-helper!))"
                                                                 , "(graph g :res (caller!))" ])
                                                               "(graph g :res (:r \"defined next to uses-helper!\"))")
    ]

valuePatterns :: Group
valuePatterns = Group "Scoping: value patterns, no quoting"
    [ ("array head and rest",                     compilesLike "(graph g :t (1 2 3) :r (case t ((?head ?..rest) rest) (otherwise t)))"
                                                               "(graph g :t (1 2 3) :r (2 3))")
    , ("patterns see values, not code",           compilesLike "(graph g :t ((+ 2 4) 3) :r (case t ((6 ?x) x) (otherwise 0)))"
                                                               "(graph g :t (6 3) :r 3)")
    , ("nested literal arrays match exactly",     compilesLike "(graph g :t ((1 2) 3) :r (case t (((1 2) ?x) x) (otherwise 0)))"
                                                               "(graph g :t ((1 2) 3) :r 3)")
    , ("record patterns match by key",            compilesLike (person "(case p ((:type \"Person\" :_label ?l) l) (otherwise \"none\"))")
                                                               (personWith "\"Manet\""))
    , ("record patterns are open",                compilesLike (person "(case p ((:type \"Person\") \"person\") (otherwise \"none\"))")
                                                               (personWith "\"person\""))
    , ("record rest binds unmentioned fields",    compilesLike (person "(case p ((:type ? ?..rest) rest) (otherwise p))")
                                                               (personWith "(:_label \"Manet\" :id \"https://linked.art/example/person/manet\")"))
    , ("symbols in patterns compare by value",    compilesLike "(graph g :pt \"Person\" :p (:type \"Person\") :r (case p ((:type pt) \"yes\") (otherwise \"no\")))"
                                                               "(graph g :pt \"Person\" :p (:type \"Person\") :r \"yes\")")
    , ("patterns work on function results",       compilesLike "(defun mk! (n) :a n) (graph g :r (case (mk! 1) ((:a ?v) v) (otherwise 0)))"
                                                               "(graph g :r 1)")
    -- These compile today; each must fail once quoting is removed
    , ("quote is no longer a special form",       failsToCompile "(graph g :a (eval (quote (+ 1 2))))")
    , ("quoting a binding is no longer possible", failsToCompile "(graph g :a (eval (quote b)) :b 1)")
    , ("the quote prefix is no longer syntax",    failsToCompile "(graph g :a (eval '(+ 1 2)))")
    ]

valueGetAndErrors :: Group
valueGetAndErrors = Group "Scoping: value-based get, no hidden errors"
    [ ("get through a function result",           compilesLike "(defun mk! (n) :a n) (graph g :rec (mk! 1) :r (get rec a))"
                                                               "(graph g :rec (:a 1) :r 1)")
    , ("get through a case result",               compilesLike "(graph g :f 1 :rec (case f (1 (:a \"one\")) (otherwise (:a \"other\"))) :r (get rec a))"
                                                               "(graph g :f 1 :rec (:a \"one\") :r \"one\")")
    , ("get through an inlay",                    compilesLike "(defun mk! (n) :a n) (graph g :rec (:inner (:x 0 (inlay (mk! 1)))) :r (get rec inner a))"
                                                               "(graph g :rec (:inner (:x 0 :a 1)) :r 1)")
    , ("errors in unused defines are reported",   failsToCompile "(graph g (define :unused (/ 1 0)) :a 1)")
    , ("errors in unused arguments are reported", failsToCompile "(defun k! (a b) (return a)) (graph g :a (k! 1 (/ 1 0)))")
    ]

cleanup :: Group
cleanup = Group "Scoping: cleanup"
    [ ("comparing functions is a type error",     failsToCompile "(defun id! (a) (return a)) (graph g :s (eq id! id!))")
    ]


-- Substitution: replacing an arithmetic expression with its value never changes the output
prop_substitution :: Property
prop_substitution = property $ do
    (src, value) <- forAll genArith
    compiledLike ("(graph g :a " <> src <> ")") ("(graph g :a " <> tshow value <> ")")

    where
    genArith :: Gen (Text, Integer)
    genArith = Gen.recursive Gen.choice
                   [ (\n -> (tshow n, n)) <$> Gen.integral (Range.linear (-10) 10) ]
                   [ Gen.subterm2 genArith genArith (binOp "+" (+))
                   , Gen.subterm2 genArith genArith (binOp "-" (-))
                   , Gen.subterm2 genArith genArith (binOp "*" (*))
                   ]

    binOp op f (s1, v1) (s2, v2) = ("(" <> op <> " " <> s1 <> " " <> s2 <> ")", f v1 v2)

-- A chain where every binding uses the previous one twice: 2^40 evaluations unless shared
prop_sharedChain :: Property
prop_sharedChain = withTests 1 . property $ do
    let n      = 40 :: Int
        link i = ":a" <> tshow i <> " (+ a" <> tshow (i - 1) <> " a" <> tshow (i - 1) <> ") "
        src    = "(graph g :a0 1 " <> T.concat (map link [1 .. n]) <> ")"

    result <- evalIO . timeout 5000000 $
                  case compileG src of
                      CompilationSuccess _ ast -> Just <$> evaluate (TL.length (toJSON defaultOptions ast))
                      CompilationFailure _ _   -> pure Nothing

    case result of
        Nothing       -> annotate "Timed out: bindings are being re-evaluated on every reference" >> failure
        Just Nothing  -> annotate "The chain failed to compile" >> failure
        Just (Just _) -> success


-- Helpers
compileG :: Text -> CompilationResult
compileG = compile "scoping-test" "g"

-- Both programs compile, to identical output
compilesLike :: Text -> Text -> Property
compilesLike src expected = withTests 1 . property $ compiledLike src expected

compiledLike :: Text -> Text -> PropertyT IO ()
compiledLike src expected =
    case (compileG src, compileG expected) of
        (CompilationSuccess _ actual, CompilationSuccess _ wanted) -> actual === wanted
        (actual, wanted) -> do
                            annotate ("program:  " <> describe actual)
                            annotate ("expected: " <> describe wanted)
                            failure

failsToCompile :: Text -> Property
failsToCompile src = withTests 1 . property $
    case compileG src of
        CompilationFailure _ _ -> success
        CompilationSuccess _ ast -> do
                                    annotate ("compiled to: " <> TL.unpack (toJSON defaultOptions ast))
                                    failure

describe :: CompilationResult -> String
describe = \case
    CompilationSuccess _ ast  -> "compiled to " <> TL.unpack (toJSON defaultOptions ast)
    CompilationFailure _ errs -> "failed with " <> show errs

-- A Manet record bound to 'p', with 'r' computed by the given expression
person :: Text -> Text
person rExpr = "(graph g :p " <> manet <> " :r " <> rExpr <> ")"

personWith :: Text -> Text
personWith rValue = "(graph g :p " <> manet <> " :r " <> rValue <> ")"

manet :: Text
manet = "(:id \"https://linked.art/example/person/manet\" :type \"Person\" :_label \"Manet\")"

tshow :: Show a => a -> Text
tshow = T.pack . show
