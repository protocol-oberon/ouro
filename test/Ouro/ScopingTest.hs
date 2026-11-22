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
import           Data.Ouro         (CompilationResult (..), defaultOptions,
                                    toJSON)
import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.Lazy    as TL
import           Hedgehog
import qualified Hedgehog.Gen      as Gen
import qualified Hedgehog.Range    as Range
import           System.Timeout    (timeout)
import           Utils             (compileG, compiledLike, compilesLike,
                                    failsToCompile, tshow)


-- Each step of the scoping redesign registers its group in Main.hs once it lands.
-- 'baseline' holds behaviour that already works and must keep working.

baseline :: Group
baseline = Group "Scoping: baseline (must keep passing)"
    [ ("forward references within a record",      compilesLike "(document g (:a b :b 1))"
                                                               "(document g (:a 1 :b 1))")
    , ("nested records see outer bindings",       compilesLike "(document g (:x 1 :inner (:y x)))"
                                                               "(document g (:x 1 :inner (:y 1)))")
    , ("function defines shadow without leaking", compilesLike (T.unlines
                                                                 [ "(define/procedure sandbox! () (let ($env \"dev\") (:fenv env)))"
                                                                 , "(document g (let ($env \"prod\") (:genv env (inlay (sandbox!)))))" ])
                                                               "(document g (:genv \"prod\" :fenv \"dev\"))")
    , ("functions are first-class values",        compilesLike (T.unlines
                                                                 [ "(define/procedure mk! (v) (:v v))"
                                                                 , "(define/procedure call! (make) ((inlay (make 1)) :d 2))"
                                                                 , "(document g (:r (call! mk!)))" ])
                                                               "(document g (:r (:v 1 :d 2)))")
    , ("arguments from a local call-site scope",  compilesLike "(define/procedure f! (a) (:out a)) (document g (:rec (:loc \"local\" :res (f! loc))))"
                                                               "(document g (:rec (:loc \"local\" :res (:out \"local\"))))")
    , ("functions calling functions",             compilesLike "(define/procedure inner! (b) b) (define/procedure outer! (a) (inner! a)) (document g (:r (outer! 7)))"
                                                               "(document g (:r 7))")
    , ("unused arguments are not forced early",   compilesLike "(define/procedure k! (a b) a) (document g (:a (k! 1 b) :b a))"
                                                               "(document g (:a 1 :b 1))")
    , ("case alternatives sugar",                 compilesLike "(document g (:n \"Proust\" :r (case n ((\"Manet\" \"Proust\") \"person\") (otherwise \"other\"))))"
                                                               "(document g (:n \"Proust\" :r \"person\"))")
    , ("case guards",                             compilesLike "(document g (:n 150 :r (case n ((gt 100) \"big\") (otherwise \"small\"))))"
                                                               "(document g (:n 150 :r \"big\"))")
    , ("cyclic bindings are an error",            failsToCompile "(document g (:a b :b a))")
    , ("wrong arity is an error",                 failsToCompile "(define/procedure id! (a) a) (document g (:r (id! 1 2)))")
    , ("direct recursion is an error",            failsToCompile "(define/procedure f! (n) (f! n)) (document g (:r (f! 1)))")
    , ("let values stay private to get",          failsToCompile "(document g (:rec (let ($hidden 5) (:a 1)) :r (get rec hidden)))")
    , ("local functions shadow top-level ones",   compilesLike (T.unlines
                                                                 [ "(define/procedure helper () \"top\")"
                                                                 , "(define/procedure caller! () (let ($helper (fn () \"local\")) (:r (helper))))"
                                                                 , "(document g (:res (caller!)))" ])
                                                               "(document g (:res (:r \"local\")))")
    , ("an expression can be replaced by its value", prop_substitution)
    ]

lexicalRecords :: Group
lexicalRecords = Group "Scoping: lexical records, shared bindings"
    [ ("bindings evaluate in their own scope",    compilesLike "(document g (:x \"outer\" :y x :inner (:x \"inner\" :z y)))"
                                                               "(document g (:x \"outer\" :y \"outer\" :inner (:x \"inner\" :z \"outer\")))")
    , ("each binding is evaluated once",          prop_sharedChain)
    ]

callSiteArguments :: Group
callSiteArguments = Group "Scoping: arguments evaluate at the call site"
    [ ("callee defines do not capture arguments", compilesLike (T.unlines
                                                                 [ "(define/procedure f! (a) (let ($x \"inside\") (:out a)))"
                                                                 , "(document g (:x \"outside\" :res (f! x)))" ])
                                                               "(document g (:x \"outside\" :res (:out \"outside\")))")
    , ("callee parameters do not capture arguments", compilesLike "(define/procedure f! (a b) b) (document g (:a \"caller\" :r (f! 1 a)))"
                                                               "(document g (:a \"caller\" :r \"caller\"))")
    , ("nesting a call is not recursion",         compilesLike "(define/procedure id! (a) a) (document g (:r (id! (id! 1))))"
                                                               "(document g (:r 1))")
    ]

lexicalFunctions :: Group
lexicalFunctions = Group "Scoping: functions resolve names where they are defined"
    [ ("a function uses the helper defined next to it", compilesLike (T.unlines
                                                                 [ "(define/procedure helper () \"defined next to uses-helper!\")"
                                                                 , "(define/procedure uses-helper! () (helper))"
                                                                 , "(define/procedure caller! () (let ($helper (fn () \"caller's own helper\")) (:r (uses-helper!))))"
                                                                 , "(document g (:res (caller!)))" ])
                                                               "(document g (:res (:r \"defined next to uses-helper!\")))")
    ]

valuePatterns :: Group
valuePatterns = Group "Scoping: value patterns, no quoting"
    [ ("array head and rest",                     compilesLike "(document g (:t (1 2 3) :r (case t ((?head ?..rest) rest) (otherwise t))))"
                                                               "(document g (:t (1 2 3) :r (2 3)))")
    , ("patterns see values, not code",           compilesLike "(document g (:t ((+ 2 4) 3) :r (case t ((6 ?x) x) (otherwise 0))))"
                                                               "(document g (:t (6 3) :r 3))")
    , ("nested literal arrays match exactly",     compilesLike "(document g (:t ((1 2) 3) :r (case t (((1 2) ?x) x) (otherwise 0))))"
                                                               "(document g (:t ((1 2) 3) :r 3))")
    , ("record patterns match by key",            compilesLike (person "(case p ((:type \"Person\" :_label ?l) l) (otherwise \"none\"))")
                                                               (personWith "\"Manet\""))
    , ("record patterns are open",                compilesLike (person "(case p ((:type \"Person\") \"person\") (otherwise \"none\"))")
                                                               (personWith "\"person\""))
    , ("record rest binds unmentioned attribute pairs", compilesLike (person "(case p ((:type ? ?..rest) rest) (otherwise p))")
                                                               (personWith "(:_label \"Manet\" :id \"https://linked.art/example/person/manet\")"))
    , ("symbols in patterns compare by value",    compilesLike "(document g (:pt \"Person\" :p (:type \"Person\") :r (case p ((:type pt) \"yes\") (otherwise \"no\"))))"
                                                               "(document g (:pt \"Person\" :p (:type \"Person\") :r \"yes\"))")
    , ("patterns work on function results",       compilesLike "(define/procedure mk! (n) (:a n)) (document g (:r (case (mk! 1) ((:a ?v) v) (otherwise 0))))"
                                                               "(document g (:r 1))")
    -- These compile today; each must fail once quoting is removed
    , ("quote is no longer a special form",       failsToCompile "(document g (:a (eval (quote (+ 1 2)))))")
    , ("quoting a binding is no longer possible", failsToCompile "(document g (:a (eval (quote b)) :b 1))")
    , ("the quote prefix is no longer syntax",    failsToCompile "(document g (:a (eval '(+ 1 2))))")
    ]

valueGetAndErrors :: Group
valueGetAndErrors = Group "Scoping: value-based get, no hidden errors"
    [ ("get through a function result",           compilesLike "(define/procedure mk! (n) (:a n)) (document g (:rec (mk! 1) :r (get rec a)))"
                                                               "(document g (:rec (:a 1) :r 1))")
    , ("get through a case result",               compilesLike "(document g (:f 1 :rec (case f (1 (:a \"one\")) (otherwise (:a \"other\"))) :r (get rec a)))"
                                                               "(document g (:f 1 :rec (:a \"one\") :r \"one\"))")
    , ("get through an inlay",                    compilesLike "(define/procedure mk! (n) (:a n)) (document g (:rec (:inner (:x 0 (inlay (mk! 1)))) :r (get rec inner a)))"
                                                               "(document g (:rec (:inner (:x 0 :a 1)) :r 1))")
    , ("errors in unused let bindings are reported", failsToCompile "(document g (let ($unused (/ 1 0)) (:a 1)))")
    , ("errors in unused arguments are reported", failsToCompile "(define/procedure k! (a b) a) (document g (:a (k! 1 (/ 1 0))))")
    ]

cleanup :: Group
cleanup = Group "Scoping: cleanup"
    [ ("comparing functions is a type error",     failsToCompile "(define/procedure id! (a) a) (document g (:s (eq id! id!)))")
    ]


-- Substitution: replacing an arithmetic expression with its value never changes the output.
-- Ouro numbers are Doubles, which represent integers exactly only up to 2^53, so the
-- generated expressions stay within that range.
prop_substitution :: Property
prop_substitution = property $ do
    (src, value) <- forAll $ Gen.filter (\(_, v) -> abs v <= 2 ^ (53 :: Int)) genArith
    compiledLike ("(document g (:a " <> src <> "))") ("(document g (:a " <> tshow value <> "))")

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
        src    = "(document g (:a0 1 " <> T.concat (map link [1 .. n]) <> "))"

    result <- evalIO . timeout 5000000 $
                  case compileG src of
                      CompilationSuccess _ ast -> Just <$> evaluate (TL.length (toJSON defaultOptions ast))
                      CompilationFailure _ _   -> pure Nothing

    case result of
        Nothing       -> annotate "Timed out: bindings are being re-evaluated on every reference" >> failure
        Just Nothing  -> annotate "The chain failed to compile" >> failure
        Just (Just _) -> success


-- Helpers
-- A Manet record bound to 'p', with 'r' computed by the given expression
person :: Text -> Text
person rExpr = "(document g (:p " <> manet <> " :r " <> rExpr <> "))"

personWith :: Text -> Text
personWith rValue = "(document g (:p " <> manet <> " :r " <> rValue <> "))"

manet :: Text
manet = "(:id \"https://linked.art/example/person/manet\" :type \"Person\" :_label \"Manet\")"

