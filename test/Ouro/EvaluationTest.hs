module Ouro.EvaluationTest (tests) where

import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import qualified Text.Megaparsec            as M
import           Text.Megaparsec.Pos        (initialPos)

import           Data.Ouro                  (CompilationResult (..), compile)
import qualified Data.Ouro.Internal.Expr    as I
import qualified Data.Ouro.Lisp.Canon       as Canon
import           Data.Ouro.Lisp.Eval.Engine (evalExpr)
import           Data.Ouro.Lisp.Eval.Types  (emptyEnv, runEvalM)
import qualified Data.Ouro.Lisp.Eval.Types  as L
import qualified Data.Ouro.Lisp.Surface     as S
import qualified Data.Ouro.Refinement.Type  as R
import qualified Data.Text                  as T

runEval :: S.Expr -> L.Expr
runEval expr = runEvalM emptyEnv (evalExpr expr)

dummyPos :: M.SourcePos
dummyPos = initialPos "test"

-- The test group that will be imported and run by Main.hs
tests :: Group
tests = Group "Evaluation Tests (Surface to Higher IR)"
    [ ("lowers String syntax to Primitive I.String",   prop_lower_string)
    , ("lowers Number syntax to Primitive I.Number",   prop_lower_number)
    , ("lowers Boolean syntax to Primitive I.Boolean", prop_lower_boolean)
    , ("lowers Empty collections correctly",           prop_lower_empty)

    , ("evaluates (array ...) forms",                   prop_list_form)
    , ("evaluates (%attr \"key\" val) forms",          prop_attr_form)

    , ("evaluates (+ 2 2) via env mapping",            prop_add_primitive)
    , ("evaluates (gt 10 5) via env mapping",           prop_gt_primitive)
    , ("evaluates (eq true true) for identity",        prop_eq_primitive)
    , ("all over written comparisons is their and",    prop_all_comparisons)

    , ("routes to the correct branch in (case ...)",   prop_case_routing)

    , ("runs Canon's passes in order",                 prop_pass_order)
    , ("runs infix, then all but four, on a pattern",  prop_pattern_passes)
    , ("runs a single pass on its own",                prop_single_pass)

    , ("returns L.EvalError for the removed attr form", prop_error_removed_attr)
    , ("returns L.EvalError for an index past the end", prop_error_index_bounds)


    , ("writes a duration as its string",              prop_write_duration)
    , ("rejects a builtin as a graph value",           prop_reject_builtin)
    , ("rejects a function as a graph value",          prop_reject_function)
    , ("rejects an infinite Number as a graph value",  prop_reject_infinity)
    , ("rejects NaN as a graph value",                 prop_reject_nan)
    ]


-- Literal Normalization (Upgraded to Property Tests)
prop_lower_string :: Property
prop_lower_string = property $ do
    str <- forAll $ Gen.string (Range.linear 0 100) Gen.unicode
    runEval (S.Literal dummyPos (S.Str (T.pack str))) === L.Primitive (I.String (T.pack str))

prop_lower_number :: Property
prop_lower_number = property $ do
    -- Assuming S.Num and I.Number take Double. Adjust Gen.double if it's Int.
    num <- forAll $ Gen.double (Range.linearFrac (-10000.0) 10000.0)
    runEval (S.Literal dummyPos (S.Num num)) === L.Primitive (I.Number num)

prop_lower_boolean :: Property
prop_lower_boolean = property $ do
    b <- forAll Gen.bool
    runEval (S.Literal dummyPos (S.Bool b)) === L.Primitive (I.Boolean b)

prop_lower_empty :: Property
prop_lower_empty = withTests 1 . property $ do
    runEval (S.Literal dummyPos S.EmptyArr) === L.Primitive I.EmptyArr
    runEval (S.Literal dummyPos S.EmptyRec) === L.Primitive I.EmptyRec


-- Structural Interceptions (Special Forms)
prop_list_form :: Property
prop_list_form = withTests 1 . property $ do
    let expr = S.Form dummyPos
                [ S.Symbol dummyPos "array"
                , S.Literal dummyPos (S.Num 1)
                , S.Literal dummyPos (S.Num 2)
                ]
    runEval expr === L.Array [ L.Primitive (I.Number 1), L.Primitive (I.Number 2) ]

prop_attr_form :: Property
prop_attr_form = withTests 1 . property $ do
    let nameExpr = S.Literal dummyPos (S.Str "host")
    let valExpr  = S.Literal dummyPos (S.Str "localhost")
    let expr = S.Form dummyPos [S.Symbol dummyPos "%attr", nameExpr, valExpr]
    runEval expr === L.Attr "host" (L.Primitive (I.String "localhost"))


-- Native Primitive Dispatch (Arithmetic & Comparison)
prop_add_primitive :: Property
prop_add_primitive = withTests 1 . property $ do
    let expr = S.Form dummyPos [ S.Symbol dummyPos "+", S.Literal dummyPos (S.Num 2), S.Literal dummyPos (S.Num 2) ]
    runEval expr === L.Primitive (I.Number 4)

prop_gt_primitive :: Property
prop_gt_primitive = withTests 1 . property $ do
    let expr = S.Form dummyPos [ S.Symbol dummyPos "gt", S.Literal dummyPos (S.Num 10), S.Literal dummyPos (S.Num 5) ]
    runEval expr === L.Primitive (I.Boolean True)

prop_eq_primitive :: Property
prop_eq_primitive = withTests 1 . property $ do
    let expr = S.Form dummyPos [ S.Symbol dummyPos "eq", S.Literal dummyPos (S.Bool True), S.Literal dummyPos (S.Bool True) ]
    runEval expr === L.Primitive (I.Boolean True)


-- A comparison takes two operands, so a chain is written as all over its neighbouring pairs,
-- (all ((lt a b) (lt b c))), which holds when every pair does
prop_all_comparisons :: Property
prop_all_comparisons = property $ do
    opName <- forAll $ Gen.element ["lt", "leq", "gt", "geq", "eq"]
    nums   <- forAll $ Gen.list (Range.linear 3 5) (fromIntegral <$> Gen.int (Range.linear 0 3))

    let op       = case opName of
                       "lt"  -> (<)
                       "leq" -> (<=)
                       "gt"  -> (>)
                       "geq" -> (>=)
                       _eq   -> (==)
        num      = S.Literal dummyPos . S.Num
        pairs    = zipWith (\a b -> S.Form dummyPos [S.Symbol dummyPos opName, num a, num b]) nums (drop 1 nums)
        expr     = S.Form dummyPos [S.Symbol dummyPos "all", S.Form dummyPos pairs]
        expected = and (zipWith op nums (drop 1 nums))

    runEval (Canon.canonical expr) === L.Primitive (I.Boolean expected)


-- Control Flow Execution (Case)
prop_case_routing :: Property
prop_case_routing = withTests 1 . property $ do
    let target   = S.Literal dummyPos (S.Num 10)
    let guard1   = S.Form dummyPos [S.Symbol dummyPos "eq", S.Literal dummyPos (S.Num 10)]
    let branch1  = S.Form dummyPos [guard1, S.Literal dummyPos (S.Num 1)]
    let fallback = S.Form dummyPos [S.Symbol dummyPos "otherwise", S.Literal dummyPos (S.Num 2)]
    let expr     = S.Form dummyPos [S.Symbol dummyPos "case", target, branch1, fallback]

    -- Guards are marked by the 'guards' pass, so the case goes through Canon first
    runEval (Canon.canonical expr) === L.Primitive (I.Number 1)


-- Canon's passes
prop_pass_order :: Property
prop_pass_order = withTests 1 . property $
    map Canon.passName Canon.passes
        === [ "infix", "placeholders", "threading", "typing", "connectives", "bindings", "guards"
            , "contexts", "adjoin", "with", "thru", "alternatives" ]

-- A parameter pattern has infix first, as an expression does, and skips typing,
-- placeholders, threading and connectives
prop_pattern_passes :: Property
prop_pattern_passes = withTests 1 . property $
    map Canon.passName Canon.patternPasses === "infix" : drop 5 (map Canon.passName Canon.passes)

-- Running 'connectives' alone makes a written all a chain of cases, and leaves arithmetic
-- alone: nothing unrolls (+ 1 2 3), which is an arity error
prop_single_pass :: Property
prop_single_pass = withTests 1 . property $ do
    let sym             = S.Symbol dummyPos
        num             = S.Literal dummyPos . S.Num
        bool            = S.Literal dummyPos . S.Bool
        asBool          = S.Asserted dummyPos (R.Named R.RBoolean)
        form            = S.Form dummyPos
        onlyConnectives = filter ((== "connectives") . Canon.passName) Canon.passes
    Canon.runPasses onlyConnectives (form [sym "all", form [bool True, bool False]])
        === form [sym "%case", asBool (bool True), form [bool True, asBool (bool False)], form [sym "otherwise", bool False]]
    Canon.runPasses onlyConnectives (form [sym "+", num 1, num 2, num 3])
        === form [sym "+", num 1, num 2, num 3]


-- Fault Isolation and Error Propagation

-- Notice how much cleaner error checking is in Hedgehog.
-- We annotate the result so if it fails, you see exactly what was returned,
-- and then we simply assert that it is an EvalError.

-- (attr key value) was removed: an attribute pair is written ':key value'
prop_error_removed_attr :: Property
prop_error_removed_attr = withTests 1 . property $ do
    let nameExpr = S.Literal dummyPos (S.Str "host")
    let valExpr  = S.Literal dummyPos (S.Str "localhost")
    let expr = S.Form dummyPos [S.Symbol dummyPos "attr", nameExpr, valExpr]

    let result = runEval expr
    annotateShow result
    assert (isEvalError result)

prop_error_index_bounds :: Property
prop_error_index_bounds = withTests 1 . property $ do
    let targetArray = S.Form dummyPos [S.Symbol dummyPos "list", S.Literal dummyPos (S.Num 1)]
    let indexExpr   = S.Literal dummyPos (S.Num 5)
    let expr = S.Form dummyPos [S.Symbol dummyPos "get", targetArray, indexExpr]

    let result = runEval expr
    annotateShow result
    assert (isEvalError result)


-- A duration is a string, in xsd:duration form, so it is written as one
prop_write_duration :: Property
prop_write_duration = withTests 1 . property $
    assert $ not (isCompileFailure (compile "test-suite" "g" "(document g (:d (years 2)))"))

-- Compile-time only values must produce a compiler error, not crash the serializer

prop_reject_builtin :: Property
prop_reject_builtin = withTests 1 . property $
    assert $ isCompileFailure (compile "test-suite" "g" "(document g (:f +))")

prop_reject_function :: Property
prop_reject_function = withTests 1 . property $
    assert $ isCompileFailure (compile "test-suite" "g" "(define/procedure id! (a) a) (document g (:f id!))")

-- JSON cannot represent non-finite numbers
prop_reject_infinity :: Property
prop_reject_infinity = withTests 1 . property $
    assert $ isCompileFailure (compile "test-suite" "g" "(document g (:big (* 1e308 10)))")

prop_reject_nan :: Property
prop_reject_nan = withTests 1 . property $
    assert $ isCompileFailure (compile "test-suite" "g" "(document g (:nan (- (* 1e308 10) (* 1e308 10))))")


-- Helpers
isEvalError :: L.Expr -> Bool
isEvalError (L.EvalError _) = True
isEvalError _               = False

isCompileFailure :: CompilationResult -> Bool
isCompileFailure (CompilationFailure _ _) = True
isCompileFailure _                        = False
