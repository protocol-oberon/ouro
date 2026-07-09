module Ouro.EvaluationTest (tests) where

import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           Control.Monad.Reader       (runReader)
import qualified Text.Megaparsec            as M
import           Text.Megaparsec.Pos        (initialPos)

import qualified Data.Ouro.Internal.Expr    as I
import           Data.Ouro.Lisp.Eval.Engine (evalExpr)
import           Data.Ouro.Lisp.Eval.Types  (emptyEnv)
import qualified Data.Ouro.Lisp.Eval.Types  as L
import qualified Data.Ouro.Lisp.Surface     as S
import qualified Data.Text                  as T

runEval :: S.Expr -> L.Expr
runEval expr = runReader (evalExpr expr) emptyEnv

dummyPos :: M.SourcePos
dummyPos = initialPos "test"

-- The test group that will be imported and run by Main.hs
tests :: Group
tests = Group "evalExpr (Semantic Transformation Pass)"
    [ ("lowers String syntax to Primitive I.String",   prop_lower_string)
    , ("lowers Number syntax to Primitive I.Number",   prop_lower_number)
    , ("lowers Boolean syntax to Primitive I.Boolean", prop_lower_boolean)
    , ("lowers Null syntax to Primitive I.Null",       prop_lower_null)
    , ("lowers Empty collections correctly",           prop_lower_empty)

    , ("wraps raw S.Quoted syntax nodes in L.Quote",   prop_quote_raw)
    , ("intercepts (quote payload) special form",      prop_quote_form)

    , ("evaluates (list ...) forms",                   prop_list_form)
    , ("evaluates (attr \"key\" val) forms",           prop_attr_form)

    , ("evaluates (+ 2 2) via env mapping",            prop_add_primitive)
    , ("evaluates (> 10 5) via env mapping",           prop_gt_primitive)
    , ("evaluates (eq true true) for identity",        prop_eq_primitive)

    , ("routes to the correct branch in (case ...)",   prop_case_routing)

    , ("returns L.EvalError for invalid attr name",    prop_error_invalid_attr)
    , ("returns L.EvalError for out-of-bounds nth",    prop_error_nth_bounds)

    , ("unwraps L.Quote and evaluates its payload",    prop_eval_quote)
    , ("yields L.EvalError if evaluating unquoted",    prop_eval_unquoted)
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

prop_lower_null :: Property
prop_lower_null = withTests 1 . property $ do
    runEval (S.Literal dummyPos S.Null) === L.Primitive I.Null

prop_lower_empty :: Property
prop_lower_empty = withTests 1 . property $ do
    runEval (S.Literal dummyPos S.EmptyArr) === L.Primitive I.EmptyArr
    runEval (S.Literal dummyPos S.EmptyRec) === L.Primitive I.EmptyRec


-- Quoting Mechanics
prop_quote_raw :: Property
prop_quote_raw = withTests 1 . property $ do
    let target = S.Literal dummyPos (S.Num 99)
    let expr   = S.Quoted dummyPos target
    runEval expr === L.Quote target

prop_quote_form :: Property
prop_quote_form = withTests 1 . property $ do
    let target = S.Literal dummyPos (S.Str "unevaluated")
    let expr   = S.Form dummyPos [S.Symbol dummyPos "quote", target]
    runEval expr === L.Quote target


-- Structural Interceptions (Special Forms)
prop_list_form :: Property
prop_list_form = withTests 1 . property $ do
    let expr = S.Form dummyPos
                [ S.Symbol dummyPos "list"
                , S.Literal dummyPos (S.Num 1)
                , S.Literal dummyPos (S.Num 2)
                ]
    runEval expr === L.Array [ L.Primitive (I.Number 1), L.Primitive (I.Number 2) ]

prop_attr_form :: Property
prop_attr_form = withTests 1 . property $ do
    let nameExpr = S.Literal dummyPos (S.Str "host")
    let valExpr  = S.Literal dummyPos (S.Str "localhost")
    let expr = S.Form dummyPos [S.Symbol dummyPos "attr", nameExpr, valExpr]
    runEval expr === L.Attr "host" (L.Primitive (I.String "localhost"))


-- Native Primitive Dispatch (Arithmetic & Comparison)
prop_add_primitive :: Property
prop_add_primitive = withTests 1 . property $ do
    let expr = S.Form dummyPos [ S.Symbol dummyPos "+", S.Literal dummyPos (S.Num 2), S.Literal dummyPos (S.Num 2) ]
    runEval expr === L.Primitive (I.Number 4)

prop_gt_primitive :: Property
prop_gt_primitive = withTests 1 . property $ do
    let expr = S.Form dummyPos [ S.Symbol dummyPos ">", S.Literal dummyPos (S.Num 10), S.Literal dummyPos (S.Num 5) ]
    runEval expr === L.Primitive (I.Boolean True)

prop_eq_primitive :: Property
prop_eq_primitive = withTests 1 . property $ do
    let expr = S.Form dummyPos [ S.Symbol dummyPos "eq", S.Literal dummyPos (S.Bool True), S.Literal dummyPos (S.Bool True) ]
    runEval expr === L.Primitive (I.Boolean True)


-- Control Flow Execution (Case)
prop_case_routing :: Property
prop_case_routing = withTests 1 . property $ do
    let target   = S.Literal dummyPos (S.Num 10)
    let guard1   = S.Form dummyPos [S.Symbol dummyPos "eq", S.Literal dummyPos (S.Num 10)]
    let branch1  = S.Form dummyPos [guard1, S.Literal dummyPos (S.Num 1)]
    let fallback = S.Form dummyPos [S.Symbol dummyPos "otherwise", S.Literal dummyPos (S.Num 2)]
    let expr     = S.Form dummyPos [S.Symbol dummyPos "case", target, branch1, fallback]

    runEval expr === L.Primitive (I.Number 1)


-- Fault Isolation and Error Propagation

-- Notice how much cleaner error checking is in Hedgehog.
-- We annotate the result so if it fails, you see exactly what was returned,
-- and then we simply assert that it is an EvalError.

prop_error_invalid_attr :: Property
prop_error_invalid_attr = withTests 1 . property $ do
    let nameExpr = S.Literal dummyPos (S.Num 404)
    let valExpr  = S.Literal dummyPos (S.Str "localhost")
    let expr = S.Form dummyPos [S.Symbol dummyPos "attr", nameExpr, valExpr]

    let result = runEval expr
    annotateShow result
    assert (isEvalError result)

prop_error_nth_bounds :: Property
prop_error_nth_bounds = withTests 1 . property $ do
    let targetArray = S.Form dummyPos [S.Symbol dummyPos "list", S.Literal dummyPos (S.Num 1)]
    let indexExpr   = S.Literal dummyPos (S.Num 5)
    let expr = S.Form dummyPos [S.Symbol dummyPos "nth", indexExpr, targetArray]

    let result = runEval expr
    annotateShow result
    assert (isEvalError result)


-- Dynamic Evaluation (eval)
prop_eval_quote :: Property
prop_eval_quote = withTests 1 . property $ do
    let quotedExpr = S.Form dummyPos [S.Symbol dummyPos "quote", S.Literal dummyPos (S.Num 42)]
    let evalForm   = S.Form dummyPos [S.Symbol dummyPos "eval", quotedExpr]

    runEval evalForm === L.Primitive (I.Number 42)

prop_eval_unquoted :: Property
prop_eval_unquoted = withTests 1 . property $ do
    let evalForm = S.Form dummyPos [S.Symbol dummyPos "eval", S.Literal dummyPos (S.Num 42)]

    let result = runEval evalForm
    annotateShow result
    assert (isEvalError result)


-- Helpers
isEvalError :: L.Expr -> Bool
isEvalError (L.EvalError _) = True
isEvalError _               = False
