
module Ouro.StructuralTest (tests) where

import           Data.Ouro.Lisp.Eval.Structural (BlockTarget (..),
                                                 determineBlockTarget)
import qualified Data.Ouro.Lisp.Surface         as S
import           Hedgehog
import qualified Hedgehog.Gen                   as Gen
import qualified Hedgehog.Range                 as Range
import           Utils                          (nullPos)

-- The test group that will be imported and run by Main.hs
tests :: Group
tests = Group "Structural Lookaheads"
    [ ("routes flat constants to TargetList",     prop_literals_targetList)
    , ("routes leading attributes to TargetRecord", prop_attr_targetRecord)
    , ("routes leading symbols to FunctionApp",   prop_symbol_targetFunctionApp)
    , ("routes 'context' bindings to TargetRecord", prop_context_targetRecord)
    , ("routes 'define' macros to TargetList",    prop_define_targetList)
    , ("routes nested objects to TargetList",     prop_nestedObject_targetList)
    , ("routes nested matrices to TargetList",    prop_nestedMatrix_targetList)
    , ("routes nested functions to TargetList",   prop_nestedFunction_targetList)
    , ("defaults empty streams to TargetList",    prop_empty_targetList)
    ]

prop_literals_targetList :: Property
prop_literals_targetList = property $ do
    -- Generate random numbers to prove ANY number literal routes to TargetList
    n <- forAll $ Gen.int (Range.linear (-1000) 1000)
    determineBlockTarget [S.Literal nullPos (S.Num (fromIntegral n))] === TargetList

prop_attr_targetRecord :: Property
prop_attr_targetRecord = property $ do
    determineBlockTarget [S.Form nullPos [S.Symbol nullPos "attr", S.Literal nullPos (S.Str "Test")]] === TargetRecord

prop_symbol_targetFunctionApp :: Property
prop_symbol_targetFunctionApp = property $ do
    -- Generate a few different valid symbols
    sym <- forAll $ Gen.element ["+", "-", "eq", "concat", "map"]
    determineBlockTarget [S.Symbol nullPos sym] === TargetFunctionApp

prop_context_targetRecord :: Property
prop_context_targetRecord = property $ do
    determineBlockTarget [S.Form nullPos [S.Symbol nullPos "context", S.Form nullPos []]] === TargetRecord

prop_define_targetList :: Property
prop_define_targetList = property $ do
    determineBlockTarget [S.Form nullPos [S.Symbol nullPos "define", S.Symbol nullPos "x"]] === TargetList

prop_nestedObject_targetList :: Property
prop_nestedObject_targetList = property $ do
    determineBlockTarget [S.Form nullPos [S.Form nullPos [S.Symbol nullPos "attr", S.Literal nullPos (S.Str "Test")]]] === TargetList

prop_nestedMatrix_targetList :: Property
prop_nestedMatrix_targetList = property $ do
    n <- forAll $ Gen.int (Range.linear (-1000) 1000)
    determineBlockTarget [S.Form nullPos [S.Literal nullPos (S.Num (fromIntegral n))]] === TargetList

prop_nestedFunction_targetList :: Property
prop_nestedFunction_targetList = property $ do
    sym <- forAll $ Gen.element ["+", "-", "*", "/"]
    determineBlockTarget [S.Form nullPos [S.Symbol nullPos sym]] === TargetList

prop_empty_targetList :: Property
prop_empty_targetList = property $ do
    determineBlockTarget [] === TargetList
