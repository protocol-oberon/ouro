
module Ouro.IntergrationSpec (spec) where

import qualified Data.Ouro.Internal.Expr as I
import           Test.Hspec
import           Utils                   (mkDate, mkObj, shouldEvalTo)


spec :: Spec
spec = do
    describe "Ouro Integration Suite" $ do

        describe "Arithmetic" $ do
            it "adds values               " $ "(:test (+ 2 2))"     `shouldEvalTo` mkObj [("test", I.Number 4)]
            it "adds variadic values      " $ "(:test (+ 2 2 6))"   `shouldEvalTo` mkObj [("test", I.Number 10)]
            it "subtracts values          " $ "(:test (- 10 3))"    `shouldEvalTo` mkObj [("test", I.Number 7)]
            it "subtracts variadic values " $ "(:test (- 20 5 2))"  `shouldEvalTo` mkObj [("test", I.Number 13)]
            it "multiplies values         " $ "(:test (* 3 4))"     `shouldEvalTo` mkObj [("test", I.Number 12)]
            it "multiplies variadic values" $ "(:test (* 2 3 4))"   `shouldEvalTo` mkObj [("test", I.Number 24)]
            it "divides values            " $ "(:test (/ 20 4))"    `shouldEvalTo` mkObj [("test", I.Number 5)]
            it "divides variadic values   " $ "(:test (/ 100 2 5))" `shouldEvalTo` mkObj [("test", I.Number 10)]

        describe "Temporal Shifting" $ do
            let today = "#date \"2026-06-14T00:00:00Z\""
            it "shifts date forward by years  " $ ("(:test (+ " <> today <> " (years 1)))")    `shouldEvalTo` mkObj [("test", mkDate "2027-06-14T00:00:00Z")]
            it "shifts date backward by months" $ ("(:test (- " <> today <> " (months 2)))")   `shouldEvalTo` mkObj [("test", mkDate "2026-04-14T00:00:00Z")]
            it "shifts date forward by days   " $ ("(:test (+ " <> today <> " (days 10)))")    `shouldEvalTo` mkObj [("test", mkDate "2026-06-24T00:00:00Z")]
            it "shifts date forward by hours  " $ ("(:test (+ " <> today <> " (hours 4)))")    `shouldEvalTo` mkObj [("test", mkDate "2026-06-14T04:00:00Z")]
            it "shifts date forward by minutes" $ ("(:test (+ " <> today <> " (minutes 30)))") `shouldEvalTo` mkObj [("test", mkDate "2026-06-14T00:30:00Z")]
            it "shifts date forward by seconds" $ ("(:test (+ " <> today <> " (seconds 45)))") `shouldEvalTo` mkObj [("test", mkDate "2026-06-14T00:00:45Z")]
            it "rolls day over via hours      " $ ("(:test (+ " <> today <> " (hours 25)))")   `shouldEvalTo` mkObj [("test", mkDate "2026-06-15T01:00:00Z")]

        describe "Comparison" $ do
            it "checks greater than           " $ "(:test (> 10 5))"   `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "checks greater than or equal  " $ "(:test (>= 10 10))" `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "checks less than              " $ "(:test (< 5 10))"   `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "checks less than or equal     " $ "(:test (<= 5 5))"   `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "checks variadic greater than  " $ "(:test (> 10 5 2))" `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "checks variadic less than     " $ "(:test (< 2 5 10))" `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "handles variadic mixed failure" $ "(:test (> 10 5 8))" `shouldEvalTo` mkObj [("test", I.Boolean False)]

        describe "Structural Identity" $ do
            it "compares URIs                   " $ "(:test (eq #uri \"https://ouro.dev\" #uri \"https://ouro.dev\"))" `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "compares booleans               " $ "(:test (eq #bool true #bool true))"                               `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "compares objects                " $ "(:test (eq (:a 1) (:a 1)))"                                       `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "compares lists                  " $ "(:test (eq ((:a 1 :b 2) (:a 1 :b 3)) ((:a 1 :b 2) (:a 1 :b 3))))" `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "differentiates disparate types  " $ "(:test (neq 42 #str \"42\"))"                                     `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "differentiates nested attributes" $ "(:test (neq (:a 1 :b 2) (:a 1 :b 3)))"                            `shouldEvalTo` mkObj [("test", I.Boolean True)]
