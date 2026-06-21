
module Ouro.IntergrationSpec (spec) where

import qualified Data.Ouro.Internal.Expr as I
import           Test.Hspec
import           Utils                   (mkArr, mkDate, mkObj, shouldEvalTo)


spec :: Spec
spec = do
    describe "Ouro Integration Suite" $ do
        describe "Arithmetic" $ do
            it "adds values                                  " $ "(:test (+ 2 2))"                                          `shouldEvalTo` mkObj [("test", I.Number 4)]
            it "adds variadic values                         " $ "(:test (+ 2 2 6))"                                        `shouldEvalTo` mkObj [("test", I.Number 10)]
            it "subtracts values                             " $ "(:test (- 10 3))"                                         `shouldEvalTo` mkObj [("test", I.Number 7)]
            it "subtracts variadic values                    " $ "(:test (- 20 5 2))"                                       `shouldEvalTo` mkObj [("test", I.Number 13)]
            it "multiplies values                            " $ "(:test (* 3 4))"                                          `shouldEvalTo` mkObj [("test", I.Number 12)]
            it "multiplies variadic values                   " $ "(:test (* 2 3 4))"                                        `shouldEvalTo` mkObj [("test", I.Number 24)]
            it "divides values                               " $ "(:test (/ 20 4))"                                         `shouldEvalTo` mkObj [("test", I.Number 5)]
            it "divides variadic values                      " $ "(:test (/ 100 2 5))"                                      `shouldEvalTo` mkObj [("test", I.Number 10)]

        describe "Temporal Shifting" $ do
            let today = "#date \"2026-06-14T00:00:00Z\""
            it "shifts date forward by years                 " $ ("(:test (+ " <> today <> " (years 1)))")                  `shouldEvalTo` mkObj [("test", mkDate "2027-06-14T00:00:00Z")]
            it "shifts date backward by months               " $ ("(:test (- " <> today <> " (months 2)))")                 `shouldEvalTo` mkObj [("test", mkDate "2026-04-14T00:00:00Z")]
            it "shifts date forward by days                  " $ ("(:test (+ " <> today <> " (days 10)))")                  `shouldEvalTo` mkObj [("test", mkDate "2026-06-24T00:00:00Z")]
            it "shifts date forward by hours                 " $ ("(:test (+ " <> today <> " (hours 4)))")                  `shouldEvalTo` mkObj [("test", mkDate "2026-06-14T04:00:00Z")]
            it "shifts date forward by minutes               " $ ("(:test (+ " <> today <> " (minutes 30)))")               `shouldEvalTo` mkObj [("test", mkDate "2026-06-14T00:30:00Z")]
            it "shifts date forward by seconds               " $ ("(:test (+ " <> today <> " (seconds 45)))")               `shouldEvalTo` mkObj [("test", mkDate "2026-06-14T00:00:45Z")]
            it "rolls day over via hours                     " $ ("(:test (+ " <> today <> " (hours 25)))")                 `shouldEvalTo` mkObj [("test", mkDate "2026-06-15T01:00:00Z")]
            it "snaps to end of current year                 " $ ("(:test (+ " <> today <> " (thru (years 0))))")           `shouldEvalTo` mkObj [("test", mkDate "2026-12-31T23:59:59Z")]
            it "shifts and snaps to next year                " $ ("(:test (+ " <> today <> " (thru (years 1))))")           `shouldEvalTo` mkObj [("test", mkDate "2027-12-31T23:59:59Z")]
            it "snaps to target month max days               " $ ("(:test (+ " <> today <> " (thru (months 1))))")          `shouldEvalTo` mkObj [("test", mkDate "2026-07-31T23:59:59Z")]
            it "statically unrolls day windows               " $ ("(:test (+ " <> today <> " (thru (days 19))))")           `shouldEvalTo` mkObj [("test", mkDate "2026-07-03T23:59:59Z")]

        describe "Comparison" $ do
            it "checks greater than                          " $ "(:test (> 10 5))"                                         `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "checks greater than or equal                 " $ "(:test (>= 10 10))"                                       `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "checks less than                             " $ "(:test (< 5 10))"                                         `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "checks less than or equal                    " $ "(:test (<= 5 5))"                                         `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "checks variadic greater than                 " $ "(:test (> 10 5 2))"                                       `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "checks variadic less than                    " $ "(:test (< 2 5 10))"                                       `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "handles variadic mixed failure               " $ "(:test (> 10 5 8))"                                       `shouldEvalTo` mkObj [("test", I.Boolean False)]

        describe "Structural Identity" $ do
            it "compares URIs                                " $ "(:test (eq #uri \"https://ouro.dev\" #uri \"https://ouro.dev\"))"             `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "compares booleans                            " $ "(:test (eq #bool true #bool true))"                                           `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "compares objects                             " $ "(:test (eq (:a 1) (:a 1)))"                                                   `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "compares lists                               " $ "(:test (eq ((:a 1 :b 2) (:a 1 :b 3)) ((:a 1 :b 2) (:a 1 :b 3))))"             `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "differentiates disparate types               " $ "(:test (neq 42 #str \"42\"))"                                                 `shouldEvalTo` mkObj [("test", I.Boolean True)]
            it "differentiates nested attributes             " $ "(:test (neq (:a 1 :b 2) (:a 1 :b 3)))"                                        `shouldEvalTo` mkObj [("test", I.Boolean True)]

        describe "Structural Block Inference" $ do
            it "infers inline objects via attribute keys     " $ "(:test (:a 1 :b 2))"                     `shouldEvalTo` mkObj [("test", mkObj [("a", I.Number 1), ("b", I.Number 2)])]
            it "infers flat arrays via literal heads         " $ "(:test (1 2 3))"                         `shouldEvalTo` mkObj [("test", mkArr [I.Number 1, I.Number 2, I.Number 3])]
            it "infers strings and mixed literals as arrays  " $ "(:test (\"a\" true 3))"                  `shouldEvalTo` mkObj [("test", mkArr [I.String "a", I.Boolean True, I.Number 3])]
            it "infers nested arrays (matrices) recursive    " $ "(:test ((1 2) (3 4)))"                   `shouldEvalTo` mkObj [("test", mkArr [ mkArr [I.Number 1, I.Number 2], mkArr [I.Number 3, I.Number 4] ])]
            it "infers arrays of objects recursively         " $ "(:test ((:id 1) (:id 2)))"               `shouldEvalTo` mkObj [("test", mkArr [ mkObj [("id", I.Number 1)], mkObj [("id", I.Number 2)] ])]
            it "infers arrays of computations via fallback   " $ "(:test ((+ 1 1) (+ 2 2)))"               `shouldEvalTo` mkObj [("test", mkArr [I.Number 2, I.Number 4])]
            it "handles mixed-type structural streams safe   " $ "(:test (1 (:id 2) (+ 1 2)))"             `shouldEvalTo` mkObj [("test", mkArr [ I.Number 1, mkObj [("id", I.Number 2)], I.Number 3 ])]
            it "handles infinitely nested structural depth   " $ "(:test (((:deep 1))))"                   `shouldEvalTo` mkObj [("test", mkArr [ mkArr [ mkObj [("deep", I.Number 1)] ] ])]

        describe "Control Flow (Case)" $ do
            it "matches an explicit equality guard           " $ "(:test (case 10 ((eq 5) 0) ((eq 10) 1) (otherwise 2)))"                          `shouldEvalTo` mkObj [("test", I.Number 1)]
            it "matches a relational inequality guard        " $ "(:test (case 50 ((> 100) 1) ((> 40) 2) (otherwise 3)))"                          `shouldEvalTo` mkObj [("test", I.Number 2)]
            it "falls through to the otherwise branch        " $ "(:test (case 5 ((> 10) 1) ((eq 8) 2) (otherwise 3)))"                            `shouldEvalTo` mkObj [("test", I.Number 3)]
            it "evaluates computational expressions target   " $ "(:test (case (+ 2 3) ((eq 5) (* 2 2)) (otherwise 0)))"                           `shouldEvalTo` mkObj [("test", I.Number 4)]
            it "evaluates computations dynamically bodies    " $ "(:test (case 1 ((eq 1) (:nested true)) (otherwise (:nested false))))"            `shouldEvalTo` mkObj [("test", mkObj [("nested", I.Boolean True)])]
            it "routes safely via strict string comparison   " $ "(:test (case #str \"B\" ((eq #str \"A\") 1) ((eq #str \"B\") 2) (otherwise 3)))" `shouldEvalTo` mkObj [("test", I.Number 2)]

        describe "Pattern Matching (Structural Identity)" $ do
            it "matches simple literal equality              " $ "(:test (case '10 ('10 1) (otherwise 0)))"                                                      `shouldEvalTo` mkObj [("test", I.Number 1)]
            it "matches list structure exactly               " $ "(:test (case '(:a 1 :b 2) ('(:a 1 :b 2) 9) (otherwise 0)))"                                    `shouldEvalTo` mkObj [("test", I.Number 9)]
            it "matches nested list structure exactly        " $ "(:test (case '((:id 1) (:id 2)) ('((:id 1) (:id 2)) 99) (otherwise 0))  )"                     `shouldEvalTo` mkObj [("test", I.Number 99)]
            it "fails to match on mismatched list length     " $ "(:test (case '(:a 1 :b 2) ('(:a 1) 88) (otherwise 0)))"                                        `shouldEvalTo` mkObj [("test", I.Number 0)]
            it "fails to match on mismatched values          " $ "(:test (case '(:a 1 :b 2) ('(:a 1 :b 3) 5) (otherwise 0)))"                                    `shouldEvalTo` mkObj [("test", I.Number 0)]
            it "matches deep nested tree structures          " $ "(:test (case '(((:val 5))) ('(((:val 5))) 7) (otherwise 0)))"                                  `shouldEvalTo` mkObj [("test", I.Number 7)]

        describe "Pattern Matching (Structural Holes)" $ do
            it "matches a literal via hole wildcard          " $ "(:test (case '10 ('? 1) (otherwise 0)))"                                                       `shouldEvalTo` mkObj [("test", I.Number 1)]
            it "matches a specific element in a list         " $ "(:test (case '(:a 1 :b 2) ('(:a 1 :b ?) 9) (otherwise 0)))"                                   `shouldEvalTo` mkObj [("test", I.Number 9)]
            it "matches nested structures with holes         " $ "(:test (case '((:id 1) (:id 2)) ('((? 1) (? 2)) 99) (otherwise 0)))"                          `shouldEvalTo` mkObj [("test", I.Number 99)]
            it "matches the tail of a list with a hole       " $ "(:test (case '(:name \"Dev\" :valid true :active true) ('(:name \"Dev\" ? ?) 88) (otherwise 0)))" `shouldEvalTo` mkObj [("test", I.Number 88)]
            it "differentiates by structure (ident holes)    " $ "(:test (case '(:a 1 :b 2) ('(:a 99 :b 2) 0) ('(:a 1 :b 2) 5) (otherwise 0)))"                   `shouldEvalTo` mkObj [("test", I.Number 5)]
            it "matches deeply nested structural trees       " $ "(:test (case '(((:val 5))) ('(((? 5))) 7) (otherwise 0)))"                                   `shouldEvalTo` mkObj [("test", I.Number 7)]
