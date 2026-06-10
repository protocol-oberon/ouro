
module Ouro.EvaluationSpec (spec) where

import Test.Hspec
import qualified Data.Ouro.Internal.Expr as I
import Utils (mkObj, shouldEvalTo)


spec :: Spec
spec = do
    describe "Ouro Inline Integration Suite" $ do
        context "Pipeline Integration: S-Expression Syntax to I.Expr JSON-LD representation" $ do

            -- ==========================================
            -- Addition Integration
            -- ==========================================
            it "integrates string parsing and desugaring with binary addition to synthesize a single-attribute object" $
                "(:test (+ 2 2))" `shouldEvalTo` mkObj [("test", I.Number 4)]

            it "integrates string parsing and desugaring with variadic addition to synthesize a single-attribute object" $
                "(:test (+ 2 2 6))" `shouldEvalTo` mkObj [("test", I.Number 10)]

            -- ==========================================
            -- Subtraction Integration
            -- ==========================================
            it "integrates string parsing and desugaring with left-associative binary subtraction to synthesize a single-attribute object" $
                "(:test (- 10 3))" `shouldEvalTo` mkObj [("test", I.Number 7)]

            it "integrates string parsing and desugaring with multi-argument variadic subtraction to synthesize a single-attribute object" $
                "(:test (- 20 5 2))" `shouldEvalTo` mkObj [("test", I.Number 13)]

            -- ==========================================
            -- Multiplication Integration
            -- ==========================================
            it "integrates string parsing and desugaring with binary multiplication to synthesize a single-attribute object" $
                "(:test (* 3 4))" `shouldEvalTo` mkObj [("test", I.Number 12)]

            it "integrates string parsing and desugaring with multi-argument variadic multiplication to synthesize a single-attribute object" $
                "(:test (* 2 3 4))" `shouldEvalTo` mkObj [("test", I.Number 24)]

            -- ==========================================
            -- Division Integration
            -- ==========================================
            it "integrates string parsing and desugaring with left-associative binary division to synthesize a single-attribute object" $
                "(:test (/ 20 4))" `shouldEvalTo` mkObj [("test", I.Number 5)]

            it "integrates string parsing and desugaring with multi-argument variadic division to synthesize a single-attribute object" $
                "(:test (/ 100 2 5))" `shouldEvalTo` mkObj [("test", I.Number 10)]

            -- ==========================================
            -- Equality Integration
            -- ==========================================
            it "integrates parsing, boolean type lowering, and positive equality checking to synthesize an explicit boolean object" $
                "(:test (eq 42 42))" `shouldEvalTo` mkObj [("test", I.Boolean True)]

            it "integrates parsing, boolean type lowering, and negative equality checking to synthesize an explicit boolean object" $
                "(:test (eq 42 99))" `shouldEvalTo` mkObj [("test", I.Boolean False)]
