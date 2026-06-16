
module Ouro.StructuralSpec where

import           Data.Ouro.Lisp.Eval.Structural (BlockTarget (..),
                                                 determineBlockTarget)
import qualified Data.Ouro.Lisp.Surface         as S
import           Test.Hspec
import           Utils                          (nullPos)

spec :: Spec
spec = describe "Structural Lookaheads" $ do
    it "routes flat constants to TargetList      " $ determineBlockTarget [S.Literal nullPos (S.Num 1)]                                      `shouldBe` TargetList
    it "routes leading attributes to TargetObject" $ determineBlockTarget [S.Attr nullPos "id"]                                              `shouldBe` TargetObject
    it "routes leading symbols to FunctionApp    " $ determineBlockTarget [S.Symbol nullPos "+"]                                             `shouldBe` TargetFunctionApp
    it "routes 'context' bindings to TargetObject" $ determineBlockTarget [S.Form nullPos [S.Symbol nullPos "context", S.Form nullPos []]]   `shouldBe` TargetObject
    it "routes 'define' macros to TargetObject   " $ determineBlockTarget [S.Form nullPos [S.Symbol nullPos "define", S.Symbol nullPos "x"]] `shouldBe` TargetObject
    it "routes nested objects to TargetList      " $ determineBlockTarget [S.Form nullPos [S.Attr nullPos "id"]]                             `shouldBe` TargetList
    it "routes nested matrices to TargetList     " $ determineBlockTarget [S.Form nullPos [S.Literal nullPos (S.Num 1)]]                     `shouldBe` TargetList
    it "routes nested functions to TargetList    " $ determineBlockTarget [S.Form nullPos [S.Symbol nullPos "+"]]                            `shouldBe` TargetList
    it "defaults empty streams to FunctionApp    " $ determineBlockTarget []                                                                 `shouldBe` TargetFunctionApp
