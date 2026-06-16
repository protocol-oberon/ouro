
module Ouro.CanonicalSpec (spec) where

import           Test.Hspec
import           Utils      (fixture, shouldCompileTo)


spec :: Spec
spec = do
    describe "Ouro Canocial Example File Suite" $ do

        describe "Compiles to JSON" $ do
            it "Auction of Stowe House           " $ fixture "ouro/auction-of-stowe-house.ouro" `shouldCompileTo` fixture "json/auction-of-stowe-house.json"
            it "Bust of a Man by Francis Hardwood" $ fixture "ouro/bust-of-a-man.ouro"          `shouldCompileTo` fixture "json/bust-of-a-man.json"
            it "Jeanne Spring by Manet           " $ fixture "ouro/jeanne-spring.ouro"          `shouldCompileTo` fixture "json/jeanne-spring.json"
            it "Purchase of Spring by Proust     " $ fixture "ouro/purchase-of-spring.ouro"     `shouldCompileTo` fixture "json/purchase-of-spring.json"
            it "The Night Watch by Rembrandt     " $ fixture "ouro/the-night-watch.ouro"        `shouldCompileTo` fixture "json/the-night-watch.json"
