
module Ouro.CanonicalSpec (spec) where

import           Test.Hspec
import           Utils      (fixture, shouldCompileTo)


spec :: Spec
spec = do
    describe "Ouro Canocial Example File Suite" $ do

        describe "Compiles to JSON" $ do
            it "Jeanne Spring by Manet           " $ fixture "ouro/jeanne-spring.ouro"   `shouldCompileTo` fixture "json/jeanne-spring.json"
            it "The Night Watch by Rembrandt     " $ fixture "ouro/the-night-watch.ouro" `shouldCompileTo` fixture "json/the-night-watch.json"
            it "Bust of a Man by Francis Hardwood" $ fixture "ouro/bust-of-a-man.ouro"   `shouldCompileTo` fixture "json/bust-of-a-man.json"
