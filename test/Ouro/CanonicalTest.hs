
module Ouro.CanonicalTest (tests) where

import           Control.Monad.IO.Class (liftIO)
import           Hedgehog
import           Utils                  (fixture, shouldCompileTo)

-- The test group that will be imported and run by Main.hs
tests :: Group
tests = Group "Ouro Canonical Example File Suite - Compiles to JSON"
    [ ("Auction of Stowe House",          prop_auctionOfStoweHouse)
    , ("Bust of a Man by Francis Hardwood", prop_bustOfAMan)
    , ("Jeanne Spring by Manet",          prop_jeanneSpring)
    , ("Purchase of Spring by Proust",    prop_purchaseOfSpring)
    , ("The Night Watch by Rembrandt",    prop_theNightWatch)
    ]

-- withTests 1 ensures Hedgehog only runs this static file check once
prop_auctionOfStoweHouse :: Property
prop_auctionOfStoweHouse = withTests 1 . property $ do
    shouldCompileTo (fixture "ouro/auction-of-stowe-house.ouro") "auction-of-stowe-house" (fixture "json/auction-of-stowe-house.json")

prop_bustOfAMan :: Property
prop_bustOfAMan = withTests 1 . property $ do
    shouldCompileTo (fixture "ouro/bust-of-a-man.ouro") "bust-of-a-man" (fixture "json/bust-of-a-man.json")

prop_jeanneSpring :: Property
prop_jeanneSpring = withTests 1 . property $ do
    shouldCompileTo (fixture "ouro/jeanne-spring.ouro") "jeanne-spring" (fixture "json/jeanne-spring.json")

prop_purchaseOfSpring :: Property
prop_purchaseOfSpring = withTests 1 . property $ do
    shouldCompileTo (fixture "ouro/purchase-of-spring.ouro") "purchase-of-spring" (fixture "json/purchase-of-spring.json")

prop_theNightWatch :: Property
prop_theNightWatch = withTests 1 . property $ do
    shouldCompileTo (fixture "ouro/the-night-watch.ouro") "the-night-watch" (fixture "json/the-night-watch.json")
