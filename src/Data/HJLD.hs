module Data.HJLD (someFunc) where

import qualified Data.HJLD.Parser as P


someFunc :: IO ()
someFunc = do
    P.main
