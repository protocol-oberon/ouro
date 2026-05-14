module Main (main) where

import qualified Data.HJLD (someFunc)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    Data.HJLD.someFunc
