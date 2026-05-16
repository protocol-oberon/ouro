module Data.HJLD (go) where

import qualified Data.HJLD.Parser as P
import qualified Data.Text.IO     as TIO


go :: IO ()
go = do
    json <- TIO.readFile "linked-art.json"
    P.go json
