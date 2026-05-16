module Data.HJLD (go) where

import qualified Data.HJLD.Parser as P
import qualified Data.Text.IO     as TIO
import           Text.Megaparsec  (errorBundlePretty)


go :: IO ()
go = do
    json <- TIO.readFile "linked-art.json"

    case P.go "linked-art.json" json of
        Left err -> do
            putStrLn "Parsing Failed!"
            putStrLn (errorBundlePretty err)

        Right expr -> do
            let ast = show expr

            putStrLn "Parsing Succeeded!"
            writeFile "ast.txt" ast
