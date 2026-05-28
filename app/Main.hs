
module Main (main) where

import           CLI.Parser (runParser)
import           CLI.Runner (runCommand)

main :: IO ()
main = runParser >>= runCommand
