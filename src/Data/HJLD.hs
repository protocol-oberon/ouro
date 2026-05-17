module Data.HJLD
( compile
, validate
) where

import qualified Data.HJLD.Parser as P
import           Data.Text        (Text, pack)
import           Text.Megaparsec  (errorBundlePretty)


-- Eventually this will output oberon code instead of just the AST
compile :: String -> Text -> Either String Text
compile filename input = case P.go filename input of
                             Left  err  -> Left (errorBundlePretty err)
                             Right expr -> Right (pack $ show expr)

validate :: String -> Text -> Either String String
validate filename input = case P.go filename input of
                              Left  err  -> Left (errorBundlePretty err)
                              Right expr -> Right (show expr )
