module Data.HJLD
( compile
, validate
) where

import qualified Data.HJLD.Parser     as P
import qualified Data.HJLD.Serializer as Ser
import           Data.Text            (Text, pack)
import qualified Data.Text.Lazy       as TL
import           Text.Megaparsec      (errorBundlePretty)
import Data.HJLD.Serializer (defaultOptions)


-- Eventually this will output oberon code instead of just the AST
compile :: String -> Text -> Either String Text
compile filename input = case P.go filename input of
                             Left  err  -> Left (errorBundlePretty err)
                             Right expr -> Right (pack $ show expr)

validate :: String -> Text -> Either String TL.Text
validate filename input = case P.go filename input of
                              Left  err  -> Left (errorBundlePretty err)
                              Right expr -> Right (Ser.toJSON defaultOptions expr)
