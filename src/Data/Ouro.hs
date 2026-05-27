{-# LANGUAGE DataKinds #-}

module Data.Ouro
( compile
, validate
, defaultOptions
, PrinterOptions(..)
, Ser.toJSON
) where

import           Data.Function              ((&))
import qualified Data.Ouro.Internal.Expr    as I
import qualified Data.Ouro.Internal.Kinds   as JLD
import qualified Data.Ouro.Json.Parser      as JP
import           Data.Ouro.Json.Serializer  (PrinterOptions (..),
                                             defaultOptions)
import qualified Data.Ouro.Json.Serializer  as Ser
import qualified Data.Ouro.Lisp.Canon       as Canon
import qualified Data.Ouro.Lisp.Eval.Engine as EN
import qualified Data.Ouro.Lisp.Lexer       as LX
import qualified Data.Ouro.Lisp.Parser      as LP
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as TL
import           Text.Megaparsec            (errorBundlePretty)


-- Complete frontend pipeline compilation pass.
compile :: String -> Text -> Either String (I.Expr 'JLD.Primitive)
compile filename input = do
                         -- 1. Lexical Pass (Map Megaparsec errors to Strings)
                         tokens     <- LX.tokenize filename input
                                       & either (Left . errorBundlePretty) Right
                         -- 2. Syntactic Pass
                         surfaceAST <- LP.parse tokens
                         -- Insert your brand new canonicalization pass here!
                         let cleanAST = Canon.construct surfaceAST
                         -- 3. Semantic Pass (Routing, Lazy Env Bindings, and Type Verifications)
                         EN.evaluateRoot cleanAST


validate :: String -> Text -> PrinterOptions -> Either String TL.Text
validate filename input opts = case JP.go filename input of
                                   Left  err  -> Left (errorBundlePretty err)
                                   Right expr -> Right (Ser.toJSON opts expr)
