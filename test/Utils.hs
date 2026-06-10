{-# LANGUAGE DataKinds #-}

module Utils where

import qualified Data.List.NonEmpty       as NE
import           Data.Ouro                (CompilationResult (..), OuroError,
                                           compile)
import qualified Data.Ouro.Internal.Expr  as I
import qualified Data.Ouro.Internal.Kinds as JLD
import qualified Data.Text                as T
import           Test.Hspec               (expectationFailure, shouldBe)
import Data.Maybe (fromJust)
import Data.Ouro.Lisp.Eval.Builtins (parseISO8601)


runCompileInline :: String -> CompilationResult
runCompileInline test = compile "Test Suite" (T.pack test)


shouldEvalTo :: String -> I.Expr 'JLD.Primitive -> IO ()
shouldEvalTo input expected =
    case runCompileInline input of
        CompilationSuccess _ val -> val `shouldBe` expected
        CompilationFailure _ err -> expectationFailure $ show err


shouldFailTo :: String -> OuroError -> IO ()
shouldFailTo input expected =
    case runCompileInline input of
        CompilationSuccess _ val -> expectationFailure $ "Compilation somehow succeeded with value: " <> show val
        CompilationFailure _ err -> (NE.head err) `shouldBe` expected


-- Generates a complete, schema-less 'JLD.Primitive Object from a list of key-value pairs.
-- Maps perfectly to: ={ "k1" = v1, "k2" = v2 }=
mkObj :: [(String, I.Expr 'JLD.Primitive)] -> I.Expr 'JLD.Primitive
mkObj pairs = I.Object I.EmptyMeta (mkSpine pairs)


-- Helper to compile a list of raw pairs into binary 'JLD.List backbone.
mkSpine :: [(String, I.Expr 'JLD.Primitive)] -> I.Expr 'JLD.List
mkSpine [] = I.Nil
mkSpine ((k, val):xs) = I.Cons (I.Attr (T.pack k) val) (mkSpine xs)


mkDate :: String -> UTC.Time
mkDate s = I.Date (fromJust $ parseISO8601 s)
