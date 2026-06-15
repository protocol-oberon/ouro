{-# LANGUAGE DataKinds #-}

module Utils where

import qualified Data.List.NonEmpty           as NE
import           Data.Maybe                   (fromJust)
import           Data.Ouro                    (CompilationResult (..),
                                               OuroError, compile,
                                               defaultOptions, toJSON)
import qualified Data.Ouro.Internal.Expr      as I
import qualified Data.Ouro.Internal.Kinds     as JLD
import           Data.Ouro.Lisp.Eval.Builtins (parseISO8601)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as TIO
import qualified Data.Text.Lazy.IO            as TLIO
import           Test.Hspec                   (expectationFailure, shouldBe)


runCompileInline :: String -> CompilationResult
runCompileInline = compile "Test Suite" . T.pack


shouldCompileTo :: FilePath -> FilePath -> IO ()
shouldCompileTo test trgt =
    do
    testContent <- TIO.readFile  test
    trgtContent <- TLIO.readFile trgt

    let res = compile test testContent

    case res of
        CompilationSuccess _ ast -> let json = toJSON defaultOptions ast
                                    in json `shouldBe` trgtContent

        CompilationFailure _ err -> expectationFailure $ show err


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


fixture :: FilePath -> FilePath
fixture path = "./test/fixtures/" <> path


-- Generates a complete, schema-less 'JLD.Primitive Object from a list of key-value pairs.
-- Maps perfectly to: ={ "k1" = v1, "k2" = v2 }=
mkObj :: [(String, I.Expr 'JLD.Primitive)] -> I.Expr 'JLD.Primitive
mkObj pairs = I.Object I.EmptyMeta (mkSpine pairs)


-- Helper to compile a list of raw pairs into binary 'JLD.List backbone.
mkSpine :: [(String, I.Expr 'JLD.Primitive)] -> I.Expr 'JLD.List
mkSpine []            = I.Nil
mkSpine ((k, val):xs) = I.Cons (I.Attr (T.pack k) val) (mkSpine xs)


mkDate :: String -> I.Expr JLD.Primitive
mkDate s = I.Date (fromJust $ parseISO8601 s)
