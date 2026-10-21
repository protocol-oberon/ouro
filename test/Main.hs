{-# LANGUAGE CPP #-}

module Main where

import           Hedgehog
import qualified Ouro.CanonicalTest  as Canonical
import qualified Ouro.EvaluationTest as Evaluation
import qualified Ouro.FunctionTest   as Function
import qualified Ouro.LanguageTest   as Language
import qualified Ouro.LetTest        as Let
import qualified Ouro.ScopingTest    as Scoping
import qualified Ouro.StructuralTest as Structural
import           System.Exit         (exitFailure, exitSuccess)
#if defined(mingw32_HOST_OS)
import           Control.Exception    (IOException, bracket_, catch)
import           System.Win32.Console (getConsoleOutputCP, setConsoleOutputCP)
#endif


main :: IO ()
main = withUtf8Console $ do
    -- mapM runs checkParallel on each Group and returns a list of Bools [True, True, True]
    results <- mapM checkParallel
        [ Canonical.tests
        , Evaluation.tests
        , Structural.tests
        -- Scoping redesign: each step adds its group here when it lands
        , Scoping.baseline
        , Scoping.valuePatterns
        , Scoping.lexicalRecords
        , Scoping.callSiteArguments
        , Scoping.lexicalFunctions
        , Scoping.valueGetAndErrors
        , Scoping.cleanup
        -- Functions: bodies are one result expression, names are plain symbols, signatures, placeholders, map, patterns, docs
        , Function.expressionBodies
        , Function.plainNames
        , Function.signatures
        , Function.placeholders
        , Function.mapping
        , Function.destructuring
        , Function.threading
        , Function.documentation
        -- Bindings: let, fn and defn replace define and defun
        , Let.letAndFn
        -- The language as specified: what reaches the output, special forms and built-ins
        , Language.serialisation
        , Language.formsAndBuiltins
        , Language.returnedFunctions
        , Language.recordKeys
        , Language.types
        , Language.operators
        , Language.dates
        , Language.classification
        , Language.encoding
        , Language.strings
        , Language.logicAndCollections
        , Language.conditions
        , Language.contexts
        , Language.contextAlgebra
        -- Known gaps: pinned as they are today; a fix flips its test
        , Language.knownGaps
        ]

    -- If all groups passed (True), exit gracefully. Otherwise, fail the CI.
    case and results of
        True  -> exitSuccess
        False -> exitFailure


-- Hedgehog writes its report as UTF-8 (✓, ━, ┃), but a Windows console decodes what it is
-- sent with its own code page, such as 850. As in ouro's own main, the console is switched
-- to UTF-8 (65001) while the tests run and restored afterwards; without a console (output
-- redirected) nothing is changed.
withUtf8Console :: IO a -> IO a
#if defined(mingw32_HOST_OS)
withUtf8Console action = do
    previous <- getConsoleOutputCP
    case previous of
        0           -> action
        _hasConsole -> bracket_ (trySet 65001) (trySet previous) action
    where
    trySet codePage = setConsoleOutputCP codePage `catch` \(_ :: IOException) -> pure ()
#else
withUtf8Console = id
#endif
