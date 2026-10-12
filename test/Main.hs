
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


main :: IO ()
main = do
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
        -- Functions: bodies are one result expression, names are plain symbols
        , Function.expressionBodies
        , Function.plainNames
        -- Bindings: let, fn and defn replace define and defun
        , Let.letAndFn
        -- The language as specified: what reaches the output, special forms and built-ins
        , Language.serialisation
        , Language.formsAndBuiltins
        , Language.returnedFunctions
        -- Known gaps: pinned as they are today; a fix flips its test
        , Language.knownGaps
        ]

    -- If all groups passed (True), exit gracefully. Otherwise, fail the CI.
    case and results of
        True  -> exitSuccess
        False -> exitFailure
