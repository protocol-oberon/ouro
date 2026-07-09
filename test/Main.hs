
module Main where

import           Hedgehog
import qualified Ouro.CanonicalTest  as Canonical
import qualified Ouro.EvaluationTest as Evaluation
import qualified Ouro.StructuralTest as Structural
import           System.Exit         (exitFailure, exitSuccess)


main :: IO ()
main = do
    -- mapM runs checkParallel on each Group and returns a list of Bools [True, True, True]
    results <- mapM checkParallel
        [ Canonical.tests
        , Evaluation.tests
        , Structural.tests
        ]

    -- If all groups passed (True), exit gracefully. Otherwise, fail the CI.
    case and results of
        True  -> exitSuccess
        False -> exitFailure
