module Main where

import           Hedgehog
import qualified Ouro.AatTest        as Aat
import qualified Ouro.AnalysisTest   as Analysis
import qualified Ouro.BuildTest      as Build
import qualified Ouro.CanonicalTest  as Canonical
import qualified Ouro.EvaluationTest as Evaluation
import qualified Ouro.FunctionTest   as Function
import qualified Ouro.ImportTest     as Import
import qualified Ouro.LanguageTest   as Language
import qualified Ouro.LetTest        as Let
import qualified Ouro.ScopingTest    as Scoping
import qualified Ouro.StdTest        as Std
import qualified Ouro.StructuralTest as Structural
import           System.Exit         (exitFailure, exitSuccess)
import           Utils               (withUtf8Console)


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
        , Function.adjoining
        , Function.constants
        , Function.documentation
        -- Modules: import, export and the bundled std
        , Import.imports
        , Import.importedModules
        , Import.moduleErrors
        , Import.sourceFrames
        -- Builds: every graph of every module, keyed by module and graph
        , Build.builds
        , Build.chosenGraphs
        -- The bundled std: Linked Art's patterns and required terms
        , Std.base
        , Std.time
        , Std.vocab
        , Std.object
        , Std.actor
        , Std.provenance
        , Std.exhibition
        , Std.digital
        , Std.everyModule
        -- The bundled aat: Linked Art's recommended terms, generated from its table
        , Aat.recommendedTerms
        -- Static analysis: every module checked as written, before anything is evaluated
        , Analysis.names
        , Analysis.calls
        , Analysis.recursion
        , Analysis.descending
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
        , Language.endOfFile
        , Language.strings
        , Language.stringBuiltins
        , Language.logicAndCollections
        , Language.conditions
        , Language.contexts
        , Language.contextAlgebra
        , Language.specialFormsAreBindings
        -- Known gaps: pinned as they are today; a fixed gap keeps its pin, run negated
        , Language.knownGaps
        ]

    -- If all groups passed (True), exit gracefully. Otherwise, fail the CI.
    case and results of
        True  -> exitSuccess
        False -> exitFailure
