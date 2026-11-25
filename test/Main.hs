module Main where

import           Hedgehog
import           Hedgehog.Internal.Property (GroupName (..))
import qualified Ouro.AatTest        as Aat
import qualified Ouro.AnalysisTest   as Analysis
import qualified Ouro.BuildTest      as Build
import qualified Ouro.EvaluationTest as Evaluation
import qualified Ouro.FixtureTest    as Fixture
import qualified Ouro.FunctionTest   as Function
import qualified Ouro.ImportTest     as Import
import qualified Ouro.LanguageTest   as Language
import qualified Ouro.LetTest        as Let
import qualified Ouro.RefinementTest as Refinement
import qualified Ouro.ScopingTest    as Scoping
import qualified Ouro.StdTest        as Std
import qualified Ouro.StructuralTest as Structural
import           System.Exit         (exitFailure, exitSuccess)
import           Utils               (withUtf8Console)


main :: IO ()
main = withUtf8Console $ do
    -- The fixtures are found by listing their folder
    fixtures <- Fixture.tests

    -- mapM runs checkParallel on each Group, in parts, and returns a list of Bools [True, True, True]
    results <- mapM checkParallel . concatMap inParts $ fixtures <>
        [ Evaluation.tests
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
        , Function.properties
        , Function.assertions
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
        -- Refinements: a failure's role, blame and trail, and the tree of built-in refinements
        , Refinement.requirements
        , Refinement.kinds
        , Language.durations
        -- Known gaps: pinned as they are today; a fixed gap keeps its pin, run negated
        , Language.knownGaps
        ]

    -- If all groups passed (True), exit gracefully. Otherwise, fail the CI.
    case and results of
        True  -> exitSuccess
        False -> exitFailure


-- A group in parts of at most 25 properties, each named after it with its number, "Name (2/3)".
-- Hedgehog redraws a running group in place, in a terminal, and a group taller than the
-- terminal cannot be redrawn, which garbles the output below it.
inParts :: Group -> [Group]
inParts (Group (GroupName name) properties) =
    case chunks properties of
        [one] -> [Group (GroupName name) one]
        parts -> zipWith part [1 :: Int ..] parts
                 where part i = Group (GroupName (name <> " (" <> show i <> "/" <> show (length parts) <> ")"))
    where
    chunks [] = []
    chunks xs = take 25 xs : chunks (drop 25 xs)
