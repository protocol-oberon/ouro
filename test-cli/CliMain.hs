module Main where

import           Hedgehog
import qualified Ouro.Cli.CommandTest as Command
import qualified Ouro.Cli.ProjectTest as Project
import           System.Exit          (exitFailure, exitSuccess)
import           Utils                (withUtf8Console)


-- The command line's tests: its project code tested from inside, and ouro itself, run as a
-- user runs it. The compiler's own tests are the ouro-test suite.
main :: IO ()
main = withUtf8Console $ do
    results <- mapM checkParallel
        [ -- Projects: package.ouro, the source folder, and the modules on disk
          Project.packageFiles
        , Project.projectsOnDisk
        , Project.modulesOnDisk
        , Project.sourceFiles
        -- The command line: ouro itself, run as a user runs it
        , Command.builds
        , Command.compiles
        , Command.validates
        -- Known gaps of the project: pinned as they are today; a fixed gap keeps its pin, run negated
        , Project.knownGaps
        ]

    case and results of
        True  -> exitSuccess
        False -> exitFailure
