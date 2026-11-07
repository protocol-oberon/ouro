
module CLI.Runner
( runCommand
) where

import           CLI.Printer       (pintDiagnostic)
import           CLI.Project       (buildProject)
import           CLI.Source        (readSource)
import           CLI.Types         (Command (..), TargetTransform (..),
                                    ValidateCommand (..))
import           Control.Monad     (join)
import           Data.Foldable     (toList)
import           Data.List         (nub)
import           Data.Ouro         (OuroError (..), OuroWarning (..),
                                    PrinterOptions (..), defaultOptions)
import qualified Data.Ouro         as O
import           Data.Ouro.Build   (BuildResult (..))
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO
import           System.Directory  (createDirectoryIfMissing)
import           System.Exit       (exitFailure)
import           System.FilePath   (takeDirectory)
import           System.IO         (IOMode (..), hSetEncoding, hSetNewlineMode,
                                    noNewlineTranslation, stderr, utf8,
                                    withFile)
import           Text.Megaparsec   (SourcePos, sourceName)


runCommand :: Command -> IO ()
runCommand = \case
              Build    dir graphs -> runBuild    dir graphs
              Validate c          -> runValidate c


-- Builds the project in a folder: the graphs chosen, every one when none is, are written to
-- <outputs>/<module>/<graph>.json, or, if anything fails, nothing is
runBuild :: FilePath -> [String] -> IO ()
runBuild dir graphs = do
    putStrLn $ "Building:            " <> dir <> " ..."
    result <- buildProject dir (map T.pack graphs) >>= either (\err -> TIO.hPutStrLn stderr err >> exitFailure) pure
    case result of
        BuildSuccess warnings outputs
            -> do
               sourceOf <- sourcesFor [] (map warnPos warnings)
               mapM_ (pintDiagnostic sourceOf . Left) warnings
               mapM_ (\(file, code) -> do
                          createDirectoryIfMissing True (takeDirectory file)
                          writeJSON file (O.toJSON defaultOptions code)
                          putStrLn $ "Wrote:               " <> file)
                     outputs
               putStrLn $ "Build Success:       " <> show (length outputs) <> " graphs"

        BuildFailure warnings errors
            -> do
               sourceOf <- sourcesFor [] (map warnPos warnings <> map errPos (toList errors))
               mapM_ (pintDiagnostic sourceOf . Left) warnings
               mapM_ (pintDiagnostic sourceOf . Right) errors
               putStrLn "Build Failed: nothing was written, due to compiler errors."
               exitFailure


runValidate :: ValidateCommand -> IO ()
runValidate (ValidateCommand fp op sp sc) =
    do
    content <- readSourceOrExit fp
    let targetOutPath = case op of
                            Just o  -> o
                            Nothing -> fp
    case sc of
        Just (Indent i) -> do
                           let opts = PrinterOptions { indentSpacing = i }
                           case O.validate fp content opts of
                               Left  err  -> do
                                             putStrLn $ "Compilation Error:\n" ++ err
                                             exitFailure
                               Right json -> do
                                             putStrLn $ "Validated JLD JSON for: " ++ fp
                                             case sp of
                                                 True  -> TIO.putStrLn $ TL.toStrict json
                                                 False -> pure ()

                                             writeJSON targetOutPath json

        Just (Merge _)  -> do
                           -- TODO! Fail loudly until merge is implemented
                           putStrLn "Error: 'validate merge' is not implemented yet."
                           exitFailure

        Nothing         -> do
                           case O.validate fp content defaultOptions of
                               Left  err  -> do
                                             putStrLn $ "Compilation Error:\n" ++ err
                                             exitFailure
                               Right json -> do
                                             putStrLn $ "Validated JLD JSON for: " ++ fp
                                             case sp of
                                                 True  -> TIO.putStrLn $ TL.toStrict json
                                                 False -> pure ()

                                             writeJSON targetOutPath json


-- Reads a source file as UTF-8, whatever the platform's code page, or reports why it cannot
readSourceOrExit :: FilePath -> IO T.Text
readSourceOrExit fp = readSource fp >>= \case
    Right content -> pure content
    Left  err     -> TIO.hPutStrLn stderr err >> exitFailure


-- Writes JSON as UTF-8 with LF line endings, regardless of platform defaults
writeJSON :: FilePath -> TL.Text -> IO ()
writeJSON path json = withFile path WriteMode $ \h -> do
    hSetEncoding    h utf8
    hSetNewlineMode h noNewlineTranslation
    TLIO.hPutStr    h json


-- The sources of every file a diagnostic is reported in, each read once: those already read,
-- such as the file compiled, a module of the project, or a bundled module, whose source is in
-- the compiler itself
sourcesFor :: [(FilePath, T.Text)] -> [SourcePos] -> IO (FilePath -> Maybe T.Text)
sourcesFor known positions = do
    others <- traverse (\file -> (file,) <$> readOther file) (nub [ sourceName pos | pos <- positions, sourceName pos `notElem` map fst known ])
    pure $ \file -> maybe (join (lookup file others)) Just (lookup file known)
    where
    readOther file = case O.bundledFile file of
                         Just source -> pure (Just source)
                         Nothing     -> either (const Nothing) Just <$> readSource file
