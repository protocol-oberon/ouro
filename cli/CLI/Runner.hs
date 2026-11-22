
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
import           Control.Exception (IOException, displayException, try)
import           System.Directory  (createDirectoryIfMissing, removeFile,
                                    renameFile)
import           System.Exit       (exitFailure)
import           System.FilePath   (takeDirectory)
import           System.IO         (IOMode (..), hPutStrLn, hSetEncoding,
                                    hSetNewlineMode, noNewlineTranslation, stderr,
                                    stdout, utf8, withFile)
import           Text.Megaparsec   (SourcePos, sourceName)


runCommand :: Command -> IO ()
runCommand = \case
              Build    dir graphs -> runBuild    dir graphs
              Validate c          -> runValidate c


-- Builds the project in a folder: the graphs chosen, every one when none is, are written to
-- <outputs>/<module>/<graph>.json, or, if anything fails, nothing is. That holds for writing
-- too (gap 35): every graph is staged beside its file, and moved into place only when all are.
runBuild :: FilePath -> [String] -> IO ()
runBuild dir graphs = do
    putStrLn $ "Building:            " <> dir <> " ..."
    result <- buildProject dir (map T.pack graphs) >>= either (\err -> TIO.hPutStrLn stderr err >> exitFailure) pure
    case result of
        BuildSuccess warnings outputs
            -> do
               sourceOf <- sourcesFor [] (map warnPos warnings)
               mapM_ (pintDiagnostic sourceOf . Left) warnings
               staged <- stageAll [ (file, O.toJSON defaultOptions code) | (file, code) <- outputs ]
               case staged of
                   Left (file, problem)
                       -> do
                          hPutStrLn stderr $ "Could not write " <> file <> ": " <> displayException problem
                          putStrLn "Build Failed: a document could not be written, so none was."
                          exitFailure
                   Right files
                       -> do
                          placed <- placeAll files
                          case placed of
                              Left (file, problem, written)
                                  -> do
                                     hPutStrLn stderr $ "Could not move " <> file <> " into place: " <> displayException problem
                                     putStrLn $ "Build Failed: " <> show written <> " of " <> show (length files) <> " documents were written before it."
                                     exitFailure
                              Right ()
                                  -> putStrLn $ "Build Success:       " <> counted (length outputs)

        BuildFailure warnings errors
            -> do
               sourceOf <- sourcesFor [] (map warnPos warnings <> map errPos (toList errors))
               mapM_ (pintDiagnostic sourceOf . Left) warnings
               mapM_ (pintDiagnostic sourceOf . Right) errors
               putStrLn "Build Failed: nothing was written, due to compiler errors."
               exitFailure


-- A number of documents, singular for one: "1 document", "3 documents"
counted :: Int -> String
counted = \case
    1 -> "1 document"
    n -> show n <> " documents"


runValidate :: ValidateCommand -> IO ()
runValidate (ValidateCommand fp op sp sc) =
    do
    content <- readSourceOrExit fp
    case sc of
        Just (Indent i) -> validated content (PrinterOptions { indentSpacing = i })

        Just (Merge _)  -> do
                           -- TODO! Fail loudly until merge is implemented
                           hPutStrLn stderr "Error: 'validate merge' is not implemented yet."
                           exitFailure

        Nothing         -> validated content defaultOptions
    where
    -- With -o the JSON is written to that file; without it, to stdout and nothing else, so it
    -- can be piped or redirected. The input is never written over. An error goes to stderr.
    validated content opts =
        case O.validate fp content opts of
            Left  err  -> do
                          hPutStrLn stderr $ "Compilation Error:\n" ++ err
                          exitFailure
            Right json -> case op of
                              Just o  -> do
                                         putStrLn $ "Validated JLD JSON for: " ++ fp
                                         case sp of
                                             True  -> TIO.putStrLn $ TL.toStrict json
                                             False -> pure ()
                                         writeJSON o json
                              Nothing -> do
                                         hSetEncoding    stdout utf8
                                         hSetNewlineMode stdout noNewlineTranslation
                                         TLIO.putStr     json


-- Reads a source file as UTF-8, whatever the platform's code page, or reports why it cannot
readSourceOrExit :: FilePath -> IO T.Text
readSourceOrExit fp = readSource fp >>= \case
    Right content -> pure content
    Left  err     -> TIO.hPutStrLn stderr err >> exitFailure


-- Writes each graph's JSON beside its file, as <file>.partial, making its folder if need be.
-- If one cannot be written, those staged are removed, and the file and why are the result.
stageAll :: [(FilePath, TL.Text)] -> IO (Either (FilePath, IOException) [(FilePath, FilePath)])
stageAll = go []
    where
    go done [] = pure (Right (reverse done))
    go done ((file, json) : rest) = do
        let partial = file <> ".partial"
        result <- try (createDirectoryIfMissing True (takeDirectory file) >> writeJSON partial json)
        case result of
            Right ()     -> go ((partial, file) : done) rest
            Left problem -> do
                            mapM_ (discard . fst) ((partial, file) : done)
                            pure (Left (file, problem))

    -- A staged file is removed if it is there; one that is not needs nothing
    discard partial = () <$ (try (removeFile partial) :: IO (Either IOException ()))

-- Moves each staged file into place, a rename within its folder, saying so for each. If one
-- cannot be moved, the result is that file, why, and how many were moved before it.
placeAll :: [(FilePath, FilePath)] -> IO (Either (FilePath, IOException, Int) ())
placeAll = go 0
    where
    go _ [] = pure (Right ())
    go written ((partial, file) : rest) = do
        result <- try (renameFile partial file)
        case result of
            Right ()     -> do
                            putStrLn $ "Wrote:               " <> file
                            go (written + 1) rest
            Left problem -> pure (Left (file, problem, written))


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
