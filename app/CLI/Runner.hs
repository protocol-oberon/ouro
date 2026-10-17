
module CLI.Runner
( runCommand
) where

import           CLI.Printer       (pintDiagnostic)
import           CLI.Types         (Command (..), TargetTransform (..),
                                    ValidateCommand (..))
import           Data.Ouro         (PrinterOptions (..), defaultOptions)
import qualified Data.Ouro         as O
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO
import           System.Exit       (exitFailure)
import           System.FilePath   (hasExtension, (</>))
import           System.IO         (IOMode (..), hSetEncoding, hSetNewlineMode,
                                    noNewlineTranslation, stderr, utf8,
                                    withFile)


runCommand :: Command -> IO ()
runCommand = \case
              Compile  ifp trgt ofp -> runCompile  ifp trgt ofp
              Validate c            -> runValidate c


runCompile :: FilePath -> String -> Maybe FilePath -> IO ()
runCompile ifp trgt mOutDir = do
    -- Calculate the actual output file path dynamically
    let ofp = case mOutDir of
                  Just path -> case hasExtension path of
                                   True  -> path
                                   False -> path </> (trgt <> ".json")
                  Nothing   -> trgt <> ".json"

    putStrLn $ "Compiling:           "<> ifp <> " [" <> trgt <> "] ..."
    -- Read and process the input file
    content <- readSourceOrExit ifp

    case O.compile ifp trgt content of
        O.CompilationSuccess warnings code
            -> do
               -- Print all accumulated diagnostics (both warnings and non-fatal/harvested errors) up front
               mapM_ (pintDiagnostic ifp (T.unpack content). Left) warnings

               -- Proceed with serialization
               let opts = defaultOptions
               putStrLn $ "Compilation Success: " ++ ifp ++ " [" ++ trgt ++ "] -> " ++ ofp
               writeJSON ofp (O.toJSON opts code)

        O.CompilationFailure warnings errors
            -> do
               -- Even in failure, print the warnings gathered up to the crash point
               mapM_ (pintDiagnostic ifp (T.unpack content) . Left) warnings

               -- Print the full harvest of architectural errors
               mapM_ (pintDiagnostic ifp (T.unpack content). Right) errors

               putStrLn $ "Compilation Failed: " ++ ifp ++ " due to compiler errors."
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
readSourceOrExit fp = O.readSource fp >>= \case
    Right content -> pure content
    Left  err     -> TIO.hPutStrLn stderr err >> exitFailure


-- Writes JSON as UTF-8 with LF line endings, regardless of platform defaults
writeJSON :: FilePath -> TL.Text -> IO ()
writeJSON path json = withFile path WriteMode $ \h -> do
    hSetEncoding    h utf8
    hSetNewlineMode h noNewlineTranslation
    TLIO.hPutStr    h json
