
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
import           System.FilePath   (replaceExtension, takeFileName, (</>))


runCommand :: Command -> IO ()
runCommand = \case
              Compile  ifp ofp -> runCompile  ifp ofp
              Validate c       -> runValidate c


runCompile :: FilePath -> Maybe FilePath -> IO ()
runCompile ifp mOutDir = do
    -- Calculate the actual output file path dynamically
    let ofp = case mOutDir of
                  Just dir -> dir </> replaceExtension (takeFileName ifp) "json"
                  Nothing  -> replaceExtension ifp "json"

    putStrLn $ "Compiling: " <> ifp <> "..."
    -- Read and process the input file
    content <- TIO.readFile ifp

    case O.compile ifp content of
        O.CompilationSuccess warnings code
            -> do
               -- Print all accumulated diagnostics (both warnings and non-fatal/harvested errors) up front
               mapM_ (pintDiagnostic ifp (T.unpack content). Left) warnings

               -- Proceed with serialization
               let opts = defaultOptions
               putStrLn $ "Compilation Success: " ++ ifp ++ " -> " ++ ofp
               TLIO.writeFile ofp (O.toJSON opts code)

        O.CompilationFailure warnings errors
            -> do
               -- Even in failure, print the warnings gathered up to the crash point
               mapM_ (pintDiagnostic ifp (T.unpack content) . Left) warnings

               -- Print the full harvest of architectural errors
               mapM_ (pintDiagnostic ifp (T.unpack content). Right) errors

               putStrLn $ "Compilation Failed: " ++ ifp ++ " due to compiler errors."


runValidate :: ValidateCommand -> IO ()
runValidate (ValidateCommand fp op sp sc) =
    do
    content <- TIO.readFile fp
    let targetOutPath = case op of
                            Just o  -> o
                            Nothing -> fp
    case sc of
        Just (Indent i) -> do
                           let opts = PrinterOptions { indentSpacing = i }
                           case O.validate fp content opts of
                               Left  err  -> putStrLn $ "Compilation Error:\n" ++ err
                               Right json -> do
                                             putStrLn $ "Validated JLD JSON for: " ++ fp
                                             case sp of
                                                 True  -> TIO.putStrLn $ TL.toStrict json
                                                 False -> pure ()

                                             TLIO.writeFile targetOutPath json

        Just (Merge _)  -> do
                           -- TODO!
                           pure ()

        Nothing         -> do
                           case O.validate fp content defaultOptions of
                               Left  err  -> putStrLn $ "Compilation Error:\n" ++ err
                               Right json -> do
                                             putStrLn $ "Validated JLD JSON for: " ++ fp
                                             case sp of
                                                 True  -> TIO.putStrLn $ TL.toStrict json
                                                 False -> pure ()

                                             TLIO.writeFile targetOutPath json
