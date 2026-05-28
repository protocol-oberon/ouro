
module CLI.Runner
( runCommand
) where

import           CLI.Printer       (pintDiagnostic)
import           CLI.Types         (Command (..), TargetTransform (..),
                                    ValidateCommand (..))
import           Data.Ouro         (OuroDiagnostic (..), PrinterOptions (..),
                                    defaultOptions)
import qualified Data.Ouro         as Ob
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO
import           System.FilePath   (replaceExtension, takeFileName, (</>))


runCommand :: Command -> IO ()
runCommand = \case
               Compile  ifp ofp -> runCompile  ifp ofp
               Validate c       -> runValidate c


-- Update the signature: the second argument is now Maybe FilePath (the output directory)
runCompile :: FilePath -> Maybe FilePath -> IO ()
runCompile ifp mOutDir = do
    -- 1. Calculate the actual output file path dynamically
    let ofp = case mOutDir of
                  Just dir -> dir </> replaceExtension (takeFileName ifp) "ouro"
                  Nothing  -> replaceExtension ifp "json"

    putStrLn $ "Compiling: " <> ifp <> "..."
    -- 2. Read and process the input file
    content <- TIO.readFile ifp
    case Ob.compile ifp content of
        Left err -> do
            -- Wrap the fatal failure in the unified DiagnosticError constructor
            pintDiagnostic ifp (T.unpack content) (DiagnosticError err)

        Right (warnings, code) -> do
            -- 3. Print all accumulated non-fatal lint diagnostics first
            mapM_ (pintDiagnostic ifp (T.unpack content)) warnings

            -- 4. Proceed with serialization and saving output
            let opts = defaultOptions
            putStrLn $ "Compilation Success: " ++ ifp ++ " -> " ++ ofp
            -- print code
            TLIO.writeFile ofp (Ob.toJSON opts code)



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
                           case Ob.validate fp content opts of
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
                           case Ob.validate fp content defaultOptions of
                               Left  err  -> putStrLn $ "Compilation Error:\n" ++ err
                               Right json -> do
                                             putStrLn $ "Validated JLD JSON for: " ++ fp
                                             case sp of
                                                 True  -> TIO.putStrLn $ TL.toStrict json
                                                 False -> pure ()

                                             TLIO.writeFile targetOutPath json
