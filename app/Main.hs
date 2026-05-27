{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Data.Char           (toLower)
import           Data.Ouro           (PrinterOptions (..), defaultOptions)
import qualified Data.Ouro           as Ob
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Lazy      as TL
import qualified Data.Text.Lazy.IO   as TLIO
import           Options.Applicative (Parser, argument, auto, command,
                                      customExecParser, eitherReader, fullDesc,
                                      header, help, helper, info, long, metavar,
                                      option, optional, prefs, progDesc, short,
                                      showHelpOnEmpty, showHelpOnError, str,
                                      strOption, subparser, switch, value,
                                      (<**>))
import           System.FilePath     (replaceExtension, takeExtension,
                                      takeFileName, (</>))


-- Configuration
data Command
    = Compile  FilePath (Maybe FilePath)
    | Validate ValidateCommand
    deriving (Show)


data ValidateCommand = ValidateCommand
    { valInputFile  :: FilePath
    , valOutputFile :: Maybe FilePath  -- If Nothing, we format inline
    , valPrint      :: Bool
    , valTransform  :: Maybe TargetTransform
    } deriving (Show)


data TargetTransform
    = Indent Int
    | Merge  FilePath  -- Second input fp
    deriving (Show)


-- Main Parser directly returns an Command
pCommand :: Parser Command
pCommand = subparser
         (  command "compile"  (info pCompile  (progDesc "Compile JSON-LD to Ouro"))
         <> command "validate" (info pValidate (progDesc "Validate JSON-LD"))
         )


pCompile :: Parser Command
pCompile =  Compile
        <$> pOuroFile
        <*> optional pOutputDirOption


pValidate :: Parser Command
pValidate = Validate <$> pValidateCommand
  where
    pValidateCommand :: Parser ValidateCommand
    pValidateCommand = ValidateCommand
                   <$> pJsonFile
                   <*> optional pOutputFileOption
                   <*> switch (long "print" <> short 'p' <> help "Print validation results")
                   <*> optional pTargetTransform

    -- The subcommands now ONLY parse their specific configuration settings
    pTargetTransform :: Parser TargetTransform
    pTargetTransform = subparser
                     (  command "indent" (info pIndent (progDesc "Format the JSON with specific indentation"))
                     <> command "merge"  (info pMerge  (progDesc "Merge another JSON file into the validation path"))
                     )

    pIndent :: Parser TargetTransform
    pIndent =  Indent
           <$> option auto (long "spaces" <> short 's' <> metavar "INT" <> help "Indentation spaces" <> value 2)

    pMerge :: Parser TargetTransform
    pMerge =  Merge
          <$> argument str (metavar "MERGE_FILE" <> help "The second JSON file to merge")


-- Reusable primitive parsers
pOutputDirOption :: Parser FilePath
pOutputDirOption = strOption (long "output" <> short 'o' <> metavar "DIR" <> help "Output directory")


pOutputFileOption :: Parser FilePath
pOutputFileOption = strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output file")


-- This now validates that the provided string is a path ending in .json
pJsonFile :: Parser FilePath
pJsonFile = argument (eitherReader validateJsonPath) (metavar "file")
    where
    validateJsonPath path = case map toLower (takeExtension path) == ".json" of
                                True  -> Right path
                                False -> Left $ "Invalid input file '" ++ path ++ "'. Input must be a .json file."


pOuroFile :: Parser FilePath
pOuroFile = argument (eitherReader validateOuroPath) (metavar "SOURCE_FILE")
    where
    validateOuroPath path = case map toLower (takeExtension path) == ".ouro" of
                               True  -> Right path
                               False -> Left $ "Invalid compiler source target '" ++ path ++ "'. Input must be an Ouro Lisp (.ouro) file."


runParser :: IO Command
runParser = customExecParser pPrefs pInfo
    where
    pPrefs = prefs $ showHelpOnError <> showHelpOnEmpty
    pInfo  = info (pCommand <**> helper)
           $ header "Ouro v0.0.1"
          <> fullDesc


-- Update the signature: the second argument is now Maybe FilePath (the output directory)
runCompile :: FilePath -> Maybe FilePath -> IO ()
runCompile ifp mOutDir = do
                         -- 1. Calculate the actual output file path dynamically
                         let ofp = case mOutDir of
                                      Just dir -> dir </> replaceExtension (takeFileName ifp) "ouro"
                                      Nothing  -> replaceExtension ifp "json"

                         -- 2. Read and process the input file
                         content <- TIO.readFile ifp
                         case Ob.compile ifp content of
                             Left  err  -> do
                                           putStrLn $ "Compilation Error: " ++ ifp
                                           putStrLn err

                             Right code -> do
                                           let opts = defaultOptions
                                           putStrLn $ "Compilation Success: " ++ ifp ++ " -> " ++ ofp
                                           print code
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


main :: IO ()
main = runParser >>= \case
                      Compile  ifp ofp -> runCompile  ifp ofp
                      Validate c       -> runValidate c
