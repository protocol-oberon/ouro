{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Data.HJLD           as HJLD
import           Data.Text           (pack)
import qualified Data.Text.IO        as TIO
import           Options.Applicative (Parser, argument, command,
                                      customExecParser, eitherReader, fullDesc,
                                      header, help, helper, info, long, metavar,
                                      optional, prefs, progDesc, short,
                                      showHelpOnEmpty, showHelpOnError,
                                      strOption, subparser, switch, (<**>))
import           System.FilePath     (replaceExtension, takeFileName, (</>), takeExtension)
import Data.Char (toLower)


-- Your Action *is* your configuration
data Action
    = Compile  FilePath (Maybe FilePath)       -- input, output
    -- Eventually Validate will only take 1 filepath and format inplace
    | Validate FilePath (Maybe FilePath) Bool  -- input, output, print
    deriving (Show)

-- Main Parser directly returns an Action
pAction :: Parser Action
pAction = subparser
    (  command "compile"  (info pCompile  (progDesc "Compile JSON-LD to Oberon"))
    <> command "validate" (info pValidate (progDesc "Validate JSON-LD"))
    )
    where
    pCompile :: Parser Action
    pCompile = Compile
        <$> pPathKeyword
        <*> optional pOutputDirOption


    pValidate :: Parser Action
    pValidate = Validate <$> pPathKeyword <*> optional pOutputFileOption <*> switch (long "print" <> short 'p')

-- Reusable primitive parsers
pOutputDirOption :: Parser FilePath
pOutputDirOption = strOption (long "output" <> short 'o' <> metavar "DIR" <> help "Output directory")

pOutputFileOption :: Parser FilePath
pOutputFileOption = strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output file")

pPathKeyword :: Parser FilePath
pPathKeyword = pLiteralPath *> pJsonFile
  where
    pLiteralPath :: Parser ()
    pLiteralPath = argument (eitherReader validateKeyword) (metavar "path")

    -- This now validates that the provided string is a path ending in .json
    pJsonFile :: Parser FilePath
    pJsonFile = argument (eitherReader validateJsonPath) (metavar "file")

    validateKeyword :: String -> Either String ()
    validateKeyword = \case
        "path" -> Right ()
        _      -> Left "Expected the literal keyword 'path'"

    validateJsonPath :: String -> Either String FilePath
    validateJsonPath path =
        -- map toLower handles .JSON, .Json, etc.
        if map toLower (takeExtension path) == ".json"
            then Right path
            else Left $ "Invalid input file '" ++ path ++ "'. Input must be a .json file."


runParser :: IO Action
runParser = customExecParser pPrefs pInfo
    where
    pPrefs = prefs $ showHelpOnError <> showHelpOnEmpty
    pInfo  = info (pAction <**> helper)
           $ header "HJLD v0.0.1"
          <> fullDesc


-- Update the signature: the second argument is now Maybe FilePath (the output directory)
runCompile :: FilePath -> Maybe FilePath -> IO ()
runCompile ifp mOutDir = do
    -- 1. Calculate the actual output file path dynamically
    let ofp = case mOutDir of
                Just dir -> dir </> replaceExtension (takeFileName ifp) "obn"
                Nothing  -> replaceExtension ifp "obn"

    -- 2. Read and process the input file
    content <- TIO.readFile ifp
    case HJLD.compile ifp content of
        Left err -> do
            putStrLn $ "Compilation Error: " ++ ifp
            putStrLn err

        Right oberonCode -> do
            putStrLn $ "Compilation Success: " ++ ifp ++ " -> " ++ ofp
            TIO.writeFile ofp oberonCode


runValidate :: FilePath -> Maybe FilePath -> Bool -> IO ()
runValidate inputFile outputFile shouldPrint = do
    content <- TIO.readFile inputFile
    case HJLD.validate inputFile content of
        Left  err -> putStrLn $ "Compilation Error:\n" ++ err
        Right ast -> do
            putStrLn $ "Validated JLD Ast for: " ++ inputFile
            case shouldPrint of
                True  -> putStrLn ast
                False -> pure ()

            case outputFile of
                -- Placeholder, this will be formatted JSON in the future
                Just o  -> TIO.writeFile o (pack ast)
                Nothing -> pure ()



main :: IO ()
main = do
    config <- runParser
    case config of
        Compile  ifp ofp    -> runCompile  ifp ofp
        Validate ifp ofp sp -> runValidate ifp ofp sp
