
module CLI.Parser
( runParser
) where

import           CLI.Types           (Command (..), TargetTransform (..),
                                      ValidateCommand (..))
import           Data.Char           (toLower)
import           Data.Version        (showVersion)
import           Options.Applicative (Parser, argument, auto, command,
                                      customExecParser, eitherReader, fullDesc,
                                      header, help, helper, info, long, metavar,
                                      option, optional, prefs, progDesc, short,
                                      showHelpOnEmpty, showHelpOnError, str,
                                      strOption, subparser, switch, value,
                                      (<**>))
import           Paths_ouro          (version)
import           System.FilePath     (takeExtension)


-- Main Parser directly returns a Command
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
    pValidateCommand =  ValidateCommand
                    <$> pJsonFile
                    <*> optional pOutputFileOption
                    <*> switch ( long "print"
                              <> short 'p'
                              <> help "Print validation results"
                               )
                    <*> optional pTargetTransform

    pTargetTransform :: Parser TargetTransform
    pTargetTransform = subparser
                     ( command "indent" (info pIndent (progDesc "Format the JSON with specific indentation"))
                    <> command "merge"  (info pMerge  (progDesc "Merge another JSON file into the validation path"))
                     )

    pIndent :: Parser TargetTransform
    pIndent =  Indent
           <$> option auto ( long "spaces"
                          <> short 's'
                          <> metavar "INT"
                          <> help "Indentation spaces"
                          <> value 2
                           )

    pMerge :: Parser TargetTransform
    pMerge =  Merge
          <$> argument str ( metavar "MERGE_FILE"
                          <> help "The second JSON file to merge"
                           )


-- Reusable primitive parsers
pOutputDirOption :: Parser FilePath
pOutputDirOption = strOption ( long "output"
                            <> short 'o'
                            <> metavar "DIR"
                            <> help "Output directory"
                             )


pOutputFileOption :: Parser FilePath
pOutputFileOption = strOption ( long "output"
                             <> short 'o'
                             <> metavar "FILE"
                             <> help "Output file"
                              )


-- Validates that the provided string is a path ending in .json
pJsonFile :: Parser FilePath
pJsonFile = argument (eitherReader validateJsonPath) (metavar "file")
    where
    validateJsonPath path = case map toLower (takeExtension path) == ".json" of
                                True  -> Right path
                                False -> Left $ "Invalid input file '"
                                             ++ path
                                             ++ "'. Input must be a .json file."


pOuroFile :: Parser FilePath
pOuroFile = argument (eitherReader validateOuroPath) (metavar "SOURCE_FILE")
    where
    validateOuroPath path = case map toLower (takeExtension path) == ".ouro" of
                               True  -> Right path
                               False -> Left $ "Invalid compiler source target '"
                                            ++ path
                                            ++ "'. Input must be an Ouro Lisp (.ouro) file."


runParser :: IO Command
runParser = customExecParser pPrefs pInfo
    where
    pPrefs = prefs $ showHelpOnError <> showHelpOnEmpty
    pInfo  = info (pCommand <**> helper)
           $ header ("Ouro v" ++ showVersion version)
          <> fullDesc
