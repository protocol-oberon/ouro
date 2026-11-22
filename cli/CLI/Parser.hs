
module CLI.Parser
( runParser
) where

import           CLI.Types           (Command (..), TargetTransform (..),
                                      ValidateCommand (..))
import           Data.Char           (toLower)
import           Data.Version        (showVersion)
import           Options.Applicative (Parser, argument, auto, command,
                                      customExecParser, eitherReader, fullDesc,
                                      header, help, helper, info, infoOption,
                                      long, many, metavar, option, optional,
                                      prefs, progDesc, short, showHelpOnEmpty,
                                      showHelpOnError, str, strOption,
                                      subparser, switch, value, (<**>))
import           Paths_ouro          (version)
import           System.FilePath     (takeExtension)


-- Main Parser directly returns a Command
pCommand :: Parser Command
pCommand = subparser
         (  command "build"    (info (pBuild    <**> helper) (progDesc "Build the documents of the project in a folder, with the settings of its package.ouro, if it has one"))
         <> command "validate" (info (pValidate <**> helper) (progDesc "Validate JSON-LD"))
         )


-- The project's folder, the current one when not given, and the graphs chosen, every one when
-- none is: -t people/person, as often as needed
pBuild :: Parser Command
pBuild =  Build
      <$> argument str ( metavar "DIR"
                      <> help "The project's folder, the current one when not given"
                      <> value "."
                       )
      <*> many (strOption ( long "target"
                         <> short 't'
                         <> metavar "MODULE/DOCUMENT"
                         <> help "A document to build, as people/person; may be given more than once, and replaces :documents"
                          ))


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

pVersion :: Parser (a -> a)
pVersion = infoOption ("Ouro v" <> showVersion version)
           (  long "version"
           <> short 'v'
           <> help "Show Ouro version"
           )


-- Reusable primitive parsers
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


runParser :: IO Command
runParser = customExecParser pPrefs pInfo
    where
    pPrefs = prefs $ showHelpOnError <> showHelpOnEmpty
    pInfo  = info (pCommand <**> helper <**> pVersion)
           $ header ("Ouro v" ++ showVersion version)
          <> fullDesc
