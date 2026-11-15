
module CLI.Types
( Command (..)
, ValidateCommand (..)
, TargetTransform (..)
) where


-- Configuration
data Command
    = Build    FilePath [String]  -- The project's folder, and the graphs chosen by -g
    | Validate ValidateCommand
    deriving (Show)


data ValidateCommand = ValidateCommand
    { valInputFile  :: FilePath
    , valOutputFile :: Maybe FilePath  -- If Nothing, the JSON is written to stdout
    , valPrint      :: Bool
    , valTransform  :: Maybe TargetTransform
    } deriving (Show)


data TargetTransform
    = Indent Int
    | Merge  FilePath  -- Second input fp
    deriving (Show)
