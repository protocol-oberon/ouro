
module CLI.Types
( Command (..)
, ValidateCommand (..)
, TargetTransform (..)
) where


-- Configuration
data Command
    = Compile  FilePath String (Maybe FilePath)
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
