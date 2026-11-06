module CLI.Source
( readSource
) where

import           Control.Exception  (try)
import qualified Data.ByteString    as BS
import           Data.Functor       ((<&>))
import           Data.Ouro          (decodeSource)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           System.IO.Error    (ioeGetErrorString, isDoesNotExistError)


-- Reading source files from disk. The library reads none: it decodes the bytes it is given
-- (decodeSource), and the command line reads them.
--
-- GHC's text IO decodes with the locale's encoding, which on Windows is a code page such as
-- 850, so 'É' would be read as two characters. The bytes are read as they are and decoded as
-- Ouro source always is, as UTF-8. A file that cannot be read, as one that does not exist,
-- is an error, never an exception.
readSource :: FilePath -> IO (Either Text Text)
readSource path = try (BS.readFile path) <&> \case
    Left  failure -> Left ("'" <> T.pack path <> "' cannot be read: " <> reason failure <> ".")
    Right bytes   -> decodeSource path bytes
    where
    reason failure
        | isDoesNotExistError failure = "it does not exist"
        | otherwise                   = T.pack (ioeGetErrorString failure)
