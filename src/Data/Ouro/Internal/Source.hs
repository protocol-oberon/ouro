module Data.Ouro.Internal.Source
( decodeSource
, readSource
) where

import           Control.Exception  (try)
import qualified Data.ByteString    as BS
import           Data.Functor       ((<&>))
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import           System.IO.Error    (ioeGetErrorString, isDoesNotExistError)


-- Reading source files.
--
-- Ouro source is always UTF-8. GHC's text IO decodes with the locale's encoding, which on
-- Windows is a code page such as 850, so 'É' would be read as two characters. The bytes
-- are read as they are and decoded as UTF-8 instead. A leading byte order mark, which some
-- Windows editors write, is dropped, and CRLF line endings become LF, so a file reads the
-- same on every platform. A file that is not valid UTF-8 is an error, not garbled text, and
-- so is a file that cannot be read, as one that does not exist: never an exception.
readSource :: FilePath -> IO (Either Text Text)
readSource path = try (BS.readFile path) <&> \case
    Left  failure -> Left ("'" <> T.pack path <> "' cannot be read: " <> reason failure <> ".")
    Right bytes   -> decodeSource path bytes
    where
    reason failure
        | isDoesNotExistError failure = "it does not exist"
        | otherwise                   = T.pack (ioeGetErrorString failure)

decodeSource :: FilePath -> BS.ByteString -> Either Text Text
decodeSource path bytes =
    case TE.decodeUtf8' (fromMaybe bytes (BS.stripPrefix byteOrderMark bytes)) of
        Left  _    -> Left ("'" <> T.pack path <> "' is not valid UTF-8. Ouro source files are read as UTF-8: perhaps save it with that encoding.")
        Right text -> Right (T.replace "\r\n" "\n" text)
    where
    byteOrderMark = BS.pack [0xEF, 0xBB, 0xBF]
