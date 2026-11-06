module Data.Ouro.Internal.Source
( decodeSource
) where

import qualified Data.ByteString    as BS
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE


-- Decoding source files. The library reads no files: whoever reads one, the command line or
-- a program embedding Ouro, decodes its bytes here, so a file reads the same wherever it is
-- compiled.
--
-- Ouro source is always UTF-8. A leading byte order mark, which some Windows editors write,
-- is dropped, and CRLF line endings become LF, so a file reads the same on every platform. A
-- file that is not valid UTF-8 is an error, not garbled text.
decodeSource :: FilePath -> BS.ByteString -> Either Text Text
decodeSource path bytes =
    case TE.decodeUtf8' (fromMaybe bytes (BS.stripPrefix byteOrderMark bytes)) of
        Left  _    -> Left ("'" <> T.pack path <> "' is not valid UTF-8. Ouro source files are read as UTF-8: perhaps save it with that encoding.")
        Right text -> Right (T.replace "\r\n" "\n" text)
    where
    byteOrderMark = BS.pack [0xEF, 0xBB, 0xBF]
