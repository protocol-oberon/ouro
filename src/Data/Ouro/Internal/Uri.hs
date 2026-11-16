module Data.Ouro.Internal.Uri
( Uri
, uriText
, uriParsed
, mkUri
, uriParser
) where

import           Control.Exception (SomeException)
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Text.Megaparsec   (MonadParsec, match)
import qualified Text.URI          as URI


-- Uri.
--
-- A URI as it was written. It is checked as a URI when it is made, and written out as the
-- text it was made from, never re-rendered from its parts: rendering would change the text,
-- dropping an empty root path, https://a.org/, and empty path segments, a//b (gaps 17, 22).
-- Two are equal when their text is.
data Uri = Uri
    { uriText   :: !Text
    , uriParsed :: !URI.URI   -- Its parts, to ask for its scheme
    }

instance Eq Uri where
    a == b = uriText a == uriText b

instance Show Uri where
    show = T.unpack . uriText


-- A URI from its text, or why the text is not one
mkUri :: Text -> Either SomeException Uri
mkUri text = Uri text <$> URI.mkURI text


-- A URI read by a parser, kept as the text it was read from
uriParser :: MonadParsec e Text m => m Uri
uriParser = uncurry Uri <$> match URI.parser
