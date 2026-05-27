
module Data.Ouro.Internal.Schema where

import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Text.URI  as URI


newtype Schema = Schema [SchemaDirective]
    deriving Eq

instance Show Schema where
    show = \case
            Schema []         -> "Schema []"
            Schema directives -> "Schema [\n" ++ indentedDirectives directives ++ "\n]"
            where
            -- Formats and indents each directive line
            indentedDirectives xs = intercalate ",\n" (map (\sd -> "  " ++ show sd) xs)


instance Semigroup Schema where
    (Schema a) <> (Schema b) = Schema (a ++ b)


instance Monoid Schema where
    mempty = Schema []


toList :: Schema -> [SchemaDirective]
toList (Schema x) = x


-- Ranking the similarities of two Schemas.
-- If the there is nothing in common, then
-- Nothing is returned.
type Score = Double

compareSchema :: Schema -> Schema -> Maybe Score
compareSchema (Schema s) (Schema s1) = let cprod = (,) <$> s <*> s1
                                           len   = fromIntegral . length
                                       in case filter (uncurry (==)) cprod of
                                              [] -> Nothing
                                              xs -> Just $ len xs / len cprod


data SchemaDirective
    = ClearContext
    | DefineTerm    Text TermDefinition
    | RemoteContext !URI.URI
    | SetBase       Text
    | SetLanguage   Text
    | SetVocab      (Either URI.URI Text)
    deriving Eq

instance Show SchemaDirective where
    show = \case
            RemoteContext c   -> "RemoteContext -> URI "  ++ URI.renderStr c
            DefineTerm    t d -> "DefineTerm -> String "  ++ show t ++ " " ++ show d

            -- Explicitly unpack the Either block to reflect the underlying type accurately
            SetVocab (Left  uri) -> "SetVocab -> URI "    ++ URI.renderStr uri
            SetVocab (Right txt) -> "SetVocab -> String " ++ show txt

            SetBase      b -> "SetBase -> String "     ++ show b
            SetLanguage  l -> "SetLanguage -> String " ++ show l
            ClearContext   -> "ClearContext"


-- Lightweight since it's only used inside DefineTerm
data TermDefinition = TermDefinition
    { targetIRI :: Text
    , typeMap   :: Maybe TypeMapping
    , container :: Maybe ContainerType
    } deriving (Show, Eq)

data TypeMapping
    = TypeID
    | TypeIRI
    | TypeCustom Text
    deriving (Show, Eq)

data ContainerType
    = ContainerList
    | ContainerSet
    | ContainerLanguage
    deriving (Show, Eq)
