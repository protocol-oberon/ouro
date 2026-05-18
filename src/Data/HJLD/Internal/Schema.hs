
module Data.HJLD.Internal.Schema where

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


toList :: Schema -> [SchemaDirective]
toList (Schema x) = x


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
            SetVocab (Left uri)  -> "SetVocab -> URI "    ++ URI.renderStr uri
            SetVocab (Right txt) -> "SetVocab -> String " ++ show txt

            SetBase       b   -> "SetBase -> String "     ++ show b
            SetLanguage   l   -> "SetLanguage -> String " ++ show l
            ClearContext      -> "ClearContext"



instance Semigroup Schema where
    (Schema a) <> (Schema b) = Schema (a ++ b)

instance Monoid Schema where
    mempty = Schema []


-- Keep this lightweight since it's only used inside DefineTerm
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
