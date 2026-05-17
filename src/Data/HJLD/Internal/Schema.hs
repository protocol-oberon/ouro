
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

data SchemaDirective
    = ClearContext
    | DefineTerm    Text TermDefinition
    | RemoteContext !URI.URI
    | SetBase       Text
    | SetLanguage   Text
    | SetVocab      Text
    deriving Eq

instance Show SchemaDirective where
    show = \case
            RemoteContext c   -> "RemoteContext -> URI "  ++ URI.renderStr c
            DefineTerm    t d -> "DefineTerm -> String "  ++ show t ++ " " ++ show d
            SetVocab      v   -> "SetVocab -> String "    ++ show v
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


-- Now your example works beautifully using (<>)
parentSchema :: Schema
parentSchema = Schema [ SetVocab "http://schema.org/"
                      , DefineTerm "name" (TermDefinition "http://schema.org/name" Nothing Nothing)
                      ]

localSchema :: Schema
localSchema = Schema [ DefineTerm "name" (TermDefinition "http://xmlns.com/foaf/0.1/name" Nothing Nothing) ]

-- Combining them using the Semigroup operator
combinedSchema :: Schema
combinedSchema = parentSchema <> localSchema
