
module Data.HJLD.Internal.Schema where

import           Data.Text (Text)

newtype Schema = Schema [SchemaDirective]
    deriving (Show, Eq)

data SchemaDirective
    = DefineTerm    Text TermDefinition
    | SetVocab      Text
    | SetBase       Text
    | SetLanguage   Text
    | RemoteContext Text
    | ClearContext
    deriving (Show, Eq)

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
