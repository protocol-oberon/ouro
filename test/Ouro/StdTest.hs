module Ouro.StdTest
( base
, vocab
) where

import           Data.Text (Text)
import           Hedgehog
import           Utils     (compilesToJSON, failsWith)


-- A program that imports std/base and std/vocab
withStd :: Text -> Text
withStd src = "(import std/base) (import std/vocab) " <> src

stdToJSON :: Text -> Text -> Property
stdToJSON src = compilesToJSON (withStd src)

stdFailsWith :: Text -> Text -> Property
stdFailsWith message src = failsWith message (withStd src)


-- std/base: Linked Art's base patterns. Every function takes values and builds a node; a
-- record is a literal, or is threaded through with and adjoin, which the language provides.
base :: Group
base = Group "std/base: Linked Art's base patterns"
    [ -- Entities from the Getty vocabularies
      ("aat builds a Getty AAT Type",              stdToJSON "(graph g (base/aat \"300033618\" \"Painting\"))"
                                                             "{\"id\": \"http://vocab.getty.edu/aat/300033618\", \"type\": \"Type\", \"_label\": \"Painting\"}")
    , ("tgn builds a Getty TGN Place",             stdToJSON "(graph g (base/tgn \"7006952\" \"Amsterdam\"))"
                                                             "{\"id\": \"http://vocab.getty.edu/tgn/7006952\", \"type\": \"Place\", \"_label\": \"Amsterdam\"}")
    , ("ulan builds a Person or a Group",          stdToJSON "(graph g (:p (base/ulan \"500010363\" \"Person\" \"Manet\") :g (base/ulan \"500115988\" \"Group\" \"Getty Museum\")))"
                                                             "{\"p\": {\"id\": \"http://vocab.getty.edu/ulan/500010363\", \"type\": \"Person\", \"_label\": \"Manet\"}, \"g\": {\"id\": \"http://vocab.getty.edu/ulan/500115988\", \"type\": \"Group\", \"_label\": \"Getty Museum\"}}")
    , ("language builds a Language",               stdToJSON "(graph g (base/language \"300388277\" \"English\"))"
                                                             "{\"id\": \"http://vocab.getty.edu/aat/300388277\", \"type\": \"Language\", \"_label\": \"English\"}")
    , ("a Getty number is a string",               stdFailsWith "expectedProperty = \"#str\""
                                                                "(graph g (base/aat 300033618 \"Painting\"))")

      -- Types classified by their meta-type
    , ("work-type is classified as Type of Work",  stdToJSON "(graph g (base/work-type \"300033618\" \"Painting\"))"
                                                             "{\"id\": \"http://vocab.getty.edu/aat/300033618\", \"type\": \"Type\", \"_label\": \"Painting\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300435443\", \"type\": \"Type\", \"_label\": \"Type of Work\"}]}")
    , ("each type has its meta-type",              stdToJSON (  "(graph g (:style (image (base/style \"1\" \"x\") classified_as _label) :shape (image (base/shape \"1\" \"x\") classified_as _label)"
                                                             <> " :nationality (image (base/nationality \"1\" \"x\") classified_as _label) :occupation (image (base/occupation \"1\" \"x\") classified_as _label)"
                                                             <> " :statement-kind (image (base/statement-kind \"1\" \"x\") classified_as _label)))" )
                                                             "{\"style\": [\"Style\"], \"shape\": [\"Shape\"], \"nationality\": [\"Nationality\"], \"occupation\": [\"Occupation\"], \"statement-kind\": [\"Brief Text\"]}")
    , ("it is the type written out",               stdToJSON "(graph g (:r (eq (base/nationality \"300111188\" \"French\") (adjoin (base/aat \"300111188\" \"French\") :classified_as (vocab/nationality)))))"
                                                             "{\"r\": true}")
    , ("its meta-type is not added twice",         stdToJSON "(graph g (adjoin (base/work-type \"300033618\" \"Painting\") :classified_as (vocab/type-of-work)))"
                                                             "{\"id\": \"http://vocab.getty.edu/aat/300033618\", \"type\": \"Type\", \"_label\": \"Painting\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300435443\", \"type\": \"Type\", \"_label\": \"Type of Work\"}]}")

      -- Embedded nodes
    , ("name takes its kind first",                stdToJSON "(graph g (base/name (vocab/primary-name) \"Spring\"))"
                                                             "{\"type\": \"Name\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300404670\", \"type\": \"Type\", \"_label\": \"Primary Name\"}], \"content\": \"Spring\"}")
    , ("a name's kind is a record",                stdFailsWith "expectedProperty = \"#rec\""
                                                                "(graph g (base/name \"Primary\" \"Spring\"))")
    , ("identifier builds an Identifier",          stdToJSON "(graph g (base/identifier (base/aat \"300312355\" \"Accession Number\") \"2014.62\"))"
                                                             "{\"type\": \"Identifier\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300312355\", \"type\": \"Type\", \"_label\": \"Accession Number\"}], \"content\": \"2014.62\"}")
    , ("statement classifies its kind Brief Text", stdToJSON "(graph g (base/statement (base/aat \"300435429\" \"Material Statement\") \"Oil on canvas\"))"
                                                             "{\"type\": \"LinguisticObject\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300435429\", \"type\": \"Type\", \"_label\": \"Material Statement\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300418049\", \"type\": \"Type\", \"_label\": \"Brief Text\"}]}], \"content\": \"Oil on canvas\"}")
    , ("a statement-kind is Brief Text once",      stdToJSON "(graph g (:r (eq (base/statement (base/statement-kind \"300435429\" \"Material Statement\") \"Oil\") (base/statement (base/aat \"300435429\" \"Material Statement\") \"Oil\"))))"
                                                             "{\"r\": true}")
    , ("description is Linked Art's Description",  stdToJSON "(graph g (base/description \"Amsterdam is a city in the Netherlands\"))"
                                                             "{\"type\": \"LinguisticObject\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300435416\", \"type\": \"Type\", \"_label\": \"Description\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300418049\", \"type\": \"Type\", \"_label\": \"Brief Text\"}]}], \"content\": \"Amsterdam is a city in the Netherlands\"}")
    , ("timespan builds a TimeSpan",               stdToJSON "(graph g (base/timespan \"1881-01-01T00:00:00Z\" \"1881-12-31T23:59:59Z\"))"
                                                             "{\"type\": \"TimeSpan\", \"begin_of_the_begin\": \"1881-01-01T00:00:00Z\", \"end_of_the_end\": \"1881-12-31T23:59:59Z\"}")
    , ("a timespan's ends are dates",              stdFailsWith "expectedProperty = \"#date\""
                                                                "(graph g (base/timespan \"soon\" \"later\"))")

      -- A record, as a literal or threaded
    , ("a record written as a literal",            stdToJSON (  "(graph g (:id #uri \"https://linked.art/example/person/manet\" :type \"Person\" :_label \"Manet\""
                                                             <> " :identified_by (list (base/name (vocab/primary-name) \"Manet\"))"
                                                             <> " :classified_as (list (base/nationality \"300111188\" \"French\") (base/occupation \"300025136\" \"Painter\"))))" )
                                                             (  "{\"id\": \"https://linked.art/example/person/manet\", \"type\": \"Person\", \"_label\": \"Manet\","
                                                             <> " \"identified_by\": [{\"type\": \"Name\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300404670\", \"type\": \"Type\", \"_label\": \"Primary Name\"}], \"content\": \"Manet\"}],"
                                                             <> " \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300111188\", \"type\": \"Type\", \"_label\": \"French\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300379842\", \"type\": \"Type\", \"_label\": \"Nationality\"}]},"
                                                             <> " {\"id\": \"http://vocab.getty.edu/aat/300025136\", \"type\": \"Type\", \"_label\": \"Painter\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300263369\", \"type\": \"Type\", \"_label\": \"Occupation\"}]}]}" ))
    , ("is the record threaded, with then adjoin", stdToJSON (  "(graph g (:r (eq (:id #uri \"https://linked.art/example/person/manet\" :type \"Person\" :_label \"Manet\""
                                                             <> " :identified_by (list (base/name (vocab/primary-name) \"Manet\"))"
                                                             <> " :classified_as (list (base/nationality \"300111188\" \"French\") (base/occupation \"300025136\" \"Painter\")))"
                                                             <> " (-> (:id #uri \"https://linked.art/example/person/manet\" :type \"Person\" :_label \"Manet\")"
                                                             <> " (with :identified_by (list (base/name (vocab/primary-name) \"Manet\")))"
                                                             <> " (with :classified_as (list (base/nationality \"300111188\" \"French\")))"
                                                             <> " (adjoin :classified_as (base/occupation \"300025136\" \"Painter\"))))))" )
                                                             "{\"r\": true}")

      -- What std/base offers
    , ("with writes a set as it is given",         stdToJSON "(graph g (with (:type \"Person\") :classified_as (base/nationality \"300111188\" \"French\")))"
                                                             "{\"type\": \"Person\", \"classified_as\": {\"id\": \"http://vocab.getty.edu/aat/300111188\", \"type\": \"Type\", \"_label\": \"French\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300379842\", \"type\": \"Type\", \"_label\": \"Nationality\"}]}}")
    , ("std/base exports its constructors only",   stdFailsWith "exports: aat, description, identifier, language, name, nationality, occupation, shape, statement, statement-kind, style, tgn, timespan, ulan, work-type."
                                                                "(graph g (base/nope 1))")
    , ("core is private",                          stdFailsWith "unexportedName = \"core\""
                                                                "(graph g (base/core #uri \"https://a.org/1\" \"Type\" \"A\"))")
    , ("meta-typed is private",                    stdFailsWith "unexportedName = \"meta-typed\""
                                                                "(graph g (base/meta-typed (vocab/style) \"1\" \"x\"))")
    ]


-- std/vocab: the 18 terms Linked Art requires, each a Getty AAT Type
vocab :: Group
vocab = Group "std/vocab: the terms Linked Art requires"
    [ ("every required term, by its AAT number",   stdToJSON (  "(graph g (:ids (image (list (vocab/primary-name) (vocab/display-name) (vocab/sort-name) (vocab/sort-value)"
                                                             <> " (vocab/brief-text) (vocab/type-of-work) (vocab/style) (vocab/shape) (vocab/nationality) (vocab/occupation) (vocab/color)"
                                                             <> " (vocab/exhibition-activity) (vocab/provenance-activity) (vocab/professional-activity) (vocab/publication-activity)"
                                                             <> " (vocab/promise-activity) (vocab/collection-item) (vocab/artwork)) id)))" )
                                                             (  "{\"ids\": [\"http://vocab.getty.edu/aat/300404670\", \"http://vocab.getty.edu/aat/300404669\", \"http://vocab.getty.edu/aat/300451544\", \"http://vocab.getty.edu/aat/300456575\","
                                                             <> " \"http://vocab.getty.edu/aat/300418049\", \"http://vocab.getty.edu/aat/300435443\", \"http://vocab.getty.edu/aat/300015646\", \"http://vocab.getty.edu/aat/300056273\","
                                                             <> " \"http://vocab.getty.edu/aat/300379842\", \"http://vocab.getty.edu/aat/300263369\", \"http://vocab.getty.edu/aat/300080438\", \"http://vocab.getty.edu/aat/300054766\","
                                                             <> " \"http://vocab.getty.edu/aat/300055863\", \"http://vocab.getty.edu/aat/300393177\", \"http://vocab.getty.edu/aat/300054686\", \"http://vocab.getty.edu/aat/300435599\","
                                                             <> " \"http://vocab.getty.edu/aat/300404024\", \"http://vocab.getty.edu/aat/300133025\"]}" ))
    , ("a term is a Type with its label",          stdToJSON "(graph g (vocab/primary-name))"
                                                             "{\"id\": \"http://vocab.getty.edu/aat/300404670\", \"type\": \"Type\", \"_label\": \"Primary Name\"}")
    , ("std/vocab exports the 18 terms only",      stdFailsWith "exports: artwork, brief-text, collection-item, color, display-name, exhibition-activity, nationality, occupation, primary-name, professional-activity, promise-activity, provenance-activity, publication-activity, shape, sort-name, sort-value, style, type-of-work."
                                                                "(graph g (vocab/nope))")
    , ("its helper is private",                    stdFailsWith "unexportedName = \"term\""
                                                                "(graph g (vocab/term \"1\" \"x\"))")
    ]
