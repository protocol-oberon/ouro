module Ouro.StdTest
( base
, time
, vocab
, object
, actor
, provenance
, exhibition
, digital
, everyModule
) where

import           Data.Text (Text)
import           Hedgehog
import           Utils     (compilesToJSON, compilesWithoutWarnings, failsWith,
                            importsFailIn)


-- A program that imports every module of std
withStd :: Text -> Text
withStd src = "(import std/base) (import std/vocab) (import std/time) (import std/object) (import std/actor) "
           <> "(import std/provenance) (import std/exhibition) (import std/digital) " <> src

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
    , ("it is the type written out",               stdToJSON "(graph g (:r (eq (base/nationality \"300111188\" \"French\") (adjoin (base/aat \"300111188\" \"French\") :classified_as vocab/nationality))))"
                                                             "{\"r\": true}")
    , ("its meta-type is not added twice",         stdToJSON "(graph g (adjoin (base/work-type \"300033618\" \"Painting\") :classified_as vocab/type-of-work))"
                                                             "{\"id\": \"http://vocab.getty.edu/aat/300033618\", \"type\": \"Type\", \"_label\": \"Painting\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300435443\", \"type\": \"Type\", \"_label\": \"Type of Work\"}]}")

      -- Embedded nodes
    , ("name takes its kind first",                stdToJSON "(graph g (base/name vocab/primary-name \"Spring\"))"
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

      -- A record, as a literal or threaded
    , ("a record written as a literal",            stdToJSON (  "(graph g (:id #uri \"https://linked.art/example/person/manet\" :type \"Person\" :_label \"Manet\""
                                                             <> " :identified_by (list (base/name vocab/primary-name \"Manet\"))"
                                                             <> " :classified_as (list (base/nationality \"300111188\" \"French\") (base/occupation \"300025136\" \"Painter\"))))" )
                                                             (  "{\"id\": \"https://linked.art/example/person/manet\", \"type\": \"Person\", \"_label\": \"Manet\","
                                                             <> " \"identified_by\": [{\"type\": \"Name\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300404670\", \"type\": \"Type\", \"_label\": \"Primary Name\"}], \"content\": \"Manet\"}],"
                                                             <> " \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300111188\", \"type\": \"Type\", \"_label\": \"French\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300379842\", \"type\": \"Type\", \"_label\": \"Nationality\"}]},"
                                                             <> " {\"id\": \"http://vocab.getty.edu/aat/300025136\", \"type\": \"Type\", \"_label\": \"Painter\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300263369\", \"type\": \"Type\", \"_label\": \"Occupation\"}]}]}" ))
    , ("is the record threaded, with then adjoin", stdToJSON (  "(graph g (:r (eq (:id #uri \"https://linked.art/example/person/manet\" :type \"Person\" :_label \"Manet\""
                                                             <> " :identified_by (list (base/name vocab/primary-name \"Manet\"))"
                                                             <> " :classified_as (list (base/nationality \"300111188\" \"French\") (base/occupation \"300025136\" \"Painter\")))"
                                                             <> " (-> (:id #uri \"https://linked.art/example/person/manet\" :type \"Person\" :_label \"Manet\")"
                                                             <> " (with :identified_by (list (base/name vocab/primary-name \"Manet\")))"
                                                             <> " (with :classified_as (list (base/nationality \"300111188\" \"French\")))"
                                                             <> " (adjoin :classified_as (base/occupation \"300025136\" \"Painter\"))))))" )
                                                             "{\"r\": true}")

      -- What std/base offers
    , ("with writes a set as it is given",         stdToJSON "(graph g (with (:type \"Person\") :classified_as (base/nationality \"300111188\" \"French\")))"
                                                             "{\"type\": \"Person\", \"classified_as\": {\"id\": \"http://vocab.getty.edu/aat/300111188\", \"type\": \"Type\", \"_label\": \"French\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300379842\", \"type\": \"Type\", \"_label\": \"Nationality\"}]}}")
    , ("std/base exports its constructors only",   stdFailsWith "exports: aat, description, identifier, language, name, nationality, occupation, shape, statement, statement-kind, style, tgn, ulan, work-type."
                                                                "(graph g (base/nope 1))")
    , ("core is private",                          stdFailsWith "unexportedName = \"core\""
                                                                "(graph g (base/core #uri \"https://a.org/1\" \"Type\" \"A\"))")
    , ("meta-typed is private",                    stdFailsWith "unexportedName = \"meta-typed\""
                                                                "(graph g (base/meta-typed vocab/style \"1\" \"x\"))")
    ]


-- std/time: Linked Art's time spans. A span's end is included, so each ends at its last second,
-- leap years included; each takes a date written to its precision.
time :: Group
time = Group "std/time: Linked Art's time spans"
    [ ("span builds a TimeSpan",                   timeToJSON "(time/span \"1881-01-01T00:00:00Z\" \"1881-06-30T23:59:59Z\")"
                                                              "1881-01-01T00:00:00Z" "1881-06-30T23:59:59Z")
    , ("a span's ends are dates",                  timeFailsWith "expectedProperty = \"#date\""
                                                                 "(time/span \"soon\" \"later\")")
    , ("day is a day, to its last second",         timeToJSON "(time/day \"1832-01-23\")"
                                                              "1832-01-23T00:00:00Z" "1832-01-23T23:59:59Z")
    , ("month ends on its last day",               timeToJSON "(time/month \"1881-02\")"
                                                              "1881-02-01T00:00:00Z" "1881-02-28T23:59:59Z")
    , ("in a leap year too",                       timeToJSON "(time/month \"1880-02\")"
                                                              "1880-02-01T00:00:00Z" "1880-02-29T23:59:59Z")
    , ("year ends on the 31st of December",        timeToJSON "(time/year \"1881\")"
                                                              "1881-01-01T00:00:00Z" "1881-12-31T23:59:59Z")
    , ("year-range includes both years",           timeToJSON "(time/year-range \"1880\" \"1885\")"
                                                              "1880-01-01T00:00:00Z" "1885-12-31T23:59:59Z")
    , ("in an event",                              compilesToJSON "(import std/time) (graph g (:born (:type \"Birth\" :timespan (time/day \"1832-01-23\"))))"
                                                                  "{\"born\": {\"type\": \"Birth\", \"timespan\": {\"type\": \"TimeSpan\", \"begin_of_the_begin\": \"1832-01-23T00:00:00Z\", \"end_of_the_end\": \"1832-01-23T23:59:59Z\"}}}")
    , ("std/time compiles without warnings",       compilesWithoutWarnings "(import std/time) (graph g (time/year \"1881\"))")
    , ("std/time exports its spans only",          timeFailsWith "exports: day, month, span, year, year-range."
                                                                 "(time/nope)")
    , ("a date is written as a string",            timeFailsWith "expectedProperty = \"#str\""
                                                                 "(time/year 1881)")

      -- Pinned limit: a string that is not a date is reported inside std/time, where it
      -- becomes one, not at the call
    , ("a bad date is reported in std/time",       importsFailIn "lib/std/time.ouro" ["expectedProperty = \"#date\""] []
                                                                 "(import std/time) (graph g (:r (time/day \"1832-13-40\")))")
    ]
    where
    timeToJSON src begin end = compilesToJSON ("(import std/time) (graph g " <> src <> ")")
                                              ("{\"type\": \"TimeSpan\", \"begin_of_the_begin\": \"" <> begin <> "\", \"end_of_the_end\": \"" <> end <> "\"}")
    timeFailsWith message src = failsWith message ("(import std/time) (graph g " <> src <> ")")


-- std/vocab: the 18 terms Linked Art requires, each a Getty AAT Type
vocab :: Group
vocab = Group "std/vocab: the terms Linked Art requires"
    [ ("every required term, by its AAT number",   stdToJSON (  "(graph g (:ids (image (list vocab/primary-name vocab/display-name vocab/sort-name vocab/sort-value"
                                                             <> " vocab/brief-text vocab/type-of-work vocab/style vocab/shape vocab/nationality vocab/occupation vocab/color"
                                                             <> " vocab/exhibition-activity vocab/provenance-activity vocab/professional-activity vocab/publication-activity"
                                                             <> " vocab/promise-activity vocab/collection-item vocab/artwork) id)))" )
                                                             (  "{\"ids\": [\"http://vocab.getty.edu/aat/300404670\", \"http://vocab.getty.edu/aat/300404669\", \"http://vocab.getty.edu/aat/300451544\", \"http://vocab.getty.edu/aat/300456575\","
                                                             <> " \"http://vocab.getty.edu/aat/300418049\", \"http://vocab.getty.edu/aat/300435443\", \"http://vocab.getty.edu/aat/300015646\", \"http://vocab.getty.edu/aat/300056273\","
                                                             <> " \"http://vocab.getty.edu/aat/300379842\", \"http://vocab.getty.edu/aat/300263369\", \"http://vocab.getty.edu/aat/300080438\", \"http://vocab.getty.edu/aat/300054766\","
                                                             <> " \"http://vocab.getty.edu/aat/300055863\", \"http://vocab.getty.edu/aat/300393177\", \"http://vocab.getty.edu/aat/300054686\", \"http://vocab.getty.edu/aat/300435599\","
                                                             <> " \"http://vocab.getty.edu/aat/300404024\", \"http://vocab.getty.edu/aat/300133025\"]}" ))
    , ("a term is a Type with its label",          stdToJSON "(graph g vocab/primary-name)"
                                                             "{\"id\": \"http://vocab.getty.edu/aat/300404670\", \"type\": \"Type\", \"_label\": \"Primary Name\"}")
    , ("std/vocab exports the 18 terms only",      stdFailsWith "exports: artwork, brief-text, collection-item, color, display-name, exhibition-activity, nationality, occupation, primary-name, professional-activity, promise-activity, provenance-activity, publication-activity, shape, sort-name, sort-value, style, type-of-work."
                                                                "(graph g vocab/nope)")
    , ("its helper is private",                    stdFailsWith "unexportedName = \"term\""
                                                                "(graph g (vocab/term \"1\" \"x\"))")
    ]


-- std/object: dimensions, units, materials, and the statements and part types that describe them
object :: Group
object = Group "std/object: physical characteristics"
    [ ("height is a Dimension classified Height",  stdToJSON "(graph g (object/height 379.5 object/centimeters))"
                                                             "{\"type\": \"Dimension\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300055644\", \"type\": \"Type\", \"_label\": \"Height\"}], \"value\": 379.5, \"unit\": {\"id\": \"http://vocab.getty.edu/aat/300379098\", \"type\": \"MeasurementUnit\", \"_label\": \"centimeters\"}}")
    , ("each kind has its AAT number",             stdToJSON (  "(graph g (:ids (image (list (object/height 1 object/meters) (object/width 1 object/meters)"
                                                             <> " (object/depth 1 object/meters) (object/weight 1 object/grams)) classified_as id)))" )
                                                             "{\"ids\": [\"http://vocab.getty.edu/aat/300055644\", \"http://vocab.getty.edu/aat/300055647\", \"http://vocab.getty.edu/aat/300072633\", \"http://vocab.getty.edu/aat/300056240\"]}")
    , ("each unit is Linked Art's",                stdToJSON (  "(graph g (:ids (image (list object/centimeters object/millimeters object/meters object/inches object/feet"
                                                             <> " object/grams object/kilograms object/pounds) id)))" )
                                                             (  "{\"ids\": [\"http://vocab.getty.edu/aat/300379098\", \"http://vocab.getty.edu/aat/300379097\", \"http://vocab.getty.edu/aat/300379099\", \"http://vocab.getty.edu/aat/300379100\","
                                                             <> " \"http://vocab.getty.edu/aat/300379101\", \"http://vocab.getty.edu/aat/300379225\", \"http://vocab.getty.edu/aat/300379226\", \"http://vocab.getty.edu/aat/300379254\"]}" ))
    , ("dimension takes any kind",                 stdToJSON "(graph g (:k (image (object/dimension (base/aat \"300055642\" \"Unknown Physical Dimension\") 12 object/centimeters) classified_as _label)))"
                                                             "{\"k\": [\"Unknown Physical Dimension\"]}")
    , ("a value is a number",                      stdFailsWith "expectedProperty = \"#num\""
                                                                "(graph g (object/height \"379.5\" object/centimeters))")
    , ("material is typed Material",               stdToJSON "(graph g (object/material \"300014078\" \"canvas\"))"
                                                             "{\"id\": \"http://vocab.getty.edu/aat/300014078\", \"type\": \"Material\", \"_label\": \"canvas\"}")
    , ("the statements are Brief Text",            stdToJSON (  "(graph g (:kinds (image (list (object/dimension-statement \"379.5 cm\") (object/material-statement \"Oil on canvas\")) classified_as id)"
                                                             <> " :meta (image (object/material-statement \"Oil on canvas\") classified_as classified_as _label)))" )
                                                             "{\"kinds\": [\"http://vocab.getty.edu/aat/300435430\", \"http://vocab.getty.edu/aat/300435429\"], \"meta\": [\"Brief Text\"]}")
    , ("part-type is classified Part Type",        stdToJSON "(graph g (object/part-type \"300014844\" \"Support\"))"
                                                             "{\"id\": \"http://vocab.getty.edu/aat/300014844\", \"type\": \"Type\", \"_label\": \"Support\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300241583\", \"type\": \"Type\", \"_label\": \"Part Type\"}]}")
    , ("std/object exports these only",            stdFailsWith "exports: centimeters, depth, dimension, dimension-statement, feet, grams, height, inches, kilograms, material, material-statement, meters, millimeters, part-type, pounds, unit, weight, width."
                                                                "(graph g object/nope)")
    ]


-- std/actor: name parts, gender, a professional life, and contact points
actor :: Group
actor = Group "std/actor: people and groups"
    [ ("a Name with its parts",                    stdToJSON "(graph g (with (base/name vocab/primary-name \"Rembrandt van Rijn\") :part (list (actor/given-name \"Rembrandt\") (actor/family-name \"van Rijn\"))))"
                                                             (  "{\"type\": \"Name\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300404670\", \"type\": \"Type\", \"_label\": \"Primary Name\"}], \"content\": \"Rembrandt van Rijn\","
                                                             <> " \"part\": [{\"type\": \"Name\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300404651\", \"type\": \"Type\", \"_label\": \"Given Name\"}], \"content\": \"Rembrandt\"},"
                                                             <> " {\"type\": \"Name\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300404652\", \"type\": \"Type\", \"_label\": \"Family Name\"}], \"content\": \"van Rijn\"}]}" ))
    , ("a middle name",                            stdToJSON "(graph g (:k (image (actor/middle-name \"Harmenszoon\") classified_as id)))"
                                                             "{\"k\": [\"http://vocab.getty.edu/aat/300404654\"]}")
    , ("gender is classified Gender",              stdToJSON "(graph g (actor/gender \"300189559\" \"Male\"))"
                                                             "{\"id\": \"http://vocab.getty.edu/aat/300189559\", \"type\": \"Type\", \"_label\": \"Male\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300055147\", \"type\": \"Type\", \"_label\": \"Gender\"}]}")
    , ("active is a professional Activity",        stdToJSON "(graph g (actor/active (time/year-range \"1625\" \"1669\")))"
                                                             "{\"type\": \"Activity\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300393177\", \"type\": \"Type\", \"_label\": \"Professional Activity\"}], \"timespan\": {\"type\": \"TimeSpan\", \"begin_of_the_begin\": \"1625-01-01T00:00:00Z\", \"end_of_the_end\": \"1669-12-31T23:59:59Z\"}}")
    , ("contact points are Identifiers",           stdToJSON "(graph g (:k (image (list (actor/street-address \"a\") (actor/telephone \"b\") (actor/email \"c\")) classified_as id) :t (image (actor/email \"c\") type)))"
                                                             "{\"k\": [\"http://vocab.getty.edu/aat/300386983\", \"http://vocab.getty.edu/aat/300435688\", \"http://vocab.getty.edu/aat/300435686\"], \"t\": [\"Identifier\"]}")
    , ("std/actor exports these only",             stdFailsWith "exports: active, email, family-name, gender, given-name, middle-name, street-address, telephone."
                                                                "(graph g actor/nope)")
    ]


-- std/provenance: a purchase, its parts, money and currencies
provenance :: Group
provenance = Group "std/provenance: changes of ownership"
    [ ("a purchase: title one way, money the other", stdToJSON (  "(graph g (provenance/purchase (:id #uri \"https://a.org/spring\" :type \"HumanMadeObject\") (:id #uri \"https://a.org/manet\" :type \"Person\")"
                                                               <> " (:id #uri \"https://a.org/proust\" :type \"Person\") (provenance/money 3000 (provenance/currency \"300412016\" \"French Francs\")) (time/year \"1881\")))" )
                                                               (  "{\"type\": \"Activity\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300055863\", \"type\": \"Type\", \"_label\": \"Provenance Activity\"}],"
                                                               <> " \"timespan\": {\"type\": \"TimeSpan\", \"begin_of_the_begin\": \"1881-01-01T00:00:00Z\", \"end_of_the_end\": \"1881-12-31T23:59:59Z\"},"
                                                               <> " \"part\": [{\"type\": \"Acquisition\", \"transferred_title_of\": [{\"id\": \"https://a.org/spring\", \"type\": \"HumanMadeObject\"}],"
                                                               <> " \"transferred_title_from\": [{\"id\": \"https://a.org/manet\", \"type\": \"Person\"}], \"transferred_title_to\": [{\"id\": \"https://a.org/proust\", \"type\": \"Person\"}]},"
                                                               <> " {\"type\": \"Payment\", \"paid_amount\": {\"type\": \"MonetaryAmount\", \"value\": 3000, \"currency\": {\"id\": \"http://vocab.getty.edu/aat/300412016\", \"type\": \"Currency\", \"_label\": \"French Francs\"}},"
                                                               <> " \"paid_from\": [{\"id\": \"https://a.org/proust\", \"type\": \"Person\"}], \"paid_to\": [{\"id\": \"https://a.org/manet\", \"type\": \"Person\"}]}]}" ))
    , ("a record writes its id beside it",         stdToJSON (  "(graph g (:id #uri \"https://a.org/p/1\" :_label \"Purchase\" (inlay (provenance/purchase (:type \"HumanMadeObject\") (:type \"Person\") (:type \"Group\")"
                                                             <> " (provenance/money 1 provenance/euros) (time/year \"1881\")))))" )
                                                             (  "{\"id\": \"https://a.org/p/1\", \"_label\": \"Purchase\", \"type\": \"Activity\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300055863\", \"type\": \"Type\", \"_label\": \"Provenance Activity\"}],"
                                                             <> " \"timespan\": {\"type\": \"TimeSpan\", \"begin_of_the_begin\": \"1881-01-01T00:00:00Z\", \"end_of_the_end\": \"1881-12-31T23:59:59Z\"},"
                                                             <> " \"part\": [{\"type\": \"Acquisition\", \"transferred_title_of\": [{\"type\": \"HumanMadeObject\"}], \"transferred_title_from\": [{\"type\": \"Person\"}], \"transferred_title_to\": [{\"type\": \"Group\"}]},"
                                                             <> " {\"type\": \"Payment\", \"paid_amount\": {\"type\": \"MonetaryAmount\", \"value\": 1, \"currency\": {\"id\": \"http://vocab.getty.edu/aat/300411996\", \"type\": \"Currency\", \"_label\": \"Euros\"}},"
                                                             <> " \"paid_from\": [{\"type\": \"Group\"}], \"paid_to\": [{\"type\": \"Person\"}]}]}" ))
    , ("each currency is Linked Art's",            stdToJSON (  "(graph g (:ids (image (list provenance/british-pounds provenance/canadian-dollars provenance/euros provenance/japanese-yen"
                                                             <> " provenance/swiss-francs provenance/us-dollars) id)))" )
                                                             (  "{\"ids\": [\"http://vocab.getty.edu/aat/300411998\", \"http://vocab.getty.edu/aat/300412000\", \"http://vocab.getty.edu/aat/300411996\","
                                                             <> " \"http://vocab.getty.edu/aat/300411997\", \"http://vocab.getty.edu/aat/300412001\", \"http://vocab.getty.edu/aat/300411994\"]}" ))
    , ("an amount is a number",                    stdFailsWith "expectedProperty = \"#num\""
                                                                "(graph g (provenance/money \"3000\" provenance/euros))")
    , ("std/provenance exports these only",        stdFailsWith "exports: acquisition, british-pounds, canadian-dollars, currency, euros, japanese-yen, money, payment, purchase, swiss-francs, us-dollars."
                                                                "(graph g provenance/nope)")
    ]


-- std/exhibition: the exhibiting, its concept and its set
exhibition :: Group
exhibition = Group "std/exhibition: exhibitions"
    [ ("exhibiting is an exhibition Activity",     stdToJSON "(graph g (exhibition/exhibiting #uri \"https://a.org/exh/1\" \"Manet and Modern Beauty\"))"
                                                             "{\"id\": \"https://a.org/exh/1\", \"type\": \"Activity\", \"_label\": \"Manet and Modern Beauty\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300054766\", \"type\": \"Type\", \"_label\": \"Exhibition Activity\"}]}")
    , ("concept is a PropositionalObject",         stdToJSON "(graph g (exhibition/concept #uri \"https://a.org/idea/1\" \"Idea\"))"
                                                             "{\"id\": \"https://a.org/idea/1\", \"type\": \"PropositionalObject\", \"_label\": \"Idea\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300417531\", \"type\": \"Type\", \"_label\": \"Exhibition\"}]}")
    , ("set is an Exhibition Collection",          stdToJSON "(graph g (exhibition/set #uri \"https://a.org/set/1\" \"Objects\"))"
                                                             "{\"id\": \"https://a.org/set/1\", \"type\": \"Set\", \"_label\": \"Objects\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300378926\", \"type\": \"Type\", \"_label\": \"Exhibition Collection\"}]}")
    , ("an id is a URI",                           stdFailsWith "expectedProperty = \"#uri\""
                                                                "(graph g (exhibition/set \"not a uri\" \"Objects\"))")
    , ("std/exhibition exports these only",        stdFailsWith "exports: concept, exhibiting, set."
                                                                "(graph g exhibition/nope)")
    ]


-- std/digital: an image and a web page, each a DigitalObject at its URL
digital :: Group
digital = Group "std/digital: digital content"
    [ ("picture is shown by a Digital Image",      stdToJSON "(graph g (digital/picture \"https://a.org/s.jpg\" \"image/jpeg\"))"
                                                             (  "{\"type\": \"VisualItem\", \"digitally_shown_by\": [{\"type\": \"DigitalObject\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300215302\", \"type\": \"Type\", \"_label\": \"Digital Image\"}],"
                                                             <> " \"format\": \"image/jpeg\", \"access_point\": [{\"id\": \"https://a.org/s.jpg\", \"type\": \"DigitalObject\"}]}]}" ))
    , ("web-page is carried by a Web Page",        stdToJSON "(graph g (digital/web-page \"https://a.org/s\"))"
                                                             (  "{\"type\": \"LinguisticObject\", \"digitally_carried_by\": [{\"type\": \"DigitalObject\", \"classified_as\": [{\"id\": \"http://vocab.getty.edu/aat/300264578\", \"type\": \"Type\", \"_label\": \"Web Page\"}],"
                                                             <> " \"format\": \"text/html\", \"access_point\": [{\"id\": \"https://a.org/s\", \"type\": \"DigitalObject\"}]}]}" ))
    , ("a URL is a URI",                           stdFailsWith "expectedProperty = \"#uri\""
                                                                "(graph g (digital/web-page \"a.org\"))")
    , ("std/digital exports these only",           stdFailsWith "exports: picture, web-page."
                                                                "(graph g digital/nope)")
    ]


-- All of std together
everyModule :: Group
everyModule = Group "std: every module"
    [ ("std compiles without warnings",            compilesWithoutWarnings (withStd "(graph g (:r 1))"))
    ]
