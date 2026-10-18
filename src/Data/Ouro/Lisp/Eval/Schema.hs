{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Data.Ouro.Lisp.Eval.Schema
( contextFromValue
) where

import           Data.Foldable               (foldlM)
import           Data.Function               ((&))
import           Data.Ouro.Error.Diagnostics (malformedContext, withBlurb)
import           Data.Ouro.Error.Types       (OuroError (..))
import qualified Data.Ouro.Internal.Expr     as I
import           Data.Ouro.Internal.Schema   (ContextEntry (..), ContextValue (..),
                                              Directives, Schema (..), directive,
                                              isKeyword)
import           Data.Ouro.Lisp.Eval.Types   (Expr (..), humanReadableType)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Text.URI                    as URI
import           Text.Megaparsec             (SourcePos)


-- contextFromValue.
--
-- A record's context is an ordinary value, written as JSON-LD writes it:
--
--   :context "https://linked.art/ns/v1/linked-art.json"             ; a remote context
--   :context (:@vocab "https://example.org/" :@language "en")       ; a local context
--   :context ("https://linked.art/ns/v1/linked-art.json"            ; several, in order
--             (:ex "https://example.org/ns/"
--              :condition (:@id "ex:condition" :@type "@id")))
--   :context null                                                   ; clears the context
--
-- The value is checked here, so a malformed context is reported where it is written: a
-- remote context is an absolute URL, a local context's '@' keys are JSON-LD's context
-- keywords, and a term maps to an IRI, null, or an expanded definition. Every problem is
-- reported at the context's expression, since the value has no positions of its own.
contextFromValue :: SourcePos -> Expr -> Either OuroError Schema
contextFromValue pos = \case
    Array values         -> mconcat <$> traverse entry values
    Primitive I.EmptyArr -> Right mempty
    value                -> entry value
    where
    -- One entry of the context, on its own or in its array
    entry :: Expr -> Either OuroError Schema
    entry = \case
        EvalError err          -> Left err
        Primitive (I.String t) -> remote t
        Primitive (I.URI uri)  -> remote (URI.render uri)
        Primitive I.Null       -> Right (Schema [ClearContext])
        Primitive I.EmptyRec   -> Right (Schema [LocalContext mempty])
        Record _ pairs         -> Schema . pure . LocalContext <$> localContext pairs
        Array _                -> problem "A context array cannot hold another array."
                                          "Write the contexts in one array, in the order they apply: (\"https://...\" (:term \"https://...\"))."
        Primitive I.EmptyArr   -> problem "A context array cannot hold another array."
                                          "Write the contexts in one array, in the order they apply."
        other                  -> problem ("A context cannot be " <> humanReadableType other <> ".")
                                          contextShapes

    remote :: Text -> Either OuroError Schema
    remote t = case URI.mkURI t of
        Right uri | Just _ <- URI.uriScheme uri -> Right (Schema [RemoteContext uri])
        _ -> problem ("The remote context '" <> t <> "' is not an absolute URL.")
                     "A remote context is the URL of a JSON-LD context document, with its scheme: \"https://linked.art/ns/v1/linked-art.json\"."

    -- A local context: its keywords and term definitions, each key once
    localContext :: [(Text, Expr)] -> Either OuroError Directives
    localContext = foldlM (\acc (key, value) -> (acc <>) <$> definition key value) mempty

    definition :: Text -> Expr -> Either OuroError Directives
    definition key value
        | isKeyword key = case lookup key contextKeywords of
              Just accepts -> directive key <$> checked ("'" <> key <> "'") accepts value
              Nothing      -> problem ("'" <> key <> "' is not a JSON-LD context keyword.")
                                      ("A local context's keywords are " <> listed (map fst contextKeywords) <> "; any other key defines a term.")
        | otherwise = directive key <$> termDefinition key value

    -- A term maps to an IRI (or a compact IRI, or another term), to null, or to an expanded
    -- definition whose keys are all keywords
    termDefinition :: Text -> Expr -> Either OuroError ContextValue
    termDefinition term = \case
        Record _ pairs       -> CObject <$> traverse (expanded term) pairs
        Primitive I.EmptyRec -> Right (CObject [])
        value                -> checked ("The term '" <> term <> "'") termValue value

    expanded :: Text -> (Text, Expr) -> Either OuroError (Text, ContextValue)
    expanded term (key, value) = case lookup key termKeywords of
        Just accepts -> (key,) <$> checked ("'" <> key <> "' in the term '" <> term <> "'") accepts value
        Nothing      -> problem ("'" <> key <> "' cannot be part of the definition of the term '" <> term <> "'.")
                                ("An expanded term definition's keys are " <> listed (map fst termKeywords) <> ".")

    -- Converts a value, checking it is one of the shapes a key accepts
    checked :: Text -> Accepts -> Expr -> Either OuroError ContextValue
    checked what accepts value = case jsonValue value of
        Left (EvalError err)            -> Left err
        Left unwritable                 -> problem (what <> " cannot hold " <> humanReadableType unwritable <> ".")
                                                   (what <> " takes " <> describe accepts <> ".")
        Right json | acceptsValue json  -> Right json
                   | otherwise          -> problem (what <> " cannot be " <> humanReadableType value <> ".")
                                                   (what <> " takes " <> describe accepts <> ".")
        where
        acceptsValue json = any (fits json) (acceptedShapes accepts)

    problem :: Text -> Text -> Either OuroError a
    problem reason blurb = malformedContext reason & withBlurb blurb & OuroError pos & Left

    contextShapes = "A context is a remote context's URL, a local context record, null, or an array of them: "
                 <> "\"https://...\", (:@vocab \"https://...\"), or (\"https://...\" (:term \"https://...\"))."

    listed = T.intercalate ", "


-- The shapes a context key accepts, and how to describe them
data Accepts = Accepts
    { acceptedShapes :: [Shape]
    , describe       :: Text
    }

data Shape = AString | ANull | ABool | ANumber | AStringArray | AnyContext

fits :: ContextValue -> Shape -> Bool
fits value shape = case (shape, value) of
    (AString,      CString _) -> True
    (ANull,        CNull)     -> True
    (ABool,        CBool _)   -> True
    (ANumber,      CNumber _) -> True
    (AStringArray, CArray vs) -> all (\case CString _ -> True; _ -> False) vs
    (AnyContext,   _)         -> True
    _                         -> False

-- What a term maps to, when it is not an expanded definition record
termValue :: Accepts
termValue = Accepts [AString, ANull] "a String (an IRI, a compact IRI or a term), null, or an expanded definition record"


-- The keywords a local context can set, from JSON-LD 1.1
contextKeywords :: [(Text, Accepts)]
contextKeywords =
    [ ("@vocab",     Accepts [AString, ANull] "a String (an IRI) or null")
    , ("@base",      Accepts [AString, ANull] "a String (an IRI) or null")
    , ("@language",  Accepts [AString, ANull] "a String (a language tag such as \"en\") or null")
    , ("@direction", Accepts [AString, ANull] "\"ltr\", \"rtl\" or null")
    , ("@version",   Accepts [ANumber]        "the Number 1.1")
    , ("@protected", Accepts [ABool]          "a Boolean")
    , ("@propagate", Accepts [ABool]          "a Boolean")
    , ("@import",    Accepts [AString]        "a String (the URL of a context to import)")
    ]

-- The keywords of an expanded term definition, from JSON-LD 1.1
termKeywords :: [(Text, Accepts)]
termKeywords =
    [ ("@id",        Accepts [AString, ANull]      "a String (an IRI) or null")
    , ("@type",      Accepts [AString]             "a String (an IRI, \"@id\" or \"@vocab\")")
    , ("@container", Accepts [AString, AStringArray, ANull] "a String such as \"@set\" or \"@list\", an Array of them, or null")
    , ("@language",  Accepts [AString, ANull]      "a String (a language tag) or null")
    , ("@direction", Accepts [AString, ANull]      "\"ltr\", \"rtl\" or null")
    , ("@reverse",   Accepts [AString]             "a String (an IRI)")
    , ("@index",     Accepts [AString]             "a String")
    , ("@nest",      Accepts [AString]             "a String")
    , ("@prefix",    Accepts [ABool]               "a Boolean")
    , ("@protected", Accepts [ABool]               "a Boolean")
    , ("@context",   Accepts [AnyContext]          "a context")
    ]


-- An evaluated value in JSON's own terms, or the value that has none: a Date, a function, or
-- an error, which is passed on
jsonValue :: Expr -> Either Expr ContextValue
jsonValue = \case
    Primitive (I.String t)  -> Right (CString t)
    Primitive (I.URI uri)   -> Right (CString (URI.render uri))
    Primitive (I.Number n)  -> Right (CNumber n)
    Primitive (I.Boolean b) -> Right (CBool b)
    Primitive I.Null        -> Right CNull
    Primitive I.EmptyArr    -> Right (CArray [])
    Primitive I.EmptyRec    -> Right (CObject [])
    Array values            -> CArray  <$> traverse jsonValue values
    Record _ pairs          -> CObject <$> traverse (traverse jsonValue) pairs
    other                   -> Left other
