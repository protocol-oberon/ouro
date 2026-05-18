{-# LANGUAGE GADTs #-}

module Data.HJLD.Serializer where

import           Data.HJLD.Internal.Expr   (Expr)
import qualified Data.HJLD.Internal.Expr   as Expr
import           Data.HJLD.Internal.Schema (Schema (..), SchemaDirective)
import qualified Data.HJLD.Internal.Schema as Sch
import           Data.Text                 (Text, replicate)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Builder    as B
import qualified Data.Time.Format          as TF
import qualified Text.URI                  as URI


toJSON :: Expr t -> TL.Text
toJSON = B.toLazyText . buildJSON 0 []


buildJSON :: Int -> [SchemaDirective] -> Expr t -> B.Builder
buildJSON indent env = \case
                     -- Primatives
                     Expr.String txt    -> escapeString txt
                     Expr.Number n      -> B.fromString $ show n
                     Expr.Boolean True  -> "true"
                     Expr.Boolean False -> "false"
                     Expr.URI  u        -> escapeString $ URI.render u
                     Expr.Date d        -> escapeString $ T.pack $ TF.formatTime TF.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" d
                     Expr.Null          -> "null"
                     Expr.BlankNode b   -> escapeString $ "_:" <> b
                     -- Structural Closures
                     Expr.Context schema inner -> let Schema derictives = schema
                                                      localEnv          = env <> derictives
                                                  in buildContextBlock indent localEnv schema inner

                     Expr.Reverse inner -> "{\n"
                                        <> (ind indent)
                                        <> "  \"@reverse\": "
                                        <> buildJSON (indent + 2) env inner
                                        <> "\n"
                                        <> (ind indent)
                                        <> "}"
                     --
                     -- Bondary Tags
                     Expr.Object propsCons _body -> let propsList = Expr.flattenProps propsCons
                                                    in case null propsList of
                                                          True  -> "{}"
                                                          False -> "{\n"
                                                                <> intercalateBuilders
                                                                       ",\n"
                                                                       (map (renderProperty (indent + 2) env) propsList)
                                                                <> "\n"
                                                                <> ind indent
                                                                <> "}"

                     Expr.Array elems -> let ls = Expr.flattenArray elems
                                         in case null ls of
                                                True  -> "[]"
                                                False -> "[\n"
                                                      <> intercalateBuilders ",\n" (map (renderArrayElement (indent + 2) env) ls)
                                                      <> "\n"
                                                      <> ind indent
                                                      <> "]"

                     -- Fallback handling for raw backbone pieces
                     Expr.Cons h t -> "[\n" <> buildArrayElements (indent + 2) env (Expr.Cons h t) <> "\n" <> ind indent <> "]"
                     Expr.Attr k v -> "{\n" <> ind (indent + 2) <> escapeString k <> ": " <> buildJSON (indent + 2) env v <> "\n" <> ind indent <> "}"
                     Expr.Nil      -> "[]"


-- Layout Builders (2-space, trailing-comma trailing mechanics)

-- Unrolls proper spinfs into structured key-value pairs
buildProps :: Int -> [SchemaDirective] -> Expr t -> B.Builder
buildProps indent env expr = go True expr
    where
    go :: Bool -> Expr a -> B.Builder
    go isFirst = \case
                  -- We thread the first-flag state through the left branch,
                  -- then use 'isNil' or track status to evaluate the tail.
                  Expr.Cons h t -> let headBuilder = go isFirst h
                                       tailBuilder = go (isFirst && isNil h) t
                                   in headBuilder <> tailBuilder

                  Expr.Attr k v -> let prefix = case isFirst of
                                                    True  -> ind indent
                                                    False -> ",\n" <> ind indent
                                   in prefix <> escapeString k <> ": " <> buildJSON indent env v

                  _             -> mempty


-- Unrolls array items with normal 2-space alignment and trailing commas
buildArrayElements :: Int -> [SchemaDirective] -> Expr t -> B.Builder
buildArrayElements indent env expr = go expr
    where
    go :: Expr a -> B.Builder
    go = \case
          Expr.Cons h t -> ind indent <> buildJSON indent env h <> ",\n" <> go t
          Expr.Nil      -> mempty
          other         -> ind indent <> buildJSON indent env other <> ",\n"


-- Handles cleanly merging an inline context alongside other sibling parameters
buildContextBlock :: Int -> [SchemaDirective] -> Schema -> Expr t -> B.Builder
buildContextBlock indent env (Schema directives) inner =
    let pad      = ind indent
        innerPad = ind (indent + 2)
    in "{\n"    <>
       innerPad <> "\"@context\": " <> renderSchemaInline (indent + 2) directives <> ",\n" <>
       -- Force a newline break directly after the body contents stream out
       stripObjectBrackets (indent + 2) env inner <> "\n" <>
       pad      <> "}"


intercalateBuilders :: B.Builder -> [B.Builder] -> B.Builder
intercalateBuilders = curry $ \case
                               (_,   [])     -> mempty
                               (sep, (x:xs)) -> x <> foldr (\b acc -> sep <> b <> acc) mempty xs


-- Low-Level Text Utilities
ind :: Int -> B.Builder
ind n = B.fromText (Data.Text.replicate n " ")


escapeString :: Text -> B.Builder
escapeString txt = "\"" <> B.fromText txt <> "\""


isNil :: Expr t -> Bool
isNil = \case { Expr.Nil -> True; _ -> False }


stripObjectBrackets :: Int -> [SchemaDirective] -> Expr t -> B.Builder
stripObjectBrackets indent env = \case
                                  Expr.Object props _ -> buildProps indent env props
                                  other               -> ind indent <> "\"@graph\": " <> buildJSON indent env other


renderProperty :: Int -> [SchemaDirective] -> (Text, Expr.SomeExpr) -> B.Builder
renderProperty indent env (k, Expr.SomeExpr v) = ind indent <> escapeString k <> ": " <> buildJSON indent env v


renderArrayElement :: Int -> [SchemaDirective] -> Expr.SomeExpr -> B.Builder
renderArrayElement indent env (Expr.SomeExpr e) = ind indent <> buildJSON indent env e


-- Emits schema directives inside standard trailing-comma objects
renderSchemaInline :: Int -> [SchemaDirective] -> B.Builder
renderSchemaInline indent directives = case directives of
                                           -- Smart Interception: Single remote string shouldn't be wrapped in braces
                                           [Sch.RemoteContext uri] -> escapeString (URI.render uri)
                                            -- Fallback for multiple or alternative context directives
                                           _                       -> case null directives of
                                                                          True  -> "{}"
                                                                          False -> "{\n"
                                                                                <> intercalateBuilders ",\n" (map renderDirective directives)
                                                                                <> "\n"
                                                                                <> ind indent
                                                                                <> "}"
    where
    renderDirective = \case
                       Sch.ClearContext          -> ind (indent + 2) <> "\"@context\": null"
                       Sch.DefineTerm    k   def -> ind (indent + 2) <> escapeString k <> ": " <> escapeString (Sch.targetIRI def)
                       Sch.RemoteContext uri     -> ind (indent + 2) <> "\"@context\": "       <> escapeString (URI.render uri)
                       Sch.SetBase       txt     -> ind (indent + 2) <> "\"@base\": "     <> escapeString txt
                       Sch.SetLanguage   txt     -> ind (indent + 2) <> "\"@language\": " <> escapeString txt
                       Sch.SetVocab (Left uri)   -> ind (indent + 2) <> "\"@vocab\": "    <> escapeString (URI.render uri)
                       Sch.SetVocab (Right txt)  -> ind (indent + 2) <> "\"@vocab\": "    <> escapeString txt
