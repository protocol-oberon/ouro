{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Data.Ouro.Json.Serializer where

import           Control.Monad.Identity    (Identity, runIdentity)
import           Control.Monad.Reader      (MonadReader (..), ReaderT (..),
                                            asks)
import           Control.Monad.State       (MonadState, StateT (..), modify)
import           Data.Ouro.Internal.Expr   (Expr)
import qualified Data.Ouro.Internal.Expr   as Expr
import qualified Data.Ouro.Internal.Kinds  as JLD
import           Data.Ouro.Internal.Schema (Schema (..), SchemaDirective)
import qualified Data.Ouro.Internal.Schema as Sch
import           Data.Text                 (Text, replicate)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Builder    as B
import qualified Data.Time.Format          as TF
import           Lens.Micro                (Lens', over, to, (%~), (^.))
import qualified Text.URI                  as URI


-- Global runtime configuration
data PrinterOptions = PrinterOptions
    { indentSpacing :: !Int
    }


defaultOptions :: PrinterOptions
defaultOptions = PrinterOptions { indentSpacing = 2 }


-- PrinterEnv.
--
-- Dynamic environment that updates as we enter sub-structures
-- It tracks the structural depth of the JSON via nesting level
data PrinterEnv = PrinterEnv
    { options      :: !PrinterOptions
    , nestingLevel :: !Int
    }


-- PrinterState.
--
-- A linear, append-only ledger for the accumulation of side effects during serialization.
-- Uses a lazy text builder to accrue JSON as the AST is descended upon.
--
-- We use a strict bang pattern ('!') on [SchemaDirective] to avoid potential space leaks
-- during deep AST traversals. Since we modify this list linearly within a state layer
-- upon entry to an Expr.Context closure, we want this metadata to evaluate immediately.
--
-- We use a Builder to drastically increase performance compared to normal string
-- concatenation; it works by generating an internal execution graph of append actions,
-- deferring chunk allocation until the entire output JSON is produced in a single pass.
data PrinterState = PrinterState
    { activeDirectives :: ![SchemaDirective]
    , outputBuffer     :: !B.Builder
    }

-- Printer a.
--
-- A newtype wrapper implementing a custom Reader-State architecture. We manually layer
-- ReaderT and StateT over the Identity monad—combined with strict record fields—to completely
-- bypass standard RWST space leaks.
--
-- ReaderT PrinterEnv handles the downward flow of contextual layout configurations, allowing
-- functions to alter the indentation level locally without impacting parent structures.
--
-- StateT PrinterState serves as our linear execution timeline, recording read-write state
-- mutations that persist across the lifetime of the AST traversal. This allows strings to be
-- appended to the outputBuffer and context rules to be pushed onto activeDirectives.
--
-- The Identity monad anchors the transformer stack, guaranteeing that the entire JSON
-- serialization process remains purely functional and free of arbitrary side effects.
newtype Printer a = Printer
    { runPrinterP :: ReaderT PrinterEnv (StateT PrinterState Identity) a
    } deriving (Functor, Applicative, Monad, MonadReader PrinterEnv, MonadState PrinterState)


-- toJSON.
--
-- A pure function that transforms a JLD AST into a pure JSON Text stream.
-- Monadic evaluation for the AST occurs by applying the initial environment and state to
-- runReaderT and runStateT before passing the computation to runIdentity. Monad transformers
-- do not exist in a vacuum; they must always layer over a base monad. Because this serializer
-- is entirely pure, we use Identity as our base to eliminate runtime monadic overhead.
--
-- Running a StateT computation unpacks a tuple of (result, finalState). Because our AST engine
-- is an append-only writer loop that returns an empty unit '()', we use a wildcard hole ('_')
-- to discard the useless return value. The accumulated JSON data *is* our side effect; by binding
-- only 'finalState', we capture the modified state record containing our completed buffer.
toJSON :: PrinterOptions -> Expr t -> TL.Text
toJSON opts expr =
    let initEnv   = PrinterEnv   { options = opts, nestingLevel = 0 }
        initState = PrinterState { activeDirectives = [], outputBuffer = mempty }

        -- Run the underlying transformer stack transformations layers
        (_, finalState) = runIdentity $ runStateT (runReaderT (runPrinterP (buildJSON expr)) initEnv) initState

        -- Append POSIX trailing newline
        finalBuffer = outputBuffer finalState <> "\n"
    in B.toLazyText finalBuffer


-- buildJSON.
--
-- A recursive descent encoder that returns a monadic ledger of commands needed to produce the output JSON.
-- We pattern match on each node of the JLD AST to either produce an immediate monadic command within the
-- current context of the Printer monad, or to recur deeper down the tree.
--
-- Do notation is used liberally to cleanly sequence layout and indentation changes.
buildJSON :: Expr t -> Printer ()
buildJSON = \case
              -- Primatives
              Expr.String    txt   -> tell $ escapeString txt
              Expr.Number    n     -> tell $ B.fromString $ show n
              Expr.Boolean   True  -> tell "true"
              Expr.Boolean   False -> tell "false"
              Expr.URI       uri   -> tell $ escapeString $ URI.render uri
              Expr.Date      date  -> tell $ escapeString $ T.pack $ TF.formatTime  TF.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" date
              Expr.Null            -> tell "null"
              Expr.BlankNode blank -> tell $ escapeString $ "_:" <> blank
              Expr.EmptyArr        -> tell $ "[]"
              Expr.EmptyObj        -> tell $ "{}"
              Expr.EmptyMeta       -> tell "null"

              -- Structural Closures
              Expr.Object metadata body -> renderFlatObject metadata body

              Expr.Array elems -> case Expr.flattenArray elems of
                                      [] -> tell "[]"
                                      ls -> do
                                            tell "[\n"
                                            nested $ intercalateM ",\n" (map renderArrayElement ls)
                                            tell "\n"
                                            emitIndent
                                            tell "]"


              -- Fallback handling for raw backbone pieces
              Expr.Cons h t -> do
                               tell "[\n"
                               nested $ buildArrayElements (Expr.Cons h t)
                               tell "\n"
                               emitIndent
                               tell "]"

              Expr.Attr k v -> do
                               tell "{\n"
                               nested $ do
                                        emitIndent
                                        tell (escapeString k <> ": ")
                                        buildJSON v
                               tell "\n"
                               emitIndent
                               tell "}"

              Expr.Nil -> pure ()


-- Manual Lenses for PrinterEnv
optionsL :: Lens' PrinterEnv PrinterOptions
optionsL f e = (\o -> e { options = o }) <$> f (options e)


nestingLevelL :: Lens' PrinterEnv Int
nestingLevelL f e = (\l -> e { nestingLevel = l }) <$> f (nestingLevel e)


activeDirectivesL :: Lens' PrinterState [SchemaDirective]
activeDirectivesL f s = (\d -> s { activeDirectives = d }) <$> f (activeDirectives s)


outputBufferL :: Lens' PrinterState B.Builder
outputBufferL f s = (\b -> s { outputBuffer = b }) <$> f (outputBuffer s)


-- Layout
-- Append a chunk to the state buffer
tell :: B.Builder -> Printer ()
tell chunk = modify (outputBufferL %~ (<> chunk))


-- nested.
--
-- Increments the lexical scoping level for an inner printing block.
--
-- We use the lens 'over' combinator to update the environment record point-free.
-- Instead of manually extracting the structure, modifying the field, and rebuilding
-- the record, 'over' maps (+ 1) directly over the target lens focus in-place.
--
-- Combined with 'local', this isolates the nesting change to just the inner computation;
-- the state automatically rolls back when the nested block finishes executing.
nested :: Printer a -> Printer a
nested = local (over nestingLevelL (+ 1))


-- Generate and emit line pading
emitIndent :: Printer ()
emitIndent = do
             -- Using 'to' to safely dive inside the nested PrinterOptions record
             spacing <- asks (^. optionsL . to indentSpacing)
             level   <- asks (^. nestingLevelL)
             tell $ B.fromText (Data.Text.replicate (level * spacing ) " ")


-- Serializes a flat JSON-LD Object block by unifying its metadata leaf
-- and data body fields into a single key-value brace block.
renderFlatObject :: Expr 'JLD.Meta -> Expr 'JLD.List -> Printer ()
renderFlatObject metadata body =
    do
    let bodyPairs = Expr.flattenProps body

    case metadata of
        -- Case A: Object has an atomic Context leaf
        Expr.Context (Schema directives)
            -> do
               modify (activeDirectivesL %~ (++ directives))
               tell "{\n"
               nested $ do
                       emitIndent
                       tell "\"@context\": "
                       renderSchemaInline directives

                       -- Interleave data properties if they exist alongside the context keys
                       case bodyPairs of
                           [] -> pure ()
                           ps -> do
                               tell ",\n"
                               intercalateM ",\n" (map renderProperty ps)
               tell "\n"
               emitIndent
               tell "}"

        -- Case B: Plain object with no schema metadata tracking
        Expr.EmptyMeta
            -> case bodyPairs of
                   [] -> tell "{}"
                   ps -> do
                           tell "{\n"
                           nested $ intercalateM ",\n" (map renderProperty ps)
                           tell "\n"
                           emitIndent
                           tell "}"


-- Formats an individual object key-value property pair with indentation alignment.
renderProperty :: (Text, Expr.SomeExpr) -> Printer ()
renderProperty (k, Expr.SomeExpr v) = do
                                      emitIndent
                                      tell (escapeString k <> ": ")
                                      buildJSON v


-- Unrolls schema directives inline, prioritizing a raw string for standalone remote context references.
renderSchemaInline :: [SchemaDirective] -> Printer ()
renderSchemaInline directives = case directives of
                                    -- Single remote string shouldn't be wrapped in braces
                                    [Sch.RemoteContext uri] -> tell (escapeString (URI.render uri))
                                    _ | null directives     -> tell "{}"
                                      | otherwise           -> do
                                                               tell "{\n"
                                                               nested $ do
                                                                        -- Map each directive to a Printer action and join them with ",\n"
                                                                        intercalateM ",\n" (map renderDirective directives)
                                                               tell "\n"
                                                               emitIndent -- Automatically aligns with the parent indentation
                                                               tell "}"
    where
    renderDirective :: Sch.SchemaDirective -> Printer ()
    renderDirective d = do
                        emitIndent
                        case d of
                            Sch.ClearContext       -> tell "\"@context\": null"
                            Sch.DefineTerm k def   -> tell (escapeString k <> ": " <> escapeString (Sch.targetIRI def))
                            Sch.RemoteContext uri  -> tell ("\"@context\": "       <> escapeString (URI.render uri))
                            Sch.SetBase txt        -> tell ("\"@base\": "          <> escapeString txt)
                            Sch.SetLanguage txt    -> tell ("\"@language\": "      <> escapeString txt)
                            Sch.SetVocab (Left u)  -> tell ("\"@vocab\": "         <> escapeString (URI.render u))
                            Sch.SetVocab (Right t) -> tell ("\"@vocab\": "         <> escapeString t)


-- Recursively unrolls sequential list elements onto individual indented lines separated by commas.
buildArrayElements :: Expr t -> Printer ()
buildArrayElements expr = case expr of
                              Expr.Cons h Expr.Nil -> do
                                                      emitIndent
                                                      buildJSON h

                              Expr.Cons h t -> do
                                               emitIndent
                                               buildJSON h
                                               tell ",\n"
                                               buildArrayElements t

                              other -> buildJSON other


-- Unpacks an existential wrapper to render an individual item inside a JSON collection.
renderArrayElement :: Expr.SomeExpr -> Printer ()
renderArrayElement (Expr.SomeExpr e) = do
                                       emitIndent
                                       buildJSON e

-- Text Utils
-- Wraps raw textual fragments inside a pair of escaped double quotes for correct JSON string conformity.
escapeString :: Text -> B.Builder
escapeString txt = "\"" <> B.fromText txt <> "\""


-- isNIl.
--
-- Collections in our JLD (Arrays and Objects) are repesented as linked list, using Cons and Nil nodes.
-- Expr.Nil is the explict base case, a terminal sentinel value.
isNil :: Expr t -> Bool
isNil = \case { Expr.Nil -> True; _ -> False }


-- intercalateM.
--
-- A monadic intercalator that stitches a collection of layout actions together.
-- It sequences a list of printing computations while seamlessly interleaving a static
-- separator ('B.Builder') directly between adjacent elements.
--
-- We pattern match to cleanly isolate the execution tracks:
--   1. An empty action list safely halts immediately with a no-op ('pure ()').
--   2. A populated list runs the first element standalone ('x'), completely bypassing
--      trailing comma artifacts. It then uses 'mapM_' to efficiently zip the separator
--      ('tell sep') ahead of each remaining sibling node. intercalator to loop through structures
intercalateM :: B.Builder -> [Printer ()] -> Printer ()
intercalateM = curry $ \case
                        (_,   [])     -> pure ()
                        (sep, (x:xs)) -> x >> mapM_ (\action -> tell sep >> action) xs
