-- Loading the modules a module imports. Each is read, lexed, parsed, linted and desugared on
-- its own, loaded once however often it is imported, and linked into its own root frame.
module Data.Ouro.Lisp.Module.Loader
( LoadEnv (..)
, Linked (..)
, LoaderT
, Lookup (..)
, ModuleKey (..)
, Resolved (..)
, bundledOnly
, bundledRoots
, fromSources
, loadImports
, loadModules
, runLoader
) where

import           Control.Monad.Reader         (ReaderT, asks, local, runReaderT)
import           Control.Monad.State.Strict   (StateT, gets, modify, runStateT)
import           Control.Monad.Trans          (lift)
import           Control.Monad.Validate       (refute, runValidateT)
import           Control.Monad.Writer         (runWriterT, tell)
import qualified Data.Map.Strict              as Map
import           Data.Ouro.Error.Diagnostics  (ambiguousModule, importCycle,
                                               moduleNotFound,
                                               moduleUnreadable)
import qualified Data.Ouro.Error.Linter       as LN
import           Data.Ouro.Error.Types        (OuroError (..))
import           Data.Ouro.Internal.Compiler  (CompilerT)
import qualified Data.Ouro.Lisp.Canon         as Canon
import qualified Data.Ouro.Lisp.Eval.Types    as L
import qualified Data.Ouro.Lisp.Lexer         as LX
import           Data.Ouro.Lisp.Module.Bundled (bundled)
import           Data.Ouro.Lisp.Module.Types  (Import (..), Module, exports,
                                               imports, qualifierOf)
import qualified Data.Ouro.Lisp.Parser        as LP
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Lens.Micro                   ((^.))
import           Text.Megaparsec              (SourcePos)


-- A module's identity: a bundled module by its path, or one of the host's by the key the
-- host gave it. The two never meet, so a host's key can be any text.
data ModuleKey
    = BundledModule Text
    | HostModule Text
    deriving (Eq, Ord, Show)


-- What the host found for an import: the key of the module it names, the files looked for
-- when there is none, or the files of a module in more than one root
data Resolved
    = Resolved Text
    | Missing [FilePath]
    | Ambiguous [FilePath]


-- What reading a module found: its file, for positions, and its source, or a file that
-- cannot be read, and why
data Lookup
    = Found FilePath Text
    | Unreadable FilePath Text


-- Where modules come from, which the host gives: the command line's files, a test's map, an
-- embedding program's own store. The host resolves an import, from the module importing it
-- (Nothing for the file compiled and the modules of a build) and the path imported, to a key
-- of its choosing, and reads a module by that key. Keys are only compared, and shown in an
-- import cycle, so a host gives readable ones. Imports of the bundled modules never reach the
-- host (see 'bundledRoots'). The import stack is the modules being loaded, innermost first:
-- importing one of them again would be a cycle.
data LoadEnv m = LoadEnv
    { resolveModule :: Maybe Text -> Text -> m Resolved
    , readModule    :: Text -> m Lookup
    , importStack   :: [ModuleKey]
    }


-- A loaded module: its canonical form, the modules it imports by qualifier, and the module
-- linked into its own root frame, which is what importing it gives
data Linked = Linked
    { linkedModule  :: Module
    , linkedImports :: Map.Map Text L.Imported
    , linkedRoot    :: L.Imported
    }


-- The modules loaded so far, by key
type Loaded = Map.Map ModuleKey Linked

-- The cache of loaded modules is a State *under* ValidateT. A StateT above it would build
-- its <*> from >>=, and stop at the first error, so two missing imports would report one
-- (see Control.Monad.Validate's documentation). ReaderT's <*> is the <*> of the monad below
-- it, so it keeps the errors of independent imports together.
type LoaderT m = ReaderT (LoadEnv m) (CompilerT (StateT Loaded m))


-- Runs a loader as part of a compile: its warnings are told, and its errors raised
runLoader :: Monad m => LoadEnv m -> LoaderT m a -> CompilerT m a
runLoader env loader = do
    ((result, warnings), _loaded) <- lift (lift (runStateT (runWriterT (runValidateT (runReaderT loader env))) Map.empty))
    tell warnings
    either refute pure result


-- The bundled modules' folders, std and aat. An import whose first segment is one of them
-- names a bundled module, whatever the host holds, so std/base means the same in every
-- program that runs Ouro.
bundledRoots :: Set.Set Text
bundledRoots = Set.fromList [ T.takeWhile (/= '/') name | name <- Map.keys bundled ]


-- Only the modules bundled with the compiler
bundledOnly :: Monad m => LoadEnv m
bundledOnly = fromSources Map.empty


-- Modules given as sources in memory, by path, as well as the bundled ones. A module at
-- path a/b is read as the file a/b.ouro, and its key is its path.
fromSources :: Monad m => Map.Map Text Text -> LoadEnv m
fromSources sources = LoadEnv
    { resolveModule = \_importer path -> pure $ if Map.member path sources
                                                    then Resolved path
                                                    else Missing [T.unpack path <> ".ouro"]
    , readModule    = \key -> let file = T.unpack key <> ".ouro"
                              in pure (maybe (Unreadable file "No module has this key.") (Found file) (Map.lookup key sources))
    , importStack   = []
    }


-- The modules a module imports, by qualifier. The imports are independent, so every one
-- that fails is reported.
loadImports :: Monad m => Module -> LoaderT m (Map.Map Text L.Imported)
loadImports m = Map.fromList <$> traverse (\imp -> (qualifierOf imp,) <$> loadImport imp) (m ^. imports)


-- Modules loaded by path, each with what it imports, as a build loads every module of a
-- project: through the one cache, so a module imported by many is still loaded once. A
-- module that cannot be loaded is reported at the position given with it. The modules are
-- independent, so every one that fails is reported.
loadModules :: Monad m => [(SourcePos, Text)] -> LoaderT m [Linked]
loadModules = traverse (\(pos, path) -> resolve pos path >>= loadKey pos path)


-- One import: the module, loaded and linked the first time it is imported. A module that is
-- still being loaded is a cycle, since Ouro has no recursion across files either.
loadImport :: Monad m => Import -> LoaderT m L.Imported
loadImport imp = do
    key   <- resolve (importPos imp) (importPath imp)
    stack <- asks importStack
    if key `elem` stack
        then refute (pure (OuroError (importPos imp) (importCycle (reverse (map keyText (key : stack))))))
        else linkedRoot <$> loadKey (importPos imp) (importPath imp) key


-- The module a path names, from the module importing it: a bundled one when the path's first
-- segment is a bundled folder, and otherwise the host's. A bundled module imports only bundled
-- ones, so the host is never asked from one. Anything wrong is reported at the given position.
resolve :: Monad m => SourcePos -> Text -> LoaderT m ModuleKey
resolve pos path
    | T.takeWhile (/= '/') path `Set.member` bundledRoots
        = if Map.member path bundled then pure (BundledModule path) else notFound []
    | otherwise
        = asks importStack >>= \case
              BundledModule _ : _ -> notFound []
              HostModule key : _  -> fromHost (Just key)
              []                  -> fromHost Nothing
    where
    fromHost importer = do
        resolver <- asks resolveModule
        fromBase (resolver importer path) >>= \case
            Resolved key     -> pure (HostModule key)
            Missing searched -> notFound searched
            Ambiguous files  -> refute (pure (OuroError pos (ambiguousModule path (map T.pack files))))

    notFound searched = refute (pure (OuroError pos (moduleNotFound path searched (Map.keys bundled))))


-- The module with a key, loaded and linked the first time it is asked for. A file that cannot
-- be read is reported at the given position, by the path imported.
loadKey :: Monad m => SourcePos -> Text -> ModuleKey -> LoaderT m Linked
loadKey pos path key = gets (Map.lookup key) >>= maybe load pure
    where
    load = do
        found <- case key of
                     BundledModule name -> pure (maybe (Unreadable (T.unpack name) "It is not bundled.") (uncurry Found) (Map.lookup name bundled))
                     HostModule name    -> asks readModule >>= \reader -> fromBase (reader name)
        case found of
            Unreadable file reason
                -> refute (pure (OuroError pos (moduleUnreadable path (T.pack file) reason)))

            Found file source
                -> do
                   tokens <- either (refute . pure) pure (LX.tokenize file source)
                   tell (LN.lintExpression tokens)
                   parsed <- either (refute . pure) pure (LP.parseModule tokens)
                   tell (LN.lintModule parsed)
                   let canonical = Canon.canonicalModule parsed
                   dependencies <- local (\env -> env { importStack = key : importStack env })
                                         (loadImports canonical)
                   let linked = Linked
                                    { linkedModule  = canonical
                                    , linkedImports = dependencies
                                    , linkedRoot    = L.Imported
                                                          { L.importedRoot    = L.defaultEnv dependencies canonical
                                                          , L.importedExports = Set.fromList (map fst (canonical ^. exports))
                                                          }
                                    }
                   modify (Map.insert key linked)
                   pure linked


-- A key as it is shown, in an import cycle
keyText :: ModuleKey -> Text
keyText = \case
    BundledModule name -> name
    HostModule key     -> key


-- A read in the base monad: disk in the CLI, a map in the tests
fromBase :: Monad m => m a -> LoaderT m a
fromBase = lift . lift . lift . lift
