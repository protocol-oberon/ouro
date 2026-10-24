-- Loading the modules a module imports. Each is read, lexed, parsed, linted and desugared on
-- its own, loaded once however often it is imported, and linked into its own root frame.
module Data.Ouro.Lisp.Module.Loader
( LoadEnv (..)
, LoaderT
, Lookup (..)
, Origin (..)
, bundledOnly
, fromDirectory
, fromSources
, loadImports
, runLoader
) where

import           Control.Monad                (when)
import           Control.Monad.Reader         (ReaderT, asks, local, runReaderT)
import           Control.Monad.State.Strict   (StateT, gets, modify, runStateT)
import           Control.Monad.Trans          (lift)
import           Control.Monad.Validate       (refute, runValidateT)
import           Control.Monad.Writer         (runWriterT, tell)
import qualified Data.Map.Strict              as Map
import           Data.Ouro.Error.Diagnostics  (importCycle, moduleNotFound,
                                               moduleUnreadable,
                                               shadowedBundledModule)
import qualified Data.Ouro.Error.Linter       as LN
import           Data.Ouro.Error.Types        (OuroError (..), OuroWarning (..))
import           Data.Ouro.Internal.Compiler  (CompilerT)
import           Data.Ouro.Internal.Source    (readSource)
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
import           System.Directory             (doesFileExist)
import           System.FilePath              (normalise, (<.>), (</>))


-- Where a module was found: in the project, or bundled with the compiler
data Origin
    = Project
    | Bundled
    deriving (Eq, Show)


-- What looking for a module found: its file and source, the files looked for when there was
-- none, or a file that could not be read, and why
data Lookup
    = Found FilePath Text Origin
    | Missing [FilePath]
    | Unreadable FilePath Text


-- Where a module's source comes from. The import stack is the modules being loaded,
-- innermost first: importing one of them again would be a cycle.
data LoadEnv m = LoadEnv
    { readModule  :: Text -> m Lookup
    , importStack :: [Text]
    }


-- The modules loaded so far, by path
type Loaded = Map.Map Text L.Imported

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


-- Only the modules bundled with the compiler
bundledOnly :: Monad m => LoadEnv m
bundledOnly = fromSources Map.empty


-- Modules given as sources in memory, by path, as well as the bundled ones. A module at
-- path a/b is read as the file a/b.ouro, and replaces a bundled module of the same name.
fromSources :: Monad m => Map.Map Text Text -> LoadEnv m
fromSources sources = LoadEnv
    { readModule  = \path -> pure $ case Map.lookup path sources of
                                        Just source -> Found (T.unpack path <> ".ouro") source Project
                                        Nothing     -> fromBundled path [T.unpack path <> ".ouro"]
    , importStack = []
    }


-- Modules in files under a directory, the importing file's: (import a/b) reads root/a/b.ouro,
-- which replaces a bundled module of the same name. A path is normalised, so one written with
-- / on Windows is shown with one kind of separator.
fromDirectory :: FilePath -> LoadEnv IO
fromDirectory root = LoadEnv
    { readModule  = \path -> do
                        let file = normalise (root </> T.unpack path <.> "ouro")
                        exists <- doesFileExist file
                        if exists
                            then either (Unreadable file) (\source -> Found file source Project) <$> readSource file
                            else pure (fromBundled path [file])
    , importStack = []
    }


-- A bundled module, when nothing in the project has its name; otherwise the files looked for
fromBundled :: Text -> [FilePath] -> Lookup
fromBundled path searched =
    case Map.lookup path bundled of
        Just (file, source) -> Found file source Bundled
        Nothing             -> Missing searched


-- The modules a module imports, by qualifier. The imports are independent, so every one
-- that fails is reported.
loadImports :: Monad m => Module -> LoaderT m (Map.Map Text L.Imported)
loadImports m = Map.fromList <$> traverse (\imp -> (qualifierOf imp,) <$> loadImport imp) (m ^. imports)


-- One import: the module, loaded and linked the first time it is imported. A module that is
-- still being loaded is a cycle, since Ouro has no recursion across files either.
loadImport :: Monad m => Import -> LoaderT m L.Imported
loadImport imp = do
    stack <- asks importStack
    if importPath imp `elem` stack
        then refute (pure (OuroError (importPos imp) (importCycle (reverse (importPath imp : stack)))))
        else gets (Map.lookup (importPath imp)) >>= maybe load pure
    where
    load = do
        reader <- asks readModule
        found  <- fromBase (reader (importPath imp))
        case found of
            Missing searched
                -> refute (pure (OuroError (importPos imp) (moduleNotFound (importPath imp) searched (Map.keys bundled))))

            Unreadable file reason
                -> refute (pure (OuroError (importPos imp) (moduleUnreadable (importPath imp) (T.pack file) reason)))

            Found file source origin
                -> do
                   -- A module of the project may take a bundled module's name, and replaces it
                   when (origin == Project && Map.member (importPath imp) bundled) $
                       tell [OuroWarning (importPos imp) (shadowedBundledModule (importPath imp) (T.pack file))]
                   tokens <- either (refute . pure) pure (LX.tokenize file source)
                   tell (LN.lintExpression tokens)
                   parsed <- either (refute . pure) pure (LP.parseModule tokens)
                   tell (LN.lintModule parsed)
                   let canonical = Canon.canonicalModule parsed
                   dependencies <- local (\env -> env { importStack = importPath imp : importStack env })
                                         (loadImports canonical)
                   let linked = L.Imported
                                    { L.importedRoot    = L.defaultEnv dependencies canonical
                                    , L.importedExports = Set.fromList (map fst (canonical ^. exports))
                                    }
                   modify (Map.insert (importPath imp) linked)
                   pure linked

    -- A read in the base monad: disk in the CLI, a map in the tests
    fromBase = lift . lift . lift . lift
