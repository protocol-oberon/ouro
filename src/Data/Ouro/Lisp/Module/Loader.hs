-- Loading the modules a module imports. Each is read, lexed, parsed, linted and desugared on
-- its own, loaded once however often it is imported, and linked into its own root frame.
module Data.Ouro.Lisp.Module.Loader
( LoadEnv (..)
, LoaderT
, bundledOnly
, loadImports
, runLoader
) where

import           Control.Monad.Reader         (ReaderT, asks, runReaderT)
import           Control.Monad.State.Strict   (StateT, gets, modify, runStateT)
import           Control.Monad.Trans          (lift)
import           Control.Monad.Validate       (refute, runValidateT)
import           Control.Monad.Writer         (runWriterT, tell)
import qualified Data.Map.Strict              as Map
import           Data.Ouro.Error.Diagnostics  (moduleNotFound)
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
import           Lens.Micro                   ((^.))


-- Where a module's source comes from: its file name, for positions, and its text
newtype LoadEnv m = LoadEnv
    { readModule :: Text -> m (Maybe (FilePath, Text))
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
bundledOnly = LoadEnv { readModule = pure . (`Map.lookup` bundled) }


-- The modules a module imports, by qualifier. The imports are independent, so every one
-- that fails is reported.
loadImports :: Monad m => Module -> LoaderT m (Map.Map Text L.Imported)
loadImports m = Map.fromList <$> traverse (\imp -> (qualifierOf imp,) <$> loadImport imp) (m ^. imports)


-- One import: the module, loaded and linked the first time it is imported
loadImport :: Monad m => Import -> LoaderT m L.Imported
loadImport imp = gets (Map.lookup (importPath imp)) >>= maybe load pure
    where
    load = do
        reader <- asks readModule
        found  <- fromBase (reader (importPath imp))
        case found of
            Nothing
                -> refute (pure (OuroError (importPos imp) (moduleNotFound (importPath imp) (Map.keys bundled))))

            Just (file, source)
                -> do
                   tokens <- either (refute . pure) pure (LX.tokenize file source)
                   tell (LN.lintExpression tokens)
                   parsed <- either (refute . pure) pure (LP.parseModule tokens)
                   tell (LN.lintModule parsed)
                   let canonical = Canon.canonicalModule parsed
                   dependencies <- loadImports canonical
                   let linked = L.Imported
                                    { L.importedRoot    = L.defaultEnv dependencies canonical
                                    , L.importedExports = Set.fromList (map fst (canonical ^. exports))
                                    }
                   modify (Map.insert (importPath imp) linked)
                   pure linked

    -- A read in the base monad: disk in the CLI, a map in the tests
    fromBase = lift . lift . lift . lift
