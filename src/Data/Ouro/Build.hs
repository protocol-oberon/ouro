{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

-- Building modules: every graph of every module, compiled together or not at all. Which
-- modules a project has, and where their graphs are written, is the host's.
module Data.Ouro.Build
( BuildResult (..)
, buildModules
) where

import           Control.Monad.Validate       (refute, runValidateT)
import           Control.Monad.Writer         (runWriterT)
import           Data.Foldable                (toList)
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map.Strict              as Map
import           Data.Ouro                    (evaluateGraph)
import           Data.Ouro.Error.Types        (OuroError (..), OuroWarning (..))
import qualified Data.Ouro.Internal.Expr      as I
import qualified Data.Ouro.Internal.Kinds     as JLD
import           Data.Ouro.Lisp.Eval.Types    (freeze)
import           Data.Ouro.Lisp.Module.Loader (LoadEnv, Linked (..), loadModules,
                                               runLoader)
import           Data.Ouro.Lisp.Module.Types  (HigherExpression (..),
                                               graphRegistry)
import           Data.Text                    (Text)
import           Lens.Micro.Platform          ((^.))
import           Text.Megaparsec              (SourcePos)


-- Every graph of a build, by its key, or every error in the build. A build of modules keys a
-- graph by its module and name; a project's build, by the file it is written to (fmap
-- changes the keys).
data BuildResult k
    = BuildSuccess [OuroWarning] [(k, I.Expr 'JLD.Primitive)]
    | BuildFailure [OuroWarning] (NonEmpty OuroError)
    deriving Functor


-- Every graph of every module, loaded through the one loader, so a module imported by many
-- is read once; each module is reported at the position given with it, if it cannot be
-- loaded. Nothing is built unless everything compiles, and then every error is reported.
-- Each graph is keyed by its module and name; where it is written is up to the caller.
buildModules :: Monad m => LoadEnv m -> [(SourcePos, Text)] -> m (BuildResult (Text, Text))
buildModules loader modules = buildResult <$> runWriterT (runValidateT build')
    where
    build' = do
        linked <- runLoader loader (loadModules modules)
        let compiled = [ ((name, graphName), freeze <$> evaluateGraph (linkedImports l) (linkedModule l) graph)
                       | ((_, name), l) <- zip modules linked
                       , graph@(Graph _ graphName _) <- Map.elems (linkedModule l ^. graphRegistry) ]
        case NE.nonEmpty (concat [ toList errs | (_, Left errs) <- compiled ]) of
            Just errs -> refute errs
            Nothing   -> pure [ (file, value) | (file, Right value) <- compiled ]

    buildResult = \case
        (Right outputs, warnings) -> BuildSuccess warnings outputs
        (Left errs, warnings)     -> BuildFailure warnings errs
