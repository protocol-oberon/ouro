{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

-- Building modules: the graphs of each module, every one or those named, compiled together
-- or not at all. Which modules a project has, and where their graphs are written, is the host's.
module Data.Ouro.Build
( BuildResult (..)
, Graphs (..)
, buildModules
) where

import           Control.Monad.Validate       (refute, runValidateT)
import           Control.Monad.Writer         (runWriterT)
import           Data.Foldable                (toList)
import           Data.Function                ((&))
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map.Strict              as Map
import           Data.Ouro                    (evaluateGraph)
import           Data.Ouro.Error.Diagnostics  (nonExistentGraph)
import           Data.Ouro.Error.Types        (ErrorContext (..), OuroError (..),
                                               OuroWarning (..), ScopeError (..))
import qualified Data.Ouro.Internal.Expr      as I
import qualified Data.Ouro.Internal.Kinds     as JLD
import           Data.Ouro.Internal.Utils     (rankBySimilarity)
import           Data.Ouro.Lisp.Eval.Types    (freeze)
import           Data.Ouro.Lisp.Module.Loader (LoadEnv, Linked (..), loadModules,
                                               runLoader)
import           Data.Ouro.Lisp.Module.Types  (Declaration (..),
                                               HigherExpression (..),
                                               graphRegistry)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Lens.Micro.Platform          ((^.))
import           Text.Megaparsec              (SourcePos, sourceName)


-- Every graph of a build, by its key, or every error in the build. A build of modules keys a
-- graph by its module and name; a project's build, by the file it is written to (fmap
-- changes the keys).
data BuildResult k
    = BuildSuccess [OuroWarning] [(k, I.Expr 'JLD.Primitive)]
    | BuildFailure [OuroWarning] (NonEmpty OuroError)
    deriving Functor


-- Which graphs of a module a build compiles: every one, or those named
data Graphs
    = AllGraphs
    | OnlyGraphs [Text]
    deriving (Show, Eq)


-- The graphs of every module, loaded through the one loader, so a module imported by many is
-- read once; each module is reported at the position given with it, if it cannot be loaded. A
-- graph named that its module does not have is SCP0004, with the module's closest graph, or,
-- for a module with no graphs, SCP0005. Nothing is built unless everything compiles, and then
-- every error is reported. Each graph is keyed by its module and name; where it is written is
-- up to the caller.
buildModules :: Monad m => LoadEnv m -> [(SourcePos, Text, Graphs)] -> m (BuildResult (Text, Text))
buildModules loader modules = buildResult <$> runWriterT (runValidateT build')
    where
    build' = do
        linked <- runLoader loader (loadModules [ (pos, name) | (pos, name, _) <- modules ])
        let chosen   = [ (name, l, selected pos l graphs) | ((pos, name, graphs), l) <- zip modules linked ]
            missing  = concat [ errs | (_, _, Left errs) <- chosen ]
            compiled = [ ((name, graphName), freeze <$> evaluateGraph (linkedImports l) (linkedModule l) graph)
                       | (name, l, Right graphs) <- chosen
                       , graph@(Graph _ graphName _) <- graphs ]
        case NE.nonEmpty (missing <> concat [ toList errs | (_, Left errs) <- compiled ]) of
            Just errs -> refute errs
            Nothing   -> pure [ (file, value) | (file, Right value) <- compiled ]

    -- The graphs of a loaded module a build compiles, or why one named is not there: reported at
    -- the closest graph, or, when the module has none, at the position given with the module
    selected :: SourcePos -> Linked -> Graphs -> Either [OuroError] [HigherExpression 'GraphExpr]
    selected at l = \case
        AllGraphs        -> Right (Map.elems registry)
        OnlyGraphs names -> case [ notFound name | name <- names, not (Map.member name registry) ] of
                                []   -> Right [ graph | name <- names, Just graph <- [Map.lookup name registry] ]
                                errs -> Left errs
        where
        registry = linkedModule l ^. graphRegistry

        notFound name = case [ (closest, graph) | (closest, _) <- rankBySimilarity name (Map.keys registry), Just graph <- [Map.lookup closest registry] ] of
            (closest, Graph pos _ _) : _ -> nonExistentGraph name closest & OuroError pos
            []                           -> Scope (NoCompilationTarget (T.pack (sourceName at))) & OuroError at

    buildResult = \case
        (Right outputs, warnings) -> BuildSuccess warnings outputs
        (Left errs, warnings)     -> BuildFailure warnings errs
