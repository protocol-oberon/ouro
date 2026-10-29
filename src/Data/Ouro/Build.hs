{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

-- Building a project: the build file that describes it, build.ouro, the modules under its
-- source roots, and every graph of every module, compiled together or not at all.
module Data.Ouro.Build
( BuildFile (..)
, BuildResult (..)
, Project (..)
, buildFileName
, buildModules
, buildProject
, compileInProject
, discoverModules
, findBuildFile
, outputFile
, projectOut
, projectRoots
, readBuildFile
) where

import           Control.Monad               (filterM, forM)
import           Control.Monad.Validate      (refute, runValidateT)
import           Control.Monad.Writer        (runWriterT, tell)
import           Data.Foldable               (toList)
import           Data.Function               ((&))
import           Data.Functor.Identity       (runIdentity)
import           Data.List                   (nub, sortOn)
import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map.Strict             as Map
import           Data.Ouro                   (CompilationResult (..),
                                              compileFile, compileUsing,
                                              evaluateGraph, readSource)
import           Data.Ouro.Error.Diagnostics (ambiguousModule, buildSettings,
                                              missingBuildSetting,
                                              missingSourceFolder, noBuildGraph,
                                              notBuildData, typeMismatch,
                                              unknownBuildSetting,
                                              unsupportedBuildSetting,
                                              withBlurb)
import qualified Data.Ouro.Error.Linter      as LN
import           Data.Ouro.Error.Types       (OuroError (..), OuroWarning (..))
import           Data.Ouro.Internal.Compiler (CompilerM)
import qualified Data.Ouro.Internal.Expr     as I
import qualified Data.Ouro.Internal.Kinds    as JLD
import qualified Data.Ouro.Lisp.Canon        as Canon
import           Data.Ouro.Lisp.Eval.Types   (freeze)
import qualified Data.Ouro.Lisp.Eval.Types   as L
import qualified Data.Ouro.Lisp.Lexer        as LX
import           Data.Ouro.Lisp.Module.Loader (LoadEnv, Linked (..), fromRoots,
                                              loadModules, runLoader)
import           Data.Ouro.Lisp.Module.Types (Declaration (..),
                                              HigherExpression (..), Import (..),
                                              Module, exports, functionRegistry,
                                              graphRegistry, imports)
import qualified Data.Ouro.Lisp.Parser       as LP
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Lens.Micro.Platform         ((^.))
import           System.Directory            (doesDirectoryExist, doesFileExist,
                                              listDirectory, makeAbsolute,
                                              makeRelativeToCurrentDirectory)
import           System.FilePath             (dropExtension, equalFilePath,
                                              joinPath, makeRelative, normalise,
                                              splitDirectories, takeDirectory,
                                              takeExtension, (<.>), (</>))
import           Text.Megaparsec             (SourcePos, initialPos)


-- The name of a build file, looked for in a directory and the directories above it
buildFileName :: FilePath
buildFileName = "build.ouro"


-- A build file's settings. Its folders are as written, relative to the build file.
data BuildFile = BuildFile
    { buildName    :: Text
    , buildSources :: [FilePath]   -- The source roots, (".") when not given
    , buildOut     :: FilePath     -- Where the graphs are written, "out" when not given
    , buildAt      :: SourcePos    -- The build graph, where its settings are reported
    } deriving (Show, Eq)


-- A build file and where it is: its folders are relative to its directory
data Project = Project
    { projectFile  :: FilePath
    , projectBuild :: BuildFile
    } deriving (Show, Eq)


-- The source roots of a project, each once
projectRoots :: Project -> [FilePath]
projectRoots project = nub [ normalise (takeDirectory (projectFile project) </> root) | root <- buildSources (projectBuild project) ]


-- Where a project's graphs are written
projectOut :: Project -> FilePath
projectOut project = normalise (takeDirectory (projectFile project) </> buildOut (projectBuild project))


-- Every graph of a build, by the file it is written to, or every error in the build
data BuildResult
    = BuildSuccess [OuroWarning] [(FilePath, I.Expr 'JLD.Primitive)]
    | BuildFailure [OuroWarning] (NonEmpty OuroError)


-- The build file in a directory or the nearest directory above it. It is named relative to the
-- current directory when it is under it, so its errors and its modules' are shown as written.
findBuildFile :: FilePath -> IO (Maybe FilePath)
findBuildFile start = makeAbsolute start >>= search
    where
    search dir = do
        let file = dir </> buildFileName
        exists <- doesFileExist file
        case (exists, takeDirectory dir) of
            (True, _)                          -> Just <$> makeRelativeToCurrentDirectory file
            (False, parent) | parent /= dir    -> search parent
            _atTheRoot                         -> pure Nothing


-- Reads a build file: lexed, parsed and evaluated as a module is, but it is data, so it holds
-- one graph named build and nothing else, and the graph's value is checked against the
-- build file's schema. Every error is reported, with the warnings found on the way.
readBuildFile :: FilePath -> Text -> ([OuroWarning], Either (NonEmpty OuroError) BuildFile)
readBuildFile file source = swap (runIdentity (runWriterT (runValidateT readIt)))
    where
    swap (result, warnings) = (warnings, result)

    readIt :: CompilerM BuildFile
    readIt = do
             tokens <- either (refute . pure) pure (LX.tokenize file source)
             tell (LN.lintExpression tokens)
             parsed <- either (refute . pure) pure (LP.parseModule tokens)
             tell (LN.lintModule parsed)
             let canonical = Canon.canonicalModule parsed
             graph <- either refute pure (buildGraph file canonical)
             value <- either refute pure (evaluateGraph Map.empty canonical graph)
             case graph of
                 Graph gPos _ _ -> either refute pure (settings gPos value)


-- The build file's one graph, named build: anything else it declares is an error
buildGraph :: FilePath -> Module -> Either (NonEmpty OuroError) (HigherExpression 'GraphExpr)
buildGraph file m =
    case (NE.nonEmpty (sortOn errPos (others <> unnamed)), Map.lookup "build" graphs) of
        (Nothing, Just graph) -> Right graph
        (Just errs, _)        -> Left errs
        (Nothing, Nothing)    -> Left (pure (OuroError (initialPos file) (noBuildGraph Nothing)))
    where
    graphs = m ^. graphRegistry

    others = [ OuroError (importPos imp) (notBuildData "an import") | imp <- m ^. imports ]
          <> [ OuroError pos (notBuildData "an export") | (_, pos) <- m ^. exports ]
          <> [ OuroError pos (notBuildData "a function") | Function pos _ _ _ _ <- Map.elems (m ^. functionRegistry) ]
          <> [ OuroError pos (notBuildData "a second graph") | Graph pos _ _ <- extra ]

    -- Without a graph named build, the first graph is reported as misnamed, and any others
    -- as second graphs
    (unnamed, extra) = case Map.member "build" graphs of
                           True  -> ([], [ g | g@(Graph _ name _) <- Map.elems graphs, name /= "build" ])
                           False -> case sortOn (\(Graph pos _ _) -> pos) (Map.elems graphs) of
                                        Graph pos name _ : rest -> ([OuroError pos (noBuildGraph (Just name))], rest)
                                        []                      -> ([], [])


-- The build graph's value against the build file's schema: a record of known settings, with
-- :name required. Every setting that is wrong is reported, at the graph.
settings :: SourcePos -> L.Expr -> Either (NonEmpty OuroError) BuildFile
settings pos = \case
    L.Record _ pairs
        -> let setting key = lookup key pairs
               unknown     = [ at (unknownBuildSetting key) | (key, _) <- pairs, key `notElem` buildSettings ]
               unsupported = [ at (unsupportedBuildSetting key) | (key, _) <- pairs, key `elem` ["graphs", "depends"] ]
               name        = maybe (Left (pure (at (missingBuildSetting "name")))) (text "name") (setting "name")
               sources     = maybe (Right ["."]) folders (setting "sources")
               out         = maybe (Right "out") (fmap T.unpack . text "out") (setting "out")
           in case NE.nonEmpty (unknown <> unsupported <> problems name <> problems sources <> problems out) of
                  Just errs -> Left errs
                  Nothing   -> BuildFile <$> name <*> sources <*> out <*> pure pos

    other
        -> Left (pure (mismatch "a Record" other
                         "A build file's graph is a record of settings, as in (graph build (:name \"my-project\"))."))
    where
    at = OuroError pos

    problems = either toList (const [])

    text key = \case
        L.Primitive (I.String t) -> Right t
        other                    -> Left (pure (mismatch "#string" other ("The build setting :" <> key <> " is a String.")))

    -- :sources is an Array of folders, even when it names one
    folders = \case
        L.Array elements
            -> let checked = [ text ("sources[" <> T.pack (show i) <> "]") element | (i, element) <- zip [0 :: Int ..] elements ]
               in case NE.nonEmpty (concatMap problems checked) of
                      Just errs -> Left errs
                      Nothing   -> map T.unpack <$> sequence checked
        other
            -> Left (pure (mismatch "an Array of #string" other
                             "The build setting :sources is an Array of folders, as in (\"src\"), even when it names one."))

    mismatch expected actual blurb = typeMismatch expected (L.humanReadableType actual) & withBlurb blurb & at


-- The modules under a project's source roots, each with the files it was found in: a module
-- in more than one root has more than one. A module's name is its path under its root,
-- written with '/'. The build file, hidden folders (.git, .jj) and the out folder are skipped,
-- as is a root that does not exist, which the build reports.
discoverModules :: Project -> IO (Map.Map Text [FilePath])
discoverModules project = do
    roots <- filterM doesDirectoryExist (projectRoots project)
    found <- forM roots $ \root -> do
                 files <- walk root
                 pure [ (moduleName root file, [file]) | file <- files ]
    pure (Map.fromListWith (flip (<>)) (concat found))
    where
    walk dir = do
        entries <- map (dir </>) . filter (not . hidden) <$> listDirectory dir
        dirs    <- filterM doesDirectoryExist entries
        let files = [ entry | entry <- entries, entry `notElem` dirs, takeExtension entry == ".ouro"
                            , not (equalFilePath entry (normalise (projectFile project))) ]
        nested  <- traverse walk (filter (not . equalFilePath (projectOut project)) dirs)
        pure (files <> concat nested)

    hidden = \case
        '.' : _ -> True
        _other  -> False

    moduleName root file = T.intercalate "/" (map T.pack (splitDirectories (dropExtension (makeRelative root file))))


-- Every graph of every module, loaded through the one loader, so a module imported by many
-- is read once; each module is reported at the position given with it, if it cannot be
-- loaded. Nothing is built unless everything compiles, and then every error is reported.
buildModules :: Monad m => LoadEnv m -> FilePath -> [(SourcePos, Text)] -> m BuildResult
buildModules loader out modules = buildResult <$> runWriterT (runValidateT build')
    where
    build' = do
        linked <- runLoader loader (loadModules modules)
        let compiled = [ (outputFile out name graphName, freeze <$> evaluateGraph (linkedImports l) (linkedModule l) graph)
                       | ((_, name), l) <- zip modules linked
                       , graph@(Graph _ graphName _) <- Map.elems (linkedModule l ^. graphRegistry) ]
        case NE.nonEmpty (concat [ toList errs | (_, Left errs) <- compiled ]) of
            Just errs -> refute errs
            Nothing   -> pure [ (file, value) | (file, Right value) <- compiled ]

    buildResult = \case
        (Right outputs, warnings) -> BuildSuccess warnings outputs
        (Left errs, warnings)     -> BuildFailure warnings errs


-- Where a graph of a module is written: out/<module>/<graph>.json
outputFile :: FilePath -> Text -> Text -> FilePath
outputFile out name graph = out </> joinPath (map T.unpack (T.splitOn "/" name)) </> T.unpack graph <.> "json"


-- Builds the project whose build file is in a directory or above it. A build file that cannot
-- be found or read is a message, since there is no source to show an error in.
buildProject :: FilePath -> IO (Either Text BuildResult)
buildProject dir = withProject dir $ \case
    (warnings, Left errs) -> pure (BuildFailure warnings errs)
    (warnings, Right project)
        -> do
           let build = projectBuild project
           missing  <- filterM (fmap not . doesDirectoryExist . snd)
                               [ (folder, normalise (takeDirectory (projectFile project) </> folder)) | folder <- buildSources build ]
           found    <- discoverModules project
           let unique    = [ (initialPos file, name) | (name, [file]) <- Map.toList found ]
               ambiguous = [ OuroError (buildAt build) (ambiguousModule name (map T.pack files)) | (name, files@(_ : _ : _)) <- Map.toList found ]
               absent    = [ OuroError (buildAt build) (missingSourceFolder (T.pack folder) (T.pack looked)) | (folder, looked) <- missing ]
           built <- buildModules (fromRoots (projectRoots project)) (projectOut project) unique
           pure $ case (built, NE.nonEmpty (absent <> ambiguous)) of
                      (BuildSuccess ws _, Just errs)     -> BuildFailure (warnings <> ws) errs
                      (BuildSuccess ws outputs, Nothing) -> BuildSuccess (warnings <> ws) outputs
                      (BuildFailure ws errs, extra)      -> BuildFailure (warnings <> ws) (maybe errs (<> errs) extra)
    where
    withProject start continue = findBuildFile start >>= \case
        Nothing   -> pure (Left ("No " <> T.pack buildFileName <> " in " <> T.pack start <> " or any folder above it."))
        Just file -> readSource file >>= \case
                         Left  err    -> pure (Left err)
                         Right source -> let (warnings, read') = readBuildFile file source
                                         in Right <$> continue (warnings, Project file <$> read')


-- Compiles a graph of a file as the build would: its imports are read from the :sources of
-- the build file in its directory or above it, and from the file's own directory when there
-- is none. A build file that cannot be read is a message.
compileInProject :: FilePath -> String -> Text -> IO (Either Text CompilationResult)
compileInProject path trgt content = findBuildFile (takeDirectory path) >>= \case
    Nothing   -> Right <$> compileFile path trgt content
    Just file -> readSource file >>= \case
        Left  err    -> pure (Left err)
        Right source -> case readBuildFile file source of
            (warnings, Left errs)  -> pure (Right (CompilationFailure warnings errs))
            (warnings, Right build) -> Right . withWarnings warnings
                                       <$> compileUsing (fromRoots (projectRoots (Project file build))) path trgt content
    where
    withWarnings ws = \case
        CompilationSuccess more ast  -> CompilationSuccess (ws <> more) ast
        CompilationFailure more errs -> CompilationFailure (ws <> more) errs
