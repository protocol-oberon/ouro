{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

-- A project on disk: a folder, the settings its package.ouro gives, if it has one, the modules
-- in the files under its :sources, and where its graphs are written. The compiler knows none
-- of this: it builds the modules it is given (Data.Ouro.Build), through the loader given to it.
module CLI.Project
( Package (..)
, Project (..)
, buildProject
, compileFile
, defaultPackage
, discoverModules
, fromRoots
, outputFile
, packageFileName
, projectOut
, projectRoot
, readPackage
) where

import           Control.Monad                (filterM)
import           Control.Monad.Validate       (refute, runValidateT)
import           Control.Monad.Writer         (runWriterT, tell)
import           Data.Foldable                (toList)
import           Data.Function                ((&))
import           Data.Functor.Identity        (runIdentity)
import           Data.List                    (nub, sortOn)
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map.Strict              as Map
import           Data.Ouro                    (CompilationResult (..),
                                               compileUsing, evaluateGraph,
                                               readSource)
import           Data.Ouro.Build              (BuildResult (..), buildModules)
import           Data.Ouro.Error.Diagnostics  (buildSettings, missingSourceFolder,
                                               noPackageFunction, notPackageData,
                                               packageParameters,
                                               reservedModule,
                                               reservedPackageName,
                                               reservedPackageNames, typeMismatch,
                                               unknownBuildSetting,
                                               unsupportedBuildSetting,
                                               withBlurb)
import qualified Data.Ouro.Error.Linter       as LN
import           Data.Ouro.Error.Types        (OuroError (..), OuroWarning (..))
import           Data.Ouro.Internal.Compiler  (CompilerM)
import qualified Data.Ouro.Internal.Expr      as I
import qualified Data.Ouro.Lisp.Canon         as Canon
import qualified Data.Ouro.Lisp.Eval.Types    as L
import qualified Data.Ouro.Lisp.Lexer         as LX
import           Data.Ouro.Lisp.Module.Loader (LoadEnv (..), Lookup (..),
                                               Resolved (..), bundledOnly,
                                               bundledRoots, loadImports,
                                               runLoader)
import           Data.Ouro.Lisp.Module.Types  (HigherExpression (..), Module,
                                               exports, functionRegistry,
                                               graphRegistry)
import qualified Data.Ouro.Lisp.Parser        as LP
import qualified Data.Ouro.Lisp.Surface       as S
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Lens.Micro.Platform          ((^.))
import           System.Directory             (canonicalizePath,
                                               doesDirectoryExist, doesFileExist,
                                               listDirectory)
import           System.FilePath              (dropExtension, equalFilePath,
                                               joinPath, makeRelative, normalise,
                                               splitDirectories, takeDirectory,
                                               takeExtension, takeFileName, (<.>),
                                               (</>))
import           Text.Megaparsec              (SourcePos, initialPos)


-- The name of a project's settings file, in the project's folder
packageFileName :: FilePath
packageFileName = "package.ouro"


-- A project's settings. Its folders are as written, relative to the project's folder.
data Package = Package
    { packageSources :: FilePath    -- The source folder, "." when not given
    , packageOutputs :: FilePath    -- Where the graphs are written, "out" when not given
    , packageAt      :: SourcePos   -- The package function, where its settings are reported
    } deriving (Show, Eq)


-- The settings of a project without a package.ouro: its folder is its source folder
defaultPackage :: FilePath -> Package
defaultPackage dir = Package "." "out" (initialPos (normalise (dir </> packageFileName)))


-- A project: its folder, and its settings
data Project = Project
    { projectDir     :: FilePath
    , projectPackage :: Package
    } deriving (Show, Eq)


-- A project's source folder
projectRoot :: Project -> FilePath
projectRoot project = normalise (projectDir project </> packageSources (projectPackage project))


-- Where a project's graphs are written
projectOut :: Project -> FilePath
projectOut project = normalise (projectDir project </> packageOutputs (projectPackage project))


-- Reads a package.ouro: lexed, parsed and evaluated as a module is, but it holds one function,
-- (defn package () ...), whose value is checked against the settings' schema. It can import
-- the bundled modules, and only them. Every error is reported, with the warnings found on the way.
readPackage :: FilePath -> Text -> ([OuroWarning], Either (NonEmpty OuroError) Package)
readPackage file source = swap (runIdentity (runWriterT (runValidateT readIt)))
    where
    swap (result, warnings) = (warnings, result)

    readIt :: CompilerM Package
    readIt = do
             tokens <- either (refute . pure) pure (LX.tokenize file source)
             tell (LN.lintExpression tokens)
             parsed <- either (refute . pure) pure (LP.parseModule tokens)
             tell (LN.lintModule parsed)
             let canonical = Canon.canonicalModule parsed
             pos      <- either refute pure (packageFunction file canonical)
             imported <- runLoader bundledOnly (loadImports canonical)
             -- Its value is a call of it, (package), evaluated as a graph is
             value    <- either refute pure (evaluateGraph imported canonical (Graph pos "package" (S.Form pos [S.Symbol pos "package"])))
             either refute pure (settings pos value)


-- package.ouro's one function, package, of no parameters, and where it is: anything else it
-- declares is an error
packageFunction :: FilePath -> Module -> Either (NonEmpty OuroError) SourcePos
packageFunction file m =
    case (NE.nonEmpty (sortOn errPos (others <> unnamed)), Map.lookup "package" functions) of
        (Nothing, Just (Function pos _ _ _ _)) -> Right pos
        (Just errs, _)                         -> Left errs
        (Nothing, Nothing)                     -> Left (pure (OuroError (initialPos file) (noPackageFunction Nothing)))
    where
    functions = m ^. functionRegistry

    others = [ OuroError pos (notPackageData "an export") | (_, pos) <- m ^. exports ]
          <> [ OuroError pos (notPackageData "a graph") | Graph pos _ _ <- Map.elems (m ^. graphRegistry) ]
          <> [ OuroError pos (reservedPackageName name) | Function pos name _ _ _ <- Map.elems functions, name `elem` reservedPackageNames ]
          <> [ OuroError pos (packageParameters (length params)) | Just (Function pos _ _ params _) <- [Map.lookup "package" functions], not (null params) ]
          <> [ OuroError pos (notPackageData "a function") | Function pos _ _ _ _ <- extra ]

    -- The functions that are neither package nor reserved. Without a function named package,
    -- the first of them is reported as misnamed, and any others as other functions.
    unreserved = sortOn (\(Function pos _ _ _ _) -> pos)
                        [ f | f@(Function _ name _ _ _) <- Map.elems functions, name /= "package", name `notElem` reservedPackageNames ]

    (unnamed, extra) = case (Map.member "package" functions, unreserved) of
                           (True, _)                               -> ([], unreserved)
                           (False, Function pos name _ _ _ : rest) -> ([OuroError pos (noPackageFunction (Just name))], rest)
                           (False, [])                             -> ([], [])


-- The package function's value against the settings' schema: a record of known settings,
-- none required, so #record-empty is every default. Every setting that is wrong is reported,
-- at the function.
settings :: SourcePos -> L.Expr -> Either (NonEmpty OuroError) Package
settings pos = \case
    L.Record _ pairs      -> fromPairs pairs
    L.Primitive I.EmptyRec -> fromPairs []
    other
        -> Left (pure (mismatch "a Record" other
                         "The package function's value is a record of settings, as in (defn package () (:sources \"src\"))."))
    where
    fromPairs pairs =
        let setting key = lookup key pairs
            unknown     = [ at (unknownBuildSetting key) | (key, _) <- pairs, key `notElem` buildSettings ]
            unsupported = [ at (unsupportedBuildSetting key) | (key, _) <- pairs, key `elem` ["graphs", "ouro"] ]
            sources     = maybe (Right ".") (folder "sources") (setting "sources")
            outputs     = maybe (Right "out") (folder "outputs") (setting "outputs")
        in case NE.nonEmpty (unknown <> unsupported <> problems sources <> problems outputs) of
               Just errs -> Left errs
               Nothing   -> Package <$> sources <*> outputs <*> pure pos

    at = OuroError pos

    problems = either toList (const [])

    folder key = \case
        L.Primitive (I.String t) -> Right (T.unpack t)
        other                    -> Left (pure (mismatch "#string" other ("The package setting :" <> key <> " is a String.")))

    mismatch expected actual blurb = typeMismatch expected (L.humanReadableType actual) & withBlurb blurb & at


-- The modules under a project's source folder, each with its file, by name. A module's name is
-- its path under the folder, written with '/'. Skipped: hidden folders (.git, .jj), the outputs
-- folder, every package.ouro, and every folder holding one, which is a project of its own.
-- Folders are compared as they are on disk, so a source folder written through .. still skips
-- the outputs. A source folder that does not exist has none, which the build reports.
discoverModules :: Project -> IO [(Text, FilePath)]
discoverModules project = do
    let root = projectRoot project
    exists <- doesDirectoryExist root
    out    <- canonicalizePath (projectOut project)
    files  <- if exists then walk out root else pure []
    pure (sortOn fst [ (moduleName root file, file) | file <- files ])
    where
    walk out dir = do
        entries <- map (dir </>) . filter (not . hidden) <$> listDirectory dir
        dirs    <- filterM doesDirectoryExist entries
        let files = [ entry | entry <- entries, entry `notElem` dirs, takeExtension entry == ".ouro", takeFileName entry /= packageFileName ]
        kept    <- filterM (built out) dirs
        nested  <- traverse (walk out) kept
        pure (files <> concat nested)

    built out dir = do
        canonical <- canonicalizePath dir
        project'  <- doesFileExist (dir </> packageFileName)
        pure (not (equalFilePath canonical out) && not project')

    hidden = \case
        '.' : _ -> True
        _other  -> False

    moduleName root file = T.intercalate "/" (map T.pack (splitDirectories (dropExtension (makeRelative root file))))


-- Modules in files under source roots, the importing file's directory or a project's :sources:
-- (import a/b) reads root/a/b.ouro, and its key is its path, a/b. A module in more than one
-- root is ambiguous, never the first root's. A path is normalised, so one written with / on
-- Windows is shown with one kind of separator.
fromRoots :: [FilePath] -> LoadEnv IO
fromRoots roots = LoadEnv
    { resolveModule = \_importer path -> do
                          found <- existing path
                          pure $ case found of
                              [_file] -> Resolved path
                              []      -> Missing (candidates path)
                              _many   -> Ambiguous found
    , readModule    = \key -> existing key >>= \case
                          [file] -> either (Unreadable file) (Found file) <$> readSource file
                          _other -> pure (Unreadable (T.unpack key <> ".ouro") "Its file was moved or deleted while the build ran.")
    , importStack   = []
    }
    where
    candidates path = nub [ normalise (root </> T.unpack path <.> "ouro") | root <- roots ]
    existing        = filterM doesFileExist . candidates


-- Where a graph of a module is written: out/<module>/<graph>.json
outputFile :: FilePath -> Text -> Text -> FilePath
outputFile out name graph = out </> joinPath (map T.unpack (T.splitOn "/" name)) </> T.unpack graph <.> "json"


-- Builds the project in a folder: every graph of every module under its :sources, to
-- <outputs>/<module>/<graph>.json. A module under a bundled folder, std or aat, is an error,
-- and is not built. A folder that does not exist, or a package.ouro that cannot be read, is a
-- message, since there is no source to show an error in.
buildProject :: FilePath -> IO (Either Text (BuildResult FilePath))
buildProject dir = doesDirectoryExist dir >>= \case
    False -> pure (Left ("No folder " <> T.pack dir <> ": ouro build builds the project in a folder."))
    True  -> readProject >>= \case
        Left message                    -> pure (Left message)
        Right (warnings, Left errs)     -> pure (Right (BuildFailure warnings errs))
        Right (warnings, Right project) -> Right <$> build warnings project
    where
    readProject = do
        let file = normalise (dir </> packageFileName)
        exists <- doesFileExist file
        if not exists
            then pure (Right ([], Right (Project dir (defaultPackage dir))))
            else readSource file >>= \case
                     Left  err    -> pure (Left err)
                     Right source -> let (warnings, read') = readPackage file source
                                     in pure (Right (warnings, Project dir <$> read'))

    build warnings project = do
        let package = projectPackage project
            root    = projectRoot project
        rootExists <- doesDirectoryExist root
        found      <- discoverModules project
        let isReserved name = T.takeWhile (/= '/') name `Set.member` bundledRoots
            absent   = [ OuroError (packageAt package) (missingSourceFolder (T.pack (packageSources package)) (T.pack root)) | not rootExists ]
            reserved = [ OuroError (initialPos file) (reservedModule name) | (name, file) <- found, isReserved name ]
            modules  = [ (initialPos file, name) | (name, file) <- found, not (isReserved name) ]
        built <- fmap (uncurry (outputFile (projectOut project)))
                     <$> buildModules (fromRoots [root]) modules
        pure $ case (built, NE.nonEmpty (absent <> reserved)) of
                   (BuildSuccess ws _, Just errs)     -> BuildFailure (warnings <> ws) errs
                   (BuildSuccess ws outputs, Nothing) -> BuildSuccess (warnings <> ws) outputs
                   (BuildFailure ws errs, extra)      -> BuildFailure (warnings <> ws) (maybe errs (<> errs) extra)


-- Compiles a file on disk: it imports the modules in files beside it, a/b.ouro for (import a/b),
-- and the bundled ones. An import of std/… or aat/… is always the bundled module.
compileFile :: FilePath -> String -> Text -> IO CompilationResult
compileFile path = compileUsing (fromRoots [takeDirectory path]) path
