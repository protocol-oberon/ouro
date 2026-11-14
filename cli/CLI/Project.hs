{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

-- A project on disk: a folder, the settings its package.ouro gives, if it has one, the modules
-- in the files under its :sources, and where its graphs are written. The compiler knows none
-- of this: it builds the modules it is given (Data.Ouro.Build), through the loader given to it.
module CLI.Project
( Collection (..)
, Package (..)
, Project (..)
, buildProject
, compileFile
, defaultPackage
, discoverModules
, fromProject
, fromRoots
, outputFile
, ouroVersion
, packageFileName
, projectOut
, projectRoot
, readCollections
, readPackage
, vendorFolderName
) where

import           CLI.Source                   (readSource)
import           Control.Monad                (filterM)
import           Control.Monad.Validate       (refute, runValidateT)
import           Control.Monad.Writer         (runWriterT, tell)
import           Data.Char                    (isDigit)
import           Data.Foldable                (toList)
import           Data.Function                ((&))
import           Data.Functor.Identity        (runIdentity)
import           Data.List                    (nub, sort, sortOn)
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map.Strict              as Map
import           Data.Ouro                    (CompilationResult (..),
                                               compileUsing, evaluateGraph)
import           Data.Ouro.Build              (BuildResult (..), Graphs (..),
                                               buildModules)
import           Data.Ouro.Error.Diagnostics  (buildSettings, collectionOverlap,
                                               graphSelectionForm, importCase,
                                               invalidVersion, missingSourceFolder,
                                               nestedVendor, newerOuro,
                                               noPackageFunction, notPackageData,
                                               packageParameters,
                                               reservedCollection, reservedModule,
                                               reservedPackageName,
                                               reservedPackageNames, typeMismatch,
                                               unknownBuildSetting, unknownModule,
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
import           Data.Version                 (versionBranch)
import           Lens.Micro.Platform          ((^.))
import           Paths_ouro                   (version)
import           System.Directory             (canonicalizePath,
                                               doesDirectoryExist, doesFileExist,
                                               listDirectory)
import           System.FilePath              (dropExtension,
                                               dropTrailingPathSeparator,
                                               equalFilePath, joinPath,
                                               makeRelative, normalise,
                                               splitDirectories, takeDirectory,
                                               takeExtension, takeFileName, (<.>),
                                               (</>))
import           Text.Megaparsec              (SourcePos, initialPos)


-- The name of a project's settings file, in the project's folder
packageFileName :: FilePath
packageFileName = "package.ouro"


-- A project's settings. Its folders are as written, relative to the project's folder.
data Package = Package
    { packageSources :: FilePath      -- The source folder, "." when not given
    , packageOutputs :: FilePath      -- Where the graphs are written, "out" when not given
    , packageGraphs  :: Maybe [Text]  -- The graphs built, as module/graph, every one when not given
    , packageAt      :: SourcePos     -- The package function, where its settings are reported
    } deriving (Show, Eq)


-- The settings of a project without a package.ouro: its folder is its source folder
defaultPackage :: FilePath -> Package
defaultPackage dir = Package "." "out" Nothing (initialPos (normalise (dir </> packageFileName)))


-- This compiler's version, as :ouro is written: 0.5.0.0
ouroVersion :: Text
ouroVersion = T.intercalate "." (map (T.pack . show) (versionBranch version))


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
-- (define/constant package ...), whose value is checked against the settings' schema. It can import
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
                         "The package function's value is a record of settings, as in (define/constant package (:sources \"src\"))."))
    where
    fromPairs pairs =
        let setting key = lookup key pairs
            unknown     = [ at (unknownBuildSetting key) | (key, _) <- pairs, key `notElem` buildSettings ]
            sources     = maybe (Right ".") (fmap T.unpack . text "sources") (setting "sources")
            outputs     = maybe (Right "out") (fmap T.unpack . text "outputs") (setting "outputs")
            graphs      = maybe (Right Nothing) (fmap Just . graphNames) (setting "graphs")
            needed      = maybe (Right ()) (\value -> text "ouro" value >>= ouro) (setting "ouro")
        in case NE.nonEmpty (unknown <> problems sources <> problems outputs <> problems graphs <> problems needed) of
               Just errs -> Left errs
               Nothing   -> Package <$> sources <*> outputs <*> graphs <*> pure pos

    at = OuroError pos

    problems = either toList (const [])

    text key = \case
        L.Primitive (I.String t) -> Right t
        other                    -> Left (pure (mismatch "#string" other ("The package setting :" <> key <> " is a String.")))

    -- :graphs is an Array of graphs, each written module/graph, even when it names one
    graphNames = \case
        L.Primitive I.EmptyArr -> Right []
        L.Array elements
            -> let checked = [ text ("graphs[" <> T.pack (show i) <> "]") element | (i, element) <- zip [0 :: Int ..] elements ]
               in case NE.nonEmpty (concatMap problems checked) of
                      Just errs -> Left errs
                      Nothing   -> sequence checked
        other
            -> Left (pure (mismatch "an Array of #string" other
                             "The package setting :graphs is an Array of graphs, as in (\"people/person\"), even when it names one."))

    -- :ouro is the version of Ouro the project needs: no newer than this compiler
    ouro written = case traverse number (T.splitOn "." written) of
        Nothing     -> Left (pure (at (invalidVersion written)))
        Just needed -> let have   = versionBranch version
                           width  = max (length needed) (length have)
                           padded = take width . (<> repeat 0)
                       in case padded needed > padded have of
                              True  -> Left (pure (at (newerOuro written ouroVersion)))
                              False -> Right ()
        where
        number part = case (T.null part, T.all isDigit part) of
                          (False, True) -> Just (read (T.unpack part) :: Int)
                          _other        -> Nothing

    mismatch expected actual blurb = typeMismatch expected (L.humanReadableType actual) & withBlurb blurb & at


-- The modules under a project's source folder, each with its file, by name. A module's name is
-- its path under the folder, written with '/'. Skipped: hidden folders (.git, .jj), the outputs
-- folder, the vendor folder, every package.ouro, and every folder holding one, which is a
-- project of its own. Folders are compared as they are on disk, so a source folder written
-- through .. still skips the outputs. A source folder that does not exist has none, which the
-- build reports.
discoverModules :: Project -> IO [(Text, FilePath)]
discoverModules project = do
    let root = projectRoot project
    exists  <- doesDirectoryExist root
    skipped <- traverse canonicalizePath [projectOut project, normalise (projectDir project </> vendorFolderName)]
    files   <- case exists of
                   True  -> walk skipped root
                   False -> pure []
    pure (sortOn fst [ (moduleName root file, file) | file <- files ])
    where
    walk skipped dir = do
        entries <- map (dir </>) . filter (not . hidden) <$> listDirectory dir
        dirs    <- filterM doesDirectoryExist entries
        let files = [ entry | entry <- entries, entry `notElem` dirs, takeExtension entry == ".ouro", takeFileName entry /= packageFileName ]
        kept    <- filterM (built skipped) dirs
        nested  <- traverse (walk skipped) kept
        pure (files <> concat nested)

    built skipped dir = do
        canonical <- canonicalizePath dir
        project'  <- doesFileExist (dir </> packageFileName)
        pure (not (any (equalFilePath canonical) skipped) && not project')

    moduleName root file = T.intercalate "/" (map T.pack (splitDirectories (dropExtension (makeRelative root file))))


-- A file or folder whose name starts with a dot, as .git and .jj
hidden :: FilePath -> Bool
hidden = \case
    '.' : _ -> True
    _other  -> False


-- The folder of a project's vendored libraries, beside its package.ouro
vendorFolderName :: FilePath
vendorFolderName = "vendor"


-- A vendored library, a folder of vendor/: its name, which its modules are imported by, its
-- folder, and its source folder, its own package.ouro's :sources or else its folder
data Collection = Collection
    { collectionName :: Text
    , collectionDir  :: FilePath
    , collectionRoot :: FilePath
    } deriving (Show, Eq)


-- The vendored libraries of a project, by name, with the warnings and errors found reading
-- them: a folder named after a bundled one (BLD0011), a library with a vendor/ of its own
-- (BLD0012), its package.ouro's errors, and a source folder that does not exist. A library
-- with an error is not among them. A package.ouro that cannot be read is a message.
readCollections :: FilePath -> IO (Either Text ([OuroWarning], [OuroError], [Collection]))
readCollections dir = do
    let vendor = normalise (dir </> vendorFolderName)
    exists  <- doesDirectoryExist vendor
    names   <- case exists of
                   True  -> filter (not . hidden) . sort <$> (listDirectory vendor >>= filterM (doesDirectoryExist . (vendor </>)))
                   False -> pure []
    results <- traverse (readCollection vendor) names
    pure $ (\read' -> (concat [ ws | (ws, _, _) <- read' ], concat [ errs | (_, errs, _) <- read' ], [ c | (_, _, Just c) <- read' ]))
           <$> sequence results
    where
    readCollection vendor name = do
        let folder = vendor </> name
            text   = T.pack name
        nested <- doesDirectoryExist (folder </> vendorFolderName)
        case (text `Set.member` bundledRoots, nested) of
            (True, _)
                -> pure (Right ([], [OuroError (initialPos folder) (reservedCollection text)], Nothing))
            (False, True)
                -> pure (Right ([], [OuroError (initialPos (folder </> vendorFolderName)) (nestedVendor text)], Nothing))
            (False, False)
                -> readPackageIn folder >>= \case
                       Left message              -> pure (Left message)
                       Right (ws, Left errs)     -> pure (Right (ws, toList errs, Nothing))
                       Right (ws, Right package) -> do
                           let root = normalise (folder </> packageSources package)
                           rootExists <- doesDirectoryExist root
                           pure $ Right $ case rootExists of
                               True  -> (ws, [], Just (Collection text folder root))
                               False -> (ws, [OuroError (packageAt package) (missingSourceFolder (T.pack (packageSources package)) (T.pack root))], Nothing)


-- Every collection name that is also a top-level folder or module of a source root, an error
-- wherever it is, imported or not: (import tate/…) must never have two meanings
overlaps :: [Text] -> FilePath -> IO [OuroError]
overlaps names root = do
    exists  <- doesDirectoryExist root
    entries <- case exists of
                   True  -> filter (not . hidden) <$> listDirectory root
                   False -> pure []
    named   <- traverse (\entry -> (,) entry <$> doesDirectoryExist (root </> entry)) entries
    pure [ OuroError (initialPos (root </> entry)) (collectionOverlap name (T.pack (dropTrailingPathSeparator root)))
         | (entry, isFolder) <- named
         , let name = case isFolder of
                          True  -> T.pack entry
                          False -> T.pack (dropExtension entry)
         , isFolder || takeExtension entry == ".ouro"
         , name `elem` names ]


-- The settings of the project or library in a folder: its package.ouro's, or the defaults
-- when it has none. A package.ouro that cannot be read is a message.
readPackageIn :: FilePath -> IO (Either Text ([OuroWarning], Either (NonEmpty OuroError) Package))
readPackageIn dir = do
    let file = normalise (dir </> packageFileName)
    exists <- doesFileExist file
    case exists of
        False -> pure (Right ([], Right (defaultPackage dir)))
        True  -> readSource file >>= \case
                     Left  err    -> pure (Left err)
                     Right source -> pure (Right (readPackage file source))


-- Modules in the files of a project and its vendored libraries. An import resolves by its first
-- segment: a collection's name reads the module from that library's source folder, and any
-- other path reads it from the source folder of the importing module's own library, the
-- project's for the project's modules. A module's key is its file, as spelt on disk, relative
-- to the project's folder, so a file is loaded once however it is reached. An import spelt in
-- another case than its file is refused (BLD0014), since only some file systems would find it.
fromProject :: FilePath -> FilePath -> [Collection] -> IO (LoadEnv IO)
fromProject dir root collections = do
    home <- canonicalizePath dir
    pure LoadEnv
        { resolveModule = \importer path -> do
              let (base, prefix, rest) = locate importer path
                  written              = splitDirectories (T.unpack rest <.> "ouro")
                  file                 = normalise (joinPath (base : written))
              exists <- doesFileExist file
              case exists of
                  False -> pure (Missing [file])
                  True  -> do
                      canonical <- canonicalizePath file
                      let spelt = reverse (take (length written) (reverse (splitDirectories canonical)))
                      pure $ case spelt == written of
                          True  -> Resolved (T.pack (makeRelative home canonical))
                          False -> Rejected (importCase path (T.intercalate "/" (prefix <> map T.pack (splitDirectories (dropExtension (joinPath spelt))))))
        , readModule    = \key -> let file = normalise (dir </> T.unpack key)
                                  in either (Unreadable file) (Found file) <$> readSource file
        , importStack   = []
        }
    where
    byName = [ (collectionName c, c) | c <- collections ]

    -- The folder an import is read from, the collection it names, if any, and the rest of its path
    locate importer path =
        case T.breakOn "/" path of
            (first, slashRest) | not (T.null slashRest), Just c <- lookup first byName
                -> (collectionRoot c, [first], T.drop 1 slashRest)
            _other
                -> (ownRoot importer, [], path)

    -- The source folder of the library the importing module is in: a key under vendor/<name>/ is
    -- a module of that collection, any other one the project's
    ownRoot = \case
        Just key | v : name : _ <- splitDirectories (T.unpack key), v == vendorFolderName, Just c <- lookup (T.pack name) byName
            -> collectionRoot c
        _other
            -> root


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


-- Builds the project in a folder: the graphs chosen, by -g, else by :graphs, else every graph
-- of every module under its :sources, to <outputs>/<module>/<graph>.json; only the chosen
-- graphs' modules, and what they import, are compiled. Its vendored libraries are read, and
-- their modules compiled only when imported. A module under a bundled folder, std or aat, is an
-- error, and is not built; nor is one under a folder named after a collection, which the overlap
-- reports. A folder that does not exist, or a package.ouro that cannot be read, is a message,
-- since there is no source to show an error in. A graph chosen by -g that is wrong is reported
-- at the argument, "-g people/persn".
buildProject :: FilePath -> [Text] -> IO (Either Text (BuildResult FilePath))
buildProject dir requested = doesDirectoryExist dir >>= \case
    False -> pure (Left ("No folder " <> T.pack dir <> ": ouro build builds the project in a folder."))
    True  -> readPackageIn dir >>= \case
        Left message                    -> pure (Left message)
        Right (warnings, Left errs)     -> pure (Right (BuildFailure warnings errs))
        Right (warnings, Right package) -> readCollections dir >>= \case
            Left message                             -> pure (Left message)
            Right (cWarnings, cErrors, collections) -> Right <$> build (warnings <> cWarnings) cErrors collections (Project dir package)
    where
    build warnings cErrors collections project = do
        let package = projectPackage project
            root    = projectRoot project
        rootExists  <- doesDirectoryExist root
        found       <- discoverModules project
        overlapping <- concat <$> traverse (overlaps (map collectionName collections)) (root : map collectionRoot collections)
        loader      <- fromProject dir root collections
        let firstSegment name  = T.takeWhile (/= '/') name
            isReserved name    = firstSegment name `Set.member` bundledRoots
            isCollection name  = T.isInfixOf "/" name && firstSegment name `elem` map collectionName collections
            absent    = [ OuroError (packageAt package) (missingSourceFolder (T.pack (packageSources package)) (T.pack root)) | not rootExists ]
            reserved  = [ OuroError (initialPos file) (reservedModule name) | (name, file) <- found, isReserved name ]
            buildable = [ (name, file) | (name, file) <- found, not (isReserved name), not (isCollection name) ]
            (wrong, modules) = case (requested, packageGraphs package) of
                (_ : _, _)       -> select buildable [ (initialPos ("-g " <> T.unpack g), g) | g <- requested ]
                ([], Just names) -> select buildable [ (packageAt package, g) | g <- names ]
                ([], Nothing)    -> ([], [ (initialPos file, name, AllGraphs) | (name, file) <- buildable ])
        built <- fmap (uncurry (outputFile (projectOut project)))
                     <$> buildModules loader modules
        pure $ case (built, NE.nonEmpty (absent <> reserved <> cErrors <> overlapping <> wrong)) of
                   (BuildSuccess ws _, Just errs)     -> BuildFailure (warnings <> ws) errs
                   (BuildSuccess ws outputs, Nothing) -> BuildSuccess (warnings <> ws) outputs
                   (BuildFailure ws errs, extra)      -> BuildFailure (warnings <> ws) (maybe errs (<> errs) extra)


-- The modules a build compiles for the graphs chosen, each written module/graph, with the
-- graphs named of each, in the order of the modules' names. A choice not written so (BLD0018),
-- or of a module the project does not build (BLD0017, with the closest module), is reported at
-- the position given with it. A graph its module does not have is the build's to report.
select :: [(Text, FilePath)] -> [(SourcePos, Text)] -> ([OuroError], [(SourcePos, Text, Graphs)])
select buildable chosen = (errors, modules)
    where
    names  = map fst buildable
    parsed = [ (pos, selection, split selection) | (pos, selection) <- chosen ]

    -- module/graph, split at its last '/', both sides non-empty
    split selection = case T.breakOnEnd "/" selection of
        (prefix, graph) | not (T.null (T.dropEnd 1 prefix)), not (T.null graph) -> Just (T.dropEnd 1 prefix, graph)
        _other                                                                   -> Nothing

    errors = [ OuroError pos (graphSelectionForm selection) | (pos, selection, Nothing) <- parsed ]
          <> [ OuroError pos (unknownModule selection name names) | (pos, selection, Just (name, _)) <- parsed, name `notElem` names ]

    byModule = Map.fromListWith (flip (<>)) [ (name, [graph]) | (_, _, Just (name, graph)) <- parsed ]
    modules  = [ (initialPos file, name, OnlyGraphs (nub graphs)) | (name, file) <- buildable, Just graphs <- [Map.lookup name byModule] ]


-- Compiles a file on disk: it imports the modules in files beside it, a/b.ouro for (import a/b),
-- and the bundled ones. An import of std/… or aat/… is always the bundled module.
compileFile :: FilePath -> String -> Text -> IO CompilationResult
compileFile path = compileUsing (fromRoots [takeDirectory path]) path
