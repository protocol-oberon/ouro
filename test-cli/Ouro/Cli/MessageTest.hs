module Ouro.Cli.MessageTest
( messages
) where

import           CLI.Printer         (renderDiagnostic)
import           CLI.Project         (buildProject)
import           CLI.Source          (readSource)
import qualified Data.ByteString     as BS
import           Data.Foldable       (toList)
import           Data.List           (nub, sort)
import           Data.String         (fromString)
import qualified Data.Map.Strict     as Map
import           Data.Ouro           (CompilationResult (..), OuroError (..),
                                      OuroWarning (..), bundledFile,
                                      compileWith)
import           Data.Ouro.Build     (BuildResult (..))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import           Hedgehog
import           System.Directory    (doesDirectoryExist, listDirectory)
import           System.Environment  (lookupEnv)
import           System.FilePath     (dropExtension, takeExtension,
                                      takeFileName, (<.>), (</>))
import           System.Info         (os)
import           Text.Megaparsec     (sourceName)


-- Every message Ouro prints, one fixture for each code a source or a project can raise, named
-- after it: CODE-Constructor.ouro, a file compiled on its own for its graph g, or CODE-Constructor/,
-- a project built. Its diagnostics are rendered as ouro prints them, without their colours, and
-- compared with the golden file beside it, CODE-Constructor.txt. With OURO_ACCEPT set, the golden
-- files are written instead, so a change of wording is reviewed as a change of the golden file.
-- Never raised, so without a fixture: SYN0002, SYN0005 and the retired SYN0004, SCP0011 (only a host with several
-- roots raises it), the internal errors, the retired BLD0001, BLD0003, BLD0004, and BLD0002, and
-- the warnings ElidedPropertyAssign and UnboundTypeReference, the retired WRN0000 and WRN0005, and
-- WRN0001, DeprecatedSyntax, which nothing is deprecated by since list became an error.
messages :: IO Group
messages = do
    fixtures <- sort . filter isFixture <$> listDirectory messagesDir
    pure (Group "Messages: every code, as ouro prints it" [ (fromString fixture, golden fixture) | fixture <- fixtures ])
    where
    isFixture name = takeExtension name /= ".txt"


-- The fixtures, read where the tests run: the package's folder
messagesDir :: FilePath
messagesDir = "test-cli" </> "fixtures" </> "messages"


-- A fixture's rendering against its golden file, or written to it with OURO_ACCEPT. Its own code
-- must be among what it prints, so a fixture cannot test another code by mistake. An import spelt
-- in another case than its file is only found where file names ignore case (BLD0014).
golden :: FilePath -> Property
golden fixture = withTests 1 . property $
    case (code == "BLD0014", os `elem` ["mingw32", "darwin"]) of
        (True, False) -> success
        _otherwise    -> do
            rendered <- evalIO (render (messagesDir </> fixture))
            annotate (T.unpack rendered)
            assert (("[" <> code <> "]") `T.isInfixOf` rendered)
            accept <- evalIO (lookupEnv "OURO_ACCEPT")
            case accept of
                Just _  -> evalIO (BS.writeFile goldenFile (TE.encodeUtf8 rendered))
                Nothing -> do
                           expected <- evalIO (TE.decodeUtf8 <$> BS.readFile goldenFile)
                           T.lines (T.filter (/= '\r') expected) === T.lines rendered
    where
    code       = T.takeWhile (/= '-') (T.pack fixture)
    goldenFile = messagesDir </> dropExtension fixture <.> "txt"


-- The diagnostics of a fixture, each rendered, warnings first: a file compiled for its graph g,
-- or a folder built as a project. A project's paths are as ouro build test-cli/fixtures/messages/…
-- prints them, with '/' on every system, so the golden files read the same everywhere.
render :: FilePath -> IO Text
render path = doesDirectoryExist path >>= \case
    False -> do
             source <- either (error . T.unpack) id <$> readSource path
             let file = takeFileName path
             pure $ case compileWith Map.empty file "g" source of
                        CompilationSuccess warnings _  -> diagnostics (known file source) warnings []
                        CompilationFailure warnings es -> diagnostics (known file source) warnings (toList es)
    True  -> buildProject path [] >>= \case
                 Left message                     -> pure (slashes message)
                 Right (BuildSuccess warnings _)  -> slashes <$> fromDisk warnings []
                 Right (BuildFailure warnings es) -> slashes <$> fromDisk warnings (toList es)
    where
    diagnostics sourceOf warnings errs =
        T.concat (map (renderDiagnostic sourceOf . Left) warnings <> map (renderDiagnostic sourceOf . Right) errs)

    known file source name = case name == file of
                                 True  -> Just source
                                 False -> bundledFile name

    -- The sources of the files the diagnostics are in, read from disk or bundled
    fromDisk warnings errs = do
        let files = nub (map (sourceName . warnPos) warnings <> map (sourceName . errPos) errs)
        read' <- traverse (\file -> (,) file . either (const (bundledFile file)) Just <$> readSource file) files
        pure (diagnostics (\file -> case lookup file read' of
                                        Just source -> source
                                        Nothing     -> bundledFile file) warnings errs)

    -- After rendering, so '\' and '/', the same width, leave the lines wrapped where they were
    slashes = case os of
                  "mingw32" -> T.replace "\\" "/"
                  _other    -> id

