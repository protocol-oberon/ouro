{-# LANGUAGE DataKinds #-}

module Utils where

import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Data.Char                    (isSpace)
import           Data.Foldable                (toList)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (fromJust)
import           Data.Ouro                    (CompilationResult (..), compile,
                                               compileWith,
                                               defaultOptions, readSource,
                                               toJSON)
import           Data.Ouro.Error.Diagnostics  (smartErrorCode, warningBlurb,
                                               warningSummary)
import           Data.Ouro.Error.Types        (OuroError (..), OuroWarning (..))
import qualified Data.Ouro.Internal.Expr      as I
import qualified Data.Ouro.Internal.Kinds     as JLD
import           Data.Ouro.Lisp.Eval.Builtins (parseISO8601)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import           Hedgehog
import           Text.Megaparsec              (SourcePos, initialPos, sourceColumn,
                                               sourceLine, sourceName, unPos)


-- runCompileInline :: String -> CompilationResult
-- runCompileInline = compile "Test Suite" . T.pack


shouldCompileTo :: (MonadTest m, MonadIO m) => FilePath -> String -> FilePath -> m ()
shouldCompileTo test graph trgt = do
    -- Both files are read as UTF-8, as the compiler reads its source, whatever the code page
    testContent <- readUtf8 test
    trgtContent <- TL.fromStrict <$> readUtf8 trgt

    let res = compile test graph testContent

    case res of
        CompilationSuccess _ ast -> do
            let json = toJSON defaultOptions ast
            -- Hedgehog's assertion operator provides automatic diffing
            json === trgtContent

        CompilationFailure _ err -> do
            -- 'annotate' attaches the error message to the test failure log
            annotate (show err)
            -- 'failure' explicitly fails the property
            failure

    where
    readUtf8 path = liftIO (readSource path) >>= \case
        Right content -> pure content
        Left  err     -> annotate (T.unpack err) >> failure

fixture :: FilePath -> FilePath
fixture path = "./test/fixtures/" <> path


-- Generates a complete, schema-less 'JLD.Primitive Record from a list of attribute pairs.
-- Maps perfectly to: ={ "k1" = v1, "k2" = v2 }=
mkObj :: [(String, I.Expr 'JLD.Primitive)] -> I.Expr 'JLD.Primitive
mkObj pairs = I.Record I.EmptyMeta (mkSpine pairs)

-- Helper to compile a list of raw pairs into binary 'JLD.List backbone.
mkSpine :: [(String, I.Expr 'JLD.Primitive)] -> I.Expr 'JLD.List
mkSpine []            = I.Nil
mkSpine ((k, val):xs) = I.Cons (I.Attr (T.pack k) val) (mkSpine xs)

mkArr :: [I.Expr 'JLD.Primitive] -> I.Expr 'JLD.Primitive
mkArr elements = I.Array (mkArrSpine elements)

-- Helper to compile a list of expressions into a binary 'JLD.List backbone.
mkArrSpine :: [I.Expr 'JLD.Primitive] -> I.Expr 'JLD.List
mkArrSpine []     = I.Nil
mkArrSpine (x:xs) = I.Cons x (mkArrSpine xs)

mkDate :: String -> I.Expr JLD.Primitive
mkDate s = I.Date (fromJust $ parseISO8601 s)

nullPos :: SourcePos
nullPos = initialPos "test-suite"


-- Inline programs: compile the graph 'g' from source text
compileG :: Text -> CompilationResult
compileG = compile "inline-test" "g"

-- Both programs compile, to identical output
compilesLike :: Text -> Text -> Property
compilesLike src expected = withTests 1 . property $ compiledLike src expected

compiledLike :: Text -> Text -> PropertyT IO ()
compiledLike src expected =
    case (compileG src, compileG expected) of
        (CompilationSuccess _ actual, CompilationSuccess _ wanted) -> actual === wanted
        (actual, wanted) -> do
                            annotate ("program:  " <> describe actual)
                            annotate ("expected: " <> describe wanted)
                            failure

failsToCompile :: Text -> Property
failsToCompile src = withTests 1 . property $
    case compileG src of
        CompilationFailure _ _ -> success
        CompilationSuccess _ ast -> do
                                    annotate ("compiled to: " <> TL.unpack (toJSON defaultOptions ast))
                                    failure

describe :: CompilationResult -> String
describe = \case
    CompilationSuccess _ ast  -> "compiled to " <> TL.unpack (toJSON defaultOptions ast)
    CompilationFailure _ errs -> "failed with " <> show errs

tshow :: Show a => a -> Text
tshow = T.pack . show

-- The program fails to compile, and one of its errors mentions the given text
failsWith :: Text -> Text -> Property
failsWith message src = withTests 1 . property $
    case compileG src of
        CompilationFailure _ errs
            | any (T.isInfixOf message . T.pack . show) errs
                -> success
            | otherwise
                -> do
                   annotate ("failed, but no error mentions: " <> T.unpack message)
                   annotate (show errs)
                   failure
        CompilationSuccess _ ast
            -> do
               annotate ("compiled to: " <> TL.unpack (toJSON defaultOptions ast))
               failure

-- The program fails to compile, and none of its errors mention the given text
failsWithout :: Text -> Text -> Property
failsWithout message src = withTests 1 . property $
    case compileG src of
        CompilationFailure _ errs
            | any (T.isInfixOf message . T.pack . show) errs
                -> do
                   annotate ("an error mentions: " <> T.unpack message)
                   annotate (show errs)
                   failure
            | otherwise
                -> success
        CompilationSuccess _ ast
            -> do
               annotate ("compiled to: " <> TL.unpack (toJSON defaultOptions ast))
               failure

-- The program compiles to the given JSON (whitespace is ignored on both sides)
compilesToJSON :: Text -> Text -> Property
compilesToJSON src expected = withTests 1 . property $
    case compileG src of
        CompilationSuccess _ ast -> squash (TL.toStrict (toJSON defaultOptions ast)) === squash expected
        failed                   -> do
                                    annotate (describe failed)
                                    failure
    where
    squash = T.filter (not . isSpace)

-- The program compiles, and its output contains the given text exactly, whitespace included,
-- for checks that 'compilesToJSON' would squash away, such as the characters inside a string
emitsExactly :: Text -> Text -> Property
emitsExactly src expected = withTests 1 . property $
    case compileG src of
        CompilationSuccess _ ast
            | expected `T.isInfixOf` TL.toStrict (toJSON defaultOptions ast) -> success
        result
            -> do
               annotate ("expected the output to contain: " <> T.unpack expected)
               annotate (describe result)
               failure

-- The program compiles, and one of its warnings mentions the given text
warnsWith :: Text -> Text -> Property
warnsWith message src = withTests 1 . property $
    case compileG src of
        CompilationSuccess warnings _
            | any (T.isInfixOf message . warningText) warnings
                -> success
            | otherwise
                -> do
                   annotate ("no warning mentions: " <> T.unpack message)
                   annotate (show warnings)
                   failure
        failed
            -> do
               annotate (describe failed)
               failure

-- The program compiles without any warnings
compilesWithoutWarnings :: Text -> Property
compilesWithoutWarnings src = withTests 1 . property $
    case compileG src of
        CompilationSuccess [] _ -> success
        CompilationSuccess warnings _
            -> do
               annotate (show warnings)
               failure
        failed
            -> do
               annotate (describe failed)
               failure

warningText :: OuroWarning -> Text
warningText (OuroWarning _ context) = warningSummary context <> "\n" <> warningBlurb context


-- Programs that import modules held in memory: each (path, source) can be imported as
-- (import path), and is read as the file path.ouro
type Modules = [(Text, Text)]

compileWithModules :: Modules -> Text -> CompilationResult
compileWithModules modules = compileWith (Map.fromList modules) "inline-test" "g"

-- With modules: the program compiles to the given JSON (whitespace is ignored on both sides)
importsToJSON :: Modules -> Text -> Text -> Property
importsToJSON modules src expected = withTests 1 . property $
    case compileWithModules modules src of
        CompilationSuccess _ ast -> squash (TL.toStrict (toJSON defaultOptions ast)) === squash expected
        failed                   -> do
                                    annotate (describe failed)
                                    failure
    where
    squash = T.filter (not . isSpace)

-- With modules: the program fails, and one of its errors mentions every given text. An
-- error shows its line and column but not its file: importsFailIn checks the file.
importsFailWith :: [Text] -> Modules -> Text -> Property
importsFailWith messages modules src = withTests 1 . property $
    case compileWithModules modules src of
        CompilationFailure _ errs
            | any (\err -> all (`T.isInfixOf` T.pack (show err)) messages) errs
                -> success
            | otherwise
                -> do
                   annotate ("failed, but no one error mentions all of: " <> show messages)
                   annotate (show errs)
                   failure
        CompilationSuccess _ ast
            -> do
               annotate ("compiled to: " <> TL.unpack (toJSON defaultOptions ast))
               failure

-- With modules: the program fails, and each given text is mentioned by one of its errors
importsFailWithEach :: [Text] -> Modules -> Text -> Property
importsFailWithEach messages modules src = withTests 1 . property $
    case compileWithModules modules src of
        CompilationFailure _ errs
            | all (\message -> any (T.isInfixOf message . T.pack . show) errs) messages
                -> success
            | otherwise
                -> do
                   annotate ("failed, but not every one of these is mentioned: " <> show messages)
                   annotate (show errs)
                   failure
        CompilationSuccess _ ast
            -> do
               annotate ("compiled to: " <> TL.unpack (toJSON defaultOptions ast))
               failure

-- With modules: the program compiles, and the given text is in exactly this many warnings
importsWarnTimes :: Int -> Text -> Modules -> Text -> Property
importsWarnTimes times message modules src = withTests 1 . property $
    case compileWithModules modules src of
        CompilationSuccess warnings _
            -> do
               annotate (show warnings)
               length (filter (T.isInfixOf message . warningText) warnings) === times
        failed
            -> do
               annotate (describe failed)
               failure

-- With modules: the program fails, and one error reported in the given file mentions every
-- given text. A file is the entry program's name, inline-test, or a module's path.ouro.
importsFailIn :: FilePath -> [Text] -> Modules -> Text -> Property
importsFailIn file messages modules src = withTests 1 . property $
    case compileWithModules modules src of
        CompilationFailure _ errs
            | any (\err -> sourceName (errPos err) == file && all (`T.isInfixOf` T.pack (show err)) messages) errs
                -> success
            | otherwise
                -> do
                   annotate ("failed, but no error in " <> file <> " mentions all of: " <> show messages)
                   annotate (show [ (sourceName (errPos err), err) | err <- toList errs ])
                   failure
        CompilationSuccess _ ast
            -> do
               annotate ("compiled to: " <> TL.unpack (toJSON defaultOptions ast))
               failure

-- With modules: the program fails with an error of the given code, at the given file, line
-- and column, whose message and hint mention every given text. Every error a feature can
-- raise is pinned this way: what it is, where it is reported, and what it tells the reader.
importsFailAt :: Text -> (FilePath, Int, Int) -> [Text] -> Modules -> Text -> Property
importsFailAt code (file, line, column) messages modules src = withTests 1 . property $
    case compileWithModules modules src of
        CompilationFailure _ errs
            | any matches errs
                -> success
            | otherwise
                -> do
                   annotate ("no " <> T.unpack code <> " error at " <> file <> ":" <> show line <> ":" <> show column <> " mentions all of: " <> show messages)
                   annotate (unlines [ T.unpack (smartErrorCode (errContext err)) <> " at " <> placeOf err <> ": " <> show err | err <- toList errs ])
                   failure
        CompilationSuccess _ ast
            -> do
               annotate ("compiled to: " <> TL.unpack (toJSON defaultOptions ast))
               failure
    where
    matches err = smartErrorCode (errContext err) == code
               && placeOf err == file <> ":" <> show line <> ":" <> show column
               && all (`T.isInfixOf` T.pack (show err)) messages

    placeOf err = let pos = errPos err
                  in sourceName pos <> ":" <> show (unPos (sourceLine pos)) <> ":" <> show (unPos (sourceColumn pos))

-- With modules: the program fails with exactly this many errors, so that one failure does
-- not bring others after it that are only its echoes
importsFailTimes :: Int -> Modules -> Text -> Property
importsFailTimes times modules src = withTests 1 . property $
    case compileWithModules modules src of
        CompilationFailure _ errs
            -> do
               annotate (show errs)
               length errs === times
        CompilationSuccess _ ast
            -> do
               annotate ("compiled to: " <> TL.unpack (toJSON defaultOptions ast))
               failure

-- With modules: the program compiles, and one warning reported in the given file mentions
-- the given text
importsWarnIn :: FilePath -> Text -> Modules -> Text -> Property
importsWarnIn file message modules src = withTests 1 . property $
    case compileWithModules modules src of
        CompilationSuccess warnings _
            | any (\w@(OuroWarning pos _) -> sourceName pos == file && message `T.isInfixOf` warningText w) warnings
                -> success
            | otherwise
                -> do
                   annotate (show [ (sourceName pos, warningText w) | w@(OuroWarning pos _) <- warnings ])
                   failure
        failed
            -> do
               annotate (describe failed)
               failure
