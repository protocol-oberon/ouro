{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module CLI.Printer
( pintDiagnostic
, renderDiagnostic
) where

import           Data.Char                     (isAlphaNum, isLetter)
import           Data.Maybe                    (fromMaybe)
import           Data.Ouro                     (BuildError (..),
                                                ErrorContext (..),
                                                InternalError (..),
                                                LinterWarning (..),
                                                OuroError (..),
                                                OuroWarning (..),
                                                PathError (..), ScopeError (..),
                                                SemanticWarning (..),
                                                SourceFrame (..),
                                                SyntaxError (..),
                                                TypeError (..),
                                                VocabularyWarning (..),
                                                WarningContext (..),
                                                smartErrorCode,
                                                smartWarningCode, sourceFrame,
                                                warningBlurb,
                                                warningSummary)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Prettyprinter                 (Doc, LayoutOptions (..),
                                                PageWidth (AvailablePerLine),
                                                Pretty (pretty), SimpleDocStream,
                                                annotate, defaultLayoutOptions,
                                                fillSep, flatAlt, group, hardline,
                                                hsep, indent, layoutSmart, nest,
                                                reAnnotateS, sep, unAnnotateS,
                                                vsep)
import           Prettyprinter.Render.Text     (renderStrict)
import           Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold,
                                                color, colorDull, renderIO)
import           System.Environment            (lookupEnv)
import           System.IO                     (hIsTerminalDevice, stderr)
import           Text.Megaparsec               (SourcePos)
import           Text.Megaparsec.Pos           (sourceColumn, sourceLine, unPos)



data OuroStyle
    = Error
    | Gutter
    | Code
    | Pointer
    | Type
    | Warning
    | NoteBody

styleToAnsi :: OuroStyle -> AnsiStyle
styleToAnsi = \case
               Code     -> mempty
               Error    -> color Red     <> bold
               Pointer  -> color Red     <> bold
               Type     -> color Magenta <> bold
               Gutter   -> color Cyan
               Warning  -> color Yellow  <> bold
               NoteBody -> mempty


-- Prints a diagnostic to stderr: in colour when stderr is a terminal and NO_COLOR is not set
-- (https://no-color.org), and as plain text otherwise, so that a log or a pipe holds no
-- escape codes (gap 37)
pintDiagnostic :: (FilePath -> Maybe Text) -> Either OuroWarning OuroError -> IO ()
pintDiagnostic sourceOf diagnostic = do
    terminal <- hIsTerminalDevice stderr
    noColour <- maybe False (not . null) <$> lookupEnv "NO_COLOR"
    let layout = diagnosticLayout sourceOf diagnostic
    case terminal && not noColour of
        True  -> renderIO stderr (reAnnotateS styleToAnsi layout)
        False -> renderIO stderr (unAnnotateS layout)


-- A diagnostic as it is printed, without its colours: what the messages' golden files hold
renderDiagnostic :: (FilePath -> Maybe Text) -> Either OuroWarning OuroError -> Text
renderDiagnostic sourceOf = renderStrict . unAnnotateS . diagnosticLayout sourceOf


-- A diagnostic laid out in the file it was reported in, which need not be the file compiled:
-- an error inside an imported module is shown with that module's name and line. The sources
-- of every file a diagnostic can be in are given by name.
diagnosticLayout :: (FilePath -> Maybe Text) -> Either OuroWarning OuroError -> SimpleDocStream OuroStyle
diagnosticLayout sourceOf diagnostic = do
    -- Source Coordinate and Frame Metric Extraction
    let (pos, headerStyle, codeText, title, details, mBlurb) = case diagnostic of
            Right (OuroError ePos context)
                -> let (errTitle, errDetails, errBlurb) = splitErrorContext context
                   in (ePos, Error, smartErrorCode context, errTitle, errDetails, errBlurb)

            Left (OuroWarning wPos context)
                -> let (warnTitle, warnDetails, warnBlurb) = splitWarningContext context
                   in (wPos, Warning, smartWarningCode context, warnTitle, warnDetails, warnBlurb)

    let frame       = sourceFrame sourceOf pos
        lineNum     = frameLine frame
        colNum      = frameColumn frame
        lineStr     = annotate Gutter (pretty $ show lineNum)
        gutterWidth = length (show lineNum)

    -- The text of the line the diagnostic is on, in its own file
    let rawLine = T.unpack (frameText frame)

    let maxWidth = 100

    -- Source Viewport Window Slicing
    -- Evaluates whether the source segment length exceeds the absolute layout boundary limit.
    -- If configuration constraints are exceeded, a bounded slice is extracted centering the
    -- offending target column within the layout viewport window.
    -- The caret pointer index (actualVisualCol) is adjusted to compensate for prefix decorations.
    let (slicedLine, visualColNum) =
          case length rawLine > maxWidth of
            False -> (rawLine, colNum)
            True  -> let startIdx = max 0 (colNum - (maxWidth `div` 2))
                         endIdx   = min (length rawLine) (startIdx + maxWidth)
                         prefix   = case startIdx > 0 of
                                        True  -> "... "
                                        False -> ""
                         suffix   = case endIdx < length rawLine of
                                        True  -> " ..."
                                        False -> ""
                         window   = take (endIdx - startIdx) (drop startIdx rawLine)
                         actualVisualCol = (colNum - startIdx) + length prefix
                     in (prefix ++ window ++ suffix, actualVisualCol)

    -- Gutter and Structural Border Configurations
    -- Defines the core horizontal spacing primitives. Pre-calculates fixed structural lines
    -- to enforce linear invariance down the terminal margin, preventing raw layout leakage.
    let mkGutter l    = l <> " │ "
        mkEmptyGutter = pretty (replicate gutterWidth ' ') <> " │ "

    -- Subsystem Error Detail Layout Formatting
    -- Prepends an invariant empty gutter tracking margin directly onto the left rail edge,
    -- then inserts a hardcoded column indentation sequence to match the caret pointer index.
    let alignedDetails = map (\d -> mkEmptyGutter <> pretty (replicate (visualColNum - 1) ' ') <> d) details

    -- Educational Diagnostic Blurb Processing
    -- Routes the trailing paragraph metadata through the line tokenizer pipeline.
    let blurbDoc = case mBlurb of
                       Nothing        -> []
                       Just blurbText -> [ "", highlightDiagnostic blurbText, "" ]

    -- Dynamic Layout Width Compensation
    -- Expands the total target calculation constraint dynamically by adding the active caret offset index.
    -- This guarantees that text forced past the caret positioning will retain its intended wrapping width canvas.
    let adjustedWidth = maxWidth + visualColNum

    -- Layout Document Generation and Output Streaming
    -- Assembles the component diagnostic vectors sequentially into a single unified stream.
    -- Caret tracking preserves raw source alignment offsets while the textual description
    -- components maintain strict alignment layout bounds relative to the vertical fence.
    let doc :: Doc OuroStyle
        doc = vsep $
            [ annotate headerStyle (pretty (renderLabel headerStyle) <> " [" <> pretty codeText <> "]: " <> pretty title)
            , pretty (replicate gutterWidth ' ') <> " ┌── │"
              <> pretty (frameFile frame) <> ":" <> annotate Gutter (pretty lineNum) <> ":" <> annotate Gutter (pretty colNum) <> "│"
            , pretty (replicate gutterWidth ' ') <> " │"
            , mkGutter lineStr <> pretty slicedLine

            -- Caret alignment tracking
            , mkEmptyGutter <> indent (visualColNum - 1) (annotate Pointer "^")
            ]
            ++ alignedDetails
            ++ (case blurbDoc of
                    [] -> [mkEmptyGutter, mempty]
                    _  -> [mkEmptyGutter] ++ blurbDoc)

    -- Rendering Pipe
    -- Instructs the layout engine to execute text-wrapping computations under an adjusted
    -- line constraint; pintDiagnostic adds the colours, renderDiagnostic leaves them out.
    let layoutOptions = LayoutOptions (AvailablePerLine adjustedWidth 1.0)
    layoutSmart layoutOptions doc

    where
    renderLabel :: OuroStyle -> Text
    renderLabel Error   = "Error"
    renderLabel Warning = "Warning"
    renderLabel _       = "Diagnostic"

-- Helper Interface matching the layout structure

-- Separates the error label from its structural inner field strings
-- so the error layout engine can position them independently.
splitErrorContext :: ErrorContext -> (Text, [Doc OuroStyle], Maybe Text)
splitErrorContext = \case
    -- If we hit the middleware, extract the text and recurse to get the base error fields
    AnnotatedContext blurb ctx
        -> let (errTitle, details, _) = splitErrorContext ctx
           in (errTitle, details, Just blurb)

    Typing (TypeMismatch expected actual)
        -> ( "Type Mismatch"
           , [ labeled "Expected: " (highlightDiagnostic expected)
             , labeled "Got:      " (highlightDiagnostic actual)
             ]
           , Nothing
           )

    Typing (PropertyMismatch expected actual)
        -> ( "Property Mismatch"
           , [ highlightDiagnostic ("Expected data to have the property " <> expected <> ", got " <> actual <> " instead.")
             ]
           , Nothing
           )

    Syntax (LexicalError lexeme)
        -> ( "Lexical Error"
           , [ highlightDiagnostic lexeme
             ]
           , Nothing
           )
    Syntax (UnbalancedDelimiter expected actual)
        -> ( "Unbalanced Delimiter"
           , [ labeled "Expected: " (pretty expected)
           , labeled "Got:      " (pretty actual)
             ]
           , Nothing
           )

    Syntax (MalformedTagPayload tag shape)
        -> ( "Malformed Payload"
           , [ labeled "Context:     " (pretty tag)
             , labeled "Got Shape:   " (pretty shape)
             ]
           , Nothing
           )

    Syntax (TargetMismatchAttr expected actual)
        -> ( "Target Mismatch"
           , [ labeled "Expected: " (pretty expected)
             , labeled "Got:      " (pretty actual)
             ]
           , Nothing
           )

    Syntax (ShadowedVariableError shadow)
        -> ("Reserved Name"
           , [ labeled "Reserved: " (pretty shadow) ]
           , Nothing
           )

    Syntax (InexhaustiveCase target branches)
        -> ( "Inexhaustive Case Match"
           , [ labeled "Unmatched Target: " (pretty target)
             , labeled "Branches Checked: " (pretty branches)
             ]
           , Nothing
           )

    Syntax MalformedCaseBranch
        -> ( "Malformed Case Branch"
           , [ labeled "Invalid Branch, expected Form:  " "(<pattern> <body>)"
             ]
           , Nothing
           )

    Syntax MissingOtherwiseFallback
        -> ( "Missing Case Fallback"
           , [ labeled "Structural Rule: " "All case statements must end with an 'otherwise' branch." ]
           , Nothing
           )

    Syntax (InvalidFunctionName name)
        -> ( "Invalid Function Name"
           , [ labeled "Naming Rule: " (pretty $ "'" <> name <> "' is read by its syntax, so a function cannot take its name.") ]
           , Nothing
           )

    Syntax MalformedPattern
        -> ( "Malformed Pattern"
           , [ labeled "Pattern Rule: " "Patterns are written as values are: (pattern ...) for an Array, (:key pattern ...) for a Record, and ? or ?name for a hole."
             ]
           , Nothing
           )

    Syntax (MalformedContext problem)
        -> ( "Malformed Context"
           , [ labeled "Context Rule: " (pretty problem) ]
           , Nothing
           )

    Syntax (MissingAttributeValue key next)
        -> ( "Missing Attribute Value"
           , [ labeled "Key:         " (pretty (":" <> key))
             , labeled "Followed by: " (pretty next)
             ]
           , Nothing
           )

    Path (MissingPathKey key keys)
        -> ( "Missing Path Key"
           , [ labeled "Missing:   " (pretty key)
             , labeled "Available: " (pretty (show (map T.unpack keys)))
             ]
           , Nothing
           )

    Scope (ScopeError var)
        -> ( "Unbound Identifier"
           , [ labeled "Expected: " "a defined symbol"
             , labeled "Got:      " (pretty var)
             ]
           , Nothing
           )

    Scope (CyclicDependencyError var)
        -> ("Circular Reference"
           , [ labeled "Expected: " "an independent expression or an outer-scope identifier"
             , labeled "Got:      " (pretty var)
             ]
           , Nothing
           )

    Scope (IncorrectArity name expr act)
        -> ("Incorrect Arity"
           , [ labeled "Expected: " (pretty $ "'" <> name <> "' takes " <> arguments expr)
             , labeled "Got:      " (pretty $ arguments act)
             ]
           , Nothing
           )

    Scope (IndexOutOfBoundsError cb tOft att act)
        -> ("Index Out of Bounds"
           , [ labeled (pretty $ "The function '" <> cb <> "' attempted to look up an element with an index: ") (pretty $ T.pack $ show att)
             , labeled (pretty $ "But it was out of bounds as the target " <> tOft <> " has a length of: ")     (pretty $ T.pack $ show act)
             ]
           , Nothing
           )

    Scope (NonExistentGraph trgt sug)
        -> ("Non Existent Graph"
           , [ labeled (pretty $ "Compilation target graph: '" <> trgt <> "' cannot be found") ""
             , labeled (pretty $ "Perhaps you meant to target the graph: '" <> sug <> "'") ""
             ]
           , Nothing
           )

    Scope (NoCompilationTarget filepath)
        -> ("No Compilation Target"
           , [ labeled (pretty $ "The module: " <> filepath <> " lacks a compilation target (Graph)") ""]
           , Nothing
           )

    Scope (ModuleNotFound path)
        -> ("Module Not Found"
           , [ labeled "Module: " (pretty path) ]
           , Nothing
           )

    Scope (UnknownQualifier qualifier)
        -> ("Unknown Qualifier"
           , [ labeled "Qualifier: " (pretty qualifier) ]
           , Nothing
           )

    Scope (NotExported qualifier name _)
        -> ("Not Exported"
           , [ labeled "Function: " (pretty $ qualifier <> "/" <> name) ]
           , Nothing
           )

    Scope (ImportCycle path)
        -> ("Import Cycle"
           , [ labeled "Cycle: " (pretty $ T.intercalate " → " path) ]
           , Nothing
           )

    Scope (ModuleUnreadable name file _)
        -> ("Module Unreadable"
           , [ labeled "Module: " (pretty name)
             , labeled "File:   " (pretty file)
             ]
           , Nothing
           )

    Scope (AmbiguousModule name files)
        -> ("Ambiguous Module"
           , [ labeled "Module: " (pretty name)
             , labeled "Files:  " (pretty (T.intercalate ", " files))
             ]
           , Nothing
           )

    Build (UnknownBuildSetting setting)
        -> ("Unknown Build Setting"
           , [ labeled "Setting: " (pretty (":" <> setting)) ]
           , Nothing
           )

    Build (MissingBuildSetting setting)
        -> ("Missing Build Setting"
           , [ labeled "Setting: " (pretty (":" <> setting)) ]
           , Nothing
           )

    Build (UnsupportedBuildSetting setting)
        -> ("Unsupported Build Setting"
           , [ labeled "Setting: " (pretty (":" <> setting)) ]
           , Nothing
           )

    Build (NotBuildData declaration)
        -> ("Not Build Data"
           , [ labeled "Found: " (pretty declaration) ]
           , Nothing
           )

    Build (NoBuildGraph found)
        -> ("No Build Graph"
           , [ labeled "Graph: " (pretty (fromMaybe "none" found)) ]
           , Nothing
           )

    Build (MissingSourceFolder folder)
        -> ("Missing Source Folder"
           , [ labeled "Folder: " (pretty folder) ]
           , Nothing
           )

    Build (NotPackageData declaration)
        -> ("Not Package Data"
           , [ labeled "Found: " (pretty declaration) ]
           , Nothing
           )

    Build (NoPackageFunction found)
        -> ("No Package Function"
           , [ labeled "Function: " (pretty (fromMaybe "none" found)) ]
           , Nothing
           )

    Build (PackageParameters count)
        -> ("Package Parameters"
           , [ labeled "Parameters: " (pretty count) ]
           , Nothing
           )

    Build (ReservedModule name)
        -> ("Reserved Module"
           , [ labeled "Module: " (pretty name) ]
           , Nothing
           )

    Build (ReservedPackageName name)
        -> ("Reserved Package Name"
           , [ labeled "Name: " (pretty name) ]
           , Nothing
           )

    Build (ReservedCollection name)
        -> ("Reserved Collection"
           , [ labeled "Collection: " (pretty name) ]
           , Nothing
           )

    Build (NestedVendor name)
        -> ("Nested Vendor"
           , [ labeled "Collection: " (pretty name) ]
           , Nothing
           )

    Build (CollectionOverlap name folder)
        -> ("Collection Overlap"
           , [ labeled "Name: " (pretty name)
             , labeled "Also in: " (pretty folder)
             ]
           , Nothing
           )

    Build (ImportCase written spelt)
        -> ("Import Case"
           , [ labeled "Written: " (pretty written)
             , labeled "File:    " (pretty spelt)
             ]
           , Nothing
           )

    Build (NewerOuro needed have)
        -> ("Newer Ouro"
           , [ labeled "Needed: " (pretty needed)
             , labeled "This:   " (pretty have)
             ]
           , Nothing
           )

    Build (InvalidVersion written)
        -> ("Invalid Version"
           , [ labeled "Written: " (pretty written) ]
           , Nothing
           )

    Build (UnknownModule selection)
        -> ("Unknown Module"
           , [ labeled "Graph: " (pretty selection) ]
           , Nothing
           )

    Build (GraphSelectionForm written)
        -> ("Graph Selection Form"
           , [ labeled "Written: " (pretty written) ]
           , Nothing
           )

    Build (GraphFileName name problem)
        -> ("Graph File Name"
           , [ labeled "Graph:   " (pretty name)
             , labeled "Problem: " (pretty problem)
             ]
           , Nothing
           )

    Build (GraphNameCase name other)
        -> ("Graph Name Case"
           , [ labeled "Graph:     " (pretty name)
             , labeled "Same file: " (pretty other)
             ]
           , Nothing
           )

    Internal (ErasureValueLeakError block)
        -> ( "Compiler Internal Error"
           , [ labeled "Expected: " "nil (erased)"
             , labeled "Got:      " (pretty block)
             ]
           , Nothing
           )

    Internal (ASTCorruptionError active found)
            -> ( "Fatal Compiler Error"
            , [ labeled "Active Symbol: " (pretty active)
              , labeled "Reason:        " (pretty found)
              ]
            , Nothing
            )



-- Standardized labels without compounding layout padding
labeled :: Doc OuroStyle -> Doc OuroStyle -> Doc OuroStyle
labeled label value = label <> value

-- A number of arguments, in words: "no arguments", "1 argument", "2 arguments"
arguments :: Int -> T.Text
arguments = \case
    0 -> "no arguments"
    1 -> "1 argument"
    n -> T.pack (show n) <> " arguments"


--- Pre-processor for Type Highlighting ---

-- Internal token types for the error string parser
data DiagnosticToken
    = PlainText Text
    | TypeKeyword Text  -- For capitalized types like "String", "Number"
    | TagKeyword  Text  -- For "#string", "#date", etc.


-- Strips characters chunk-by-chunk using simple guards and recursion.
-- Tokenizes by words to prevent fillSep spacing blowups around quotes.
tokenizeErrorString :: Text -> [DiagnosticToken]
tokenizeErrorString t = map classify (T.words t)
    where
    classify w
        -- 1. Catch quotes embedded in a word
        | "'" `T.isInfixOf` w
        = case T.breakOn "'" w of
              (before, rest) ->
                  let remainder = T.drop 1 rest
                  in case T.breakOn "'" remainder of
                         (_, "") | not (T.null before)         && not (T.null remainder)
                                   && isLetter (T.last before) && isLetter (T.head remainder)
                                   -> PlainText w

                             -- Otherwise, it's a syntax quote like "(')" or "'target". Color it!
                             | otherwise -> TagKeyword w

                         -- Two quotes found, safely reconstruct and tag it.
                         (body, after) -> TagKeyword (before <> "'" <> body <> "'" <> T.drop 1 after)

        | "#" `T.isPrefixOf` w = TagKeyword w
        | otherwise            = PlainText w

-- Post-processes a flat text string and applies dynamic type color highlighting
highlightDiagnostic :: Text -> Doc OuroStyle
highlightDiagnostic txt =
    -- Split by lines first to preserve explicit newlines, then stitch vertically
    vsep (map highlightLine (T.lines txt))

    where
    highlightLine :: Text -> Doc OuroStyle
    highlightLine lineStr = case T.null (T.strip lineStr) of
                                -- If the line is purely empty space or an extracted newline gap, preserve it
                                True  -> ""
                                -- Otherwise, tokenize the single line horizontally using fillSep
                                -- to allow text-wrapping within the 80-char boundaries
                                False -> fillSep (map renderToken (tokenizeErrorString lineStr))

    renderToken :: DiagnosticToken -> Doc OuroStyle
    renderToken token = case token of
                            PlainText t   -> annotate NoteBody (pretty t)
                            TypeKeyword t -> annotate Type (pretty t)
                            TagKeyword t  -> annotate Type (pretty t)


-- Separates the warning label from its structural inner field strings
splitWarningContext :: WarningContext -> (Text, [Doc OuroStyle], Maybe Text)
splitWarningContext ctx =
    let title = warningSummary ctx
        blurb = warningBlurb ctx
    in case ctx of
        Lint (UnanchoredLiteral _ val)
            -> ( title
               , [ labeled "Discarded: " (highlightDiagnostic val) ]
               , Just blurb
               )
        Lint (DeprecatedSyntax _ newName)
            -> ( title
               , [ labeled "Instead: " (pretty newName) ]
               , Just blurb
               )
        Lint (ParenthesisedResult _ val)
            -> ( title
               , [ labeled "Array of: " (highlightDiagnostic val) ]
               , Just blurb
               )
        Lint (ShadowedBuiltin _ name)
            -> ( title
               , [ labeled "Replaced Built-in: " (highlightDiagnostic name) ]
               , Just blurb
               )
        Lint (NotKebabCase _ _ kebab)
            -> ( title
               , [ labeled "Kebab-case: " (highlightDiagnostic kebab) ]
               , Just blurb
               )
        Lint (ShadowedBundledModule _ file)
            -> ( title
               , [ labeled "Replaced by: " (highlightDiagnostic file) ]
               , Just blurb
               )
        Lint (ProcedureOfNoParameters name)
            -> ( title
               , [ labeled "Constant: " (highlightDiagnostic name) ]
               , Just blurb
               )
        Semantic (ElidedPropertyAssign key construct)
            -> ( title
               , [ labeled "Omitted Attribute: " (highlightDiagnostic key)
                 , labeled "Erasure Node: "      (pretty construct)
                 ]
               , Just blurb
               )
        Vocab (UnboundTypeReference typeId _)
            -> ( title
               , [ labeled "Unmapped Entity: " (highlightDiagnostic typeId) ]
               , Just blurb
               )
