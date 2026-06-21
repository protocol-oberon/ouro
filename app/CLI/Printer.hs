{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module CLI.Printer (pintDiagnostic) where

import           Data.Char                     (isAlphaNum, isLetter)
import           Data.Ouro                     (ErrorContext (..),
                                                InternalError (..),
                                                LinterWarning (..),
                                                OuroError (..),
                                                OuroWarning (..),
                                                PathError (..), ScopeError (..),
                                                SemanticWarning (..),
                                                SyntaxError (..),
                                                TypeError (..),
                                                VocabularyWarning (..),
                                                WarningContext (..),
                                                smartErrorCode,
                                                smartWarningCode, warningBlurb,
                                                warningSummary)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Prettyprinter                 (Doc, LayoutOptions (..),
                                                PageWidth (AvailablePerLine),
                                                Pretty (pretty), annotate,
                                                defaultLayoutOptions, fillSep,
                                                flatAlt, group, hardline, hsep,
                                                indent, layoutSmart, nest,
                                                reAnnotateS, sep, vsep)
import           Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold,
                                                color, renderIO, colorDull)
import           System.IO                     (stderr)
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


pintDiagnostic :: FilePath -> String -> Either OuroWarning OuroError -> IO ()
pintDiagnostic fp sourceContent diagnostic = do
    -- Source Coordinate and Frame Metric Extraction
    let (pos, headerStyle, codeText, title, details, mBlurb) = case diagnostic of
            Right (OuroError ePos context)
                -> let (errTitle, errDetails, errBlurb) = splitErrorContext context
                   in (ePos, Error, smartErrorCode context, errTitle, errDetails, errBlurb)

            Left (OuroWarning wPos context)
                -> let (warnTitle, warnDetails, warnBlurb) = splitWarningContext context
                   in (wPos, Warning, smartWarningCode context, warnTitle, warnDetails, warnBlurb)

    let lineNum     = unPos (sourceLine pos)
        colNum      = unPos (sourceColumn pos)
        lineStr     = annotate Gutter (pretty $ show lineNum)
        gutterWidth = length (show lineNum)

    -- Source Code Buffering
    -- Safely retrieve the raw source string matching the target line index.
    -- An empty string fallback is provided to prevent index out of bounds errors.
    let rawLine = case lineNum <= length (lines sourceContent) of
                      True  -> lines sourceContent !! (lineNum - 1)
                      False -> ""

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
              <> pretty fp <> ":" <> annotate Gutter (pretty lineNum) <> ":" <> annotate Gutter (pretty colNum) <> "│"
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
    -- line constraint before converting annotations to ANSI terminal stream codes.
    let layoutOptions = LayoutOptions (AvailablePerLine adjustedWidth 1.0)
    renderIO stderr (reAnnotateS styleToAnsi (layoutSmart layoutOptions doc))

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
        -> ( "Malformed Tag Payload"
           , [ labeled "Tag Context: " (pretty tag)
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

    Syntax (ShadowedVariableError _shadow)
        -> ("Shadowed Attribute"
           , [ labeled "This attribute key shadows an builtin function" ""]
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

    Syntax (InvalidTemplateName name)
        -> ( "Invalid Template Name"
           , [ labeled "Formating Rule: " "All template marco names must end with a '!'."
             , labeled (pretty $ "Perhaps add '!' to the end of " <> name <> ".") ""]
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
           , [ labeled "Expected :" "an independent expression or an outer-scope identifier"
             , labeled "Got:      " (pretty var)
             ]
           , Nothing
           )

    Scope (IncorrectArity name exp act)
        -> ("Incorrect Arity"
           , [ labeled (pretty $ "The function '" <> name <> "' expects " <> (T.pack $ show exp) <> "number of arguments.") ""
             , labeled (pretty $ "Got: " <> (T.pack $ show act) <> "number of argumnets instead.") ""
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


--- Pre-processor for Type Highlighting ---

-- Internal token types for the error string parser
data DiagnosticToken
    = PlainText Text
    | TypeKeyword Text  -- For capitalized types like "String", "Number"
    | TagKeyword  Text  -- For "#str", "#date", etc.


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
        Lint (DeprecatedSyntax feature)
            -> ( title
               , [ labeled "Legacy Structure: " (pretty feature) ]
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
