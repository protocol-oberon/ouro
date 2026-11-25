{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Ouro.Error.Types where

import           Data.Ouro.Refinement.Requirement (Failure)
import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)
import           Text.Megaparsec                  (SourcePos (sourceColumn, sourceLine), unPos)


-- The single data type that represents any compiler diagnostic emission
data OuroDiagnostic
    = DiagnosticError   OuroError
    | DiagnosticWarning OuroWarning
    deriving (Show, Eq, Ord, Generic)


data OuroError = OuroError
    { errPos     :: SourcePos
    , errContext :: ErrorContext
    } deriving (Eq, Ord, Generic)


instance Show OuroError where
    show (OuroError pos context) =
        "OuroError at line " ++ show (unPos (sourceLine pos))
        ++ ", col " ++ show (unPos (sourceColumn pos))
        ++ ": " ++ show context


-- ErrorContext.
--
-- A Sum type for logical failures.
--
-- A formalized domain of structural, syntactic, and semantic violations
-- detectable across the Ouro compilation and evaluation pipelines.
data ErrorContext
    = Syntax   SyntaxError
    | Typing   TypeError
    | Path     PathError
    | Scope    ScopeError
    | Internal InternalError
    | Build    BuildError
    | AnnotatedContext Text ErrorContext -- The Structural Decorator Middleware
    deriving (Show, Eq, Ord, Generic)


--- SYNTAX VIOLATIONS ---
data SyntaxError
    -- SYNTAX VIOLATION: INVALID LEXICAL CHARACTER
    -- Occurs during the lexical scanning phase when the compiler encounters
    -- an unrecognized character sequence or an orphaned modifier symbol that
    -- cannot be converted into a valid atomic token stream.
    --
    -- * Remediation: Abort layout processing. Highlight the offending source code
    --   offset and display valid lexical characters within that sub-domain.
    = LexicalError
      { rawLexeme :: Text   -- The invalid string slice or character sequence that broke the scanner
      }

    -- SYNTAX VIOLATION: UNBALANCED BOUNDARY DELIMITER
    -- Occurs during structural S-Expression block framing when a block boundary
    -- layout fails to close properly (e.g., mismatched parentheses or brackets),
    -- or when a trailing bracket wall sequence cuts off an open scope prematurely.
    --
    -- * Remediation: Halt parsing. The presentation layer should indicate the
    --   unclosed starting delimiter and pinpoint where the layout stream abruptly broke.
    | UnbalancedDelimiter
      { expectedDelim :: Text -- The closing token expected by the tree structure (e.g., ")", "}")
      , actualDelim   :: Text -- The token actually found, or a marker indicating EOF (End of File)
      }

    -- SYNTAX VIOLATION: MALFORMED UNIT ARGUMENT
    -- Occurs when a duration unit, such as (days n), is given other than the one number it
    -- takes.
    --
    -- * Remediation: Name the unit, and say that it takes exactly one number.
    | MalformedUnitArgument
      { activeUnit     :: Text -- The unit that was given the wrong arguments, such as days
      , foundNodeShape :: Text -- A descriptive text string detailing the structural shape of the invalid AST node
      }

    -- SYNTAX VIOLATION: SYNTACTIC ROLE MISMATCH
    -- Occurs when the evaluation engine detects an explicit attribute literal bind indicator
    -- (prefixed with a colon, e.g., @:key@) in a structural code position reserved strictly
    -- for lookup symbol evaluation handles (e.g., inside a @(get ...)@ path array).
    --
    -- * Remediation: The presentation layer must instruct the user to remove the leading
    --   colon operator to transition the token from an expression binder to an active identifier.
    | TargetMismatchAttr
      { expected   :: Text  -- The token category expected by the evaluator (typically "Symbol")
      , actualAttr :: Text  -- The raw text identifier of the invalid attribute binder encountered
      }

    -- RETIRED, never raised: a key or a let binding named after a special form, which was
    -- reserved. A binding now shadows a special form, as in Scheme (gap 43).
    | ShadowedVariableError
      { shadows :: Text
      }

    -- SYNTAX VIOLATION: INEXHAUSTIVE CASE PATTERN MATCH
    -- Occurs when a `case` branching expression is evaluated but none of the provided
    -- structural patterns or boolean guards successfully match the target value, and
    -- the block is missing the mandatory `otherwise` fallback clause.
    --
    -- * Remediation: The presentation layer should display the value that fell through
    --   and instruct the user to append an explicit `(otherwise <fallback-expr>)`
    --   branch to the end of the case block.
    | InexhaustiveCase
      { unmatchedTarget :: Text -- A human-readable string representation of the target value
      , parsedBranches  :: Int  -- The number of branches that were sequentially attempted
      }

    -- SYNTAX VIOLATION: MALFORMED CASE BRANCH
    -- Occurs during structural dispatch when a branch within a `case` block
    -- fails to adhere to the strict `(<pattern> <body>)` bipartite tuple format.
    -- This typically happens if a user provides a single expression without a
    -- return body, or passes three or more expressions within the branch boundary.
    --
    -- * Remediation: The compiler or editing layer should highlight the offending
    --   branch layout and instruct the user to ensure the branch contains exactly
    --   two elements: the evaluation pattern (or guard) and its corresponding return
    --   expression.
    | MalformedCaseBranch

    -- SYNTAX VIOLATION: MISSING OTHERWISE FALLBACK
    -- Occurs during static analysis when a `case` branching expression lacks
    -- an explicit `otherwise` branch at the very end of its branch list.
    | MissingOtherwiseFallback


    | InvalidFunctionName
      { invalidName :: Text }

    -- SYNTAX VIOLATION: MALFORMED PATTERN
    -- Occurs when a pattern, in a case branch or a function's parameter list, is not
    -- written as Ouro's patterns are: a hole inside a function call, a '?..rest' hole
    -- that is not last, or a Record pattern holding something other than ':key pattern'.
    | MalformedPattern

    -- SYNTAX VIOLATION: MALFORMED CONTEXT
    -- Occurs when a record's :context is not a JSON-LD context: a remote context that is not
    -- an absolute URL, an unknown '@' keyword, or a term mapped to something other than an
    -- IRI, null or an expanded definition.
    | MalformedContext
      { contextProblem :: Text }

    -- A key with no value: followed by another key, or by the end of its form
    | MissingAttributeValue
      { valuelessKey :: Text
      , followedBy   :: Text
      }
    deriving (Show, Eq, Ord, Generic)


--- TYPE VIOLATIONS ---
data TypeError
    -- TYPE VIOLATION: VALUE DOMAIN MISMATCH
    -- Occurs when a value is checked against a refinement (e.g., uri, date, string)
    -- or during backend GADT conversion when an expression resolves to a primitive literal
    -- or structure that contradicts the declared domain constraint.
    --
    -- * Remediation: Reject the evaluation frame. Output must state the explicit type constraints
    --   and provide structural layout expectations (e.g., ISO-8601 formatting for date types).
    = TypeMismatch
      { expectedType :: Text  -- The semantic type or specification structure enforced by the constraint
      , actualType   :: Text  -- The runtime type evaluation primitive actually generated by the AST node
      }

    -- TYPE VIOLATION: A BUILT-IN'S OPERAND WITHOUT THE REFINEMENT IT NEEDS
    -- Occurs when a built-in checks its own operand, a condition that must be a boolean, a date
    -- that must parse, with no signature behind it. Until the built-ins are declared with
    -- requirements, when each becomes a RefinementFailed.
    --
    -- * Remediation: Name both refinements as they are written in Ouro, and say why when the
    --   value's own refinement is not reason enough (a String that is not a date).
    | RefinementMismatch
      { expectedRefinement :: Text  -- The refinement the operand needs, as written: "boolean"
      , actualRefinement   :: Text  -- The refinement the operand has, with a reason where needed
      }

    -- TYPE VIOLATION: A REQUIREMENT FAILED
    -- Occurs when a value is outside a requirement: a signature's, (<n ⊢ number>), or an
    -- assertion's, <x ⊢ string>.
    --
    -- * Remediation: The failure is data: what was required and where, the value given and its
    --   kind, the role, and the trail down to the part that failed; blame is read off the role.
    --   The two texts are the failure rendered, kept until the printer and the tests read the
    --   failure alone.
    | RefinementFailed
      { expectedRefinement :: Text     -- The type required, rendered: "(array number)"
      , actualRefinement   :: Text     -- The value given, sketched
      , failure            :: Failure  -- The failure as data
      }
    deriving (Show, Eq, Ord, Generic)


--- PATH VIOLATIONS ---
data PathError
    -- PATH VIOLATION: STRUCTURAL FIELD MISSING
    -- Occurs during multi-layered property traversal via the @get@ operator: the value reached
    -- so far is a Record, but it has no attribute for the current path segment key.
    --
    -- * Remediation: Abort traversal. The reporting layer should compute the string distance
    --   (Levenshtein metric) between the missing key and available attributes to suggest adjacent typos.
    = MissingPathKey
      { missingKey    :: Text    -- The string token handle that failed to map to a valid structural slot
      , availableKeys :: [Text]  -- The list of legitimate sibling attribute keys present in the target block
      }
    deriving (Show, Eq, Ord, Generic)


--- SCOPE VIOLATIONS ---
data ScopeError
    -- SCOPE VIOLATION: UNBOUND IDENTIFIER REFERENCE
    -- Occurs when a raw variable symbol handle fails to resolve against the active nested
    -- environment frames. The engine stepped completely up through the current 'localScope'
    -- and all 'parentEnv' thunk links without establishing a memory reference allocation.
    --
    -- * Remediation: Fail compilation instantly. Signal an unmapped reference pointer error
    --   at the precise source position coordinates.
    = ScopeError
      { variableName :: Text  -- The unbound identifier string that triggered the frame search failure
      }

    | CyclicDependencyError
      { variableName :: Text -- The indentifier causing a infinate loop
      }

    | IncorrectArity
      { nameOfFunc   :: Text
      , expectNoArgs :: Int
      , actualNoArgs :: Int
      }

    | IndexOutOfBoundsError
      { calledBy       :: Text
      , typeOfTarget   :: Text
      , attemptedIndex :: Int
      , actualLen      :: Int
      }

    | NonExistentGraph
      { trgtGraph      :: Text
      , suggestedGraph :: Text
      }

    | NoCompilationTarget Text

    -- An (import path) names a module that cannot be found
    | ModuleNotFound
      { missingModule :: Text
      }

    -- A qualified name, qualifier/name, whose qualifier names no import of the module
    | UnknownQualifier
      { qualifierName :: Text
      }

    -- A qualified name the imported module does not export
    | NotExported
      { fromQualifier  :: Text
      , unexportedName :: Text
      , exportedNames  :: [Text]
      }

    -- Modules that import each other in a circle, as the path of imports that closes it
    | ImportCycle
      { cyclePath :: [Text]
      }

    -- A module's file exists but cannot be read, such as one that is not UTF-8
    | ModuleUnreadable
      { unreadableModule :: Text
      , unreadableFile   :: Text
      , unreadableReason :: Text
      }

    -- A module found in more than one source root, so its name does not name one file
    | AmbiguousModule
      { ambiguousPath  :: Text
      , ambiguousFiles :: [Text]
      }
    deriving (Show, Eq, Ord, Generic)


--- LIFECYCLE / INTERNAL VIOLATIONS ---
data InternalError
    -- LIFECYCLE VIOLATION: ERASURE SYMBOL VALUE LEAK
    -- Occurs when a macro directive or configuration declaration block earmarked for
    -- compile-time erasure (such as a structural @(define ...)@ wrapper form) escapes
    -- environment frame population and leaks down to the terminal output generation layer.
    --
    -- * Remediation: Halt the emitter pipeline. This indicates a compiler orchestration bug
    --   where Pass 3 ('evalExpr') encountered an un-evaluated tracking block that should have
    --   been swept and cleared during Pass 2 ('emitProps').
    = ErasureValueLeakError
      { blockName :: Text  -- The name of the compile-time directive block that leaked to runtime
      }

    | ASTCorruptionError
      { activeSymbol  :: Text
      , foundASTShape :: Text
      }
    deriving (Show, Eq, Ord, Generic)


--- BUILD VIOLATIONS ---
-- What is wrong with a project's build: its package.ouro, the settings of a build, and the
-- modules under its :sources. A setting of the wrong type is a TypeMismatch. A retired error
-- keeps its constructor, never raised, so its code is never given to another error.
data BuildError
    -- A setting package.ouro's schema does not have, such as :sorces
    = UnknownBuildSetting
      { unknownSetting :: Text
      }

    -- RETIRED, never raised: a setting every build needed that build.ouro did not give, its
    -- :name. No setting of package.ouro is required.
    | MissingBuildSetting
      { missingSetting :: Text
      }

    -- A setting of the schema that this version of Ouro does not act on yet. None is now: kept
    -- for a setting added before it is acted on.
    | UnsupportedBuildSetting
      { unsupportedSetting :: Text
      }

    -- RETIRED, never raised: a declaration build.ouro could not hold, an import, an export, a
    -- function or a second graph. package.ouro's are NotPackageData and ReservedPackageName.
    | NotBuildData
      { buildDeclaration :: Text
      }

    -- RETIRED, never raised: build.ouro's graph was not named build, or it had no graph.
    -- package.ouro's is NoPackageFunction.
    | NoBuildGraph
      { foundGraph :: Maybe Text
      }

    -- The folder of :sources does not exist
    | MissingSourceFolder
      { sourceFolder :: Text
      }

    -- A declaration package.ouro cannot hold: a graph, an export or a function other than package
    | NotPackageData
      { packageDeclaration :: Text
      }

    -- package.ouro has no function named package: the name of the function it has instead, if any
    | NoPackageFunction
      { foundFunction :: Maybe Text
      }

    -- The package function takes parameters
    | PackageParameters
      { parameterCount :: Int
      }

    -- A module under :sources whose path starts with a bundled folder, std or aat
    | ReservedModule
      { reservedPath :: Text
      }

    -- A function of package.ouro named inputs, build or exports, names kept for later versions
    | ReservedPackageName
      { reservedName :: Text
      }

    -- A folder of vendor/ named after a bundled folder, std or aat
    | ReservedCollection
      { reservedFolder :: Text
      }

    -- A vendored library with a vendor/ folder of its own
    | NestedVendor
      { nestedCollection :: Text
      }

    -- A collection's name that is also a top-level folder or module of a source root
    | CollectionOverlap
      { overlapName   :: Text
      , overlapFolder :: Text   -- The source root it is also in
      }

    -- An import spelt in another case than the file it finds, which only a case-insensitive
    -- file system finds
    | ImportCase
      { writtenImport :: Text   -- As written in the import
      , fileImport    :: Text   -- As the file spells it
      }

    -- An :ouro newer than the compiler: as written, and the compiler's version
    | NewerOuro
      { neededOuro   :: Text
      , compilerOuro :: Text
      }

    -- An :ouro that is not a version, as 0.5
    | InvalidVersion
      { writtenVersion :: Text
      }

    -- A document chosen by :documents or -t whose module the project does not have
    | UnknownModule
      { unknownSelection :: Text
      }

    -- A document chosen by :documents or -t not written module/document
    | GraphSelectionForm
      { writtenSelection :: Text
      }

    -- A graph built whose name is not a file name on every system, and why
    | GraphFileName
      { fileNameGraph   :: Text
      , fileNameProblem :: Text   -- "holds '?'", "is a name Windows keeps for a device"
      }

    -- Graphs of one module, both built, named apart only by case: they would share a file
    -- where file names ignore case
    | GraphNameCase
      { caseGraph :: Text
      , caseOther :: Text   -- The graph before it, of the same name in another case
      }
    deriving (Show, Eq, Ord, Generic)


--- Warnings ---
data OuroWarning = OuroWarning
    { warnPos     :: SourcePos
    , warnContext :: WarningContext
    } deriving (Show, Eq, Ord, Generic)


-- WarningContext.
--
-- A Sum type for non-fatal architectural, structural, and semantic deviations.
--
-- Maps to static analysis passes designed to catch unintended layout behaviors,
-- dangling expressions, or mapping anomalies that do not fundamentally break
-- AST tree formulation, but modify or degrade the output serialized graph.
data WarningContext
    = Lint     LinterWarning
    | Semantic SemanticWarning
    | Vocab    VocabularyWarning
    deriving (Show, Eq, Ord, Generic)


--- LINTER WARNINGS ---
data LinterWarning
    -- LINT WARNING: UNANCHORED LITERAL (RETIRED, never raised)
    -- Was raised for a string written after the old hash-marked empty record or array, saying it would be
    -- discarded. It never was: in an array the string is kept, and in a record it is an error.
    -- Kept so that its code, WRN0000, is never given to another warning (gap 41).
    = UnanchoredLiteral
      { siblingForm  :: Text -- The old empty value or layout position where the leak occurred
      , erasedValue  :: Text -- The literal value string layout bound for compile-time erasure
      }

    -- LINT WARNING: DEPRECATED SYNTAX FEATURE
    -- Occurs when a program uses a form that still works but is now written another way, and
    -- will be an error in a later version: graph, which is now document, or an old hash-marked
    -- refinement, which is now <x ⊢ p>.
    --
    -- * Remediation: Issue warning. Name the form that replaces it.
    | DeprecatedSyntax
      { structuralFeature :: Text -- The old form, as written: graph
      , replacement       :: Text -- The form to write in its place: document
      }

    -- LINT WARNING: PARENTHESISED RESULT
    -- Occurs when a function's result is a single plain value in parentheses, such as
    -- (fn () ("hello")). Parentheses around values make an array, so the function
    -- returns ["hello"], which is rarely what was meant.
    --
    -- * Remediation: Issue warning. Suggest the value without its parentheses.
    | ParenthesisedResult
      { resultOwner :: Text -- The function's name, or "fn"
      , resultValue :: Text -- The value inside the parentheses
      }

    -- LINT WARNING: SHADOWED BUILT-IN
    -- Occurs when a function is named after a built-in, such as (define/procedure eq (a b) ...), or a
    -- special form, such as (define/procedure list (x) ...). The function replaces a built-in throughout the file:
    -- in calls, in case guards, and under any symbol the built-in is also written with, so
    -- (define/procedure = ...) replaces eq too; and it shadows a special form wherever it is called.
    --
    -- * Remediation: Issue warning. Suggest another name, unless the replacement is intended.
    | ShadowedBuiltin
      { shadowingKind :: Text -- "function" or "refinement"
      , shadowedName  :: Text -- The name of the built-in or special form the function replaces
      }

    -- LINT WARNING: NOT KEBAB CASE
    -- Occurs when a function or a graph is named in another case, such as makePlace or
    -- make_place. Ouro names functions and graphs in kebab-case, as Lisps do: make-place.
    -- Attribute keys and local bindings are exempt: keys follow the data's vocabulary.
    --
    -- * Remediation: Issue warning. Suggest the kebab-case name.
    | NotKebabCase
      { declarationKind :: Text -- "function", "refinement" or "document"
      , declarationName :: Text -- The name as written
      , kebabName       :: Text -- The suggested kebab-case name
      }

    -- LINT WARNING: SHADOWED BUNDLED MODULE (RETIRED, never raised)
    -- Was raised when a module of the project had the name of a module bundled with the
    -- compiler and was imported in its place. An import of std/… or aat/… is now always the
    -- bundled module. Kept so that its code, WRN0005, is never given to another warning.
    | ShadowedBundledModule
      { shadowingModule :: Text -- The module's name
      , shadowingFile   :: Text -- The project's file that replaces it
      }

    -- LINT WARNING: PROCEDURE OF NO PARAMETERS
    -- Occurs when a define/procedure takes no parameters, (define/procedure c () value). A
    -- function of no parameters is a constant, its value worked out once per graph, whichever
    -- keyword defines it, and define/constant says so.
    --
    -- * Remediation: Issue warning. Suggest define/constant.
    | ProcedureOfNoParameters
      { constantName :: Text -- The function's name
      }

    -- LINT WARNING: UNDOCUMENTED REFINEMENT
    -- Occurs when a define/refinement has no (doc "..."). A refinement's doc is the rule it
    -- asserts: a value that fails it is reported with the doc as the rule it broke, so
    -- without one a failure can only name the refinement.
    --
    -- * Remediation: Issue warning. Suggest a doc.
    | UndocumentedRefinement
      { undocumentedName :: Text -- The refinement's name
      }

    -- LINT WARNING: EMPTY RECORD TYPE
    -- Occurs when a type is written (record), a record type with no keys. It requires none,
    -- so it is every record, which the type record already says.
    --
    -- * Remediation: Issue warning. Suggest record.
    | EmptyRecordType

    -- LINT WARNING: TRIPLE-QUOTED STRING
    -- Occurs when a string is written """Spring""", as in Python or Kotlin. Ouro has no
    -- triple-quoted strings, so it is read as three strings, "", "Spring" and "", which an
    -- array keeps without complaint.
    --
    -- * Remediation: Issue warning. Suggest prose, (prose | …).
    | TripleQuotedString
    deriving (Show, Eq, Ord, Generic)


--- SEMANTIC WARNINGS ---
data SemanticWarning
    -- SEMANTIC WARNING: ELIDED PROPERTY ASSIGNMENT
    -- Occurs when a property mapping attribute key is directly assigned an expression that
    -- evaluates exclusively to an elided, compile-time control node (such as an un-nested 'define').
    -- Because elided forms yield zero serialization output bytes, the entire key pair is omitted.
    --
    -- * Remediation: Print warning notification. Direct the user to restructure their parentheses
    --   grouping so the real payload value is correctly nested within the body of the definition.
    = ElidedPropertyAssign
      { propertyKey     :: Text -- The attribute key block whose value expression is elided (e.g., ":calculated_index")
      , elidedConstruct :: Text -- The semantic name of the erasure constructor encountered (e.g., "define")
      }
    deriving (Show, Eq, Ord, Generic)


--- VOCABULARY WARNINGS ---
data VocabularyWarning
    -- VOCABULARY WARNING: UNBOUND TYPE REFERENCE
    -- Occurs when a type signature identifier or model type reference string fails to resolve
    -- against active Linked-Art schema definitions or imported vocabulary contexts. The compiler
    -- continues assembly under the assumption of an unvalidated, arbitrary context extension.
    --
    -- * Remediation: Log the mismatch. Run string distance matching against active known vocabularies
    --   to catch schema structural typos, and prompt the user to inspect remote reference frames.
    = UnboundTypeReference
      { typeIdentifier    :: Text -- The text of the unmapped type declaration found in the graph node
      , structuralContext :: Text -- The surrounding parent block context where the type reference was used
      }
    deriving (Show, Eq, Ord, Generic)
