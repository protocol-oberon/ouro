{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Ouro.Error.Types where

import           Data.Text       (Text)
import           GHC.Generics    (Generic)
import           Text.Megaparsec (SourcePos (sourceColumn, sourceLine), unPos)


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

    -- SYNTAX VIOLATION: MALFORMED TAG MODIFIER METADATA
    -- Occurs when a type tag macro modifier (e.g., #uri, #date) is immediately
    -- followed by an invalid expression sequence instead of its required string literal
    -- target or resolvable payload node.
    --
    -- * Remediation: Flag the malformed tag binding block and remind the developer
    --   of the strict primitive typing requirements tied to that specific tag specification.
    | MalformedTagPayload
      { activeTag      :: Text -- The name of the semantic modifier tag that triggered the error
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

    -- SYNTAX VIOLATION: BUILTIN KEYWORD SHADOWING
    -- Occurs when a structural layout configuration or user-defined assignment attempts
    -- to bind a localized variable or graph attribute name to an identifier string
    -- that is strictly reserved for core system primitives and evaluator keywords.
    --
    -- * Remediation: The compiler or editing layer must inform the user that core system
    --   operators cannot be re-bound or overwritten, and suggest choosing a unique alternate
    --   identifier name that does not conflict with the foundational engine registry.
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


    | InvalidTemplateName
      { invalidName :: Text }
    deriving (Show, Eq, Ord, Generic)


--- TYPE VIOLATIONS ---
data TypeError
    -- TYPE VIOLATION: VALUE DOMAIN MISMATCH
    -- Occurs during the explicit tag modification evaluation phase (e.g., @#uri@, @#date@, @#str@)
    -- or during backend GADT conversion when an expression resolves to a primitive literal
    -- or structure that contradicts the declared domain constraint.
    --
    -- * Remediation: Reject the evaluation frame. Output must state the explicit type constraints
    --   and provide structural layout expectations (e.g., ISO-8601 formatting for date types).
    = TypeMismatch
      { expectedType :: Text  -- The semantic type or specification structure enforced by the constraint
      , actualType   :: Text  -- The runtime type evaluation primitive actually generated by the AST node
      }
    deriving (Show, Eq, Ord, Generic)


--- PATH VIOLATIONS ---
data PathError
    -- PATH VIOLATION: STRUCTURAL FIELD MISSING
    -- Occurs during multi-layered property traversal via the @get@ operator. The 'resolvePath'
    -- engine successfully located a parent block node, but the current string path segment key
    -- does not map to any active defined attributes within that target horizontal layer stream.
    --
    -- * Remediation: Abort traversal. The reporting layer should compute the string distance
    --   (Levenshtein metric) between the missing key and available fields to suggest adjacent typos.
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
    -- LINT WARNING: UNANCHORED LITERAL EXPRESSION
    -- Occurs when a primitive literal value sits directly inside an object or array block
    -- container without an associated property key binder or macro anchor sequence.
    -- The value will be evaluated but silently discarded during final serialization passes.
    --
    -- * Remediation: Issue warning. Compute format suggestions to wrap the value
    --   in a proper property assignment framework (e.g., using a smart quote decorator).
    = UnanchoredLiteral
      { tagAttempted :: Text -- The sibling tag or layout position where the leak occurred
      , erasedValue  :: Text -- The literal value string layout bound for compile-time erasure
      }

    -- LINT WARNING: DEPRECATED SYNTAX FEATURE
    -- Occurs when the compilation pass matches a valid but legacy structural configuration
    -- pattern earmarked for complete deprecation in upcoming compiler iterations.
    --
    -- * Remediation: Flag visual warnings. Surface modern alternative syntax layout recommendations.
    | DeprecatedSyntax
      { structuralFeature :: Text -- The name or token layout string of the legacy feature
      }
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
    -- Occurs when a type signature identifier or model tag reference string fails to resolve
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
