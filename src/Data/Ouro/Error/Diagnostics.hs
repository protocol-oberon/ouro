{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}

module Data.Ouro.Error.Diagnostics where

import           Data.Data                        (Proxy (..))
import           Data.Ouro.Error.Types            (BuildError (..), ErrorContext (..),
                                                   InternalError (..),
                                                   LinterWarning (..), PathError (..),
                                                   ScopeError (..),
                                                   SemanticWarning (..),
                                                   SyntaxError (..), TypeError (..),
                                                   VocabularyWarning (..),
                                                   WarningContext (..))
import           Data.Ouro.Internal.Utils         (rankBySimilarity)
import           Data.Ouro.Lisp.Eval.Types        (humanReadableType)
import qualified Data.Ouro.Lisp.Eval.Types        as L
import           Data.Ouro.Refinement.Requirement (Failure (..))
import           Data.Ouro.Refinement.Type        (renderType)
import           Data.Ouro.Refinement.Value       (refinementOf)
import qualified Data.Set                         as Set
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           GHC.Generics                     (C1, D1, Generic (from), M1 (..),
                                                   Rep, type (:+:) (..))
import           Text.Megaparsec                  (SourcePos, sourceColumn, sourceLine,
                                                   sourceName, unPos)
import           Text.Printf                      (printf)


-- Append an educational explanation block to any underlying error context.
withBlurb :: Text -> ErrorContext -> ErrorContext
withBlurb blurb innerContext = AnnotatedContext blurb innerContext


--- Builders for the Error Types ---

-- Syntax Errors
lexicalError :: Text -> ErrorContext
lexicalError lexeme = Syntax $ LexicalError { rawLexeme = lexeme }


unbalancedDelimiter :: Text -> Text -> ErrorContext
unbalancedDelimiter expected actual = Syntax $ UnbalancedDelimiter
    { expectedDelim = expected
    , actualDelim   = actual
    }


malformedUnitArgument :: Text -> Text -> ErrorContext
malformedUnitArgument unit shape = Syntax $ MalformedUnitArgument
    { activeUnit     = unit
    , foundNodeShape = shape
    }


targetMismatch :: Text -> Text -> ErrorContext
targetMismatch expected actual = Syntax $ TargetMismatchAttr
    { expected   = expected
    , actualAttr = actual
    }


inexhaustiveCase :: Text -> Int -> ErrorContext
inexhaustiveCase fallthrough attempts = Syntax $ InexhaustiveCase
    { unmatchedTarget = fallthrough
    , parsedBranches  = attempts
    }


malformedCaseBranch :: ErrorContext
malformedCaseBranch = Syntax $ MalformedCaseBranch


malformedPattern :: ErrorContext
malformedPattern = Syntax $ MalformedPattern


malformedContext :: Text -> ErrorContext
malformedContext = Syntax . MalformedContext


-- A key with no value, and what follows it instead: another key, or the end of its form
missingAttributeValue :: Text -> Text -> ErrorContext
missingAttributeValue key next =
    withBlurb ( "The key ':" <> key <> "' has no value: it is followed by " <> next <> ". An attribute pair is a key and its value, "
             <> ":key value. Perhaps a value is missing, or the key should be removed."
              )
              (Syntax (MissingAttributeValue { valuelessKey = key, followedBy = next }))


missingOtherwise :: ErrorContext
missingOtherwise = Syntax $ MissingOtherwiseFallback


invalidFunctionName :: Text -> ErrorContext
invalidFunctionName = Syntax . InvalidFunctionName


-- Type Errors
typeMismatch :: Text -> Text -> ErrorContext
typeMismatch expected actual = Typing $ TypeMismatch
    { expectedType = expected
    , actualType   = actual
    }

-- A value without the built-in refinement a built-in needs of its operand
refinementMismatch :: Text -> Text -> ErrorContext
refinementMismatch expected actual = Typing $ RefinementMismatch
    { expectedRefinement = expected
    , actualRefinement   = actual
    }

-- A value outside a requirement, a signature's or an assertion's: the failure as data, its two
-- texts read off it
requirementFailed :: Failure -> ErrorContext
requirementFailed outside = Typing $ RefinementFailed
    { expectedRefinement = renderType (required outside)
    , actualRefinement   = given outside
    , failure            = outside
    }


-- Path Violations
missingPathKey :: Text -> [Text] -> ErrorContext
missingPathKey key keys = Path $ MissingPathKey
    { missingKey    = key
    , availableKeys = keys
    }


-- Scope Violations
unboundIdentifier :: Text -> ErrorContext
unboundIdentifier var = Scope $ ScopeError { variableName = var }


-- A name nothing binds where it is written, with the nearest of the names that are in scope
-- there, if one is near. An old name says what it is now instead: list, the Array
-- constructor (gap 55), and =, /= and ≠, the comparisons, which are words. A module that
-- defines its own list binds it, so this is never said of that list.
unboundName :: Text -> [Text] -> ErrorContext
unboundName name inScope =
    withBlurb ( "'" <> name <> "' is not bound here: no attribute pair, let binding or parameter around it, "
             <> "function of this module or built-in has that name." <> suggestion
              )
              (unboundIdentifier name)
    where
    suggestion = case (lookup name renamed, rankBySimilarity name inScope) of
                     (Just example, _)                        -> "\n\n'" <> name <> "' is now " <> example <> "."
                     (_, (bestMatch, score) : _) | score <= 3 -> "\n\nPerhaps you meant: '" <> bestMatch <> "'?"
                     _noneNear                                -> ""

    renamed = [ ("list", "'array': (array 1 2 3)")
              , ("=",    "'eq': (eq a b)")
              , ("/=",   "'neq': (neq a b)")
              , ("≠",    "'neq': (neq a b)") ]


-- A refinement's name that names no refinement: with the nearest of the module's refinements and
-- the built-in names, if one is near. An imported refinement, people/titled, is not yet read.
unknownRefinement :: Text -> [Text] -> ErrorContext
unknownRefinement name refinements =
    withBlurb ( "No refinement of this module, and no built-in refinement, is named '" <> name <> "'. "
             <> "A refinement is defined with (define/refinement " <> name <> " (<r ⊢ record>) ...); an imported one, people/titled, is not yet checked."
             <> suggestion
              )
              (lexicalError ("Unknown refinement: " <> name))
    where
    suggestion = case rankBySimilarity name (refinements <> builtinNames) of
                     (bestMatch, score) : _ | score <= 3 -> "\n\nPerhaps you meant: '" <> bestMatch <> "'?"
                     _noneNear                           -> ""

    -- On a tie the earlier name is suggested, so the short names come last: str is string, not uri
    builtinNames = ["string", "number", "boolean", "record", "array", "function", "uri", "date"]


-- A refinement's name that names a function of the procedure kind: only a refinement is asserted
notARefinement :: Text -> ErrorContext
notARefinement name =
    withBlurb ("Perhaps call it, (" <> name <> " x), or define it with define/refinement.")
              (lexicalError ("'" <> name <> "' is a function, not a refinement, so it cannot be asserted."))


cyclicDependency :: Text -> ErrorContext
cyclicDependency var = Scope $ CyclicDependencyError { variableName = var }


incorrectArity :: Text -> Int -> Int -> ErrorContext
incorrectArity name argNo actNo = Scope $ IncorrectArity
    { nameOfFunc   = name
    , expectNoArgs = argNo
    , actualNoArgs = actNo
    }


-- A function that reaches itself by its name without going down into a value each time round:
-- the circle of functions, from the one called back to it
recursiveCall :: [Text] -> ErrorContext
recursiveCall circle =
    withBlurb ( "'" <> T.concat (take 1 circle) <> "' calls itself" <> through <> ", and might never finish. "
             <> "Recursion must go down into a value: each time round, some call passes on a part of one of its "
             <> "parameters, such as a path into it, (get x part), a hole of its pattern, or an element of it, through map. "
             <> "A number counting down is not a part."
              )
              (typeMismatch "a terminating function call" "a recursive function call")
    where
    through = case circle of
                  [_, _]  -> ""
                  _longer -> ", through " <> T.intercalate " → " circle

indexOutOfBounds :: Text -> Text -> Int -> Int -> ErrorContext
indexOutOfBounds = \cb tOft att act -> Scope $ IndexOutOfBoundsError cb tOft att act


nonExistentGraph :: Text -> Text -> ErrorContext
nonExistentGraph = \trgt sug -> Scope $ NonExistentGraph trgt sug


-- An (import path) of a module that cannot be found, given the files looked for and the
-- bundled modules' names
moduleNotFound :: Text -> [FilePath] -> [Text] -> ErrorContext
moduleNotFound path searched bundledNames =
    withBlurb (looked <> hint) (Scope (ModuleNotFound { missingModule = path }))
    where
    looked = case searched of
                 []    -> ""
                 files -> "Looked for " <> T.intercalate ", " (map T.pack files) <> ". "

    -- A folder of bundled modules, such as std, is not a module itself: name its modules,
    -- suggesting its base module, which the others build on, when it has one
    hint = case folder of
               first : _ -> "'" <> path <> "' is a folder of modules, not a module: perhaps (import " <> suggested first <> "). "
                         <> "Its modules are: " <> T.intercalate ", " folder <> "."
               []        -> "The bundled modules are in the folders " <> andList roots <> ", and importing a folder, as (import std), "
                         <> "lists its modules." <> closest path bundledNames

    folder = filter ((path <> "/") `T.isPrefixOf`) bundledNames

    -- The folders the bundled modules are in, std and aat, each once: listing every module
    -- would bury the message
    roots = Set.toList (Set.fromList [ T.takeWhile (/= '/') name | name <- bundledNames ])

    andList = \case
        []    -> ""
        [one] -> one
        names -> T.intercalate ", " (init names) <> " and " <> last names

    suggested first
        | (path <> "/base") `elem` folder = path <> "/base"
        | otherwise                       = first


-- A module whose file exists but cannot be read: its file, and why
moduleUnreadable :: Text -> Text -> Text -> ErrorContext
moduleUnreadable path file reason =
    withBlurb reason (Scope (ModuleUnreadable { unreadableModule = path, unreadableFile = file, unreadableReason = reason }))


-- A qualified name whose qualifier names no import, given the file's qualifiers and the
-- bundled modules' names
unknownQualifier :: Text -> [Text] -> [Text] -> ErrorContext
unknownQualifier qualifier known bundledNames =
    withBlurb blurb (Scope (UnknownQualifier { qualifierName = qualifier }))
    where
    blurb
        | not (T.null (closest qualifier known))
            = "A qualified name, module/function, names a function of an imported module." <> closest qualifier known
        | Just path <- bundledWith qualifier
            = "A qualified name needs its module imported: perhaps add (import " <> path <> ") at the top of the file."
        | null known
            = "A qualified name, module/function, names a function of a module imported at the top of the file with (import ...). This file imports nothing."
        | otherwise
            = "A qualified name, module/function, names a function of an imported module. This file imports: " <> T.intercalate ", " known <> "."

    -- The bundled module this qualifier would name, std/base for base
    bundledWith q = case filter ((== q) . T.takeWhileEnd (/= '/')) bundledNames of
                        path : _ -> Just path
                        []       -> Nothing


-- A qualified name the module does not export, given what it does export
notExported :: Text -> Text -> [Text] -> ErrorContext
notExported qualifier name exportNames =
    withBlurb ("The module imported as '" <> qualifier <> "' exports: " <> T.intercalate ", " exportNames <> "." <> closest name exportNames)
              (Scope (NotExported { fromQualifier = qualifier, unexportedName = name, exportedNames = exportNames }))


-- Modules that import each other in a circle: the path of imports, ending where it began
importCycle :: [Text] -> ErrorContext
importCycle path =
    withBlurb ( "Modules cannot import each other in a circle: " <> T.intercalate " → " path <> ". Imports go one way, "
             <> "so a module is checked before the modules that import it. Perhaps move what they share into a module of its own, which each imports."
              )
              (Scope (ImportCycle { cyclePath = path }))


-- A module found in more than one source root
ambiguousModule :: Text -> [Text] -> ErrorContext
ambiguousModule path files =
    withBlurb ( "The module '" <> path <> "' is in more than one source root: " <> T.intercalate " and " files <> ". "
             <> "A module's name must name one file: perhaps rename or move one of them."
              )
              (Scope (AmbiguousModule { ambiguousPath = path, ambiguousFiles = files }))


-- Build errors: package.ouro and the modules under its :sources

-- The settings package.ouro can have, in the order they are listed
buildSettings :: [Text]
buildSettings = ["sources", "outputs", "documents", "ouro"]


-- The names package.ouro cannot define, kept for later versions of it
reservedPackageNames :: [Text]
reservedPackageNames = ["inputs", "build", "exports"]


-- A setting of package.ouro that is not one of its schema's
unknownBuildSetting :: Text -> ErrorContext
unknownBuildSetting setting =
    withBlurb ( "package.ouro's settings are " <> T.intercalate ", " (map (":" <>) buildSettings) <> "."
             <> closest (":" <> setting) (map (":" <>) buildSettings)
              )
              (Build (UnknownBuildSetting { unknownSetting = setting }))


-- A setting of the schema that this version of Ouro does not act on
unsupportedBuildSetting :: Text -> ErrorContext
unsupportedBuildSetting setting =
    withBlurb ( ":" <> setting <> " is a setting of package.ouro, but this version of Ouro does not act on it yet. "
             <> explanation
              )
              (Build (UnsupportedBuildSetting { unsupportedSetting = setting }))
    where
    explanation = case setting of
                      "documents" -> "Until it does, every document of every module is built: perhaps remove it."
                      _other    -> "Perhaps remove it."


-- The folder of :sources that does not exist, as written and as it was looked for
missingSourceFolder :: Text -> Text -> ErrorContext
missingSourceFolder folder looked =
    withBlurb ( "The source folder '" <> folder <> "' does not exist: looked for " <> looked <> ". "
             <> "package.ouro's :sources is a folder beside it, such as \"src\"."
              )
              (Build (MissingSourceFolder { sourceFolder = folder }))


-- A declaration package.ouro cannot hold, such as "a document"
notPackageData :: Text -> ErrorContext
notPackageData declaration =
    withBlurb ( "package.ouro holds one function, (define/constant package ...), and nothing else, so it cannot have "
             <> declaration <> ". Perhaps move it into a module under :sources."
              )
              (Build (NotPackageData { packageDeclaration = declaration }))


-- package.ouro without a function named package: the name of the function it has instead, if any
noPackageFunction :: Maybe Text -> ErrorContext
noPackageFunction found =
    withBlurb ( "package.ouro holds one function, named package, as in (define/constant package (:sources \"src\"))."
             <> maybe " An empty package.ouro is an error: perhaps delete it, since a project needs none."
                      (\name -> " Its function is named '" <> name <> "': perhaps rename it to package.") found
              )
              (Build (NoPackageFunction { foundFunction = found }))


-- A package function that takes parameters, how many
packageParameters :: Int -> ErrorContext
packageParameters count =
    withBlurb "The package function takes no parameters: its settings are a value, as in (define/constant package (:sources \"src\"))."
              (Build (PackageParameters { parameterCount = count }))


-- A module under :sources whose path starts with a bundled folder, such as std/base
reservedModule :: Text -> ErrorContext
reservedModule name =
    withBlurb ( "The module '" <> name <> "' is under :sources, but " <> root <> " is the folder of the modules bundled with Ouro, "
             <> "so (import " <> name <> ") always means the bundled one. Perhaps rename the " <> renamed
              )
              (Build (ReservedModule { reservedPath = name }))
    where
    root    = T.takeWhile (/= '/') name
    renamed = case root == name of
                  True  -> "file."
                  False -> "folder."


-- A function of package.ouro with a name kept for later versions of it
reservedPackageName :: Text -> ErrorContext
reservedPackageName name =
    withBlurb ( "'" <> name <> "' is reserved for a later version of package.ouro: "
             <> "the names inputs, build and exports cannot be defined in it."
              )
              (Build (ReservedPackageName { reservedName = name }))


-- A folder of vendor/ named after a bundled folder, such as vendor/std
reservedCollection :: Text -> ErrorContext
reservedCollection name =
    withBlurb ( "vendor/" <> name <> " cannot be a collection: " <> name <> " is the folder of the modules bundled with Ouro, "
             <> "so (import " <> name <> "/…) always means the bundled one. Perhaps rename the folder."
              )
              (Build (ReservedCollection { reservedFolder = name }))


-- A vendored library with a vendor/ folder of its own
nestedVendor :: Text -> ErrorContext
nestedVendor name =
    withBlurb ( "vendor/" <> name <> " has a vendor/ folder of its own: vendoring is flat, so a library's dependencies "
             <> "are vendored beside it, in vendor/. Perhaps move its folders up."
              )
              (Build (NestedVendor { nestedCollection = name }))


-- A collection's name that is also a top-level folder or module of a source root, as written
collectionOverlap :: Text -> Text -> ErrorContext
collectionOverlap name folder =
    withBlurb ( name <> " is a collection, vendor/" <> name <> ", and a module or folder of " <> folder
             <> ", so (import " <> name <> "/…) could mean either. Perhaps rename one of them."
              )
              (Build (CollectionOverlap { overlapName = name, overlapFolder = folder }))


-- An import spelt in another case than its file: as written, and as the file spells it
importCase :: Text -> Text -> ErrorContext
importCase written spelt =
    withBlurb ( "The module is spelt " <> spelt <> ": perhaps write (import " <> spelt <> "). "
             <> "A file system that ignores case finds it as written, but one that does not, such as Linux's, would not."
              )
              (Build (ImportCase { writtenImport = written, fileImport = spelt }))


-- An :ouro newer than this compiler: as written, and this compiler's version
newerOuro :: Text -> Text -> ErrorContext
newerOuro needed have =
    withBlurb ( "The project needs Ouro " <> needed <> ", and this is Ouro " <> have <> ". Perhaps update Ouro, "
             <> "or lower :ouro if the project builds with this version."
              )
              (Build (NewerOuro { neededOuro = needed, compilerOuro = have }))


-- An :ouro that is not a version
invalidVersion :: Text -> ErrorContext
invalidVersion written =
    withBlurb ( "The package setting :ouro is the version of Ouro the project needs, numbers apart by dots, as \"0.5\"."
              )
              (Build (InvalidVersion { writtenVersion = written }))


-- A document chosen by :documents or -t whose module the project does not have, with the project's
-- modules to suggest the closest of
unknownModule :: Text -> Text -> [Text] -> ErrorContext
unknownModule selection name modules =
    withBlurb ( "The project has no module " <> name <> ", so it has no document " <> selection <> "."
             <> closest name modules
              )
              (Build (UnknownModule { unknownSelection = selection }))


-- A document chosen by :documents or -t not written module/document
graphSelectionForm :: Text -> ErrorContext
graphSelectionForm written =
    withBlurb ( "A document is chosen by its module and its name, as people/person for the document person of the module people."
              )
              (Build (GraphSelectionForm { writtenSelection = written }))


-- A document built whose name cannot be a file name on every system
graphFileName :: Text -> Text -> ErrorContext
graphFileName name problem =
    withBlurb ( "A document is written to a file named after it, " <> name <> ".json, so its name must be a file name on every system: "
             <> "it cannot hold a slash or a backslash, nor any of < > : \" | ? *, nor be a name Windows keeps for a device, con, prn, aux, nul, com1 to com9 or lpt1 to lpt9, in any case."
              )
              (Build (GraphFileName { fileNameGraph = name, fileNameProblem = problem }))


-- Documents of one module named apart only by case
graphNameCase :: Text -> Text -> ErrorContext
graphNameCase name other =
    withBlurb ( "Where file names ignore case, as on Windows and macOS, " <> name <> ".json and " <> other <> ".json are one file, so one document would be written over the other. "
             <> "Name them apart by more than case."
              )
              (Build (GraphNameCase { caseGraph = name, caseOther = other }))


-- " Perhaps you meant 'x'." for the closest candidate, when one is close enough
closest :: Text -> [Text] -> Text
closest target candidates =
    case rankBySimilarity target candidates of
        (candidate, score) : _ | score <= 2 -> " Perhaps you meant '" <> candidate <> "'."
        _noneClose                          -> ""


-- Internal Violations
internalValueLeak :: Text -> ErrorContext
internalValueLeak block = Internal $ ErasureValueLeakError { blockName = block }

astCorruption :: Text -> Text -> ErrorContext
astCorruption active found = Internal $ ASTCorruptionError
    { activeSymbol  = active
    , foundASTShape = found
    }


--- Generic Error Code Generation ---

-- Automatically generates a zero-padded, 0-indexed error string
-- directly from the structural position inside the AST record domain.
smartErrorCode :: ErrorContext -> Text
smartErrorCode = \case
                  Syntax          sub     -> "SYN" <> renderIndex sub
                  Typing          sub     -> "TYP" <> renderIndex sub
                  Path            sub     -> "PTH" <> renderIndex sub
                  Scope           sub     -> "SCP" <> renderIndex sub
                  Internal        sub     -> "INT" <> renderIndex sub
                  Build           sub     -> "BLD" <> renderIndex sub
                  AnnotatedContext _  ctx -> smartErrorCode ctx

    where
    renderIndex :: (Generic a, GConstructorIndex (Rep a)) => a -> Text
    renderIndex val = T.pack $ printf "%04d" (gIndex (from val))


--- Generic Constructor Indexing Machinery ---
class GConstructorIndex f where
    gIndex :: f a -> Int

instance (GConstructorIndex a, GConstructorCount a, GConstructorIndex b) => GConstructorIndex (a :+: b) where
    gIndex (L1 x) = gIndex x
    gIndex (R1 x) = gIndex x + constructorCount (Proxy :: Proxy a)

instance GConstructorIndex (C1 c a) where
    gIndex _ = 0

instance GConstructorIndex a => GConstructorIndex (D1 c a) where
    gIndex (M1 x) = gIndex x


class GConstructorCount f where
    constructorCount :: proxy f -> Int

instance (GConstructorCount a, GConstructorCount b) => GConstructorCount (a :+: b) where
    constructorCount _ = constructorCount (Proxy :: Proxy a) + constructorCount (Proxy :: Proxy b)

instance GConstructorCount (C1 c a) where
    constructorCount _ = 1


--- Diagnostics Messages ---

-- Type Error
typeMismatchBlurb :: L.Expr -> Text
typeMismatchBlurb value =
    "Ouro's built-in refinements are string, number, boolean, uri, date, record, array and function."
    <> case refinementOf value of
           refinement
               | refinement `elem` builtInNames -> ""
               | otherwise                     -> " This value is " <> refinement <> ", which has no built-in refinement."
    <> maybe "" (\call -> " This value is a function: perhaps call it, " <> call <> ".") (L.callOf value)


-- The built-in refinements a value can have, by the names messages give them
builtInNames :: [Text]
builtInNames = ["string", "number", "boolean", "uri", "date", "record", "array", "function"]


binaryOpMismatchBlurb :: L.Expr -> L.Expr -> Text
binaryOpMismatchBlurb base modif = "The provided operands " <> humanReadableType base
                                <> " and " <> humanReadableType modif
                                <> " do not support the requested operation. "
                                <> "Verify that your input data matches the expected structural schema."


inexhaustiveCaseBlurb :: Int -> Text
inexhaustiveCaseBlurb attempts =
    let baseBlurb = "Ouro requires absolute determinism for branch evaluation. Because every case form must completely map its input domain, a failure to match implies an incomplete data flow contract."
        remediation = "\n\nRemediation: Append an explicit '(otherwise <fallback-expr>)' clause as the final branch of the case statement to safely handle unmatched states."
    in case attempts of
        -- Standard Fallthroughs
        0 -> baseBlurb
          <> "\n\nThe case block contained no evaluated branches."
          <> remediation

        1 -> baseBlurb
          <> "\n\nThe evaluation engine sequentially verified 1 conditional branch, but it failed to match the target payload."
          <> remediation

        n -> baseBlurb
          <> "\n\nThe evaluation engine sequentially verified " <> T.pack (show n) <> " conditional branches, but all failed to match the target payload."
          <> remediation


missingOtherwiseBlurb :: Text
missingOtherwiseBlurb =
    "Ouro enforces strict determinism at compile time. A 'case' statement was detected without a terminal 'otherwise' branch. "
    <> "To guarantee that the compiled output never silently drops data due to an unhandled state, you must explicitly declare a fallback behavior."
    <> "\n\nPerhaps append '(otherwise <fallback-expr>)' as the final branch."



--- Builder for Linter Warnings ---
-- Create a lint warning for a form with a new name, the old one and the new
deprecatedSyntax :: Text -> Text -> WarningContext
deprecatedSyntax feature newName = Lint $ DeprecatedSyntax
    { structuralFeature = feature
    , replacement       = newName
    }

-- Create a lint warning for a function result that is one value in parentheses
parenthesisedResult :: Text -> Text -> WarningContext
parenthesisedResult owner val = Lint $ ParenthesisedResult
    { resultOwner = owner
    , resultValue = val
    }

-- Create a warning for a function that replaces a built-in
shadowedBuiltin :: Text -> Text -> WarningContext
shadowedBuiltin kind name = Lint $ ShadowedBuiltin { shadowingKind = kind, shadowedName = name }

-- Create a warning for a function or graph name that is not kebab-case
notKebabCase :: Text -> Text -> Text -> WarningContext
notKebabCase kind name kebab = Lint $ NotKebabCase
    { declarationKind = kind
    , declarationName = name
    , kebabName       = kebab
    }

-- Create a warning for a define/procedure of no parameters, which is a constant
procedureOfNoParameters :: Text -> WarningContext
procedureOfNoParameters name = Lint $ ProcedureOfNoParameters { constantName = name }

-- Create a warning for a refinement without a doc, the rule its failures show
undocumentedRefinement :: Text -> WarningContext
undocumentedRefinement name = Lint $ UndocumentedRefinement { undocumentedName = name }

-- Create a warning for (record), a record type with no keys, which is the type record
emptyRecordType :: WarningContext
emptyRecordType = Lint EmptyRecordType

-- Create a warning for """text""", which Ouro reads as three strings
tripleQuotedString :: WarningContext
tripleQuotedString = Lint TripleQuotedString

-- Create a compile-time elided property assignment semantic warning
elidedPropertyAssign :: Text -> Text -> WarningContext
elidedPropertyAssign key construct = Semantic $ ElidedPropertyAssign
    { propertyKey     = key
    , elidedConstruct = construct
    }

-- Create an unmapped model or schema type vocabulary warning
unboundTypeReference :: Text -> Text -> WarningContext
unboundTypeReference typeId ctx = Vocab $ UnboundTypeReference
    { typeIdentifier    = typeId
    , structuralContext = ctx
    }

-- Automatically generates a zero-padded, 0-indexed warning string
-- directly using existing GConstructorIndex machinery
smartWarningCode :: WarningContext -> Text
smartWarningCode wrapper = "WRN" <> case wrapper of
    Lint     sub -> renderIndex sub
    Semantic sub -> renderIndex sub
    Vocab    sub -> renderIndex sub
    where
    renderIndex :: (Generic a, GConstructorIndex (Rep a)) => a -> Text
    renderIndex val = T.pack $ printf "%04d" (gIndex (from val))


formatSmartLiteral :: Text -> Text
formatSmartLiteral t
    | "\"" `T.isPrefixOf` t = t
    | otherwise             = "\"" <> t <> "\""


--- Warnings Messages ---
warningBlurb :: WarningContext -> Text
warningBlurb = \case
    Lint (UnanchoredLiteral sibling val)
        -> "Ouro evaluates floating values inside array and record layers as unanchored literals. "
        <> "Because this value is positioned directly next to " <> sibling <> ", it has no semantic binding key "
        <> "and will be completely erased from properties at compile time.\n\n"
        <> "Perhaps assign it to a property attribute key (e.g., :label " <> formatSmartLiteral val <> "), if it is intended to be saved."

    Lint (DeprecatedSyntax feature newName)
        -> "'" <> feature <> "' still works, as '" <> newName <> "' does, and will be an error in a later version.\n\n"
        <> "Perhaps write '" <> newName <> "' in its place."

    Lint (ParenthesisedResult _ val)
        -> "A function's result is one expression, and parentheses around values make an array: "
        <> "(" <> val <> ") is [" <> val <> "].\n\n"
        <> "Perhaps remove the parentheses: " <> val <> ", if a single value is intended."

    Lint (ShadowedBuiltin kind name)
        -> "A function named after a built-in or a special form replaces it throughout the file, as a definition does in Scheme: "
        <> "a built-in in calls, in case guards and under any symbol it is also written with (= is eq, ∪ is union), "
        <> "a special form wherever it is called.\n\n"
        <> "Perhaps choose another name for the " <> kind <> ", unless replacing '" <> name <> "' is intended."

    Lint (NotKebabCase _ _ kebab)
        -> "Functions and documents are named in kebab-case, as in other Lisps: lowercase words joined by '-'. "
        <> "Attribute keys and local bindings may be named as their data needs.\n\n"
        <> "Perhaps rename it '" <> kebab <> "'."

    Lint (ShadowedBundledModule name file)
        -> "A module of the project with the name of a bundled module is imported in its place: " <> file <> " replaces the bundled '" <> name <> "'.\n\n"
        <> "Perhaps rename the file, unless replacing the bundled module is intended."

    Lint (ProcedureOfNoParameters name)
        -> "A function of no parameters is a constant: named as a value it is its result, worked out once per document, "
        <> "whichever keyword defines it.\n\n"
        <> "Perhaps define it as one: (define/constant " <> name <> " value)."

    Lint (UndocumentedRefinement name)
        -> "A refinement's doc is the rule it asserts: a value that fails it is reported with the doc as the rule it broke, "
        <> "as in '" <> name <> ": Has a label.'. Without one, a failure can only name the refinement.\n\n"
        <> "Perhaps write one: (define/refinement " <> name <> " (<r ⊢ record>) (doc \"...\") ...)."

    Lint EmptyRecordType
        -> "A record type lists the keys a record must have, each with its type. With none it requires nothing, "
        <> "so it is every record, which record already says.\n\n"
        <> "Perhaps write record: <r ⊢ record>."

    Lint TripleQuotedString
        -> "Ouro has no triple-quoted strings: \"\"\"Spring\"\"\" is read as three strings, \"\", \"Spring\" and \"\", "
        <> "which an array keeps without complaint.\n\n"
        <> "Perhaps write long text with prose: (prose | …), each line beginning with a bar, its ')' on a line of its own."

    Semantic (ElidedPropertyAssign key construct)
        -> "The expression assigned to the property attribute '" <> key <> "' evaluates exclusively to a compile-time '"
        <> construct <> "' block. Because elided control structures leave zero footprint in final binary payloads, "
        <> "this entire attribute pair will vanish from the compiled JSON-LD document context entirely."

    Vocab (UnboundTypeReference typeId ctx)
        -> "The active validation profile cannot resolve the declaration type reference '" <> typeId <> "' "
        <> "within " <> ctx <> ". The compiler will allow compilation to complete as a flexible schema extension, "
        <> "but the output attribute pair might fail automated Linked-Art semantic shape checks down the pipeline."


--- Warning Context Diagnostic Summaries ---
warningSummary :: WarningContext -> Text
warningSummary = \case
    Lint (UnanchoredLiteral _ val)      -> "Unanchored primitive literal " <> val <> " detected"
    Lint (DeprecatedSyntax feature new) -> "'" <> feature <> "' is now written '" <> new <> "'"
    Lint (ParenthesisedResult owner _)  -> "The result of '" <> owner <> "' is an array of one value"
    Lint (ShadowedBuiltin kind name)    -> "The " <> kind <> " '" <> name <> "' replaces the built-in '" <> name <> "'"
    Lint (NotKebabCase kind name _)     -> "The " <> kind <> " '" <> name <> "' is not named in kebab-case"
    Lint (ShadowedBundledModule name _) -> "The project's module '" <> name <> "' replaces the bundled '" <> name <> "'"
    Lint (ProcedureOfNoParameters name) -> "The procedure '" <> name <> "' takes no parameters, so it is a constant"
    Lint (UndocumentedRefinement name)  -> "The refinement '" <> name <> "' has no doc"
    Lint EmptyRecordType                -> "(record) is the type record"
    Lint TripleQuotedString             -> "Ouro has no triple-quoted strings"
    Semantic (ElidedPropertyAssign k _) -> "Property assignment '" <> k <> "' binds to an elided value"
    Vocab (UnboundTypeReference t _)    -> "Unbound schema type reference '" <> t <> "'"


-- Where a diagnostic is shown: the file it was reported in, its line and column, and the
-- text of that line. The file is the one the position names, which is not always the file
-- compiled: an error inside an imported module is shown in that module, with its line.
data SourceFrame = SourceFrame
    { frameFile   :: FilePath
    , frameLine   :: Int
    , frameColumn :: Int
    , frameText   :: Text
    } deriving (Show, Eq)

sourceFrame :: (FilePath -> Maybe Text) -> SourcePos -> SourceFrame
sourceFrame sourceOf pos = SourceFrame file line (unPos (sourceColumn pos)) text
    where
    file = sourceName pos
    line = unPos (sourceLine pos)
    text = case drop (line - 1) . T.lines <$> sourceOf file of
               Just (found : _) -> found
               _noSuchLine      -> ""
