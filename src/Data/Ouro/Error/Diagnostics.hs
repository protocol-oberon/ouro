{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}

module Data.Ouro.Error.Diagnostics where

import           Data.Data                 (Proxy (..))
import           Data.Ouro.Error.Types     (ErrorContext (..),
                                            InternalError (..),
                                            LinterWarning (..), PathError (..),
                                            ScopeError (..),
                                            SemanticWarning (..),
                                            SyntaxError (..), TypeError (..),
                                            VocabularyWarning (..),
                                            WarningContext (..))
import qualified Data.Ouro.Internal.Expr   as I
import           Data.Ouro.Lisp.Eval.Types (humanReadableType)
import qualified Data.Ouro.Lisp.Eval.Types as L
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Generics              (C1, D1, Generic (from), M1 (..),
                                            Rep, type (:+:) (..))
import           Text.Printf               (printf)


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


malformedTag :: Text -> Text -> ErrorContext
malformedTag tag shape = Syntax $ MalformedTagPayload
    { activeTag      = tag
    , foundNodeShape = shape
    }


targetMismatch :: Text -> Text -> ErrorContext
targetMismatch expected actual = Syntax $ TargetMismatchAttr
    { expected   = expected
    , actualAttr = actual
    }


-- Type Errors
typeMismatch :: Text -> Text -> ErrorContext
typeMismatch expected actual = Typing $ TypeMismatch
    { expectedType = expected
    , actualType   = actual
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


cyclicDependency :: Text -> ErrorContext
cyclicDependency var = Scope $ CyclicDependencyError { variableName = var }


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
typeMismatchBlurb =
    let blurb = "Ouro evaluates type tags as strict domain assertions. When layout constraints fail, the engine isolates the node to protect the structural integrity of the compiled JSON."
    in \case
        (L.Primitive p) -> let suggestion   = blurb <> "\n\nPerhaps use the type assertion tag "
                               cannotAssert = blurb <> "\n\nYou cannot make a type assertion for "
                         in case p of
                                -- Primitives
                                I.BlankNode _ -> cannotAssert <> "a blank node"
                                I.Boolean   _ -> suggestion   <> "#bool"
                                I.Date      _ -> suggestion   <> "#data"
                                I.EmptyArr    -> suggestion   <> "#arr-empty"
                                I.EmptyObj    -> suggestion   <> "#obj-empty"
                                I.Null        -> cannotAssert <> "a null value"
                                I.Number    _ -> suggestion   <> "#num"
                                I.String    _ -> suggestion   <> "#str"
                                I.URI       _ -> suggestion   <> "#uri"
                                -- Structural Primitives
                                I.Array  _    -> cannotAssert <> "an array"
                                I.Object _ _  -> cannotAssert <> "a object"

        _ -> blurb
          <> "You can only make type assertions on primitive values. Supported types include:"
          <> "\n  String  -> #str"
          <> "\n  Number  -> #num"
          <> "\n  URI     -> #uri"
          <> "\n  Boolean -> #bool"
          <> "\n  Date    -> #date"


binaryOpMismatchBlurb :: L.Expr -> L.Expr -> Text
binaryOpMismatchBlurb base modif =
    "Ouro evaluates type tags as strict domain assertions. "
    <> "The provided operands " <> humanReadableType base
    <> " and " <> humanReadableType modif
    <> " do not support the requested operation. "
    <> "Verify that your input data matches the expected structural schema."


--- Builder for Linter Warnings ---
-- Create a non-fatal unanchored primitive literal lint warning
unanchoredLiteral :: Text -> Text -> WarningContext
unanchoredLiteral tag val = Lint $ UnanchoredLiteral
    { tagAttempted = tag
    , erasedValue  = formatSmartLiteral val
    }

-- Create a legacy syntax usage lint warning
deprecatedSyntax :: Text -> WarningContext
deprecatedSyntax feature = Lint $ DeprecatedSyntax
    { structuralFeature = feature
    }

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
    Lint (UnanchoredLiteral tag val)
        -> "Ouro evaluates floating values inside array and object layers as unanchored literals. "
        <> "Because this value is positioned directly next to " <> tag <> ", it has no semantic binding key "
        <> "and will be completely erased from properties at compile time.\n\n"
        <> "Perhaps assign it to a property attribute key (e.g., :label " <> formatSmartLiteral val <> "), if it is intendented to be saved."

    Lint (DeprecatedSyntax feature)
        -> "The structural pattern '" <> feature <> "' has been designated as obsolete legacy syntax. "
        <> "While still parsed for backwards-compatibility, it will be completely removed in future breaking spec bumps."

    Semantic (ElidedPropertyAssign key construct)
        -> "The expression assigned to the property attribute '" <> key <> "' evaluates exclusively to a compile-time '"
        <> construct <> "' block. Because elided control structures leave zero footprint in final binary payloads, "
        <> "this entire key-value pair will vanish from the compiled JSON-LD document context entirely."

    Vocab (UnboundTypeReference typeId ctx)
        -> "The active validation profile cannot resolve the declaration type reference '" <> typeId <> "' "
        <> "within " <> ctx <> ". The compiler will allow compilation to complete as a flexible schema extension, "
        <> "but the output field might fail automated Linked-Art semantic shape checks down the pipeline."


--- Warning Context Diagnostic Summaries ---
warningSummary :: WarningContext -> Text
warningSummary = \case
    Lint (UnanchoredLiteral _ val)      -> "Unanchored primitive literal " <> val <> " detected"
    Lint (DeprecatedSyntax feature)     -> "Use of deprecated syntactic feature: " <> feature
    Semantic (ElidedPropertyAssign k _) -> "Property assignment '" <> k <> "' binds to an elided value"
    Vocab (UnboundTypeReference t _)    -> "Unbound schema type reference '" <> t <> "'"
