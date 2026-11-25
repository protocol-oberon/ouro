{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Ouro.Lisp.Module.Types where

import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe)
import qualified Data.Set                  as Set
import           Data.Ouro.Lisp.Surface    (Expr, Param (..))
import           Data.Ouro.Refinement.Type (Type)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Lens.Micro.TH             (makeLenses)
import           Text.Megaparsec           (SourcePos)


-- DataKind to represent expression types
data Declaration
    = FunctionExpr FunctionDeclaration
    | GraphExpr
    deriving (Show, Eq)

-- DataKind to represent the different types of function
data FunctionDeclaration
    = Procedure
    | Refinement
    deriving (Show, Eq)

-- The kind of a function, at runtime: matching on it tells the type checker which it is
data FunctionKind (k :: FunctionDeclaration) where
    ProcedureKind  :: FunctionKind 'Procedure
    RefinementKind :: FunctionKind 'Refinement

deriving instance Show (FunctionKind k)
deriving instance Eq   (FunctionKind k)

-- The declaration a kind witnesses, as a plain value: what a closure keeps of its function's
-- kind once the function is a value, where the type index is gone
declarationOf :: FunctionKind k -> FunctionDeclaration
declarationOf = \case
    ProcedureKind  -> Procedure
    RefinementKind -> Refinement

data SomeFunction where
    SomeFunction :: HigherExpression ('FunctionExpr k) -> SomeFunction

instance Eq SomeFunction where
    SomeFunction a@(Function ka _ _ _ _ _ _) == SomeFunction b@(Function kb _ _ _ _ _ _) =
        case (ka, kb) of
            (ProcedureKind, ProcedureKind)   -> a == b
            (RefinementKind, RefinementKind) -> a == b
            _differentKinds                  -> False

instance Show SomeFunction where
    showsPrec d (SomeFunction f) = showParen (d > 10) (showString "SomeFunction " . showsPrec 11 f)


-- Higher Expression Surface AST of a module, indexed by the DeclType
-- A function keeps its documentation, from a (doc "...") in its body, if it has one, and
-- its signature: its parameters, each with its requirement, and the requirement on its
-- result, its return type, which is checked when it is called, not written into its body
data HigherExpression (t :: Declaration) where
    Function :: FunctionKind k
             -> SourcePos
             -> Text
             -> Maybe Text
             -> [Param]
             -> Maybe (SourcePos, Type)
             -> Expr
             -> HigherExpression ('FunctionExpr k)

    Graph    :: SourcePos -> Text -> Expr -> HigherExpression 'GraphExpr

deriving instance Show (HigherExpression t)
deriving instance Eq   (HigherExpression t)


-- Maps a transformation function over the underlying Expr inside a HigherExpression.
mapExpr :: (Expr -> Expr) -> HigherExpression t -> HigherExpression t
mapExpr f exprNode =
    case exprNode of
        Function kind pos name doc args result expr -> Function kind pos name doc args result (f expr)
        Graph    pos name expr                      -> Graph    pos name (f expr)


-- An (import path) or (import path as alias) declaration
data Import = Import
    { importPos   :: SourcePos
    , importPath  :: Text         -- The module's path: std, or linked-art/v1/people
    , importAlias :: Maybe Text   -- The name written after 'as'
    } deriving (Show, Eq)


-- The name an import's functions are qualified with: its alias, or its path's last segment,
-- so (import linked-art/v1/people) is people/person
qualifierOf :: Import -> Text
qualifierOf imp = fromMaybe (T.takeWhileEnd (/= '/') (importPath imp)) (importAlias imp)


-- A qualified name split at its first '/': base/core is ("base", "core"). Both sides must be
-- non-empty, so the operators / and /= are not qualified names. A built-in family's variant,
-- shift/forward, date/before, is no qualified name: its '/' names a variant, as '/' always does.
splitQualified :: Text -> Maybe (Text, Text)
splitQualified name =
    case T.breakOn "/" name of
        (qualifier, rest)
            | not (T.null qualifier), T.length rest > 1, qualifier `notElem` builtinFamilies -> Just (qualifier, T.drop 1 rest)
        _notQualified                                                                     -> Nothing


-- The built-ins written family/variant: the shifts of a date, operations on dates and on
-- durations. A module cannot be imported under one of these names.
builtinFamilies :: [Text]
builtinFamilies = ["shift", "date", "duration"]


-- Module Environment, mathematically proven to be segregated. A module's imports and
-- exports are written first in its file; the exports name functions of this module only.
-- The static analysis adds the calls by name it found to go down into a value, by where
-- their function's name is written, which may call a function already being called.
data Module = Module
    { _functionRegistry :: Map.Map Text SomeFunction
    , _graphRegistry    :: Map.Map Text (HigherExpression 'GraphExpr)
    , _imports          :: [Import]
    , _exports          :: [(Text, SourcePos)]
    , _acceptedCalls    :: Set.Set SourcePos
    } deriving (Show, Eq)


-- Applies an expression transformation across all declarations in a Module.
mapModuleExpr :: (Expr -> Expr) -> Module -> Module
mapModuleExpr transform m = m
    { _functionRegistry = Map.map (\(SomeFunction f) -> SomeFunction (mapExpr transform f)) (_functionRegistry m)
    , _graphRegistry    = Map.map (mapExpr transform) (_graphRegistry m)
    -- imports and exports are names, left untouched
    }


-- Applies an expression transformation to the patterns of every function's parameters.
-- A fn's parameters are part of its form, so they are reached by 'mapModuleExpr'.
mapParamPatterns :: (Expr -> Expr) -> Module -> Module
mapParamPatterns transform m = m
    { _functionRegistry = Map.map patterns (_functionRegistry m) }
    where
    patterns (SomeFunction (Function kind pos name doc args result expr)) =
        SomeFunction (Function kind pos name doc [ arg { paramPattern = transform <$> paramPattern arg } | arg <- args ] result expr)

makeLenses ''Module
