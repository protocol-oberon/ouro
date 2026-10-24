{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Ouro.Lisp.Module.Types where

import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe)
import           Data.Ouro.Lisp.Surface (Expr, Param (..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Lens.Micro.TH          (makeLenses)
import           Text.Megaparsec        (SourcePos)


-- DataKind to represent expression types
data Declaration
    = FunctionExpr
    | GraphExpr
    deriving (Show, Eq)


-- Higher Expression Surface AST of a module, indexed by the DeclType
-- A function keeps its documentation, from a (doc "...") in its body, if it has one
data HigherExpression (t :: Declaration) where
    Function :: SourcePos -> Text -> Maybe Text -> [Param] -> Expr -> HigherExpression 'FunctionExpr
    Graph    :: SourcePos -> Text -> Expr                          -> HigherExpression 'GraphExpr

deriving instance Show (HigherExpression t)
deriving instance Eq   (HigherExpression t)


-- Maps a transformation function over the underlying Expr inside a HigherExpression.
mapExpr :: (Expr -> Expr) -> HigherExpression t -> HigherExpression t
mapExpr f exprNode =
    case exprNode of
        Function pos name doc args expr -> Function pos name doc args (f expr)
        Graph    pos name expr          -> Graph    pos name (f expr)


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


-- A qualified name split at its first '/': std/core is ("std", "core"). Both sides must be
-- non-empty, so the operators / and /= are not qualified names.
splitQualified :: Text -> Maybe (Text, Text)
splitQualified name =
    case T.breakOn "/" name of
        (qualifier, rest)
            | not (T.null qualifier), T.length rest > 1 -> Just (qualifier, T.drop 1 rest)
        _notQualified                                   -> Nothing


-- Module Environment, mathematically proven to be segregated. A module's imports and
-- exports are written first in its file; the exports name functions of this module only.
data Module = Module
    { _functionRegistry :: Map.Map Text (HigherExpression 'FunctionExpr)
    , _graphRegistry    :: Map.Map Text (HigherExpression 'GraphExpr)
    , _imports          :: [Import]
    , _exports          :: [(Text, SourcePos)]
    } deriving (Show, Eq)


-- Applies an expression transformation across all declarations in a Module.
mapModuleExpr :: (Expr -> Expr) -> Module -> Module
mapModuleExpr transform m = m
    { _functionRegistry = Map.map (mapExpr transform) (_functionRegistry m)
    , _graphRegistry    = Map.map (mapExpr transform) (_graphRegistry m)
    -- imports and exports are names, left untouched
    }


-- Applies an expression transformation to the patterns of every defn's parameters.
-- A fn's parameters are part of its form, so they are reached by 'mapModuleExpr'.
mapParamPatterns :: (Expr -> Expr) -> Module -> Module
mapParamPatterns transform m = m
    { _functionRegistry = Map.map patterns (_functionRegistry m) }
    where
    patterns (Function pos name doc args expr) =
        Function pos name doc [ arg { paramPattern = transform <$> paramPattern arg } | arg <- args ] expr

makeLenses ''Module
