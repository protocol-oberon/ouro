{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Ouro.Lisp.Module.Types where

import qualified Data.Map.Strict        as Map
import           Data.Ouro.Lisp.Surface (Expr)
import           Data.Text              (Text)
import           Lens.Micro.TH          (makeLenses)
import           Text.Megaparsec        (SourcePos)


-- DataKind to represent expression types
data Declaration
    = FunctionExpr
    | TemplateExpr
    | GraphExpr
    deriving (Show, Eq)


-- Higher Expression Surface AST of a module, indexed by the DeclType
data HigherExpression (t :: Declaration) where
    Template :: SourcePos -> Text -> [Text] -> Expr -> HigherExpression 'TemplateExpr
    Graph    :: SourcePos -> Text -> Expr           -> HigherExpression 'GraphExpr

deriving instance Show (HigherExpression t)
deriving instance Eq   (HigherExpression t)


-- Maps a transformation function over the underlying Expr inside a HigherExpression.
mapExpr :: (Expr -> Expr) -> HigherExpression t -> HigherExpression t
mapExpr f exprNode =
    case exprNode of
        Template pos name args expr -> Template pos name args (f expr)
        Graph    pos name expr      -> Graph    pos name (f expr)


-- The ExportMap mirrors the segregation to maintain type safety across boundaries
data ExportMap = ExportMap
    { _exportedTemplates :: Map.Map Text (HigherExpression 'TemplateExpr)
    } deriving (Show, Eq)


-- Module Environment, mathematically proven to be segregated
data Module = Module
    { _templateRegistry :: Map.Map Text (HigherExpression 'TemplateExpr)
    , _graphRegistry    :: Map.Map Text (HigherExpression 'GraphExpr)
    , _importRegistry   :: Map.Map Text ExportMap
    } deriving (Show, Eq)


-- Applies an expression transformation across all declarations in a Module.
mapModuleExpr :: (Expr -> Expr) -> Module -> Module
mapModuleExpr transform m = m
    { _templateRegistry = Map.map (mapExpr transform) (_templateRegistry m)
    , _graphRegistry    = Map.map (mapExpr transform) (_graphRegistry m)
    -- _importRegistry is left untouched
    }

makeLenses ''ExportMap
makeLenses ''Module
