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
    Function :: SourcePos -> Text -> [Text] -> Expr -> HigherExpression 'FunctionExpr
    Template :: SourcePos -> Text -> [Text] -> Expr -> HigherExpression 'TemplateExpr
    Graph    :: SourcePos -> Text -> Expr           -> HigherExpression 'GraphExpr

deriving instance Show (HigherExpression t)
deriving instance Eq   (HigherExpression t)


-- The ExportMap mirrors the segregation to maintain type safety across boundaries
data ExportMap = ExportMap
    { _exportedTemplates :: Map.Map Text (HigherExpression 'TemplateExpr)
    , _exportedFunctions :: Map.Map Text (HigherExpression 'FunctionExpr)
    } deriving (Show, Eq)


-- Module Environment, mathematically proven to be segregated
data Module = Module
    { _functionRegistry :: Map.Map Text (HigherExpression 'FunctionExpr)
    , _templateRegistry :: Map.Map Text (HigherExpression 'TemplateExpr)
    , _graphRegistry    :: Map.Map Text (HigherExpression 'GraphExpr)
    , _importRegistry   :: Map.Map Text ExportMap
    } deriving (Show, Eq)


makeLenses ''ExportMap
makeLenses ''Module
