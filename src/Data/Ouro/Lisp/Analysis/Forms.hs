-- What a form is and what it binds, read off the form alone: whether it is a call, a record or
-- an array, and the names a record or a let binds. The static analysis and the evaluator share
-- these, so the two cannot disagree about either (see the org note, "One scope, not two").
module Data.Ouro.Lisp.Analysis.Forms
( BlockTarget (..)
, FrameEntry (..)
, bindableAttr
, checkBindableName
, collectBindings
, determineBlockTarget
, letBindings
, reservedNames
) where

import           Control.Monad               (when)
import           Data.Function               ((&))
import           Data.Ouro.Error.Diagnostics (lexicalError, shadowedVariable,
                                              shadowedVariableBlurb, withBlurb)
import           Data.Ouro.Error.Types       (OuroError (..))
import           Data.Ouro.Lisp.Function     (bindingWithoutValue,
                                              isFunctionKeyword, keyInLet,
                                              misplacedBinding, removedDefine,
                                              removedLocalDefn)
import qualified Data.Ouro.Lisp.Module.Types as M
import qualified Data.Ouro.Lisp.Surface      as S
import           Data.Text                   (Text)
import           Text.Megaparsec             (SourcePos)


-- Data type representing the structural target resolved by lookahead routing.
data BlockTarget
    = TargetRecord
    | TargetList
    | TargetFunctionApp
    deriving (Show, Eq)

-- Inspects incoming form tokens to determine if they compose an Record or a List.
determineBlockTarget :: [S.Expr] -> BlockTarget
determineBlockTarget items =
    case items of
        []       -> TargetList
        (x : xs) -> case isFunctionApplication (x : xs) of
                        True -> TargetFunctionApp
                        False -> case any isStructuralAttr items of
                                         True  -> TargetRecord
                                         False -> TargetList

    where
    isFunctionApplication :: [S.Expr] -> Bool
    isFunctionApplication =
        \case
         (S.Symbol _ name : _) -> not (isFunctionKeyword name || name == "define")
         _NotaFunc             -> False

    isStructuralAttr :: S.Expr -> Bool
    isStructuralAttr =
        \case
         S.Form _ (S.Symbol _ "%attr"    : _) -> True
         S.Form _ (S.Symbol _ "%context" : _) -> True
         _other                               -> False


-- FrameEntry.
--
-- One binding: a Record's ':key value' attribute pair, a let binding, or a function argument.
-- Each entry becomes its own promise, so a Record with duplicate keys still outputs
-- every attribute pair, while lookups of the name see the first entry.
data FrameEntry = FrameEntry
    { entryKey     :: Text
    , entryPos     :: SourcePos
    , entryExpr    :: S.Expr
    , entryIsAttr  :: Bool       -- A Record attribute pair (part of its output), not a let binding or argument
    }


-- collectBindings.
--
-- Sweeps a Record's expressions for the attribute pairs it binds, in source order. Context
-- forms, malformed attributes and loose expressions bind nothing. 'define' and a local
-- 'defn' were replaced by 'let' and 'fn', and a private '$name value' binding belongs
-- in a let; all are reported where they appear.
collectBindings :: [S.Expr] -> Either OuroError [FrameEntry]
collectBindings = \case
    [] -> pure []

    S.Form _ (S.Symbol _ "context" : _) : xs
        -> collectBindings xs

    S.Form pos (S.Symbol _ "define" : _) : _
        -> Left (removedDefine pos)

    S.Form pos (S.Symbol _ keyword : _) : _ | isFunctionKeyword keyword
        -> Left (removedLocalDefn pos)

    S.Form pos [S.Symbol _ "%binder", S.Literal _ (S.Str name)] : _
        -> Left (misplacedBinding name pos)

    attrExpr : xs
        | Just (key, pos, valExpr) <- bindableAttr attrExpr
        -> do
           checkBindableName key pos
           (FrameEntry key pos valExpr True :) <$> collectBindings xs

    _unbound : xs
        -> collectBindings xs


-- letBindings.
--
-- The bindings of (let ($name value ...) expr): only '$name value' pairs. They are
-- not part of any output, and can refer to each other in any order.
letBindings :: [S.Expr] -> Either OuroError [FrameEntry]
letBindings = traverse $ \case
    S.Form pos [S.Symbol _ "%bind", S.Literal _ (S.Str name), valExpr]
        -> FrameEntry name pos valExpr False <$ checkBindableName name pos

    S.Form pos [S.Symbol _ "%bind", S.Literal _ (S.Str name)]
        -> Left (bindingWithoutValue name pos)

    S.Form pos (S.Symbol _ "%attr" : S.Literal _ (S.Str key) : _)
        -> Left (keyInLet key pos)

    binding
        -> lexicalError "A let binds private values with '$name value' pairs."
           & withBlurb "Perhaps write (let ($name value ...) expr)."
           & OuroError (S.exprPos binding)
           & Left


-- Special forms are recognised by their head symbol, so they cannot be bound as names, and
-- a '/' qualifies an imported name, so a binding cannot have one
checkBindableName :: Text -> SourcePos -> Either OuroError ()
checkBindableName key pos
    | Just _ <- M.splitQualified key
        = lexicalError ("The name '" <> key <> "' has a '/' in it.")
          & withBlurb "A '/' qualifies an imported name, as in base/core, so a binding cannot have one. Perhaps use '-' instead."
          & OuroError pos
          & Left
    | otherwise
        = when (key `elem` reservedNames) $
              shadowedVariable key
              & withBlurb (shadowedVariableBlurb key)
              & OuroError pos
              & Left

-- The removed forms, nth, attr, define and defun, are not reserved: their names shadow nothing,
-- and a call of one is still reported with its replacement, by its head symbol
reservedNames :: [Text]
reservedNames = ["list", "case", "get", "context", "inlay", "insert", "defn", "let", "fn"]


-- A ':key value' attribute pair that binds a name. Pairs whose value is itself a structural
-- form (a nested attribute or context) bind nothing.
bindableAttr :: S.Expr -> Maybe (Text, SourcePos, S.Expr)
bindableAttr = \case
    S.Form pos [S.Symbol _ "%attr", S.Literal _ (S.Str key), valExpr]
        | not (isStructuralExpr valExpr) -> Just (key, pos, valExpr)
    _notBindable                         -> Nothing

    where
    isStructuralExpr :: S.Expr -> Bool
    isStructuralExpr = \case
                        S.Form _ (S.Symbol _ "%attr"    : _) -> True
                        S.Form _ (S.Symbol _ "context"  : _) -> True
                        _otherForm                           -> False
