-- What a form is and what it binds, read off the form alone: whether it is a call, a record or
-- an array, and the names a record, a let, a parameter or a hole binds. The static analysis and
-- the evaluator share these, so the two cannot disagree about either (see the org note, "One
-- scope, not two").
module Data.Ouro.Lisp.Analysis.Forms
( BlockTarget (..)
, FrameEntry (..)
, bindableAttr
, checkBindableName
, collectBindings
, determineBlockTarget
, hasValidOtherwise
, holeName
, isRestHole
, isSubjectHole
, letBindings
, param
, shadowsForm
) where

import           Data.Function               ((&))
import           Data.Ouro.Error.Diagnostics (lexicalError, withBlurb)
import           Data.Ouro.Error.Types       (OuroError (..))
import           Data.Ouro.Lisp.Function     (bindingWithoutValue,
                                              isFunctionKeyword, keyInLet,
                                              misplacedBinding, patternParam,
                                              removedLocalDefn, specialForms)
import qualified Data.Ouro.Lisp.Module.Types as M
import qualified Data.Ouro.Lisp.Surface      as S
import           Data.Text                   (Text)
import qualified Data.Text                   as T
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
         (S.Symbol _ name : _) -> not (isFunctionKeyword name)
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
-- forms, malformed attributes and loose expressions bind nothing. A local definition was
-- replaced by a let-bound 'fn', and a private '$name value' binding belongs in a let;
-- both are reported where they appear.
collectBindings :: [S.Expr] -> Either OuroError [FrameEntry]
collectBindings = \case
    [] -> pure []

    S.Form _ (S.Symbol _ "context" : _) : xs
        -> collectBindings xs

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


-- A '/' qualifies an imported name, so a binding cannot have one. Any other name can be bound,
-- a special form's too: the binding shadows the form, as in Scheme (see 'shadowsForm')
checkBindableName :: Text -> SourcePos -> Either OuroError ()
checkBindableName key pos
    | Just _ <- M.splitQualified key
        = lexicalError ("The name '" <> key <> "' has a '/' in it.")
          & withBlurb "A '/' qualifies an imported name, as in base/core, so a binding cannot have one. Perhaps use '-' instead."
          & OuroError pos
          & Left
    | otherwise
        = Right ()


-- Whether a form is a call of a binding rather than a special form, given whether a name is
-- bound where the form is written. As in Scheme, a special form's name means the form unless a
-- binding around it has the name: an attribute key, a let binding, a parameter, a hole or a
-- function of the module. A head Canon wrote, such as %case, is never a binding's (see 'S.formName').
shadowsForm :: (Text -> Bool) -> [S.Expr] -> Bool
shadowsForm bound = \case
    S.Symbol _ name : _ -> name `elem` specialForms && bound name
    _notNamed           -> False


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


-- A function parameter is a name or a pattern, optionally typed: n, <n ⊢ number>, (?a ?b)
-- or <(?a ?b) ⊢ array>, or by the old tag before it, #number n. A pattern is named by its position among the parameters.
-- A '$name' is read as (%binder "name"), which is a binding, not a pattern.
param :: (Int, S.Expr) -> Maybe S.Param
param (index, expr) = case expr of
    S.Symbol _ name                        -> Just (S.Param name Nothing Nothing)
    S.Asserted pos ty (S.Symbol _ name)    -> Just (S.Param name (Just (pos, ty)) Nothing)
    S.Form _ (S.Symbol _ "%binder" : _)    -> Nothing
    pattern@(S.Form _ _)                   -> Just (patternParam index Nothing pattern)
    S.Asserted pos ty pattern@(S.Form _ _) -> Just (patternParam index (Just (pos, ty)) pattern)
    _notAParameter                         -> Nothing


-- The name a pattern's hole binds: '?' binds nothing; '?name' and '?..name' bind 'name'
holeName :: Text -> Maybe Text
holeName hole =
    case T.dropWhile (\c -> c == '?' || c == '.') hole of
        ""   -> Nothing
        name -> Just name

-- A hole that binds the rest of a Record or an Array
isRestHole :: Text -> Bool
isRestHole = T.isPrefixOf "?.."


hasValidOtherwise :: [S.Expr] -> Bool
hasValidOtherwise branches =
    case reverse branches of
        -- Check the very last branch in the list (the first element of the reversed list)
        (S.Form _ [S.Symbol _ "otherwise", _] : _) -> True
        _noOtherwise                               -> False


-- The subject of a refinement written on its own, (satisfies #p), which a case fills in
isSubjectHole :: S.Expr -> Bool
isSubjectHole = \case
    S.Hole _ "%subject" -> True
    _notTheSubject      -> False
