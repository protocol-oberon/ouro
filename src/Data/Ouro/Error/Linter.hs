{-# LANGUAGE GADTs #-}

module Data.Ouro.Error.Linter where

import           Data.Char                   (isDigit, isLower, isUpper, toLower)
import qualified Data.Map.Strict             as Map
import           Data.Ouro.Error.Diagnostics (deprecatedSyntax, notKebabCase,
                                              parenthesisedResult,
                                              procedureOfNoParameters,
                                              shadowedBuiltin,
                                              unanchoredLiteral)
import           Data.Ouro.Error.Types       (OuroWarning (..))
import           Data.Ouro.Lisp.Eval.Builtins (builtinRegistry)
import           Data.Ouro.Lisp.Function     (specialForms)
import qualified Data.Ouro.Lisp.Module.Types as M
import qualified Data.Ouro.Lisp.Surface      as S
import qualified Data.Ouro.Lisp.Tokens       as Tkn
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Lens.Micro                  ((^.))


-- Recursive Check tokens for dead layout patterns
lintExpression :: [Tkn.Token] -> [OuroWarning]
lintExpression =
    \case
     -- MATCH: UNANCHORED OBJECT LITERALS
     (Tkn.Token pos Tkn.TagRecordEmpty : Tkn.Token _ (Tkn.String val) : rest)
         -> OuroWarning pos (unanchoredLiteral "#record-empty" val) : lintExpression rest

     -- MATCH: UNANCHORED ARRAY LITERALS
     (Tkn.Token pos Tkn.TagArrayEmpty : Tkn.Token _ (Tkn.String val) : rest)
         -> OuroWarning pos (unanchoredLiteral "#array-empty" val) : lintExpression rest

     -- A define/procedure of no parameters, (define/procedure c () value) or with a return
     -- type, (define/procedure c #string () value), is a constant: define/constant says so.
     -- The parsed tree does not keep which keyword was written, so it is found in the tokens.
     (Tkn.Token pos Tkn.OpenParen : Tkn.Token _ Tkn.DefineProcedure : Tkn.Token _ (Tkn.Symbol name) : rest)
         | Tkn.Token _ Tkn.OpenParen : Tkn.Token _ Tkn.CloseParen : _ <- afterReturnType rest
         -> OuroWarning pos (procedureOfNoParameters name) : lintExpression rest

     -- Structural recursion to keep scanning down the token stream
     (_ : rest) -> lintExpression rest
     []         -> []

    where
    -- The tokens after a definition's return type, which is one property tag, if it has one
    afterReturnType = \case
        Tkn.Token _ tag : rest
            | isReturnType tag -> rest
        tokens                 -> tokens

    isReturnType = \case
        Tkn.TagUri          -> True
        Tkn.TagDate         -> True
        Tkn.TagString       -> True
        Tkn.TagNumber       -> True
        Tkn.TagBoolean      -> True
        Tkn.TagRecord       -> True
        Tkn.TagArray        -> True
        Tkn.TagFunction     -> True
        Tkn.TagProperty _   -> True
        _notAType           -> False


-- lintModule.
--
-- Checks the parsed functions and graphs. A function's result is one expression, and
-- parentheses around a single plain value make it an array: (fn () ("hello")) returns
-- ["hello"], which is rarely what was meant. A function named after a built-in or a special form
-- replaces it, which is allowed, as in Scheme, but worth saying. Functions and graphs are named in
-- kebab-case.
lintModule :: M.Module -> [OuroWarning]
lintModule ouroModule =
       concat [ lintResult name body <> lintFns body
              | M.Function _ name _ _ (S.Form _ [body]) <- Map.elems (ouroModule ^. M.functionRegistry) ]
    <> concat [ lintFns expr
              | M.Graph _ _ expr <- Map.elems (ouroModule ^. M.graphRegistry) ]
    <> [ OuroWarning pos (shadowedBuiltin name)
       | M.Function pos name _ _ _ <- Map.elems (ouroModule ^. M.functionRegistry)
       , Map.member name builtinRegistry || name `elem` specialForms ]
    <> [ OuroWarning pos (notKebabCase "function" name kebab)
       | M.Function pos name _ _ _ <- Map.elems (ouroModule ^. M.functionRegistry)
       , Just kebab <- [kebabCase name] ]
    <> [ OuroWarning pos (notKebabCase "graph" name kebab)
       | M.Graph pos name _ <- Map.elems (ouroModule ^. M.graphRegistry)
       , Just kebab <- [kebabCase name] ]
    <> case Map.member "list" (ouroModule ^. M.functionRegistry) of
           True  -> []
           False -> concat [ oldNames body | M.Function _ _ _ _ body <- Map.elems (ouroModule ^. M.functionRegistry) ]
                 <> concat [ oldNames expr | M.Graph _ _ expr <- Map.elems (ouroModule ^. M.graphRegistry) ]


-- Every (list ...), the Array constructor's old name, which is now array (gap 55). A module
-- that defines its own list calls it, so it is not warned about.
oldNames :: S.Expr -> [OuroWarning]
oldNames = \case
    S.Form _ (S.Symbol pos "list" : rest) -> OuroWarning pos (deprecatedSyntax "list" "array") : concatMap oldNames rest
    S.Form    _ exprs                     -> concatMap oldNames exprs
    S.Tagged  _ _ expr                    -> oldNames expr
    _leaf                                 -> []


-- The kebab-case spelling of a name with capitals or underscores, or Nothing when it has
-- neither. A capital after a lowercase letter or a digit starts a new word, so makePlace
-- and make_place are both make-place.
kebabCase :: Text -> Maybe Text
kebabCase name
    | T.any (\c -> isUpper c || c == '_') name = Just (T.pack (go ' ' (T.unpack name)))
    | otherwise                                = Nothing
    where
    go _ [] = []
    go prev (c : cs)
        | c == '_'                                = '-' : go c cs
        | isUpper c, isLower prev || isDigit prev = '-' : toLower c : go c cs
        | otherwise                               = toLower c : go c cs


-- Finds every (fn (args) expr) and checks its result
lintFns :: S.Expr -> [OuroWarning]
lintFns = \case
    S.Form _ [S.Symbol _ "fn", S.Form _ _, body] -> lintResult "fn" body <> lintFns body
    S.Form    _ exprs                            -> concatMap lintFns exprs
    S.Tagged  _ _ expr                           -> lintFns expr
    _leaf                                        -> []


lintResult :: Text -> S.Expr -> [OuroWarning]
lintResult owner = \case
    S.Form pos [S.Literal _ lit]
        | Just val <- plainValue lit -> [OuroWarning pos (parenthesisedResult owner val)]
    _otherResult                     -> []

    where
    plainValue :: S.LiteralValue -> Maybe Text
    plainValue = \case
        S.Str  t     -> Just ("\"" <> t <> "\"")
        S.Num  n     -> Just (renderNum n)
        S.Bool True  -> Just "true"
        S.Bool False -> Just "false"
        _emptyTag    -> Nothing

    renderNum :: Double -> Text
    renderNum n
        | n == fromInteger (round n) = T.pack (show (round n :: Integer))
        | otherwise                  = T.pack (show n)
