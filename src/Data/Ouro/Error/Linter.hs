{-# LANGUAGE GADTs #-}

module Data.Ouro.Error.Linter where

import           Data.Char                   (isDigit, isLower, isUpper, toLower)
import qualified Data.Map.Strict             as Map
import           Data.Ouro.Error.Diagnostics (notKebabCase,
                                              parenthesisedResult,
                                              shadowedBuiltin,
                                              unanchoredLiteral)
import           Data.Ouro.Error.Types       (OuroWarning (..))
import           Data.Ouro.Lisp.Eval.Builtins (builtinRegistry)
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
         -> OuroWarning pos (unanchoredLiteral "#obj-empty" val) : lintExpression rest

     -- MATCH: UNANCHORED ARRAY LITERALS
     (Tkn.Token pos Tkn.TagArrEmpty : Tkn.Token _ (Tkn.String val) : rest)
         -> OuroWarning pos (unanchoredLiteral "#arr-empty" val) : lintExpression rest

     -- Structural recursion to keep scanning down the token stream
     (_ : rest) -> lintExpression rest
     []         -> []


-- lintModule.
--
-- Checks the parsed functions and graphs. A function's result is one expression, and
-- parentheses around a single plain value make it an array: (fn () ("hello")) returns
-- ["hello"], which is rarely what was meant. A defn named after a built-in replaces it,
-- which is allowed, as in Scheme, but worth saying. Functions and graphs are named in
-- kebab-case.
lintModule :: M.Module -> [OuroWarning]
lintModule ouroModule =
       concat [ lintResult name body <> lintFns body
              | M.Function _ name _ _ (S.Form _ [body]) <- Map.elems (ouroModule ^. M.functionRegistry) ]
    <> concat [ lintFns expr
              | M.Graph _ _ expr <- Map.elems (ouroModule ^. M.graphRegistry) ]
    <> [ OuroWarning pos (shadowedBuiltin name)
       | M.Function pos name _ _ _ <- Map.elems (ouroModule ^. M.functionRegistry)
       , Map.member name builtinRegistry ]
    <> [ OuroWarning pos (notKebabCase "function" name kebab)
       | M.Function pos name _ _ _ <- Map.elems (ouroModule ^. M.functionRegistry)
       , Just kebab <- [kebabCase name] ]
    <> [ OuroWarning pos (notKebabCase "graph" name kebab)
       | M.Graph pos name _ <- Map.elems (ouroModule ^. M.graphRegistry)
       , Just kebab <- [kebabCase name] ]


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
    S.Bracket _ exprs                            -> concatMap lintFns exprs
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
        S.Null       -> Just "null"
        _emptyTag    -> Nothing

    renderNum :: Double -> Text
    renderNum n
        | n == fromInteger (round n) = T.pack (show (round n :: Integer))
        | otherwise                  = T.pack (show n)
