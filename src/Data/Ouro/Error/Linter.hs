{-# LANGUAGE GADTs #-}

module Data.Ouro.Error.Linter where

import           Data.Char                   (isDigit, isLower, isUpper, toLower)
import qualified Data.Map.Strict             as Map
import           Data.Ouro.Error.Diagnostics (notKebabCase, parenthesisedResult,
                                              procedureOfNoParameters,
                                              shadowedBuiltin,
                                              undocumentedRefinement)
import           Data.Ouro.Error.Types       (OuroWarning (..))
import           Data.Ouro.Lisp.Eval.Builtins (builtinRegistry)
import           Data.Ouro.Lisp.Function     (specialForms)
import qualified Data.Ouro.Lisp.Module.Types as M
import qualified Data.Ouro.Lisp.Surface      as S
import qualified Data.Ouro.Lisp.Tokens       as Tkn
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Lens.Micro                  ((^.))


-- Checks the tokens, for what the parsed tree does not keep. (WRN0000, a string after
-- #record-empty or #array-empty, was read here, and retired: gap 41.)
lintExpression :: [Tkn.Token] -> [OuroWarning]
lintExpression =
    \case
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
    -- The tokens after a definition's return type, ⊢ and a refinement's name or one refinement
    -- tag, if it has one
    afterReturnType = \case
        Tkn.Token _ (Tkn.Symbol "assert") : Tkn.Token _ (Tkn.Symbol _) : rest
                               -> rest
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
        Tkn.TagRefinement _ -> True
        _notAType           -> False


-- lintModule.
--
-- Checks the parsed functions and graphs. A function's result is one expression, and
-- parentheses around a single plain value make it an array: (fn () ("hello")) returns
-- ["hello"], which is rarely what was meant, unless a return type says what is meant. A
-- function named after a built-in or a special form
-- replaces it, which is allowed, as in Scheme, but worth saying. Functions and graphs are named in
-- kebab-case. A refinement has a doc, the rule a value that fails it is reported with.
lintModule :: M.Module -> [OuroWarning]
lintModule ouroModule =
       concat [ maybe (lintResult name body) (const []) resultType <> lintFns body
              | M.SomeFunction (M.Function _ _ name _ _ resultType (S.Form _ [body])) <- Map.elems (ouroModule ^. M.functionRegistry) ]
    <> concat [ lintFns expr
              | M.Graph _ _ expr <- Map.elems (ouroModule ^. M.graphRegistry) ]
    <> [ OuroWarning pos (shadowedBuiltin (kindName f) name)
       | f@(M.SomeFunction (M.Function _ pos name _ _ _ _)) <- Map.elems (ouroModule ^. M.functionRegistry)
       , Map.member name builtinRegistry || name `elem` specialForms ]
    <> [ OuroWarning pos (notKebabCase (kindName f) name kebab)
       | f@(M.SomeFunction (M.Function _ pos name _ _ _ _)) <- Map.elems (ouroModule ^. M.functionRegistry)
       , Just kebab <- [kebabCase name] ]
    <> [ OuroWarning pos (notKebabCase "graph" name kebab)
       | M.Graph pos name _ <- Map.elems (ouroModule ^. M.graphRegistry)
       , Just kebab <- [kebabCase name] ]
    <> [ OuroWarning pos (undocumentedRefinement name)
       | M.SomeFunction (M.Function M.RefinementKind pos name Nothing _ _ _) <- Map.elems (ouroModule ^. M.functionRegistry) ]


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
    S.Form     _ exprs                           -> concatMap lintFns exprs
    S.Asserted _ _ expr                          -> lintFns expr
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


-- What a function is called in a warning: a refinement, or a function
kindName :: M.SomeFunction -> Text
kindName = \case
    M.SomeFunction (M.Function M.RefinementKind _ _ _ _ _ _) -> "refinement"
    M.SomeFunction (M.Function M.ProcedureKind _ _ _ _ _ _)  -> "function"
