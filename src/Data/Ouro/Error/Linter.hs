{-# LANGUAGE GADTs #-}

module Data.Ouro.Error.Linter where

import           Data.Char                   (isDigit, isLower, isUpper, toLower)
import qualified Data.Map.Strict             as Map
import           Data.Ouro.Error.Diagnostics (deprecatedSyntax, emptyRecordType, notKebabCase,
                                              parenthesisedResult,
                                              procedureOfNoParameters,
                                              refinementOverValue,
                                              shadowedBuiltin,
                                              tripleQuotedString,
                                              undocumentedRefinement)
import           Data.Ouro.Error.Types       (OuroWarning (..))
import           Data.Ouro.Lisp.Eval.Builtins (builtinRegistry)
import           Data.Ouro.Lisp.Function     (specialForms)
import qualified Data.Ouro.Lisp.Module.Types as M
import qualified Data.Ouro.Lisp.Surface      as S
import qualified Data.Ouro.Lisp.Tokens       as Tkn
import qualified Data.Ouro.Refinement.Type   as R
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Lens.Micro                  ((^.))
import           Text.Megaparsec             (SourcePos (sourceColumn, sourceLine), unPos)


-- Checks the tokens, for what the parsed tree does not keep. (WRN0000, a string after the old
-- hash-marked empty record or array, was read here, and retired: gap 41.)
lintExpression :: [Tkn.Token] -> [OuroWarning]
lintExpression =
    \case
     -- A define/procedure of no parameters, (define/procedure c () value) or with a return
     -- type, (define/procedure c ⊢ string () value), is a constant: define/constant says so.
     -- The parsed tree does not keep which keyword was written, so it is found in the tokens.
     (Tkn.Token pos Tkn.OpenParen : more@(Tkn.Token _ Tkn.DefineProcedure : Tkn.Token _ (Tkn.Symbol name) : rest))
         | Tkn.Token _ Tkn.OpenParen : Tkn.Token _ Tkn.CloseParen : _ <- afterReturnType rest
         -> OuroWarning pos (procedureOfNoParameters name) : lintExpression more

     -- graph, the old name of document: it still works, with a warning that names document
     (Tkn.Token pos Tkn.RetiredGraph : rest)
         -> OuroWarning pos (deprecatedSyntax "graph" "document") : lintExpression rest

     -- The old hash-marked spellings still work until 0.8.0.0, each with a warning that names
     -- what replaces it. A refinement's, written as a definition's return type, is now ⊢ p
     (Tkn.Token _ definer : Tkn.Token _ (Tkn.Symbol _) : Tkn.Token pos (Tkn.RetiredRefinement p) : rest)
         | definer `elem` [Tkn.DefineProcedure, Tkn.DefineConstant]
         -> OuroWarning pos (deprecatedSyntax ("#" <> p) ("⊢ " <> p)) : lintExpression rest
     (Tkn.Token _ (Tkn.Symbol "fn") : Tkn.Token pos (Tkn.RetiredRefinement p) : rest)
         -> OuroWarning pos (deprecatedSyntax ("#" <> p) ("⊢ " <> p)) : lintExpression rest

     -- A refinement at the end of a form is satisfies's, which asks: a case head's
     -- (satisfies #p) is now (∈ p), and (satisfies x #p) is <x ∈ p>
     (Tkn.Token pos Tkn.OpenParen : Tkn.Token _ (Tkn.Symbol "satisfies") : Tkn.Token _ (Tkn.RetiredRefinement p)
        : rest@(Tkn.Token _ Tkn.CloseParen : _))
         -> OuroWarning pos (deprecatedSyntax ("(satisfies #" <> p <> ")") ("(∈ " <> p <> ")")) : lintExpression rest
     (Tkn.Token pos (Tkn.RetiredRefinement p) : rest@(Tkn.Token _ Tkn.CloseParen : _))
         -> OuroWarning pos (deprecatedSyntax ("(satisfies x #" <> p <> ")") ("<x ∈ " <> p <> ">")) : lintExpression rest

     -- Anywhere else it asserts, as <x ⊢ p> does, of a value or a parameter
     (Tkn.Token pos (Tkn.RetiredRefinement p) : rest)
         -> OuroWarning pos (deprecatedSyntax ("#" <> p) ("<x ⊢ " <> p <> ">")) : lintExpression rest

     (Tkn.Token pos Tkn.RetiredEmptyRecord : rest)
         -> OuroWarning pos (deprecatedSyntax "#record-empty" "(make record)") : lintExpression rest
     (Tkn.Token pos Tkn.RetiredEmptyArray : rest)
         -> OuroWarning pos (deprecatedSyntax "#array-empty" "(make array)") : lintExpression rest

     -- """Spring""": an empty string, then a string opening where it closed, two columns on,
     -- which only a triple quote writes; "" "x", written apart, is two strings
     (Tkn.Token pos (Tkn.String "") : rest@(Tkn.Token next (Tkn.String _) : more))
         | sourceLine next == sourceLine pos
         , unPos (sourceColumn next) == unPos (sourceColumn pos) + 2
         -> OuroWarning pos tripleQuotedString : lintExpression more
         | otherwise
         -> lintExpression rest

     -- Structural recursion to keep scanning down the token stream
     (_ : rest) -> lintExpression rest
     []         -> []

    where
    -- The tokens after a definition's return type, ⊢ and a refinement's name, or a refinement in
    -- its old spelling, if it has one
    afterReturnType = \case
        Tkn.Token _ (Tkn.Symbol "assert") : Tkn.Token _ (Tkn.Symbol _) : rest
                                                   -> rest
        Tkn.Token _ (Tkn.RetiredRefinement _) : rest -> rest
        tokens                                     -> tokens


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
    <> [ OuroWarning pos (notKebabCase "document" name kebab)
       | M.Graph pos name _ <- Map.elems (ouroModule ^. M.graphRegistry)
       , Just kebab <- [kebabCase name] ]
    <> [ OuroWarning pos (undocumentedRefinement name)
       | M.SomeFunction (M.Function M.RefinementKind pos name Nothing _ _ _) <- Map.elems (ouroModule ^. M.functionRegistry) ]
    <> concat [ concatMap lintShifts bodyExprs
              | M.SomeFunction (M.Function _ _ _ _ _ _ (S.Form _ bodyExprs)) <- Map.elems (ouroModule ^. M.functionRegistry) ]
    <> concat [ lintShifts expr
              | M.Graph _ _ expr <- Map.elems (ouroModule ^. M.graphRegistry) ]


-- The old spellings of a shift, each warned of with the operation that replaces it: + and -
-- of a date and a duration, written with a unit, (+ d (days 1)), or with thru; thru, which
-- only a shift through the end of a unit meant; and the shift whose duration's sign said its
-- direction. Read before Canon, which rewrites thru away.
lintShifts :: S.Expr -> [OuroWarning]
lintShifts = \case
    S.Form _ [S.Symbol pos op, a, b]
        | op `elem` ["+", "-"], Just replacement <- shiftWritten op a b
        -> OuroWarning pos (deprecatedSyntax ("(" <> op <> " date duration)") replacement) : concatMap operand [a, b]
    S.Form _ [S.Symbol pos "shift", date, duration]
        -> OuroWarning pos (deprecatedSyntax "shift" (shiftReplacement duration)) : lintShifts date <> operand duration
    S.Form _ (S.Symbol pos "thru" : rest)
        -> OuroWarning pos (deprecatedSyntax "thru" "shift/forward-through or shift/backward-through") : concatMap lintShifts rest
    S.Form _ items
        -> concatMap lintShifts items
    S.Asserted _ _ inner
        -> lintShifts inner
    _leaf
        -> []

    where
    -- What + or - of a unit's duration, or of a thru, is now written
    shiftWritten op a b = case (any isThru [a, b], any isUnit [a, b], op) of
        (True, _, "+")  -> Just "shift/forward-through"
        (True, _, _)    -> Just "shift/backward-through"
        (_, True, "+")  -> Just "shift/forward"
        (_, True, _)    -> Just "shift/backward"
        _notAShift      -> Nothing

    shiftReplacement duration = case isThru duration of
        True  -> "shift/forward-through"
        False -> "shift/forward or shift/backward"

    -- An operand already warned of with its shift: a thru's own warning is not repeated
    operand = \case
        S.Form _ (S.Symbol _ "thru" : rest) -> concatMap lintShifts rest
        other                               -> lintShifts other

    isThru = \case
        S.Form _ (S.Symbol _ "thru" : _) -> True
        _other                           -> False

    isUnit = \case
        S.Form _ [S.Symbol _ unit, _] -> unit `elem` ["years", "months", "days", "hours", "minutes", "seconds"]
        _other                        -> False


-- lintTypes.
--
-- Checks the types written in the module after Canon, when an assertion in an expression,
-- <x ⊢ (record)>, and a fn's binder are typed nodes too: every parameter's and result's type,
-- and every assertion in a function's or a graph's body. (record), a record type with no
-- keys, is every record, which record already says. And a refinement declared over value,
-- <x ⊢ value>, says nothing about its subject.
lintTypes :: M.Module -> [OuroWarning]
lintTypes ouroModule =
       concat [ concatMap paramType params <> resultType result <> assertedTypes body
              | M.SomeFunction (M.Function _ _ _ _ params result body) <- Map.elems (ouroModule ^. M.functionRegistry) ]
    <> concatMap overValueDomain (Map.elems (ouroModule ^. M.functionRegistry))
    <> concat [ assertedTypes expr
              | M.Graph _ _ expr <- Map.elems (ouroModule ^. M.graphRegistry) ]
    where
    -- A refinement whose subject, its first parameter, is declared over value
    overValueDomain :: M.SomeFunction -> [OuroWarning]
    overValueDomain = \case
        M.SomeFunction (M.Function M.RefinementKind _ name _ (S.Param _ (Just (pos, R.Named R.RValue)) _ : _) _ _)
            -> [OuroWarning pos (refinementOverValue name)]

        _otherFunction
            -> []

    paramType = \case
        S.Param _ (Just (pos, ty)) _ -> emptyRecords pos ty
        _untyped                     -> []

    resultType = \case
        Just (pos, ty) -> emptyRecords pos ty
        Nothing        -> []


-- Every assertion in an expression, however deep: a fn's binders are among them after Canon
assertedTypes :: S.Expr -> [OuroWarning]
assertedTypes = \case
    S.Asserted pos ty inner -> emptyRecords pos ty <> assertedTypes inner
    S.Form     _ items      -> concatMap assertedTypes items
    _leaf                   -> []


-- (record) wherever a type holds it: on its own, in ∪ ∩ ∖, in an array, or at another
-- record type's key
emptyRecords :: SourcePos -> R.Type -> [OuroWarning]
emptyRecords pos = \case
    R.RecordOf []    -> [OuroWarning pos emptyRecordType]
    R.RecordOf pairs -> concatMap (emptyRecords pos . snd) pairs
    R.ArrayOf  t     -> emptyRecords pos t
    R.Union    a b   -> emptyRecords pos a <> emptyRecords pos b
    R.Inter    a b   -> emptyRecords pos a <> emptyRecords pos b
    R.Minus    a b   -> emptyRecords pos a <> emptyRecords pos b
    R.Named    _     -> []


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
        _emptyValue  -> Nothing

    renderNum :: Double -> Text
    renderNum n
        | n == fromInteger (round n) = T.pack (show (round n :: Integer))
        | otherwise                  = T.pack (show n)


-- What a function is called in a warning: by its kind, a refinement or a procedure
kindName :: M.SomeFunction -> Text
kindName = \case
    M.SomeFunction (M.Function M.RefinementKind _ _ _ _ _ _) -> "refinement"
    M.SomeFunction (M.Function M.ProcedureKind _ _ _ _ _ _)  -> "procedure"
