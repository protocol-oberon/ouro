module Data.Ouro.Lisp.Canon.Connectives
( connectives
) where

import qualified Data.Ouro.Lisp.Surface as S
import           Text.Megaparsec        (SourcePos)


-- connectives.
--
-- (and a b) is true when both operands are, and (or a b) when at least one is: ∧ and ∨.
-- (all (a b c)) is true when every element of an array is, and (any (a b c)) when one is:
-- ⋀ and ⋁, the n-ary and and or, since many values are an array. Each becomes a chain of
-- cases, so an operand is only evaluated when the ones before it have not already decided
-- the answer, and each operand is checked as a #boolean:
--
--   (and a b)        (case #boolean a (true #boolean b) (otherwise false))
--   (or a b)         (case #boolean a (true true) (otherwise #boolean b))
--   (all (a b c))    (case #boolean a (true (case #boolean b ...)) (otherwise false))
--   (all ())         true, and (any ()) is false
--
-- Only an array written in place is a chain, a literal one or a (list ...): it is what stops
-- the guard (all ((has r year) (gt (get r year) 1600))) reading a year r does not have. An
-- array given any other way is evaluated whole, and all and any read its Booleans. An and
-- or or with another number of operands is left for the evaluator to report.
--
-- Top-down, so the rest of a chain is rewritten in turn. Runs after 'threading', so in
-- (-> x (and y)) the step receives x as its first operand. A case's patterns and a fn's
-- parameters are matched, not evaluated, so they are left as they are.
connectives :: S.Expr -> S.Expr
connectives = \case
    S.Form pos [S.Symbol _ "and", a, b]
        -> connectives (chain pos True [a, b])
    S.Form pos [S.Symbol _ "or", a, b]
        -> connectives (chain pos False [a, b])
    S.Form pos [S.Symbol _ "all", array]
        | Just elems <- writtenArray array
        -> connectives (chain pos True elems)
    S.Form pos [S.Symbol _ "any", array]
        | Just elems <- writtenArray array
        -> connectives (chain pos False elems)

    S.Form pos (cSym@(S.Symbol _ (S.formName -> "case")) : target : branches)
        -> S.Form pos (cSym : connectives target : map branch branches)

    -- A fn's parameters are names and patterns, so only its body is read
    S.Form pos [fnSym@(S.Symbol _ (S.formName -> "fn")), params, body]
        -> S.Form pos [fnSym, params, connectives body]

    S.Form    pos items     -> S.Form    pos (map connectives items)
    S.Tagged  pos tag inner -> S.Tagged  pos tag (connectives inner)
    leaf                    -> leaf

    where
    -- Only a branch's body: its pattern is matched, not evaluated
    branch = \case
        S.Form bPos [pattern, body] -> S.Form bPos [pattern, connectives body]
        malformed                   -> malformed


-- The chain of cases for and and all, or for or and any when isAll is False
chain :: SourcePos -> Bool -> [S.Expr] -> S.Expr
chain pos isAll = \case
    []
        -> S.Literal pos (S.Bool isAll)
    [operand]
        -> asBool operand
    operand : rest
        -> S.Form pos [ S.Symbol pos "%case"
                      , asBool operand
                      , S.Form pos [S.Literal pos (S.Bool True), whenTrue]
                      , S.Form pos [S.Symbol pos "otherwise",    whenFalse]
                      ]
           where
           (whenTrue, whenFalse)
               | isAll     = (chain pos isAll rest, S.Literal pos (S.Bool False))
               | otherwise = (S.Literal pos (S.Bool True), chain pos isAll rest)

    where
    -- Each operand is checked where it is written
    asBool operand = S.Tagged (S.exprPos operand) S.BooleanTag operand


-- The elements of an array written in place: (), #array-empty, an (array ...), or a form
-- that is neither a call, with a name at its head, nor a record, with an attribute pair in it
writtenArray :: S.Expr -> Maybe [S.Expr]
writtenArray = \case
    S.Literal _ S.EmptyArr               -> Just []
    S.Form _ (S.Symbol _ "array" : elems) -> Just elems
    S.Form _ elems@(first : _)
        | not (isName first)
        , not (any S.isAttrForm elems)  -> Just elems
    _notWritten                         -> Nothing
    where
    isName = \case
        S.Symbol _ _ -> True
        _notAName    -> False
