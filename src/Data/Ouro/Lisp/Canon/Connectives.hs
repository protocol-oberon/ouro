module Data.Ouro.Lisp.Canon.Connectives
( connectives
) where

import qualified Data.Ouro.Lisp.Surface as S
import           Text.Megaparsec        (SourcePos)


-- connectives.
--
-- (all a ...) is true when every operand is, and (some a ...) when at least one is. Each
-- becomes a chain of cases, so an operand is only evaluated when the ones before it have
-- not already decided the answer, and each operand is checked as a #bool:
--
--   (all a b c)    (case #bool a (true (all b c)) (otherwise false))
--   (some a b c)   (case #bool a (true true) (otherwise (some b c)))
--   (all a)        #bool a
--   (all)          true, and (some) is false
--
-- Top-down, so the rest of a chain is rewritten in turn. Runs after 'threading', so in
-- (-> x (all y)) the step receives x as its first operand. A case's patterns and a fn's
-- parameters are matched, not evaluated, so they are left as they are.
connectives :: S.Expr -> S.Expr
connectives = \case
    S.Form pos (S.Symbol _ "all" : operands)
        -> connectives (chain pos True operands)
    S.Form pos (S.Symbol _ "some" : operands)
        -> connectives (chain pos False operands)

    S.Form pos (cSym@(S.Symbol _ "case") : target : branches)
        -> S.Form pos (cSym : connectives target : map branch branches)

    -- A fn's parameters are names and patterns, so only its body is read
    S.Form pos [fnSym@(S.Symbol _ "fn"), params, body]
        -> S.Form pos [fnSym, params, connectives body]

    S.Form    pos items     -> S.Form    pos (map connectives items)
    S.Bracket pos items     -> S.Bracket pos (map connectives items)
    S.Tagged  pos tag inner -> S.Tagged  pos tag (connectives inner)
    leaf                    -> leaf

    where
    -- Only a branch's body: its pattern is matched, not evaluated
    branch = \case
        S.Form bPos [pattern, body] -> S.Form bPos [pattern, connectives body]
        malformed                   -> malformed


-- The chain of cases for (all ...), or for (some ...) when isAll is False
chain :: SourcePos -> Bool -> [S.Expr] -> S.Expr
chain pos isAll = \case
    []
        -> S.Literal pos (S.Bool isAll)
    [operand]
        -> asBool operand
    operand : rest
        -> S.Form pos [ S.Symbol pos "case"
                      , asBool operand
                      , S.Form pos [S.Literal pos (S.Bool True), whenTrue]
                      , S.Form pos [S.Symbol pos "otherwise",    whenFalse]
                      ]
           where
           restOf name     = S.Form pos (S.Symbol pos name : rest)
           (whenTrue, whenFalse)
               | isAll     = (restOf "all", S.Literal pos (S.Bool False))
               | otherwise = (S.Literal pos (S.Bool True), restOf "some")

    where
    -- Each operand is checked where it is written
    asBool operand = S.Tagged (S.exprPos operand) S.BoolTag operand
