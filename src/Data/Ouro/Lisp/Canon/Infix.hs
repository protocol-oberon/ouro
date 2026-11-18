module Data.Ouro.Lisp.Canon.Infix
( infixForms
) where

import qualified Data.Ouro.Lisp.Surface as S


-- infixForms.
--
-- <a op b> is (op a b), as SRFI 105's {a op b} is, read by the parser into (%infix a op b)
-- with its shape already checked: an odd number of elements, at least three, every second
-- one the same name, which chains only if a left fold means something (see the parser's
-- 'infixForm'). A chain folds left by threading, so 'threading' writes it out:
--
--   <a lt b>             (lt a b)
--   <a + b + c>          (-> a (+ b) (+ c))         that is (+ (+ a b) c)
--   <a lt <b + 1>>       (lt a (+ b 1))             nested by the brackets, inner first
--   <? gt 1600>          (gt ? 1600)                a function of its hole, by 'placeholders'
--
-- Runs first, top-down, so every later pass sees prefix forms only. The outer form is at
-- the '<', and each operator keeps its own position, which an arity error is reported at.
infixForms :: S.Expr -> S.Expr
infixForms = \case
    S.Form pos (S.Symbol _ "%infix" : first : op : second : rest)
        -> infixForms $ case rest of
               []     -> S.Form pos [op, first, second]
               _chain -> S.Form pos (S.Symbol pos "->" : first : steps (op : second : rest))

    S.Form    pos items     -> S.Form    pos (map infixForms items)
    S.Tagged  pos tag inner -> S.Tagged  pos tag (infixForms inner)
    leaf                    -> leaf

    where
    -- Each operator with the operand after it, a step of the threading
    steps = \case
        op : operand : more -> S.Form (S.exprPos op) [op, operand] : steps more
        _rest               -> []
