module Data.Ouro.Lisp.Canon.Typing
( typing
) where

import qualified Data.Ouro.Lisp.Surface as S


-- typing.
--
-- ⊢ asserts and ∈ asks, each of a value and a refinement written by its name, in a type
-- position: the name is never evaluated, so no binding changes what it names. ⊢ is the
-- assertion the evaluator already reads, and ∈ the question satisfies asked:
--
--   (⊢ x string)         #string x                         the value, or a type error at x
--   <x ⊢ titled>         #titled x
--   (∈ x titled)         (%in x #titled)                   true or false
--   ((∈ titled) e)       ((%in #titled) e)                 a case branch's head
--   (fn ⊢ number (<n ⊢ number>) e)   (fn #number (#number n) e)   a fn's signature
--
-- %in is in, written so that no binding, of satisfies or any name, can capture it. Any other
-- shape is left as it is, for the evaluator to report as a misplaced assert or in. Runs after
-- 'infix', so <x ⊢ p> is already (⊢ x p); after 'threading', so a chain <x ⊢ p ⊢ q>, which
-- infix threads, is read; and after 'placeholders', so <? ∈ p> is already a function whose
-- body it reads.
typing :: S.Expr -> S.Expr
typing = \case
    S.Form _ [S.Symbol _ "assert", value, S.Symbol rPos name]
        -> S.Tagged rPos (S.refinementNamed name) (typing value)
    S.Form pos [S.Symbol _ "in", value, S.Symbol rPos name]
        -> S.Form pos [S.Symbol pos "%in", typing value, subjectless rPos name]
    S.Form pos [S.Symbol _ "in", S.Symbol rPos name]
        -> S.Form pos [S.Symbol pos "%in", subjectless rPos name]

    -- A fn's return type, (fn ⊢ number (n) body), as the fn reads #number before its
    -- parameters, (fn #number (n) body). Its parameters' binders are read as any (⊢ x p)
    S.Form pos (fnHead@(S.Symbol _ (S.formName -> "fn")) : S.Symbol _ "assert" : S.Symbol rPos name : params@(S.Form _ _) : body)
        -> typing (S.Form pos (fnHead : S.Tagged rPos (S.refinementNamed name) params : body))

    S.Form    pos items     -> S.Form    pos (map typing items)
    S.Tagged  pos tag inner -> S.Tagged  pos tag (typing inner)
    leaf                    -> leaf

    where
    -- A refinement on its own, as the parser reads #p at the end of a form
    subjectless rPos name = S.Tagged rPos (S.refinementNamed name) (S.Hole rPos "%subject")
