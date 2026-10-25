module Data.Ouro.Lisp.Canon.Adjoin
( adjoinForms
) where

import           Data.List.NonEmpty            (NonEmpty (..), groupBy)
import           Data.Ouro.Lisp.Canon.Traverse (bottomUp)
import qualified Data.Ouro.Lisp.Surface        as S


-- adjoinForms.
--
-- (adjoin base item ...) is base with items added, the dual of with: where with sets a key,
-- adjoin adds to the key's set of values. Its items are grouped into the values they add, in
-- order, and %adjoin adds each in turn:
--
--   (adjoin base :k v :l w)    (%adjoin base (:k v :l w))     attribute pairs, with any
--                                                              context beside them, as a record
--   (adjoin base (inlay x))    (%adjoin base x)                an inlaid value as it is
--   (adjoin base e)            (%adjoin base (list e))         any other item, an element
--   (adjoin base)              base
--
-- Whether base is a record or an array is known only once it is evaluated, so %adjoin
-- decides: a record adds a record's attribute pairs, an array an array's elements, and
-- anything else is an error there. '%' cannot be written in Ouro, so the call cannot be
-- confused with a user's function. Runs after 'threading', so an adjoin step of (-> x ...)
-- has already received x as its base, and after 'contexts', so a context is (%context e).
adjoinForms :: S.Expr -> S.Expr
adjoinForms = bottomUp $ \case
    S.Form pos (S.Symbol sPos "adjoin" : base : items)
        -> case items of
               [] -> base
               _  -> S.Form pos (S.Symbol sPos "%adjoin" : base : map addition (groupBy (\a b -> isPair a && isPair b) items))
    other
        -> other

    where
    addition = \case
        S.Form _ [S.Symbol _ "inlay", inlaid] :| [] -> inlaid
        item :| [] | not (isPair item)            -> S.Form (S.exprPos item) [S.Symbol (S.exprPos item) "list", item]
        first :| rest                             -> S.Form (S.exprPos first) (first : rest)

    -- An attribute pair, or a context, which is written beside them
    isPair = \case
        S.Form _ (S.Symbol _ "%attr"    : _) -> True
        S.Form _ (S.Symbol _ "%context" : _) -> True
        _otherItem                           -> False
