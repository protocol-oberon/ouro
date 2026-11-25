{-# LANGUAGE GADTs #-}

module Data.Ouro.Refinement.Value where

import           Data.Maybe                (isJust)
import qualified Data.Ouro.Internal.Expr   as I
import           Data.Ouro.Lisp.Eval.Types (Expr (..))
import qualified Data.Ouro.Lisp.Eval.Types as L
import qualified Data.Ouro.Refinement.Type as R
import           Data.Text                 (Text)
import qualified Data.Text                 as T


-- The built-in refinement a value has, written as in Ouro, for messages: "string", "record". A
-- value with no built-in refinement, such as null or a Duration, is described instead.
refinementOf :: Expr -> Text
refinementOf =
    \case
     Primitive p
         -> case p of
                I.String    _    -> "string"
                I.Number    _    -> "number"
                I.Boolean   _    -> "boolean"
                I.URI       _    -> "uri"
                I.Date      _    -> "date"
                I.EmptyRec       -> "record"
                I.Record    _ _  -> "record"
                I.EmptyArr       -> "array"
                I.Array     _    -> "array"
                I.Null           -> "null"
                I.BlankNode _    -> "a blank node"
     Record      _ _        -> "record"
     Array       _          -> "array"
     FunctionClosure {}     -> "function"
     PrimitiveOp _          -> "function"
     Duration    _ _        -> "a Duration"
     Attr        _ _        -> "an attribute pair"
     EvalError   _          -> "an error"


-- A value as a reader would name it, with its article: "an array", "a record", "a uri". A
-- description that has its own article already, "a blank node", is kept as it is.
aValue :: Expr -> Text
aValue value = case refinementOf value of
    described | any (`T.isPrefixOf` described) ["a ", "an "] -> described
    vowel     | T.take 1 vowel `elem` ["a", "e", "i", "o"]   -> "an " <> vowel
    kind                                                       -> "a " <> kind


-- The built-in refinement a value is of, its kind, as data: what an error's code says was given.
-- Nothing for what has no kind of its own, a duration, an attribute pair or an error.
kindOf :: Expr -> Maybe R.Refinement
kindOf =
    \case
     Primitive p
         -> case p of
                I.String    _    -> Just R.RString
                I.Number    _    -> Just R.RNumber
                I.Boolean   _    -> Just R.RBoolean
                I.URI       _    -> Just R.RUri
                I.Date      _    -> Just R.RDate
                I.EmptyRec       -> Just R.RRecord
                I.Record    _ _  -> Just R.RRecord
                I.EmptyArr       -> Just R.RArray
                I.Array     _    -> Just R.RArray
                I.Null           -> Nothing
                I.BlankNode _    -> Nothing
     Record      _ _        -> Just R.RRecord
     Array       _          -> Just R.RArray
     FunctionClosure {}     -> Just R.RFunction
     PrimitiveOp _          -> Just R.RFunction
     Duration    _ _        -> Nothing
     Attr        _ _        -> Nothing
     EvalError   _          -> Nothing


-- Whether a value is of a built-in kind, a refinement checked by the value's shape alone:
-- string, number, boolean, record, array or function. Any other refinement is no kind.
hasKind :: R.Refinement -> Expr -> Bool
hasKind refinement value = case (refinement, value) of
    (R.RString,   Primitive (I.String _))           -> True
    (R.RNumber,   Primitive (I.Number _))           -> True
    (R.RBoolean,  Primitive (I.Boolean _))          -> True
    (R.RRecord,   _) | isJust (L.asRecord value)    -> True
    (R.RArray,    _) | isJust (L.asArray value)     -> True
    (R.RFunction, FunctionClosure {})               -> True
    (R.RFunction, PrimitiveOp _)                    -> True
    _notOfTheKind                                   -> False
