{-# LANGUAGE GADTs #-}

module Data.Ouro.Refinement.Value where

import           Data.Maybe                (isJust)
import qualified Data.Ouro.Internal.Expr   as I
import           Data.Ouro.Lisp.Eval.Types (Expr (..))
import qualified Data.Ouro.Lisp.Eval.Types as L
import qualified Data.Ouro.Refinement.Type as R
import           Data.Text                 (Text)


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
