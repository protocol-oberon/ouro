{-# LANGUAGE GADTs #-}

module Data.Ouro.Refinement.Value where

import           Data.Maybe                  (isJust)
import qualified Data.Ouro.Internal.Expr     as I
import           Data.Ouro.Lisp.Eval.Types   (Expr (..))
import qualified Data.Ouro.Lisp.Eval.Types   as L
import qualified Data.Ouro.Lisp.Module.Types as M
import qualified Data.Ouro.Refinement.Type   as R
import           Data.Text                   (Text)
import qualified Data.Text                   as T


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
     FunctionClosure _ kind _ _ _ _
         -> case kind of
                M.Procedure  -> Just R.RProcedure
                M.Refinement -> Just R.RRefinement
     PrimitiveOp _          -> Just R.RProcedure
     Duration    _ _        -> Nothing
     Attr        _ _        -> Nothing
     EvalError   _          -> Nothing


-- The built-in refinements as the tree they form, each with what it holds or an example of it,
-- each node's set within its parent's. The tree a message draws, and grows with the language.
data Node = Node R.Refinement Text [Node]

builtIns :: Node
builtIns =
    Node R.RValue "every value Ouro has"
        [ Node R.RData "what can be written to JSON-LD"
              [ Node R.RString "\"text\""
                    [ Node R.RUri  "\"https://linked.art/example/1\"" []
                    , Node R.RDate "\"1642-01-01T00:00:00Z\""         []
                    , Node R.RDuration "\"P19D\", \"PT3H\""           []
                    ]
              , Node R.RNumber  "1, 2.5"                []
              , Node R.RBoolean "true, false"           []
              , Node R.RRecord  "(:k v), (make record)" []
              , Node R.RArray   "(1 2 3), (make array)" []
              ]
        , Node R.RFunction "what can be called"
              [ Node R.RProcedure  "builds data: define/procedure, fn, the built-ins" []
              , Node R.RRefinement "judges data: define/refinement"                   []
              ]
        ]


-- The tree drawn for a value outside a built-in refinement, the refinement required and the
-- value's own kind marked, so that a reader sees where each sits. Nothing when the two are kinds
-- of data side by side, a string required and a number given, where the lines saying what was
-- required and given already say all the tree would.
refinementTree :: R.Refinement -> Expr -> Maybe Text
refinementTree required value = case (parentOf required, given >>= parentOf) of
    (Just R.RData, Just R.RData) -> Nothing
    _otherwise                   -> Just ("Ouro's built-in refinements, each within the one above it:\n\n" <> T.intercalate "\n" (draw "" "" builtIns))

    where
    given = kindOf value

    -- A node's row, then its children's, each under the branch that leads to it
    draw here below (Node refinement what children) =
        row (here <> R.renderRefinement refinement) what refinement
        : concat (zipWith (child below (length children)) [1 :: Int ..] children)

    child below count i node = case i == count of
        True  -> draw (below <> "└── ") (below <> "    ") node
        False -> draw (below <> "├── ") (below <> "│   ") node

    -- Indented by two spaces, so that it is printed as it is drawn
    row name what refinement = T.stripEnd ("  " <> T.justifyLeft 26 ' ' name <> T.justifyLeft 52 ' ' what <> mark refinement)

    mark refinement = case (Just refinement == Just required, Just refinement == given) of
        (True, _)      -> "◀ required"
        (False, True)  -> "◀ given"
        (False, False) -> ""

    -- The refinement a built-in lies directly within
    parentOf refinement = go builtIns
        where
        go (Node above _ children) = case [ above | Node r _ _ <- children, r == refinement ] of
            parent : _ -> Just parent
            []         -> foldr (\c found -> maybe (go c) Just found) Nothing children


-- Whether a value is of a built-in kind, a refinement checked by the value's shape alone:
-- value, everything; data, what can be written, string, number, boolean, record or array; and
-- function, a procedure or a refinement, a closure by the kind it keeps and a built-in a
-- procedure. Any other refinement is no kind.
hasKind :: R.Refinement -> Expr -> Bool
hasKind refinement value = case (refinement, value) of
    (R.RValue,      _)                                       -> True
    (R.RData,       _) | isData                              -> True
    (R.RString,     Primitive (I.String _))                  -> True
    (R.RNumber,     Primitive (I.Number _))                  -> True
    (R.RBoolean,    Primitive (I.Boolean _))                 -> True
    (R.RRecord,     _) | isJust (L.asRecord value)           -> True
    (R.RArray,      _) | isJust (L.asArray value)            -> True
    (R.RFunction,   FunctionClosure {})                      -> True
    (R.RFunction,   PrimitiveOp _)                           -> True
    (R.RProcedure,  FunctionClosure _ M.Procedure _ _ _ _)   -> True
    (R.RProcedure,  PrimitiveOp _)                           -> True
    (R.RRefinement, FunctionClosure _ M.Refinement _ _ _ _)  -> True
    _notOfTheKind                                            -> False

    where
    -- A string (a uri and a date among them), a number, a boolean, a record or an array
    isData = kindOf value `elem` map Just [R.RString, R.RUri, R.RDate, R.RNumber, R.RBoolean, R.RRecord, R.RArray]
