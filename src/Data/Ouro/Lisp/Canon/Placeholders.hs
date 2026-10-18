
module Data.Ouro.Lisp.Canon.Placeholders
( hole
, holeFreeForms
, holeFunction
, placeholders
) where

import           Data.List              (mapAccumL)
import qualified Data.Ouro.Lisp.Surface as S
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Text.Megaparsec        (SourcePos)


-- placeholders.
--
-- A call with '?' holes among its arguments is a function of its holes:
--
--   (+ ? 1)          (fn (%1) (+ %1 1))
--   (node ? "Type")  (fn (%1) (node %1 "Type"))
--   (* ?n ?n)        (fn (?n) (* ?n ?n))       a named hole is one parameter, however often it is written
--   (f #num ? x)     (fn (#num %1) (f %1 x))   a tagged hole is a typed parameter
--
-- A hole belongs to the call it is an argument of, so (f (g ?)) passes f a function. The
-- parameters are the holes in the order they are first written. Their names cannot be
-- written in Ouro ('%1', and '?n', which lexes as a hole), so they never capture a name
-- the call refers to. A case pattern's holes match values instead, so patterns are left
-- as they are, and so are a fn's parameters, which can be patterns too. A rest hole
-- ('?..name') belongs only in a pattern, and is reported by the evaluator wherever else
-- it is written. Runs before 'construct', so the function's body is desugared like any
-- other call.
placeholders :: S.Expr -> S.Expr
placeholders = \case
    S.Form pos (cSym@(S.Symbol _ "case") : target : branches)
        -> S.Form pos (cSym : placeholders target : map branch branches)

    -- A fn's parameters are names and patterns, not arguments, so only its body is read
    S.Form pos [fnSym@(S.Symbol _ "fn"), params, body]
        -> S.Form pos [fnSym, params, placeholders body]

    S.Form pos (headExpr@(S.Symbol _ name) : args)
        | name `notElem` holeFreeForms
        , not (any S.isAttrForm args)
        , any hole args
        -> holeFunction pos headExpr (map placeholders args)

    S.Form    pos items     -> S.Form    pos (map placeholders items)
    S.Bracket pos items     -> S.Bracket pos (map placeholders items)
    S.Tagged  pos tag inner -> S.Tagged  pos tag (placeholders inner)
    leaf                    -> leaf

    where
    -- Only a branch's body: its pattern is matched, not called
    branch = \case
        S.Form bPos [pattern, body] -> S.Form bPos [pattern, placeholders body]
        malformed                   -> malformed


-- Forms with their own structure, whose arguments are not values a hole could stand for
holeFreeForms :: [Text]
holeFreeForms = [ "%attr", "case", "context", "define", "defn", "defun", "fn", "graph"
                , "inlay", "let", "thru", "%binder" ]


-- An argument that is a hole, or a hole under a type tag. A rest hole is not a parameter.
hole :: S.Expr -> Bool
hole = \case
    S.Hole _ name                  -> not ("?.." `T.isPrefixOf` name)
    S.Tagged _ _ inner@(S.Hole _ _) -> hole inner
    _notAHole                      -> False


-- Builds (fn (params) (head args)) from a call with holes
holeFunction :: SourcePos -> S.Expr -> [S.Expr] -> S.Expr
holeFunction pos headExpr args =
    S.Form pos [ S.Symbol pos "fn"
               , S.Form pos (firstOfEach [ p | Just p <- params ])
               , S.Form pos (headExpr : args')
               ]
    where
    (args', params) = unzip (snd (mapAccumL fill (1 :: Int) args))

    -- Each anonymous hole is a new parameter, named by its position among them
    fill n = \case
        S.Hole hPos "?"
            -> let ref = S.Symbol hPos ("%" <> T.pack (show n))
               in (n + 1, (ref, Just ref))
        arg@(S.Hole hPos name)
            | hole arg
            -> let ref = S.Symbol hPos name
               in (n, (ref, Just ref))
        arg@(S.Tagged tPos tag inner)
            | hole arg
            -> let (n', (ref, _)) = fill n inner
               in (n', (ref, Just (S.Tagged tPos tag ref)))
        arg
            -> (n, (arg, Nothing))

    -- A named hole written twice is one parameter: the first, with its type if it has one
    firstOfEach = foldr (\p kept -> p : filter ((/= paramName p) . paramName) kept) []
    paramName = \case
        S.Tagged _ _ ref -> paramName ref
        S.Symbol _ name  -> name
        _notAParam       -> ""
