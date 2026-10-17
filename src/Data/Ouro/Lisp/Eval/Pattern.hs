{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Data.Ouro.Lisp.Eval.Pattern
( Bindings
, matchPattern
) where

import           Data.Function               ((&))
import           Data.Ouro.Error.Diagnostics (malformedCaseBranch, withBlurb)
import           Data.Ouro.Error.Types       (OuroError (..))
import qualified Data.Ouro.Internal.Expr     as I
import           Data.Ouro.Lisp.Eval.Types   (EvalM, Expr (..))
import qualified Data.Ouro.Lisp.Eval.Types   as L
import qualified Data.Ouro.Lisp.Surface      as S
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Text.Megaparsec             (SourcePos)


type Bindings = [(Text, L.Expr)]

-- Left: the pattern itself is malformed. Right Nothing: the value does not match.
type MatchResult = Either OuroError (Maybe Bindings)


-- matchPattern.
--
-- Matches an evaluated value against a pattern written in Ouro's own construction syntax:
--
--   * Record patterns  (:key pattern ... ?..rest)  match by key, in any order. Attribute pairs the
--     pattern does not mention are allowed, and '?..rest' binds them as a Record.
--   * Array patterns   (pattern ... ?..rest)       match by position; '?..rest' must come last
--     and binds the remaining elements as an Array.
--   * Holes            ?  matches anything,  ?name  binds the value.
--   * Anything else (literals, symbols, function calls) is evaluated and compared by value,
--     so '((+ 2 4) ?x)' and '(6 ?x)' are the same pattern.
matchPattern
    :: (S.Expr -> EvalM L.Expr)    -- Evaluates the non-pattern parts of a pattern
    -> S.Expr
    -> L.Expr
    -> EvalM MatchResult
matchPattern evaluator pattern value =
    case pattern of
        S.Hole _ name
            -> matched (bindHole name value)

        S.Form pos elems
            | any S.isAttrForm elems     -> matchRecord pos elems value
            | S.Symbol _ _ : _ <- elems  -> byValue      -- a function call, not an array
            | otherwise                  -> matchArray elems value

        _valueExpr
            -> byValue

    where
    byValue
        | S.containsHole pattern
            = malformed (S.exprPos pattern)
                        ( "A '?' hole cannot appear inside a function call or type assertion. "
                       <> "Holes can only stand in for Record values or Array elements." )
        | otherwise
            = do
              expected <- evaluator pattern
              pure $ case expected of
                         EvalError err -> Left err
                         _value        -> Right (if expected == value then Just [] else Nothing)

    matchArray :: [S.Expr] -> L.Expr -> EvalM MatchResult
    matchArray elems = \case
        Array xs             -> elements elems xs
        Primitive I.EmptyArr -> elements elems []
        _notAnArray          -> noMatch

    elements :: [S.Expr] -> [L.Expr] -> EvalM MatchResult
    elements ps xs =
        case (ps, xs) of
            ([S.Hole _ name], rest)
                | isRestHole name           -> matched (bindHole name (Array rest))
            (S.Hole hPos name : _ : _, _)
                | isRestHole name           -> malformed hPos
                                                   "A '?..rest' hole must be the last element of an Array pattern."
            (p : ps', x : xs')              -> matchPattern evaluator p x `andThen` elements ps' xs'
            ([], [])                        -> matched []
            _differentLengths               -> noMatch

    matchRecord :: SourcePos -> [S.Expr] -> L.Expr -> EvalM MatchResult
    matchRecord pos elems = \case
        Record meta pairs    -> attrs meta pairs
        Primitive I.EmptyRec -> attrs I.EmptyMeta []
        _notARecord          -> noMatch
        where
        keyed = [ (key, p) | S.Form _ [S.Symbol _ "attr", S.Literal _ (S.Str key), p] <- elems ]
        rests = [ name     | S.Hole _ name <- elems, isRestHole name ]

        strays = [ e | e <- elems, not (isKeyed e), not (isRest e) ]
        isKeyed = \case
                   S.Form _ [S.Symbol _ "attr", S.Literal _ (S.Str _), _] -> True
                   _other                                                  -> False
        isRest = \case
                  S.Hole _ name -> isRestHole name
                  _other        -> False

        attrs meta pairs =
            case (strays, rests) of
                (stray : _, _)
                    -> malformed (S.exprPos stray)
                                 "Record patterns can only contain ':key pattern' entries and one '?..rest' hole."
                (_, _ : _ : _)
                    -> malformed pos
                                 "A Record pattern can contain at most one '?..rest' hole."
                _valid
                    -> let unmentioned = [ (k, v) | (k, v) <- pairs, k `notElem` map fst keyed ]
                           restBinding = concat [ bindHole name (Record meta unmentioned) | name <- rests ]
                           attr (key, p) = case lookup key pairs of
                                                Just v  -> matchPattern evaluator p v
                                                Nothing -> noMatch
                       in foldr (andThen . attr) (matched restBinding) keyed


-- Runs the second match only when the first one succeeded, collecting both sets of bindings
andThen :: EvalM MatchResult -> EvalM MatchResult -> EvalM MatchResult
andThen first second =
    first >>= \case
        Right (Just bindings) -> (fmap . fmap . fmap) (bindings <>) second
        stop                  -> pure stop


-- '?' binds nothing; '?name' and '?..name' bind 'name'
bindHole :: Text -> L.Expr -> Bindings
bindHole hole value =
    case T.dropWhile (\c -> c == '?' || c == '.') hole of
        ""   -> []
        name -> [(name, value)]

isRestHole :: Text -> Bool
isRestHole = T.isPrefixOf "?.."

matched :: Bindings -> EvalM MatchResult
matched = pure . Right . Just

noMatch :: EvalM MatchResult
noMatch = pure (Right Nothing)

malformed :: SourcePos -> Text -> EvalM MatchResult
malformed pos explanation =
    malformedCaseBranch
    & withBlurb explanation
    & OuroError pos
    & Left
    & pure
