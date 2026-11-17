{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ouro.Lisp.Eval.Builtins
( builtinArity
, builtinRegistry
, parseISO8601
, isNumber
, distinctValues
) where

import           Control.Monad.Reader        (Reader)
import           Data.Function               ((&))
import           Data.List                   (find, sortOn)
import qualified Data.Map                    as Map
import           Data.Maybe                  (isNothing)
import           Data.Ouro.Error.Diagnostics (binaryOpMismatchBlurb,
                                              incorrectArity, propertyMismatch,
                                              typeMismatch, withBlurb)
import           Data.Ouro.Error.Types       (ErrorContext (..), OuroError (..),
                                              SyntaxError (..))
import qualified Data.Ouro.Internal.Expr     as I
import qualified Data.Ouro.Internal.Kinds    as JLD
import           Data.Ouro.Internal.Uri      (uriText)
import           Data.Ouro.Lisp.Eval.Types   (Env, PeriodUnit (..),
                                              humanReadableType)
import qualified Data.Ouro.Lisp.Eval.Types   as L
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time                   (UTCTime (..), addUTCTime,
                                              fromGregorian,
                                              gregorianMonthLength, toGregorian)
import           Data.Time.Calendar          (addDays,
                                              addGregorianMonthsClip,
                                              addGregorianYearsClip)
import           Text.Megaparsec             (SourcePos)


-- Maps syntax strings to their respective first-class execution handles
builtinRegistry :: Map.Map Text (SourcePos -> [L.Expr] -> Reader Env L.Expr)
builtinRegistry = Map.fromList
                      [ ("+",           handleAddition)
                      , ("-",           handleSubtraction)
                      , ("*",           handleMultiplication)
                      , ("/",           handleDivision)
                      , (">=",          handleGreaterEq)
                      , (">",           handleGreater)
                      , ("<=",          handleLessEq)
                      , ("<",           handleLess)
                      , ("eq",          handleEquality)
                      , ("neq",         handleNeq)
                      , ("not",         handleNot)
                      , ("len",         handleLength)
                      , ("contains",    handleContains)
                      , ("distinct",    handleDistinct)
                      , ("union",       handleUnion)
                      , ("intersect",   handleIntersect)
                      , ("setminus",    handleSetminus)
                      , ("subset",      handleSubset)
                      , ("unions",        handleUnions)
                      -- (adjoin base item ...), from Canon.Adjoin: '%' cannot be written in Ouro
                      , ("%adjoin",     handleAdjoin)
                      , ("intersections", handleIntersections)
                      , ("years",       handleYearsModifier)
                      , ("months",      handleMonthsModifier)
                      , ("days",        handleDaysModifier)
                      , ("hours",       handleHoursModifier)
                      , ("minutes",     handleMinutesModifier)
                      , ("seconds",     handleSecondsModifier)
                      -- Runtime temporal handlers
                      , ("years-end",   handleYearsEndModifier)
                      , ("months-end",  handleMonthsEndModifier)
                      -- Strings: each takes the string first, so -> threads through them
                      , ("to-string",   handleToString)
                      , ("split",       handleSplit)
                      , ("join",        handleJoin)
                      , ("upper-case",  handleUpperCase)
                      , ("lower-case",  handleLowerCase)
                      , ("trim",        handleTrim)
                      , ("starts-with", handleStartsWith)
                      , ("ends-with",   handleEndsWith)
                      , ("slice",       handleSlice)
                      , ("pad-start",   handlePadStart)
                      , ("replace-all", handleReplaceAll)
                      , ("zip",         handleZip)
                      , ("min",         handleMin)
                      , ("max",         handleMax)
                      -- (start..end) and (first second .. end), from the parser: '%' cannot be written in Ouro
                      , ("%range",      handleRange)
                      ]


-- builtinArity.
--
-- How many arguments each built-in takes, as every built-in takes a fixed number, and the
-- hint for a call with another number. The static analysis checks a call written with a
-- built-in's name against it, wherever the call is written; a handler reports a call made
-- through a value, as in (map xs f), with the same hint. A built-in Ouro cannot write, as
-- %adjoin, is made only by Canon and has none.
builtinArity :: Map.Map Text (Int, Text)
builtinArity = Map.fromList $
       [ (op, (2, operandHint op)) | op <- ["+", "-", "*", "/", ">", ">=", "<", "<=", "eq", "neq"] ]
    <> [ ("union",         (2, setHint "union" "is every value of either" "unions"))
       , ("intersect",     (2, setHint "intersect" "is every value of both" "intersections"))
       , ("setminus",      (2, setHint "setminus" "is every value of the first that is not in the second" "setminus"))
       , ("subset",        (2, setHint "subset" "is true when every value of the first is in the second" "subset"))
       , ("not",           (1, "not takes one Boolean: (not b)."))
       , ("len",           (1, "len takes one Array or String: (len xs)."))
       , ("contains",      (2, "contains takes an Array or a String, then the value to look for: (contains (1 2 3) 2)."))
       , ("distinct",      (1, "distinct takes one Array: (distinct (1 2 1))."))
       , ("unions",        (1, "unions takes one Array of Arrays: (unions ((1) (2 3)))."))
       , ("intersections", (1, "intersections takes one Array of Arrays: (intersections ((1 2) (2 3)))."))
       ]
    <> [ (unit, (1, unit <> " takes one Number: (" <> unit <> " 2)."))
       | unit <- ["years", "months", "days", "hours", "minutes", "seconds", "years-end", "months-end"] ]
    <> [ ("to-string",   (1, "to-string takes one value: (to-string 42)."))
       , ("split",       (2, "split takes a String, then the separator: (split \"a, b\" \", \")."))
       , ("join",        (2, "join takes an Array of Strings, then the separator: (join (\"a\" \"b\") \", \")."))
       , ("upper-case",  (1, "upper-case takes one String: (upper-case s)."))
       , ("lower-case",  (1, "lower-case takes one String: (lower-case s)."))
       , ("trim",        (1, "trim takes one String: (trim s)."))
       , ("starts-with", (2, "starts-with takes a String, then the prefix: (starts-with s \"lob:\")."))
       , ("ends-with",   (2, "ends-with takes a String, then the suffix: (ends-with s \".jpg\")."))
       , ("slice",       (3, "slice takes a String, a start and an end: (slice \"C-0042\" 2 6)."))
       , ("pad-start",   (3, "pad-start takes a String, a width and one character: (pad-start \"42\" 4 \"0\")."))
       , ("replace-all", (3, "replace-all takes a String, what to replace and what with: (replace-all s \"-\" \" \")."))
       , ("zip",         (2, "zip takes two Arrays of one length: (zip (1 2) (\"a\" \"b\"))."))
       , ("min",         (1, "(min xs) takes one Array of Numbers, (min (3.72 3.36)); several Numbers are written as one, (min (a b c))."))
       , ("max",         (1, "(max xs) takes one Array of Numbers, (max (3.72 3.36)); several Numbers are written as one, (max (a b c))."))
       ]


-- The hint for an operator given any number of operands but two
operandHint :: Text -> Text
operandHint op = op <> " takes two operands, (" <> op <> " a b). For more, fold them: (fold (a b c) start " <> op <> ")."

-- The hint for a set operation given any number of Arrays but two, pointing to the operation
-- over an Array of sets, where there is one
setHint :: Text -> Text -> Text -> Text
setHint name meaning many =
    name <> " takes two Arrays: (" <> name <> " (1 2) (2 3)) " <> meaning <> "." <> manySets
    where
    manySets = case many == name of
                   True  -> ""
                   False -> " For more, " <> many <> " takes one Array of them: (" <> many <> " (a b c))."


--- Core Math & String Accumulators ---

-- handleAddition.
--
-- Binary addition operator. Handles numbers, string concatenations,
-- and polymorphic datetime shifts.
handleAddition :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleAddition pos =
    \case
     [a, b]
         -> case (a, b) of
                (L.Primitive (I.Date utc),   L.Duration unit amt)       -> pure $ L.Primitive (I.Date (applyDuration utc unit amt))
                (L.Duration unit amt,        L.Primitive (I.Date utc))  -> pure $ L.Primitive (I.Date (applyDuration utc unit amt))
                (L.Primitive (I.String s1),  L.Primitive (I.String s2)) -> pure $ L.Primitive (I.String (s1 <> s2))
                (L.Primitive (I.Number n1),  L.Primitive (I.Number n2)) -> pure $ L.Primitive (I.Number (n1 + n2))
                -- Arrays join, whatever their elements: like JSON arrays, they need not be homogeneous
                (arrX, arrY)
                    | Just xs <- L.asArray arrX, Just ys <- L.asArray arrY
                    -> pure $ L.Array (xs <> ys)

                -- Error propagation
                (err@(L.EvalError _), _) -> pure err
                (_, err@(L.EvalError _)) -> pure err

                _typeMismatch
                    -> Data.Ouro.Error.Diagnostics.typeMismatch "Matching numeric, string, or date/duration pairs"
                             (humanReadableType a <> " + " <> humanReadableType b)
                       & Data.Ouro.Error.Diagnostics.withBlurb (Data.Ouro.Error.Diagnostics.binaryOpMismatchBlurb a b)
                       & OuroError pos
                       & L.EvalError
                       & pure

     -- The wrong number of operands, as written: each operator compares or combines two values
     operands
         -> wrongOperandCount "+" operands
            & OuroError pos
            & L.EvalError
            & pure


-- handleSubtraction.
--
-- Binary subtraction operator. Handles also handles backward calendar traversal.
handleSubtraction :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleSubtraction pos =
    \case
     [a, b] -> case (a , b) of
                   (L.Primitive (I.Date utc),  L.Duration unit amt)       -> pure $ L.Primitive (I.Date (applyDuration utc unit (-amt)))
                   (L.Duration unit amt,       L.Primitive (I.Date utc))  -> pure $ L.Primitive (I.Date (applyDuration utc unit (-amt)))
                   (L.Primitive (I.Number n1), L.Primitive (I.Number n2)) -> pure $ L.Primitive (I.Number (n1 - n2))

                   -- Error propagation
                   (err@(L.EvalError _), _) -> pure err
                   (_, err@(L.EvalError _)) -> pure err

                   _typeMismatch
                       -> Data.Ouro.Error.Diagnostics.typeMismatch "Matching numeric values or a Date minus a Duration"
                               (humanReadableType a <> " - " <> humanReadableType b)
                          & Data.Ouro.Error.Diagnostics.withBlurb (Data.Ouro.Error.Diagnostics.binaryOpMismatchBlurb a b)
                          & OuroError pos
                          & L.EvalError
                          & pure

     -- The wrong number of operands, as written: each operator compares or combines two values
     operands
         -> wrongOperandCount "-" operands
            & OuroError pos
            & L.EvalError
            & pure


-- handleMultiplication.
--
-- Variadic multiplication operator. Folds across numeric values.
-- Operates purely within the Reader monad.
handleMultiplication :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleMultiplication pos =
    \case
     [a, b] -> case (a, b) of
                   (L.Primitive (I.Number n1), L.Primitive (I.Number n2)) -> pure $ L.Primitive (I.Number (n1 * n2))

                   -- Error propagation
                   (err@(L.EvalError _), _) -> pure err
                   (_, err@(L.EvalError _)) -> pure err

                   _typeMismatch
                       -> Data.Ouro.Error.Diagnostics.typeMismatch "Matching numeric values for multiplication"
                               (humanReadableType a <> " * " <> humanReadableType b)
                          & Data.Ouro.Error.Diagnostics.withBlurb (Data.Ouro.Error.Diagnostics.binaryOpMismatchBlurb a b)
                          & OuroError pos
                          & L.EvalError
                          & pure

     -- The wrong number of operands, as written: each operator compares or combines two values
     operands
         -> wrongOperandCount "*" operands
            & OuroError pos
            & L.EvalError
            & pure


-- handleDivision.
--
-- Binary division operator. Expects exactly two arguments.
-- Operates purely within the Reader monad.
handleDivision :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleDivision pos =
    \case
     [a, b] -> case (a, b) of
         -- Division by Zero check
         (L.Primitive (I.Number _), L.Primitive (I.Number 0))
             -> Data.Ouro.Error.Diagnostics.typeMismatch "A non-zero Number divisor" "a zero (0)"
                & Data.Ouro.Error.Diagnostics.withBlurb "Attempted division by zero."
                & OuroError pos
                & L.EvalError
                & pure

         -- Successful Division
         (L.Primitive (I.Number n1), L.Primitive (I.Number n2))
             -> pure $ L.Primitive (I.Number (n1 / n2))

         -- Error propagation
         (err@(L.EvalError _), _) -> pure err
         (_, err@(L.EvalError _)) -> pure err

         -- Type Mismatch
         _typeMismatch
             -> Data.Ouro.Error.Diagnostics.typeMismatch "Matching numeric values for division"
                             (humanReadableType a <> " / " <> humanReadableType b)
                & Data.Ouro.Error.Diagnostics.withBlurb (Data.Ouro.Error.Diagnostics.binaryOpMismatchBlurb a b)
                & OuroError pos
                & L.EvalError
                & pure

     -- The wrong number of operands, as written: each operator compares or combines two values
     operands
         -> wrongOperandCount "/" operands
            & OuroError pos
            & L.EvalError
            & pure

handleGreater :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleGreater pos =
    \case
     [a, b] -> case (a, b) of
                   (L.Primitive (I.Number n1), L.Primitive (I.Number n2)) -> pure $ L.Primitive (I.Boolean (n1 > n2))

                   -- Error propagation
                   (err@(L.EvalError _), _) -> pure err
                   (_, err@(L.EvalError _)) -> pure err

                   _typeMismatch
                       -> Data.Ouro.Error.Diagnostics.typeMismatch "Matching numeric values for greater than"
                               (humanReadableType a <> " > " <> humanReadableType b)
                          & Data.Ouro.Error.Diagnostics.withBlurb (Data.Ouro.Error.Diagnostics.binaryOpMismatchBlurb a b)
                          & OuroError pos
                          & L.EvalError
                          & pure

     -- The wrong number of operands, as written: each operator compares or combines two values
     operands
         -> wrongOperandCount ">" operands
            & OuroError pos
            & L.EvalError
            & pure


handleGreaterEq :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleGreaterEq pos =
    \case
     [a, b] -> case (a, b) of
                   (L.Primitive (I.Number n1), L.Primitive (I.Number n2)) -> pure $ L.Primitive (I.Boolean (n1 >= n2))

                   -- Error propagation
                   (err@(L.EvalError _), _) -> pure err
                   (_, err@(L.EvalError _)) -> pure err

                   _typeMismatch
                       -> Data.Ouro.Error.Diagnostics.typeMismatch "Matching numeric values for greater than or equal to"
                               (humanReadableType a <> " >= " <> humanReadableType b)
                          & Data.Ouro.Error.Diagnostics.withBlurb (Data.Ouro.Error.Diagnostics.binaryOpMismatchBlurb a b)
                          & OuroError pos
                          & L.EvalError
                          & pure

     -- The wrong number of operands, as written: each operator compares or combines two values
     operands
         -> wrongOperandCount ">=" operands
            & OuroError pos
            & L.EvalError
            & pure


handleLess :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleLess pos =
    \case
     [a, b] -> case (a, b) of
                   (L.Primitive (I.Number n1), L.Primitive (I.Number n2)) -> pure $ L.Primitive (I.Boolean (n1 < n2))

                   -- Error propagation
                   (err@(L.EvalError _), _) -> pure err
                   (_, err@(L.EvalError _)) -> pure err

                   _typeMismatch
                       -> Data.Ouro.Error.Diagnostics.typeMismatch "Matching numeric values for less than"
                               (humanReadableType a <> " < " <> humanReadableType b)
                           & Data.Ouro.Error.Diagnostics.withBlurb (Data.Ouro.Error.Diagnostics.binaryOpMismatchBlurb a b)
                           & OuroError pos
                           & L.EvalError
                           & pure

     -- The wrong number of operands, as written: each operator compares or combines two values
     operands
         -> wrongOperandCount "<" operands
            & OuroError pos
            & L.EvalError
            & pure


handleLessEq :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleLessEq pos =
    \case
     [a, b] -> case (a, b) of
                   (L.Primitive (I.Number n1), L.Primitive (I.Number n2)) -> pure $ L.Primitive (I.Boolean (n1 <= n2))

                   -- Error propagation
                   (err@(L.EvalError _), _) -> pure err
                   (_, err@(L.EvalError _)) -> pure err

                   _typeMismatch
                       -> Data.Ouro.Error.Diagnostics.typeMismatch "Matching numeric values for less than or equal to"
                               (humanReadableType a <> " <= " <> humanReadableType b)
                           & Data.Ouro.Error.Diagnostics.withBlurb (Data.Ouro.Error.Diagnostics.binaryOpMismatchBlurb a b)
                           & OuroError pos
                           & L.EvalError
                           & pure

     -- The wrong number of operands, as written: each operator compares or combines two values
     operands
         -> wrongOperandCount "<=" operands
            & OuroError pos
            & L.EvalError
            & pure


-- handleEquality.
--
-- Binary equality operator. Expects exactly two arguments.
-- Operates purely within the Reader monad.
handleEquality :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleEquality pos =
    \case
     [a, b] -> case (a, b) of
                   -- Error propagation
                   (err@(L.EvalError _), _) -> pure err
                   (_, err@(L.EvalError _)) -> pure err

                   -- Functions have no meaningful equality
                   (v1, v2) | isFunction v1 || isFunction v2 -> functionComparison pos "eq" v1 v2

                   -- Comparison using structural equality
                   (v1, v2) -> pure $ L.Primitive (I.Boolean (v1 == v2))

     -- The wrong number of operands, as written: each operator compares or combines two values
     operands
         -> wrongOperandCount "eq" operands
             & OuroError pos
             & L.EvalError
             & pure


-- handleNeq
--
-- Binary equality operator. Expects exactly two arguments.
-- Operates purely within the Reader monad.
handleNeq :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleNeq pos =
    \case
     [a, b] -> case (a, b) of
                   -- Error propagation
                   (err@(L.EvalError _), _) -> pure err
                   (_, err@(L.EvalError _)) -> pure err

                   -- Functions have no meaningful equality
                   (v1, v2) | isFunction v1 || isFunction v2 -> functionComparison pos "neq" v1 v2

                   -- Comparison using structural equality
                   (v1, v2) -> pure $ L.Primitive (I.Boolean (v1 /= v2))

     -- The wrong number of operands, as written: each operator compares or combines two values
     operands
         -> wrongOperandCount "neq" operands
             & OuroError pos
             & L.EvalError
             & pure


-- handleNot.
--
-- The negation of a Boolean.
handleNot :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleNot pos =
    \case
     [err@(L.EvalError _)]
         -> pure err

     [L.Primitive (I.Boolean b)]
         -> pure $ L.Primitive (I.Boolean (not b))

     [other]
         -> Data.Ouro.Error.Diagnostics.propertyMismatch "#boolean" (L.propertyOf other)
            & OuroError pos
            & L.EvalError
            & pure

     args
         -> Data.Ouro.Error.Diagnostics.incorrectArity "not" 1 (length args)
            & OuroError pos
            & L.EvalError
            & pure


-- Functions cannot be compared: two different functions can behave identically,
-- so any answer 'eq' gave would be misleading
isFunction :: L.Expr -> Bool
isFunction = \case
              L.PrimitiveOp _             -> True
              L.FunctionClosure _ _ _ _   -> True
              _notAFunction               -> False

functionComparison :: SourcePos -> Text -> L.Expr -> L.Expr -> Reader Env L.Expr
functionComparison pos op a b =
    Data.Ouro.Error.Diagnostics.typeMismatch "two comparable values (functions cannot be compared)"
        (humanReadableType a <> " " <> op <> " " <> humanReadableType b)
    & Data.Ouro.Error.Diagnostics.withBlurb ( "Functions have no meaningful equality. "
                                           <> "Perhaps compare the values they return instead, e.g. (eq (f! x) (g! x))."
                                            )
    & OuroError pos
    & L.EvalError
    & pure


-- An operator written with the wrong number of operands, e.g. (+ 1) or (+ 1 2 3). Every
-- operator takes two, as every built-in takes a fixed number of arguments; many values are
-- an Array, folded.
wrongOperandCount :: Text -> [L.Expr] -> ErrorContext
wrongOperandCount op operands =
    Data.Ouro.Error.Diagnostics.incorrectArity op 2 (length operands)
    & Data.Ouro.Error.Diagnostics.withBlurb (operandHint op)


isNumber :: L.Expr -> Bool
isNumber = \case
            (L.Primitive (I.Number _)) -> True
            _notANum                   -> False


--- Time Shifting & L.Duration Modifiers ---
-- Shifts a date by a duration. Months and years clip to the end of a shorter month rather
-- than rolling over into the next: 31 January + 1 month is the last day of February, and
-- 29 February + 1 year is 28 February. A shift never skips past the month it lands in.
applyDuration :: Integral a => UTCTime -> PeriodUnit -> a -> UTCTime
applyDuration utc unit amt = case unit of
                                 Years   -> utc { utctDay = addGregorianYearsClip (fromIntegral amt) (utctDay utc) }
                                 Months  -> utc { utctDay = addGregorianMonthsClip (fromIntegral amt) (utctDay utc) }
                                 Days    -> utc { utctDay = addDays (fromIntegral amt) (utctDay utc) }
                                 Hours   -> addUTCTime (fromIntegral amt * 3600) utc
                                 Minutes -> addUTCTime (fromIntegral amt * 60) utc
                                 Seconds -> addUTCTime (fromIntegral amt) utc

                                 -- Runtime handles for dynamic calendar snapping
                                 YearsToEnd -> let advancedDay = addGregorianYearsClip (fromIntegral amt) (utctDay utc)
                                                   (y, _, _)   = toGregorian advancedDay
                                               in utc { utctDay = fromGregorian y 12 31, utctDayTime = 86399 }

                                 MonthsToEnd -> let advancedDay = addGregorianMonthsClip (fromIntegral amt) (utctDay utc)
                                                    (y, m, _)   = toGregorian advancedDay
                                                    lastDay     = gregorianMonthLength y m
                                                in utc { utctDay = fromGregorian y m lastDay, utctDayTime = 86399 }




-- handleLength.
--
-- The number of elements in an array, or of characters in a string. Characters are Unicode
-- code points, not bytes, so "Málaga" has 6. A string cannot be taken apart in Ouro, so its
-- length cannot be written in Ouro itself.
handleLength :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleLength pos =
    \case
     [L.Primitive (I.String s)]
         -> pure $ L.Primitive (I.Number (fromIntegral (T.length s)))

     [arr]
         | Just elements <- L.asArray arr
         -> pure $ L.Primitive (I.Number (fromIntegral (length elements)))

     -- Error propagation
     [err@(L.EvalError _)]
         -> pure err

     [other]
         -> Data.Ouro.Error.Diagnostics.typeMismatch "An Array or a String" (humanReadableType other)
            & OuroError pos
            & L.EvalError
            & pure

     args
         -> Data.Ouro.Error.Diagnostics.incorrectArity "len" 1 (length args)
            & OuroError pos
            & L.EvalError
            & pure


-- handleContains.
--
-- (contains coll x): whether an Array has x as one of its elements, compared as eq
-- compares them, or whether a String has x as a substring.
handleContains :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleContains pos =
    \case
     -- Error propagation
     [err@(L.EvalError _), _]
         -> pure err
     [_, err@(L.EvalError _)]
         -> pure err

     [L.Primitive (I.String s), L.Primitive (I.String part)]
         -> pure $ L.Primitive (I.Boolean (part `T.isInfixOf` s))

     [coll, x]
         | Just elements <- L.asArray coll
         -> case (find isError elements, find isFunction (x : elements)) of
                (Just err, _) -> pure err
                (_, Just f)   -> uncomparable pos "contains" f
                _noProblem    -> pure $ L.Primitive (I.Boolean (x `elem` elements))

     [L.Primitive (I.String _), other]
         -> Data.Ouro.Error.Diagnostics.typeMismatch "a String to look for in a String" (humanReadableType other)
            & OuroError pos
            & L.EvalError
            & pure

     [other, _]
         -> Data.Ouro.Error.Diagnostics.typeMismatch "an Array or a String to search" (humanReadableType other)
            & Data.Ouro.Error.Diagnostics.withBlurb "contains looks for an element in an Array, (contains (1 2 3) 2), or for a substring in a String, (contains \"Amsterdam\" \"dam\")."
            & OuroError pos
            & L.EvalError
            & pure

     args
         -> Data.Ouro.Error.Diagnostics.incorrectArity "contains" 2 (length args)
            & OuroError pos
            & L.EvalError
            & pure


-- handleDistinct.
--
-- An Array without repeated elements, compared as eq compares them. The first of each is
-- kept, in order, so (distinct (1 2 1 3)) is (1 2 3).
handleDistinct :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleDistinct pos =
    \case
     [err@(L.EvalError _)]
         -> pure err

     [arr]
         | Just elements <- L.asArray arr
         -> case (find isError elements, find isFunction elements) of
                (Just err, _) -> pure err
                (_, Just f)   -> uncomparable pos "distinct" f
                _noProblem    -> pure $ L.Array (distinct elements)

     [other]
         -> Data.Ouro.Error.Diagnostics.typeMismatch "an Array" (humanReadableType other)
            & OuroError pos
            & L.EvalError
            & pure

     args
         -> Data.Ouro.Error.Diagnostics.incorrectArity "distinct" 1 (length args)
            & OuroError pos
            & L.EvalError
            & pure


-- handleUnion, handleIntersect, handleSetminus and handleSubset.
--
-- Arrays as sets, as JSON-LD reads a property's values: elements are compared as eq
-- compares them, and a result has no repeats, keeping the first of each in the order it
-- is first written. Each takes two Arrays, as every built-in takes a fixed number of
-- arguments; many sets are an Array of them, for unions and intersections:
--
--   (union (1 2) (2 3))          (1 2 3)      in either
--   (intersect (1 2 3) (3 2 4))  (2 3)        in both, in the first's order
--   (setminus (1 2 3) (2))       (1 3)        in the first and not the second
--   (subset (1 2) (2 3 1))       true         every value of the first is in the second
handleUnion :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleUnion pos = twoSets pos "union" unionOf

handleIntersect :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleIntersect pos = twoSets pos "intersect" intersectionOf

handleSetminus :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleSetminus pos = twoSets pos "setminus" (L.Array . difference)

-- a ⊆ b exactly when a ∖ b is empty
handleSubset :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleSubset pos = twoSets pos "subset" (L.Primitive . I.Boolean . null . difference)

-- A set operation of exactly two Arrays; with another number, an arity error with the hint
-- the static analysis gives (see 'builtinArity')
twoSets :: SourcePos -> Text -> ([[L.Expr]] -> L.Expr) -> [L.Expr] -> Reader Env L.Expr
twoSets pos name operation = \case
    args@[_, _] -> withSets pos name args operation
    args        -> Data.Ouro.Error.Diagnostics.incorrectArity name 2 (length args)
                   & maybe id (Data.Ouro.Error.Diagnostics.withBlurb . snd) (Map.lookup name builtinArity)
                   & OuroError pos
                   & L.EvalError
                   & pure

-- handleUnions and handleIntersections.
--
-- The union and the intersection of one Array of Arrays, for sets that are computed rather
-- than written: (unions (map parts (get ? classified_as))) is every classification of
-- every part. The union of no sets is (), and the intersection of no sets is an error.
handleUnions :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleUnions pos = overSets pos "unions" $ \case
    []   -> pure (L.Array [])
    sets -> withSets pos "unions" sets unionOf

handleIntersections :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleIntersections pos = overSets pos "intersections" $ \case
    []   -> Data.Ouro.Error.Diagnostics.typeMismatch "at least one Array to intersect" "an empty Array"
            & Data.Ouro.Error.Diagnostics.withBlurb "The intersection of no sets is not defined: (intersections ((1 2) (2 3))) is (2)."
            & OuroError pos
            & L.EvalError
            & pure
    sets -> withSets pos "intersections" sets intersectionOf

-- Applies a set operation to the elements of one Array
overSets :: SourcePos -> Text -> ([L.Expr] -> Reader Env L.Expr) -> [L.Expr] -> Reader Env L.Expr
overSets pos name operation = \case
    [err@(L.EvalError _)]
        -> pure err

    [arr]
        | Just sets <- L.asArray arr
        -> operation sets

    [other]
        -> Data.Ouro.Error.Diagnostics.typeMismatch "an Array of Arrays" (humanReadableType other)
           & Data.Ouro.Error.Diagnostics.withBlurb (name <> " takes one Array of Arrays: (" <> name <> " ((1 2) (2 3))).")
           & OuroError pos
           & L.EvalError
           & pure

    args
        -> Data.Ouro.Error.Diagnostics.incorrectArity name 1 (length args)
           & OuroError pos
           & L.EvalError
           & pure

-- Every value in any of the sets
unionOf :: [[L.Expr]] -> L.Expr
unionOf = L.Array . distinct . concat

-- The values of one Array as a set, for image: repeats are dropped as union drops them,
-- and an error or a function among them is reported as union reports it
distinctValues :: SourcePos -> Text -> [L.Expr] -> Reader Env L.Expr
distinctValues pos name values = withSets pos name [L.Array values] unionOf

-- The values of the first set that are in all the rest
intersectionOf :: [[L.Expr]] -> L.Expr
intersectionOf = L.Array . \case
    first : rest -> let sets = map setOf rest
                    in filter (\x -> all (member x) sets) (distinct first)
    []           -> []

-- The values of the first set in none of the rest
difference :: [[L.Expr]] -> [L.Expr]
difference = \case
    first : rest -> let sets = map setOf rest
                    in filter (\x -> not (any (member x) sets)) (distinct first)
    []           -> []


-- Values as a set, compared as eq compares them.
--
-- Values have no order of their own, so each is given a key that equal values always share,
-- and values are kept in groups by key: only the values of one group are compared with eq.
-- A key may be shared by values that differ (a record's context is not part of it), which
-- costs a comparison and never a wrong answer. So a set operation costs n log n, where
-- comparing every value with every other cost the square (gap 56).
newtype Set = Set (Map.Map SetKey [L.Expr])

data SetKey
    = KString  Text
    | KNumber  Double
    | KNaN                          -- Never equal to itself, but a key must be
    | KBoolean Bool
    | KUri     Text
    | KDate    UTCTime
    | KBlank   Text
    | KRecord  [(Text, SetKey)]
    | KArray   [SetKey]
    | KOther
    deriving (Eq, Ord)

-- A value's key: a record's attribute pairs in key order, as eq compares them
setKey :: L.Expr -> SetKey
setKey = \case
    L.Primitive p -> primitiveKey p
    L.Record _ kvs -> KRecord [ (k, setKey v) | (k, v) <- sortOn fst kvs ]
    L.Array xs    -> KArray (map setKey xs)
    _other        -> KOther
    where
    primitiveKey :: I.Expr 'JLD.Primitive -> SetKey
    primitiveKey = \case
        I.String s     -> KString s
        I.Number n
            | isNaN n  -> KNaN
            | otherwise -> KNumber n
        I.Boolean b    -> KBoolean b
        I.URI u        -> KUri (uriText u)
        I.Date d       -> KDate d
        I.BlankNode b  -> KBlank b
        I.Record _ kvs -> KRecord [ (k, primitiveKey v) | (k, v) <- sortOn fst kvs ]
        I.Array xs     -> KArray (map primitiveKey xs)
        _other         -> KOther

emptySet :: Set
emptySet = Set Map.empty

member :: L.Expr -> Set -> Bool
member x (Set groups) = maybe False (elem x) (Map.lookup (setKey x) groups)

insert :: L.Expr -> Set -> Set
insert x (Set groups) = Set (Map.insertWith (<>) (setKey x) [x] groups)

setOf :: [L.Expr] -> Set
setOf = foldr insert emptySet

-- The values without repeats, keeping the first of each in order
distinct :: [L.Expr] -> [L.Expr]
distinct = go emptySet
    where
    go _ [] = []
    go seen (x : xs) = case member x seen of
        True  -> go seen xs
        False -> x : go (insert x seen) xs


-- Runs a set operation on its Arrays. The first error among the arguments or their
-- elements is the result; so is a type error for a value that is not an Array, or for a
-- function, which cannot be compared.
withSets :: SourcePos -> Text -> [L.Expr] -> ([[L.Expr]] -> L.Expr) -> Reader Env L.Expr
withSets pos name args operation =
    case (args, find isError args, traverse L.asArray args) of
        ([], _, _)
            -> Data.Ouro.Error.Diagnostics.incorrectArity name 1 0
               & Data.Ouro.Error.Diagnostics.withBlurb (name <> " takes one or more Arrays: (" <> name <> " (1 2) (2 3)).")
               & OuroError pos
               & L.EvalError
               & pure

        (_, Just err, _)
            -> pure err

        (_, _, Nothing)
            -> case find (isNothing . L.asArray) args of
                   Just other -> Data.Ouro.Error.Diagnostics.typeMismatch "an Array, as a set" (humanReadableType other)
                                 & Data.Ouro.Error.Diagnostics.withBlurb (name <> " treats Arrays as sets: (" <> name <> " (1 2) (2 3)).")
                                 & OuroError pos
                                 & L.EvalError
                                 & pure
                   Nothing    -> pure (operation [])

        (_, _, Just sets)
            -> let elements = concat sets
               in case (find isError elements, find isFunction elements) of
                      (Just err, _) -> pure err
                      (_, Just f)   -> uncomparable pos name f
                      _noProblem    -> pure (operation sets)


isError :: L.Expr -> Bool
isError = \case
           L.EvalError _ -> True
           _notAnError   -> False


-- An Array holding a function cannot be searched or deduplicated, for the reason eq
-- cannot compare functions
uncomparable :: SourcePos -> Text -> L.Expr -> Reader Env L.Expr
uncomparable pos name f =
    Data.Ouro.Error.Diagnostics.typeMismatch ("values " <> name <> " can compare (functions cannot be compared)") (humanReadableType f)
    & Data.Ouro.Error.Diagnostics.withBlurb "Functions have no meaningful equality, so an Array holding one cannot be searched or deduplicated."
    & OuroError pos
    & L.EvalError
    & pure


-- handleAdjoin.
--
-- (%adjoin base addition ...), which (adjoin base item ...) becomes (see 'Canon.Adjoin'), adds
-- each addition to base in turn: a record's attribute pairs to a record, each value to its
-- key's set of values, and an array's elements to an array. A value is added unless it is
-- already there, compared as eq compares; the values already there are kept as they are,
-- repeats included, and a key that is added to always holds an Array:
--
--   (adjoin (:k 1) :k 2)              {"k": [1, 2]}
--   (adjoin (:k (1 1)) :k (1 3))      {"k": [1, 1, 3]}
--   (adjoin (1 2) 2 3)                (1 2 3)
--
-- A key keeps its place in the record, and a new key goes after the others. The base's
-- context is layered under an addition's, as an inlay's is under the record's.
handleAdjoin :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleAdjoin pos = pure . \case
    base : additions -> foldl adjoinTo base additions
    []               -> mismatch "a value to adjoin to" "nothing"
    where
    adjoinTo acc addition = case (acc, addition) of
        (L.EvalError _, _)               -> acc
        (_, L.EvalError _)               -> addition
        (L.Record m pairs, L.Record m' added)
            -> foldl addPair (L.Record (m <> m') pairs) added
        (L.Array xs, L.Array ys)
            -> withComparable (xs <> ys) (L.Array (xs <> newAmong xs ys))
        (L.Record _ _, other)
            -> mismatch "attribute pairs, written :key value, to adjoin to a Record" (described other)
        (L.Array _, other)
            -> mismatch "elements to adjoin to an Array" (described other)
        (other, _)
            -> mismatch "a Record or an Array to adjoin to" (humanReadableType other)

    -- A value is added to its key's values; an error, or a pair that is not a key's, is kept
    addPair acc (key, value) = case acc of
        L.Record m pairs
            | key `elem` ["*err*", "@context"] -> L.Record m (pairs <> [(key, value)])
            | otherwise
                -> let existing = maybe [] valuesOf (lookup key pairs)
                       adjoined = case value of
                                      L.EvalError _ -> value
                                      _aValue       -> withComparable (existing <> valuesOf value)
                                                                      (L.Array (existing <> newAmong existing (valuesOf value)))
                   in L.Record m (placed key adjoined pairs)
        other
            -> other

    -- An Array is its elements, any other value is one value
    valuesOf = \case
        L.Array xs -> xs
        other      -> [other]

    -- The values not already there, each once, in the order written
    newAmong existing = let there = setOf existing
                        in distinct . filter (\x -> not (member x there))

    -- A key already there keeps its place; a new one goes after the others
    placed key value pairs
        | any ((== key) . fst) pairs = [ (k, case k == key of True -> value; False -> v) | (k, v) <- pairs ]
        | otherwise                  = pairs <> [(key, value)]

    -- A function cannot be compared, so it cannot be looked for among a key's values
    withComparable values result = case find isFunction values of
        Just f     -> uncomparableValue f
        Nothing    -> result

    uncomparableValue f =
        Data.Ouro.Error.Diagnostics.typeMismatch "values adjoin can compare (functions cannot be compared)" (humanReadableType f)
        & Data.Ouro.Error.Diagnostics.withBlurb "adjoin adds a value unless it is already there, and functions have no meaningful equality."
        & OuroError pos
        & L.EvalError

    described = \case
        L.Array _    -> "elements, which adjoin adds to an Array"
        L.Record _ _ -> "attribute pairs, which adjoin adds to a Record"
        other        -> humanReadableType other

    mismatch expected got =
        Data.Ouro.Error.Diagnostics.typeMismatch expected got
        & Data.Ouro.Error.Diagnostics.withBlurb "adjoin adds attribute pairs to a Record, (adjoin r :classified_as t), and elements to an Array, (adjoin (1 2) 3)."
        & OuroError pos
        & L.EvalError


-- Shared helper for duration modifiers to ensure consistent error handling.
mkDurationHandler :: Text -> PeriodUnit -> SourcePos -> [L.Expr] -> Reader Env L.Expr
mkDurationHandler tagName unit pos args =
    case args of
        [L.Primitive (I.Number n)] -> pure $ L.Duration unit (floor n)
        [badArg]                   -> Data.Ouro.Error.Diagnostics.typeMismatch ("A Number representing the amount of " <> tagName)
                                                   (humanReadableType badArg)
                                      & Data.Ouro.Error.Diagnostics.withBlurb ( "The (" <> tagName <> ") modifier expects a single numeric argument "
                                                 <> "representing the duration period."
                                                  )
                                      & OuroError pos
                                      & L.EvalError
                                      & pure

        _ ->
            pure $ L.EvalError $ OuroError pos $ Syntax $ MalformedTagPayload
                { activeTag      = tagName
                , foundNodeShape = "The (" <> tagName <> ") modifier expects exactly one numeric argument."
                }


handleYearsModifier, handleMonthsModifier, handleDaysModifier,
  handleHoursModifier, handleMinutesModifier, handleSecondsModifier,
  handleYearsEndModifier, handleMonthsEndModifier
    :: SourcePos -> [L.Expr] -> Reader Env L.Expr

handleYearsModifier   = mkDurationHandler "years"   Years
handleMonthsModifier  = mkDurationHandler "months"  Months
handleDaysModifier    = mkDurationHandler "days"    Days
handleHoursModifier   = mkDurationHandler "hours"   Hours
handleMinutesModifier = mkDurationHandler "minutes" Minutes
handleSecondsModifier = mkDurationHandler "seconds" Seconds

handleYearsEndModifier  = mkDurationHandler "years-end"  YearsToEnd
handleMonthsEndModifier = mkDurationHandler "months-end" MonthsToEnd

--- Shared Utility Predicates ---
parseISO8601 :: String -> Maybe UTCTime
parseISO8601 = I.parseDate


--- Strings ---
-- Each takes the string it works on first, so (-> label (trim) (upper-case)) threads. A
-- character is a Unicode code point, as for len.

-- Runs a built-in of a fixed arity: an error among its arguments is passed on, and the wrong
-- number of them is an arity error
withArity :: Text -> Int -> ([L.Expr] -> Reader Env L.Expr) -> SourcePos -> [L.Expr] -> Reader Env L.Expr
withArity name arity body pos args =
    case (find isError args, length args == arity) of
        (Just err, _)    -> pure err
        (Nothing, False) -> Data.Ouro.Error.Diagnostics.incorrectArity name arity (length args)
                            & OuroError pos
                            & L.EvalError
                            & pure
        (Nothing, True)  -> body args

-- An argument of the wrong kind, with what the built-in takes
wrongArgument :: SourcePos -> Text -> Text -> L.Expr -> Reader Env L.Expr
wrongArgument pos expected blurb other =
    Data.Ouro.Error.Diagnostics.typeMismatch expected (humanReadableType other)
    & withBlurb blurb
    & OuroError pos
    & L.EvalError
    & pure

string :: Text -> Reader Env L.Expr
string = pure . L.Primitive . I.String

boolean :: Bool -> Reader Env L.Expr
boolean = pure . L.Primitive . I.Boolean

-- A Number that is a whole number, as an Int
wholeNumber :: Double -> Maybe Int
wholeNumber n =
    case n == fromInteger (round n) of
        True  -> Just (round n)
        False -> Nothing


-- handleToString.
--
-- (to-string x): a String, Number, Boolean, URI or Date as a String, written as the JSON
-- output writes it, so (to-string 4.9) is "4.9" and (to-string 123456789) is "123456789".
handleToString :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleToString pos = withArity "to-string" 1 written pos
    where
    written = \case
        [L.Primitive (I.String s)]   -> string s
        [L.Primitive (I.Number n)]
            | not (isNaN n || isInfinite n)
                                     -> string (I.renderNumber n)
        [L.Primitive (I.Boolean b)]  -> string (case b of True -> "true"; False -> "false")
        [L.Primitive (I.URI uri)]    -> string (uriText uri)
        [L.Primitive (I.Date date)]  -> string (I.renderDate date)
        other : _                    -> wrongArgument pos "a String, a finite Number, a Boolean, a URI or a Date" toStringBlurb other
        []                           -> wrongArgument pos "a String, a finite Number, a Boolean, a URI or a Date" toStringBlurb (L.Array [])

    toStringBlurb = "to-string writes one value as text: (to-string 42) is \"42\". A Record or an Array has no single text; build one with join."


-- handleSplit.
--
-- (split s separator): the pieces of s between each separator, as an Array of Strings, so
-- (split "Manet, Monet" ", ") is ("Manet" "Monet"). A string without the separator is one piece.
handleSplit :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleSplit pos = withArity "split" 2 pieces pos
    where
    pieces = \case
        [L.Primitive (I.String s), L.Primitive (I.String sep)]
            | not (T.null sep) -> pure (L.Array (map (L.Primitive . I.String) (T.splitOn sep s)))
            | otherwise        -> wrongArgument pos "a separator of at least one character" splitBlurb (L.Primitive (I.String sep))
        [L.Primitive (I.String _), other] -> wrongArgument pos "a String to split at" splitBlurb other
        other : _                         -> wrongArgument pos "a String to split" splitBlurb other
        []                                -> wrongArgument pos "a String to split" splitBlurb (L.Array [])

    splitBlurb = "split takes the String first and the separator second: (split \"Manet, Monet\" \", \")."


-- handleJoin.
--
-- (join strings separator): an Array of Strings joined into one, with the separator between
-- each, so (join ("a" "b") ", ") is "a, b". An empty Array is "".
handleJoin :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleJoin pos = withArity "join" 2 joined pos
    where
    joined = \case
        [arr, L.Primitive (I.String sep)]
            | Just elements <- L.asArray arr
            -> case [ e | e <- elements, not (isString e) ] of
                   []        -> string (T.intercalate sep [ s | L.Primitive (I.String s) <- elements ])
                   other : _ -> wrongArgument pos "an Array of Strings to join" joinBlurb other
        [arr, other]
            | Just _ <- L.asArray arr     -> wrongArgument pos "a String to join with" joinBlurb other
        other : _                         -> wrongArgument pos "an Array of Strings to join" joinBlurb other
        []                                -> wrongArgument pos "an Array of Strings to join" joinBlurb (L.Array [])

    joinBlurb = "join takes the Array first and the separator second: (join (\"a\" \"b\") \", \"). Write a Number with to-string first: (map xs to-string)."
    isString  = \case
        L.Primitive (I.String _) -> True
        _notAString              -> False


-- handleUpperCase, handleLowerCase and handleTrim.
--
-- One String changed: every letter upper or lower case, or the white space at both ends removed.
handleUpperCase, handleLowerCase, handleTrim :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleUpperCase = oneString "upper-case" T.toUpper
handleLowerCase = oneString "lower-case" T.toLower
handleTrim      = oneString "trim"       T.strip

oneString :: Text -> (Text -> Text) -> SourcePos -> [L.Expr] -> Reader Env L.Expr
oneString name change pos = withArity name 1 changed pos
    where
    changed = \case
        [L.Primitive (I.String s)] -> string (change s)
        other : _                  -> wrongArgument pos "a String" ("(" <> name <> " s) takes one String.") other
        []                         -> wrongArgument pos "a String" ("(" <> name <> " s) takes one String.") (L.Array [])


-- handleStartsWith and handleEndsWith.
--
-- (starts-with s prefix), (ends-with s suffix): whether s begins or ends with the other String.
handleStartsWith, handleEndsWith :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleStartsWith = twoStrings "starts-with" T.isPrefixOf
handleEndsWith   = twoStrings "ends-with"   T.isSuffixOf

twoStrings :: Text -> (Text -> Text -> Bool) -> SourcePos -> [L.Expr] -> Reader Env L.Expr
twoStrings name test pos = withArity name 2 answer pos
    where
    answer = \case
        [L.Primitive (I.String s), L.Primitive (I.String part)] -> boolean (test part s)
        [L.Primitive (I.String _), other]                        -> wrongArgument pos "a String to look for" blurb other
        other : _                                                -> wrongArgument pos "a String to look in" blurb other
        []                                                       -> wrongArgument pos "a String to look in" blurb (L.Array [])
    blurb = "(" <> name <> " s part) takes the String to look in first: (" <> name <> " \"https://a.org\" \"https\")."


-- handleSlice.
--
-- (slice s start end): the characters of s from start up to, not including, end. A negative
-- index counts from the end, as in a path, so (slice "C-0042" 2 6) and (slice "C-0042" -4 6)
-- are both "0042". Indexes outside the String are an error, as an index past the end of an
-- Array is.
handleSlice :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleSlice pos = withArity "slice" 3 sliced pos
    where
    sliced = \case
        [L.Primitive (I.String s), L.Primitive (I.Number a), L.Primitive (I.Number b)]
            | Just i <- wholeNumber a, Just j <- wholeNumber b
            -> let size    = T.length s
                   from k  = case k < 0 of
                                 True  -> size + k
                                 False -> k
                   (i', j') = (from i, from j)
               in case 0 <= i' && i' <= j' && j' <= size of
                      True  -> string (T.take (j' - i') (T.drop i' s))
                      False -> Data.Ouro.Error.Diagnostics.typeMismatch
                                   ("a start and an end within the String's " <> T.pack (show size) <> " characters, the start first")
                                   (T.pack (show i <> " to " <> show j))
                               & withBlurb sliceBlurb
                               & OuroError pos
                               & L.EvalError
                               & pure
        [L.Primitive (I.String _), a, b] -> wrongArgument pos "a whole number for each of start and end" sliceBlurb (notWhole a b)
        other : _                        -> wrongArgument pos "a String to slice" sliceBlurb other
        []                               -> wrongArgument pos "a String to slice" sliceBlurb (L.Array [])

    sliceBlurb = "slice takes the characters from start up to, not including, end: (slice \"C-0042\" 2 6) is \"0042\". A negative index counts from the end."
    notWhole a b = case a of
        L.Primitive (I.Number n) | Just _ <- wholeNumber n -> b
        _notWhole                                          -> a


-- handlePadStart.
--
-- (pad-start s width fill): s with the fill character written before it until it is width
-- characters long, so (pad-start "42" 4 "0") is "0042". A String already that long is unchanged.
handlePadStart :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handlePadStart pos = withArity "pad-start" 3 padded pos
    where
    padded = \case
        [L.Primitive (I.String s), L.Primitive (I.Number w), L.Primitive (I.String fill)]
            | Just width <- wholeNumber w, width >= 0
            -> case T.uncons fill of
                   Just (c, rest) | T.null rest -> string (T.justifyRight width c s)
                   _notOneCharacter             -> wrongArgument pos "one character to pad with" padBlurb (L.Primitive (I.String fill))
        [L.Primitive (I.String _), w, _]
            | not (isWholeNumber w)       -> wrongArgument pos "a whole number of characters, at least 0" padBlurb w
        [L.Primitive (I.String _), _, other] -> wrongArgument pos "one character to pad with" padBlurb other
        other : _                         -> wrongArgument pos "a String to pad" padBlurb other
        []                                -> wrongArgument pos "a String to pad" padBlurb (L.Array [])

    padBlurb = "pad-start takes the String, its width and one character: (pad-start \"42\" 4 \"0\") is \"0042\"."
    isWholeNumber = \case
        L.Primitive (I.Number n) | Just k <- wholeNumber n -> k >= 0
        _notWhole                                          -> False


-- handleReplaceAll.
--
-- (replace-all s old new): s with every old replaced by new, so (replace-all "a b c" " " "-")
-- is "a-b-c".
handleReplaceAll :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleReplaceAll pos = withArity "replace-all" 3 replaced pos
    where
    replaced = \case
        [L.Primitive (I.String s), L.Primitive (I.String old), L.Primitive (I.String new)]
            | not (T.null old) -> string (T.replace old new s)
            | otherwise        -> wrongArgument pos "a String of at least one character to replace" replaceBlurb (L.Primitive (I.String old))
        [L.Primitive (I.String _), L.Primitive (I.String _), other] -> wrongArgument pos "a String to replace it with" replaceBlurb other
        [L.Primitive (I.String _), other, _]                         -> wrongArgument pos "a String to replace" replaceBlurb other
        other : _                                                    -> wrongArgument pos "a String to replace in" replaceBlurb other
        []                                                           -> wrongArgument pos "a String to replace in" replaceBlurb (L.Array [])

    replaceBlurb = "replace-all takes the String, what to replace and what to write instead: (replace-all \"a b\" \" \" \"-\") is \"a-b\"."


-- handleRange.
--
-- (start..end): the whole numbers from start to end, both included, as Haskell's [1..5]; an
-- end below the start is the empty Array, so (1..0) is (). (first second .. end) counts in
-- steps of second - first, as Haskell's [2,4..10], and down when the step is negative, so
-- (5 4 .. 1) is (5 4 3 2 1). A step of 0 would never end, and is an error.
handleRange :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleRange pos args =
    case (find isError args, traverse whole args) of
        (Just err, _)                    -> pure err
        (Nothing, Just [from, to])       -> numbers [from .. to]
        (Nothing, Just [from, next, to])
            | next == from               -> Data.Ouro.Error.Diagnostics.typeMismatch "a second number different from the first, to count in steps"
                                                                                     (T.pack (show from) <> " twice")
                                            & withBlurb rangeBlurb
                                            & OuroError pos
                                            & L.EvalError
                                            & pure
            | otherwise                  -> numbers [from, next .. to]
        (Nothing, _)                     -> wrongArgument pos "a whole number at each end of a range" rangeBlurb
                                                          (case filter (isNothing . whole) args of
                                                               notWhole : _ -> notWhole
                                                               []           -> L.Array args)
    where
    numbers ks = pure (L.Array [ L.Primitive (I.Number (fromIntegral k)) | k <- ks ])
    whole = \case
        L.Primitive (I.Number n) -> wholeNumber n
        _notANumber              -> Nothing
    rangeBlurb = "A range counts in whole numbers, both ends included: (1..5) is (1 2 3 4 5), and (2 4 .. 10) counts in steps, (2 4 6 8 10)."


-- handleZip.
--
-- (zip xs ys): the elements of two Arrays paired in order, each pair an Array of two, so
-- (zip (1 2) ("a" "b")) is ((1 "a") (2 "b")). Haskell's zip stops at the shorter list, for
-- zipping with an endless one; Ouro has none, so Arrays of different lengths are an error
-- rather than the longer one's last elements being lost.
handleZip :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleZip pos = withArity "zip" 2 paired pos
    where
    paired = \case
        [xs, ys]
            | Just left <- L.asArray xs, Just right <- L.asArray ys
            -> case (find isError (left <> right), length left == length right) of
                   (Just err, _) -> pure err
                   (_, True)     -> pure (L.Array [ L.Array [x, y] | (x, y) <- zip left right ])
                   (_, False)    -> Data.Ouro.Error.Diagnostics.typeMismatch "two Arrays of the same length"
                                                                             (T.pack (show (length left) <> " elements and " <> show (length right)))
                                    & withBlurb zipBlurb
                                    & OuroError pos
                                    & L.EvalError
                                    & pure
        [xs, ys] -> wrongArgument pos "an Array to zip" zipBlurb (case L.asArray xs of
                                                                      Nothing -> xs
                                                                      Just _  -> ys)
        other    -> wrongArgument pos "an Array to zip" zipBlurb (L.Array other)

    zipBlurb = "zip pairs two Arrays of the same length, element by element: (zip (1..3) (\"a\" \"b\" \"c\")) is ((1 \"a\") (2 \"b\") (3 \"c\"))."


-- handleMin and handleMax.
--
-- (min xs) and (max xs): the smallest or largest Number of an Array, so a summary is worked
-- out from the data, (max weights). They take one Array, as every built-in takes a fixed
-- number of arguments, and many values are an Array. Ordering compares Numbers only, and
-- there is no smallest of nothing.
handleMin, handleMax :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleMin = extreme "min" minimum
handleMax = extreme "max" maximum

extreme :: Text -> ([Double] -> Double) -> SourcePos -> [L.Expr] -> Reader Env L.Expr
extreme name pick pos = \case
    [err@(L.EvalError _)]
        -> pure err
    [arr]
        | Just elements <- L.asArray arr
        -> case (find isError elements, elements, [ e | e <- elements, not (isNumber e) ]) of
               (Just err, _, _) -> pure err
               (_, [], _)       -> Data.Ouro.Error.Diagnostics.typeMismatch "at least one Number" "an empty Array"
                                   & withBlurb blurb
                                   & OuroError pos
                                   & L.EvalError
                                   & pure
               (_, _, other : _) -> wrongArgument pos "Numbers to compare" blurb other
               (_, _, [])        -> pure (L.Primitive (I.Number (pick [ n | L.Primitive (I.Number n) <- elements ])))
    [other]
        -> wrongArgument pos "an Array of Numbers" blurb other
    args
        -> Data.Ouro.Error.Diagnostics.incorrectArity name 1 (length args)
           & withBlurb blurb
           & OuroError pos
           & L.EvalError
           & pure
    where
    blurb = "(" <> name <> " xs) takes one Array of Numbers, (" <> name <> " (3.72 3.36)); several Numbers are written as one, (" <> name <> " (a b c))."
