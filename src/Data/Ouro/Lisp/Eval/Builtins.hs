{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ouro.Lisp.Eval.Builtins
( builtinRegistry
, parseISO8601
, isNumber
) where

import           Control.Monad.Reader        (Reader)
import           Data.Function               ((&))
import           Data.List                   (find, nub)
import qualified Data.Map                    as Map
import           Data.Ouro.Error.Diagnostics (astCorruption,
                                              binaryOpMismatchBlurb,
                                              incorrectArity, propertyMismatch,
                                              typeMismatch, withBlurb)
import           Data.Ouro.Error.Types       (ErrorContext (..), OuroError (..),
                                              SyntaxError (..))
import qualified Data.Ouro.Internal.Expr     as I
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
import           Data.Time.Format            (defaultTimeLocale, parseTimeM)
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
                      , ("or",          handleOr)
                      , ("not",         handleNot)
                      , ("len",         handleLength)
                      , ("contains",    handleContains)
                      , ("distinct",    handleDistinct)
                      -- Internal: emitted only by the desugarer for chained comparisons
                      , ("%and",        handleAnd)
                      , ("years",       handleYearsModifier)
                      , ("months",      handleMonthsModifier)
                      , ("days",        handleDaysModifier)
                      , ("hours",       handleHoursModifier)
                      , ("minutes",     handleMinutesModifier)
                      , ("seconds",     handleSecondsModifier)
                      -- Runtime temporal handlers
                      , ("years-end",   handleYearsEndModifier)
                      , ("months-end",  handleMonthsEndModifier)
                      ]


--- Core Math & String Accumulators ---
-- Note: Variadic forms are unrolled during the compiler's desugar pass.

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
                       -> Data.Ouro.Error.Diagnostics.typeMismatch "Matching numeric values or a Date minus a L.Duration"
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


-- handleAnd.
--
-- Binary logical conjunction. Internal only: the desugar pass emits it to
-- chain variadic comparisons, e.g. (< 1 2 3) -> (%and (< 1 2) (< 2 3)).
handleAnd :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleAnd pos =
    \case
     [a, b] -> case (a, b) of
                   -- Error propagation
                   (err@(L.EvalError _), _) -> pure err
                   (_, err@(L.EvalError _)) -> pure err

                   (L.Primitive (I.Boolean b1), L.Primitive (I.Boolean b2)) -> pure $ L.Primitive (I.Boolean (b1 && b2))

                   _typeMismatch
                       -> Data.Ouro.Error.Diagnostics.typeMismatch "Boolean results from a chained comparison"
                               (humanReadableType a <> " and " <> humanReadableType b)
                          & OuroError pos
                          & L.EvalError
                          & pure

     -- Arity Fallback: The desugar pass failed to normalize the AST
     _badAST
         -> Data.Ouro.Error.Diagnostics.astCorruption "%and" "Conjunction received non-binary arguments after desugaring."
             & OuroError pos
             & L.EvalError
             & pure


-- handleOr.
--
-- Binary disjunction of two Booleans. As for any built-in, both operands are evaluated
-- first; (some a b ...) is the form that stops at the first true operand.
handleOr :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleOr pos =
    \case
     [a, b] -> case (a, b) of
                   -- Error propagation
                   (err@(L.EvalError _), _) -> pure err
                   (_, err@(L.EvalError _)) -> pure err

                   (L.Primitive (I.Boolean b1), L.Primitive (I.Boolean b2)) -> pure $ L.Primitive (I.Boolean (b1 || b2))

                   (L.Primitive (I.Boolean _), notBool) -> notABoolean notBool
                   (notBool, _)                         -> notABoolean notBool

     -- The wrong number of operands, as written: each operator compares or combines two values
     operands
         -> wrongOperandCount "or" operands
            & OuroError pos
            & L.EvalError
            & pure

    where
    notABoolean value = Data.Ouro.Error.Diagnostics.propertyMismatch "#bool" (L.propertyOf value)
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
         -> Data.Ouro.Error.Diagnostics.propertyMismatch "#bool" (L.propertyOf other)
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


-- An operator written with the wrong number of operands, e.g. (+ 1) or (neq 1 2 3).
-- Variadic arithmetic and comparisons are unrolled into pairs by the desugarer, so only
-- a count that is wrong as written reaches a handler.
wrongOperandCount :: Text -> [L.Expr] -> ErrorContext
wrongOperandCount op operands = Data.Ouro.Error.Diagnostics.incorrectArity op 2 (length operands)


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
-- code points, not bytes, so "Málaga" has 6. Recursion is banned, so a length cannot be
-- written in Ouro itself.
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
                _noProblem    -> pure $ L.Array (nub elements)

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
parseISO8601 s = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" s
