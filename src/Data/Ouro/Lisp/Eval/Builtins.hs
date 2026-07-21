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
import qualified Data.Map                    as Map
import           Data.Ouro.Error.Diagnostics (astCorruption,
                                              binaryOpMismatchBlurb,
                                              typeMismatch, withBlurb)
import           Data.Ouro.Error.Types       (ErrorContext (..), OuroError (..),
                                              SyntaxError (..))
import qualified Data.Ouro.Internal.Expr     as I
import           Data.Ouro.Lisp.Eval.Types   (Env, PeriodUnit (..),
                                              humanReadableType)
import qualified Data.Ouro.Lisp.Eval.Types   as L
import           Data.Text                   (Text)
import           Data.Time                   (UTCTime (..), addUTCTime,
                                              fromGregorian,
                                              gregorianMonthLength, toGregorian)
import           Data.Time.Calendar          (addDays,
                                              addGregorianMonthsRollOver,
                                              addGregorianYearsRollOver)
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
                (arrX@(L.Array xs),          arrY@(L.Array ys))
                    -> case L.structuralEq arrX arrY of
                           True  -> pure $ L.Array (xs <> ys)
                           False -> Data.Ouro.Error.Diagnostics.typeMismatch
                                        "Matching Arrays of the same type"
                                        (humanReadableType arrX <> " + " <> humanReadableType arrY)
                                    & Data.Ouro.Error.Diagnostics.withBlurb (Data.Ouro.Error.Diagnostics.binaryOpMismatchBlurb arrX arrY)
                                    & OuroError pos
                                    & L.EvalError
                                    & pure


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

     -- Arity Fallback: The desugar pass failed to normalize the AST
     _badAST
         -> Data.Ouro.Error.Diagnostics.astCorruption "+" "Addition received non-binary arguments after desugaring."
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

     -- Arity Fallback: The desugar pass failed to normalize the AST
     _badAST
         -> Data.Ouro.Error.Diagnostics.astCorruption "-" "Addition received non-binary arguments after desugaring."
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

     -- Arity Fallback: The desugar pass failed to normalize the AST
     _badAST
         -> Data.Ouro.Error.Diagnostics.astCorruption "*" "Addition received non-binary arguments after desugaring."
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

     -- Arity Fallback: The desugar pass failed to normalize the AST
     _badAST
         -> Data.Ouro.Error.Diagnostics.astCorruption "/" "Division received non-binary arguments after desugaring."
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

     -- Arity Fallback: The desugar pass failed to normalize the AST
     _badAST
         -> Data.Ouro.Error.Diagnostics.astCorruption ">" "Greater than received non-binary arguments after desugaring."
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

     -- Arity Fallback: The desugar pass failed to normalize the AST
     _badAST
         -> Data.Ouro.Error.Diagnostics.astCorruption ">=" "Greater than or equal to, received non-binary arguments after desugaring."
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

     -- Arity Fallback
     _badAST
         -> Data.Ouro.Error.Diagnostics.astCorruption "<" "Less than received non-binary arguments after desugaring."
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

     -- Arity Fallback
     _badAST
         -> Data.Ouro.Error.Diagnostics.astCorruption "<=" "Less than or equal to, received non-binary arguments after desugaring."
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

                   -- Comparison using structural equality
                   (v1, v2) -> pure $ L.Primitive (I.Boolean (v1 == v2))

     -- Arity Fallback: The desugar pass failed to normalize the AST
     _badAST
         -> Data.Ouro.Error.Diagnostics.astCorruption "eq" "Equality received non-binary arguments after desugaring."
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

                   -- Comparison using structural equality
                   (v1, v2) -> pure $ L.Primitive (I.Boolean (v1 /= v2))

     -- Arity Fallback: The desugar pass failed to normalize the AST
     _badAST
         -> Data.Ouro.Error.Diagnostics.astCorruption "not" "Equality received non-binary arguments after desugaring."
             & OuroError pos
             & L.EvalError
             & pure


isNumber :: L.Expr -> Bool
isNumber = \case
            (L.Primitive (I.Number _)) -> True
            _notANum                   -> False


--- Time Shifting & L.Duration Modifiers ---
-- Direct calendar shifting calendar logic
applyDuration :: Integral a => UTCTime -> PeriodUnit -> a -> UTCTime
applyDuration utc unit amt = case unit of
                                 Years   -> utc { utctDay = addGregorianYearsRollOver (fromIntegral amt) (utctDay utc) }
                                 Months  -> utc { utctDay = addGregorianMonthsRollOver (fromIntegral amt) (utctDay utc) }
                                 Days    -> utc { utctDay = addDays (fromIntegral amt) (utctDay utc) }
                                 Hours   -> addUTCTime (fromIntegral amt * 3600) utc
                                 Minutes -> addUTCTime (fromIntegral amt * 60) utc
                                 Seconds -> addUTCTime (fromIntegral amt) utc

                                 -- Runtime handles for dynamic calendar snapping
                                 YearsToEnd -> let advancedDay = addGregorianYearsRollOver (fromIntegral amt) (utctDay utc)
                                                   (y, _, _)   = toGregorian advancedDay
                                               in utc { utctDay = fromGregorian y 12 31, utctDayTime = 86399 }

                                 MonthsToEnd -> let advancedDay = addGregorianMonthsRollOver (fromIntegral amt) (utctDay utc)
                                                    (y, m, _)   = toGregorian advancedDay
                                                    lastDay     = gregorianMonthLength y m
                                                in utc { utctDay = fromGregorian y m lastDay, utctDayTime = 86399 }




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
