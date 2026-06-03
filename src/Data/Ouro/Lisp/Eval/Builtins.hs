{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ouro.Lisp.Eval.Builtins
( builtinRegistry
, parseISO8601
, isNumber
) where

import           Control.Monad               (foldM)
import           Control.Monad.Reader        (Reader)
import           Data.Function               ((&))
import           Data.Ouro.Error.Diagnostics (binaryOpMismatchBlurb,
                                              typeMismatch, withBlurb)
import           Data.Ouro.Error.Types       (ErrorContext (..), OuroError (..),
                                              SyntaxError (..))
import qualified Data.Ouro.Internal.Expr     as I
import           Data.Ouro.Lisp.Eval.Types   (Env, PeriodUnit (..),
                                              humanReadableType)
import qualified Data.Ouro.Lisp.Eval.Types   as L
import           Data.Text                   (Text)
import           Data.Time                   (UTCTime (..))
import           Data.Time.Calendar          (addDays,
                                              addGregorianMonthsRollOver,
                                              addGregorianYearsRollOver)
import           Data.Time.Format            (defaultTimeLocale, parseTimeM)
import           Text.Megaparsec             (SourcePos)


-- Maps syntax strings to their respective first-class execution handles
builtinRegistry :: Text -> Maybe L.Expr
builtinRegistry = \case
                   "+"      -> Just $ L.PrimitiveOp handleAddition
                   "-"      -> Just $ L.PrimitiveOp handleSubtraction
                   "*"      -> Just $ L.PrimitiveOp handleMultiplication
                   "/"      -> Just $ L.PrimitiveOp handleDivision
                   "years"  -> Just $ L.PrimitiveOp handleYearsModifier
                   "months" -> Just $ L.PrimitiveOp handleMonthsModifier
                   "days"   -> Just $ L.PrimitiveOp handleDaysModifier
                   _        -> Nothing


--- Core Math & String Accumulators ---
--  handleAddition.
--
-- Variadic addition operator. Folds across numbers, string concatenations,
-- and polymorphic datetime shifts. Operates purely within EvalM.
handleAddition :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleAddition pos args =
    case args of
        [] -> pure $ L.EvalError $ OuroError pos $ Syntax $ MalformedTagPayload
                { activeTag      = "+"
                , foundNodeShape = "The addition operator requires at least one argument."
                }

        -- Pure Numerical Addition Fallback
        nums | all isNumber nums ->
            pure $ L.Primitive $ I.Number $ sum [n | L.Primitive (I.Number n) <- nums]

        -- Time-Shift or String Folding
        (baseVal : modifiers) -> foldM applyModifier baseVal modifiers

    where
    applyModifier :: L.Expr -> L.Expr -> Reader Env L.Expr
    applyModifier base modif =
        case (base, modif) of
            (L.Primitive (I.Date utc),   L.Duration unit amt) -> pure $ L.Primitive (I.Date (applyDuration utc unit amt))
            (L.Primitive (I.String s1),  L.Primitive (I.String s2)) -> pure $ L.Primitive (I.String (s1 <> s2))
            (L.Duration unit amt,        L.Primitive (I.Date utc))  -> pure $ L.Primitive (I.Date (applyDuration utc unit amt))
            (L.Primitive (I.Number n1),  L.Primitive (I.Number n2)) -> pure $ L.Primitive (I.Number (n1 + n2))

            -- Error propagation
            (err@(L.EvalError _), _) -> pure err
            (_, err@(L.EvalError _)) -> pure err

            _ -> pure $ L.EvalError $ OuroError pos $
                   typeMismatch "Matching numeric, string, or date/duration pairs"
                                (humanReadableType base <> " + " <> humanReadableType modif)
                   & withBlurb (binaryOpMismatchBlurb base modif)


-- handleSubtraction.
--
-- Variadic subtraction operator. Handles unary inversion and backward calendar
-- traversal. Operates purely within the Reader monad.
handleSubtraction :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleSubtraction pos args =
    case args of
        [] -> pure $ L.EvalError $ OuroError pos $ Syntax $ MalformedTagPayload
                { activeTag      = "-"
                , foundNodeShape = "The minus operator requires at least one argument."
                }

        [singleVal] -> case singleVal of
            L.Primitive (I.Number n) -> pure $ L.Primitive (I.Number (-n))
            err@(L.EvalError _)      -> pure err
            badArg                   -> typeMismatch "A plain Number for unary negation"
                                                     (humanReadableType badArg)
                                        & withBlurb ( "Unary negation is only supported for numeric types. "
                                                   <> "Ensure the value is a number or check your scope."
                                                   )
                                        & OuroError pos
                                        & L.EvalError
                                        & pure

        (baseVal : modifiers) -> foldM applySubtraction baseVal modifiers

    where
    applySubtraction :: L.Expr -> L.Expr -> Reader Env L.Expr
    applySubtraction base modif =
        case (base, modif) of
            (L.Primitive (I.Number n1), L.Primitive (I.Number n2))
                -> pure $ L.Primitive (I.Number (n1 - n2))

            -- Subtracting a duration moves the calendar backward
            (L.Primitive (I.Date utc), L.Duration unit amt)
                -> pure $ L.Primitive (I.Date (applyDuration utc unit (-amt)))

            -- Error propagation
            (err@(L.EvalError _), _) -> pure err
            (_, err@(L.EvalError _)) -> pure err

            _ -> pure $ L.EvalError $ OuroError pos $
                    typeMismatch "Matching numeric values or a Date minus a L.Duration"
                                 (humanReadableType base <> " - " <> humanReadableType modif)
                    & withBlurb ("The '-' operator expects either two numbers or a Date and a L.Duration. "
                              <> "Please verify the types of your operands.")


-- handleMultiplication.
--
-- Variadic multiplication operator. Folds across numeric values.
-- Operates purely within the Reader monad.
handleMultiplication :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleMultiplication pos args =
    case args of
        [] -> pure $ L.EvalError $ OuroError pos $ Syntax $ MalformedTagPayload
                { activeTag      = "*"
                , foundNodeShape = "The multiplication operator requires at least one argument."
                }

        (baseVal : modifiers) -> foldM applyMul baseVal modifiers

    where
    applyMul :: L.Expr -> L.Expr -> Reader Env L.Expr
    applyMul base modif =
        case (base, modif) of
            (L.Primitive (I.Number n1), L.Primitive (I.Number n2))
                -> pure $ L.Primitive (I.Number (n1 * n2))

            -- Error propagation
            (err@(L.EvalError _), _) -> pure err
            (_, err@(L.EvalError _)) -> pure err

            _ -> pure $ L.EvalError $ OuroError pos $
                    typeMismatch "Matching numeric values for multiplication"
                                 (humanReadableType base <> " * " <> humanReadableType modif)
                    & withBlurb ("Multiplication is only supported for numeric values. "
                              <> "Please ensure all operands are Numbers.")


-- handleDivision.
--
-- Variadic division operator. Supports Lisp-style reciprocal syntax (/ x).
-- Operates purely within the Reader monad.
handleDivision :: SourcePos -> [L.Expr] -> Reader Env L.Expr
handleDivision pos args =
    case args of
        [] -> pure $ L.EvalError $ OuroError pos $ Syntax $ MalformedTagPayload
                { activeTag      = "/"
                , foundNodeShape = "The division operator requires at least one argument."
                }

        -- Idiomatic Lisp: (/ 2) means 1 / 2 (reciprocal)
        [singleVal] -> case singleVal of
            L.Primitive (I.Number 0) -> pure $ L.EvalError $ OuroError pos $
                                        typeMismatch "A non-zero Number for reciprocal division" "a zero (0)"
                                        & withBlurb "Division by zero is undefined in Ouro numeric arithmetic."
            L.Primitive (I.Number n) -> pure $ L.Primitive (I.Number (1.0 / n))
            err@(L.EvalError _)      -> pure err
            badArg                   -> pure $ L.EvalError $ OuroError pos $
                                        typeMismatch "A plain Number for reciprocal division" (humanReadableType badArg)
                                        & withBlurb "The division operator expects a numeric value."

        (baseVal : modifiers) -> foldM applyDiv baseVal modifiers

    where
    applyDiv :: L.Expr -> L.Expr -> Reader Env L.Expr
    applyDiv base modif =
        case (base, modif) of
            (L.Primitive (I.Number _), L.Primitive (I.Number 0))
                -> pure $ L.EvalError $ OuroError pos $
                         typeMismatch "A non-zero Number divisor" "a zero (0)"
                         & withBlurb "Attempted division by zero."

            (L.Primitive (I.Number n1), L.Primitive (I.Number n2))
                -> pure $ L.Primitive (I.Number (n1 / n2))

            -- Error propagation
            (err@(L.EvalError _), _) -> pure err
            (_, err@(L.EvalError _)) -> pure err

            _ -> pure $ L.EvalError $ OuroError pos $
                    typeMismatch "Matching numeric values for division"
                                 (humanReadableType base <> " / " <> humanReadableType modif)
                    & withBlurb "The division operator only supports numeric operands."


isNumber :: L.Expr -> Bool
isNumber = \case
            (L.Primitive (I.Number _)) -> True
            _                          -> False


--- Time Shifting & L.Duration Modifiers ---
-- Direct calendar shifting calendar logic
applyDuration :: Integral a => UTCTime -> PeriodUnit -> a -> UTCTime
applyDuration utc unit amt = case unit of
                                 Years  -> utc { utctDay = addGregorianYearsRollOver (fromIntegral amt) (utctDay utc) }
                                 Months -> utc { utctDay = addGregorianMonthsRollOver (fromIntegral amt) (utctDay utc) }
                                 Days   -> utc { utctDay = addDays (fromIntegral amt) (utctDay utc) }


-- Shared helper for duration modifiers to ensure consistent error handling.
mkDurationHandler :: Text -> PeriodUnit -> SourcePos -> [L.Expr] -> Reader Env L.Expr
mkDurationHandler tagName unit pos args =
    case args of
        [L.Primitive (I.Number n)] -> pure $ L.Duration unit (floor n)
        [badArg]                   -> typeMismatch ("A Number representing the amount of " <> tagName)
                                                   (humanReadableType badArg)
                                      & withBlurb ( "The (" <> tagName <> ") modifier expects a single numeric argument "
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

-- | Implementation of handlers using the helper
handleYearsModifier, handleMonthsModifier, handleDaysModifier
    :: SourcePos -> [L.Expr] -> Reader Env L.Expr

handleYearsModifier  = mkDurationHandler "years"  Years
handleMonthsModifier = mkDurationHandler "months" Months
handleDaysModifier   = mkDurationHandler "days"   Days


--- Shared Utility Predicates ---
parseISO8601 :: String -> Maybe UTCTime
parseISO8601 s = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" s
