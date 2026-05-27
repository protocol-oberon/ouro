{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ouro.Lisp.Eval.Builtins
( builtinRegistry
, parseISO8601
, isNumber
) where

import           Control.Monad             (foldM)
import qualified Data.Ouro.Internal.Expr   as I
import           Data.Ouro.Lisp.Eval.Types (PeriodUnit (..),
                                            Value (Duration, Primitive, PrimitiveOp))
import           Data.Text                 (Text)
import           Data.Time                 (UTCTime (..))
import           Data.Time.Calendar        (addDays, addGregorianMonthsRollOver,
                                            addGregorianYearsRollOver)
import           Data.Time.Format          (defaultTimeLocale, parseTimeM)


-- Maps syntax strings to their respective first-class execution handles
builtinRegistry :: Text -> Maybe Value
builtinRegistry = \case
                   "+"      -> Just $ PrimitiveOp handleAddition
                   "-"      -> Just $ PrimitiveOp handleSubtraction
                   "*"      -> Just $ PrimitiveOp handleMultiplication
                   "/"      -> Just $ PrimitiveOp handleDivision
                   "years"  -> Just $ PrimitiveOp handleYearsModifier
                   "months" -> Just $ PrimitiveOp handleMonthsModifier
                   "days"   -> Just $ PrimitiveOp handleDaysModifier
                   _        -> Nothing


--- Core Math & String Accumulators ---
-- Variadic addition operator. Folds across integers, string structures,
-- and handles polymorphic datetime shifting.
handleAddition :: [Value] -> Either String Value
handleAddition = \case
                  [] -> Left "Type Error: '+' operator requires at least one argument."

                  -- Pure Numerical Addition Fallback
                  nums | all isNumber nums -> do
                                              let sumVals = sum [n | Primitive (I.Number n) <- nums]
                                              pure $ Primitive (I.Number sumVals)

                  -- Time-Shift Folding (e.g., Date + Duration + Duration)
                  (baseVal : modifiers) -> foldM applyModifier baseVal modifiers

    where
    -- The inner fold for type-inference
    applyModifier :: Value -> Value -> Either String Value
    applyModifier base modif =
        case (base, modif) of
            (Primitive (I.Date utc),    Duration unit amt)         -> pure $ Primitive (I.Date (applyDuration utc unit amt))
            (Primitive (I.String str1), Primitive (I.String str2)) -> pure $ Primitive (I.String (str1 <> str2))
            (Duration unit amt,         Primitive (I.Date utc))    -> pure $ Primitive (I.Date (applyDuration utc unit amt))
            (Primitive (I.Number n1),   Primitive (I.Number n2))   -> pure $ Primitive (I.Number (n1 + n2))
            _ -> Left "Type Error: Incompatible operand types encountered inside '+' addition chain."


-- Variadic subtraction operator. Handles unary inversion and backward calendar traversal.
handleSubtraction :: [Value] -> Either String Value
handleSubtraction = \case
                     []          -> Left "Type Error: '-' operator requires at least one argument."
                     [singleVal] -> case singleVal of
                                        Primitive (I.Number n) -> pure $ Primitive (I.Number (-n))
                                        _                      -> Left "Type Error: Unary negation requires a Number."

                     (baseVal : modifiers) -> foldM applySubtraction baseVal modifiers

    where
    -- A tiny custom subtraction worker that leverages your existing type system
    applySubtraction :: Value -> Value -> Either String Value
    applySubtraction base modif =
        case (base, modif) of
            (Primitive (I.Number n1), Primitive (I.Number n2)) -> pure $ Primitive (I.Number (n1 - n2))
            -- Subtracting a duration moves the calendar backward (negate the amount)
            (Primitive (I.Date utc),  Duration unit amt)       -> pure $ Primitive (I.Date (applyDuration utc unit (-amt)))
            _ -> Left "Type Error: Incompatible operand types encountered inside '-' subtraction chain."


-- Variadic multiplication operator.
handleMultiplication :: [Value] -> Either String Value
handleMultiplication  = \case
                         [] -> Left "Type Error: '*' operator requires at least one argument"
                         (baseVal : modifiers) -> foldM applyMul baseVal modifiers

    where
    applyMul :: Value -> Value -> Either String Value
    applyMul base modif = case (base, modif) of
                              (Primitive (I.Number n1), Primitive (I.Number n2)) -> pure $ Primitive (I.Number (n1 * n2))
                              _ -> Left "Type Error: Incompatible operand types encountered inside '*' subtraction chain."


-- Variadic division operator. Supports Lisp-style reciprocal syntax (/ x).
handleDivision :: [Value] -> Either String Value
handleDivision = \case
                  []          -> Left "Type Error: '/' operator requires at least one argument."
                  -- Idiomatic Lisp: (/ 2) means 1 divided by 2 (reciprocal)
                  [singleVal] -> case singleVal of
                                     Primitive (I.Number 0) -> Left "Math Error: Division by zero encountered in reciprocal."
                                     Primitive (I.Number n) -> pure $ Primitive (I.Number (1.0 / n))
                                     _                      -> Left "Type Error: Reciprocal requires a Number."

                  (baseVal : modifiers) -> foldM applyDiv baseVal modifiers

    where
    applyDiv :: Value -> Value -> Either String Value
    applyDiv base modif = case (base, modif) of
                              (Primitive (I.Number _),  Primitive (I.Number 0))  -> Left "Math Error: Attempted division by zero."
                              (Primitive (I.Number n1), Primitive (I.Number n2)) -> pure $ Primitive (I.Number (n1 / n2))
                              _ -> Left "Type Error: Incompatible operand types encountered inside '/' division chain."


isNumber :: Value -> Bool
isNumber = \case
            (Primitive (I.Number _)) -> True
            _                        -> False


--- Time Shifting & Duration Modifiers ---
-- Direct calendar shifting calendar logic
applyDuration :: Integral a => UTCTime -> PeriodUnit -> a -> UTCTime
applyDuration utc unit amt = case unit of
                                 Years  -> utc { utctDay = addGregorianYearsRollOver (fromIntegral amt) (utctDay utc) }
                                 Months -> utc { utctDay = addGregorianMonthsRollOver (fromIntegral amt) (utctDay utc) }
                                 Days   -> utc { utctDay = addDays (fromIntegral amt) (utctDay utc) }


-- Handle for the (years <num>) duration modifier
handleYearsModifier :: [Value] -> Either String Value
handleYearsModifier = \case
                       [Primitive (I.Number n)] -> pure $ Duration Years (floor n)
                       _                        -> Left "Type Error: (years) modifier expects exactly one numeric argument."


-- Handle for the (months <num>) duration modifier
handleMonthsModifier :: [Value] -> Either String Value
handleMonthsModifier = \case
                        [Primitive (I.Number n)] -> pure $ Duration Months (floor n)
                        _                        -> Left "Type Error: (months) modifier expects exactly one numeric argument."


-- Handle for the (days <num>) duration modifier
handleDaysModifier :: [Value] -> Either String Value
handleDaysModifier = \case
                      [Primitive (I.Number n)] -> pure $ Duration Days (floor n)
                      _                        -> Left "Type Error: (days) modifier expects exactly one numeric argument."


--- Shared Utility Predicates ---
parseISO8601 :: String -> Maybe UTCTime
parseISO8601 s = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" s
