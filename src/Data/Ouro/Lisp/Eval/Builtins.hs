{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ouro.Lisp.Eval.Builtins
( builtinRegistry
, parseISO8601
, isNumber
) where

import           Control.Monad             (foldM)
import           Data.Ouro.Error.Types     (ErrorContext (..), OuroError (..),
                                            SyntaxError (..), TypeError (..))
import qualified Data.Ouro.Internal.Expr   as I
import           Data.Ouro.Lisp.Eval.Types (PeriodUnit (..),
                                            Value (Duration, Primitive, PrimitiveOp))
import qualified Data.Ouro.Lisp.Eval.Types as Ty
import           Data.Text                 (Text)
import           Data.Time                 (UTCTime (..))
import           Data.Time.Calendar        (addDays, addGregorianMonthsRollOver,
                                            addGregorianYearsRollOver)
import           Data.Time.Format          (defaultTimeLocale, parseTimeM)
import           Text.Megaparsec           (SourcePos)


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
handleAddition :: SourcePos -> [Value] -> Either OuroError Value
handleAddition pos =
    \case
     [] -> let context = Syntax MalformedTagPayload
                         { activeTag      = "+"
                         , foundNodeShape = "The addition operator requires at least one argument."
                         }
           in Left (OuroError pos context)

     -- Pure Numerical Addition Fallback
     nums | all isNumber nums -> do
                                 let sumVals = sum [n | Primitive (I.Number n) <- nums]
                                 pure $ Primitive (I.Number sumVals)

     -- Time-Shift Folding (e.g., Date + Duration + Duration)
     (baseVal : modifiers) -> foldM applyModifier baseVal modifiers

    where
    -- The inner fold for type-inference
    applyModifier :: Value -> Value -> Either OuroError Value
    applyModifier base modif =
        case (base, modif) of
            (Primitive (I.Date utc),    Duration unit amt)         -> pure $ Primitive (I.Date (applyDuration utc unit amt))
            (Primitive (I.String str1), Primitive (I.String str2)) -> pure $ Primitive (I.String (str1 <> str2))
            (Duration unit amt,         Primitive (I.Date utc))    -> pure $ Primitive (I.Date (applyDuration utc unit amt))
            (Primitive (I.Number n1),   Primitive (I.Number n2))   -> pure $ Primitive (I.Number (n1 + n2))
            _ -> let context = Typing TypeMismatch
                               { expectedType = "Matching numeric, string, or date/duration pairs for addition"
                               , actualType   = Ty.humanReadableType base <> " + " <> Ty.humanReadableType modif
                               }
                 in Left (OuroError pos context)


-- Variadic subtraction operator. Handles unary inversion and backward calendar traversal.
handleSubtraction :: SourcePos -> [Value] -> Either OuroError Value
handleSubtraction pos =
    \case
     []          -> let context = Syntax MalformedTagPayload
                                  { activeTag      = "-"
                                  , foundNodeShape = "The minus operator requires at least one argument."
                                  }
                    in Left (OuroError pos context)

     [singleVal] -> case singleVal of
                        Primitive (I.Number n) -> pure $ Primitive (I.Number (-n))
                        badArg                 -> let context = Typing TypeMismatch
                                                                { expectedType = "A plain Number for unary negation"
                                                                , actualType   = (Ty.humanReadableType badArg)
                                                                }
                                                  in Left (OuroError pos context)

     (baseVal : modifiers) -> foldM applySubtraction baseVal modifiers

    where
    -- A tiny custom subtraction worker that leverages the existing type system
    applySubtraction :: Value -> Value -> Either OuroError Value
    applySubtraction base modif =
        case (base, modif) of
            (Primitive (I.Number n1), Primitive (I.Number n2)) -> pure $ Primitive (I.Number (n1 - n2))
            -- Subtracting a duration moves the calendar backward (negate the amount)
            (Primitive (I.Date utc),  Duration unit amt)       -> pure $ Primitive (I.Date (applyDuration utc unit (-amt)))
            _ -> let context = Typing TypeMismatch
                                { expectedType = "Matching numeric values or a Date minus a Duration"
                                , actualType   = Ty.humanReadableType base <> " - " <> Ty.humanReadableType modif
                                }
                 in Left (OuroError pos context)


-- Variadic multiplication operator.
handleMultiplication :: SourcePos -> [Value] -> Either OuroError Value
handleMultiplication  pos = \case
                             [] -> let context = Syntax MalformedTagPayload
                                                 { activeTag      = "*"
                                                 , foundNodeShape = "The multiplication operator requires at least one argument."
                                                 }
                                   in Left (OuroError pos context)
                             (baseVal : modifiers) -> foldM applyMul baseVal modifiers

    where
    applyMul :: Value -> Value -> Either OuroError Value
    applyMul base modif = case (base, modif) of
                              (Primitive (I.Number n1), Primitive (I.Number n2)) -> pure $ Primitive (I.Number (n1 * n2))
                              _ -> let context = Typing TypeMismatch
                                                 { expectedType = "Matching numeric values for multiplication"
                                                 , actualType   = Ty.humanReadableType base <> " * " <> Ty.humanReadableType modif
                                                 }
                                   in Left (OuroError pos context)


-- Variadic division operator. Supports Lisp-style reciprocal syntax (/ x).
handleDivision :: SourcePos -> [Value] -> Either OuroError Value
handleDivision pos =
    \case
     []          -> let context = Syntax MalformedTagPayload
                                  { activeTag      = "/"
                                  , foundNodeShape = "The division operator requires at least one argument."
                                  }
                    in Left (OuroError pos context)
     -- Idiomatic Lisp: (/ 2) means 1 divided by 2 (reciprocal)
     [singleVal] -> case singleVal of
                        Primitive (I.Number 0) -> let context = Typing TypeMismatch
                                                                { expectedType = "A non-zero Number for reciprocal division"
                                                                , actualType   = "a zero (0)"
                                                                }
                                                  in Left (OuroError pos context)
                        Primitive (I.Number n) -> pure $ Primitive (I.Number (1.0 / n))
                        badArg                 -> let context = Typing TypeMismatch
                                                                { expectedType = "A plain Number for reciprocal division"
                                                                , actualType   = (Ty.humanReadableType badArg)
                                                                }
                                                  in Left (OuroError pos context)

     (baseVal : modifiers) -> foldM applyDiv baseVal modifiers

    where
    applyDiv :: Value -> Value -> Either OuroError Value
    applyDiv base modif = case (base, modif) of
                              (Primitive (I.Number _),  Primitive (I.Number 0))  -> let context = Typing TypeMismatch
                                                                                                  { expectedType = "A non-zero Number divisor"
                                                                                                  , actualType   = "a zero (0)"
                                                                                                  }
                                                                                    in Left (OuroError pos context)
                              (Primitive (I.Number n1), Primitive (I.Number n2)) -> pure $ Primitive (I.Number (n1 / n2))
                              _ -> let context = Typing TypeMismatch
                                                 { expectedType = "Matching numeric values for division"
                                                 , actualType   = Ty.humanReadableType base <> " / " <> Ty.humanReadableType modif
                                                 }
                                   in Left (OuroError pos context)


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
handleYearsModifier :: SourcePos -> [Value] -> Either OuroError Value
handleYearsModifier pos =
    \case
     [Primitive (I.Number n)] -> pure $ Duration Years (floor n)
     [badArg]                 -> let context = Typing TypeMismatch
                                               { expectedType = "A Number representing the amount of years"
                                               , actualType   = (Ty.humanReadableType badArg)
                                               }
                                 in Left (OuroError pos context)
     _                        -> let context = Syntax MalformedTagPayload
                                               { activeTag      = "years"
                                               , foundNodeShape = "The (years) modifier expects exactly one numeric argument."
                                               }
                                 in Left (OuroError pos context)


-- Handle for the (months <num>) duration modifier
handleMonthsModifier :: SourcePos -> [Value] -> Either OuroError Value
handleMonthsModifier pos =
    \case
     [Primitive (I.Number n)] -> pure $ Duration Months (floor n)
     [badArg]                 -> let context = Typing TypeMismatch
                                               { expectedType = "A Number representing the amount of months"
                                               , actualType   = (Ty.humanReadableType badArg)
                                               }
                                 in Left (OuroError pos context)
     _                        -> let context = Syntax MalformedTagPayload
                                               { activeTag      = "months"
                                               , foundNodeShape = "The (months) modifier expects exactly one numeric argument."
                                               }
                                 in Left (OuroError pos context)


-- Handle for the (days <num>) duration modifier
handleDaysModifier :: SourcePos -> [Value] -> Either OuroError Value
handleDaysModifier pos =
    \case
     [Primitive (I.Number n)] -> pure $ Duration Days (floor n)
     [badArg]                 -> let context = Typing TypeMismatch
                                               { expectedType = "A Number representing the amount of days"
                                               , actualType   = (Ty.humanReadableType badArg)
                                               }
                                 in Left (OuroError pos context)
     _                        -> let context = Syntax MalformedTagPayload
                                               { activeTag      = "days"
                                               , foundNodeShape = "The (days) modifier expects exactly one numeric argument."
                                               }
                                 in Left (OuroError pos context)


--- Shared Utility Predicates ---
parseISO8601 :: String -> Maybe UTCTime
parseISO8601 s = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" s
