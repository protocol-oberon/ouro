module Data.Ouro.Internal.Duration
( Duration (..)
, Unit (..)
, parseDuration
, renderDuration
, unitDuration
, largestUnit
) where

import           Control.Monad   (guard)
import           Data.Char       (isDigit)
import           Data.Maybe      (isJust)
import           Data.Scientific (FPFormat (Fixed), Scientific, formatScientific, isInteger)
import           Data.Text       (Text)
import qualified Data.Text       as T


-- Duration.
--
-- A duration as xsd:duration writes it, PnYnMnDTnHnMnS: each part kept as written or left out,
-- so a duration remembers its units, (months 0) being months, which a shift through the end of
-- its largest unit needs. Only the seconds may have a fraction, as a calendar cannot apply half
-- a month exactly. A leading - is xsd:duration's, kept so that a negative duration written the
-- old way still reads; Ouro's own durations are magnitudes, the direction the operation's.
data Duration = Duration
    { negative :: Bool
    , years    :: Maybe Integer
    , months   :: Maybe Integer
    , days     :: Maybe Integer
    , hours    :: Maybe Integer
    , minutes  :: Maybe Integer
    , seconds  :: Maybe Scientific
    }
    deriving (Show, Eq)


-- The units a duration is written in, largest first
data Unit
    = Years
    | Months
    | Days
    | Hours
    | Minutes
    | Seconds
    deriving (Show, Eq, Ord, Enum, Bounded)


-- A duration from its text, if it is one: an optional -, P, the date's parts in order, years,
-- months, days, each optional, then T and the time's, hours, minutes, seconds, at least one
-- after a T and at least one in all
parseDuration :: Text -> Maybe Duration
parseDuration written = do
    let (isNegative, unsigned) = maybe (False, written) (\rest -> (True, rest)) (T.stripPrefix "-" written)
    body <- T.stripPrefix "P" unsigned
    let (datePart, timePart) = T.breakOn "T" body
    (y, r1)  <- whole 'Y' datePart
    (mo, r2) <- whole 'M' r1
    (d, r3)  <- whole 'D' r2
    guard (T.null r3)
    (h, mi, s) <- case T.stripPrefix "T" timePart of
        Nothing -> pure (Nothing, Nothing, Nothing)
        Just time -> do
            (h, t1)  <- whole 'H' time
            (mi, t2) <- whole 'M' t1
            (s, t3)  <- decimal t2
            guard (T.null t3 && (isJust h || isJust mi || isJust s))
            pure (h, mi, s)
    let duration = Duration isNegative y mo d h mi s
    guard (isJust (largestUnit duration))
    pure duration

    where
    -- A whole number followed by its unit's letter, or nothing taken when the next part is
    -- another unit's
    whole unit text = case T.span isDigit text of
        ("", _)        -> Just (Nothing, text)
        (digits, rest) -> case T.uncons rest of
            Just (letter, after) | letter == unit -> Just (Just (read (T.unpack digits)), after)
            _otherUnit                            -> Just (Nothing, text)

    -- The seconds, a decimal, followed by S
    decimal text = case T.span (\c -> isDigit c || c == '.') text of
        ("", _)        -> Just (Nothing, text)
        (number, rest) -> do
            after <- T.stripPrefix "S" rest
            guard (validDecimal number)
            pure (Just (read (T.unpack (padded number))), after)

    validDecimal number = case T.splitOn "." number of
        [int]       -> not (T.null int)
        [int, frac] -> not (T.null int) && not (T.null frac)
        _tooMany    -> False

    padded number = case T.isSuffixOf "." number of
        True  -> number <> "0"
        False -> number


-- A duration as xsd:duration writes it, each part written as it was given, in order
renderDuration :: Duration -> Text
renderDuration d =
    sign <> "P" <> part 'Y' (years d) <> part 'M' (months d) <> part 'D' (days d) <> time
    where
    sign = case negative d of
        True  -> "-"
        False -> ""

    time = case (hours d, minutes d, seconds d) of
        (Nothing, Nothing, Nothing) -> ""
        _someTime                   -> "T" <> part 'H' (hours d) <> part 'M' (minutes d) <> secondsPart

    part letter = maybe "" (\n -> T.pack (show n) <> T.singleton letter)

    secondsPart = maybe "" (\s -> T.pack (renderDecimal s) <> "S") (seconds d)

    renderDecimal s = case isInteger s of
        True  -> formatScientific Fixed (Just 0) s
        False -> formatScientific Fixed Nothing s


-- A duration of one unit, as (days 19) writes it: a whole number, but for the seconds, which
-- may have a fraction. A negative amount is written with xsd:duration's -, for the old shifts
-- that took one; or why the amount cannot be one.
unitDuration :: Unit -> Scientific -> Either Text Duration
unitDuration unit amount = case (unit, isInteger amount) of
    (Seconds, _)  -> Right (only (\d -> d { seconds = Just magnitude }))
    (_, False)    -> Left "a calendar cannot apply a fraction of a year, month, day, hour or minute exactly"
    (_, True)     -> Right (only (set unit (floor magnitude)))
    where
    magnitude = abs amount
    none      = Duration (amount < 0) Nothing Nothing Nothing Nothing Nothing Nothing
    only f    = f none

    set u n d = case u of
        Years    -> d { years = Just n }
        Months   -> d { months = Just n }
        Days     -> d { days = Just n }
        Hours    -> d { hours = Just n }
        Minutes  -> d { minutes = Just n }
        Seconds  -> d { seconds = Just (fromInteger n) }


-- The largest unit a duration is written in, whatever its amount: (months 0) is in months
largestUnit :: Duration -> Maybe Unit
largestUnit d = case filter fst (zip written [minBound ..]) of
    (_, unit) : _ -> Just unit
    []            -> Nothing
    where
    written = [isJust (years d), isJust (months d), isJust (days d), isJust (hours d), isJust (minutes d), isJust (seconds d)]
