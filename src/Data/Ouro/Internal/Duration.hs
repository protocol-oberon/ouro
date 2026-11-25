module Data.Ouro.Internal.Duration
( Duration (..)
, Unit (..)
, Direction (..)
, parseDuration
, renderDuration
, unitDuration
, largestUnit
, shiftBy
, shiftThrough
, addDurations
) where

import           Control.Monad      (guard)
import           Data.Char          (isDigit)
import           Data.Maybe         (fromMaybe, isJust)
import           Data.Scientific    (FPFormat (Fixed), Scientific, formatScientific, isInteger)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time          (UTCTime (..), addUTCTime)
import           Data.Time.Calendar (addDays, addGregorianMonthsClip, addGregorianYearsClip, fromGregorian,
                                     gregorianMonthLength, toGregorian)


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


-- Which way a shift moves a date
data Direction
    = Forward
    | Backward
    deriving (Show, Eq)


-- A date moved by a duration, its parts applied in order, years, months, days, then the time,
-- so that the clipping of months is the same however a duration is written: 31 January and one
-- month is the last day of February, never March. A duration written with xsd:duration's -
-- moves the other way.
shiftBy :: Direction -> Duration -> UTCTime -> UTCTime
shiftBy direction d utc = addUTCTime (fromRational (sign * time)) utc { utctDay = dated }
    where
    sign = case (direction, negative d) of
        (Forward,  False) -> 1
        (Backward, True)  -> 1
        _otherway         -> -1

    count f = round sign * fromMaybe 0 (f d)

    dated = addDays (count days)
          . addGregorianMonthsClip (count months)
          . addGregorianYearsClip (count years)
          $ utctDay utc

    time = toRational (3600 * fromMaybe 0 (hours d) + 60 * fromMaybe 0 (minutes d))
         + toRational (fromMaybe 0 (seconds d))


-- A date moved by a duration and then through its largest unit: forward, to the end of that
-- unit, its last second, as end_of_the_end means; backward, to its start. (days 1) forward
-- from 3 January is 4 January at 23:59:59; (months 0) forward is the end of the current month.
shiftThrough :: Direction -> Duration -> UTCTime -> UTCTime
shiftThrough direction d utc = case (direction, largestUnit d) of
    (Forward,  Just unit) -> endOf unit moved
    (Backward, Just unit) -> startOf unit moved
    (_, Nothing)          -> moved
    where
    moved = shiftBy direction d utc


-- The last second of the unit a date is in
endOf :: Unit -> UTCTime -> UTCTime
endOf unit utc = case unit of
    Years   -> let (y, _, _) = toGregorian day in UTCTime (fromGregorian y 12 31) lastSecond
    Months  -> let (y, m, _) = toGregorian day in UTCTime (fromGregorian y m (gregorianMonthLength y m)) lastSecond
    Days    -> UTCTime day lastSecond
    Hours   -> UTCTime day (fromInteger (whole 3600 * 3600 + 3599))
    Minutes -> UTCTime day (fromInteger (whole 60 * 60 + 59))
    Seconds -> utc
    where
    day        = utctDay utc
    lastSecond = 86399
    whole n    = floor (toRational (utctDayTime utc)) `div` n


-- The first second of the unit a date is in
startOf :: Unit -> UTCTime -> UTCTime
startOf unit utc = case unit of
    Years   -> let (y, _, _) = toGregorian day in UTCTime (fromGregorian y 1 1) 0
    Months  -> let (y, m, _) = toGregorian day in UTCTime (fromGregorian y m 1) 0
    Days    -> UTCTime day 0
    Hours   -> UTCTime day (fromInteger (whole 3600 * 3600))
    Minutes -> UTCTime day (fromInteger (whole 60 * 60))
    Seconds -> utc
    where
    day     = utctDay utc
    whole n = floor (toRational (utctDayTime utc)) `div` n


-- Two durations added part by part, (days 1) and (hours 2) being P1DT2H: a part written in
-- either is written in the sum
addDurations :: Duration -> Duration -> Duration
addDurations a b = Duration
    { negative = False
    , years    = plus years
    , months   = plus months
    , days     = plus days
    , hours    = plus hours
    , minutes  = plus minutes
    , seconds  = plus seconds
    }
    where
    plus :: Num n => (Duration -> Maybe n) -> Maybe n
    plus part = case (part a, part b) of
        (Nothing, Nothing) -> Nothing
        (x, y)             -> Just (fromMaybe 0 x + fromMaybe 0 y)
