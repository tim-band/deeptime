module Ticks exposing (Tick, getTicks)

import Base exposing (..)

import Time
import Time.Extra

type alias Tick =
  { y : Float
  , rendering : String
  }

-- gets SI magnitude from x, (1, ""), (1000, "k"), (1e9, "M"), or (1e12, "G")
getBigMagnitude : Float -> (Float, String)
getBigMagnitude x =
  let
    gbm mag symbol symbols y = case symbols of
      [] -> (mag, symbol)
      ( s :: symbols1 ) -> if y < 2e3
        then (mag, symbol)
        else gbm (mag * 1e3) s symbols1 (y * 1e-3)
  in gbm 1 "" [ "k", "M", "G" ] x

getLittleMagnitude : Float -> (Float, String)
getLittleMagnitude x =
  let
    glm mag symbol symbols y = case symbols of
      [] -> (mag, symbol)
      ( s :: symbols1 ) -> if 2 < y
        then (mag, symbol)
        else glm (mag * 1e-3) s symbols1 (y * 1e3)
  in glm 1 "" [ "m", "\u{00b5}", "n", "p", "f", "a", "z", "y", "r", "q" ] x

renderAdBc : Float -> String
renderAdBc y = let yr = Basics.round y in if yr < 1
  then String.append (String.fromInt (1 - yr)) "BC"
  else String.append (String.fromInt yr) "AD"

renderGregorianYear : Float -> String
renderGregorianYear y = y |> Basics.round |> String.fromInt

tickListMax : Int -> (Float -> String) -> Float -> Float -> Float -> Float -> List Tick
tickListMax max_count render num numStep yPos yStep =
  let
    getTl rec_count n y = if rec_count == 0 || 1 <= y
      then []
      else { y = y, rendering = render n } :: getTl (rec_count - 1) (n + numStep) (y + yStep)
  in getTl max_count num yPos

tickList : (Float -> String) -> Float -> Float -> Float -> Float -> List Tick
tickList render num numStep yPos yStep = tickListMax 50 render num numStep yPos yStep

-- Adjusts for going into BC where the ticks need to go to -9, -19 and so on
-- in order to land on round BC numbers
tickListAdjusted : (Float -> String) -> Float -> Float -> Float -> Float -> List Tick
tickListAdjusted render num numStep yPos yStep =
  let
    getAdjustedTl n y = if 1 <= y
      then []
      else { y = y, rendering = render n } :: getAdjustedTl (n + numStep) (y + yStep)
    getTl n y = if 1 <= y
      then []
      else if n < -1
      then getAdjustedTl (n + 1) (y + (yStep / numStep))
      else { y = y, rendering = render n } :: getTl (n + numStep) (y + yStep)
  in getTl num yPos

getTickSize : Float -> Float -> Float
getTickSize minTickSize maxTickSize =
  let
    tickSizes = List.filter ((>=) maxTickSize) [minTickSize * 5, minTickSize * 2]
  in case List.head tickSizes of
      Just t -> t
      Nothing -> minTickSize

getYearsAgoTicks : TimeDelta -> TimeDelta -> Float -> Float -> List Tick
getYearsAgoTicks yearsAgo height minTickSize maxTickSize =
  let
    (mag, symbol) = getBigMagnitude yearsAgo
    yamag = yearsAgo / mag
    heightMag = height / mag
    tickSize = getTickSize (minTickSize / mag) (maxTickSize / mag)
    firstTick = toFloat (Basics.ceiling (yamag / tickSize)) * tickSize |> Basics.max 0
    render y = String.fromFloat y ++ symbol ++ "yr"
  in tickList render firstTick tickSize ((firstTick - yamag) / heightMag) (tickSize / heightMag)

getAdBcTicks : TimeDelta -> TimeDelta -> Float -> Float -> List Tick
getAdBcTicks ad height minTickSize maxTickSize =
  let
    tickSize = getTickSize minTickSize maxTickSize
    firstTickNumber = (Basics.min ad latestYear) / tickSize |> Basics.floor
    firstTick = toFloat firstTickNumber * tickSize
  in tickListAdjusted renderAdBc firstTick -tickSize ((ad - firstTick) / height) (tickSize / height)

getGregorianYearTicks : TimeDelta -> TimeDelta -> Float -> Float -> List Tick
getGregorianYearTicks ad height minTickSize maxTickSize =
  let
    tickSize = getTickSize minTickSize maxTickSize
    adClamped = Basics.min ad latestYear
    firstTick = toFloat (Basics.floor (adClamped / tickSize)) * tickSize
  in tickList renderGregorianYear firstTick -tickSize ((ad - firstTick) / height) (tickSize / height)

daysPerYear : Float
daysPerYear = 365.25636

secondsPerYear : Float
secondsPerYear = daysPerYear * 24 * 60 * 60

millisecondsPerYear : Float
millisecondsPerYear = secondsPerYear * 1000

monthToQuarter : Time.Month -> Time.Month
monthToQuarter m = case m of
    Time.Jan -> Time.Jan
    Time.Feb -> Time.Jan
    Time.Mar -> Time.Jan
    Time.Apr -> Time.Apr
    Time.May -> Time.Apr
    Time.Jun -> Time.Apr
    Time.Jul -> Time.Jul
    Time.Aug -> Time.Jul
    Time.Sep -> Time.Jul
    Time.Oct -> Time.Oct
    Time.Nov -> Time.Oct
    Time.Dec -> Time.Oct

monthName : Time.Month -> String
monthName m = case m of
    Time.Jan -> "Jan"
    Time.Feb -> "Feb"
    Time.Mar -> "Mar"
    Time.Apr -> "Apr"
    Time.May -> "May"
    Time.Jun -> "Jun"
    Time.Jul -> "Jul"
    Time.Aug -> "Aug"
    Time.Sep -> "Sep"
    Time.Oct -> "Oct"
    Time.Nov -> "Nov"
    Time.Dec -> "Dec"

millisToParts : Float -> Time.Extra.Parts
millisToParts m = m |> Basics.round |> Time.millisToPosix |> Time.Extra.posixToParts Time.utc

tickFromMonth : Time -> TimeDelta -> Time.Posix -> Tick
tickFromMonth top height p =
  let
    { year, month } = Time.Extra.posixToParts Time.utc p
    m = p |> Time.posixToMillis |> toFloat
  in
    { y = (top - m) / height
    , rendering = String.concat [monthName month, " ", String.fromInt year]
    }

getGregorianMonthTicks : TimeDelta -> TimeDelta -> Float -> List Tick
getGregorianMonthTicks ad height maxTickSize =
  let
    (numMonths, rounder) =
      if maxTickSize <= 1/4
      then (1, Basics.identity)
      else (3, monthToQuarter)
    posixTimeTop = (ad - 1970) * millisecondsPerYear
    millis = height * millisecondsPerYear
    posixTimeBottom = posixTimeTop - millis
    { year, month } = millisToParts posixTimeBottom
    firstTickDate = { year = year, month = rounder month, day = 1, hour = 0, minute = 0, second = 0, millisecond = 0 }
    firstTickPosix = Time.Extra.partsToPosix Time.utc firstTickDate
    endDate = posixTimeTop |> Basics.round |> Time.millisToPosix
    tickDates = Time.Extra.range Time.Extra.Month numMonths Time.utc firstTickPosix endDate
  in List.map (tickFromMonth posixTimeTop millis) tickDates

tickFromDay : Time -> TimeDelta -> Time.Posix -> Tick
tickFromDay top height p =
  let
    { year, month, day } = Time.Extra.posixToParts Time.utc p
    m = p |> Time.posixToMillis |> toFloat
  in
    { y = (top - m) / height
    , rendering = String.concat [String.fromInt day, " ", monthName month, " ", String.fromInt year]
    }

getGregorianDayTicks : TimeDelta -> TimeDelta -> Float -> List Tick
getGregorianDayTicks ad height maxTickSize =
  let
    (interval, days) =
      if maxTickSize <= 5/365
      then (Time.Extra.Day, [0])
      else if maxTickSize <= 10/365
      then (Time.Extra.Month, [0, 5, 10, 15, 20, 25])
      else (Time.Extra.Month, [0, 10, 20])
    posixTimeTop = (ad - 1970) * millisecondsPerYear
    millis = height * millisecondsPerYear
    posixTimeBottom = posixTimeTop - millis
    { year, month } = millisToParts posixTimeBottom
    firstMonthTickDate = { year = year, month = month, day = 1, hour = 0, minute = 0, second = 0, millisecond = 0 }
    firstMonthTickPosix = Time.Extra.partsToPosix Time.utc firstMonthTickDate
    endDate = posixTimeTop |> Basics.round |> Time.millisToPosix
    baseTickDates = Time.Extra.range interval 1 Time.utc firstMonthTickPosix endDate
    addDays : Time.Posix -> List Time.Posix
    addDays p = List.map (\d -> Time.Extra.add Time.Extra.Day d Time.utc p) days
    -- could trim this list if we really wanted to
    tickDates = List.concatMap addDays baseTickDates
  in List.map (tickFromDay posixTimeTop millis) tickDates

getGregorianTicks : TimeDelta -> TimeDelta -> Float -> Float -> List Tick
getGregorianTicks ad height minTickSize maxTickSize =
  if 0.9 < minTickSize
    then getGregorianYearTicks ad height minTickSize maxTickSize
    else if 0.083 < minTickSize
    then getGregorianMonthTicks ad height maxTickSize
    else getGregorianDayTicks ad height maxTickSize

getEarlySecondsTicks : Time -> TimeDelta -> Float -> List Tick
getEarlySecondsTicks minute heightMinutes maxTickSizeMinutes =
  let
    second = minute * 60
    (mag, symbol) = getLittleMagnitude second
    secondMag = second / mag |> Basics.max 0
    height = heightMinutes * 60 / mag
    maxTickSize = maxTickSizeMinutes * 60 / mag
    minTickSize = getMinTickSize maxTickSize
    tickSize = getTickSize minTickSize maxTickSize
    tickListNumber = secondMag / tickSize |> Basics.floor
    firstTick = toFloat tickListNumber * tickSize
    renderEarlySeconds s = String.fromFloat s ++ symbol ++ "s"
  in tickListMax (tickListNumber + 1) renderEarlySeconds firstTick -tickSize ((secondMag - firstTick) / height) (tickSize / height)

getEarlyTicks : String -> Float -> (Time -> TimeDelta -> Float -> List Tick) -> Time -> TimeDelta -> Float -> List Tick
getEarlyTicks unit mult nextFn year heightYears maxTickSizeYears =
  let
    day = year * mult
    height = heightYears * mult
    maxTickSize = maxTickSizeYears * mult
  in if day < 5
    then nextFn day height maxTickSize
    else
    let
      minTickSize = getMinTickSize maxTickSize
      tickSize = getTickSize minTickSize maxTickSize
      firstTickNumber = Basics.floor (day / tickSize)
      firstTick = toFloat firstTickNumber * tickSize
      render t = String.fromFloat t ++ unit
    in tickListMax (firstTickNumber + 1) render firstTick -tickSize ((day - firstTick) / height) (tickSize / height)

getEarlyMinutesTicks : Time -> TimeDelta -> Float -> List Tick
getEarlyMinutesTicks =
  getEarlyTicks "min" 60 getEarlySecondsTicks

getEarlyHoursTicks : Time -> TimeDelta -> Float -> List Tick
getEarlyHoursTicks =
  getEarlyTicks "hr" 24 getEarlyMinutesTicks

getEarlyDaysTicks : Time -> TimeDelta -> Float -> List Tick
getEarlyDaysTicks =
  getEarlyTicks "d" 365.25 getEarlyHoursTicks

getEarlyYearsTicks : Time -> TimeDelta -> Float -> Float -> List Tick
getEarlyYearsTicks year height minTickSize maxTickSize =
  let
    (mag, symbol) = getBigMagnitude year
    tickSize = getTickSize (minTickSize / mag) (maxTickSize / mag)
    yearMag = year / mag
    firstTickNumber = Basics.floor (yearMag / tickSize)
    firstTick = toFloat firstTickNumber * tickSize
    heightMag = height / mag
    renderEarlyYear yr = String.fromFloat yr ++ symbol ++ "yr"
  in tickListMax (firstTickNumber + 1) renderEarlyYear firstTick -tickSize ((yearMag - firstTick) / heightMag) (tickSize / heightMag)

getMinTickSize : TimeDelta -> TimeDelta
getMinTickSize maxTickSize =
  let
    digitCount = Basics.logBase 10 maxTickSize |> Basics.floor
  in 10 ^ toFloat digitCount

-- Gets a list of Ticks that should be rendered in the provided time window
getTicks : TimeWindow -> List Tick
getTicks window =
  let
    { top, bottom } = window
    height = top - bottom
    maxTickSize = height / 5
    minTickSize = getMinTickSize maxTickSize
    yearsAgo = present - top
  in if 10000 < yearsAgo + height
    then if top < earlyUniverseTime
      then if top < 2
        then getEarlyDaysTicks top height maxTickSize
        else getEarlyYearsTicks top height minTickSize maxTickSize
      else getYearsAgoTicks yearsAgo height minTickSize maxTickSize
    else let ad = top - bc1Time in if 1582 <= ad
      then getGregorianTicks ad height minTickSize maxTickSize
      else getAdBcTicks ad height minTickSize maxTickSize
