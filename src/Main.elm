module Main exposing (main)

import Basics exposing (..)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, style, value)
import Html.Events exposing (onInput, onMouseUp)
import Http
import List
import List.Extra exposing (minimumBy, takeWhile)
import Random
import Svg
import Svg.Attributes as Svga
import Task
import Time
import Time.Extra
import Html.Events exposing (onClick)

import Base exposing (..)
import Stratigraphy
import Event exposing (Event, Events, ScreenEvent)

import Debug

-- time (in seconds) between frames. The reciprocal of the frame rate
frameDelta : Float
frameDelta = 0.05

main : Program () Model Msg
main = Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

initWindow : Time -> TimeDelta -> TimeWindow
initWindow top size = { top = top, bottom = top - size }

type MoveMode = MoveConstant | MoveToFocus

type alias Model =
  { events : Event.Events
  , moveMode : MoveMode
  , zoomRate : Float
  , moveRate : Float
  , window : TimeWindow
  , width : Float
  , height : Float
  , focused : Maybe FadedScreenEvent
  , stratigraphyData : Maybe Stratigraphy.DataDict
  }

type alias FadedScreenEvent =
  { sev : Event.ScreenEvent
  , fade : Float
  }

fadeUpFocused : FadedScreenEvent -> FadedScreenEvent
fadeUpFocused fse =
  let fade = Basics.min 2 (fse.fade + 0.1)
  in { fse | fade = fade }

fadeDownFocused : Maybe FadedScreenEvent -> Maybe FadedScreenEvent
fadeDownFocused mfse = mfse |> Maybe.andThen (\fse -> 
  let fade = fse.fade - 0.1 in if fade <= 0
    then Nothing
    else Just { fse | fade = fade })

updateFadedScreenEventY : Time -> TimeDelta ->FadedScreenEvent -> FadedScreenEvent
updateFadedScreenEventY top height fsev = let ev = fsev.sev.ev in
  { sev =
    { top = Event.yEventTop top height ev
    , bottom = Event.yEventBottom top height ev
    , unclampedMiddle = Event.yEventMiddle top height ev
    , ev = ev
    }
  , fade = fsev.fade
  }

type Msg
  = NoMsg
  | SetEvents (List Event.Event)
  | NextFrame
  | Move Float
  | MoveReleased
  | Viewport Float Float
  | FocusOn Event
  | StratigraphyLoad (List String) (Result Http.Error Stratigraphy.DataDict)
  | StratigraphyIntervalsLoad (List String) (Result Http.Error Stratigraphy.IntervalDict)

init : () -> (Model, Cmd Msg)
init _ = let window = initWindow present 100 in
  ( { events = Event.findScreenEvents window []
    , moveMode = MoveConstant
    , zoomRate = 1
    , moveRate = 0
    , window = window
    , width = 500
    , height = 500
    , focused = Nothing
    , stratigraphyData = Nothing
    }
  , Cmd.batch
    [ Random.generate SetEvents Event.randomEvents
    , Task.attempt viewportMsg getViewport
    , Http.get
      { url = Stratigraphy.timeline_data_url
      , expect = Http.expectJson
        (StratigraphyLoad Stratigraphy.timeline_data_url_alternatives)
        Stratigraphy.decodeData
      }
    ]
  )

viewportMsg : Result x Browser.Dom.Viewport -> Msg
viewportMsg r = case r of
  Ok { viewport } -> let { width, height } = viewport in Viewport width height
  Err _ -> NoMsg

showError : Http.Error -> String
showError e = case e of
    Http.BadUrl s -> "Bad URL: " ++ s
    Http.Timeout -> "Timeout"
    Http.NetworkError -> "Network error"
    Http.BadStatus i -> "Failed: " ++ String.fromInt i
    Http.BadBody s -> "Bad body: " ++ s

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoMsg -> (model, Cmd.none)
    NextFrame -> (updateModel model, Cmd.none)
    SetEvents evs1 ->
      ( { model | events = Event.findScreenEvents model.window evs1 }
      , Cmd.none
      )
    Move d -> ({ model | moveMode = MoveConstant, moveRate = d }, Cmd.none)
    -- just stop when released for now
    MoveReleased -> ({ model | moveRate = 0 }, Cmd.none)
    Viewport width height -> ({ model | width = width, height = height }, Cmd.none)
    FocusOn ev ->
      let
        { top, bottom } = model.window
        height = top - bottom
      in
        ({ model
          | focused = Just { sev = Event.screenify top height ev, fade = 2 }
          , moveMode = MoveToFocus
          }
        , Cmd.none
        )
    StratigraphyLoad [] (Err _) -> (model, Cmd.none)
    StratigraphyLoad (url :: urls) (Err _) ->
      ( model
      , Http.get
        { url = url
        , expect = Http.expectJson
          (StratigraphyLoad urls)
          Stratigraphy.decodeData
        }
      )
    StratigraphyLoad _ (Ok str) -> Debug.log "loaded strat data"
      ( { model
        | stratigraphyData = Just str
        }
      , Http.get
        { url = Stratigraphy.time_interval_data_url
        , expect = Http.expectJson
          (StratigraphyIntervalsLoad Stratigraphy.time_interval_data_url_alternatives)
          Stratigraphy.decodeIntervals
        }
      )
    StratigraphyIntervalsLoad [] (Err e) -> (model, Debug.log ("Failed to load intervals" ++ showError e) Cmd.none)
    StratigraphyIntervalsLoad (url :: urls) (Err _) ->
      ( model
      , Http.get
        { url = url
        , expect = Http.expectJson
          (StratigraphyIntervalsLoad urls)
          Stratigraphy.decodeIntervals
        }
      )
    StratigraphyIntervalsLoad _ (Ok ints) -> case model.stratigraphyData of
      Nothing -> (model, Cmd.none)
      Just data ->
        ( { model
          | events = Event.findScreenEvents model.window (Stratigraphy.events data ints)
          , stratigraphyData = Nothing
          }
        , Cmd.none
        )

getInterpolatedAt : Float -> List Float -> Maybe Float
getInterpolatedAt t0 xs =
  let
    gia last t zs = case zs of
      [] -> Just last
      (y :: ys) -> if 1 <= t then gia y (t - 1) ys else case ys of
        [] -> Just y
        (y1 :: _) -> Just (y + t * (y1 - y))
  in case xs of
    [] -> Nothing
    (y :: ys) -> gia y t0 ys

midTime : Event -> Time
midTime { start, end } = (start + end) / 2

getZoomRate : Events -> TimeWindow -> Float -> Float -> Float
getZoomRate events window moveDist currentZoomRate =
  let
    minimalEventCount = 10
    secondsLookahead = 1.2
    idealProportionNewEventsPerSecond = 0.1
    zoomRateCoefficient = 1
    smoothingConstant = 0.1
    visibleEventCount = List.length events.visibleEvents
    hardMaxEvents = Basics.max 0 <| 200 - visibleEventCount
    frameLookahead = secondsLookahead * frameDelta
    height = window.top - window.bottom
    mid = window.bottom + height / 2
    timeIsClose t = if moveDist < 0
      then window.bottom + moveDist * frameLookahead < t
      else t < window.top + moveDist * frameLookahead
    nextEventTimes = List.map midTime <| if moveDist < 0
      then events.previousEvents |> List.take hardMaxEvents
      else events.nextEvents |> List.take hardMaxEvents
    visibleEventTimes = List.map (\ev -> midTime ev.ev) events.visibleEvents
    -- extended (floating point) visible event count, including
    -- a proportion of the next event depending on how close it is
    visibleEventCountF = toFloat visibleEventCount + case nextEventTimes of
      [] -> 0
      (t :: _) -> if moveDist < 0
        then case List.minimum visibleEventTimes of
          Nothing -> 0
          Just vt -> (vt - window.bottom) / (vt - t)
        else case List.maximum visibleEventTimes of
          Nothing -> 0
          Just vt -> (window.top - vt) / (t - vt)
    closeEventTimes = takeWhile timeIsClose nextEventTimes
    totalEventCount = visibleEventCount + List.length closeEventTimes
    desiredVisibleEventCount =
      idealProportionNewEventsPerSecond * toFloat totalEventCount
      + minimalEventCount
    desiredExtraEventCount = desiredVisibleEventCount - visibleEventCountF
    idealHeight = if desiredExtraEventCount < 0
      then height * (desiredVisibleEventCount / visibleEventCountF)
      else case getInterpolatedAt desiredExtraEventCount nextEventTimes of
        Nothing -> height
        Just t -> 2 * abs (mid - t)
    ratio = idealHeight / height
    logIdealZoomRate = zoomRateCoefficient * frameDelta * Basics.logBase Basics.e ratio
    logCurrentZoomRate = Basics.logBase Basics.e currentZoomRate
    logSmoothedZoomRate = logCurrentZoomRate + smoothingConstant * (logIdealZoomRate - logCurrentZoomRate)
    maxDistance = if moveDist < 0
      then case List.minimum nextEventTimes of
        Nothing -> 10^9
        Just t -> mid - t
      else case List.maximum nextEventTimes of
        Nothing -> 10^9
        Just t -> t - mid
    logMaxDistance = Basics.logBase Basics.e (maxDistance + 1)
    clampedLogZoomRate = Basics.clamp -logMaxDistance logMaxDistance logSmoothedZoomRate
  in Basics.e ^ clampedLogZoomRate

updateModel : Model -> Model
updateModel model =
  let
    { zoomRate, moveMode, moveRate, window, focused } = model
    half = (window.top - window.bottom) / 2
    moveRate1 = case moveMode of
      MoveConstant -> moveRate
      MoveToFocus -> case focused of
        Nothing -> moveRate
        Just foc ->
          let
            halfWay = window.top - half
            force = (midTime foc.sev.ev - halfWay) * 0.03
          in (moveRate + force) * 16 * frameDelta
    mid0 = window.top - half + moveRate1
    newHalf = half * zoomRate
    mid = Basics.clamp 0 endTime mid0
    window1 = { top = mid + newHalf, bottom = mid - newHalf }
    events = Event.adjustScreenEvents window1 model.events
    middlest = getMiddlest events.visibleEvents
    focused1 = case moveMode of
      MoveToFocus -> focused
      MoveConstant -> case focused of
        Nothing -> Maybe.map (\e -> { sev = e, fade = 0 }) middlest
        Just foc0 ->
          case middlest of
            Nothing -> fadeDownFocused focused
            Just sevp -> if sevp.ev == foc0.sev.ev
              then Just <| fadeUpFocused foc0
              else fadeDownFocused focused
    focused2 = Maybe.map (updateFadedScreenEventY window1.top (newHalf * 2)) focused1
  in
    { model
    | window = window1
    , moveRate = moveRate1
    , events = events
    , zoomRate = getZoomRate events window1 moveRate1 zoomRate
    , focused = focused2
    }

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ onResize (\w h -> Viewport (toFloat w) (toFloat h))
    , Time.every (1 / frameDelta) (\_ -> NextFrame)
    ]

renderEvent : ScreenEvent -> Html Msg
renderEvent e = div (eventAttrs e) [text e.ev.name]

eventPosX : Event -> Int
eventPosX ev = 5 + ev.category * 5

eventAttrs : ScreenEvent -> List (Html.Attribute Msg)
eventAttrs { top, bottom, ev } =
  [ style "position" "fixed"
  , style "top" (String.fromFloat (top * 100) ++ "%")
  , style "height" (String.fromFloat ((bottom - top) * 100) ++ "%")
  , style "left" (String.fromInt (eventPosX ev) ++ "%")
  , style "background" ev.fill
  , style "z-index" "2"
  , style "cursor" "pointer"
  , style "text-orientation" "mixed"
  , style "writing-mode" "vertical-rl"
  , style "overflow" "hidden"
  , onClick <| FocusOn ev
  ]

type alias Tick =
  { y : Float
  , rendering : String
  }

renderYearsAgo : Float -> String
renderYearsAgo ya = if ya < 2e6
  then String.append (String.fromFloat (ya / 1000)) "kyr"
  else if ya < 2e9
  then String.append (String.fromFloat (ya / 1e6)) "Myr"
  else String.append (String.fromFloat (ya / 1e9)) "Gyr"

renderAdBc : Float -> String
renderAdBc y = let yr = Basics.round y in if yr < 1
  then String.append (String.fromInt (1 - yr)) "BC"
  else String.append (String.fromInt yr) "AD"

renderGregorianYear : Float -> String
renderGregorianYear y = y |> Basics.round |> String.fromInt

tickList : (Float -> String) -> Float -> Float -> Float -> Float -> List Tick
tickList render num numStep yPos yStep =
  let
    getTl n y = if 1 <= y
      then []
      else { y = y, rendering = render n } :: getTl (n + numStep) (y + yStep)
  in getTl num yPos

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

getYearsAgoTicks : TimeDelta -> TimeDelta -> Float -> Float -> List Tick
getYearsAgoTicks yearsAgo height minTickSize maxTickSize =
  let
    tickSizes = List.filter ((>=) maxTickSize) [minTickSize * 5, minTickSize * 2]
    tickSize = case List.head tickSizes of
      Just t -> t
      Nothing -> minTickSize
    firstTick = toFloat (Basics.ceiling (yearsAgo / tickSize)) * tickSize
  in tickList renderYearsAgo firstTick tickSize ((firstTick - yearsAgo) / height) (tickSize / height)

getAdBcTicks : TimeDelta -> TimeDelta -> Float -> Float -> List Tick
getAdBcTicks ad height minTickSize maxTickSize =
  let
    tickSizes = List.filter ((>=) maxTickSize) [minTickSize * 5, minTickSize * 2]
    tickSize = case List.head tickSizes of
      Just t -> t
      Nothing -> minTickSize
    firstTick = toFloat (Basics.floor (ad / tickSize)) * tickSize
  in tickListAdjusted renderAdBc firstTick -tickSize ((ad - firstTick) / height) (tickSize / height)

getGregorianYearTicks : TimeDelta -> TimeDelta -> Float -> Float -> List Tick
getGregorianYearTicks ad height minTickSize maxTickSize =
  let
    tickSizes = List.filter ((>=) maxTickSize) [minTickSize * 5, minTickSize * 2]
    tickSize = case List.head tickSizes of
      Just t -> t
      Nothing -> minTickSize
    firstTick = toFloat (Basics.floor (ad / tickSize)) * tickSize
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

getTicks : TimeWindow -> List Tick
getTicks window =
  let
    { top, bottom } = window
    height = top - bottom
    maxTickSize = height / 5
    digitCount = Basics.logBase 10 maxTickSize |> Basics.floor
    minTickSize = 10 ^ toFloat digitCount
    yearsAgo = present - top
  in if 10000 < yearsAgo + height
    then getYearsAgoTicks yearsAgo height minTickSize maxTickSize
    else let ad = top - bc1Time in if 1582 <= ad
      then getGregorianTicks ad height minTickSize maxTickSize
      else getAdBcTicks ad height minTickSize maxTickSize

renderTick : Tick -> Html Msg
renderTick { y, rendering } = Html.div
  [ style "position" "fixed"
  , style "top" (String.fromFloat (y * 100) ++ "%")
  , style "left" "0"
  , style "border-top" "black solid 1px"
  ] [ Html.text rendering ]

logit : Float -> Float
logit x = Basics.logBase Basics.e ((1 + x) / (1 - x))

stringToMove : Float -> String -> Msg
stringToMove speed s = case String.toFloat s of
  Just x -> Move <| logit (x * 0.999) * speed
  Nothing -> NoMsg

getMiddlest : List ScreenEvent -> Maybe ScreenEvent
getMiddlest = minimumBy (\{ unclampedMiddle } -> abs (0.5 - unclampedMiddle))

view : Model -> Html Msg
view { events, window, width, height, moveRate, focused } =
  let
    timeHeight = window.top - window.bottom
    sliderWidth = 30
    sliderHeight = 0.5 * height
    sliderTop = 0.25 * height
    stopped = if moveRate == 0 then [ value "0" ] else []
    { visibleEvents } = events
    eventBox elts = Html.div
      [ style "position" "fixed"
      , style "width" "60%"
      , style "left" "40%"
      , style "top" "0%"
      , style "height" "100%"
      , style "z-index" "1"
      ] elts
    focusIndicators = case focused of
      Nothing -> [ eventBox [] ]
      Just { sev, fade } -> let opacity = fade |> Basics.min 1 |> String.fromFloat in
        [ Svg.svg
          [ style "position" "fixed"
          , style "top" "0%"
          , style "left" "0%"
          , style "z-index" "1"
          , Svga.width "100%"
          , Svga.height "100%"
          , [0, 0, width, height]
            |> List.map String.fromFloat
            |> String.join " "
            |> Svga.viewBox
          ]
          [ Svg.line
            [ toFloat (eventPosX sev.ev) / 100 * width |> Basics.round |> String.fromInt |> Svga.x1
            , (sev.top + sev.bottom) * 0.5 * height |> Basics.round |> String.fromInt |> Svga.y1
            , 0.4 * width |> Basics.round |> String.fromInt |> Svga.x2
            , 0.5 * height |> Basics.round |> String.fromInt |> Svga.y2
            , Svga.stroke "black"
            , Svga.opacity opacity
            ] []
          ]
        , eventBox
          [ Html.div
            [ style "position" "absolute"
            , style "top" "50%"
            , style "transform" "translateY(-50%)"
            , style "opacity" opacity
            ] [ text sev.ev.name ]
          ]
        ]
  in div []
    ([ visibleEvents |> List.map renderEvent |> Html.div [ ]
    , getTicks window |> List.map renderTick |> Html.div []
    , Html.input (stopped ++
      [ attribute "type" "range"
      , attribute "min" "-1"
      , attribute "max" "1"
      , attribute "step" "0.01"
      , onInput <| stringToMove (timeHeight * 0.1)
      , onMouseUp MoveReleased
      , style "position" "fixed"
      , style "top" "0"
      , style "left" "0"
      , style "width" (String.fromFloat sliderHeight ++ "px")
      , style "height" (String.fromInt sliderWidth ++ "px")
      , style "transform"
        ( "translateY("
        ++ (String.fromFloat (sliderHeight + sliderTop))
        ++ "px) translateX("
        ++ (String.fromFloat (width - toFloat sliderWidth))
        ++ "px) rotate(-90deg)"
        )
      , style "transform-origin" "top left"
      , style "z-index" "2"
      ]) []
    ] ++ focusIndicators)
