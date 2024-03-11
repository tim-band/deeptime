module Main exposing (main)

import Basics exposing (..)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Csv.Decode as DC
import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, style, value, height)
import Html.Events exposing (onInput, onMouseUp)
import Html.Keyed
import Http
import Json.Decode as DJ
import List
import List.Extra exposing (minimumBy)
import Svg
import Svg.Attributes as Svga
import Task
import Time
import Html.Events exposing (onClick)

import Base exposing (..)
import Event exposing (Event, Events, ScreenEvent, eventEnd)
import Bigbang
import Stratigraphy

import Debug
import Geography
import Event exposing (zoomHintHeight)
import Gts
import Human
import Ticks exposing (Tick, getTicks)

-- time (in seconds) between frames. The reciprocal of the frame rate
frameDelta : Float
frameDelta = 0.05

jogAmount : Float
jogAmount = 0.0003

jogDecay : Float
jogDecay = 0.6 ^ frameDelta

main : Program () Model Msg
main = Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

initWindow : Time -> TimeDelta -> TimeWindow
initWindow top size = { top = top, bottom = top - size }

type MoveMode = MoveFreeFocus | MoveKeepFocus | MoveToFocus

type alias Model =
  { events : Event.Events
  , moveMode : MoveMode
  , zoomRate : Float
  , moveRate : Float
  -- How much moveRate is scaled by per frame.
  -- 1 means no decay, 0 means instant decay.
  , moveDecay : Float
  -- where to force the slider to go to
  , setSlider : Maybe Float
  , window : TimeWindow
  , width : Float
  , height : Float
  , focused : Maybe FadedScreenEvent
  , stratigraphyIntervals : Maybe Stratigraphy.IntervalDict
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

updateFadedScreenEventY : Time -> TimeDelta -> FadedScreenEvent -> FadedScreenEvent
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
  | NextFrame
  | Move Float
  | MoveReleased
  | MoveJog Float
  | Viewport Float Float
  | FocusOn Event
  | DataLoad (String -> Model -> (Result String Model, Cmd Msg)) (Result Http.Error String)

init : () -> (Model, Cmd Msg)
init _ = let window = initWindow present 100 in
  ( { events = Event.findScreenEvents window [Geography.geography]
    , moveMode = MoveFreeFocus
    , zoomRate = 1
    , moveRate = 0
    , moveDecay = 1
    , setSlider = Just 0
    , window = window
    , width = 500
    , height = 500
    , focused = Nothing
    , stratigraphyIntervals = Nothing
    , stratigraphyData = Nothing
    }
  , Cmd.batch
    [ Task.attempt viewportMsg getViewport
    , Http.get
      { url = Stratigraphy.timeline_data_url
      , expect = Http.expectString
        (DataLoad stratigraphyDataUpdate)
      }
    , Http.get
      { url = Stratigraphy.time_interval_data_url
      , expect = Http.expectString
        (DataLoad stratigraphyIntervalsUpdate)
      }
    , Http.get
      { url = Human.evolution_events_url
      , expect = Http.expectString
        (DataLoad humanEvolutionUpdate)
      }
    , Http.get
      { url = Bigbang.bigbang_events_url
      , expect = Http.expectString
        (DataLoad bigBangUpdate)
      }
    , Http.get
      { url = Gts.gts_url
      , expect = Http.expectString (DataLoad gts2020Update)
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

decodeResult : (err -> String) -> Result err a -> Result String a
decodeResult errorToString r = case r of
    Ok v -> Ok v
    Err e -> e |> errorToString |> Err

decodeJsonResult : Result DJ.Error a -> Result String a
decodeJsonResult r = decodeResult DJ.errorToString r

stratigraphyIntervalsUpdate : String -> Model -> (Result String Model, Cmd Msg)
stratigraphyIntervalsUpdate resp model =
  let
    res = DJ.decodeString Stratigraphy.decodeIntervals resp
    updateStrat ints = case model.stratigraphyData of
      Nothing -> { model | stratigraphyIntervals = Just ints }
      Just data ->
        { model
        | events = Event.mergeScreenEvents model.events <|
          Event.findScreenEvents model.window <|
          Stratigraphy.events data ints
        , stratigraphyIntervals = Just ints
        }
  in (res |> Result.map updateStrat |> decodeJsonResult, Cmd.none)

stratigraphyDataUpdate : String -> Model -> (Result String Model, Cmd Msg)
stratigraphyDataUpdate resp model =
  let
    res = DJ.decodeString Stratigraphy.decodeData resp
    updateStrat data = case model.stratigraphyIntervals of
      Nothing -> { model | stratigraphyData = Just data }
      Just ints ->
        { model
        | events = Event.mergeScreenEvents model.events <|
          Event.findScreenEvents model.window <|
          Stratigraphy.events data ints
        , stratigraphyIntervals = Just ints
        }
  in (res |> Result.map updateStrat |> decodeJsonResult, Cmd.none)

eventListUpdate : DJ.Decoder (List Event) -> String -> Model -> (Result String Model, Cmd Msg)
eventListUpdate decoder resp model =
  let
    res = DJ.decodeString decoder resp
    updateHuman events = 
      { model
      | events = Event.mergeScreenEvents model.events <|
        Event.findScreenEvents model.window events
      }
  in (res |> Result.map updateHuman |> decodeJsonResult, Cmd.none)

humanEvolutionUpdate : String -> Model -> (Result String Model, Cmd Msg)
humanEvolutionUpdate resp model = eventListUpdate Human.decode resp model

bigBangUpdate : String -> Model -> (Result String Model, Cmd Msg)
bigBangUpdate resp model = eventListUpdate Bigbang.decode resp model

decodeCsvResult : Result DC.Error a -> Result String a
decodeCsvResult r = decodeResult DC.errorToString r

gts2020Update : String -> Model -> (Result String Model, Cmd Msg)
gts2020Update resp model =
  let
    res = DC.decodeCsv DC.FieldNamesFromFirstRow Gts.decodeToEvents resp
    updateGts events =
      { model
      | events = Event.mergeScreenEvents model.events <|
        Event.findScreenEvents model.window events
      }
  in (res |> Result.map updateGts |> decodeCsvResult, Cmd.none)

onScreen : TimeWindow -> Maybe FadedScreenEvent -> Bool
onScreen { top, bottom } mfse = case mfse of
  Nothing -> False
  Just { sev } -> bottom <= eventEnd sev.ev && sev.ev.start <= top

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    focusedEventStillVisible = onScreen model.window model.focused
    nextFocus = if focusedEventStillVisible
      then MoveKeepFocus
      else MoveFreeFocus
    { top, bottom } = model.window
    height = top - bottom
  in case msg of
    NoMsg -> (model, Cmd.none)
    NextFrame -> (updateModel model, Cmd.none)
    Move d -> (
      { model
      | moveMode = case model.moveMode of
        MoveFreeFocus -> MoveFreeFocus
        MoveToFocus -> nextFocus
        MoveKeepFocus -> nextFocus
      , focused = if focusedEventStillVisible
        then model.focused
        else Nothing
      , moveRate = d
      , moveDecay = 1
      , setSlider = Nothing
      }, Cmd.none)
    MoveJog d -> let moveRate = model.moveRate - jogAmount * d in (
      { model
      | moveMode = case model.moveMode of
        MoveFreeFocus -> MoveFreeFocus
        MoveToFocus -> nextFocus
        MoveKeepFocus -> nextFocus
      , focused = if focusedEventStillVisible
        then model.focused
        else Nothing
      , moveRate = moveRate
      , setSlider = antilogit moveRate |> Just
      , moveDecay = jogDecay
      }, Cmd.none)
    -- just stop when released for now
    MoveReleased -> ({ model | moveRate = 0, setSlider = Just 0 }, Cmd.none)
    Viewport vp_width vp_height -> ({ model | width = vp_width, height = vp_height }, Cmd.none)
    FocusOn ev ->
      ({ model
        | focused = Just { sev = Event.screenify top height ev, fade = 2 }
        , moveMode = MoveToFocus
        , setSlider = Just 0
        }
      , Cmd.none
      )
    DataLoad decode (Ok str) -> case decode str model of
      (Err err, cmd) -> let _ = Debug.log "failed to decode" err in (model, cmd)
      (Ok model1, cmd) -> (model1, cmd)
    DataLoad _ (Err err) -> let _ = Debug.log "failed to get" (showError err) in (model, Cmd.none)

midTime : Event -> Time
midTime ev = (ev.start + eventEnd ev) / 2

getZoomRate : Events -> TimeWindow -> Float -> Float -> Float
getZoomRate events window moveRate currentZoomRate =
  let
    maximumScreensPerSecond = 0.8
    zoomRateCoefficient = 1
    -- smoothingConstant 0: never change, 1: instantly change
    smoothingConstant = 0.1
    minimumHeight = Basics.abs moveRate / maximumScreensPerSecond
    visibleEventCount = List.length events.visibleEvents
    hardMaxEvents = Basics.max 0 <| 200 - visibleEventCount
    height = window.top - window.bottom
    mid = window.bottom + height / 2
    nextEventTimes = List.map midTime <| if moveRate < 0
      then events.previousEvents |> List.take hardMaxEvents
      else events.nextEvents |> List.take hardMaxEvents
    idealHeight = zoomHintHeight mid events.zoomHints
    ratio = max minimumHeight idealHeight / height
    logIdealZoomRate = zoomRateCoefficient * frameDelta * Basics.logBase Basics.e ratio
    logCurrentZoomRate = Basics.logBase Basics.e currentZoomRate
    logSmoothedZoomRate = logCurrentZoomRate + smoothingConstant * (logIdealZoomRate - logCurrentZoomRate)
    maxDistance = if moveRate < 0
      then case List.minimum nextEventTimes of
        Nothing -> 1e9
        Just t -> mid - t
      else case List.maximum nextEventTimes of
        Nothing -> 1e9
        Just t -> t - mid
    logMaxDistance = Basics.logBase Basics.e (maxDistance + 1)
    clampedLogZoomRate = Basics.clamp -logMaxDistance logMaxDistance logSmoothedZoomRate
  in Basics.e ^ clampedLogZoomRate

updateModel : Model -> Model
updateModel model =
  let
    springConstant = 0.03
    damping = 0.07
    { zoomRate, moveMode, moveRate, moveDecay, setSlider, window, focused } = model
    height = window.top - window.bottom
    half = height / 2
    moveRate1 = case moveMode of
      MoveFreeFocus -> moveRate * moveDecay
      MoveKeepFocus -> moveRate * moveDecay
      MoveToFocus -> case focused of
        Nothing -> moveRate
        Just foc ->
          let
            halfWay = window.top - half
            e = foc.sev.ev
            dist = if halfWay < e.start
              then e.start - halfWay
              else if eventEnd e < halfWay
              then eventEnd e - halfWay
              else 0
            force = dist * springConstant / height - moveRate * damping
          in moveRate + force * 16 * frameDelta
    mid0 = window.top - half
    mid1 = mid0 + moveRate1 * frameDelta * height
    newHalf = half * zoomRate
    -- clamp to a fraction of a screen away from the beginning and end of time
    mid = Basics.clamp (half * 0.1) (endTime - half * 0.5) mid1
    effectiveMoveRate = (mid - mid0) / frameDelta
    window1 = { top = mid + newHalf, bottom = mid - newHalf }
    events = Event.adjustScreenEvents window1 model.events
    middlest = getMiddlest events.visibleEvents
    focused1 = case moveMode of
      MoveToFocus -> focused
      MoveKeepFocus -> if onScreen window focused then focused else Nothing
      MoveFreeFocus -> case focused of
        Nothing -> Maybe.map (\e -> { sev = e, fade = 0 }) middlest
        Just foc0 ->
          case middlest of
            Nothing -> fadeDownFocused focused
            Just sevp -> if Event.same sevp.ev foc0.sev.ev
              then Just <| fadeUpFocused foc0
              else fadeDownFocused focused
    focused2 = Maybe.map (updateFadedScreenEventY window1.top (newHalf * 2)) focused1
  in
    { model
    | window = window1
    , moveRate = moveRate1
    , setSlider = Maybe.map (\_ -> antilogit moveRate1) setSlider
    , events = events
    , zoomRate = getZoomRate events window1 effectiveMoveRate zoomRate
    , focused = focused2
    }

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ onResize (\w h -> Viewport (toFloat w) (toFloat h))
    , Time.every (1 / frameDelta) (\_ -> NextFrame)
    ]

renderEvent : ScreenEvent -> (String, Html Msg)
renderEvent e =
  ( "event_" ++ e.ev.name
  , case e.ev.end of
    Just _ -> div (intervalAttrs e) [ text e.ev.name ]
    Nothing -> div (eventAttrs e) [ text e.ev.name ]
  )

eventPosX : Event -> Int
eventPosX ev = 5 + ev.category * 4

eventAttrs : ScreenEvent -> List (Html.Attribute Msg)
eventAttrs { top, ev } =
  [ style "position" "fixed"
  , style "top" (String.fromFloat (top * 100) ++ "%")
  , style "height" "100px"
  , style "left" (String.fromInt (eventPosX ev) ++ "%")
  , style "border" "1px solid black"
  , style "border-radius" "0 10px 10px 10px"
  , style "background" ev.fill
  , style "z-index" "2"
  , style "cursor" "pointer"
  , style "text-orientation" "mixed"
  , style "writing-mode" "vertical-rl"
  , style "overflow" "hidden"
  , style "color" ev.color
  , onClick <| FocusOn ev
  ]

intervalAttrs : ScreenEvent -> List (Html.Attribute Msg)
intervalAttrs { top, bottom, ev } =
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
  , style "color" ev.color
  , onClick <| FocusOn ev
  ]

renderTick : Tick -> Html Msg
renderTick { y, rendering } = Html.div
  [ style "position" "fixed"
  , style "top" (String.fromFloat (y * 100) ++ "%")
  , style "left" "0"
  , style "border-top" "black solid 1px"
  ] [ Html.text rendering ]

logit : Float -> Float
logit x = Basics.logBase Basics.e ((1 + x) / (1 - x))

-- y = ln (1+x)/(1-x)
-- e^y = (2-(1-x))/(1-x) = 2/(1-x) - 1
-- e^y + 1 = 2/(1-x)
-- 2/(e^y + 1) = 1-x
-- x = 1 - 2/(e^y + 1)
antilogit : Float -> Float
antilogit y = 1 - 2 / (1 + Basics.e ^ y)

stringToMove : String -> Msg
stringToMove s = case String.toFloat s of
  Just x -> x * 0.999 |> logit |> Move
  Nothing -> NoMsg

getMiddlest : List ScreenEvent -> Maybe ScreenEvent
getMiddlest = minimumBy (\{ unclampedMiddle } -> abs (0.5 - unclampedMiddle))

view : Model -> Html Msg
view { events, window, width, height, focused, setSlider } =
  let
    timeHeight = window.top - window.bottom
    timeMiddle = window.bottom + timeHeight / 2
    sliderWidth = 30
    sliderHeight = 0.5 * height
    sliderTop = 0.25 * height
    setPosition = case setSlider of
        Nothing -> []
        Just v -> [ String.fromFloat v |> value ]
    { visibleEvents } = events
    eventBox elts = Html.div
      [ style "position" "fixed"
      , style "width" "50%"
      , style "left" "50%"
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
            , 0.5 * width |> Basics.round |> String.fromInt |> Svga.x2
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
            ]
            [ Event.pointIndex timeMiddle sev.ev
            |> sev.ev.renderPoint width
            |> Html.map (\_ -> NoMsg)
            ]
          ]
        ]
  in div
    [ Html.Events.on "wheel" <| DJ.map MoveJog <| DJ.field "deltaY" DJ.float
    ]
    ([ Html.Keyed.node "div" [] (List.map renderEvent visibleEvents)
    , getTicks window |> List.map renderTick |> Html.div []
    , Html.input (setPosition ++
      [ attribute "type" "range"
      , attribute "min" "-1"
      , attribute "max" "1"
      , attribute "step" "0.01"
      , onInput stringToMove
      , onMouseUp MoveReleased
      , style "position" "fixed"
      , style "top" "0"
      , style "left" "0"
      , style "width" (String.fromFloat sliderHeight ++ "px")
      , style "height" (String.fromInt sliderWidth ++ "px")
      , style "transform"
        ( "translate("
        ++ (String.fromFloat (width - toFloat sliderWidth))
        ++ "px,"
        ++ (String.fromFloat (sliderHeight + sliderTop))
        ++ "px) rotate(-90deg)"
        )
      , style "transform-origin" "top left"
      , style "z-index" "2"
      ]) []
    ] ++ focusIndicators)
