module Main exposing (main)

import Basics exposing (..)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Csv.Decode as DC
import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, style, value, height)
import Html.Events exposing (onInput, onMouseUp, on)
import Html.Events.Extra.Touch as Touch
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

import BackgroundGradient exposing (ColourStop)
import Debug
import Dinosaurs
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
main = Browser.document
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
  , licenses : Dict.Dict String String
  , dinosaurData : List Dinosaurs.Dinosaur
  , backgroundGradientStops : List BackgroundGradient.ColourStop
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
  | SliderTo Float  -- set the slider to number between -1 and 1, and set the move rate accordingly
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
    , licenses = Dict.empty
    , dinosaurData = []
    , backgroundGradientStops = []
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
      { url = BackgroundGradient.background_gradient_url
      , expect = Http.expectString
        (DataLoad backgroundGradientUpdate)
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
    , Http.get
      { url = "resources/licenses.json"
      , expect = Http.expectString <| DataLoad licenseUpdate
      }
    , Http.get
      { url = Dinosaurs.dinosaurs_url
      , expect = Http.expectString <| DataLoad dinosaurUpdate
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

backgroundGradientUpdate : String -> Model -> (Result String Model, Cmd Msg)
backgroundGradientUpdate resp model =
  let
    res = DJ.decodeString BackgroundGradient.decode resp
    updateBackgroundGradient stops =
      { model
      | backgroundGradientStops = stops
      }
    in (res |> Result.map updateBackgroundGradient |> decodeJsonResult, Cmd.none)

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
      | events = Event.findScreenEventsCustom 10 0.3 model.window events
        |> Event.mergeScreenEvents model.events
      }
  in (res |> Result.map updateGts |> decodeCsvResult, Cmd.none)

updateLicenceAndDinosaurEvents : Dict.Dict String String -> List Dinosaurs.Dinosaur -> Model -> Model
updateLicenceAndDinosaurEvents lics dinos model =
  { model
  | licenses = lics
  , dinosaurData = dinos
  , events = Event.mergeScreenEvents model.events <|
    Event.findScreenEvents model.window <|
    Dinosaurs.dinosaurEvents dinos lics
  }

dinosaurUpdate : String -> Model -> (Result String Model, Cmd Msg)
dinosaurUpdate resp model =
  let
    res = DJ.decodeString Dinosaurs.decode resp
    updateDino dinos =
      { model
      | dinosaurData = dinos
      }
  in if Dict.isEmpty model.licenses
    then (res |> Result.map updateDino |> decodeJsonResult, Cmd.none)
    else (res |> Result.map (\dinos -> updateLicenceAndDinosaurEvents model.licenses dinos model) |> decodeJsonResult, Cmd.none)

licenseUpdate: String -> Model -> (Result String Model, Cmd Msg)
licenseUpdate resp model =
  let
    res = DJ.decodeString (DJ.dict DJ.string) resp
    updateLicense lics = { model | licenses = lics }
  in if List.isEmpty model.dinosaurData
    then (res |> Result.map updateLicense |> decodeJsonResult, Cmd.none)
    else (res |> Result.map (\lics -> updateLicenceAndDinosaurEvents lics model.dinosaurData model) |> decodeJsonResult, Cmd.none)

onScreen : TimeWindow -> Maybe FadedScreenEvent -> Bool
onScreen { top, bottom } mfse = case mfse of
  Nothing -> False
  Just { sev } -> bottom <= eventEnd sev.ev && sev.ev.start <= top

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    adjustMoveMode model0 =
      let
        focusedEventStillVisible = onScreen model.window model.focused
        nextFocus = if focusedEventStillVisible
          then MoveKeepFocus
          else MoveFreeFocus
      in
        { model0
        | moveMode = case model0.moveMode of
          MoveFreeFocus -> MoveFreeFocus
          MoveToFocus -> nextFocus
          MoveKeepFocus -> nextFocus
        , focused = if focusedEventStillVisible
          then model0.focused
          else Nothing
        }
    { top, bottom } = model.window
    height = top - bottom
  in case msg of
    NoMsg -> (model, Cmd.none)
    NextFrame -> (updateModel model, Cmd.none)
    Move d -> let model1 = adjustMoveMode model in (
      { model1
      | moveRate = d
      , moveDecay = 1
      , setSlider = Nothing
      }, Cmd.none)
    SliderTo d -> let model1 = adjustMoveMode model in (
      { model1
      | moveRate = floatToMoveRate d
      , setSlider = Just d
      , moveDecay = 1
      }, Cmd.none)
    MoveJog d ->
      let
        moveRate = model.moveRate - jogAmount * d
        model1 = adjustMoveMode model
      in (
        { model1
        | moveRate = moveRate
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

-- produce a keyed node for the HTML representing an event (or interval)
-- on the timeline
renderEvent : ScreenEvent -> (String, Html Msg)
renderEvent e =
  ( "event_" ++ e.ev.name
  , case e.ev.end of
    Just _ -> div (intervalAttrs e) [ text e.ev.name ]
    Nothing -> div (eventAttrs e) [ text e.ev.name ]
  )

eventPosX : Event -> Int
eventPosX ev = 5 + ev.category * 4 + ev.xOffset

-- attributes for a rendered event (so, not an interval) in an event bar
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
  , style "text-shadow" "-1px 1px #444"
  , style "writing-mode" "vertical-rl"
  , style "overflow" "hidden"
  , style "color" ev.color
  , style "border-style" "solid"
  , style "border-color" "#666"
  , style "border-width" "0px 0px 2px 2px"
  , style "border-radius" "12px"
  , onClick <| FocusOn ev
  ]

renderTick : Tick -> (String, Html Msg)
renderTick { y, rendering } = ("tick_" ++ rendering, Html.div
  [ style "position" "fixed"
  , style "top" (String.fromFloat (y * 100) ++ "%")
  , style "left" "0"
  , style "border-top" "black solid 1px"
  , style "z-index" "2"
  ] [ Html.text rendering ])

logit : Float -> Float
logit x = Basics.logBase Basics.e ((1 + x) / (1 - x))

-- y = ln (1+x)/(1-x)
-- e^y = (2-(1-x))/(1-x) = 2/(1-x) - 1
-- e^y + 1 = 2/(1-x)
-- 2/(e^y + 1) = 1-x
-- x = 1 - 2/(e^y + 1)
antilogit : Float -> Float
antilogit y = 1 - 2 / (1 + Basics.e ^ y)

floatToMoveRate : Float -> Float
floatToMoveRate x = x * 0.999 |> logit

stringToMove : String -> Msg
stringToMove s = case String.toFloat s of
  Just x -> x |> floatToMoveRate |> Move
  Nothing -> NoMsg

getMiddlest : List ScreenEvent -> Maybe ScreenEvent
getMiddlest = minimumBy (\{ unclampedMiddle } -> abs (0.5 - unclampedMiddle))

getBackground : List ColourStop -> TimeWindow -> String
getBackground backgroundGradientStops { top, bottom } =
  let
    lerpTimeColour : Float -> ColourStop -> ColourStop -> ColourStop
    lerpTimeColour t cs0 cs1 = let p = (t - cs0.t) / (cs1.t - cs0.t) in
      ColourStop t (cs0.r + (cs1.r - cs0.r) * p) (cs0.g + (cs1.g - cs0.g) * p) (cs0.b + (cs1.b - cs0.b) * p)
    code0 : Int
    code0 = Char.toCode '0'
    codeA10 : Int
    codeA10 = Char.toCode 'a' - 10
    hexgit v =
      if v < 0
      then '0'
      else if v < 10
      then code0 + v |> Char.fromCode
      else codeA10 + v |> Char.fromCode
    hex v =
      let
        v1 = v * 16 |> Basics.floor
        v0 = Basics.floor (v * 256) - v1 * 16
      in if 15 < v1 then "ff" else String.fromList [hexgit v1, hexgit v0]
    colour cs = "#" ++ hex cs.r ++ hex cs.g ++ hex cs.b
    stopPercentage t = 100 * (t - bottom) / (top - bottom) |> String.fromFloat
    stopDefinition : ColourStop -> String
    stopDefinition cs = colour cs ++ " " ++ stopPercentage cs.t ++ "%"
    getRemainingGradStops gradStop gradStops = case gradStops of
        [] -> [gradStop, { gradStop | t = top }]
        gs :: gss -> if gs.t < top
          then gradStop :: getRemainingGradStops gs gss
          else [gradStop, lerpTimeColour top gradStop gs]
    getScreenGradStops gradStop gradStops = case gradStops of
        [] -> colour gradStop
        gs :: gss -> if gs.t < bottom
          then getScreenGradStops gs gss
          else
            let
              bottomColour = lerpTimeColour bottom gradStop gs |> colour
              remainingStops = getRemainingGradStops gs gss
              stopDefs = remainingStops |> List.map stopDefinition |> String.join ","
            in "linear-gradient(to top," ++ bottomColour ++ " 0%," ++ stopDefs ++ ")"
  in case backgroundGradientStops of
      [] -> ""
      gs :: gss -> getScreenGradStops gs gss

focusedEventBackground : Event -> String
focusedEventBackground ev = String.concat
  [ "linear-gradient(135deg, "
  , ev.fill
  ,", "
  , anticontrastColor ev.fill
  , ")"
  ]

dontPropagateEvent : String -> Html.Attribute Msg
dontPropagateEvent ev = Html.Events.stopPropagationOn ev <| DJ.succeed (NoMsg, True)

view : Model -> Browser.Document Msg
view { events, window, width, height, focused, setSlider, backgroundGradientStops } =
  let
    timeHeight = window.top - window.bottom
    timeMiddle = window.bottom + timeHeight / 2
    sliderWidth = 30
    sliderHeight = 0.5 * height
    sliderTop = 0.25 * height
    setPosition = case setSlider of
      Nothing -> []
      Just v -> [ String.fromFloat v |> value ]
    touchMoveSlider : Touch.Event -> Msg
    touchMoveSlider event = case List.head event.touches of
      Nothing -> NoMsg
      Just touch -> 1 - clamp 0 2 (2 * (Tuple.second touch.clientPos - sliderTop) / sliderHeight) |> SliderTo
    { visibleEvents } = events
    eventBox elts = Html.div
      [ style "position" "fixed"
      , style "width" "50%"
      , style "left" "50%"
      , style "top" "0%"
      , style "height" "100%"
      , style "z-index" "1"
      ] elts
    -- focused event box and line joining it to the appropriate place in the event bar
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
            , style "max-width" "92%"
            , style "max-height" "90%"
            , style "overflow" "auto"
            , style "scrollbar-width" "thin"
            , style "opacity" opacity
            , style "color" sev.ev.color
            , style "padding" "20px"
            , style "border-style" "solid"
            , style "border-width" "0px 0px 2px 2px"
            , style "border-color" "#444"
            , style "border-radius" "20px"
            , focusedEventBackground sev.ev |> style "background-image"
            , dontPropagateEvent "wheel"
            ]
            [ Event.pointIndex timeMiddle sev.ev
            |> sev.ev.renderPoint width
            |> Html.map (\_ -> NoMsg)
            ]
          ]
        ]
    element = div
      [ Html.Events.on "wheel" <| DJ.map MoveJog <| DJ.field "deltaY" DJ.float  -- MoveJog message
      ]
      ([ Html.Keyed.node "div" [] (List.map renderEvent visibleEvents)  -- Event bars
      , getTicks window |> List.map renderTick |> Html.Keyed.node "div" []  -- Time axis ticks
      , Html.input (setPosition ++  -- Time travel slider
        [ attribute "type" "range"
        , attribute "min" "-1"
        , attribute "max" "1"
        , attribute "step" "0.01"
        , onInput stringToMove
        , onMouseUp MoveReleased
        , Touch.onEnd <| \_ -> MoveReleased
        , Touch.onMove touchMoveSlider
        , Touch.onStart touchMoveSlider
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
        , style "touch-action" "none"
        ]) []
      , Html.div  -- Background gradient
        [ style "background" (getBackground backgroundGradientStops window)
        , style "position" "fixed"
        , style "top" "0"
        , style "left" "0"
        , style "height" "100%"
        , style "width" "100%"
        , style "z-index" "0"
        ] []
      ] ++ focusIndicators)  -- focused event infromation
  in
    { title = "DeepTime"
    , body = [element]
    }
