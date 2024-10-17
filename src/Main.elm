module Main exposing (main)

import Basics exposing (..)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize, onAnimationFrameDelta)
import Csv.Decode as DC
import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, style, value, height)
import Html.Events exposing (onInput, onMouseUp, on)
import Html.Keyed
import Http
import Json.Decode as DJ
import List
import List.Extra exposing (minimumBy)
import Set
import Svg
import Svg.Attributes as Svga
import Task
import Time
import Html.Events exposing (onClick)

import BackgroundGradient exposing (ColourStop)
import Base exposing (..)
import Bigbang
import Configure
import Debug
import Dinosaurs
import Geography
import Event exposing (Event, Events, ScreenEvent, eventEnd, zoomHintHeight)
import Gts
import Human
import Stratigraphy
import Ticks exposing (Tick, getTicks)
import TimeSlider exposing (TimeSlider)

jogAmount : Float
jogAmount = 0.0003

jogDecay : Float
jogDecay = 0.6

main : Program () Model Msg
main = Browser.document
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

initWindow : Time -> TimeDelta -> TimeWindow
initWindow top size = { top = top, bottom = top - size }

category2lane : Dict.Dict String Float
category2lane = Dict.fromList
  [ ("strat", 0)
  , ("geog", 1)
  , ("bang", 1)
  , ("human", 1.2)
  , ("dino", 1.4)
  , ("gts", 1.4)
  ]

type MoveMode = MoveFreeFocus String | MoveKeepFocus | MoveToFocus

type alias Model =
  { events : Event.Events
  , moveMode : MoveMode
  , zoomRate : Float
  , moveRate : Float
  -- How much moveRate is scaled by per frame.
  -- 1 means no decay, 0 means instant decay.
  , moveDecay : Float
  , window : TimeWindow
  , width : Float
  , height : Float
  , focused : Maybe FadedScreenEvent
  , stratigraphyIntervals : Maybe Stratigraphy.IntervalDict
  , stratigraphyData : Maybe Stratigraphy.DataDict
  , licenses : Dict.Dict String String
  , dinosaurData : List Dinosaurs.Dinosaur
  , backgroundGradientStops : List BackgroundGradient.ColourStop
  , timeSlider : TimeSlider Msg
  , frameDelta : Float -- time in seconds since last frame
  , enabledCategories : Set.Set String
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
  | NextFrame Float
  | MoveReleased
  | MoveJog Float
  | SliderTo Float  -- set the slider to number between -1 and 1, and set the move rate accordingly
  | Viewport Float Float
  | FocusOn Event
  | DataLoad (String -> Model -> (Result String Model, Cmd Msg)) (Result Http.Error String)
  | StartTimeSliderDrag TimeSlider.TimeSliderStartMsg
  | EnableCategory String
  | DisableCategory String

initialTimeSlider : TimeSlider Msg
initialTimeSlider = TimeSlider.init
  StartTimeSliderDrag
  SliderTo
  MoveReleased
  NoMsg

init : () -> (Model, Cmd Msg)
init _ = let window = initWindow present 100 in
  ( { events = Event.findScreenEvents window [Geography.geography]
    , moveMode = MoveFreeFocus "geog"
    , zoomRate = 1
    , moveRate = 0
    , moveDecay = 1
    , window = window
    , width = 500
    , height = 500
    , focused = Nothing
    , stratigraphyIntervals = Nothing
    , stratigraphyData = Nothing
    , licenses = Dict.empty
    , dinosaurData = []
    , backgroundGradientStops = []
    , timeSlider = initialTimeSlider
    , frameDelta = 0.05
    , enabledCategories = category2lane |> Dict.keys |> Set.fromList
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

-- Returns mv if p is true otherwise Nothing
unless : (a -> Bool) -> Maybe a -> Maybe a
unless p mv = mv |> Maybe.andThen (\v -> if p v then mv else Nothing)

-- If there is a focused event and it is on screen return it.
onScreen : TimeWindow -> Maybe FadedScreenEvent -> Maybe FadedScreenEvent
onScreen { top, bottom } mfse = mfse |> unless
  (\{sev} -> bottom <= eventEnd sev.ev && sev.ev.start <= top)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    adjustMoveMode model0 =
      let
        focused = onScreen model.window model.focused
        nextFocus = case focused of
          Nothing -> MoveKeepFocus
          Just fsev -> MoveFreeFocus fsev.sev.ev.category
      in
        { model0
        | moveMode = case model0.moveMode of
          MoveFreeFocus c -> MoveFreeFocus c
          MoveToFocus -> nextFocus
          MoveKeepFocus -> nextFocus
        , focused = focused
        }
    { top, bottom } = model.window
    height = top - bottom
  in case msg of
    NoMsg -> (model, Cmd.none)
    NextFrame delta -> (updateModel model (delta * 0.001), Cmd.none)
    SliderTo d -> let model1 = adjustMoveMode model in (
      { model1
      | moveRate = floatToMoveRate d
      , moveDecay = 1
      , timeSlider = TimeSlider.setValue model.timeSlider d
      }, Cmd.none)
    MoveJog d ->
      let
        moveRate = model.moveRate - jogAmount * d
        model1 = adjustMoveMode model
      in (
        { model1
        | moveRate = moveRate
        , moveDecay = jogDecay ^ model.frameDelta
        , timeSlider = antilogit moveRate |> TimeSlider.setValue model1.timeSlider
        }, Cmd.none)
    -- stop time movement when released
    MoveReleased -> (
      { model
      | moveRate = 0
      , timeSlider = TimeSlider.reset model.timeSlider
      }, Cmd.none)
    Viewport vp_width vp_height ->
      ({ model
        | width = vp_width
        , height = vp_height
        , timeSlider = TimeSlider.setDimensions
          model.timeSlider
          100
          (vp_height/2)
          (vp_width - 100)
          (vp_height /4)
      }, Cmd.none)
    FocusOn ev ->
      ({ model
        | focused = Just { sev = Event.screenify top height ev, fade = 2 }
        , moveMode = MoveToFocus
        , timeSlider = TimeSlider.reset model.timeSlider
        }
      , Cmd.none
      )
    DataLoad decode (Ok str) -> case decode str model of
      (Err err, cmd) -> let _ = Debug.log "failed to decode" err in (model, cmd)
      (Ok model1, cmd) -> (model1, cmd)
    DataLoad _ (Err err) -> let _ = Debug.log "failed to get" (showError err) in (model, Cmd.none)
    StartTimeSliderDrag sm -> ({model | timeSlider = TimeSlider.start model.timeSlider sm}, Cmd.none)
    EnableCategory cat -> (
      {model
      | enabledCategories = model.enabledCategories |> Set.insert cat
      }, Cmd.none)
    DisableCategory cat -> (
      {model
      | enabledCategories = model.enabledCategories |> Set.remove cat
      }, Cmd.none)

midTime : Event -> Time
midTime ev = (ev.start + eventEnd ev) / 2

getZoomRate : Events -> TimeWindow -> Set.Set String -> Float -> Float -> Float -> Float
getZoomRate events window categories moveRate currentZoomRate frameDelta =
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
    idealHeight = zoomHintHeight mid categories events.zoomHints
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

updateModel : Model -> Float -> Model
updateModel model frameDelta =
  let
    springConstant = 0.03
    damping = 0.07
    { zoomRate, moveMode, moveRate, moveDecay, window, focused } = model
    height = window.top - window.bottom
    half = height / 2
    moveRate1 = case moveMode of
      MoveFreeFocus _ -> moveRate * moveDecay
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
    focused1 = case moveMode of
      MoveToFocus -> focused
      MoveKeepFocus -> onScreen window focused
      MoveFreeFocus  category -> 
        let
          middlest = getMiddlest category events.visibleEvents
        in case focused of
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
    , events = events
    , zoomRate = getZoomRate
      events
      window1
      model.enabledCategories
      effectiveMoveRate
      zoomRate
      model.frameDelta
    , focused = focused2
    , timeSlider = TimeSlider.setValue model.timeSlider <| antilogit moveRate1
    , frameDelta = frameDelta
    }

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ onResize (\w h -> Viewport (toFloat w) (toFloat h))
    , onAnimationFrameDelta NextFrame
    , Browser.Events.onMouseUp <| DJ.succeed MoveReleased
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

eventPosX : Event -> Float
eventPosX ev =
  let
    lane = Dict.get ev.category category2lane |> Maybe.withDefault -1
  in 27 + (lane + ev.xOffset) * 180

-- attributes for a rendered event (so, not an interval) in an event bar
eventAttrs : ScreenEvent -> List (Html.Attribute Msg)
eventAttrs { top, ev } =
  [ style "position" "fixed"
  , style "top" (String.fromFloat (top * 100) ++ "%")
  , style "height" "100px"
  , style "left" (String.fromFloat (eventPosX ev) ++ "px")
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
  , style "left" (String.fromFloat (eventPosX ev) ++ "px")
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

-- get the closest event in the requested category to the middle of the screen
getMiddlest : String -> List ScreenEvent -> Maybe ScreenEvent
getMiddlest category sevs =
  sevs
  |> List.filter (\{ ev } -> ev.category == category)
  |> minimumBy (\{ unclampedMiddle } -> abs (0.5 - unclampedMiddle))

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
view
  { events
  , window
  , width
  , height
  , focused
  , backgroundGradientStops
  , timeSlider
  , enabledCategories
  } =
  let
    timeHeight = window.top - window.bottom
    timeMiddle = window.bottom + timeHeight / 2
    sliderWidth = 100
    sliderHeight = 0.5 * height
    sliderTop = 0.25 * height
    isRelevant : ScreenEvent -> Bool
    isRelevant sev = Set.member sev.ev.category enabledCategories
    relevantVisibleEvents = List.filter isRelevant events.visibleEvents
    -- At what x co-ordinate (in pixels) does the event box start?
    eventBoxLeftPx = 480
    eventBox elts = Html.div
      [ style "position" "fixed"
      , style "width" <| String.fromFloat (width - toFloat eventBoxLeftPx) ++ "px"
      , style "left"  <| String.fromInt eventBoxLeftPx ++ "px"
      , style "top" "0%"
      , style "height" "100%"
      , style "z-index" "1"
      ] elts
    -- focused event box and line joining it to the appropriate place in the event bar
    focusIndicators = case focused of
      Nothing -> [ ("event-box", eventBox []) ]
      Just { sev, fade } -> let opacity = fade |> Basics.min 1 |> String.fromFloat in
        [ ("line-from-event-to-focused", Svg.svg
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
            [ eventPosX sev.ev |> Basics.round |> String.fromInt |> Svga.x1
            , (sev.top + sev.bottom) * 0.5 * height |> Basics.round |> String.fromInt |> Svga.y1
            , eventBoxLeftPx |> String.fromInt |> Svga.x2
            , 0.5 * height |> Basics.round |> String.fromInt |> Svga.y2
            , Svga.stroke "black"
            , Svga.opacity opacity
            ] []
          ])
        , ("event-box", eventBox
          [ Html.div
            [ style "position" "absolute"
            , style "top" "50%"
            , style "transform" "translateY(-50%)"
            , style "max-width" "82%"
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
          ])
        ]
    element = Html.Keyed.node "div"
      [ Html.Events.on "wheel" <| DJ.map MoveJog <| DJ.field "deltaY" DJ.float  -- MoveJog message
      ]
      ([ ("event-bars", Html.Keyed.node "div" [] (List.map renderEvent relevantVisibleEvents))
      , ("time-axis-ticks", getTicks window |> List.map renderTick |> Html.Keyed.node "div" [])
      , ("time-slider", TimeSlider.view timeSlider)
      , ("background-gradient", Html.div
        [ style "background" (getBackground backgroundGradientStops window)
        , style "position" "fixed"
        , style "top" "0"
        , style "left" "0"
        , style "height" "100%"
        , style "width" "100%"
        , style "z-index" "0"
        ] [])
      ] ++
      [("cog"
      , Configure.settings
        EnableCategory
        DisableCategory
        (Dict.keys category2lane)
        enabledCategories
      )] ++ focusIndicators)  -- focused event infromation
  in
    { title = "DeepTime"
    , body = [element]
    }
