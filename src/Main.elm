module Main exposing (main)

import Basics
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, style, value)
import Html.Events exposing (onInput, onMouseUp)
import List.Extra exposing (minimumBy)
import Random
import Svg
import Svg.Attributes as Svga
import Task
import Time
import Html.Events exposing (onClick)

main : Program () Model Msg
main = Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

type alias Time = Float
type alias TimeDelta = Float

type alias TimeWindow =
  { top : Time
  , bottom : Time
  }

initWindow : Time -> TimeDelta -> TimeWindow
initWindow top size = { top = top, bottom = top - size }

type MoveMode = MoveConstant | MoveToFocus

type alias Model =
  { events : Events
  , moveMode : MoveMode
  , zoomRate : Float
  , moveRate : Float
  , window : TimeWindow
  , width : Float
  , height : Float
  , focused : Maybe FadedScreenEvent
  }

type alias FadedScreenEvent =
  { sev : ScreenEvent
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
    { y = yEventPosition top height ev
    , ev = ev
    }
  , fade = fsev.fade
  }

type alias Event =
  { category : Int
  , time : Time
  , name : String
  }

type alias ScreenEvent =
  { y: Float
  , ev: Event
  }

type alias Events =
  { previousEvents : List Event  -- in reverse time order
  , visibleEvents : List ScreenEvent
  , nextEvents : List Event -- in forward order
  }

forwardEvents : List Event -> List Event
forwardEvents = List.sortBy (\e -> -e.time)
reverseEvents : List Event -> List Event
reverseEvents = List.sortBy .time

yEventPosition : Time -> TimeDelta -> Event -> Time
yEventPosition top height ev = (top - ev.time) / height

screenify : Time -> TimeDelta -> Event -> ScreenEvent
screenify top height ev = { y = yEventPosition top height ev, ev = ev }

findScreenEvents : TimeWindow -> List Event -> Events
findScreenEvents window evs =
  let
    { top, bottom } = window
    height = top - bottom
    tooEarly { time } = time < bottom
    tooLate { time } = top < time
    justRight t = not (tooEarly t || tooLate t)
    visibles = evs |> List.filter justRight |> List.map (screenify top height)
  in
    { previousEvents = List.filter tooEarly evs |> forwardEvents
    , visibleEvents = visibles
    , nextEvents = List.filter tooLate evs |> reverseEvents
    }

adjustScreenEvents : TimeWindow -> Events -> Events
adjustScreenEvents { top, bottom } { previousEvents, visibleEvents, nextEvents } =
  let
    tooEarly { time } = time < bottom
    tooLate { time } = top < time
    (earlies1, notEarlies) = List.partition (.ev >> tooEarly) visibleEvents
    earlies = List.map .ev earlies1 |> forwardEvents
    (lates1, notLates) = List.partition (.ev >> tooLate) notEarlies
    stillVisible = List.map .ev notLates
    lates = List.map .ev lates1 |> reverseEvents
    (stillPrevs, nowIn1) = List.partition tooEarly previousEvents
    (stillNexts, nowIn2) = List.partition tooLate nextEvents
    visible = nowIn1 ++ nowIn2 ++ stillVisible
    height = top - bottom
  in
    { previousEvents = earlies ++ stillPrevs
    , visibleEvents = List.map (screenify top height) visible
    , nextEvents = lates ++ stillNexts
    }

randomJoin : Random.Generator (Random.Generator a) -> Random.Generator a
randomJoin rr = rr |> Random.andThen (\r -> r)

randomChar : Random.Generator Char
randomChar = Random.int (Char.toCode 'a') (Char.toCode 'z') |> Random.map Char.fromCode

randomCharList : Random.Generator (List Char)
randomCharList = Random.weighted
  (1, Random.constant [])
  [(9, Random.map2 (::) randomChar (Random.lazy (\_ -> randomCharList)))]
  |> randomJoin

randomCharListMin : Int -> Random.Generator (List Char)
randomCharListMin min = Random.map2 (++) (Random.list min randomChar) randomCharList

randomString : Int -> Random.Generator String
randomString min = Random.map String.fromList (randomCharListMin min)

randomEvent : Time -> TimeDelta -> Random.Generator Event
randomEvent max size = Random.int 0 4 |> Random.andThen (\cat ->
  randomString 3 |> Random.andThen (\name ->
  Random.float (max - size) max |> Random.map (\t ->
  { category = cat, time = t, name = name })))

present : Time
present = 14e9

randomEvents : Random.Generator (List Event)
randomEvents =
  Random.list 999 (randomEvent present 1) |> Random.andThen (\es0 ->
  Random.list 999 (randomEvent present 100) |> Random.andThen (\es1 ->
  Random.list 999 (randomEvent present 1000) |> Random.andThen (\es2 ->
  Random.list 999 (randomEvent present 10000) |> Random.andThen (\es3 ->
  Random.list 999 (randomEvent present 100000) |> Random.andThen (\es4 ->
  Random.constant (es0 ++ es1 ++ es2 ++ es3 ++ es4))))))

type Msg
  = NoMsg
  | SetEvents (List Event)
  | NextFrame
  | ZoomIn
  | ZoomOut
  | ZoomStop
  | Move Float
  | MoveReleased
  | Viewport Float Float
  | FocusOn Event

init : () -> (Model, Cmd Msg)
init _ = let window = initWindow present 100 in
  ( { events = findScreenEvents window []
    , moveMode = MoveConstant
    , zoomRate = 1
    , moveRate = 0
    , window = window
    , width = 500
    , height = 500
    , focused = Nothing
    }
  , Cmd.batch
    [ Random.generate SetEvents randomEvents
    , Task.attempt viewportMsg getViewport
    ]
  )

viewportMsg : Result x Browser.Dom.Viewport -> Msg
viewportMsg r = case r of
  Ok { viewport } -> let { width, height } = viewport in Viewport width height
  Err _ -> NoMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoMsg -> (model, Cmd.none)
    ZoomStop -> ({ model | zoomRate = 1 }, Cmd.none)
    ZoomOut -> ({ model | zoomRate = 1.02 }, Cmd.none)
    ZoomIn -> ({ model | zoomRate = 0.98 }, Cmd.none)
    NextFrame -> (updateModel model, Cmd.none)
    SetEvents evs1 ->
      ( { model | events = findScreenEvents model.window evs1 }
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
          | focused = Just { sev = screenify top height ev, fade = 2 }
          , moveMode = MoveToFocus
          }
        , Cmd.none
        )

getZoomRate : Int -> Int -> Float -> Events -> Float -> Float
getZoomRate minEvents maxEvents zoomRateMultiplier events moveDist =
  let
    visibleCount = List.length events.visibleEvents
    stationaryRate = if visibleCount < minEvents
      then toFloat (minEvents - visibleCount)
      else if maxEvents < visibleCount
      then toFloat (maxEvents - visibleCount)
      else 0
    motionRate = if moveDist < 1
      then 1 - moveDist
      else if 1 < moveDist
      then moveDist - 1
      else 0
  in Basics.e ^ (zoomRateMultiplier * (stationaryRate + motionRate))

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
            force = (foc.sev.ev.time - halfWay) * 0.03
          in (moveRate + force) * 0.80
    mid0 = window.top - half + moveRate1 * half
    newHalf = half * zoomRate
    mid = Basics.max mid0 0
    window1 = { top = mid + newHalf, bottom = mid - newHalf }
    events = adjustScreenEvents window1 model.events
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
    , zoomRate = getZoomRate 10 30 0.001 events (moveRate1 * 100)
    , focused = focused2
    }

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ onResize (\w h -> Viewport (toFloat w) (toFloat h))
    , Time.every 20 (\_ -> NextFrame)
    ]

renderEvent : ScreenEvent -> Html Msg
renderEvent e = div (eventAttrs e) [ text "*" ]

eventPosX : Event -> Int
eventPosX ev = 5 + ev.category * 5

eventAttrs : ScreenEvent -> List (Html.Attribute Msg)
eventAttrs { y, ev } =
  [ style "position" "fixed"
  , style "top" (String.fromFloat (y * 100) ++ "%")
  , style "left" (String.fromInt (eventPosX ev) ++ "%")
  , style "z-index" "2"
  , style "transform" "translateY(-30%)"
  , style "cursor" "pointer"
  , onClick <| FocusOn ev
  ]

type alias Tick =
  { time : Time
  , rendering : String
  }

tickList : Time -> TimeDelta -> Time -> List Tick
tickList start step min = if start <= min
  then []
  else { time = start, rendering = String.fromFloat start } :: tickList (start - step) step min

getTicks : TimeWindow -> List Tick
getTicks window =
  let
    { top, bottom } = window
    height = top - bottom
    maxTickSize = height / 5
    digitCount = Basics.logBase 10 maxTickSize |> Basics.floor
    minTickSize = 10 ^ toFloat digitCount
    tickSizes = List.filter ((>=) maxTickSize) [minTickSize * 5, minTickSize * 2]
    tickSize = case List.head tickSizes of
      Just t -> t
      Nothing -> minTickSize
    firstTick = toFloat (Basics.ceiling (top / tickSize)) * tickSize
  in tickList firstTick tickSize bottom

renderTick : TimeWindow -> Tick -> Html Msg
renderTick w { time, rendering } = Html.div
  [ style "position" "fixed"
  , style "top" (String.fromFloat ((w.top - time) / (w.top - w.bottom) * 100) ++ "%")
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
getMiddlest = minimumBy (\{y} -> abs (0.5 - y))

view : Model -> Html Msg
view { events, window, width, height, moveRate, focused } =
  let
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
            , sev.y * height |> Basics.round |> String.fromInt |> Svga.y1
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
    , getTicks window |> List.map (renderTick window) |> Html.div []
    , Html.input (stopped ++
      [ attribute "type" "range"
      , attribute "min" "-1"
      , attribute "max" "1"
      , attribute "step" "0.01"
      , onInput <| stringToMove 0.1
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
