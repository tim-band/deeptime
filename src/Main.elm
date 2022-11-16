module Main exposing (main)

import Basics
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, style, value)
import Html.Events exposing (onInput, onMouseUp)
import Random
import Task
import Time

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
initWindow top size = { top = top, bottom = top + size }

type alias Model =
  { events : Events
  , zoomRate : Float
  , moveRate : Float
  , window : TimeWindow
  , width : Float
  , height : Float
  }

type alias Event =
  { category: Int
  , time: Time
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
forwardEvents = List.sortBy .time
reverseEvents : List Event -> List Event
reverseEvents = List.sortBy (\e -> -e.time)

screenify : Time -> TimeDelta -> Event -> ScreenEvent
screenify top height ev = { y = (ev.time - top) / height, ev = ev }

findScreenEvents : TimeWindow -> List Event -> Events
findScreenEvents window evs =
  let
    { top, bottom } = window
    height = bottom - top
    tooEarly { time } = time < top
    tooLate { time } = bottom < time
    justRight t = not (tooEarly t || tooLate t)
    visibles = evs |> List.filter justRight |> List.map (screenify top height)
  in
    { previousEvents = List.filter tooEarly evs |> reverseEvents
    , visibleEvents = visibles
    , nextEvents = List.filter tooLate evs |> forwardEvents
    }

adjustScreenEvents : TimeWindow -> Events -> Events
adjustScreenEvents { top, bottom } { previousEvents, visibleEvents, nextEvents } =
  let
    tooEarly { time } = time < top
    tooLate { time } = bottom < time
    (earlies1, notEarlies) = List.partition (.ev >> tooEarly) visibleEvents
    earlies = List.map .ev earlies1 |> reverseEvents
    (lates1, notLates) = List.partition (.ev >> tooLate) notEarlies
    stillVisible = List.map .ev notLates
    lates = List.map .ev lates1 |> forwardEvents
    (stillPrevs, nowIn1) = List.partition tooEarly previousEvents
    (stillNexts, nowIn2) = List.partition tooLate nextEvents
    visible = nowIn1 ++ nowIn2 ++ stillVisible
    height = bottom - top
  in
    { previousEvents = earlies ++ stillPrevs
    , visibleEvents = List.map (screenify top height) visible
    , nextEvents = lates ++ stillNexts
    }

randomEvent : Float -> Random.Generator Event
randomEvent max = Random.int 0 4 |> Random.andThen (\cat ->
  Random.float 0 max |> Random.map (\t ->
  { category = cat, time = t }))
randomEvents : Random.Generator (List Event)
randomEvents =
  Random.list 999 (randomEvent 100) |> Random.andThen (\es1 ->
  Random.list 999 (randomEvent 1000) |> Random.andThen (\es2 ->
  Random.list 999 (randomEvent 10000) |> Random.andThen (\es3 ->
  Random.list 999 (randomEvent 100000) |> Random.andThen (\es4 ->
  Random.constant (es1 ++ es2 ++ es3 ++ es4)))))

type Msg
  = NoMsg
  | SetEvents (List Event)
  | Tick
  | ZoomIn
  | ZoomOut
  | ZoomStop
  | Move Float
  | MoveReleased
  | Viewport Float Float

init : () -> (Model, Cmd Msg)
init _ = let window = initWindow 0 100 in
  ( { events = findScreenEvents window []
    , zoomRate = 1
    , moveRate = 0
    , window = window
    , width = 500
    , height = 500
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
    Tick -> (tickModel model, Cmd.none)
    SetEvents evs1 ->
      ( { model | events = findScreenEvents model.window evs1 }
      , Cmd.none
      )
    Move d -> ({ model | moveRate = d }, Cmd.none)
    -- just stop when released for now
    MoveReleased -> ({ model | moveRate = 0 }, Cmd.none)
    Viewport width height -> ({ model | width = width, height = height }, Cmd.none)

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

tickModel : Model -> Model
tickModel model =
  let
    { zoomRate, moveRate, window } = model
    half = (window.bottom - window.top) / 2
    mid0 = window.top + half + moveRate * half
    newHalf = half * zoomRate
    mid = Basics.max mid0 newHalf
    window1 = { top = mid - newHalf, bottom = mid + newHalf }
    events = adjustScreenEvents window1 model.events
  in
    { model
    | window = window1
    , events = events
    , zoomRate = getZoomRate 10 30 0.001 events (moveRate * 100)
    }

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ onResize (\w h -> Viewport (toFloat w) (toFloat h))
    , Time.every 20 (\_ -> Tick)
    ]

renderEvent : ScreenEvent -> Html Msg
renderEvent e = div (eventAttrs e) [ text "*" ]

eventAttrs : ScreenEvent -> List (Html.Attribute msg)
eventAttrs { y, ev } =
  [ style "position" "fixed"
  , style "top" (String.fromFloat (y * 100) ++ "%")
  , style "left" (String.fromInt (5 + ev.category * 15) ++ "%")
  ]

stepList : number -> number -> number -> List number
stepList start step max = if max < start
  then []
  else start :: stepList (start + step) step max

getTicks : TimeWindow -> List Float
getTicks window =
  let
    { top, bottom } = window
    height = bottom - top
    maxTickSize = height / 5
    digitCount = Basics.logBase 10 maxTickSize |> Basics.floor
    minTickSize = 10 ^ toFloat digitCount
    tickSizes = List.filter ((>=) maxTickSize) [minTickSize * 5, minTickSize * 2]
    tickSize = case List.head tickSizes of
      Just t -> t
      Nothing -> minTickSize
    firstTick = toFloat (Basics.ceiling (top / tickSize)) * tickSize
  in stepList firstTick tickSize bottom

renderTick : TimeWindow -> Float -> Html Msg
renderTick w y = Html.div
  [ style "position" "fixed"
  , style "top" (String.fromFloat ((y - w.top) / (w.bottom - w.top) * 100) ++ "%")
  , style "left" "0"
  , style "border-top" "black solid 1px"
  ] [ Html.text <| String.fromFloat y ]

logit : Float -> Float
logit x = Basics.logBase Basics.e ((1 + x) / (1 - x))

stringToMove : Float -> String -> Msg
stringToMove speed s = case String.toFloat s of
  Just x -> Move <| logit (x * 0.999) * speed
  Nothing -> NoMsg

view : Model -> Html Msg
view { events, window, width, height, moveRate } =
  let
    sliderWidth = 30
    sliderHeight = 0.5 * height
    sliderTop = 0.25 * height
    stopped = if moveRate == 0 then [ value "0" ] else []
    { visibleEvents } = events
  in div []
    [ visibleEvents |> List.map renderEvent |> Html.div []
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
      ]) []
    ]
