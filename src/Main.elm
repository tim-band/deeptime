module Main exposing (main)

import Basics
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (attribute, style, value)
import Html.Events exposing (onInput, onMouseDown, onMouseUp, onMouseLeave)
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
  { events : List Event
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

randomEvent : Random.Generator Event
randomEvent = Random.int 0 4 |> Random.andThen (\cat ->
  Random.float 0 100 |> Random.andThen (\t ->
  Random.constant { category = cat, time = t }))
randomEvents : Random.Generator (List Event)
randomEvents = Random.list 999 randomEvent

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
init _ =
  ( { events = []
    , zoomRate = 1
    , moveRate = 0
    , window = initWindow 0 100
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
    SetEvents evs1 -> ({ model | events = evs1 }, Cmd.none)
    Move d -> ({ model | moveRate = d }, Cmd.none)
    -- just stop when released for now
    MoveReleased -> ({ model | moveRate = 0 }, Cmd.none)
    Viewport width height -> ({ model | width = width, height = height }, Cmd.none)

tickModel : Model -> Model
tickModel model =
  let
    { zoomRate, moveRate, window } = model
    half = (window.bottom - window.top) / 2
    mid = window.top + half + moveRate * half
    newHalf = half * zoomRate
  in
    { model | window = { top = mid - newHalf, bottom = mid + newHalf } }

subscriptions : Model -> Sub Msg
subscriptions { zoomRate, moveRate } =
  let
    resize = onResize (\w h -> Viewport (toFloat w) (toFloat h))
    all = if zoomRate == 1 && moveRate == 0
      then resize
      else Sub.batch [resize, Time.every 20 (\_ -> Tick)]
  in all

type alias ScreenEvent =
  { y: Float
  , ev: Event
  }

findScreenEvents : TimeWindow -> List Event -> List ScreenEvent
findScreenEvents window evs0 =
  let
    { top, bottom } = window
    height = bottom - top
    fse evs = case evs of
      [] -> []
      (e :: es) -> let t = e.time in if t < top || bottom < t
        then fse es
        else { y = (t - top) / height, ev = e } :: fse es
  in fse evs0

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
    zoomButtonHeight = 20
    zoomButtonHeightText = String.fromInt zoomButtonHeight ++ "px"
    stopped = if moveRate == 0 then [ value "0" ] else []
  in div []
    [ button
      [ onMouseDown ZoomOut
      , onMouseUp ZoomStop
      , onMouseLeave ZoomStop
      , style "position" "fixed"
      , style "top" zoomButtonHeightText
      , style "height" zoomButtonHeightText
      , style "right" "0"
      , style "width" zoomButtonHeightText
      ] [ text "-" ]
    , button
      [ onMouseDown ZoomIn
      , onMouseUp ZoomStop
      , onMouseLeave ZoomStop
      , style "position" "fixed"
      , style "top" "0"
      , style "height" zoomButtonHeightText
      , style "right" "0"
      , style "width" zoomButtonHeightText
      ] [ text "+" ]
    , findScreenEvents window events |> List.map renderEvent |> Html.div []
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
