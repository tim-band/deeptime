module TimeSlider exposing
  ( TimeSlider
  , TimeSliderStartMsg
  , init
  , drag
  , reset
  , setDimensions
  , setValue
  , start
  , view
  )

import Bitwise
import Json.Decode as Decode
import Html exposing (Html)
import Html.Keyed as Keyed
import Html.Attributes exposing (style)
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Html.Attributes exposing (value)
import Svg
import Svg.Attributes as Svga
import Html.Keyed as Keyed
import Html.Events

type alias DragOrigin = { value : Float, y : Float }

type TimeSliderStartMsg
  = TimeSliderMouseStart  Float
  | TimeSliderTouchStart Float

type alias TimeSlider msg =
  { value : Float
  , width : Float
  , height : Float
  , left : Float
  , top : Float
  , startMsg : TimeSliderStartMsg -> msg
  , dragMsg : Float -> msg
  , releaseMsg : msg
  , noMsg : msg
  , dragOrigin : Maybe DragOrigin
  , isMouse : Bool
  }

init : (TimeSliderStartMsg -> msg) -> (Float -> msg) -> msg -> msg -> TimeSlider msg
init startMessage dragMessage releaseMessage noMessage =
  { value = 0
  , width = 100
  , left = 0
  , height = 100
  , top = 0
  , startMsg = startMessage
  , dragMsg = dragMessage
  , releaseMsg = releaseMessage
  , noMsg = noMessage
  , dragOrigin = Nothing
  , isMouse = False
  }

reset : TimeSlider msg -> TimeSlider msg
reset ts =
  { ts
  | dragOrigin = Nothing
  , isMouse = False -- mouse is no longer dragging
  }

setValue : TimeSlider msg -> Float -> TimeSlider msg
setValue ts v = { ts | value = v }

setDimensions : TimeSlider msg -> Float -> Float  -> Float -> Float  -> TimeSlider msg
setDimensions ts width height left top =
  { ts
  | width = width
  , height = height
  , left = left
  , top = top
  }

start : TimeSlider msg -> TimeSliderStartMsg -> TimeSlider msg
start ts m = case m of
  TimeSliderMouseStart y ->
    { ts
    | dragOrigin = Just { y = y, value = ts.value }
    , isMouse = True
    }
  TimeSliderTouchStart y ->
    { ts
    | dragOrigin = Just { y = y, value = ts.value }
    , isMouse = False
    }

drag : TimeSlider msg -> Float -> TimeSlider msg
drag ts y = { ts | value = y }

eventToDragMsg : TimeSlider msg -> Pointer.Event -> msg
eventToDragMsg ts e = case ts.dragOrigin of
  Nothing -> ts.noMsg
  Just { y, value } ->
    let
      dragY = (Tuple.second e.pointer.pagePos) - y
      dDrag = -dragY * 2 / ts.height
      newValue = dDrag + value |> clamp -1 1
    in ts.dragMsg newValue

touchStart : TimeSlider msg -> Html.Attribute msg
touchStart ts =
  let
    event2msg event = case List.head event.touches of
      Nothing -> ts.noMsg
      Just { pagePos } ->
        pagePos
        |> Tuple.second
        |> TimeSliderTouchStart
        |> ts.startMsg
  in Touch.onWithOptions "touchstart"
    { stopPropagation = True
    , preventDefault = True
    } event2msg

mouseDown  : TimeSlider msg -> Html.Attribute msg
mouseDown ts =
  let
    event2msg { pagePos } =
      pagePos
      |> Tuple.second
      |> TimeSliderMouseStart
      |> ts.startMsg
  in Mouse.onWithOptions "mousedown"
    { stopPropagation = True
    , preventDefault = True
    } event2msg

-- Why did we do it like this? Why not just use Pointer.onUp?
-- It seems sometimes if the pointer is moving while the button
-- is released we miss the "up" message. In this case we use the
-- first "move" message that shows the main button not being
-- pressed to release the control.
pointerMove : TimeSlider msg -> Html.Attribute msg
pointerMove ts =
  let 
    addOptions message =
      { message = message
      , stopPropagation = True
      , preventDefault = True
      }
    decoder = Decode.map2 moveOrRelease
      (Decode.field "buttons" Decode.int)
      Pointer.eventDecoder
    moveOrRelease buttons pev =
      if Bitwise.and 1 buttons == 0
        then ts.releaseMsg
        else eventToDragMsg ts pev
  in Html.Events.custom "pointermove" (Decode.map addOptions decoder)

view : TimeSlider msg -> Html msg
view ts =
  let
    halfTrackWidth = 5.0
    attrs1 =
      [ style "position" "fixed"
      , style "top" (String.fromFloat ts.top ++ "px")
      , style "height" (String.fromFloat ts.height ++ "px")
      , style "left" (String.fromFloat ts.left ++ "px")
      , style "width" (String.fromFloat ts.width ++ "px")
      , style "z-index" "2"
      , style "touch-action" "none"
      ]
    trackAttrs =
      [ style "position" "fixed"
      , style "top" (String.fromFloat ts.top ++ "px")
      , style "height" (String.fromFloat ts.height ++ "px")
      , style "left" (String.fromFloat (ts.left + ts.width / 2 - halfTrackWidth) ++ "px")
      , style "width" (String.fromFloat (halfTrackWidth * 2) ++ "px")
      , style "background" "linear-gradient(0deg, #020e0e, #206060, #020e0e)"
      , style "border" "solid #808080 2px"
      , style "border-radius" "7px"
      , style "touch-action" "none"
      ]
    thumb = Svg.svg
      [ Svga.width "100px"
      , Svga.viewBox "-50 -50 100 100"
      , Svga.version "1.1"
      , style "pointer-events" "none"
      , style "touch-action" "none"
      , style "position" "relative"
      , style "transform" "translateY(-50%)"
      , style "top" (String.fromFloat ((1 - ts.value) * ts.height / 2) ++ "px")
      ]
      [ Svg.defs []
        [ Svg.linearGradient
          [ Svga.id "steel"
          , Svga.gradientTransform "rotate(60)"
          ]
          [ Svg.stop
            [ Svga.offset "0"
            , Svga.stopColor "#7ca0a3"
            ] []
          , Svg.stop
            [ Svga.offset "1"
            , Svga.stopColor "#3e5e5e"
            ] []
          ]]
      , Svg.circle
        [ Svga.cx "0"
        , Svga.cy "0"
        , Svga.r "40"
        , Svga.stroke "black"
        , Svga.fill "url('#steel')"
        , Svga.strokeWidth "4"
        ] []
      , Svg.path
        [ Svga.d "M -20,10 0,30 20,10"
        , Svga.stroke "black"
        , Svga.strokeWidth "4"
        , Svga.strokeLinecap "round"
        , Svga.fill "none"
        ] []
      , Svg.path
        [ Svga.d "M -20,-10 0,-30 20,-10"
        , Svga.stroke "black"
        , Svga.strokeWidth "4"
        , Svga.strokeLinecap "round"
        , Svga.fill "none"
        ] []
      ]
  in if ts.isMouse
    then -- mouse is dragging
      Keyed.node "div" attrs1
        [ ("track", Html.div trackAttrs [])
        , ("thumb", thumb)
        , ("capturer", Html.div
          [ style "top" "0"
          , style "left" "0"
          , style "width" "100%"
          , style "height" "100%"
          , style "position" "fixed"
          , style "z-index" "99"
          , Pointer.onUp <| \_ -> ts.releaseMsg
          , Pointer.onOut <| \_ -> ts.releaseMsg
          , pointerMove ts
          ] [])
        ]
    else -- touch dragging or no dragging
      Keyed.node "div" (attrs1 ++
        [ mouseDown ts
        , touchStart ts
        , pointerMove ts
        , Pointer.onUp <| \_ -> ts.releaseMsg
        ]
      )
      [ ("track", Html.div trackAttrs [])
      , ("thumb", thumb)
      ]
