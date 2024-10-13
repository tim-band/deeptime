module TimeSlider exposing (TimeSlider, drag, reset, setHeight, setValue, start, view)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events.Extra.Pointer as Pointer
import Html.Attributes exposing (value)

type alias DragOrigin = { value : Float, y : Float }

type alias TimeSlider msg =
  { value : Float
  , height : Float
  , top : Float
  , startMsg : Float -> msg
  , dragMsg : Float -> msg
  , releaseMsg : msg
  , noMsg : msg
  , dragOrigin : Maybe DragOrigin
  }

reset : TimeSlider msg -> TimeSlider msg
reset ts = { ts | dragOrigin = Nothing }

setValue : TimeSlider msg -> Float -> TimeSlider msg
setValue ts v = { ts | value = v }

setHeight : TimeSlider msg -> Float -> Float  -> TimeSlider msg
setHeight ts height top =
  { ts
  | height = height
  , top = top
  }

getY : Pointer.Event -> Float
getY e = e.pointer.screenPos |> Tuple.second

start : TimeSlider msg -> Float -> TimeSlider msg
start ts y =
  { ts
  | dragOrigin = Just { y = y, value = ts.value }
  }

drag : TimeSlider msg -> Float -> TimeSlider msg
drag ts y = { ts | value = y }

eventToDragMsg : TimeSlider msg -> Pointer.Event -> msg
eventToDragMsg ts e = case ts.dragOrigin of
  Nothing -> ts.noMsg
  Just { y, value } ->
    let
      dragY = getY e - y
      dDrag = -dragY * 2 / ts.height
      newValue = dDrag + value |> clamp -1 1
    in ts.dragMsg newValue

view : TimeSlider msg -> List (Html.Attribute msg) -> Html msg
view ts attrs =
  let
    attrs1 = style "position" "fixed"
      :: style "top" (String.fromFloat ts.top ++ "px")
      :: style "height" (String.fromFloat ts.height ++ "px")
      :: style "background" "linear-gradient(0deg, #ffff80, #0080ff, #ffff80)"
      :: style "border" "solid #808080 2px"
      :: style "border-radius" "7px"
      :: style "z-index" "2"
      :: style "touch-action" "none"
      :: attrs
    thumb = Html.div
      [ style "height" "30px"
      , style "position" "relative"
      , style "top" (String.fromFloat (((1 - ts.value) * ts.height / 2) - 15) ++ "px")
      , style "width" "100%"
      , style "height" "30px"
      , style "background" "#8090a0"
      , style "border" "solid black 1px"
      , style "pointer-events" "none"
      , style "touch-action" "none"
      ] []
  in case ts.dragOrigin of
    Nothing ->  -- Not dragging yet
      Html.div (attrs1 ++
        [ Pointer.onDown (\e -> e |> getY |> ts.startMsg)
        ]
      ) [ thumb ]
    Just _ -> -- Dragging now
      Html.div attrs1
      [ thumb
      , Html.div
        [ style "top" "0"
        , style "left" "0"
        , style "width" "100%"
        , style "height" "100%"
        , style "position" "fixed"
        , Pointer.onMove <| eventToDragMsg ts
        , Pointer.onUp <| \_ -> ts.releaseMsg
        , Pointer.onOut <| \_ -> ts.releaseMsg
        , Pointer.onMove <| eventToDragMsg ts
        , Pointer.onUp <| \_ -> ts.releaseMsg
        , Pointer.onOut <| \_ -> ts.releaseMsg
        ] []
      ]
