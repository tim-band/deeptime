module Geography exposing (..)

import Html
import Html.Attributes exposing (attribute)

import Base exposing (present)
import Event exposing (Event)

geography : Event
geography =
  { name = "geography"
  , category = 7
  , start = present - 4.1e9
  , end = present
  , fill = "#604010"
  , color = "white"
  , pointCount = 42
  , renderPoint = \i -> Html.img
    [ attribute "src"
      ( "resources/geography/robinson/image_"
      ++ String.fromInt (410 - i * 10)
      ++ ".jpg"
      )] []
  }
