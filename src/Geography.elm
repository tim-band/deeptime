module Geography exposing (..)

import Html
import Html.Attributes exposing (attribute)

import Base exposing (present)
import Event exposing (Event)

geography : Event
geography =
  { name = "Geography"
  , category = 0
  , start = present - 410e6
  , end = present
  , fill = "#604010"
  , color = "yellow"
  , pointCount = 42
  , renderPoint = \i -> Html.img
    [ attribute "src"
      ( "resources/geography/robinson/image_"
      ++ String.fromInt (410 - i * 10)
      ++ ".jpg"
      )] []
  }
