module Geography exposing (..)

import Html
import Html.Attributes exposing (attribute)

import Base exposing (present)
import Event exposing (Event)

imageDir : Float -> String
imageDir w = if w < 1000
  then "resources/geography/robinson/300x150"
  else if w < 1600
  then "resources/geography/robinson/500x250"
  else "resources/geography/robinson/800x400"

geography : Event
geography =
  { name = "Geography"
  , category = 0
  , start = present - 410e6
  , end = Just present
  , fill = "#604010"
  , color = "yellow"
  , pointCount = 42
  , renderPoint = \w i -> Html.img
    [ attribute "src"
      ( imageDir w
      ++ "/image_"
      ++ String.fromInt (410 - i * 10)
      ++ ".jpg"
      )] []
  }
