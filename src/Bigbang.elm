module Bigbang exposing (bigbang_events_url, decode)

import Html
import Json.Decode as D

import Base exposing (contrastColor)
import Event exposing (Event)

bigbang_events_url : String
bigbang_events_url = "resources/bigbang.json"

seconds_per_year : Float
seconds_per_year = 365.25 * 24 * 3600

decode : D.Decoder (List Event)
decode =
  let
    bb2e name start end fill description =
      { name = name
      , category = 3
      , start = start / seconds_per_year
      , end = end / seconds_per_year |> Just
      , fill = fill
      , color = contrastColor fill
      , pointCount = 1
      , renderPoint = \_ _ -> Html.text description
      }
    bb_dec : D.Decoder Event
    bb_dec = D.map5 bb2e
      (D.field "name" D.string)
      (D.field "start" D.float)
      (D.field "end" D.float)
      (D.field "fill" D.string)
      (D.field "description" D.string)
  in D.list bb_dec
