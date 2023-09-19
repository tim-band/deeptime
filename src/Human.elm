module Human exposing (..)

import Html
import Json.Decode as D

import Base exposing (present, contrastColor)
import Event exposing (Event)

evolution_events_url : String
evolution_events_url = "resources/human_evolution.json"

decode : D.Decoder (List Event)
decode =
  let
    h2e id fill mya =
      { name = id
      , category = 8
      , start = present - mya * 1e6
      , end = Nothing
      , fill = fill
      , color = contrastColor fill
      , pointCount = 1
      , renderPoint = \_ _ -> Html.text id
      }
    h_dec : D.Decoder Event
    h_dec = D.map3 h2e
      (D.field "id" D.string)
      (D.field "fill" D.string)
      (D.field "mya" D.float)
  in D.list h_dec
