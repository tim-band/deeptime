module Human exposing (evolution_events_url, decode)

import Html
import Json.Decode as D

import Base exposing (present, contrastColor)
import Event exposing (Event)
import Html.Attributes

evolution_events_url : String
evolution_events_url = "resources/human_evolution.json"

decode : D.Decoder (List Event)
decode =
  let
    h2e id fill mya picture =
      { name = id
      , category = 8
      , start = present - mya * 1e6
      , end = Nothing
      , fill = fill
      , color = contrastColor fill
      , pointCount = 1
      , renderPoint = case picture of
          Nothing -> \_ _ -> Html.text id
          Just pic -> \_ _ -> Html.div []
            [ Html.img [Html.Attributes.src pic] []
            , Html.div [] [Html.text id]
            ]
      }
    h_dec : D.Decoder Event
    h_dec = D.map4 h2e
      (D.field "id" D.string)
      (D.field "fill" D.string)
      (D.field "mya" D.float)
      (D.maybe (D.field "picture" D.string))
  in D.list h_dec
