module Human exposing (evolution_events_url, decode)

import Html
import Json.Decode as D

import Base exposing (present, contrastColor)
import Event exposing (Event)
import Html.Attributes

evolution_events_url : String
evolution_events_url = "resources/human_evolution.json"

maybeToList : Maybe x -> List x
maybeToList x = case x of
    Nothing -> []
    Just v -> [v]

decode : D.Decoder (List Event)
decode =
  let
    h2e id fill mya picture text =
      { name = id
      , category = "human"
      , start = present - mya * 1e6
      , end = Nothing
      , fill = fill
      , color = contrastColor fill
      , pointCount = 1
      , xOffset = 0
      , renderPoint = case picture of
          Nothing -> \_ _ -> Html.text id
          Just pic -> \_ _ -> Html.div [] (
            [ Html.h2 [] [Html.text id]
            , Html.img [Html.Attributes.src pic, Html.Attributes.style "float" "inline-start"] []
            ] ++ (text |> Maybe.map Html.text |> maybeToList))
      }
    h_dec : D.Decoder Event
    h_dec = D.map5 h2e
      (D.field "id" D.string)
      (D.field "fill" D.string)
      (D.field "mya" D.float)
      (D.field "picture" D.string |> D.maybe)
      (D.field "text" D.string |> D.maybe)
  in D.list h_dec
