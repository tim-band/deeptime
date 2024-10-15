module Dinosaurs exposing (Dinosaur, dinosaurs_url, decode, dinosaurEvents)

import Dict
import Html
import Html.Attributes as Attrs
import Json.Decode as D
import Json.Decode.Pipeline as DP

import Base exposing (Time, present)
import Event exposing (Event, setXOffsets)
import Base exposing (contrastColor)

dinosaurs_url : String
dinosaurs_url = "resources/dinosaurs.json"

type alias Dinosaur =
  { name: String
  , mya_start: Time
  , mya_end: Time
  , text: String
  , credit_text: String
  , license_text: String
  , image: String
  , credit_image: String
  , license_image: String
  }

dino_decode : D.Decoder Dinosaur
dino_decode = D.succeed Dinosaur
  |> DP.required "name" D.string
  |> DP.required "mya_start" D.float
  |> DP.required "mya_end" D.float
  |> DP.required "text" D.string
  |> DP.optional "credit_text" D.string ""
  |> DP.required "license_text" D.string
  |> DP.required "image" D.string
  |> DP.optional "credit_image" D.string ""
  |> DP.required "license_image" D.string

license_link : Dict.Dict String String -> String -> Html.Html msg
license_link text2url text =
  let
    style = [Attrs.style "font-size" "small", Attrs.style "padding" "0px 5px"]
  in case Dict.get text text2url of
    Nothing -> Html.span style [Html.text text]
    Just url -> Html.a (Attrs.href url :: style) [Html.text text]

to_interval : Dict.Dict String String -> Dinosaur -> Event
to_interval license_href
  { name
  , mya_start
  , mya_end
  , text
  , credit_text
  , license_text
  , image
  , credit_image
  , license_image
  } =
  { category = "dino"
  , start = present - mya_start * 1e6
  , end = present - mya_end * 1e6 |> Maybe.Just
  , name = name
  , fill = "#80c010"
  , color = "#ffffff"
  , pointCount = 1
  , xOffset = 0
  , renderPoint = \_ _ -> Html.div [] (
    let
      render credit license main =
        let
          license_elt = license_link license_href license
        in if String.isEmpty credit
          then [ main, license_elt ]
          else [ main, Html.span [Attrs.style "font-size" "small"] [Html.text credit, license_elt]]
      rendered_image = render credit_image license_image <| Html.div [] [Html.img
        [ Attrs.src ("resources/dinosaurs/" ++ image)
        , Attrs.width 600
        , Attrs.attribute "max-height" "400px"
        ] []]
      rendered_text = render credit_text license_text <| Html.span [] [Html.text text]
    in
      [ Html.h2 [] [Html.text name]
      , Html.div [] rendered_image
      , Html.div [] rendered_text])
  }

decode : D.Decoder (List Dinosaur)
decode = dino_decode |> D.list

setColours : List Event -> List Event
setColours events =
  let
    colorLoop = ["#aa7f08", "#84aa08", "#aa2e08", "#777529", "#537729", "#774e29", "#517a0f"]
    sc es cl = case es of
      [] -> []
      (e0 :: es0) -> case cl of
        [] -> sc es colorLoop
        (c0 :: cl0) -> {e0 | fill = c0, color = contrastColor c0} :: sc es0 cl0
    in sc events []

dinosaurEvents : List Dinosaur -> Dict.Dict String String -> List Event
dinosaurEvents ds licenses = List.map (to_interval licenses) ds |> setXOffsets |> setColours
