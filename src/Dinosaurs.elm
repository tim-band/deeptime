module Dinosaurs exposing (Dinosaur, dinosaurs_url, decode, dinosaurEvents)

import Dict
import Html
import Html.Attributes as Attrs
import Json.Decode as D
import Json.Decode.Pipeline as DP

import Base exposing (Time, present)
import Event exposing (Event, setXOffsets)

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
license_link text2url text = case Dict.get text text2url of
    Nothing -> Html.span [Attrs.class "license"] [Html.text text]
    Just url -> Html.a [Attrs.href url, Attrs.class "license"] [Html.text text]

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
  { category = 9
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
          else [ main, Html.p [Attrs.class "credit"] [Html.text credit, license_elt]]
      rendered_image = render credit_image license_image <| Html.img
        [ Attrs.src ("resources/dinosaurs/" ++ image)
        , Attrs.width 600
        , Attrs.attribute "max-height" "400px"
        ] []
      rendered_text = render credit_text license_text <| Html.p [] [Html.text text]
    in Html.h2 [] [Html.text name] :: rendered_image ++ rendered_text)
  }

decode : D.Decoder (List Dinosaur)
decode = dino_decode |> D.list

dinosaurEvents : List Dinosaur -> Dict.Dict String String -> List Event
dinosaurEvents ds licenses = List.map (to_interval licenses) ds |> setXOffsets
