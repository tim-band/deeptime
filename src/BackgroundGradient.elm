module BackgroundGradient exposing (background_gradient_url, decode, ColourStop)
import Base exposing (colorToRgb)

import Json.Decode as D

type alias ColourStop =
    { t : Float
    , r : Float
    , g : Float
    , b : Float
    }

background_gradient_url : String
background_gradient_url = "resources/background_gradient.json"

decode : D.Decoder (List ColourStop)
decode =
  let
    divide : Int -> Int -> Float
    divide n d =  toFloat n / toFloat d
    colStop t colour = case colorToRgb colour of
      Err err -> D.fail err
      Ok ((r, g, b), max) ->
        D.succeed { t = t, r = divide r max, g = divide g max, b = divide b max }
    bgg_dec : D.Decoder ColourStop
    bgg_dec = D.field "time" D.float |> D.andThen (\time ->
      D.field "colour" D.string |> D.andThen (\colour ->
      colStop time colour))
  in D.list bgg_dec
