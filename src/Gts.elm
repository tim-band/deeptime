module Gts exposing (..)

import Base exposing (present)
import Csv.Decode as D
import Event exposing (Event)
import Html
import Html.Attributes
import Parser as P exposing ((|.), (|=))
import Svg
import Svg.Attributes as Svga

type alias ZonalRangeAssignment =
  { clade: String  -- ammonoid
  , zonation: String  -- NW Europe
  , zone: String  -- Eumorphoceras bisulcatum Zone
  }

gts_url : String
-- unfortunately .csv makes Elm reactor serve up an HTML table,
-- so we have to use .txt
gts_url = "resources/GTS2020.txt"

maybeP : P.Parser a -> P.Parser (Maybe a)
maybeP p = P.oneOf [ P.succeed Just |= p, P.succeed Nothing ]

maybePOr : P.Parser a -> a -> P.Parser a
maybePOr p default = P.oneOf [ p, P.succeed default ]

decodeZra : D.Decoder ZonalRangeAssignment
decodeZra = D.into ZonalRangeAssignment
  |> D.pipeline (D.field "Zonal range assignment/clade" D.string)
  |> D.pipeline (D.field "Zonal range assignment/zonation" D.string)
  |> D.pipeline (D.field "Zonal range assignment/zone" D.string)

type alias LatLng =
  { representation : String
  , lat : Float
  , lng : Float
  }

deadEndsToString : List P.DeadEnd -> String
deadEndsToString des =
  let
    p2s p = case p of
      P.Expecting s -> String.concat [ "expecting '", s, "'" ]
      P.ExpectingInt -> "expecting int"
      P.ExpectingHex -> "expecting hex"
      P.ExpectingOctal -> "expecting octal"
      P.ExpectingBinary -> "expecting binary"
      P.ExpectingFloat -> "expecting float"
      P.ExpectingNumber -> "expecting number"
      P.ExpectingVariable -> "expecting variable"
      P.ExpectingSymbol sy -> String.concat [ "expecting symbol '", sy, "'" ]
      P.ExpectingKeyword kw -> String.concat [ "expecting keyword '", kw, "'" ]
      P.ExpectingEnd -> "expecting end"
      P.UnexpectedChar -> "unexpected character"
      P.Problem s -> s
      P.BadRepeat -> "bad repeat"
    de2s { col, problem } = String.concat [ "column ", String.fromInt col, ": ", p2s problem ]
  in des |> List.map de2s |> String.join ", "

decodeLatLng : String -> D.Decoder LatLng
decodeLatLng s =
  let
    mkFloat : Int -> (Int, Int) -> Float
    mkFloat whole (num, den) = toFloat whole + toFloat num / toFloat den
    code0 = Char.toCode '0'
    getNumDen : String -> (Int, Int)
    getNumDen str = String.foldl (\c (n,d) -> (n * 10 + Char.toCode c - code0, 10 * d)) (0,1) str
    parseDigits : P.Parser (Int, Int)
    -- TODO: should reject if there are no digits at all
    parseDigits = P.map getNumDen <| P.getChompedString <| P.chompWhile Char.isDigit
    parseInt : P.Parser Int
    parseInt = P.map Tuple.first parseDigits
    float : P.Parser Float
    float = P.succeed mkFloat
      |= parseInt
      |= maybePOr (P.succeed identity |. P.symbol "." |= parseDigits) (0, 1)
    degminsecneg : Float -> Float -> Float -> Float -> Float
    degminsecneg deg min sec n = n * (deg + min / 60 + sec / 3600)
    posneg p n = P.oneOf [ P.succeed 1.0 |. P.symbol p, P.succeed -1.0 |. P.symbol n ]
    valunit : String -> P.Parser Float
    valunit unit = float |. P.token unit
    angle : String -> String -> P.Parser Float
    angle pos neg = P.succeed degminsecneg
      |= valunit "o"
      |= maybePOr (valunit "'") 0.0
      |= maybePOr (valunit "\"") 0.0
      |= posneg pos neg
    parser : P.Parser LatLng
    parser = P.succeed (LatLng s)
      |. maybeP (P.token "~")
      |= angle "N" "S"
      |. P.symbol ","
      |. P.spaces
      |= angle "E" "W"
      |. maybeP (P.token "?")
      |. P.end
  in case P.run parser s of
    Result.Ok v -> D.succeed v
    Result.Err e -> D.fail <| String.concat [ "could not parse '", s, "' as Lat-Lng: ", deadEndsToString e ]

type alias Gts =
  { gts2020id : String  -- Cb18
  , gts2012id : String  -- Cb18
  , sample : String  -- tonstein; Gabriela coal (seam 365)
  , locality : String  -- Julius Fućık Mine, Petrvald, Moravia- Silesia Region, Czech Republic
  , latLng : Maybe LatLng  -- 49o49.166'N, 18o22.691'E
  , lithostratigraphy : String  -- Jaklovec Member, Ostrava Fm
  , time : Float  -- 325.64
  , analytical2s : Float  -- ± 0.13
  , total2s : Float  -- ± 0.4
  , ageType : String  -- 206Pb/238U
  , primaryRadioisotopicAgeDetails : String  -- Five of eight single zircon grain analyses (excluding one older and two younger grains) have a weighted mean 206Pb/238U age
  , zonalRangeAssignment : ZonalRangeAssignment
  , biostratigraphy : String  -- Jaklovec and Poruba Members of the Ostrava Formation assigned to...
  , reference : String  -- Jirasek et al. (2018)
  }

plusMinus : String -> D.Decoder Float
plusMinus pm =
  let
    parser = P.succeed identity
      |. maybeP (P.symbol "±")
      |. P.spaces
      |= P.float
      |. P.end
  in case P.run parser pm of
    Result.Ok v -> D.succeed v
    Result.Err _ -> D.fail <| String.concat ["Could not parse '", pm, "' as +/- float"]

ageMaToTime : String -> D.Decoder Float
ageMaToTime s =
  let
    parser = P.succeed identity
      |. maybeP (P.symbol "≤")
      |. P.spaces
      |= P.float
      |. P.end
  in case P.run parser s of
    Result.Ok v -> D.succeed <| present - v * 1e6
    Result.Err _ -> D.fail <| String.concat ["Could not parse '", s, "' as an age"]

decode : D.Decoder Gts
decode = D.into Gts
  |> D.pipeline (D.field "GTS 2020 ID" D.string)
  |> D.pipeline (D.field "GTS 2012 ID" D.string)
  |> D.pipeline (D.field "Sample" D.string)
  |> D.pipeline (D.field "Locality" D.string)
  |> D.pipeline (D.string |> D.andThen decodeLatLng |> D.blank |> D.field "Lat-Long")
  |> D.pipeline (D.field "Lithostratigraphy" D.string)
  |> D.pipeline (D.field "Age (Ma)" D.string |> D.andThen ageMaToTime)
  |> D.pipeline (D.field "± 2s analytical" D.string |> D.andThen plusMinus)
  |> D.pipeline (D.field "± 2s total" D.string |> D.andThen plusMinus)
  |> D.pipeline (D.field "Age Type" D.string)
  |> D.pipeline (D.field "Primary radioisotopic age details" D.string)
  |> D.pipeline decodeZra
  |> D.pipeline (D.field "Biostratigraphy" D.string)
  |> D.pipeline (D.field "Reference" D.string)

htmlTd : String -> Html.Html ()
htmlTd str = Html.td [] [ Html.text str ]

htmlTr2 : String -> String -> Html.Html ()
htmlTr2 k v = Html.tr [] [ htmlTd k, htmlTd v ]

latLngText : Maybe LatLng -> String
latLngText mll = case mll of
  Nothing -> "-"
  Just ll -> ll.representation

gtsTable : Gts -> Html.Html ()
gtsTable g = Html.table [] [ Html.tbody
  [ Html.Attributes.style "font-size" "small" ]
  [ htmlTr2 "GTS 2020 ID" g.gts2020id
  , htmlTr2 "GTS 2012 ID" g.gts2012id
  , htmlTr2 "Sample" g.sample
  , htmlTr2 "Locality" g.locality
  , htmlTr2 "Lat-Lng" (latLngText g.latLng)
  , htmlTr2 "Lithostratigraphy" g.lithostratigraphy
  , g.analytical2s |> String.fromFloat |> htmlTr2 "2s analytical"
  , g.total2s |> String.fromFloat |> htmlTr2 "2s total"
  , htmlTr2 "Age Type" g.ageType
  , htmlTr2 "Primary Radioisotopic age details" g.primaryRadioisotopicAgeDetails
  , htmlTr2 "Zonal Range Assignment: Clade" g.zonalRangeAssignment.clade
  , htmlTr2 "ZRA: zonation" g.zonalRangeAssignment.zonation
  , htmlTr2 "ZRA: zone" g.zonalRangeAssignment.zone
  , htmlTr2 "Biostratigraphy" g.biostratigraphy
  , htmlTr2 "Reference" g.reference
  ]]

gtsMap : Float -> Maybe LatLng -> Html.Html a
gtsMap width latlng =
  let
    mapAttrs = if width < 1000
      then { width=300, height=150, dir="300x150", r=10.0, stroke=1.5 }
      else if width < 1600
      then { width=500, height=250, dir="500x250", r=18.0, stroke=2 }
      else { width=800, height=400, dir="800x400", r=25.0, stroke=2 }
    radius = String.fromFloat mapAttrs.r
    diameter = mapAttrs.r * 2 |> String.fromFloat
    strokeWidth = String.fromFloat mapAttrs.stroke
  in case latlng of
    Nothing -> Html.div [] []
    Just { lat, lng } -> Html.div
      [ Html.Attributes.style "position" "relative"
      ]
      [ Html.div
        [ Html.Attributes.style "transform" ("translate("
          ++ String.fromFloat ((180 + lng) * mapAttrs.width / 360 - mapAttrs.r)
          ++ "px,"
          ++ String.fromFloat ((90 - lat) * mapAttrs.height / 180 - mapAttrs.r)
          ++ "px)")
        , Html.Attributes.style "position" "absolute"
        ]
        [ Svg.svg
          [ Svga.width diameter
          , Svga.height diameter
          , [ "0 0 ", diameter, " ", diameter ] |> String.concat |> Svga.viewBox
          , Svga.strokeWidth strokeWidth
          , Svga.fillOpacity "0"
          ]
          [ Svg.circle
            [ Svga.cx radius
            , Svga.cy radius
            , mapAttrs.r - mapAttrs.stroke / 2
              |> String.fromFloat
              |> Svga.r
            , Svga.stroke "white"
            ] []
          , Svg.circle
            [ Svga.cx radius
            , Svga.cy radius
            , mapAttrs.r - mapAttrs.stroke * 3 / 2
              |> String.fromFloat
              |> Svga.r
            , Svga.stroke "red"
            ] []
          ]
        ]
      , Html.img
        [ [ "resources/geography/rectangular/"
          , mapAttrs.dir
          , "/image_0.jpg"
          ]
          |> String.concat
          |> Html.Attributes.src
        ] []
      ]

gtsToEvent : Gts -> Event
gtsToEvent gts =
  { category = 9
  , start = gts.time
  , end = Nothing
  , name = gts.sample
  , fill = "#403020"
  , color = "white"
  , pointCount = 1
  , xOffset = 0
  , renderPoint = \w _ -> Html.div
    [] [gtsMap w gts.latLng, gtsTable gts]
  }

decodeToEvents : D.Decoder Event.Event
decodeToEvents = D.map gtsToEvent decode
