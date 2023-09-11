module Gts exposing (..)

import Base exposing (present)
import Csv.Decode as D
import Event exposing (Event)
import Html
import Parser as P exposing ((|.), (|=))

type alias ZonalRangeAssignment =
  { clade: String  -- ammonoid
  , zonation: String  -- NW Europe
  , zone: String  -- Eumorphoceras bisulcatum Zone
  }

gts_url : String
-- unfortunately .csv makes Elm reactor serve up an HTML table,
-- so we have to use .txt
gts_url = "resources/GTS2020.txt"

decodeZra : D.Decoder ZonalRangeAssignment
decodeZra = D.into ZonalRangeAssignment
  |> D.pipeline (D.field "Zonal range assignment/clade" D.string)
  |> D.pipeline (D.field "Zonal range assignment/zonation" D.string)
  |> D.pipeline (D.field "Zonal range assignment/zone" D.string)

type alias Gts =
  { gts2020id : String  -- Cb18
  , gts2012id : String  -- Cb18
  , sample : String  -- tonstein; Gabriela coal (seam 365)
  , locality : String  -- Julius Fućık Mine, Petrvald, Moravia- Silesia Region, Czech Republic
  , latLng : String  -- 49o49.166'N, 18o22.691'E
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
      |. P.oneOf [ P.symbol "±", P.succeed () ]
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
      |. P.oneOf [ P.symbol "≤", P.succeed () ]
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
  |> D.pipeline (D.field "Lat-Long" D.string)
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

gtsTable : Gts -> Html.Html ()
gtsTable g = Html.table [] [ Html.tbody []
  [ htmlTr2 "GTS 2020 ID" g.gts2020id
  , htmlTr2 "GTS 2012 ID" g.gts2012id
  , htmlTr2 "Sample" g.sample
  , htmlTr2 "Locality" g.locality
  , htmlTr2 "Lat-Lng" g.latLng
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

gtsToEvent : Gts -> Event
gtsToEvent gts =
  { category = 9
  , start = gts.time
  , end = Nothing
  , name = gts.sample
  , fill = "#403020"
  , color = "white"
  , pointCount = 1
  , renderPoint = \_ -> gtsTable gts
  }

decodeToEvents : D.Decoder Event.Event
decodeToEvents = D.map gtsToEvent decode
