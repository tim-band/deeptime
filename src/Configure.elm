module Configure exposing (..)

import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (id, style, type_, for, checked, value)
import Html.Events exposing (onClick)
import Set
import Svg
import Svg.Attributes as Svga

categoryChecklist :
  (String -> msg)
  -> (String -> msg)
  -> List String
  -> Set.Set String
  -> Html msg
categoryChecklist checkMsg uncheckMsg categories catsChecked =
  let
    checkify : String -> Html msg
    checkify s =
      let
        isChecked = Set.member s catsChecked
      in div [ onClick <| if isChecked then uncheckMsg s else checkMsg s ]
      [ input
        [ checked isChecked
        , type_ "checkbox"
        , id <| "category_switch_" ++ s
        ] []
      , label [ for <| "category_switch_" ++ s ] [text s]
      ]
  in div
    [ style "position" "fixed"
    , style "top" "100px"
    , style "right" "0"
    , style "background" "#80e0f0"
    , style "z-index" "3"
    ] <| List.map checkify categories

settings :
  (String -> msg)
  -> (String -> msg)
  -> List String
  -> Set.Set String
  -> Html msg
settings checkMsg uncheckMsg categories enabled =
  div [] [ cogIcon, categoryChecklist checkMsg uncheckMsg categories enabled ]

cogIcon : Html msg
cogIcon = Svg.svg [ Svga.width "100px"
  , Svga.height "100px"
  , Svga.viewBox "-50 -50 100 100"
  , Svga.version "1.1"
  , style "position" "fixed"
  , style "top" "0px"
  , style "right" "0px"
  , style "z-index" "10"
  ] [ cog 8 33 40 0.5 0.8 12 ]


cog : Float -> Float -> Float -> Float -> Float -> Int -> Svg.Svg msg
cog rAxle rInner rOuter start duty teeth =
  let
    teethf = toFloat teeth
    intToStart : Int -> Float
    intToStart n = (toFloat n + start) / teethf |> Basics.turns
    toothCentres = List.range 0 (teeth - 1) |> List.map intToStart
    -- The tooth cap length and space length should be the same (let's call it s)
    -- Maximum size is where the tooth has parallel sides.
    -- Roughly when the tooth cap length is equal to the tooth base length,
    -- which is when the space length is equal to the tooth base length,
    -- which is when the space length is half the pitch between the spaces.
    -- So duty = 0 is space length = 0, duty = 1 is space length is half the
    -- distance between teeth leading edges on the inner ring.
    baseRadians = ((2 - duty) / 4) / teethf |> Basics.turns -- number of radians for half a tooth base
    spaceRadians = duty / 4 / teethf |> Basics.turns -- number of radians for half a space
    toothRadians = spaceRadians * rInner / rOuter -- number of radians for half a space on the outer ring
    corner radius offset radians = (radius * Basics.cos (radians + offset), radius * Basics.sin (radians + offset))
    toothCorner1 r = corner rInner -baseRadians r
    toothCorner2 r = corner rOuter -toothRadians r
    toothCorner3 r = corner rOuter toothRadians r
    toothCorner4 r = corner rInner baseRadians r
    initial r = let s = String.fromFloat r in  "A " ++ s ++ " " ++ s ++ " 0 0 1 "
    coords (x, y) = String.fromFloat x ++ "," ++ String.fromFloat y ++ " "
    tooth r = initial rInner ++ coords (toothCorner1 r)
      ++ "L " ++ coords (toothCorner2 r)
      ++ initial rOuter ++ coords (toothCorner3 r)
      ++ "L " ++ coords (toothCorner4 r)
    startPoint = 0 |> intToStart |> toothCorner1 |> coords
    teethText = toothCentres |> List.map tooth |> String.concat
  in Svg.path
    [ Svga.d <|  "M " ++ startPoint ++ teethText ++ String.join (String.fromFloat rAxle) ["Z M ", " 0 A ", " ", " 0 1 0 ", " 0.0001"]
    , Svga.stroke "black"
    , Svga.strokeWidth "2"
    , Svga.fill "white"
    ] []
