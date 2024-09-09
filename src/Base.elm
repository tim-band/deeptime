module Base exposing (..)

import Hex

type alias Time = Float
type alias TimeDelta = Float

type alias TimeWindow =
  { top : Time
  , bottom : Time
  }

present : Time
present = 14e9

latestYear : number
latestYear = 2030

bc1Time : Time
bc1Time = present - 2022

earlyUniverseTime : Time
earlyUniverseTime = 1e9

endTime : Time
endTime = present + 2100

-- takes a string like "#04ffa3" and
-- returns Ok ((red, green, blue), max)
-- max is 15 for #rgb and 255 for #rrggbb
-- red, green and blue are between 0 and max inclusive
-- returns Err if the format is not #rgb or #rrggbb
colorToRgb : String -> Result String ((Int, Int, Int), Int)
colorToRgb s =
  if String.left 1 s == "#"
    then
      let
        sl = String.toLower s
        n = (String.length sl - 1) // 3
        max = 16 ^ n - 1
        r = Hex.fromString <| String.slice 1 (1 + n) sl
        g = Hex.fromString <| String.slice (1 + n) (1 + 2*n) sl
        b = Hex.fromString <| String.slice (1 + 2*n) (1 + 3*n) sl
        triple x y z = (x, y, z)
      in (Result.map3 triple r g b |> Result.andThen (\rgb -> Ok (rgb, max)))
    else Err "Not a #colour"

mapDarkLight : String -> String -> String -> Result String String
mapDarkLight dark light s = colorToRgb s |> Result.map (\((r, b, g), max) ->
  let
    lum = 30 * r + 59 * g + 11 * b
  in if lum < (max * 50) then dark else light)

contrastColor : String -> String
contrastColor s = mapDarkLight "white" "black" s |> Result.withDefault "red"

anticontrastColor : String -> String
anticontrastColor s = mapDarkLight "black" "white" s |> Result.withDefault "red"
