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

contrastColor : String -> String
contrastColor s =
  if String.left 1 s == "#"
    then
      let
        n = (String.length s - 1) // 3
        max = 16 ^ n - 1
        r = Hex.fromString <| String.slice 1 (1 + n) s
        g = Hex.fromString <| String.slice (1 + n) (1 + 2*n) s
        b = Hex.fromString <| String.slice (1 + 2*n) (1 + 3*n) s
        lum ri gi bi = 30 * ri + 59 * gi + 11 * bi
      in case Result.map3 lum r g b of
        Ok v -> if v < (max * 50) then "white" else "black"
        Err _ -> "red"
    else "black"
