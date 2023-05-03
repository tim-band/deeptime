module Base exposing (..)

import Hex
import Random

type alias Time = Float
type alias TimeDelta = Float

type alias TimeWindow =
  { top : Time
  , bottom : Time
  }

present : Time
present = 14e9

bc1Time : Time
bc1Time = present - 2022

endTime : Time
endTime = present + 2100
randomJoin : Random.Generator (Random.Generator a) -> Random.Generator a
randomJoin rr = rr |> Random.andThen (\r -> r)

randomChar : Random.Generator Char
randomChar = Random.int (Char.toCode 'a') (Char.toCode 'z') |> Random.map Char.fromCode

randomCharList : Random.Generator (List Char)
randomCharList = Random.weighted
  (1, Random.constant [])
  [(9, Random.map2 (::) randomChar (Random.lazy (\_ -> randomCharList)))]
  |> randomJoin

randomCharListMin : Int -> Random.Generator (List Char)
randomCharListMin min = Random.map2 (++) (Random.list min randomChar) randomCharList

randomString : Int -> Random.Generator String
randomString min = Random.map String.fromList (randomCharListMin min)

contrastColor : String -> String
contrastColor s =
  if String.left 1 s == "#"
    then
      let
        n = (String.length s - 1) // 3
        r = Hex.fromString <| String.slice 1 (1 + n) s
        g = Hex.fromString <| String.slice (1 + n) (1 + 2*n) s
        b = Hex.fromString <| String.slice (1 + 2*n) (1 + 3*n) s
        lum ri gi bi = 30 * ri + 59 * gi + 11 * bi
      in case Result.map3 lum r g b of
        Ok v -> if v < 1280 then "black" else "white"
        Err _ -> "red"
    else "black"
