module Base exposing (..)

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
