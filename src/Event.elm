module Event exposing (..)

import Random

import Base exposing (Time, TimeDelta, TimeWindow, randomString, present)

type alias Event =
  { category : Int
  , start : Time
  , end : Time
  , name : String
  , fill : String
  }

type alias ScreenEvent =
  { top : Float
  , bottom : Float
  , unclampedMiddle : Float
  , ev : Event
  }

type alias Events =
  { previousEvents : List Event  -- in reverse time order
  , visibleEvents : List ScreenEvent
  , nextEvents : List Event -- in forward order
  }

randomEvent : Time -> TimeDelta -> Random.Generator Event
randomEvent max size = Random.int 0 4 |> Random.andThen (\cat ->
  randomString 3 |> Random.andThen (\name ->
  Random.float (max - size) max |> Random.map (\t ->
  { category = cat, end = t, start = t - 0.1, name = name, fill = "#4499EE" })))

forwardEvents : List Event -> List Event
forwardEvents = List.sortBy (\e -> -e.start - e.end)

reverseEvents : List Event -> List Event
reverseEvents = List.sortBy .start

yEventTop : Time -> TimeDelta -> Event -> Time
yEventTop top height ev = max 0 <| (top - ev.end) / height

yEventBottom : Time -> TimeDelta -> Event -> Time
yEventBottom top height ev = min 1 <| (top - ev.start) / height

yEventMiddle : Time -> TimeDelta -> Event -> Time
yEventMiddle top height ev = (top - (ev.start + ev.end) / 2) / height

screenify : Time -> TimeDelta -> Event -> ScreenEvent
screenify top height ev =
  { top = yEventTop top height ev
  , bottom = yEventBottom top height ev
  , unclampedMiddle = yEventMiddle top height ev
  , ev = ev
  }

findScreenEvents : TimeWindow -> List Event -> Events
findScreenEvents window evs =
  let
    { top, bottom } = window
    height = top - bottom
    tooEarly { end } = end < bottom
    tooLate { start } = top < start
    justRight t = not (tooEarly t || tooLate t)
    visibles = evs |> List.filter justRight |> List.map (screenify top height)
  in
    { previousEvents = List.filter tooEarly evs |> forwardEvents
    , visibleEvents = visibles
    , nextEvents = List.filter tooLate evs |> reverseEvents
    }

adjustScreenEvents : TimeWindow -> Events -> Events
adjustScreenEvents { top, bottom } { previousEvents, visibleEvents, nextEvents } =
  let
    tooEarly { end } = end < bottom
    tooLate { start } = top < start
    (earlies1, notEarlies) = List.partition (.ev >> tooEarly) visibleEvents
    earlies = List.map .ev earlies1 |> forwardEvents
    (lates1, notLates) = List.partition (.ev >> tooLate) notEarlies
    stillVisible = List.map .ev notLates
    lates = List.map .ev lates1 |> reverseEvents
    (stillPrevs, nowIn1) = List.partition tooEarly previousEvents
    (stillNexts, nowIn2) = List.partition tooLate nextEvents
    visible = nowIn1 ++ nowIn2 ++ stillVisible
    height = top - bottom
  in
    { previousEvents = earlies ++ stillPrevs
    , visibleEvents = List.map (screenify top height) visible
    , nextEvents = lates ++ stillNexts
    }

randomEvents : Random.Generator (List Event)
randomEvents =
  Random.list 999 (randomEvent present 1) |> Random.andThen (\es0 ->
  Random.list 999 (randomEvent present 100) |> Random.andThen (\es1 ->
  Random.list 999 (randomEvent present 1000) |> Random.andThen (\es2 ->
  Random.list 999 (randomEvent present 10000) |> Random.andThen (\es3 ->
  Random.list 999 (randomEvent present 100000) |> Random.andThen (\es4 ->
  Random.constant (es0 ++ es1 ++ es2 ++ es3 ++ es4))))))

