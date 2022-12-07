module Event exposing (..)

import Random

import Base exposing (Time, TimeDelta, TimeWindow, randomString, present)

type alias Event =
  { category : Int
  , time : Time
  , name : String
  }

type alias ScreenEvent =
  { y: Float
  , ev: Event
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
  { category = cat, time = t, name = name })))

forwardEvents : List Event -> List Event
forwardEvents = List.sortBy (\e -> -e.time)

reverseEvents : List Event -> List Event
reverseEvents = List.sortBy .time

yEventPosition : Time -> TimeDelta -> Event -> Time
yEventPosition top height ev = (top - ev.time) / height

screenify : Time -> TimeDelta -> Event -> ScreenEvent
screenify top height ev = { y = yEventPosition top height ev, ev = ev }

findScreenEvents : TimeWindow -> List Event -> Events
findScreenEvents window evs =
  let
    { top, bottom } = window
    height = top - bottom
    tooEarly { time } = time < bottom
    tooLate { time } = top < time
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
    tooEarly { time } = time < bottom
    tooLate { time } = top < time
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

