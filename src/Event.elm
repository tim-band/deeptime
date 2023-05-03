module Event exposing (..)

import Random

import Base exposing (Time, TimeDelta, TimeWindow, randomString, present)
import Html

type alias Event =
  { category : Int
  , start : Time
  , end : Time
  , name : String
  , fill : String
  , color : String
  , pointCount : Int
  , renderPoint : Int -> Html.Html ()
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

pointIndex : Float -> Event -> Int
pointIndex t { start, end, pointCount } =
  let
    n = (toFloat pointCount) * (t - start) / (end - start) |> Basics.round
  in Basics.clamp 0 (pointCount - 1) n

randomEvent : Time -> TimeDelta -> Random.Generator Event
randomEvent max size = Random.int 0 4 |> Random.andThen (\cat ->
  randomString 3 |> Random.andThen (\name ->
  Random.float (max - size) max |> Random.map (\t ->
  { category = cat
  , end = t
  , start = t - 0.1
  , name = name
  , fill = "#4499EE"
  , color = "white"
  , pointCount = 1
  , renderPoint = \_ -> Html.text name
  })))

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

-- xs and ys should be ordered so that if x0 is an earlier
-- element and x1 is a later element of the same list then
-- f x0 x1 is true. Then this will also hold for the result
mergeWith : (a -> a -> Bool) -> List a -> List a -> List a
mergeWith f xs ys = case xs of
  ( x :: xs0 ) -> case ys of
    ( y :: ys0 ) -> if f x y
      then x :: mergeWith f xs0 ys
      else y :: mergeWith f xs ys0
    [] -> xs
  [] -> ys

mergeScreenEvents : Events -> Events -> Events
mergeScreenEvents evs0 evs1 =
  let
    earlier e1 e2 = e2.start + e2.end <= e1.start + e1.end
    later e1 e2 = e1.start + e1.end <= e2.start + e2.end
  in
    { previousEvents = mergeWith later evs0.previousEvents evs1.previousEvents
    , nextEvents = mergeWith earlier evs0.nextEvents evs1.nextEvents
    , visibleEvents = List.append evs0.visibleEvents evs1.visibleEvents
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
