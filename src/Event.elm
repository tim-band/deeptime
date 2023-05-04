module Event exposing (..)

import Base exposing (Time, TimeDelta, TimeWindow, present)
import Html

type alias Event =
  { category : Int
  , start : Time
  , end : Maybe Time
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
pointIndex t { start, end, pointCount } = case end of
  Nothing -> 0
  Just jend ->
    let
      n = (toFloat pointCount) * (t - start) / (jend - start) |> Basics.round
    in Basics.clamp 0 (pointCount - 1) n

eventEnd : Event -> Time
eventEnd { end, start } = case end of
  Nothing -> start
  Just j -> j

forwardEvents : List Event -> List Event
forwardEvents = List.sortBy (\e -> -e.start - eventEnd e)

reverseEvents : List Event -> List Event
reverseEvents = List.sortBy .start

yEventTop : Time -> TimeDelta -> Event -> Time
yEventTop top height ev = max 0 <| (top - eventEnd ev) / height

yEventBottom : Time -> TimeDelta -> Event -> Time
yEventBottom top height ev = min 1 <| (top - ev.start) / height

yEventMiddle : Time -> TimeDelta -> Event -> Time
yEventMiddle top height ev = (top - (ev.start + eventEnd ev) / 2) / height

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
    earlier e1 e2 = e2.start + eventEnd e2 <= e1.start + eventEnd e1
    later e1 e2 = e1.start + eventEnd e1 <= e2.start + eventEnd e2
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
    tooEarly ev = eventEnd ev < bottom
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
    tooEarly ev = eventEnd ev < bottom
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
