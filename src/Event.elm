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
  -- Render the focused event (will be put in the right hand pane)
  -- viewport width, and arguments are point index focused
  , renderPoint : Float -> Int -> Html.Html ()
  }

same : Event -> Event -> Bool
same a b = a.category == b.category && a.name == b.name  -- is this enough?

type alias ScreenEvent =
  { top : Float
  , bottom : Float
  , unclampedMiddle : Float
  , ev : Event
  }

type alias ZoomHints =
  { previouss : List (Time, TimeDelta)  -- reverse time order
  , previous : (Time, TimeDelta)
  , next : (Time, TimeDelta)
  , nexts : List (Time, TimeDelta)  -- forward time order
  }

type alias Events =
  { previousEvents : List Event  -- in reverse time order
  , visibleEvents : List ScreenEvent
  , nextEvents : List Event -- in forward order
  , zoomHints : List ZoomHints
  }

singleZoomHintHeight : Time -> ZoomHints -> TimeDelta
singleZoomHintHeight t zh =
  let
    (t0, d0) = zh.previous
    (t1, d1) = zh.next
    td = t1 - t0
    tp = t - t0
    dd = d1 - d0
  in if td < 1 then d0 else d0 + dd * tp / td

zoomHintHeight : Time -> List ZoomHints -> TimeDelta
zoomHintHeight t zhs = zhs
  |> List.map (singleZoomHintHeight t)
  |> List.minimum
  |> Maybe.withDefault 1e3
  |> Basics.max 1e3

pointIndex : Float -> Event -> Int
pointIndex t { start, end, pointCount } = case end of
  Nothing -> 0
  Just jend ->
    let
      n = (toFloat pointCount) * (t - start) / (jend - start) |> Basics.round
    in Basics.clamp 0 (pointCount - 1) n

eventPointTimes : Event -> List Time
eventPointTimes { start, end, pointCount } = case end of
  Nothing -> [ start ]
  Just jend -> if pointCount < 2
    then [ (jend + start) / 2 ]
    else
      let
        c = (jend - start) / toFloat (pointCount - 1)
        mult n = c * toFloat n
      in List.range 0 pointCount |> List.map mult

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
    , zoomHints = List.append evs0.zoomHints evs1.zoomHints
    }

unzipList : (a -> Bool) -> List a -> (List a, List a)
unzipList p xs =
  let
    uzl ps ns =
      case ns of
        [] -> (ps, [])
        n :: ns0 ->
          if p n then uzl (n :: ps) ns0 else (ps, ns)
  in uzl [] xs

removeDuplicates : List comparable -> List comparable
removeDuplicates sorted_list =
  let
    rd x xs = case xs of
      [] -> [x]
      x0 :: x1s -> if x == x0
        then rd x x1s
        else x :: rd x0 x1s
  in case sorted_list of
    [] -> []
    h :: ts -> rd h ts

findScreenEvents : TimeWindow -> List Event -> Events
findScreenEvents window evs =
  let
    minimalEventCount = 10
    { top, bottom } = window
    height = top - bottom
    mid = bottom + height / 2
    zoomSizeTooEarly (t, _) = t < mid
    bookend : List Time -> List Time
    bookend ts = 0 :: present :: ts
    zoomSizes = evs |> List.concatMap eventPointTimes |> bookend |> List.sort |> removeDuplicates |> idealZoomSizes minimalEventCount
    (zoomSizesPrev, zoomSizesNext) = unzipList zoomSizeTooEarly zoomSizes
    ((zp, zps), (zn, zns)) = case zoomSizesNext of
      [] -> case zoomSizesPrev of
        [] -> (((present, present), []), ((present, present), []))
        [z] -> ((z, []), (z, []))
        z0 :: z1 :: zs -> ((z1, zs), (z0, []))
      [z] -> case zoomSizesPrev of
        [] -> ((z, []), (z, []))
        z0 :: zs -> ((z0, zs), (z, []))
      z0 :: z1 :: zs -> case zoomSizesPrev of
        [] -> ((z0, []), (z1, zs))
        zp0 :: zp0s -> ((zp0, zp0s), (z0, z1 :: zs))
    tooEarly ev = eventEnd ev < bottom
    tooLate { start } = top < start
    justRight t = not (tooEarly t || tooLate t)
    visibles = evs |> List.filter justRight |> List.map (screenify top height)
    zoomHints =
      { previouss = zps
      , previous = zp
      , next = zn
      , nexts = zns
      }
  in
    { previousEvents = List.filter tooEarly evs |> forwardEvents
    , visibleEvents = visibles
    , nextEvents = List.filter tooLate evs |> reverseEvents
    , zoomHints = [ zoomHints ]
    }

zoomSizesGoBack : Time -> ZoomHints -> ZoomHints
zoomSizesGoBack t zh =
  case zh.previouss of
      [] -> zh
      (pz :: pzs) -> if Tuple.first zh.previous <= t then zh else zoomSizesGoBack t
        { zh
        | previouss = pzs
        , previous = pz
        , next = zh.previous
        , nexts = zh.next :: zh.nexts
        }

zoomSizesGoForward : Time -> ZoomHints -> ZoomHints
zoomSizesGoForward t zh =
  case zh.nexts of
      [] -> zh
      (nz :: nzs) -> if t <= Tuple.first zh.next then zh else zoomSizesGoForward t
        { zh
        | previouss = zh.previous :: zh.previouss
        , previous = zh.next
        , next = nz
        , nexts = nzs
        }

moveZoomHintsTo : Time -> Events -> Events
moveZoomHintsTo t evs =
  let
    mzh zh = zh |> zoomSizesGoForward t |> zoomSizesGoBack t
  in
    { evs
    | zoomHints = List.map mzh evs.zoomHints
    }

adjustScreenEvents : TimeWindow -> Events -> Events
adjustScreenEvents { top, bottom } evs =
  let
    height = top - bottom
    mid = bottom + height / 2
    zsCorrect = moveZoomHintsTo mid evs
    { previousEvents, visibleEvents, nextEvents } = zsCorrect
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
  in
    { zsCorrect
    | previousEvents = earlies ++ stillPrevs
    , visibleEvents = List.map (screenify top height) visible
    , nextEvents = lates ++ stillNexts
    }

-- evs is a list of times of events from earliest to latest
idealZoomSizes : Int -> List Time -> List (Time, TimeDelta)
idealZoomSizes idealEventCount evs =
  let
    ahead = List.drop idealEventCount evs
    dif t0 t1 = let d = t1 - t0 in (t0 + d/2, d)
  in List.map2 dif evs ahead
