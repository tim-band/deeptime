module Event exposing (..)

import Base exposing (Time, TimeDelta, TimeWindow, present)
import Dict
import Html
import Heap
import Set

type alias Event =
  { category : String
  , start : Time
  , end : Maybe Time
  , name : String
  , fill : String
  , color : String
  , pointCount : Int
  -- offset this event towards the next lane
  -- (between 0 and 1, where 1 is actually in the next lane)
  , xOffset : Float
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

-- All of these (Time, TimeDelta) pairs come from Events run
-- through idealZoomSizes.
-- Each hint is a time and a delta. This indicates that the
-- ideal height of the screen is the delta if the screen is
-- centred on the time.
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
  , zoomHints : Dict.Dict String ZoomHints -- where the nearest events are for each category
  }

minimumWindowSize : Float
minimumWindowSize = 1e-38

singleZoomHintHeight : Time -> ZoomHints -> TimeDelta
singleZoomHintHeight t zh =
  let
    -- d0 and d1 are the "ideal" screen sizes when looking at the
    -- previous or next events.
    (t0, d0) = zh.previous
    (t1, d1) = zh.next
    td = t1 - t0 -- gap between the previous and next events
    tp = t - t0 -- position of the gap relative to t (the middle of the screen)
    dd = d1 - d0
    -- lerp between d0 an d1 depending on how close we are to each
  in d0 + dd * tp / td

-- Get the ideal zoom from the zoom hints in the appropriate categories
zoomHintHeight : Time -> Set.Set String -> Dict.Dict String ZoomHints -> TimeDelta
zoomHintHeight t categories zhs = zhs
  |> Dict.filter (\k _ -> Set.member k categories)
  |> Dict.values
  |> List.map (singleZoomHintHeight t)
  |> List.minimum
  |> Maybe.withDefault minimumWindowSize
  |> Basics.max minimumWindowSize

pointIndex : Float -> Event -> Int
pointIndex t { start, end, pointCount } = case end of
  Nothing -> 0
  Just jend ->
    let
      n = (toFloat pointCount) * (t - start) / (jend - start) |> Basics.round
    in Basics.clamp 0 (pointCount - 1) n

-- get the event start, end and point times
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

-- Merge events; zoomHints categories must not overlap
mergeScreenEvents : Events -> Events -> Events
mergeScreenEvents evs0 evs1 =
  let
    earlier e1 e2 = e2.start + eventEnd e2 <= e1.start + eventEnd e1
    later e1 e2 = e1.start + eventEnd e1 <= e2.start + eventEnd e2
  in
    { previousEvents = mergeWith later evs0.previousEvents evs1.previousEvents
    , nextEvents = mergeWith earlier evs0.nextEvents evs1.nextEvents
    , visibleEvents = List.append evs0.visibleEvents evs1.visibleEvents
    , zoomHints = Dict.union evs0.zoomHints evs1.zoomHints
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
findScreenEvents = findScreenEventsCustom 10 0.07

findScreenEventsCustom : Int -> Float -> TimeWindow -> List Event -> Events
findScreenEventsCustom minimalEventCount minimumEventSeparation window evs =
  let
    { top, bottom } = window
    height = top - bottom
    mid = bottom + height / 2
    bookend : List Time -> List Time
    bookend ts = 0 :: present :: ts
    zoomSizes = evs
      |> List.concatMap eventPointTimes
      |> bookend
      |> List.sort
      |> removeDuplicates
      |> idealZoomSizes minimalEventCount minimumEventSeparation
    (zp, zn, zns) = case zoomSizes of
      [] -> ((0, minimumWindowSize), (0, minimumWindowSize), [])
      [z] -> ((0, minimumWindowSize), z, [])
      y :: z :: zs -> (y, z, zs)
    tooEarly ev = eventEnd ev < bottom
    tooLate { start } = top < start
    justRight t = not (tooEarly t || tooLate t)
    visibles = evs |> List.filter justRight |> List.map (screenify top height)
    zoomHints =
      { previouss = []
      , previous = zp
      , next = zn
      , nexts = zns
      }
  in moveZoomHintsTo mid
    { previousEvents = List.filter tooEarly evs |> forwardEvents
    , visibleEvents = visibles
    , nextEvents = List.filter tooLate evs |> reverseEvents
    , zoomHints = case List.head evs of
        Nothing -> Dict.empty
        Just ev -> Dict.singleton ev.category zoomHints
    }

-- move ZoomHints back to t, if possible
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

-- move ZoomHints forward to t, if possible
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

-- change ZoomHints to be centred on time t
moveZoomHintsTo : Time -> Events -> Events
moveZoomHintsTo t evs =
  let
    mzh _ zh = zh |> zoomSizesGoForward t |> zoomSizesGoBack t
  in
    { evs
    | zoomHints = Dict.map mzh evs.zoomHints
    }

-- Adjust Events to fit the supplied TimeWindow.
-- As Events knows which events are visible and which are
-- too early or too late, it needs to be adjusted to the
-- current TimeWindow every time it changes.
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

-- Make a list of zoom hints.
-- evs is a list of times of events from earliest to latest
idealZoomSizes : Int -> Float -> List Time -> List (Time, TimeDelta)
idealZoomSizes idealEventCount minEventSeparation evs =
  let
    ahead1 = List.drop 1 evs
    ahead = List.drop idealEventCount evs
    -- current = current event, next = next event, away = idealEventCount'th next event
    makeTimeHint current next away =
      let
        away_delta = away - current
        limit = (next - current) / minEventSeparation -- make sure we can distinguish events that are close together
        delta = Basics.min away_delta limit
      in (current + (delta / 2), delta)
  in List.map3 makeTimeHint evs ahead1 ahead

-- Take a list of events in one category, and set their xOffsets so that
-- none of them overlap.
setXOffsets : List Event -> List Event
setXOffsets events =
  let
    eventsSorted = List.sortBy (\e -> e.start) events
    end e = Maybe.withDefault e.start e.end
    -- set the X offset of the head of es (if it exists), then continue with the rest of es
    doXOffsets : Float -> Heap.Heap Float -> Heap.Heap Event -> List Event -> List Event
    doXOffsets next unoccupied currentEvents es = case es of
      -- there are no new events, so just output current events
      [] -> Heap.toList currentEvents
      e0 :: es0 -> doXOffsetsHT next unoccupied currentEvents e0 es0
    -- set the X offset of e0, then continue with the events in es0
    -- after outputting all the events that finish before e0
    doXOffsetsHT : Float -> Heap.Heap Float -> Heap.Heap Event -> Event -> List Event -> List Event
    doXOffsetsHT next unoccupied currentEvents e0 es0 = case Heap.pop currentEvents of
      -- There are no current events to end, so the next edge is the start of the next event
      Nothing -> setNextXOffset next unoccupied currentEvents e0 es0
      -- There are current events, so does the next event to end end before the next
      -- event to start starts?
      Just (ce, ces) -> if end ce < e0.start
        -- current event ending next
        then ce :: doXOffsetsHT next (Heap.push ce.xOffset unoccupied) ces e0 es0
        -- New event starting next
        else setNextXOffset next unoccupied currentEvents e0 es0
    -- set the X offset of e as whatever the next x offset should be
    setNextXOffset next unoccupied currentEvents e es = case Heap.pop unoccupied of
      -- there are no unoccupied offsets, so we'll make a new one
      Nothing -> doXOffsets (next + 1) unoccupied (Heap.push {e | xOffset = next} currentEvents) es
      -- there is an unoccupied offset, so we'll use that
      Just (occ, occs) -> doXOffsets next occs (Heap.push {e | xOffset = occ} currentEvents) es
    initEventHeap = Heap.empty (Heap.smallest |> Heap.by end)
    -- events with their xOffsets set to integer values
    events1 = doXOffsets 0 (Heap.empty Heap.smallest) initEventHeap eventsSorted
    -- the maximum xOffset
    maxOffset = events1 |> List.map (\e -> e.xOffset) |> List.maximum |> Maybe.withDefault 1
  in events1 |> List.map (\e -> {e | xOffset = e.xOffset / maxOffset})
