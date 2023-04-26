module EventTimeline exposing (..)

import Event exposing (Event)

type EventHierarchy = EventHierarchy
  { nextAtThisLevel : Event
  , deeper : Deeper
  } | EndEventHierarchy Deeper

type Deeper = NoDeeper | Deeper EventHierarchy

-- moves all events that match the predicate from the d2 to d1
-- assuming that the closest events will return True then all
-- the further events will return False
zipDeeper : (Event -> Bool) -> Deeper -> Deeper -> (Deeper, Deeper)
zipDeeper p d1 d2 = case d2 of
  NoDeeper -> (d1, d2)
  Deeper eh2 -> case d1 of
    NoDeeper ->
      let
        (deh1, deh2) = zipHierarchy p (EndEventHierarchy NoDeeper) eh2
      in (Deeper deh1, Deeper deh2)
    Deeper eh1 ->
      let
        (deh1, deh2) = zipHierarchy p eh1 eh2
      in (Deeper deh1, Deeper deh2)

zipHierarchy : (Event -> Bool) -> EventHierarchy -> EventHierarchy
zipHierarchy p eh1 eh2 = case eh2 of
  EndEventHierarchy d2 -> case eh1 of
    EndEventHierarchy d1 ->
      let (dd1, dd2) = zipDeeper p d1 d2
      in (EndEventHierarchy dd1, EndEventHierarchy dd2)
    EventHierarchy e1 d1 ->
      let (dd1, dd2) = zipDeeper p d1 d2
      in (EventHierarchy e1 dd1, EndEventHierarchy dd2)
  EventHierarchy e2 d2 -> if p e2
    then this does not work either does it?

