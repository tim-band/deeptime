module Stratigraphy exposing (..)

import Dict
import Json.Decode as D

import Base exposing (..)

-- [{"id","name","fill","type","narrow":[],"broad":[]}]
timeline_data_url: String
timeline_data_url = "/resources/timeline_data.json"
--timeline_data_url = "https://github.com/CSIRO-enviro-informatics/interactive-geological-timescale/blob/master/src/assets/timeline_data.json"
-- {id:{"hasBeginning": float, "hasEnd": float}}
time_interval_data_url: String
time_interval_data_url = "/resources/time_interval_data.json"
--time_interval_data_url = "https://github.com/CSIRO-enviro-informatics/interactive-geological-timescale/blob/master/src/assets/time_interval_data.json"

type alias StratigraphyData =
  { name: String
  , fill: String
  , type_: String
  , narrow: List String
  , broad: List String
  }

type FloatInterval = FloatInterval Float Float
type alias DataDict = Dict.Dict String StratigraphyData
type alias IntervalDict = Dict.Dict String FloatInterval

makeStratPair : String -> String -> String -> String -> List String -> List String -> (String, StratigraphyData)
makeStratPair id name f t n b = (id, { name=name, fill=f, type_=t, narrow=n, broad=b })

decodeStratigraphy : D.Decoder (String, StratigraphyData)
decodeStratigraphy = D.map6 makeStratPair
  (D.field "id" D.string)
  (D.field "name" D.string)
  (D.field "fill" D.string)
  (D.field "type" D.string)
  (D.field "narrow" (D.list D.string))
  (D.field "broad" (D.list D.string))

decodeData : D.Decoder DataDict
decodeData = D.map Dict.fromList <| D.list decodeStratigraphy

decodeStratigraphyInterval : D.Decoder FloatInterval
decodeStratigraphyInterval = D.map2 FloatInterval
  (D.field "hasBeginning" D.float)
  (D.field "hasEnd" D.float)

decodeIntervals : D.Decoder IntervalDict
decodeIntervals = D.dict decodeStratigraphyInterval

events : DataDict -> IntervalDict -> List
  { category : Int
  , time : Float
  , name : String
  }
events dd ints =
  let
    addEvent : String -> StratigraphyData -> List {category:Int, time:Float,name:String} -> List {category:Int, time:Float,name:String}
    addEvent id std acc = case Dict.get id ints of
      Nothing -> acc
      Just (FloatInterval start end) ->
        { category = 1
        , time = present - (start + end) * 5e5
        , name = std.name
        } :: acc
  in Dict.foldr addEvent [] dd
