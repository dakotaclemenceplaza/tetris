module Types exposing (..)

import Time exposing (Time)
import Dict exposing (..)

type Msg = Tick Time
         | Start
         | RandomFig Int
         | RandomRot Int

type alias Position = (Int, Int)    
type alias Pile = List Position
type alias Rotation = List Position

type alias Figure = Dict Int Rotation
