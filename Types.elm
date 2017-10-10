module Types exposing (..)

import Time exposing (Time)

type Msg = Tick Time
         | Start
         | RandomFig Int
         | RandomRot Int

type alias Position = (Int, Int)    
type alias Pile = List Position
type alias Rotation = List Position
type alias Figure = List Rotation
