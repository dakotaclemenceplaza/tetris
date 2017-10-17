module Types exposing (..)

import Time exposing (Time)
import Dict exposing (Dict)
import Keyboard exposing (KeyCode)

type Msg = Tick Time
         | RandomFig Int
         | RandomRot Int
         | Key KeyCode

type alias Position = (Int, Int)    
type alias Pile = List Position
type alias Rotation = List Position

type alias Figure = Dict Int Rotation

type alias Level = Int
type alias Score = Int
    
type alias Model =
    { figure : Figure,
      rotation : Int,
      currentPos : Rotation,
      position : Position,
      pile : Pile,
      pause : Bool,
      down : Bool,
      start : Bool,
      level : Level,
      score : Score,
      btbTetris : Bool
    }
