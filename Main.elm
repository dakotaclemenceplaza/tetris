import Html exposing (Html, table, tr, td, program)
import Html.Attributes exposing (style, id)
import Time exposing (Time, second, every)
import Random exposing (generate, int)

import Figures exposing (..)
import Display exposing (..)

main = program { init = init, update = update, subscriptions = subscriptions, view = view }

type alias Model =
    { figure : Figure,
      rotation : Int,
      position : Position,
      pile : Pile
    }

type alias Pile = List Position

type alias Position = (Int, Int)

model = { figure = [], rotation = 1, position = (5, 20), pile = [] }
        
type Msg = Tick Time
         | Start
         | RandomFig Int
         | RandomRot Int

init = (model, generate RandomFig (int 1 7))
       
subscriptions : Model -> Sub Msg
subscriptions model =
    every second Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick _ -> ({ model | position = (Tuple.first model.position,
                                         Tuple.second model.position - 1) }, Cmd.none)
                  
        Start -> (model, generate RandomFig (int 1 7))

        RandomFig rand -> ({ model | figure = randomFigure rand},
                           generate RandomRot (int 1 (List.length model.figure)))

        RandomRot rot -> ({ model | rotation = rot } , Cmd.none)
                          
view : Model -> Html msg
view { figure, rotation, position, pile } =
    let (col, row) = position
        fuck = List.head (List.drop (rotation - 1) figure)
    in
        case fuck of
            Just just -> let currentRotation = just
                             currentPositions = List.map (\(c, r) -> (col + c, row + r)) currentRotation
                         in mainDisplay currentPositions
            Nothing -> Html.text "fuck"
