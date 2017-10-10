import Html exposing (Html, program)
import Time exposing (second, every)
import Random exposing (generate, int)

import Types exposing (..)
import Figures exposing (..)
import Display exposing (..)
import Update exposing (..)

main = program { init = init, update = update, subscriptions = subscriptions, view = view }

type alias Model =
    { figure : Figure,
      rotation : Int,
      currentPos : Rotation,
      position : Position,
      pile : Pile
    }

initialPile =
    [(1, 1), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0), (8, 0), (9, 0), (10, 0)] 

model =
    { figure = [], rotation = 1, currentPos = [], position = (5, 20), pile = initialPile }
           
init = (model, generate RandomFig (int 1 7))
       
subscriptions : Model -> Sub Msg
subscriptions model =
    every second Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick _ -> updateOnTick msg model
                  
        Start -> (model, generate RandomFig (int 1 7))

        RandomFig rand -> ({ model | figure = randomFigure rand},
                           generate RandomRot (int 1 (List.length model.figure)))

        RandomRot rot -> updateOnRandomRot { model | rotation = rot }
                          
view : Model -> Html msg
view { currentPos, pile } =
    mainDisplay currentPos pile

