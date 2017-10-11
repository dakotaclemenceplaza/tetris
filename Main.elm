import Html exposing (Html, program)
import Time exposing (second, every)
import Random exposing (generate, int)
import Dict exposing (..)
import Keyboard exposing (downs)

import Types exposing (..)
import Figures exposing (..)
import Display exposing (..)
import Update exposing (..)

main = program { init = init, update = update, subscriptions = subscriptions, view = view }

type alias Model =
    { figure : Maybe Figure,
      rotation : Int,
      currentPos : Rotation,
      position : Position,
      pile : Pile,
      pause : Bool
    }

initialPile =
    [(1, 1), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0), (8, 0), (9, 0), (10, 0)] 

model =
    { figure = Just (fromList [(0, [])]),
      rotation = 1,
      currentPos = [],
      position = (5, 20),
      pile = initialPile,
      pause = True
    }
           
init = (model, generate RandomFig (int 1 7))
       
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [downs Key,
               if model.pause
               then Sub.none
               else every second Tick]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick _ -> updateOnTick msg model
                  
        Start -> (model, generate RandomFig (int 1 7))

        RandomFig rand -> let fig = get rand randomFigure
                          in case Maybe.map size fig of
                                 Just n -> ({ model | figure = fig},
                                            generate RandomRot (int 1 n))
                                 Nothing -> (model, Cmd.none)

        RandomRot rot -> updateOnRandomRot { model | rotation = rot }

        Key code -> case code of
                        32 -> ({ model | pause = not model.pause }, Cmd.none)
                        _ -> (model, Cmd.none)
                          
view : Model -> Html msg
view { currentPos, pile } =
    mainDisplay currentPos pile

