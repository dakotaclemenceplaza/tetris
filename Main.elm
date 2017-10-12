import Html exposing (Html, program, div, h2, text)
import Time exposing (second, millisecond, every)
import Random exposing (generate, int)
import Dict exposing (fromList, get, size)
import Keyboard exposing (downs)

import Types exposing (..)
import Figures exposing (randomFigure)
import Display exposing (mainDisplay, initialPile)
import Update exposing (..)

main = program { init = init, update = update, subscriptions = subscriptions, view = view }

type alias Model =
    { figure : Maybe Figure,
      rotation : Int,
      rotationSize : Int,
      currentPos : Rotation,
      position : Position,
      pile : Pile,
      pause : Bool,
      down : Bool
    }

model =
    { figure = Just (fromList [(0, [])]),
      rotation = 1,
      rotationSize = 0,
      currentPos = [],
      position = (5, 20),
      pile = initialPile,
      pause = True,
      down = False
    }
           
init = (model, generate RandomFig (int 1 7))
       
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [downs Key,
               if model.pause
               then Sub.none
               else if model.down
                    then every (10 * millisecond) Tick
                    else every second Tick]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick _ -> updateOnTick model
                  
        RandomFig rand -> let fig = get rand randomFigure
                          in case Maybe.map size fig of
                                 Just n -> ({ model | figure = fig,
                                                  rotationSize = n },
                                                generate RandomRot (int 0 (n - 1)))
                                 Nothing -> (model, Cmd.none)

        RandomRot rot -> updateRotation model rot

        Key code -> case code of
                        32 -> ({ model | down = not model.down }, Cmd.none)
                        80 -> ({ model | pause = not model.pause }, Cmd.none)
                        82 -> updateRotation model ((model.rotation + 1) % model.rotationSize)
                        37 -> move model ((-) 1)
                        39 -> move model ((+) 1)
                        _ -> (model, Cmd.none)
                            
view : Model -> Html msg
view { currentPos, pile } =
    div []
        [  h2 [] [ text "Tetris" ],
           mainDisplay currentPos pile ]

