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
    { figure : Figure,
      rotation : Int,
      currentPos : Rotation,
      position : Position,
      pile : Pile,
      pause : Bool,
      down : Bool,
      start : Bool
    }

model =
    { figure = fromList [(0, [])],
      rotation = 1,
      currentPos = [],
      position = (5, 20),
      pile = initialPile,
      pause = False,
      down = False,
      start = False
    }
           
init = (model, Cmd.none)
       
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [downs Key,
               if model.pause || not model.start
               then Sub.none
               else if model.down
                    then every (10 * millisecond) Tick
                    else every second Tick]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick _ -> case upd { model | position = (Tuple.first model.position,
                                                     Tuple.second model.position - 1) } of
                      Just newModel -> (newModel, Cmd.none)
                      Nothing -> ({ model | pile =
                                       destroyRow (model.currentPos ++ model.pile) 1,
                                       position = (5, 20),
                                       down = if model.down then False else model.down } ,
                                 generate RandomFig (int 1 7))
                  
        RandomFig rand -> case get rand randomFigure of
                              Just fig -> ({ model | figure = fig },
                                               generate RandomRot (int 0 (size fig - 1)))
                              Nothing -> (model, Cmd.none)

        RandomRot rot -> (Maybe.withDefault model (upd { model | rotation = rot }), Cmd.none)

        Key code -> case code of
                        32 -> ({ model | down = not model.down }, Cmd.none)
                        80 -> ({ model | pause = not model.pause }, Cmd.none)
                        82 -> (Maybe.withDefault model (upd { model | rotation = 
                                                           (model.rotation + 1) % (size model.figure) }) , Cmd.none)
                        37 -> (Maybe.withDefault model (upd ({ model | position =
                                                      (Tuple.first model.position - 1,
                                                           Tuple.second model.position) })) ,
                                   Cmd.none)
                        39 -> (Maybe.withDefault model (upd ({ model | position =
                                                      (Tuple.first model.position + 1,
                                                           Tuple.second model.position) })) ,
                                   Cmd.none)
                        83 -> ({ model | start = not model.start }, generate RandomFig (int 1 7))
                        _ -> (model, Cmd.none)

view : Model -> Html msg
view { currentPos, pile } =
    div []
        [  h2 [] [ text "Tetris" ],
           mainDisplay currentPos pile ]

