import Html exposing (Html, program, div, h2, p, text, span)
import Html.Attributes exposing (id, class, property)
import Json.Encode exposing (string)
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
    let rot = model.rotation
        (col, row) = model.position
    in
    case msg of
        Tick _ ->
            case upd { model | position = (col, row - 1) } of
                Just newModel -> (newModel, Cmd.none)
                Nothing -> ({ model | pile =
                                  destroyRow (model.currentPos ++ model.pile) 1,
                                  position = (5, 20),
                                  down = if model.down then False else model.down },
                                generate RandomFig (int 1 7))
                  
        RandomFig rand ->
            case get rand randomFigure of
                Just fig -> ({ model | figure = fig },
                                 generate RandomRot (int 0 (size fig - 1)))
                Nothing -> (model, Cmd.none)

        RandomRot rot ->
            (Maybe.withDefault model (upd { model | rotation = rot }), Cmd.none)

        Key code ->
            case code of
                32 -> ({ model | down = not model.down }, Cmd.none)
                80 -> ({ model | pause = not model.pause }, Cmd.none)
                82 -> (Maybe.withDefault model
                           (upd { model | rotation = 
                                      (rot + 1) % (size model.figure) }) , Cmd.none)
                37 -> (Maybe.withDefault model (upd ({ model | position =
                                                      (col - 1, row) })), Cmd.none)
                39 -> (Maybe.withDefault model (upd ({ model | position =
                                                      (col + 1, row) })), Cmd.none)
                83 -> ({ model | start = not model.start }, generate RandomFig (int 1 7))
                _ -> (model, Cmd.none)

view : Model -> Html msg
view { currentPos, pile, start } =
    div [ id "main" ]
        [  h2 [] [ text "Tetris" ],
           if not start
           then p [ id "starting" ] [ text "Press Space to start the Game" ]
           else text "",
           div [ id (if not start then "opacity" else "") ] [ mainDisplay currentPos pile ],
           div [ id "controls" ] [
                p [ class "rotate" ] [ text "r" ],
                p [ class "rotate" ] [ text "rotate" ],
                p []
                    [ leftArr, span [ id "left" ] [ text "left" ],
                      span [ id "right" ] [ text "right", rightArr ] ],
                p [ class "down" ] [ text "down" ] ,
                p [ class "down", property "innerHTML" (string "&darr;") ] [] ] 
        ]

leftArr = span [ class "left", property "innerHTML" (string "&larr;") ] []
rightArr = span [ id "rarr", property "innerHTML" (string "&rarr;") ] []
