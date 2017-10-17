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

model =
    { figure = fromList [(0, [])],
      rotation = 1,
      currentPos = [],
      position = (5, 20),
      pile = initialPile,
      pause = False,
      down = False,
      start = False,
      level = 1,
      nextLevel = 0,
      score = 0,
      btbTetris = False
    }
           
init = (model, Cmd.none)
       
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [downs Key,
               if model.pause || not model.start
               then Sub.none
               else if model.down
                    then every (10 * millisecond) Tick
                    else every (second / (toFloat (model.nextLevel // 10 + 1))) Tick]

        
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let rot = model.rotation
        (col, row) = model.position
    in
    case msg of
        Tick _ ->
            case upd { model | position = (col, row - 1) } of
                Just newModel -> (newModel, Cmd.none)
                Nothing -> let (newPile, addScore, next) =
                                   destroyRows (model.currentPos ++ model.pile) 1 0 (model.level + 1) model.nextLevel
                           in ({ model | pile = newPile,
                                     position = (5, 20),
                                     score = model.score + addScore,
                                     nextLevel = next,
                                     level = next // 10 + 1,
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
                32 -> if not model.start then
                          ({ model | start = True },
                               generate RandomFig (int 1 7))
                      else if not model.pause then
                               ({ model | down = not model.down }, Cmd.none)
                           else (model, Cmd.none)
                80 -> if model.start then
                          ({ model | pause = not model.pause }, Cmd.none)
                      else (model, Cmd.none)
                82 -> if not model.pause then
                          (Maybe.withDefault model
                               (upd { model | rotation = 
                                          (rot + 1) % (size model.figure) }) , Cmd.none)
                      else (model, Cmd.none)
                37 -> if not model.pause then
                          (Maybe.withDefault model (upd ({ model | position =
                                                               (col - 1, row) })), Cmd.none)
                      else (model, Cmd.none)
                39 -> if not model.pause then
                          (Maybe.withDefault model (upd ({ model | position =
                                                               (col + 1, row) })), Cmd.none)
                      else (model, Cmd.none)
                _ -> (model, Cmd.none)

                     
view : Model -> Html Msg
view { currentPos, position, pile, start, level, score } =
    let leftArr = span [ class "left", property "innerHTML" (string "&larr;") ] []
        rightArr = span [ id "rarr", property "innerHTML" (string "&rarr;") ] []
    in
        div [ id "main" ]
            [  h2 [] [ text "Tetris" ],
                   if not start
                   then p [ id "starting" ] [ text "Press Space to start the Game" ]
                   else text "",
                   div [ id "gameInfo" ] [
                        p [ class "info" ] [ text ("Score: " ++ (toString score)) ],
                        p [ class "info" ] [ text ("Level: " ++ (toString level)) ]
                       ],
                   div [ id (if not start then "opacity" else "") ] [ mainDisplay currentPos pile ],
                   div [ id "controls" ] [
                        div [ class "rotate" ] [ p [] [ text "r" ],
                                                     p [] [ text "Rotate" ] ],
                            p []
                            [ leftArr, span [ id "left" ] [ text "Left" ],
                                  span [ id "right" ] [ text "Right", rightArr ] ],
                            div [ class "down" ] [ p [] [ text "Down" ] ,
                                                       p [] [ text "space" ] ],
                            div [ class "down" ] [ p [] [ text "Pause" ],
                                                       p [] [ text "p" ] ] ]
            ]

