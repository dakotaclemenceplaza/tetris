import Html exposing (Html, program, div, h2, p, text, span, a)
import Html.Attributes exposing (id, class, property, href)
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
      down = False,
      level = 1,
      lines = 0,
      score = 0,
      gameState = Start
    }
           
init = (model, Cmd.none)
       
subscriptions : Model -> Sub Msg
subscriptions { down, lines, gameState } =
    Sub.batch [downs Key,
               if gameState == Play
               then if down
                    then every (10 * millisecond) Tick
                    else every (second / (toFloat (lines // 10 + 1))) Tick
               else Sub.none]
        
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let (col, row) = model.position
        gameState = model.gameState
    in
    case msg of
        Tick _ ->
            case upd { model | position = (col, row - 1) } of
                Just newModel -> (newModel, Cmd.none)
                Nothing -> let (newPile, addScore, newLines) =
                                   destroyRows (model.currentPos ++ model.pile) 1 0 model.level model.lines
                           in ({ model | pile = newPile,
                                     position = (5, 20),
                                     score = model.score + addScore,
                                     lines = newLines,
                                     level = newLines // 10 + 1,
                                     down = if model.down then False else model.down },
                                   generate RandomFig (int 1 7))
                  
        RandomFig rand ->
            case get rand randomFigure of
                Just fig -> ({ model | figure = fig },
                                 generate RandomRot (int 0 (size fig - 1)))
                Nothing -> (model, Cmd.none)

        RandomRot rot ->
            (Maybe.withDefault { model | gameState = Over }
                 (upd { model | rotation = rot }), Cmd.none)

        Key code ->
            case code of
                32 -> if gameState == Start then
                          ({ model | gameState = Play },
                               generate RandomFig (int 1 7))
                      else if gameState == Play then
                               ({ model | down = not model.down }, Cmd.none)
                           else (model, Cmd.none)
                80 -> if gameState == Play then
                          ({ model | gameState = Pause }, Cmd.none)
                      else if gameState == Pause then
                               ({ model | gameState = Play }, Cmd.none)
                           else (model, Cmd.none)
                82 -> if gameState == Play then
                          (Maybe.withDefault model
                               (upd { model | rotation = 
                                          (model.rotation + 1) % (size model.figure) }) , Cmd.none)
                      else (model, Cmd.none)
                37 -> if gameState == Play then
                          (Maybe.withDefault model (upd ({ model | position =
                                                               (col - 1, row) })), Cmd.none)
                      else (model, Cmd.none)
                39 -> if gameState == Play then
                          (Maybe.withDefault model (upd ({ model | position =
                                                               (col + 1, row) })), Cmd.none)
                      else (model, Cmd.none)
                _ -> (model, Cmd.none)

                     
view : Model -> Html Msg
view { currentPos, pile, level, score, gameState } =
    let leftArr = span [ class "left", property "innerHTML" (string "&larr;") ] []
        rightArr = span [ id "rarr", property "innerHTML" (string "&rarr;") ] []
    in
        div [ id "main" ]
            [  h2 [] [ text "Tetris" ],
                   if gameState == Start
                   then p [ id "starting" ] [ text "Press Space to start the Game" ]
                   else if gameState == Over
                        then p [ id "starting" ] [ text "Game Over" ]
                        else text "",
                   div [ id "gameInfo" ] [
                        p [ class "info" ] [ text ("Score: " ++ (toString score)) ],
                        p [ class "info" ] [ text ("Level: " ++ (toString level)) ]
                       ],
                   div [ id (if gameState == Start || gameState == Over
                             then "opacity"
                             else "") ] [ mainDisplay currentPos pile ],
                   div [ id "controls" ] [
                        div [ class "rotate" ] [ p [] [ text "r" ],
                                                     p [] [ text "Rotate" ] ],
                            p []
                            [ leftArr, span [ id "left" ] [ text "Left" ],
                                  span [ id "right" ] [ text "Right", rightArr ] ],
                            div [ class "down" ] [ p [] [ text "Down" ] ,
                                                       p [] [ text "space" ] ],
                            div [ class "down" ] [ p [] [ text "Pause" ],
                                                       p [] [ text "p" ] ] ],
                   div [ id "elm" ]
                       [ p [] [ text "Written in " ,
                                a [ href "http://elm-lang.org/" ] [ text "Elm" ] ] ]
            ]
