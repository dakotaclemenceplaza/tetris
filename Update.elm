module Update exposing (..)

import Random exposing (generate, int)
import Types exposing (..)
import Dict exposing (..)

updateOnTick msg model =
    let position = model.position
        newPos = Tuple.second position - 1
        newCurrentPos = List.map (\(c, r) -> (c, r - 1)) model.currentPos
    in if not (List.any (\(c, r) -> List.any
                                    (\(co, ro) -> co == c && ro == r)
                                        model.pile)
                   newCurrentPos)
       then ({ model | currentPos = newCurrentPos, position = (Tuple.first position, 
                                                               newPos) }, Cmd.none)
       else ({ model | pile = model.currentPos ++ model.pile, position = (5, 20) },
            generate RandomFig (int 1 7))

updateOnRandomRot model =
    case model.figure of
        Just fig ->
            let rotation = model.rotation
                (col, row) = model.position
                chooseRotation = get rotation fig
            in case chooseRotation of
                   Just rot ->
                       ({ model | currentPos = List.map (\(c, r) -> (col + c, row + r)) rot },
                        Cmd.none)
                   Nothing -> (model, Cmd.none)
        Nothing -> (model, Cmd.none)
                                              
