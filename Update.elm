module Update exposing (..)

import Random exposing (generate, int)
import Types exposing (..)
import Dict exposing (..)

updateOnTick model =
    let position = model.position
        newPos = Tuple.second position - 1
        newCurrentPos = List.map (\(c, r) -> (c, r - 1)) model.currentPos
    in if not (List.any (\(c, r) -> List.any
                                    (\(co, ro) -> co == c && ro == r)
                                        model.pile)
                   newCurrentPos)
       then ({ model | currentPos = newCurrentPos, position = (Tuple.first position, 
                                                               newPos) }, Cmd.none)
       else ({ model | pile = model.currentPos ++ model.pile,
                              position = (5, 20),
                              down = if model.down then False else model.down },
            generate RandomFig (int 1 7))

updateRotation model =
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
                                              
move model move =
    case move of
        Left -> let position = (Tuple.first model.position - 1,
                                Tuple.second model.position)
                    newCurrentPos = List.map (\(c, r) -> (c - 1, r)) model.currentPos
                in if not (List.any (\(c, r) -> List.any
                                    (\(co, ro) -> co == c && ro == r)
                                        model.pile)
                               newCurrentPos)
                   then ({ model | currentPos = newCurrentPos,
                               position = position }, Cmd.none)
                   else (model, Cmd.none)
        Right -> let position = (Tuple.first model.position + 1,
                                Tuple.second model.position)
                     newCurrentPos = List.map (\(c, r) -> (c + 1, r)) model.currentPos
                 in if not (List.any (\(c, r) -> List.any
                                      (\(co, ro) -> co == c && ro == r)
                                          model.pile)
                               newCurrentPos)
                   then ({ model | currentPos = newCurrentPos,
                               position = position }, Cmd.none)
                   else (model, Cmd.none)
