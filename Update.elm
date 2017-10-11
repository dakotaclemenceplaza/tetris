module Update exposing (..)

import Random exposing (generate, int)
import Types exposing (..)
import Dict exposing (..)

updateOnTick model =
    let newCurrentPos = List.map (\(c, r) -> (c, r - 1)) model.currentPos
    in if notPile model.pile newCurrentPos
       then ({ model | currentPos = newCurrentPos,
                                    position = (Tuple.first model.position, 
                                                Tuple.second model.position - 1) }, Cmd.none)
       else ({ model | pile = model.currentPos ++ model.pile,
                              position = (5, 20),
                              down = if model.down then False else model.down },
            generate RandomFig (int 1 7))

updateRotation model rot =
    let chooseRotation = model.figure |> Maybe.andThen (get rot)
    in (changePosition chooseRotation model.position model rot, Cmd.none)
                                                
move model f =
    let position = (abs (f (Tuple.first model.position)),
                        Tuple.second model.position)
        chooseRotation = model.figure |> Maybe.andThen (get model.rotation)
    in (changePosition chooseRotation position model model.rotation, Cmd.none)
               
changePosition currentRot position model rotai =
    case currentRot of
        Just rot -> let (col, row) = position
                        newCurrentPos = List.map (\(c, r) -> (col + c, row + r)) rot
                    in if notPile model.pile newCurrentPos
                       then { model | currentPos = newCurrentPos,
                                  position = position,
                                  rotation = rotai }
                       else model
        Nothing -> model

notPile pile pos =
    not (List.any (\(c, r) -> List.any (\(co, ro) -> co == c && ro == r) pile) pos)
