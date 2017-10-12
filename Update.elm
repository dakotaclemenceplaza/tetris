module Update exposing (..)

import Random exposing (generate, int)
import Types exposing (..)
import Dict exposing (get)

updateOnTick model =
    let newCurrentPos = List.map (\(c, r) -> (c, r - 1)) model.currentPos
    in if notPile model.pile newCurrentPos
       then ({ model | currentPos = newCurrentPos,
                                    position = (Tuple.first model.position, 
                                                Tuple.second model.position - 1) }, Cmd.none)
       else ({ model | pile = destroyRow (model.currentPos ++ model.pile) 1,
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
    List.all (\(c, r) -> List.all (\(co, ro) -> co /= c || ro /= r) pile) pos

destroyRow pile currentRow =
    if currentRow < 21
    then if List.length (List.filter (\(c, r) -> r == currentRow) pile) == 12
         then destroyRow (destroy pile 0) currentRow
         else destroyRow pile (currentRow + 1)
    else pile
            
destroy pile col =
    if col > 11
    then pile
    else 
        let (column, other) = List.partition (\(c, r) -> c == col) pile
        in if col > 0 && col < 11
           then (Maybe.withDefault [] (List.tail (List.reverse (List.sortWith (\(q,w) (e,r) -> compare w r) column)))) ++ destroy other (col + 1)
           else column ++ destroy other (col + 1)
