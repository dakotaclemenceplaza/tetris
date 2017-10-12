module Update exposing (upd, destroyRow)

import Random exposing (generate, int)
import Types exposing (..)
import Dict exposing (get)

upd model =
    let chooseRotation = get model.rotation model.figure
        (col, row) = model.position
    in case chooseRotation of
           Just rot -> let newPos = List.map (\(c, r) -> (col + c, row + r)) rot
                       in if notPile model.pile newPos
                          then Just { model | currentPos = newPos }
                          else Nothing
           Nothing -> Nothing
                      
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
