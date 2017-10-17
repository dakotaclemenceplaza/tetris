module Update exposing (upd, destroyRow)

import Types exposing (..)
import Dict exposing (get)

upd model =
    let chooseRotation = get model.rotation model.figure
        (col, row) = model.position
    in Maybe.map (List.map (\(c, r) -> (col + c, row + r))) chooseRotation
        |> Maybe.andThen (notPile model.pile)
        |> Maybe.andThen (\newPos -> Just { model | currentPos = newPos })

notPile pile pos =
    if List.all (\(c, r) -> List.all (\(co, ro) -> co /= c || ro /= r) pile) pos
    then Just pos
    else Nothing

destroyRow pile currentRow score level =
    if currentRow < 21
    then if List.length (List.filter (\(c, r) -> r == currentRow) pile) == 12
         then destroyRow (destroy pile 0) currentRow (updateScore score (level + 1)) level
         else destroyRow pile (currentRow + 1) score level
    else (pile, score)
            
destroy pile col =
    if col > 11
    then pile
    else 
        let (column, other) = List.partition (\(c, r) -> c == col) pile
        in if col > 0 && col < 11
           then (Maybe.withDefault [] (List.tail (List.reverse (List.sortWith (\(q,w) (e,r) -> compare w r) column)))) ++ destroy other (col + 1)
           else column ++ destroy other (col + 1)

updateScore score level =
    if score == 0 then score + (40 * level)
        else if score == (40 * level) then score + (60 * level)
            else if score == (100 * level) then score + (200 * level)
                 else score + (900 * level)
