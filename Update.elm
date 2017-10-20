module Update exposing (upd, destroyRows)

import Types exposing (..)
import Dict exposing (get)

upd : Model -> Maybe Model
upd model =
    let rotation = get model.rotation model.figure
        (col, row) = model.position
    in Maybe.map (List.map (\(c, r) -> (col + c, row + r))) rotation
        |> Maybe.andThen (notPile model.pile)
        |> Maybe.andThen (\newPos -> Just { model | currentPos = newPos })

           
notPile : Pile -> Rotation -> Maybe Rotation           
notPile pile pos =
    if List.all (\(c, r) -> List.all (\(co, ro) -> co /= c || ro /= r) pile) pos
    then Just pos
    else Nothing


destroyRows : Pile -> Int -> Score -> Level -> Lines -> (Pile, Score, Lines)        
destroyRows pile currentRow score level lines =
    if currentRow < 21
    then if List.length (List.filter (\(c, r) -> r == currentRow) pile) == 12
         then destroyRows (destroy pile 0) currentRow (updateScore score level) level (lines + 1)
         else destroyRows pile (currentRow + 1) score level lines
    else (pile, score, lines)


destroy : Pile -> Int -> Pile        
destroy pile col =
    if col > 11
    then pile
    else let (column, other) = List.partition (\(c, r) -> c == col) pile
         in if col > 0 && col < 11
            then (Maybe.withDefault []
                      (List.tail
                           (List.reverse
                                (List.sortWith
                                     (\(q,w) (e,r) -> compare w r) column))))
                ++ destroy other (col + 1)
            else column ++ destroy other (col + 1)


updateScore : Score -> Level -> Score                
updateScore score level =
    if score == 0 then score + (40 * level)
        else if score == (40 * level) then score + (60 * level)
            else if score == (100 * level) then score + (200 * level)
                 else score + (900 * level)
