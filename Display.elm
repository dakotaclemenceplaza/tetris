module Display exposing (mainDisplay)

import Html exposing (table, tr, td)
import Html.Attributes exposing (style)

mainDisplay pos =
    table [] (List.map (makeRows pos) trList)

makeRows pos row =
    tr [] (List.map (makeCell pos row) tdList)

makeCell pos row col =
    if List.any (\(c, r) -> r == row && c == col) pos
    then td [ style [("background-color", "green")] ] []
    else td [] []
                                         
trList = [20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
tdList = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
