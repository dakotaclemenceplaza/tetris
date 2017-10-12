module Display exposing (mainDisplay, initialPile)

import Html exposing (table, tr, td)
import Html.Attributes exposing (style)

mainDisplay position pile =
    let pos = position ++ (List.filter (\(c, r) -> r /= 0) pile)
    in table [] (List.map (makeRows pos) trList)

makeRows pos row =
    tr [] (List.map (makeCell pos row) tdList)

makeCell pos row col =
    if List.any (\(c, r) -> r == row && c == col) pos
    then td [ style [("background-color", "#a6a6a6")] ] []
    else td [] []
                                         
trList = [20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
tdList = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

initialPile =
    [(1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0), (8, 0), (9, 0), (10, 0), 
     (0, 1), (0, 2), (0, 3), (0, 4), (0, 5),
     (0, 6), (0, 7), (0, 8), (0, 9), (0, 10),
     (0, 11), (0, 12), (0, 13), (0, 14), (0, 15),
     (0, 16), (0, 17), (0, 18), (0, 19), (0, 20),
     (11, 1), (11, 2), (11, 3), (11, 4), (11, 5),
     (11, 6), (11, 7), (11, 8), (11, 9), (11, 10),
     (11, 11), (11, 12), (11, 13), (11, 14), (11, 15),
     (11, 16), (11, 17), (11, 18), (11, 19), (11, 20)]
