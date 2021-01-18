module Solver where
import Hidato_Tools
import Hugs.Observe

-- sh = [ [-1] ++ row | row  hidato'']

near_final :: [[Int]] -> (Int,Int) -> Int -> Bool
near_final hidato pos len =
    let 
        cn = [ 1 | mov <- movs, let npos = pos +. mov, not (box_out hidato npos), (hidato !! (fst npos)) !! (snd npos) == len]
    in length cn > 0


solver_hidato_aux :: Int -> Int ->  [[Int]] -> (Int,Int) -> [[[Int]]] -> [Int] -> [[[Int]]]
solver_hidato_aux len value hidato actual_pos solutions setted
        | value == (len-1) && near_final hidato actual_pos len = [hidato] ++ solutions
        | otherwise =
            let 
                solutions' = [solver_hidato_aux len (value+1) hidato' new_pos solutions setted' | mov <- movs, let new_pos = actual_pos +. mov, let hidato' = set_matrix_value hidato new_pos (value+1), let setted' = if elem (value+1) setted then setted else [value+1] ++ setted, not (box_out hidato new_pos), (to_play_box hidato new_pos && not (elem (value+1) setted)) || (hidato !! (fst new_pos)) !! (snd new_pos) == value + 1]       
            in myconcat solutions'



solver_hidato :: ([[Int]], (Int,Int), (Int,Int)) -> [[[Int]]]
solver_hidato (hidato, init, (fr,fc)) =
    let
        len = ((hidato !! fr) !! fc)
        in solver_hidato_aux len 1 hidato init [] (settedfunc hidato)