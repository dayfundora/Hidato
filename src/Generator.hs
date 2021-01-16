module Generator where


import Random
import Hidato_Tools
import Hugs.Observe


random_hidato_len seed= fst (randomR (30,60) (mkStdGen seed) :: (Int, StdGen))




next_to_fill :: [[Int]] -> (Int,Int) -> (Int,Int)
next_to_fill hidato (r,c)
        | r == length hidato = next_to_fill hidato (0,0)
        | c == length (hidato !! 0) = next_to_fill hidato (r + 1,0)
        | (hidato !! r) !! c == -2 = (r,c)
        | otherwise = next_to_fill hidato (r,c + 1)

selectv :: [[Int]] -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Int -> Int
selectv hidato (rr,cc) (ir,ic) (fr,fc) t
        | (rr,cc) == (ir,ic) = 1
        | (rr,cc) == (fr,fc) = t
        | (hidato !! rr) !! cc == -1 = -1
        | otherwise = -2 
        


select_better_no_empty_aux :: [[Int]] -> [[Int]] -> Int -> Int -> [Int] -> ([[Int]], Int)
select_better_no_empty_aux complete_hidato shidato to_fill ri to_select
        | to_fill < 1 = (shidato, ri)
        | otherwise = 
                let
                        seed = lista !! ri
                        rposval = fst (randomR (0, (length to_select)-1) (mkStdGen seed) :: (Int, StdGen))
                        value1 = to_select !! rposval
                        value2 = value1 + 2
                        pos1 = search_pos complete_hidato value1
                        shidato1 = set_matrix_value shidato pos1 value1
                        to_select1 = mydrop value1 to_select
                        
                        (shidato', to_fill', to_select')
                                | elem value2 to_select1 =
                                        let 
                                        pos2 = search_pos complete_hidato value2
                                        shi = set_matrix_value shidato1 pos2 value2
                                        ts = mydrop value2 to_select1
                                        in (shi, to_fill - 2, ts)
                                | otherwise = (shidato1, to_fill - 1, to_select1)
                        
                in select_better_no_empty_aux complete_hidato shidato' to_fill' (ri+1) to_select'



select_better_no_empty :: ([[Int]],(Int,Int),(Int,Int), Int) -> ([[Int]], Int)
select_better_no_empty (hidato, ip, (fr,fc), ri) =
        let
                t = (hidato !! fr) !! fc
                to_fill = div (35*t) 100
                (n,m) = shape hidato              
                shidato = [[val | y <- [0..m-1], let val = selectv hidato (x,y) ip (fr,fc) t] | x <- [0..n-1]]
                to_select = not_settedfunc shidato t
        in select_better_no_empty_aux hidato shidato to_fill ri to_select
        


select_no_empty_aux :: [[Int]] -> [[Int]] -> Int -> Int -> ([[Int]], Int)
select_no_empty_aux complete_hidato shidato to_fill ri
        | to_fill == 0 = (shidato, ri)
        | otherwise = 
                let
                        (r,c) = random_box complete_hidato ri
                        (r',c') = next_to_fill shidato (r,c)
                        value = (complete_hidato !! r') !! c' 
                        shidato' = set_matrix_value shidato (r',c') value
                in select_no_empty_aux complete_hidato shidato' (to_fill-1) (ri+1)


select_no_empty :: ([[Int]],(Int,Int),(Int,Int), Int) -> ([[Int]], Int)
select_no_empty (hidato, ip, (fr,fc), ri) =
        let
                t = (hidato !! fr) !! fc
                to_fill = div (30*t) 100
                (n,m) = shape hidato              
                shidato = [[val | y <- [0..m-1], let val = selectv hidato (x,y) ip (fr,fc) t] | x <- [0..n-1]]
        in select_no_empty_aux hidato shidato to_fill ri
                

create_hidato_aux :: Int -> ([[Int]], (Int,Int)) -> (Int,Int) -> Int -> Int
        -> ([[Int]], (Int,Int), (Int,Int), Int)
create_hidato_aux len (hidato, init_pos) actual_pos value randomindex
        | value == len = (set_matrix_value hidato actual_pos value, init_pos, actual_pos, randomindex)
        | otherwise = let
                hidato' = set_matrix_value hidato actual_pos value
                seed = lista !! randomindex

                emptys = [pos_pos | pmov <- movs, 
                        let pos_pos = actual_pos +. pmov,
                        box_empty hidato pos_pos]
                outs = [pos_pos | pmov <- movs, 
                        let pos_pos = actual_pos +. pmov,
                        box_out hidato pos_pos]
                        -- && not (int_box hidato pos_pos))]


                res = fst (randomR (0, (length emptys)-1) (mkStdGen seed) :: (Int, StdGen))
                ros = fst (randomR (0, (length outs)-1) (mkStdGen (seed* fst
                        (random (mkStdGen seed) :: (Int, StdGen)))) :: (Int, StdGen))
                (npi, new_pos)
                        | length emptys > 0 = (res, emptys !! res) 
                        | length outs > 0 = (ros, outs !! ros)
                        | otherwise = (-2, (-1,-1))


                (hidato'', init_pos', new_pos')
                        | fst new_pos < 0 = let
                                ff = [-1 | x <- [1..length (hidato' !! 0)]]
                                in (ff : hidato', init_pos +. (1,0), new_pos +. (1,0))
                        | fst new_pos == length hidato' = let
                                ff = [-1 | x <- [1..length (hidato' !! 0)]]
                                in (hidato' ++ [ff], init_pos, new_pos)
                        | otherwise = (hidato', init_pos, new_pos)


                (hidato''', init_pos'', new_pos'')
                        | snd new_pos < 0 = let
                                sh = [ [-1] ++ row | row <- hidato'']
                                in (sh, init_pos' +. (0,1), new_pos' +. (0,1))
                        | snd new_pos == length (hidato'' !! 0) = let
                                sh = [ row ++ [-1] | row <- hidato'']
                                in (sh, init_pos', new_pos')
                        | otherwise = (hidato'', init_pos', new_pos')
                
                value' = value + 1
                randomindex' = randomindex + 1

                in 
                        if npi /= -2 then
                        create_hidato_aux len (hidato''', init_pos'') new_pos'' value' randomindex' 
                        else bkpt "bad" (hidato', init_pos, actual_pos, randomindex)



create_hidato :: Int -> Int -> ([[Int]], (Int,Int), (Int,Int), Int, [[Int]])
create_hidato len ri = 
        let
                (hidato, init, final, ri') = create_hidato_aux len ([[]], (0,0)) (0,0) 1 ri
                (hidato', ri'') = select_better_no_empty (hidato, init, final, ri')
        in (hidato', init, final, ri'', hidato)