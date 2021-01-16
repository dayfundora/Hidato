module Hidato_Tools where
import Random
import Hugs.Observe

movs :: [(Int,Int)]
movs = [(1,1), (1,-1), (1,0), (-1,-1), (-1,1), (-1,0), (0,1), (0,-1)]

-- Aleatoriedad
lista = randoms (mkStdGen 44) :: [Int]


myelem :: (Eq a) => a -> [a] -> Bool 
myelem v [] = False 
myelem v (x:xs) 
        | v == x = True 
        | otherwise = myelem v xs


mydrop :: (Eq a) => a -> [a] -> [a]
mydrop val list = [list !! i | i <- [0..(length list)-1], list !! i /= val]


settedfunc :: [[Int]] -> [Int]
settedfunc hidato =
        let (n,m) = shape hidato
        in [v | r <- [0..n-1], c <- [0..m-1], let v = ((hidato !! r) !! c), v /= -1, v /= -2]

not_settedfunc :: [[Int]] -> Int -> [Int]
not_settedfunc hidato len = 
        let st = settedfunc hidato 
        in [ i | i <- [1..len], not (elem i st)]

search_pos :: [[Int]] -> Int -> (Int,Int)
search_pos hidato val =
        let (n,m) = shape hidato
        in head [(r,c) | r <- [0..n-1], c <- [0..m-1], (hidato !! r) !! c == val]




splitat_nesimo :: [a] -> Int -> ([a],[a])
splitat_nesimo lista n = (fst p1, snd p2)
    where 
        p1 = splitAt n lista
        p2 = splitAt 1 (snd p1)


(+.) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(+.) (r1,c1) (r2,c2) = (r1+r2, c1+c2)


shape :: [[Int]] -> (Int,Int)
shape hidato = (length hidato, length (hidato !! 0))



set_matrix_value :: [[Int]] -> (Int,Int) -> Int -> [[Int]]
set_matrix_value hidato (row,col) value = let
                (pf1,pf2) = splitat_nesimo hidato row
                (pc1,pc2) = splitat_nesimo (hidato !! row) col
                in pf1 ++ [pc1 ++ [value] ++ pc2] ++ pf2

set_array_value :: [a] -> Int -> a -> [a]
set_array_value hidato pos value = let
                (p1,p2) = splitat_nesimo hidato pos
                in p1 ++ [value] ++ p2


box_out :: [[Int]] -> (Int,Int) -> Bool
box_out hidato (r,c) = r >= length hidato || c >= length (hidato !! 0)
                        || r < 0 || c < 0

box_empty :: [[Int]] -> (Int,Int) -> Bool
box_empty hidato (r,c) = (not (box_out hidato (r,c))) && ((hidato !! r) !! c) == -1


to_fill_box :: [[Int]] -> (Int,Int) -> Bool
to_fill_box hidato pos = (box_out hidato pos) || (box_empty hidato pos)

to_play_box :: [[Int]] -> (Int,Int) -> Bool
to_play_box hidato (r,c) = (hidato !! r) !! c == -2


int_box :: [[Int]] -> (Int,Int) -> Bool
int_box hidato pos = 
        foldl (\acc mov -> acc && not (to_fill_box hidato (pos +. mov))) True movs

random_box :: [[Int]] -> Int -> (Int,Int)
random_box hidato ri = 
        let
                seed = lista !! ri
                (n,m) = shape hidato
                (r,_) = randomR (0, n) (mkStdGen seed) :: (Int, StdGen)
                (c,_) = randomR (0, m) (mkStdGen (seed*100)) :: (Int, StdGen)
        in (r,c)

myconcat :: [[a]] -> [a] 
myconcat l = foldl (\acc sls -> acc ++ sls) [] l