module Utils where




print_row :: [Int] -> Int -> String
print_row row index
    | index == length row = ""
    | otherwise =
        let 
            v = row !! index
            value = if v == -1 then "  " else if div v 10 == 0 then " " ++ show v else show v
            in show value ++ " " ++ (print_row row (index + 1))


pretty_print_aux :: [[Int]] -> Int -> String
pretty_print_aux matrix index
    | index == length matrix = "\n"
    | otherwise = (print_row (matrix !! index) 0) ++ "\n" ++ (pretty_print_aux matrix (index + 1))


pretty_print matrix = pretty_print_aux matrix 0


view_solutions_aux :: [[[Int]]] -> Int -> String
view_solutions_aux solutions index
        | index == length solutions = "\n"
        | otherwise = "Solution " ++ show index ++ "\n" ++ pretty_print (solutions !! index) 
                ++ view_solutions_aux solutions (index+1)