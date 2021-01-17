-- data mov  = (1,1)  
        -- | (1,-1)
        -- | (1,0)
        -- | (-1,-1)
        -- | (-1,1)
        -- | (-1,0)
        -- | (0,1)
        -- | (0,-1)
        -- deriving (Show, Eq)
{- 
        -El sudoku va a representarse como una matriz o lista de listas donde cada casilla
        tiene el valor que se le asigna
        -Las casillas vacias tendran valor -1
        -Supongo que el valor minimo no tiene que ser 1 siempre
        -Pero si voy a asumir que los valores son mayores que cero



        -Luego cuando tenga la lista de listas tengo que ver cuales valores quito de forma tal
        que la solucion sea unica (se me ocurre hacer esto por backtraking teniendo una funcion
        que diga si el sudoku tiene solucion unica (bruto, pero bueno...))


        -Solucionar el sudoku si puede ser por backtraking

        -Para generar el sudoku:
                -- Primero ver el tamanno N de la figura (generar random)
                -- Luego la matriz que la representa va a ser de dimension N*N (porque yo no se si
                al final la figura va a crecer mas a lo ancho o a lo largo y como maximo por cada
                lado va a crecer N)
                -- Donde no haya ningun valor de la figura el valor sera -1
                -- Constriur las dos cosas a la vez: el camino y la matriz, a lo mejor el camino
                se puede quedar como algo representativo
                -- Elegir random la posicion desde la cual va a comenzar el camino
-}

{-
        Mejor solucion: Empiezo por un hidato que contiene solo una casilla y por cada
        movimiento que doy si me salgo del hidato annado una nueva fila o columna o ambos

- Puzzles with unique solutions 
-}


import Random
import Hugs.Observe
import Generator
import Utils
import Solver

f 0 = 1
f n = n * (bkpt "fun" $ observe "sfun" f (n-1))



hidatos_list :: Int -> Int -> Int -> String
hidatos_list 0 _ _  = "\n"
hidatos_list n len ri =
        let
                (hidato, init, final, ri', truely_hidato) = create_hidato len ri
                solutions = solver_hidato (hidato, init, final)
                mt = pretty_print hidato
                th = pretty_print truely_hidato
                in "Hidato " ++ show ri ++ "\n" ++ mt ++ view_solutions_aux solutions 0  ++ "\n"
                ++ (hidatos_list (n-1) len ri')
                --in "Hidato " ++ show ri ++ "\n" ++ mt ++ "Truely_hidato " ++ show ri ++ "\n"
                -- ++ th ++ (hidatos_list (observe "n" (n-1)) len ri')

main = 
    do
        putStr (hidatos_list 5 20 0)