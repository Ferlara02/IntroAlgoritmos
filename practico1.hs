-- Práctico 1 HASKELL | Introducción a los Algoritmos - FAMAFC

absoluto :: Int -> Int
absoluto x | x > 0 || x == 0   = x
           | x < 0   = -x

-- Ej. 19-24)

esMultiploDe x y = (mod y x == 0)

esBisiesto x = (mod x 400 == 0 || mod x 4 == 0) && mod x 100 /= 0 

dispersion x y z = (max x (max y z)) - (min x (min y z)) --Toma tres valores numéricos y devuelve la diferenbcia entre el más alto y el más bajo.



-- Ej. 25)
rangoPrecioParametrizado :: (Num a, Ord a) => a -> (a, a) -> String
rangoPrecioParametrizado x (menor, mayor)
    | x < 0               = "esto no puede ser!"
    | x < menor           = "muy barato"
    | x > mayor           = "demasiado caro"
    | otherwise           = "hay que verlo bien"


mayor3 :: (Num a, Ord a) => (a, a, a) -> (Bool, Bool, Bool)
mayor3 (a, b, c) = (a > 3, b > 3, c > 3)


segundo3 :: (Num a, Ord a) => (a, a, a) -> a
segundo3 (a, b, c) = b 

ordena :: (Num a, Ord a) => (a, a) -> (a, a)
ordena (x, y) = ((min x y), (max x y))  

todosIguales :: (Num a, Ord a) => (a, a, a) -> Bool 
todosIguales (x, y, z) = x == y && y == z