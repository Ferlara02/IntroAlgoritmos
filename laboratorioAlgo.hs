--Lab 1

esCero :: Int -> Bool
esCero x = x == 0

esPositivo :: Int -> Bool
esPositivo x = x > 0

esVocal :: Char -> Bool 
esVocal x = (x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u')

valorAbsoluto:: Int -> Int 
valorAbsoluto x 
    | x < 0 = -x
    | otherwise = x

--Lab 2

paraTodo :: [Bool] -> Bool
paraTodo [] = True
paraTodo (x:xs) = (x == True && paraTodo xs)

sumatoria :: [Int] -> Int
sumatoria [] = 0 
sumatoria (x:xs) = x + sumatoria xs 

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs


--Lab 3
todosPositivos :: [Int] -> Bool
todosPositivos [] = True
todosPositivos (x:xs) = x > 0 && todosPositivos xs

--igualA :: Eq a -> [a] -> Bool
igualA a [] = False
igualA a (x:xs) = (a == x) || igualA a xs

todosIguales :: Eq a => [a] -> Bool
todosIguales [] = True
todosIguales [a] = True
todosIguales (y:x:xs) = y == x && todosIguales (x:xs)

--Lab 4

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

length1 :: [Int] -> Int
length1 [] = 0
length1 (x:xs) = 1 + length1 xs

promedio :: [Int] -> Float
promedio xs = fromIntegral(sumatoria xs) / fromIntegral(length1 xs)

maximo :: [Int] -> Int
maximo [x] = x 
maximo (x:xs) 
    | x >= maximo xs = x
    | x < maximo xs = maximo xs

minimo :: [Int] -> Int
minimo [x] = x 
minimo (x:xs) 
    | x <= minimo xs = x
    | x > minimo xs = minimo xs

isMaxMin :: [Int] -> [Int] -> Bool
isMaxMin (x:xs) (y:ys) = maximo (x:xs) < minimo (y:ys)

-- Lab 5

todos :: [Bool] -> Bool
todos [] = True
todos (x:xs) = x == True && todos xs

-- Lab 6

--funcion auxiliar pertenece:
pert :: Int -> [Int] -> Bool
pert n [] = False
pert n (x:xs) = n == x || pert n xs

masGrande :: Int -> [Int] -> Bool
masGrande n [] = True
masGrande n (x:xs) 
    | pert n (x:xs) = n >= x && masGrande n xs
    | otherwise = False

