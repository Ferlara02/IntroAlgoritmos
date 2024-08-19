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

productoPares :: [Int] -> Int
productoPares [] = 1
productoPares (x:xs)
    | mod x 2 /= 0 = productoPares xs
    | otherwise = x * productoPares xs

posicion :: Int -> [Int] -> Int
posicion a [] = -1
posicion a (x:xs) 
    | a == x = 0
    | pert a (xs) = 1 + posicion a xs 
    | otherwise = -1

sumaPosicionPar :: [Int] -> Int
sumaPosicionPar [] = 0
sumaPosicionPar (x:y:xs) 
    | (mod (posicion x (y:xs)) 2 == 0) = x + sumaPosicionPar (y:xs)
    | otherwise = sumaPosicionPar (y:xs)

-- Lab 7

paraTodo2 :: [a] -> (a -> Bool) -> Bool
paraTodo2 [] t = True
paraTodo2 (x:xs) t = t x && paraTodo2 xs t

existe2 :: [a] -> (a -> Bool) -> Bool
existe2 [] t = False
existe2 (x:xs) t = t x || existe2 xs t

sumatoria2 :: [a] -> (a -> Int) -> Int 
sumatoria2 [] t = 0
sumatoria2 (x:xs) t = t x + sumatoria2 xs t  

productoria2 :: [a] -> (a -> Int) -> Int 
productoria2 [] t = 1
productoria2 (x:xs) t = t x * productoria2 xs t

--Lab 9

--auxiliares:
esPar :: Int->Bool
esPar x = mod x 2 == 0

esMultiplo :: Int -> Int -> Bool
esMultiplo a x = mod x a == 0

cuadrado :: Int->Int
cuadrado n = n^2

esDivisor :: Int -> Int -> Bool
esDivisor n a = mod n a == 0
----------------------
todosPares :: [Int] -> Bool
todosPares [] = True
todosPares (x:xs) = paraTodo2 (x:xs) esPar

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo a [] = False
hayMultiplo a (x:xs) = existe2 (x:xs) (esMultiplo a)

sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria2 [0..n] cuadrado

existeDivisor :: Int -> [Int] -> Bool 
existeDivisor n [] = False
existeDivisor n (x:xs) = existe2 (x:xs) (esDivisor n)

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = existeDivisor n [2..n-1] == False

factorial2 :: 
