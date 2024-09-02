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


todos :: [Bool] -> Bool
todos [] = True
todos (x:xs) = x == True && todos xs

-LAB 2

data Carrera = Matematica | Fisica | Computacion | Astronomia 
deriving Eq
titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matemática"
titulo Fisica = "Licenciatura en Física"
titulo Computacion = "Licenciatura en Ciencias de la Computación"
titulo Astronomia = "Licenciatura en Astronomia"

data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si 
 deriving (Ord, Eq, Show)

cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'

--LAB 4

minimoElemento:: (Ord a) => [a] -> a
minimoElemento [x] = x
minimoElemento (x:xs) = min x (minimoElemento xs)

minimoElemento':: (Bounded a, Ord a) => [a] -> a
minimoElemento' [] = maxBound
minimoElemento' [x] = x
minimoElemento' (x:xs) = min x (minimoElemento' xs)

-- LAB 5

--Sin ́onimos de tipo
type Altura = Int
type NumCamiseta = Int
--Tipos algebr ́aicos sin par ́ametros (aka enumerados)
data Zona = Arco | Defensa | Mediocampo | Delantera
data TipoReves = DosManos | UnaMano
data Modalidad = Carretera | Pista | Monte | BMX
data PiernaHabil = Izquierda | Derecha
-- Sin ́onimo
type ManoHabil = PiernaHabil
-- Deportista es un tipo algebraico con constructores param ́etricos
data Deportista = Ajedrecista | Ciclista Modalidad  | Velocista Altura | Tenista TipoReves ManoHabil Altura | Futbolista Zona NumCamiseta PiernaHabil Altura 

contarVelocistas:: [Deportista] -> Int
contarVelocistas [] = 0
contarVelocistas (Velocista(_):xs) = 1 + contarVelocistas xs
contarVelocistas ((_):xs) = contarVelocistas xs

contarFut :: [Deportista] -> Zona -> Int
contarFut [] z = 0
contarFut ((Futbolista (Arco)(_)(_)(_)):xs) Arco = 1 + contarFut xs Arco
contarFut ((Futbolista (Mediocampo)(_)(_)(_)):xs) Mediocampo = 1 + contarFut xs Mediocampo
contarFut ((Futbolista (Delantera)(_)(_)(_)):xs) Delantera = 1 + contarFut xs Delantera
contarFut ((Futbolista (Defensa)(_)(_)(_)):xs) Defensa = 1 + contarFut xs Defensa
contarFut ((_):xs) z = contarFut xs z

contarFutb :: [Deportista] -> Zona -> Int
contarFutb [] z = 0
contarFutb ((Futbolista (Arco)(_)(_)(_)):xs) Arco = 1 + contarFutb xs Arco
contarFutb ((Futbolista (Mediocampo)(_)(_)(_)):xs) Mediocampo = 1 + contarFutb xs Mediocampo
contarFutb ((Futbolista (Delantera)(_)(_)(_)):xs) Delantera = 1 + contarFutb xs Delantera
contarFutb ((Futbolista (Defensa)(_)(_)(_)):xs) Defensa = 1 + contarFutb xs Defensa
contarFutb ((_):xs) z = contarFutb xs z



