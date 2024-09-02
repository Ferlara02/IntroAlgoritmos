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

sum_cuad:: [Int] -> Int
sum_cuad [] = 0
sum_cuad (x:xs) = x*x + sum_cuad xs

iga :: Eq a => a -> [a] -> Bool
iga a [] = True
iga a (x:xs) = a == x && (iga a xs)

exp :: Int -> Int -> Int
exp x n = x*n

sumPar :: Int -> Int
sumPar 0 = 0
sumPar n
    | even n = n + sumPar (n-1)
    | odd n = sumPar (n-1)

cuantos :: (Int -> Bool) -> [Int] -> Int
cuantos p xs = length (filter p xs)

--LAB 2

data Dia = Lunes | Martes | Miercoles  | Jueves | Viernes | Sabado | Domingo
    deriving (Eq, Show, Ord, Bounded, Enum)
data Tarea = Trabajar | TrabajarPoco | Salir | Descansar
    deriving (Eq, Show, Ord, Bounded, Enum)

tareaDiaria :: Dia -> Tarea
tareaDiaria Viernes = TrabajarPoco
tareaDiaria Sabado = Salir
tareaDiaria _ = Trabajar


horasTrabajo :: Dia -> Int
horasTrabajo d = case tareaDiaria d of
                        Trabajar -> 8 
                        TrabajarPoco -> 4
                        _ -> 0

type Punto = (Float, Float)
type Radio = Float
data Figura = Circulo Punto Radio | Rectangulo Punto Punto

area :: Figura -> Float
area (Circulo p r) = 3.1416 * r * r
area (Rectangulo p q) = base * altura 
                                where base = fst q - fst p
                                      altura = snd q - snd p



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
minimoElemento' [] = minBound
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

-- Sinonimo
type ManoHabil = PiernaHabil

-- Deportista es un tipo algebraico con constructores parametricos
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
contarFutb ((Futbolista (p) (_) (_) (_)):xs) z = case z of
                                            p -> 1 + contarFutb xs z
                                            _ -> contarFutb xs z 
contarFutb ((_):xs) z = contarFutb xs z

--Lab 10
sonidoNatural :: NotaBasica -> Int
sonidoNatural Do = 0
sonidoNatural Re = 2
sonidoNatural Mi = 4
sonidoNatural Fa = 5
sonidoNatural Sol = 7
sonidoNatural La = 9
sonidoNatural Si = 11

data Alteracion = Bemol | Natural | Sostenido
    deriving Eq 
data NotaMusical = Nota NotaBasica Alteracion
    


sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota b a) = case a of
                            Sostenido -> sonidoNatural b + 1
                            Bemol -> sonidoNatural b - 1
                            Natural -> sonidoNatural b


instance Eq NotaMusical where 
    x == y = sonidoCromatico x == sonidoCromatico y

instance Ord NotaMusical where
    x <= y = sonidoCromatico x <= sonidoCromatico y
