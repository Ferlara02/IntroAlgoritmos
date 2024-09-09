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

equal :: Int -> Int
equal n = n*1
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

factorial2 :: Int -> Int 
factorial2 n = productoria2 [1..n] equal


--filter ya está predefinida en Haskell, sólo la defino nuevamente para que se entienda su funcionamiento.
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p [] = []
filter1 p (x:xs) | p x = x : filter1 p xs 
                | otherwise = filter1 p xs
---

multiplica :: [Int] -> Int
multiplica [] = 1
multiplica (x:xs) = x * multiplica xs

multiplicaPrimos :: [Int] -> Int
multiplicaPrimos (x:xs) = multiplica (filter (esPrimo) (x:xs))


--Lab 12

--usando recursión:
primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA x [] = []  -- Caso base: si la lista está vacía, retorna una lista vacía
primIgualesA x (y:ys)
  | x == y    = y : primIgualesA x ys
  | otherwise = []  -- Si el primer elemento no es igual a x, detiene la recursión

--usando takeWhile:
primIgualesA2 :: Eq a => a -> [a] -> [a]
primIgualesA2 x xs = takeWhile (== x) xs

--Lab 13

--a) (recursión):
primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales [x] = [x]
primIguales (x:y:xs) 
        | x == y = x : primIguales (y:xs)
        | otherwise = [x]

-- b) (sin recursión) 

primIguales2 :: Eq a => [a] -> [a]
primIguales2 [] = []
primIguales2 (x:xs) = primIgualesA2 x (x:xs)

--Lab 14: Cuantificador general
cuantGen :: (b -> b -> b) -> b -> [a] -> (a -> b) -> b
cuantGen op z [] t = z
cuantGen op z (x:xs) t = op (t x) (cuantGen op z xs t)

--reescritura de las funciones del lab 7 usando cuantGen

paraTodo3 :: [a] -> (a -> Bool) -> Bool
paraTodo3 (x:xs) t = cuantGen (&&) (True) (x:xs) (t)

existe3 :: [a] -> (a -> Bool) -> Bool
existe3 (x:xs) t = cuantGen (||) (False) (x:xs) (t)

sumatoria3 :: [a] -> (a -> Int) -> Int 
sumatoria3 (x:xs) t = cuantGen (+) (0) (x:xs) (t)

productoria3 :: [a] -> (a -> Int) -> Int 
productoria3 (x:xs) t = cuantGen (*) (1) (x:xs) (t)

--Lab 15
primQueCumplen :: [a] -> (a -> Bool) -> [a]
primQueCumplen (x:xs) p = takeWhile p (x:xs)

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

--Sinonimos de tipo

type Altura = Int
type NumCamiseta = Int

--Tipos algebraicos sin parametros (aka enumerados)
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
contarFut ((Futbolista Arco _ _ _):xs) Arco = 1 + contarFut xs Arco
contarFut ((Futbolista Mediocampo _ _ _):xs) Mediocampo = 1 + contarFut xs Mediocampo
contarFut (Futbolista Delantera _ _ _):xs) Delantera = 1 + contarFut xs Delantera
contarFut ((Futbolista (Defensa _ _ _):xs) Defensa = 1 + contarFut xs Defensa
contarFut ((_):xs) z = contarFut xs z



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

prod :: [Int] -> [Int] -> Int
prod [] ys = 0
prod xs [] = 0
prod (x:xs) (y:ys) = (x*y) + (prod (xs) (ys))


gSumAnt :: Int -> [Int] -> Bool
gSumAnt n [] = False
gSumAnt n (x:xs) = x == n || gSumAnt (n+x) xs

--CLASE 9/9
--LAB 11
dividir :: Int -> Int -> Maybe Int
dividir x 0 = Nothing
dividir x y = Just (x `div` y)


primerElemento :: [a] -> Maybe a 
primerElemento [] = Nothing
primerElemento (x:xs) = Just x

--LAB 12
data Cola = VaciaC | Encolada Deportista Cola
    deriving Show

atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada d c) = Just c

encolar :: Deportista -> Cola -> Cola
encolar d VaciaC = (Encolada d VaciaC)
encolar d (Encolada p c) = Encolada p (encolar d c)

busca :: Cola -> Zona -> Maybe Deportista
busca VaciaC z = Nothing
busca (Encolada (Futbolista Arco b s d) c) Arco = Just (Futbolista Arco b s d)
busca (Encolada (Futbolista Defensa b s d) c) Defensa = Just (Futbolista Defensa b s d)
busca (Encolada (Futbolista Mediocampo b s d) c) Mediocampo = Just (Futbolista Mediocampo b s d)
busca (Encolada (Futbolista Delantera b s d) c) Delantera = Just (Futbolista Delantera b s d)
busca (Encolada (_) c) z = busca c z


--Lab 13

data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b)
    deriving Show
--instanciacion de a y b:
type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String
type GuiaTelefonica = ListaAsoc String Int

--funciones lab 13
laLong :: ListaAsoc a b -> Int 
laLong Vacia = 0
laLong (Nodo a b c) = 1 + laLong c 

laConcat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
laConcat Vacia Vacia = Vacia
laConcat Vacia (Nodo a b c) = Nodo a b c
laConcat (Nodo a b c) Vacia = Nodo a b c
laConcat (Nodo a b c) (Nodo d e f) = Nodo a b (laConcat c (Nodo d e f))

laAgregar :: Eq a => ListaAsoc a b -> a -> b -> ListaAsoc a b
laAgregar Vacia a b = Nodo a b Vacia
laAgregar (Nodo a b c) d e 
    | a == d = Nodo a e c
    | otherwise = Nodo a b (laAgregar c d e)

data Palabra = PVacia | Agregar Char Palabra

mostrar :: Palabra -> String
mostrar PVacia = ""
mostrar (Agregar l p) = l : mostrar p

data ListaInt = LVacia | ConsI Int ListaInt
--esto se corresponde a los constructores de lista ya definidos : y []  

--TIPOS RECURSIVOS Y POLIMORFICOS

data Lista a = Vacia2 | Cons a (Lista a)
--es como ListaInt solo que polimorfica, recibe el tipo de datos que sea


data Clase = Teorico | Taller
hayClase :: Dia -> Maybe Clase
hayClase Lunes = Just Taller
hayClase Martes = Just Teorico
hayClase Jueves = Just Teorico
hayClase _ = Nothing 


actividad :: Dia -> String
actividad d = case hayClase d of
                Nothing -> "Tareas"
                Just Teorico -> "Teorico"
                Just Taller -> "Taller"
