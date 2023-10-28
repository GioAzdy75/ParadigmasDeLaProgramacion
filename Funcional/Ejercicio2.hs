-- A. Definir la función divisores tal que divisores x es la lista de los divisores de x
-- divisores 4 == [1,-1,2,-2,4,-4]
-- divisores (-6) == [1,-1,2,-2,3,-3,6,-6]


-- B. Dada una cantidad de segundos, devuelve la cantidad de horas, minutos y segundos equivalente.
convertirHora :: Int -> (Int, Int, Int)
convertirHora x = (hora, minutos, segundos)
  where
    hora = div x 3600
    minutos = div (x - 3600 * hora) 60
    segundos = (x - 3600 * hora - 60 * minutos)

-- C. Definir la función primo tal que primo x se verifica si x es primo
esDivisible :: Integer -> Integer -> Bool
esDivisible x y = x `mod` y == 0

esPrimo :: Integer -> Integer -> Bool
esPrimo x y
  | y <= 1 = True
  | esDivisible x y = False
  | otherwise = esPrimo x (y - 1)

primo :: Integer -> Bool
primo x
  | x <= 1 = False
  | otherwise = esPrimo x (x - 1)

-- D. Definir una función que devuelva una listas de primos hasta un valor N
primosHastaN :: Integer -> [Integer]
primosHastaN n = [x | x <- [2 .. n], primo x]

-- E. Definir la función tomar tal que tomar n l es la lista de los n primeros elementos de l.
tomar :: Int -> [a] -> [a]
tomar 0 _ = []
tomar _ [] = []
tomar n (x : xs) = x : tomar (n - 1) xs

-- F. Redefinir la función tomarMientras tal que tomarMientras p l es la lista de los elementos iniciales de l que verifican el predicado p
tomarMientras :: (a -> Bool) -> [a] -> [a]
tomarMientras _ [] = []
tomarMientras p (x : xs)
  | p x = x : tomarMientras p xs
  | otherwise = []

-- G. Definir la función nIndex tal que nIndex l n es elemento enésimo de l, empezando a numerar con el 0.
nIndex :: Int -> [a] -> a
nIndex n (x : xs)
  | n < 0 = error "Indice Fuera de Rango"
  | null xs = error "Indice Fuera de Rango"
  | n == 0 = x
  | otherwise = nIndex (n - 1) xs

-- H. Redefinir la función elem tal que elem e l se verifica si e es un elemento de l.
elementoLista :: Eq a => a -> [a] -> Bool
elementoLista e (x : xs) = (e == x) || elementoLista e xs

-- I. Definir la función que convierte el número decimal en su correspondiente número binario. Los números binarios deben almacenarse como una lista
decimalABinario :: Int -> [Int]
decimalABinario 0 = [0] -- Caso base: 0 en decimal es 0 en binario.
decimalABinario 1 = [1] -- Caso base: 1 en decimal es 1 en binario.
decimalABinario n
  | n < 0 = error "Número negativo no es binario" -- Manejo de números negativos.
  | otherwise = decimalABinario (n `div` 2) ++ [n `mod` 2]

-- J. Definir un nuevo tipo de datos para un número complejo y algunas operaciones básicas como la suma y la multiplicación de números complejos
data Complejo = Complejo Float Float deriving (Show)

sumacomplejos :: Complejo -> Complejo -> Complejo
sumacomplejos (Complejo a b) (Complejo c d) = (Complejo (a+c) (b+d))

multiplicacioncomplejos :: Complejo -> Complejo -> Complejo
multiplicacioncomplejos (Complejo a b) (Complejo c d) = sumacomplejos (Complejo (a*c) (a*d)) (Complejo (b*d) (b*c))

-- K. Definir al menos 10 colores definiendo su propio tipo de datos. Ingresar la lista y evaluar si la lista tiene los colores predefinidos por el usuario.

data Color = Rojo | Verde | Azul | Amarillo | Naranja | Rosa | Morado | Blanco | Negro | Gris deriving (Show,Eq)

coloresPredefinidos :: [Color]
coloresPredefinidos = [Rojo, Azul, Amarillo]

coloresenLista :: [Color] -> [Color] -> Bool
coloresenLista [] _ = True
coloresenLista (x:xs) y = x `elem` y && coloresenLista xs y


-- L. Teniendo en cuenta lo definido en el punto anterior, armar una función que permita ingresar 2 colores y devolver el nuevo color que se genera. Puede ser un string el nuevo color.
generarColor :: Color -> Color -> Color
generarColor Azul Amarillo = Verde
generarColor Rojo Amarillo = Naranja
generarColor Rojo Azul = Morado

-- M. Crear una función ocurrencias, que toma un elemento y una lista y devuelve el número de ocurrencias del elemento en la lista.
ocurrencias :: Eq a => a -> [a] -> Int
ocurrencias _ [] = 0  -- Caso base: lista vacía, no hay ocurrencias.
ocurrencias e (x:xs)
                    | (e == x) = 1 + ocurrencias e xs
                    | otherwise = ocurrencias e xs 

-- N. Escribir un programa que genere todas las permutaciones de n objetos diferentes (se ingresan los datos como una lista).
permutaciones :: Eq a => [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = [x : p | x <- xs, p <- permutaciones (filter (/= x) xs)]



-- Ñ. Crear una lista que contenga todos los enteros dentro de un rango dado
crearListaRango :: Int -> Int -> [Int]
crearListaRango n m
    | n<0 && m<0 = error "Indice Invalido"
    | n > m = error "Indice Invalido"
    | otherwise = [x | x <- [n..m]]

-- O. Generar las combinaciones de K objetos distintos elegidos de los N elementos de una lista
combinaciones :: Int -> [a] -> [[a]] 
combinaciones 0 _ = [[]]
combinaciones _ [] = []
combinaciones k (x:xs) = [ x : resto | resto <- combinaciones (k - 1) xs ] ++ combinaciones k xs
--combinaciones k (x:xs) = combinaciones k xs ++ map (x:) (combinaciones (k-1) xs)

-- P. mapToSucesor: dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
mapToSucesor :: [Int] -> [Int]
mapToSucesor [] = []
mapToSucesor xs = [succ x | x <- xs]

-- Q. filtrarPositivos: dada una lista de enteros, devuelve una lista con los elementos que son positivos.
filtrarPositivos :: [Int] -> [Int]
filtrarPositivos [] = []
filtrarPositivos xs = [x | x <- xs , x > 0]

-- R. reversa’: dada una lista de enteros, devuelve la lista con los mismos elementos de atrás para adelante.
reversa :: [Int] -> [Int]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]