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
