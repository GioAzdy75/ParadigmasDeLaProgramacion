
{-
Ejercicio 1.1. Definir, por comprensión y por recursion, la función:
    A. suma de cuadrados
    B. suma de cuadrados impares
    C. suma de cuadrados pares
-}
-- A
--Funcion
sumaCuadrados :: Num a => [a] -> [a]
sumaCuadrados [] = []
sumaCuadrados (x:xs) = x * x : sumaCuadrados xs
--Comprensión 
sumaCuadrados' :: Num a => [a] -> [a]
sumaCuadrados' xs = [x * x | x <- xs]  

-- B
--Funcion
sumaCuadradosImpares :: [Int] -> [Int]
sumaCuadradosImpares [] = []
sumaCuadradosImpares (x:xs)
                            | (odd x) = x * x:sumaCuadradosImpares xs
                            | otherwise = sumaCuadradosImpares xs
--Comprensión
sumaCuadradosImpares' :: [Int] -> [Int]
sumaCuadradosImpares' xs = [x*x | x <- xs , odd x ]

-- C
--Funcion
sumaCuadradosPares :: [Int] -> [Int]
sumaCuadradosPares [] = []
sumaCuadradosPares (x:xs)
                            | (even x) = x * x:sumaCuadradosPares xs
                            | otherwise = sumaCuadradosPares xs
--Comprensión
sumaCuadradosPares' :: [Int] -> [Int]
sumaCuadradosPares' xs = [x*x | x <- xs , even x ]

-- 1.2

-- A. Lista de números pares hasta el valor n ingresado.
listaParesN :: Int -> [Int]
listaParesN n = [x | x <- [0..n], even x]

--B. Dado dos valores ingresados n y m, generar la lista de valores pares hasta n teniendo en cuenta que solo pueden ser generados aquellos mayores a m
listParesN_M :: Int -> Int -> [Int]
listParesN_M n m = [x | x <- [n..m], even x]

--C. Mostrar los divisores de un número 
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], (n `mod` x) == 0]

--D. Para una lista de números positivos reemplazar cada número x por x copias del mismo.
--Funcion que se copia un numero x las count veces
copia :: Int -> Int -> [Int]
copia _ 0 = []
copia x count = x : copia x (count-1)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:rest) = xs ++ concat' rest

copiasMismo :: [Int] -> [Int]
copiasMismo xs = concat'[(copia x x) | x <- xs ]

--E. Definir una función que permita recibir una tupla de 3 elementos que indique el número de elementos pares
tuplasPares :: (Int,Int,Int) -> Int
tuplasPares (a, b, c) = length [x | x <- [a, b, c], even x]

--F. Devolver una lista de números primos de 1 a n
{-
esPrimo :: Int -> Bool
esPrimo n
  | n <= 1    = False
  | otherwise = null [x | x <- [2..floor (sqrt (fromIntegral n))], n `mod` x == 0]
-}

isPrime k = if k > 1 then null [ x | x <- [2..k - 1], k `mod` x == 0] else False
{-
isPrime k 
        | k > 1 = null [ x | x <- [2..k - 1], k `mod` x == 0] == True
        | otherwise = False
-}
numerosPrimos :: Int -> [Int]
numerosPrimos n = [x | x <- [1..n],isPrime x]


--G. Un tupla de tres elementos (x, y, z) de valores enteros positivos se puede denominar pitagórico si
-- x2 + y2 = z2. Utilizando una lista por comprensión, definir una función pitagoricamente
pitagoricamente :: Int -> [(Int,Int,Int)]
pitagoricamente n = [(x,y,z)| x <- [1..n] , y <- [x..n], z <- [y..n],x^2+y^2 == z^2 ]

{-
H. Un número entero positivo es perfecto si es igual a la suma de todos sus factores, excluyendo el
número mismo. Usando una lista por comprensión.
Definir una función numeroPerfecto :: Int→[Int], que permite devolver la lista de todos los números
perfectos hasta un límite dado
-}
factores :: Int -> [Int]
factores n = [x | x <- [1..n-1], n `mod` x == 0]

esPerfecto :: Int -> Bool
esPerfecto x = x == sum (factores x)

numeroPerfecto :: Int -> [Int]
numeroPerfecto n = [x | x <- [1..n], esPerfecto x]

{-
I. El producto escalar de dos listas de enteros xs e ys de longitud n esta conformada por la suma de los
productos de los enteros correspondientes. Utilizando listas por comprensión, definir una función
que devuelva el producto escalar de dos listas "
-}
dotProduct :: [Int] -> [Int] -> Int
dotProduct xs ys = sum [ x * y | (x,y) <- zip xs ys]

{-
J.
buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
tal que (buscaCrucigrama l pos lon ps) es la lista de las palabras de la lista de palabras ps que tienen
longitud lon y poseen la letra l en la posición pos (comenzando en 0). Por ejemplo,
ghci> buscaCrucigrama 'c' 1 7 ["ocaso", "acabado", "ocupado"]
["acabado","ocupado"]
-}
elementoIesimo :: Int -> String -> Char
elementoIesimo _ [] = ' '
elementoIesimo 0 (p:ps) = p
elementoIesimo pos (p : ps) = elementoIesimo (pos-1) ps  


buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama letter pos long ps = [p | p <- ps , elementoIesimo pos p == letter , length p >= long ]

{-
K. posiciones :: String -> Char -> [Int] tal que (posiciones xs y) es la lista de la posiciones del carácter y en la cadena xs. Por ejemplo,
posiciones "Salamamca" 'a' == [1,3,5,8]
-}

posiciones :: String -> Char -> [Int]
posiciones ps c = [i | (x,i) <- zip ps [0..], x == c ]

{-
L. Se consideran las siguientes reglas de mayúsculas iniciales para los titulos:
la primera palabra comienza en mayúscula y todas las palabras que tienen 4 letras como mínimo empiezan con mayúsculas.
titulo :: [String] -> [String]
tal que (titulo ps) es la lista de las palabras de ps con las reglas de mayúsculas iniciales de los títulos.
Por ejemplo,
ghci> titulo ["eL","arTE","DE","La","proGraMacion"]
["El","Arte","de","la","Programacion"]
-}
tuplaAbecedario :: [(Char,Char)]
tuplaAbecedario = zip ['A'..'Z'] ['a'..'z']

minus :: Char -> [(Char,Char)] -> Char 
minus c ((p1,p2) : ps)
                    | c == p1 = p2
                    | otherwise = minus c ps 

mayus :: Char -> [(Char,Char)] -> Char 
mayus c ((p1,p2) : ps)
                    | c == p2 = p1
                    | otherwise = mayus c ps 

es_minus :: Char -> Bool
es_minus c =  elem c ['a'..'z']

{-
M. La criba de Erastótenes es un método para calcular números primos.
Se comienza escribiendo todos los números desde 2 hasta (supongamos) 100.
El primer número (el 2) es primo.
Ahora eliminamos todos los múltiplos de 2.
El primero de los números restantes (el 3) también es primo.
Ahora eliminamos todos los múltiplos de 3.
El primero de los números restantes (el 5) también es primo . . . y así sucesivamente.
Cuando no quedan números, se han encontrado todos los números primos en el rango fijado.
Definir la función cribaErastotenes que permita calcular los números primos hasta el valor n.
-}


--Ejercicio 1.3. Teniendo en cuenta la definición de la función predefinida foldr, redefinir la suma y el producto

suma' :: [Int] -> Int
suma' xs = foldr (+) 0 xs

suma'' :: [Int] -> Int
suma'' xs = foldr1 (+) xs

mult' :: [Int] -> Int
mult' xs = foldr (*) 1 xs

mult'' :: [Int] -> Int
mult'' xs = foldr1 (+) xs