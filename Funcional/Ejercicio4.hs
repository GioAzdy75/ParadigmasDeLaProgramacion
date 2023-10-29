
-- A. suma, que suma todos los elementos de una lista de números
suma :: Num a => [a] -> a
suma [] = 0
suma (x:xs) = x + suma xs

-- B. alguno, que devuelve True si algún elemento de una lista de valores booleanos es True, y False en caso contrario
alguno :: [Bool] -> Bool
alguno [] = False
alguno (x:xs) = (x == True) || alguno xs

-- C. todos, que devuelve True si todos los elementos de una lista de valores booleanos son True, y False en caso contrario
todos :: [Bool] -> Bool
todos [] = True
todos (x:xs) = (x == True) && todos xs 

-- D. codigos, que dada una lista de caracteres, devuelve la lista de sus ordinales
codigos :: [Char] -> [Int]
codigos [] = []
codigos (x:xs) = fromEnum x : codigos xs

-- E. restost, que calcula la lista de los restos de la división de los elementos de una lista de números dada por otro número dado
restost :: [Int] -> Int -> [Int]
restost [] _ = []
restost (x:xs) divisor = (x `mod` divisor) : restost xs divisor

-- F. Incrementar todos los elementos de una lista de enteros.
incrementar :: [Int] -> [Int]
incrementar [] = []
incrementar (x : xs) = x+1 : incrementar xs

-- G. cuadrados, que dada una lista de números, devuelva la lista de sus cuadrados
cuadrados :: Num a => [a] -> [a]
cuadrados [] = []
cuadrados (x : xs) = x*x : cuadrados xs

-- H. longitudes, que dada una lista de listas, devuelve la lista de sus longitudes
longitudes :: Num a => [[a]] -> [Int]
longitudes [] = []
longitudes (x:xs) = length x : longitudes xs

-- I. orden, que dada una lista de pares de números, 
-- devuelve la lista de aquellos pares en los que la primera componente es menor que el triple de la segunda
orden :: [(Int,Int)] -> [(Int,Int)]
orden [] = []
orden ((a,b):xs)
                | a < (3 * b) = (a,b) : orden xs
                | otherwise = orden xs

-- J. pares, que dada una lista de enteros, devuelve la lista de los elementos pares
pares :: [Int] -> [Int]
pares [] = []
pares (x:xs)
            | even x = x : pares xs
            | otherwise = pares xs

-- K. letras, que dada una lista de caracteres, devuelve la lista de aquellos que son letras (minúsculas o mayúsculas)
letras :: [Char] -> [Char]
letras [] = []

-- L. masDe, que dada una lista de listas xs y un número n, devuelve la lista de aquellas listas de xs con longitud mayor que n