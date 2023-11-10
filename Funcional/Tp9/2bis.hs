{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Char
import Data.List
--TP N2 - Haskell

--EJERCICIO 1

-- 1?

--1.2: DEFINIR LOS SIGUIENTES ENUNCIADOS POR COMPRENSION

{-A. Lista de números pares hasta el valor n ingresado.-}
pares :: Int -> [Int]
pares n = [x | x <- [0..n], even x]

{-B. Dado dos valores ingresados n y m, generar la lista de valores pares hasta n teniendo en cuenta que
solo pueden ser generados aquellos mayores a m-}
paresInt :: Int -> Int -> [Int]
paresInt m n = [x | x <- [m..n], even x]

{-C. Mostrar los divisores de un número-}
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

{-D. Para una lista de números positivos reemplazar cada número x por x copias del mismo.-}
copia :: [Int] -> [Int]
copia xs = [y | x <- xs, y <- replicate x x]

{-E. Definir una función que permita recibir una tupla de 3 elementos que indique el número de elementos pares........FALTA-}
cantidad :: (Int,Int,Int) -> Int
cantidad (x,y,z) = sum [1 | even x,even y, even z]

{-F. numeros primos de 1 a n-}
primos :: Int -> [Int]
primos n = [x | x <- [2 .. n], divisores x == [1,x]]

{-G. Generar las ternas pitagoricas hasta n-}
pitagoricamente :: Int -> [(Int,Int,Int)]
pitagoricamente n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2, x >= y] 

{-H. Hallar numeros perfectos hasta n-}
numeroPerfecto :: Int -> [Int]
numeroPerfecto n = [x | x <- [1..n], sum (divisores x) == 2*x]

{-I. Producto escalar entre dos listas-}
prodEscalar :: [Int] -> [Int] -> Int
prodEscalar xs ys = sum [x*y | (x,y) <- zip xs ys]

prodEscalar' :: [Int] -> [Int] -> Int
prodEscalar' xs ys = sum (zipWith (*) xs ys)

{-J. buscaCrucigrama-}
position :: Char -> Int -> String -> Bool
position x _ [] = False
position x n (y:xs)
    |x == y && n == 0 = True
    |x /= y && n == 0 = False
    |otherwise = position x (n-1) xs

buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama l pos lon ps = [x | x <- ps, length x == lon, position l pos x == True]

{-K. tal que (posiciones xs y) es la lista de la posiciones del carácter y en la cadena xs. Por ejemplo,-}
posiciones :: String -> Char -> [Int]
posiciones p c = [x | x <- [0..length p], position c x p == True]

{-L. convertir una lista de strings en un titulo-}

lower = map toLower
firstT w = toUpper (head w) : lower (tail w)

titulo :: String -> String
titulo [] = []
titulo w
    |length w > 2 = toUpper (head w)  : lower (tail w)
    |otherwise = lower w

toTitulo :: [String] -> [String]
toTitulo (w:ws) = firstT w : map titulo ws


{-M. criba de Eratostones-}
criba :: [Int] -> [Int]
criba [] = []
criba (p:xs) = p : criba [x | x <- xs, x `mod` p > 0] 

cribaEratostenes :: Int -> [Int]
cribaEratostenes n = criba [2..n]

--1.3: DEFNIR USANDO FOLDR

--Producto
producto' :: Num a => [a] -> a
producto' xs = foldr (*) 1 xs

--Suma
suma' :: Num a => [a] -> a
suma' xs = foldr (+) 0 xs



--1.4: FUNCIONES DE ALTO ORDEN
--Usando map,filter,takewhile,dropwhile,iterate,zipwith


{-A. numero de elementos alineados en dos listas-}
trueOne :: Bool -> Int
trueOne True = 1
trueOne False = 0

alineados :: Eq a => [a] -> [a] -> Int
alineados xs ys = sum (map trueOne (zipWith (==) xs ys))


{-B. Crea una función que toma una lista y otra función como entrada, aplicar esa función a cada elemento
de esa lista y devolver la nueva lista.-}
map' :: [a] -> (a -> b) -> [b]
map' [] _ = []
map' (x:xs) f = f x : map' xs f

{-C. Filtrar los números de una lista que sean menores a 5-}
gt5 :: [Int] -> [Int]
gt5 xs = filter (>=5) xs

{-D.Crear una función que multiplique todos los elementos mayores a 0 × 2-}
mul2 :: Ord a => Num a => [a] -> [a]
mul2 [] = []
mul2 (x:xs)
    |x > 0 = 2*x : mul2 xs
    |otherwise = x : mul2 xs


--1.5 Implementar usando funciones de orden superior

{-A. Definir la función de orden superior dosVeces tal que repita una función y un argumento dos veces.-}

{-B. Definir la función que imprima cada elemento de una matriz de forma en espiral transversal hacia
el centro de la matriz.-}
{-type Offset = Int
type Size = Int
type Currsize = Int
newOff :: Offset -> Bool -> Offset
newOff x b
    |b == True = x^2 - 2
    |b == False = 2 - x^2

ruta :: Size -> Currsize -> Offset -> Bool -> [Int]
ruta size off s b
    | b == True = [off..off+s-1] ++ [off+s+size-1,off+s+2*size-1..off + s^2 -1]
    | b == False = [off,off-1..off-s+1]  ++ [off-2*s1,off-3*s+1..off - s^2 + 1]-}

{-C. De numero a palabras-}
num2Word :: Int -> String
num2Word n
    |n == 0 = "cero"
    |n == 1 = "uno"
    |n == 2 = "dos"
    |n == 3 = "tres"
    |n == 4 = "cuatro"
    |n == 5 = "cinco"
    |n == 6 = "seis"
    |n == 7 = "siete"
    |n == 8 = "ocho"
    |n == 9 = "nueve"
    |otherwise = num2Word (n `div` 10) ++ ['-'] ++ num2Word (n `mod` 10)


--EJERCICIOS INTEGRADORES

--Parentesis, corchetes y llaves balanceados
type Stack a = [a]

push :: a -> Stack a -> Stack a
push x s = x:s

pop :: Stack a -> Stack a
pop [] = []
pop s = tail s

head' :: [Char] -> Char
head' [] = ' '
head' (x:xs) = x


parentesis :: String -> Stack Char -> Bool
parentesis [] emptyStack = True
parentesis (x:xs) s
    |x == '(' || x == '[' || x == '{' = parentesis xs (push x s)
    |x == ')' && head' s == '(' = parentesis xs (pop s)
    |x == ']' && head' s == '[' = parentesis xs (pop s)
    |x == '}' && head' s == '{' = parentesis xs (pop s)
    |otherwise = False




--Periodo orbital

data Planeta = Mercurio | Venus | Tierra | Marte | Jupiter | Saturno | Urano | Neptuno deriving(Enum,Eq,Show)

periodo :: Planeta -> Float
periodo x
    |x == Mercurio = 0.2408467
    |x == Venus = 0.61519726 
    |x == Tierra = 1
    |x == Marte =  1.8808158
    |x == Jupiter = 11.862615
    |x == Saturno = 29.447498
    |x == Urano = 84.016846
    |x == Neptuno = 164.79132

edad :: Float -> Planeta -> Float
edad x p = (x / 31557600) / periodo p


--Contar apariciones de cada palabra en una frase
remove :: [Char] -> String -> String
remove _ [] = []
remove c (x:xs)
    |x `elem` c = remove c xs
    |otherwise = x: remove c xs 

wordList :: String -> [String]
wordList s =   map lower (words s)

cleanList :: String -> [String]
cleanList s = map (remove "-=,") (wordList s)

count :: String -> [String] -> Int
count x xs = length [y | y <- xs, y == x]

contarPalabras :: String -> [(String,Int)]
contarPalabras xs = [(x,count x (cleanList xs)) | x <- cleanList xs]

removeDupes :: (Ord a) => [a] -> [a]
removeDupes = map head . group . sort


cuentaPalabras :: String -> [(String,Int)]
cuentaPalabras xs = (removeDupes . contarPalabras) xs

--ISBN
type ISBN = String

undash :: ISBN -> String
undash = filter (/= '-')

toIntList :: ISBN -> [Int]
toIntList s = map digitToInt (undash s)

isValid :: ISBN -> Bool
isValid s = sum (zipWith (*) (toIntList s) [10,9..1]) `mod` 11 == 0


--Irish pub
type Bebida = String
type Cantidad = Int

letra :: Cantidad -> Bebida -> [IO()]
letra 0 s = [putStrLn ("No hay mas botellas de " ++ s ++ " en la pared, no hay mas botellas de " ++ s), putStrLn ("Ve a la tienda y compra mas. 99 botellas de " ++ s ++ " en la pared")]
letra 1 s = putStrLn ("1 botella de " ++ s ++" en la pared, 1 botella de "++ s ++ ". Una se cayo, no mas botellas de " ++ s ++ " en la pared") : letra 0 s
letra n s = putStrLn (show n ++ " botellas de " ++ s ++ " en la pared, " ++ show n ++ " botellas de " ++ s ++ ". Una se cayo y quedaron " ++ show (n-1) ++ " botellas de " ++ s ++" en la pared") : letra (n-1) s

printLetra :: Cantidad -> Bebida -> IO()
printLetra n s = sequence_ (letra n s)


--R2D2
type Ubicacion = (Int,Int)
type Instrucciones = String
data Orientacion = Norte | Este | Sur | Oeste deriving(Show,Enum,Ord,Eq)
data Robo = Robo {posicion :: Ubicacion, direccion :: Orientacion} deriving Show

turnRight :: Robo -> Robo
turnRight (Robo a dir) = Robo a (toEnum ((fromEnum dir + 1) `mod` 4))

turnLeft :: Robo -> Robo
turnLeft (Robo a dir) = Robo a (toEnum ((fromEnum dir - 1) `mod` 4))

avanzar :: Robo -> Robo
avanzar (Robo (x,y) dir)
    |dir == Norte = Robo (x,y+1) dir
    |dir == Sur = Robo (x,y-1) dir
    |dir == Este = Robo (x+1,y) dir
    |dir == Oeste = Robo (x-1,y) dir

modelar :: Robo -> Instrucciones -> Robo
modelar (Robo a dir) [] = Robo a dir
modelar (Robo a dir) (x:xs) 
    |x == 'A' = modelar (avanzar (Robo a dir)) xs
    |x == 'D' = modelar (turnRight (Robo a dir)) xs
    |x == 'L' = modelar (turnLeft (Robo a dir)) xs


-----------
intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala x ys]

permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones (x:xs) = concat [intercala x ys | ys <- permutaciones xs]


combinaciones :: Int -> [a] -> [[a]]
combinaciones 0 _ = [[]]
combinaciones _ [] = []
combinaciones n (x:xs) = map (x :) (combinaciones (n-1) xs) ++ combinaciones n xs

variaciones n x = concat (map permutaciones (combinaciones n x))