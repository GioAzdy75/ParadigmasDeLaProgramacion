--A. Calcular El volumen de una esfera
volumenEsfera :: Float -> Float
volumenEsfera r = 4/3 * pi * r^3

--B. La suma de monedas tal que (sumaCoins a b c d e) es la suma de los pesos correspondientes a: a
-- monedas de 1 centavo, b de 5 centavos, c de 10 centavos, d de 50 centavos, e de 1 peso.
sumaCoins :: (Int, Int, Int, Int, Int) -> Float
sumaCoins (a, b, c, d, e) = fromIntegral a * 0.01 + fromIntegral b * 0.05 + fromIntegral c * 0.1  + fromIntegral d * 0.5  + fromIntegral e

-- C. Incrementar todos los elementos de una tupla de tres enteros.
incrementarTuplas :: (Int,Int,Int) -> (Int,Int,Int)
incrementarTuplas (a,b,c) = (a+1,b+1,c+1)

-- D. Calcular el cuadrado de un número.
cuadradoNumero :: Float -> Float
cuadradoNumero x = x * x

-- E. calcular el valor de un número elevado a la 4 utilizando la función del inciso anterior
elevado_4 :: Float -> Float
elevado_4 x = cuadradoNumero(cuadradoNumero x)

-- F. Calcular la media aritmética de tres valores numéricos.
mediaAritmetica :: Float -> Float -> Float -> Float
mediaAritmetica a b c = (a + b + c) / 3 

-- G. Calcular el máximo entre 3 números, puede utilizar la función predefinida max
maximo3Numeros :: Float -> Float -> Float -> Float
maximo3Numeros a b c = max (max a b) c

-- H. Calcular el máximo entre 6 números, puede utilizar la función predefinida max
maximo6Numeros :: Float -> Float -> Float -> Float -> Float -> Float -> Float
maximo6Numeros a b c d e f = max (maximo3Numeros a b c) (maximo3Numeros d e f) 

-- I. Definir la función area tal que la función área a b c es el área de un triangulo de lados a,b,c.
--areaTriangulo :: Float -> Float -> Float -> Float
--areaTriangulo a b _ = a * b / 2

-- J. Definir una función tal que reciba un par (x, y) y retorne el cuadrante, puede informar mediante
--mensaje o devolviendo el número de cuadrante. El par no esta sobre los ejes x ni y.

-- K. La función igualesTres verifica que tres elementos x, y y z son iguales.
igualesTres :: Eq a => a -> a -> a -> Bool
igualesTres a b c = a == b && b == c
-- igualesTres a b c = (a == b) == c # Porque Error? :Rta (a==b) devuelve un Bool y luego con el otro == estamos comparando un bool con un Num y da error

-- L. La función diferentesTres verifica que tres elementos x, y y z son diferentes.
diferentesTres :: Eq a => a -> a -> a -> Bool
diferentesTres a b c = a /= b && a /= c && b/= c

-- M. La función igualesCuatro verifica que tres elementos v, x, y y z son iguales utilizando la función definida en el punto D.
igualesCuatro :: Eq a => a -> a -> a -> a -> Bool
igualesCuatro a b c d = igualesTres a b c  && igualesTres d b c

-- N. Definir las raíces de una ecuación de segundo grado

-- Ñ. Determinar si un año es bisiesto

-- O. Definir el operador XOR
xor :: Eq a => a -> a -> Bool
xor a b = a /= b



-- G. Definir la función nIndex tal que nIndex l n es elemento enésimo de l, empezando a numerar con el 0.
--nIndex [] _ = error "Lista Vacia"
nIndex :: [a] -> Int -> a
nIndex l n  | l == [] = error "Lista Vacia"
            | n < 0 = error "Indice Fuera de Rango"
            | n >= length l = error "Indice Fuera de Rango" 
            
nIndex (x:_) 0 = x
nIndex (_:xs) n = nIndex xs (n-1)
