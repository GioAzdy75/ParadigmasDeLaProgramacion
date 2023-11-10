
import Data.List
--Ejercicios Integradores

{-Ejercicio 2.1. Dada una cadena que contiene corchetes [], llaves , paréntesis () o cualquier combinación
de estos, se debe verificar que todos y cada uno de los pares coincidan y estén anidados correctamente.-}

--Parentesis, corchetes y llaves balanceados
-- Importa el módulo Data.List para usar la función `elemIndex`.
import Data.List (elemIndex)

-- Función principal para verificar la anidación de los corchetes, llaves y paréntesis.
verificarAnidacion :: String -> Bool
verificarAnidacion = verificar []

-- Función auxiliar que realiza la verificación de anidación.
verificar :: [Char] -> String -> Bool
verificar [] [] = True  -- Todos los corchetes, llaves y paréntesis están anidados correctamente.
verificar _ [] = False  -- Alguno de los corchetes, llaves o paréntesis no tiene su par correspondiente.
verificar pila (c:resto)
    | c `elem` "({[" = verificar (c:pila) resto  -- Abre un nuevo paréntesis, corchete o llave.
    | c `elem` ")}]" = case pila of
                            [] -> False  -- No hay ningún paréntesis, corchete o llave que coincida.
                            (top:restoPila) -> if coincide top c
                                                then verificar restoPila resto
                                                else False
    | otherwise = verificar pila resto  -- Otros caracteres, ignorar.

-- Función auxiliar que verifica si dos caracteres forman un par válido.
coincide :: Char -> Char -> Bool
coincide '(' ')' = True
coincide '{' '}' = True
coincide '[' ']' = True
coincide _ _ = False

{-Ejercicio 2.2. Si dijeramos que alguien tiene una edad de 1.000.000.000 segundos tambien podriamos
poder decir que tiene 31.69 años terrestres.El periodo orbital es el tiempo que le toma a un astro recorrer su órbita y tiene un valor en años de
nuestro planeta. Teniendo en cuenta los siguientes periodos orbitales de nuestro sistema solar:-
Mercurio: periodo orbital equivale a 0.2408467 años terrestres
Venus: periodo orbital equivale a 0.61519726 años terrestres
Tierra: periodo orbital equivale a 1.0 años terrestres, 365.25 días terrestres, or 31557600 segundos
Marte: periodo orbital equivale a 1.8808158 años terrestres
Jupiter: periodo orbital equivale a 11.862615 años terrestres
Saturno: periodo orbital equivale a 29.447498 años terrestres
Urano: periodo orbital equivale a 84.016846 años terrestres
Neptuno: periodo orbital equivale a 164.79132 años terrestres
Calcular en base a una cantidad de segundos y un planeta mostrar los años terrestres equivalente
edad Marte 1000000000 = 16.84
-}
data Planeta = Mercurio | Venus | Tierra | Jupiter | Saturno | Urano | Neptuno | Marte deriving(Eq)
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

edad :: Planeta -> Int -> Float
edad p tiempo = (fromIntegral tiempo / 31557600) / periodo p 

{-
Ejercicio 2.3. A partir de una frase, contar las apariciones de cada palabra en dicha frase. Algunas acotaciones
sobre la conformación de palabras:
Una palabra puede ser un número compuesto por uno o más dígitos ASCII (es decir, 0 o bb232a3)
Una palabra simple compuesta de una o más letras ASCII (es decir, a o ellos) O
Una contracción de dos palabras simples unidas por un solo apóstrofe (es decir, es o son)
Al contar palabras, puede asumir las siguientes reglas:
El recuento no distingue entre mayúsculas y minúsculas (es decir, “Usted”,“usted” y “USTED” son 3
usos de la misma palabra)
El recuento no está ordenado; las pruebas deben ignorar cómo se ordenan las palabras y los recuentos
Aparte del apóstrofe en una contracción, se ignoran todas las formas de puntuación
Las palabras se pueden separar por cualquier forma de espacio en blanco (es decir, \t, \n, “”)
Por ejemplo, para la frase “Mi hija es un dolor de cabeza, diosss”. el conteo va a ser:
ghci> contarPalabras "Mi hija es un dolor de cabeza, diosss"
Para completar este ejercicio, debe implementar la función contarPalabras, que toma un texto y devuelve
cuántas veces aparece cada palabra. Se puede utilizar la siguiente firma como base(no es obligatorio).
contarPalabras :: String -> [(String, Int)]
-}


-- Si le colocas una palabra que no esta dentro de la lista entra en loop infinito, no nos importa porque siempre comprobamos que la palabra esta dentro antes de usarla
sumarPalabra :: [(String, Int)] -> String -> [(String, Int)] -> [(String, Int)]
sumarPalabra axs p ((x1,x2):xs)
                            | p == x1 = (x1,x2+1) : (xs ++ axs)
                            | otherwise = sumarPalabra [(x1,x2)] p xs

encontrarPalabra :: String -> [(String,Int)] -> Bool
encontrarPalabra [] [] = False
encontrarPalabra _ [] = False
encontrarPalabra p ((x1,_):xs)
                                | p == x1 = True
                                | otherwise = encontrarPalabra p xs

contarPalabrasWrap :: String -> [(String, Int)] -> String -> [(String, Int)]
contarPalabrasWrap _ l [] = l
contarPalabrasWrap palabra lista (y:ys) 
                    | y == ' ' || y == '\n' || y == '\t' = if encontrarPalabra palabra lista then contarPalabrasWrap [] (sumarPalabra [] palabra lista) ys 
                                 else contarPalabrasWrap [] ((palabra,1):lista) ys
                    | otherwise = contarPalabrasWrap (palabra ++ [y]) lista ys --Agrego a la Lista

contarPalabras :: String -> [(String, Int)]
contarPalabras a = contarPalabrasWrap [] [] (a ++ " ") 

{-
Ejercicio 2.4. El proceso de verificación ISBN-10 se utiliza para validar los números de identificación de
libros. Estos normalmente contienen guiones y su codificación así: 3-598-21508-8
El formato ISBN-10 tiene 9 dígitos (0 a 9) más un carácter de verificación (ya sea un dígito o solo una
X). En el caso de que el carácter de verificación sea una X, esto representa el valor ’10’. Estos se pueden
comunicar con o sin guiones, y se puede verificar su validez mediante la siguiente fórmula:
(x1 * 10 + x2 * 9 + x3 * 8 + x4 * 7 + x5 * 6 +
x6 * 5 + x7 * 4 + x8 * 3 + x9 * 2 + x10 * 1) mod 11 == 0
Si el resultado es 0, entonces es un ISBN-10 válido; de lo contrario, no es válido.
Si tomamos como ejemplo el ISBN-10 3-598-21508-8. Lo evaluamos mediante la fórmula y obtenemos:
(3 * 10 + 5 * 9 + 9 * 8 + 8 * 7 + 2 * 6 + 1 * 5 + 5 * 4 + 0 * 3 + 8 * 2 + 8 * 1) mod 11 == 0
Dado el resultado = 0, esto significa que nuestro ISBN es válido.
Se solicita que dado una cadena, el programa debe verificar si dicha cadena es un ISBN-10 válido. El
programa debe poder verificar ISBN-10 con y sin utilizar guiones.
-}
convertChrInt :: Char -> Int
convertChrInt x| x == '0' = 0 | x == '1' = 1 | x == '2' = 2 | x == '3' = 3 | x == '4' = 4 | x == '5' = 5 | x == '6' = 0 | x == '6' = 0 | x == '7' = 7
                |x == '8' = 8 |x == '9' = 9 |x == 'X' = 10

extraerStringAEntero :: String -> Int -> [Int]
extraerStringAEntero [] _ = []
extraerStringAEntero (x:xs) p
                            | x == '-' = extraerStringAEntero xs p
                            | otherwise = (convertChrInt x) * p  : extraerStringAEntero xs (p-1)

validarCodigo :: [Int] -> Bool
validarCodigo enteros = mod (foldl (+) 0 enteros) 11 == 0

isISBN :: String -> Bool
isISBN code = length (listaEnteros) == 10  && validarCodigo listaEnteros
            where listaEnteros = extraerStringAEntero code 10

{-
Ejercicio 2.5. Escribir la letra de una canción conocida en los bares irlandeses: 99 Botellas de cerveza(ron,
whisky, manaos, lo que más le guste) en la pared. Tenga en cuenta que no todos los versos son idénticos.
99 botellas de cerveza en la pared, 99 botellas de cerveza. Una se cayó y quedaron 98 botellas de
cerveza en la pared.
98 botellas de cerveza en la pared, 98 botellas de cerveza. Una se cayó y quedaron 97 botellas de
cerveza en la pared.
97 botellas de cerveza en la pared, 97 botellas de cerveza. Una se cayó y quedaron 96 botellas de
cerveza en la pared.
96 botellas de cerveza en la pared, 96 botellas de cerveza. Una se cayó y quedaron 95 botellas de
cerveza en la pared.
...
...
...
2 botellas de cerveza en la pared, 2 botellas de cerveza. Una se cayó, 1 botella de cerveza en la
pared.
1 botella de cerveza en la pared, 1 botella de cerveza. Una se cayó, no más botellas de cerveza en la
pared.
No hay más botellas de cerveza en la pared, no más botellas de cerveza.
Ve a la tienda y compra más, 99 botellas de cerveza en la pared.
-}

letraCancion :: Int -> String
letraCancion 1 = "1 botella de cerveza en la pared, 1 botella de cerveza. Una se cayó, no más botellas de cerveza en la pared. No hay más botellas de cerveza en la pared, no más botellas de cerveza. Ve a la tienda y compra más, 99 botellas de cerveza en la pared."
letraCancion x = toEnum x : verso ++ "," ++ toEnum x : verso ++ "Una se cayó, " ++ verso
                where verso = "botellas de cerveza en la pared"

{-
Ejercicio 2.6. Ayudar a evaluar el funcionamiento de un lindo R2-D2. Se necesita hacer una prueba de
fábrica de robots necesita un programa para verificar los movimientos del robot.
Los robots tienen tres movimientos posibles:
doblar a la derecha
doblar a la izquierda
avanzar
Los robots se colocan en una cuadrícula hipotéticamente infinita donde el inicio de los ejes es en
la esquina superior oeste, con una dirección particular (Norte, Este, Sur u Oeste) en un conjunto de
coordenadas (x, y), por ejemplo, (3,8), con coordenadas que aumentan hacia el norte y el este.
Luego, el robot recibe una serie de instrucciones, momento en el cual la prueba verifica la nueva
posición del robot y en qué dirección apunta.
Una cadena de letras "DAALAL"significa:
Girar a la derecha
Avanzar dos veces
Girar a la izquierda
Avanzar una vez
Gira a la izquierda una vez más
Un robot comienza en (7, 3) ubicado hacia el norte. Luego, ejecutando esta secuencia de instrucciones
queda en (9, 4) hacia el oeste.
Se pide modelar un robot que se mueva desde una posición (x,y) a otro par de coordenadas mediante
un conjunto de instrucciones.
Recomendaciones:
Se aconseja crear el tipo de datos Robo y ubicación e implementar las siguientes funciones:
• crearRobot
• simular
• coordenadas
• girar a la izquierda
• girar a la derecha
NOTA: Las funciones a definir son de ayuda, si ud. desea implementar más o menos funciones puede
hacerlo tranquilamente.
-}

type Coordenada = (Int , Int)
data Direccion = Norte | Este | Sur  | Oeste deriving (Show,Enum)
data Robot = Robot Coordenada Direccion deriving (Show)

crearRobot :: Coordenada -> Direccion -> Robot
crearRobot cord dir = Robot cord dir

girarDerecha :: Direccion -> Direccion
girarDerecha Oeste = Norte
girarDerecha dir = succ dir

girarIzquierda :: Direccion -> Direccion
girarIzquierda Norte = Oeste
girarIzquierda dir = pred dir

avanzar :: Direccion -> Coordenada -> Coordenada
avanzar Norte (x,y) = (x,y+1)
avanzar Sur (x,y) = (x,y-1)
avanzar Este (x,y) = (x+1,y)
avanzar Oeste (x,y) = (x-1,y)

--DAALAL
simular :: Robot -> String -> Robot
simular robot [] = robot
simular (Robot coordenada dir) (p:ps) | p == 'D' = simular (Robot coordenada (girarDerecha dir)) ps 
                                     | p == 'L' = simular (Robot coordenada (girarIzquierda dir)) ps
                                     | p == 'A' = simular (Robot (avanzar dir coordenada) dir) ps