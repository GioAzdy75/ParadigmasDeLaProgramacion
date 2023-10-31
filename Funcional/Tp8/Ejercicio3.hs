-- A. sumaPar: dada una lista de pares, devuelve una nueva lista en la que cada elemento es la suma de los elementos de cada par
sumaPar :: [(Int,Int)] -> [Int]
sumaPar [] = []
sumaPar ((a,b) : xs) = [a + b] ++ sumaPar xs

-- B. zipMaximos: dadas dos listas de enteros, 
-- devuelve una lista donde el elemento n es el máximo entre el elemento n de la lista 1 y de la lista 2.
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos xs ys =[max x y | (x, y) <- zip xs ys]

-- C. zipSort: dadas dos listas de enteros de igual longitud, devuelve una lista de pares (min,max), donde
-- min y max son el mínimo y el máximo entre los elementos de ambas listas en la misma posición.
zipSort :: [Int] -> [Int] -> [(Int,Int)]
zipSort xs ys = [(max x y,min x y) | (x,y) <- zip xs ys ]

-- D. takePersonas: dada una lista de Personas [nombre, apellido y fecha de nacimiento] (también declare
-- un tipo de dato Date) ordenada ascende*+ntemente por fecha de nacimiento; y una fecha, devuelve
-- el segmento más largo de la lista con las personas que nacieron antes dicha fecha.1

data Date = Date Int Int Int deriving (Show) -- Año, Mes, Día
data Persona = Persona String String Date  deriving (Show)-- Nombre, Apellido, Fecha de nacimiento

--Funcion Compara fechas
compararFechasMenor :: Date -> Date -> Bool
compararFechasMenor (Date d1 m1 a1) (Date d2 m2 a2)
        | a1 < a2 = True
        | a1 > a2 = False
        | m1 < m2 = True
        | m1 > m2 = False
        | d1 < d2 = True
        | d1 > d2 = False
        | otherwise = False -- Las fechas son iguales

-- Funcion Ordena la Lista de personas por fecha de nacimiento


-- Función para tomar el segmento más largo de personas nacidas antes de una fecha
takePersonas :: [Persona] -> Date -> [Persona]
takePersonas [] _ = []  -- Si la lista está vacía, no hay personas que cumplan la condición
takePersonas (p:ps) fecha
    | compararFechasMenor (fechaNacimiento p) fecha = p : takePersonas ps fecha  -- Agregar a la persona si nació antes
    | otherwise = []  -- Detener la recursión cuando se encuentra la primera persona que no cumple la condición
    where fechaNacimiento (Persona _ _ fn) = fn

-- Variable De Prueba
pruebaPersonas :: [Persona] -- Declaración del tipo y nombre de la variable
pruebaPersonas = [Persona "Pepe" "Rogo" (Date 12 15 01),Persona "Sergio" "Ro" (Date 12 15 10),Persona "Juan" "Domingo" (Date 12 15 21), Persona "Molina" "Rodrigo" (Date 12 15 10), Persona "Felipe" "Strus" (Date 11 21 21)]


-- E. dropPrecio: dada una lista de Pizzas [lista de ingredientes y precio] en orden ascendente por precio,
-- dropPrecio devuelve el segmento más largo de la lista que comienza con la pizza que tiene el menor
-- precio superior a $200

data Pizza = Pizza { ingredientes :: [String], precio :: Double } deriving (Show)

dropPrecio :: [Pizza] -> [Pizza]
dropPrecio [] = []
dropPrecio (pizza:pizzas)
  | precio pizza < 200 = dropPrecio pizzas
  | otherwise = pizza : []

-- Variable De Prueba
pizzas :: [Pizza]
pizzas = [ Pizza ["tomate", "queso"] 150.0
         , Pizza ["pepperoni", "queso"] 180.0
         , Pizza ["champiñones", "queso"] 220.0
         , Pizza ["jamón", "queso"] 250.0
         , Pizza ["piña", "queso"] 300.0
         ]

--  F. takeNombresPersonas: dada una lista de Personas y una fecha devuelve los nombres de las personas
-- incluidas en segmento más largo de la lista con las personas que nacieron antes dicha fecha.


{- G. Definir una función que permita indicar que nota saco el alumno en base al número obtenido en el parcial.
-- a) Utilice una función con guardas
-- b) Utilice una función con patrones
-- c) Utilice un tipo de datos definidos por el usuario con las notas que es posible sacarse. Utilizarlo en el ejercicio
-}

{-
H. En Geometría euclidiana plana recibe el nombre de cuadrante cada una de las cuatro regiones infinitas
en que los ejes del Sistema Cartesiano bidimensional dividen al plano. Definir un tipo de dato
que contenga los cuadrantes, el origen, los ejes cartesianos, es decir los posibles espacios donde un
punto puede estar localizado en un eje cartesiano. Posteriormente defina la función cuadrante que
permita dado dos puntos mostrar el cuadrante a donde pertenece.
-}


{-
I. Definir el tipo de dato Animal y una función que muestre un mensaje respecto al animal.
-}
data Animal = Vaca|Obeja|Cerdo|Mono deriving (Eq)
sonidoAnimal :: Animal -> String
sonidoAnimal animal
            | (animal == Vaca) = "Muuuu"
            | (animal == Cerdo) = "Oing Oing"
            | (animal == Obeja) = "Meeee"
            | (animal == Mono) = "Grito Mono"

