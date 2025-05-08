-- types
type Fila = [Int]
type Tablero = [Fila]
type Posicion = (Int, Int) -- (fila, columna)
type Camino = [Posicion]

-- ejercicio 1 ----------------------------------------
-- maximo devuelve el numero mas grande en el tablero
maximo :: Tablero -> Int
maximo [] = 0
maximo (x:xs) | maximoFila x >= maximo xs = maximoFila x
              | otherwise = maximo xs  

-- maximoFila me dice el numero mas grande en una fila
maximoFila :: Fila -> Int
maximoFila [] = 0
maximoFila (x:xs) | x >= maximoFila xs = x
                  | otherwise = maximoFila xs  


-- ejercicio 2 ----------------------------------------
-- masRepetido me dice el numero que mas se repitio en el tablero
masRepetido :: Tablero -> Int
masRepetido [] = 0
masRepetido ([]:xs) = masRepetido xs
masRepetido ((x:fila):xs) | repeticionesDe x tablero >= repeticionesDe (masRepetido (fila:xs)) tablero = x
                          | otherwise = masRepetido (fila:xs)  
                        where
                            tablero = (x:fila):xs

-- repeticionesDe me dice cantidad de repeticiones de x
repeticionesDe :: Int -> Tablero -> Int
repeticionesDe x [] = 0
repeticionesDe x ([]:xs) = repeticionesDe x xs
repeticionesDe x ((y:fila):ys)| x == y = 1 + repeticionesDe x (fila:ys)
                              | otherwise = repeticionesDe x (fila:ys)    

-- Ejercicio 3 -------------------------------------

--valoresDeCamino: devuelve el valor en el tablero de cada posicion en la lista camino
valoresDeCamino :: Tablero -> Camino -> [Int]
valoresDeCamino _ [] = []
valoresDeCamino xs (z:zs) = moverseA z xs: valoresDeCamino xs zs

-- moverseA: Te devuelve el valor en el tablero de una posicion
moverseA :: Posicion -> Tablero -> Int
moverseA (1, 1) ((y:ys):xs) = y
moverseA (1, columna) ((y:ys):xs) = moverseA (1, columna - 1) (ys:xs)
moverseA (fila, columna) (ys:xs) = moverseA (fila - 1, columna) xs





-- Ejemplo ----------------------------------------
matriz :: Tablero
matriz = [
    [1,2,3,4,5,6,7,8],
    [2,4,6,8,10,12,14,16],
    [8,7,6,5,4,3,2,1],
    [9,10,4,7,8,2,20,22]
    ]