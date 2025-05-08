-- Ejercicio 1 --------------------
-- No me acuerdo cual era la consigna y x ahora no la encuentro

-- Ejercicio 2 --------------------

-- le pasas una lista (producto, id, cantidad) y cuanto tiene q haber de como minimo de cada producto
-- y te devuelve una lista con todos los productos que tienen menos de ese minimo
hayQueReponerProducto :: [(String, Int, Int)] -> Int -> [(String, Int)]
hayQueReponerProducto [] _ = []
hayQueReponerProducto ((prod, id, cant):xs) min |cant > min = hayQueReponerProducto xs min
                                                |otherwise = (prod, id) : hayQueReponerProducto xs min    

-- Ejercicio 3 --------------------

-- crea una lista con todas las columnas que al sumar sus valores dan cero, si no es vacia da True
hayColumnasSumaCero :: [[Int]] -> Bool
hayColumnasSumaCero xs = [x | x <- [1..(cantidadColumnas xs)], sumaColumna x xs == 0] /= []

-- Suma todos los valores en una columna
sumaColumna :: Int -> [[Int]] -> Int
sumaColumna _ [] = 0
sumaColumna col (x:xs) = valorColumna col x + sumaColumna col xs

-- Funcion auxiliar de suma columna, devuelve el valor en columna n de la primera fila
valorColumna :: Int -> [Int] -> Int
valorColumna 1 (x:xs) = x
valorColumna n (x:xs) = valorColumna (n-1) xs

-- te dice la cantidad de columnas que hay en una matriz
cantidadColumnas :: [[Int]] -> Int
cantidadColumnas ([]:xs) = 0
cantidadColumnas ((x:xs):ys) = 1 + cantidadColumnas (xs:ys)

-- Ejercicio 4 --------------------
primerosKNumerosAritmeticos :: Int -> [Int]
primerosKNumerosAritmeticos k = [kNumeroAritmetico x | x <- [1..k]]

kNumeroAritmetico :: Int -> Int
kNumeroAritmetico 1 = 1
kNumeroAritmetico k = siguienteAritmetico (kNumeroAritmetico (k - 1))

siguienteAritmetico :: Int -> Int
siguienteAritmetico n | esAritmetico (n + 1) = n + 1
                      | otherwise = siguienteAritmetico (n + 1) 
                    
esAritmetico :: Int -> Bool
esAritmetico n = mod sumaDivisores cantidadDivisores == 0
            where
                sumaDivisores = sumaLista (divisoresDe n)
                cantidadDivisores = longitud (divisoresDe n)

divisoresDe :: Int -> [Int]
divisoresDe n = [x | x <- [1..n], mod n x == 0]

sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

longitud :: [Int] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- ejemplo
matriz :: [[Int]] 
matriz = [
    [-1,2,3,4,5,6,7,8],
    [2,4,6,8,10,12,14,16],
    [8,7,6,5,4,3,2,1],
    [-9,10,4,7,8,2,20,22]
    ]