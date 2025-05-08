-- Problema 1.a: Implementar la funcion parcial f :: Integer -> Integer definida por extension de la siguiente manera:
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use foldr" #-}
f :: Int -> Int
f 1 = 8 
f 4 = 131
f 16 = 16

-- 1.b: especificar e implementar la funcion parcial g :: Integer -> Integer, tal que
g :: Int -> Int
g 8 = 16 
g 16 = 4
g 131 = 1

-- 1.c: partir de las funciones definidas implementar las funciones parciales fog y gof
fog :: Int -> Int
fog n = f (g n)

gof :: Int -> Int
gof n = g (f n)


-- Problema 2: Especificar e implementar las siguientes funciones, incluyendo su signatura:
-- 2.a: calcula el valor absoluto de un numero entero.
absoluto :: Int -> Int
absoluto n | n > 0 = n  -- usamos pipes con otherwise, casi seguro que con patern matching no se puede
           |otherwise = -n 

-- 2.b: devuelve el maximo entre el valor absoluto de dos numeros enteros.
maximoabsoluto :: Int -> Int -> Int
maximoabsoluto n m | an > am = an
                   | otherwise = am
                   where
                    an = absoluto n
                    am = absoluto m 

-- 2.c: devuelve el maximo entre tres numeros enteros. 
maximo3 :: Int -> Int -> Int -> Int
maximo3 i j k | i >= j && i >= k = i
              | j >= i && j >= k = j 
              | k >= i && k >= j = k  

-- 2.d: dados dos numeros racionales, decide si alguno es igual a 0 (resolverlo con y sin pattern matching).
algunoEsCeroP :: Int -> Int -> Bool
algunoEsCeroP 0 _ = True
algunoEsCeroP _ 0 = True
algunoEsCeroP n m = False

algunoEsCero :: Int -> Int -> Bool
-- Aca uso guardas pero descubri que no son necesarias
-- algunoEscero n m = n == 0 || m == 0 (hace exactamente lo mismo)
algunoEsCero n m | n == 0 || m == 0 = True
                 | otherwise = False   
                
-- 2.e: dados dos números racionales, decide si ambos son iguales a 0 (resolverlo con y sin pattern matching).
ambosSonCeroP :: Int -> Int -> Bool
ambosSonCeroP 0 0 = True
ambosSonCeroP n m = False

ambosSonCero :: Int -> Int -> Bool
ambosSonCero n m = n == 0 && m == 0 
 
{-- 2.f: dados dos números reales, indica si están relacionados por la relación de equivalencia en R cuyas
clases de equivalencia son: (−∞, 3], (3, 7] y (7, ∞) --}
enMismoIntervalo :: Int -> Int -> Bool
enMismoIntervalo n m |(n <= 3 && m <= 3) = True
                     |(n > 7 && m > 7) = True
                     |(n>3 && n<=7 && m>3 && m<=7) = True
                     |otherwise = False
                    
-- 2.g: dados tres números enteros calcule la suma sin sumar repetidos (si los hubiera).
comparacion3 :: Int -> Int -> Int -> [Int]
comparacion3 i j k | i == j && i == k = [i]
                   | i == j = [i, k]
                   | i == k = [i, j]
                   | j == k = [i, j]
                   | otherwise = [i, j, k] 

sumaL :: [Int] -> Int
sumaL [] = 0
sumaL (x:xs) = x + sumaL xs

sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos i j k = sumaL (comparacion3 i j k) 

-- 2.h: dados dos números naturales, decide si el primero es múltiplo del segundo.
esMultiploDe :: Int -> Int -> Bool
esMultiploDe n m |n <= 0 || m <= 0 = False      
                 |mod n m == 0 = True
                 |otherwise = False

-- 2.i: dado un número entero, extrae su dı́gito de las unidades.
digitoUnidades :: Int -> Int
digitoUnidades n = mod n 10

-- 2.j: dado un número entero mayor a 9, extrae su dı́gito de las decenas.
digitoDecenas :: Int -> Int
digitoDecenas n = mod n 100

{-- Problema 3: Implementar una función estanRelacionados :: Integer -> Integer -> Bool
                requiere: {a /= 0 && b /= 0}
                asegura: {(res=True) <-> (a * a + a * b * k = 0 para k Int /= 0)} --}

estanRelacionados :: Int -> Int -> Bool
estanRelacionados n m | mod n m == 0 = True 
                      |otherwise = False

-- Problema 4: Especificar e implementar las siguientes funciones utilizando tuplas para representar pares y ternas de num
-- 4.a: calcular el producto interno entre 2 tuplas de RxR.

productoInterno :: (Int, Int) -> (Int, Int) -> Int
productoInterno (a, b) (c, d) = a*d + c*b

{-- 4.b: dadas dos tuplas de R × R, decide si cada coordenada de la primera tupla es menor a la coordenada
correspondiente de la segunda tupla. --}

esParMenor :: (Int, Int) -> (Int, Int) -> Bool
esParMenor (a, b) (c, d) = (a > c) && (b > d)  

-- 4.c: calcula la distancia euclı́dea entre dos puntos de R2 .

distancia :: (Double, Double) -> (Double, Double) -> Double
distancia (a, b) (c, d) = sqrt ((a - c)^2 + (b - d)^2)

-- 4.d: dada una terna de enteros, calcula la suma de sus tres elementos.

sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (a, b, c) = a + b + c

-- 4.e: dada una terna de números enteros y un natural, calcula la suma de los elementos de la terna que son múltiplos del número natural.

sumarSoloMultiplos :: (Int, Int, Int) -> Int -> Int
sumarSoloMultiplos (a, b, c) d = sumaL [x | x <- [a, b, c], mod x d == 0]

-- 4.f: dada una terna de enteros, devuelve la posición del primer número par si es que hay alguno, o devuelve 4 si son todos impares.

posPrimerPar :: (Int, Int, Int) -> Int
posPrimerPar (a, b, c) | even a = 0
                       | even b = 1
                       | even c = 2
                       | otherwise = 4

-- 4.g: a partir de dos componentes, crea un par con esos valores.

crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

-- 4.f: invierte los elementos del par pasado como parámetro.

invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)

-- Problema 5: Implementar la funcion todosMenores (especificacion en el pdf)

todosMenores :: (Int, Int, Int) -> Bool
todosMenores ( a, b, c) = ( fInterna a > gInterna a ) && ( fInterna b > gInterna b) && ( fInterna c > gInterna c)

fInterna :: Int -> Int
fInterna n | n <= 7 = n^2
           | n > 7 = 2 * n - 1

gInterna :: Int -> Int
gInterna n | mod n 2 == 0 = div n 2
           | otherwise = 3 * n + 1  
                         
-- problema 6: funcion bisiesto
type Anio = Int
type EsBisiesto = Bool

bisiesto :: Anio -> EsBisiesto
bisiesto a |(mod a 400 == 0) = True
           |(mod a 100 /= 0) && (mod a 4 == 0) = True
           |otherwise = False 

-- problema 7: funcion distanciaManhattan
type Punto3D = (Float, Float, Float)
distanciaManhattan :: Punto3D -> Punto3D -> Float
distanciaManhattan (a, b, c) (d, e, f) = absoluto2 (a - d) + absoluto2 (b - e) + absoluto2 (c - f) 

absoluto2 :: Float -> Float
absoluto2 a | a < 0 = -a 
            | otherwise = a    

-- problema 8: Implementar la función comparar :: Integer -> Integer -> Integer

-- Primero voy a implementar la otra funcion que me da en la especificacion.
sumaUltimosDosDigitos ::  Int -> Int
sumaUltimosDosDigitos n = mod (absoluto n) 10 + mod (div (absoluto n) 10) 10

comparar :: Int -> Int -> Int
comparar a b | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b = 1
             | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = -1
             | otherwise = 0

-- Primer guia de haskell Terminada
-- observaciones:
--      No tener variables es un poco raro, pero lo mas raro es no tener estructuras de control normales para usar
--      La cantidad de veces que intente iterar sobre algun valor y acordarme que tengo que usar recursion es incontable
--      Tengo que empezar a implementar la recursion mas, lo de currificacion ni lo intente pero porque no lo vi util ahora
--      el lenguaje se ve interesante, tiene sus restricciones medio boludas pero en si es divertido. Tiene lo suyo, hay que
--      ver que onda en el futuro. Lo que me gusta mas que nada es el enfoque que le estamos dando a la modularidad        