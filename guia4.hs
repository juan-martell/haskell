{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Use foldr" #-}

-- problema 1: Implementar la función fibonacci:: Integer ->Integer que devuelve el i-ésimo número de Fibonacci.
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Lista de n numeros de la funcion
listfib :: Int -> [ Int ]
listfib n = [fibonacci x|x <- [0..n]]

-- Funcion de funciones, es el generico de la lista anterior
listGen :: (Int -> Int) -> Int  -> [Int]
listGen f n = [f x|x <- [0..n]]

-- Suma de todos los elementos de una lista
sumaL :: [ Int ] -> Int
sumaL [] = 0
sumaL (x:xs) = x + sumaL xs

-- maximo dentro de una lista
maximo :: (Ord a)=> [a] -> a
maximo [] = undefined
maximo [x] = x
maximo (x:xs) | x > maximo xs = x
              | otherwise = maximo xs  


-- Problema 2: Implementar una función parteEntera :: Float ->Integer / requiere: {x >= 0}, asegura: {res <= x < res + 1}

-- No lo pude resolver yo :(, pero esta solucion esta buena, lo q hace es llamarse a si misma
-- recursivamente siempre restando 1 del valor inicial, hasta que llegue al rango entre 0 y 1
-- ahi devuelve 0. En cada iteracion suma 1 y despues devuelve la suma de todas las iteraciones
parteEntera :: Float -> Integer
parteEntera n | (0 <= n) && (n < 1) = 0
              | otherwise = 1 + parteEntera(n-1)


--problema 4: Especificar e implementar la función sumaImpares :: Integer ->Integer que dado n ∈ N sume los primeros n números impares.
imparGen :: Int -> Int
imparGen 0 = 0
imparGen n = n*2 - 1

sumaImpares :: Int -> Int
sumaImpares 0 = 0
sumaImpares n = imparGen n + sumaImpares (n - 1)

-- Lo mismo pero sin recursion
sumaImpares2 :: Int -> Int
sumaImpares2 n = sumaL (listGen imparGen n)

-- problema 3: Especificar e implementar la función esDivisible :: Integer ->Integer ->Bool que dados dos números naturales determinar si el primero es divisible por el segundo. No está permitido utilizar las funciones mod ni div.

esDivisible :: Int -> Int -> Bool
esDivisible _ 0 = False
esDivisible n m = cociente n m 1 * m == n 

cociente :: Int -> Int -> Int -> Int
cociente n m x | x*m > n = x - 1
               | otherwise = cociente n m (x + 1)

-- Esto iba para otra solucion q tenia pensada pero no use, igual lo dejo
esEntero :: (Num a,Eq a, Ord a) => a -> Bool
esEntero n | (f n > 0) && (f n < 1) = False
           | (f n < 0) && (f n > -1) = False 
           | f n == 0 || f n == 1 = True


f :: (Num a,Eq a, Ord a) => a -> a
f n | n < 0 = neg n
    | n > 0 = pos n
    | otherwise = 0 

neg :: (Num a,Eq a, Ord a) => a -> a
neg n | (n <= 0) && (n > -1) = n
      | otherwise = neg ( n + 1 )  

pos :: (Num a,Eq a, Ord a) => a -> a
pos n | (n >= 0) && (n < 1) = n
      | otherwise = pos (n - 1)  

-- problema 5: Implementar la función medioFact :: Integer ->Integer que dado n ∈ N calcula n!! = n (n − 2)(n − 4) · · · .
medioFact :: Int -> Int
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n*medioFact(n - 2)

-- problema 6: Implementar la función todosDigitosIguales :: Integer ->Bool que determina si todos los dı́gitos de un número natural son iguales.

cantidadDigitos :: Int -> Int
cantidadDigitos 0 = 1
cantidadDigitos n | div n 10 == 0 = 1 
                  |n > 0 = 1 + cantidadDigitos (div n 10)
                  |n < 0 = 1 + cantidadDigitos (div n (-10))

-- problema 7: Implementar la función iesimoDigito :: Integer ->Integer ->Integer que dado un n ∈ Z mayor o igual a 0 y un i ∈ Z mayor o igual a 1 menor o igual a la cantidad de dı́gitos de n, devuelve el i-ésimo dı́gito de n.

iesimoDigito :: Int -> Int -> Int
iesimoDigito n m = mod (div n (10^(cantidadDigitos n - m))) 10


-- Problema 6: Implementar la función todosDigitosIguales :: Integer ->Bool que determina si todos los dı́gitos de un número natural son iguales.
todosDigitosIguales :: Int -> Bool
todosDigitosIguales n | cantidadDigitos n == 1 = True
                      | iesimoDigito n 1 == iesimoDigito n 2 = todosDigitosIguales (mod n (10^(cantidadDigitos n - 1)))
                      | otherwise = False


-- Problema 8: Especificar e implementar la función sumaDigitos :: Integer ->Integer que calcula la suma de dı́gitos de un número natural. Para esta función pueden utilizar div y mod.

sumaDigitos :: Int -> Int
sumaDigitos 0 = 0
sumaDigitos n = mod n 10 + sumaDigitos (div n 10)

-- Problema 9: Especificar e implementar una función esCapicua :: Integer ->Bool que dado n ∈ N≥0 determina si n es un número capicúa.

esCapicua :: Int -> Bool
esCapicua n | n < 10 = True
            | iesimoDigito n 1 == iesimoDigito n (cantidadDigitos n) = esCapicua (sacarPyU n) 
            | otherwise = False


sacarPyU :: Int -> Int
sacarPyU n | n < 10 = 0
           | otherwise = mod (div n 10) (10^(cantidadDigitos n - 2)) 

-- ejercicio 10:
-- a)
f1 :: Int -> Int
f1 0 = 1
f1 n = 2^n + f1 (n - 1)

-- b)
f2 :: Int -> Int -> Int
f2 1 q = q 
f2 n q = q^n + f2 (n - 1) q



-- ejercicio 16: Recordemos que un entero p > 1 es primo si y sólo si no existe un entero k tal que 1 < k < p y k divida a p.

-- a) Implementar menorDivisor :: Integer ->Integer que calcule el menor divisor (mayor que 1) de un natural n pasado como parámetro.

menorDivisor :: Int -> Int
menorDivisor 0 = 0
menorDivisor n = primerDivisor n 2
      where 
            primerDivisor x y | mod x y == 0 = y  
                              | otherwise = primerDivisor x (y+1)

-- b) Implementar la función esPrimo :: Integer ->Bool que indica si un número natural pasado como parámetro es primo.

esPrimo :: Int -> Bool
esPrimo n = menorDivisor n == n

-- c) Implementar la función sonCoprimos :: Integer ->Integer ->Bool que dados dos números naturales indica si no tienen algún divisor en común mayor estricto que 1.

sonCoprimos :: Int -> Int -> Bool
sonCoprimos n m = not (compartenDiv n m 2)
 

compartenDiv n m i | esDivisor (ndivisor n i) m = True
        | n == ndivisor n i = False
        | otherwise = compartenDiv n m (i + 1)   

-- nos dice si un numero esta entre los divisores de m
esDivisor :: Int -> Int -> Bool
esDivisor n m = aux n m 2
            where
                  aux n m i | n == ndivisor m i = True
                            | m == ndivisor m i = False   
                            | otherwise = aux n m (i + 1)  


-- nos dice el divisor numero n de m
ndivisor :: Int -> Int -> Int
ndivisor 0 _ = 0
ndivisor _ 0 = 0
ndivisor m n | n > m = 0
             |otherwise = siguienteDivisor m (ndivisor m (n - 1)) 


-- le pasas m y n y te da el divisor de m que le sigue a n
siguienteDivisor :: Int -> Int -> Int
siguienteDivisor m n | m <= n = m 
                     | mod m (n+1) == 0 = n + 1 
                     | otherwise = siguienteDivisor m (n + 1)   


-- d) Implementar la función nEsimoPrimo :: Integer ->Integer que devuelve el n-ésimo primo (n ≥ 1). Recordar que el primer primo es el 2, el segundo es el 3, el tercero es el 5, etc.

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = siguientePrimo (nEsimoPrimo (n - 1))

siguientePrimo :: Int -> Int
siguientePrimo 1 = 2
siguientePrimo n | esPrimo (n + 1) = n + 1
                 | otherwise = siguientePrimo (n + 1) 


{--
nEsimoPrimo 3 = siguientePrimo (nEsimoPrimo 2)
-> siguientePrimo (siguientePrimo (nEsimoPrimo 1))
-> siguientePrimo (siguientePrimo (2))
-> siguientePrimo 3 
-> 5
--}

{-- 
Ejercicio 17. Implementar la función esFibonacci :: Integer ->Bool según la siguiente especificación:
problema esFibonacci (n: Z) : B {
requiere: { n ≥ 0 }
asegura: { resultado = true ↔ n es algún valor de la secuencia de Fibonacci definida en el ejercicio 1}
}
--}
esFibonacci :: Int -> Bool
esFibonacci 0 = True
esFibonacci n = aux n 1
            where
                  aux n i | n == fibonacci i = True
                          | n < fibonacci i = False    
                          | otherwise = aux n (i + 1)    

{-- Ejercicio 18. Implementar una función mayorDigitoPar :: Integer ->Integer según la siguiente especificación:
problema mayorDigitoPar (n: N) : N {
requiere: { True }
asegura: { resultado es el mayor de los dı́gitos pares de n. Si n no tiene ningún dı́gito par, entonces resultado es -1.
}
}
--}
-- llama a recursion con 2 valores, n y su cant de digitos.
mayorDigitoPar :: Int -> Int
mayorDigitoPar 0 = 0
mayorDigitoPar n = recursion n (cantidadDigitos n)
             where -- compara cada digito de n, si el digito es par y >= a todos los anteriores lo devuelve    sino usa el siguiente 
                  recursion n i | i == 0 = 0 -- aca termina la recursion siempre
                                | digitoN n i >= recursion n (i - 1) && mod (digitoN n i) 2 == 0 = digitoN n i
                                | otherwise = recursion n (i - 1)          


digitoN :: Int -> Int ->Int
digitoN n 0 = 0
digitoN n i |i > cantidadDigitos n = undefined 
            |n > 0 = div (mod n (10^(cantidadDigitos n - i + 1))) (10^(cantidadDigitos n - i))
            |n < 0 = div (mod n (-(10^(cantidadDigitos n - i + 1)))) (-(10^(cantidadDigitos n - i)))


