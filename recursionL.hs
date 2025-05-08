-- ejercicios que hicimos en la teorica 17/04 y 2 que aparecen en las diapositivas q no llegamos a hacer !!
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

longitud :: [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs 

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- pertenece: te dice si x pertenece a la lista, en la clase lo hicimos para tipo Int pero asi funciona
--            para cualquier tipo.

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece x [] = False
pertenece x (y:ys) = x == y || pertenece x ys


-- sacarBlancosRepetidos: Saca todos los espacios repetidos en una string
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:y:xs) | x == ' ' && y == ' ' = sacarBlancosRepetidos (y:xs)
                               | otherwise = x:sacarBlancosRepetidos (y:xs) 


-- palabrasConEspaciosRepetidos: toma una lista y crea una nueva con todas las palabras q hay en la lista original
palabrasConEspaciosRepetidos :: [Char] -> [[Char]]
palabrasConEspaciosRepetidos [] = []
palabrasConEspaciosRepetidos xs = primeraPalabra xs:palabras (quitarPalabra xs)

-- primeraPalabra: funcion auxiliar, devuelve la primera palabra que encuentra en un string
primeraPalabra :: [Char] -> [Char]
primeraPalabra [] = []
primeraPalabra (' ':xs) = []
primeraPalabra (x:xs) = x:primeraPalabra xs

-- quitarPalabra: funcion auxiliar, saca la primera palabra que encuentra en un string
quitarPalabra :: [Char] -> [Char] 
quitarPalabra [] = []
quitarPalabra (' ':xs) = xs
quitarPalabra (x:xs) = quitarPalabra xs

-- palabras: esta funcion esta para corregir casos en los que hay espacios de mas, o espacios al principio/final
palabras :: [Char] -> [[Char]]
palabras [] = []
palabras (' ':xs) = palabras xs
palabras xs = palabrasConEspaciosRepetidos (sacarBlancosRepetidos xs)


-- contarPalabras: dada una lista de caracteres devuelve la cantidad de palabras que tiene
contarPalabras :: [Char] -> Int
contarPalabras [] = 0
contarPalabras xs = longitud (palabras xs)

-- Estos ejercicios no los llegamos a hacer pero estaban en las diapositivas


-- Problema palabraMasLarga:

-- palabraMasLarga: devuelve la palabra mas larga en un string
palabraMasLarga :: [Char] -> [Char]
palabraMasLarga [] = []
palabraMasLarga xs = primeraPalabra (nQuitarPalabras (indexMaxEnLista (longitudPalabras xs)) xs)

--longitudPalabras Devuelve una lista con la longitud de cada palabra
longitudPalabras :: [Char] -> [Int]
longitudPalabras [] = []
longitudPalabras xs = longitud (primeraPalabra xs):longitudPalabras (quitarPalabra xs)

-- index2: le das un valor y una lista y te devuelve el index de su primera aparicion
index2 :: Int -> [Int] -> Int
index2 _ [] = 0
index2 n (x:xs) | n == x = 1
                | otherwise = 1 + index2 n xs    

-- Te dice el numero mas grande en una lista
maxEnLista :: [Int] -> Int
maxEnLista [] = 0
maxEnLista (x:xs) | x > maxEnLista xs = x
                  | otherwise = maxEnLista xs  

-- Te dice el index de el numero mas grande en una lista
indexMaxEnLista :: [Int] -> Int
indexMaxEnLista xs = index2 (maxEnLista xs) xs

-- Devuelve el index de la palabra mas larga en una lista
indexPalabraMasLarga :: [Char] -> Int
indexPalabraMasLarga xs = indexMaxEnLista (longitudPalabras xs)

-- sacan las primeras n palabras de un string
nQuitarPalabras :: Int -> [Char] -> [Char]
nQuitarPalabras _ [] = []
nQuitarPalabras n xs| n == 1 = xs
                    | otherwise = nQuitarPalabras (n - 1) (quitarPalabra xs)


-- Problema aplanar:

-- aplanar: agarra una lista de listas y las une en una sola lista.
aplanar :: (Eq t) => [[t]] -> [t]
aplanar [] = []
aplanar [x] = x
aplanar (x:xs) = concatenar x (aplanar xs)

-- concatenar: Une 2 listas
concatenar :: (Eq t) =>[t] -> [t] -> [t]
concatenar [] [] = []
concatenar [] (y:ys) = y:concatenar [] ys
concatenar (x:xs) (y:ys) | xs /= [] = x:concatenar xs (y:ys)
                          | otherwise = x:y:concatenar [] ys 
