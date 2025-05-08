-- Ejercicio 9 -----------------
-- funcion divisoresPropios, devuelve todos los divisores de n 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

divisoresPropios :: Int -> [Int]
divisoresPropios 0 = []
divisoresPropios n = [x | x <- [1..n], mod n x == 0, n /= x]

-- aca hago divisores propios pero mas rustico pq no se si me dejan hacer el anterior
dP :: Int -> [Int]
dP n = listaDiv n [1..n]

listaDiv :: Int -> [Int] -> [Int]
listaDiv _ [] = []
listaDiv n (x:xs) | n /= x && mod n x == 0 = x : listaDiv n xs
                  | otherwise = listaDiv n xs  

-- Ejercicio 10 -----------------
-- funcion sonAmigos, devuelve true si dado a y b, a es igual a la suma de los divisores de b y viseversa
sonAmigos :: Int -> Int -> Bool
sonAmigos a b = sumaLista (divisoresPropios a) == b && sumaLista (divisoresPropios b) == a

sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

-- Ejercicio 11 -----------------
-- losPrimerosNperfectos devuelve los primeros n numeros perfectos
losPrimerosNperfectos :: Int -> [Int]
losPrimerosNperfectos 0 = []
losPrimerosNperfectos n = losPrimerosNperfectos (n - 1) ++ [nPerfecto n]

nPerfecto :: Int -> Int
nPerfecto 1 = 6
nPerfecto n = siguientePerfecto (nPerfecto (n - 1))

siguientePerfecto :: Int -> Int
siguientePerfecto n | esPerfecto (n + 1) = n + 1
                    | otherwise = siguientePerfecto (n + 1)   

esPerfecto :: Int -> Bool
esPerfecto n = sumaLista (divisoresPropios n) == n

-- Ejercicio 12 -----------------