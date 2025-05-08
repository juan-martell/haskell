-- Ejercicio 1: Definir las siguientes funciones:
-- 1. longitud :: [t] -> Int , que dada una lista devuelve su cantidad de elementos.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use even" #-}

longitud :: [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs 


{-- 2. ultimo :: [t] -> t según la siguiente especificación:
problema ultimo (s: seq⟨T ⟩) : T {
requiere: { |s| > 0 }
asegura: { resultado = s[|s| − 1] }
} --}

ultimo :: (Eq t) => [t] -> t
ultimo (x:xs) | xs /= [] = ultimo xs
              | otherwise = x  

{--
3. principio :: [t] -> [t] según la siguiente especificación:
problema principio (s: seq⟨T ⟩) : seq⟨T ⟩ {
requiere: { |s| > 0 }
asegura: { resultado = subseq(s, 0, |s| − 1) }
}
--}

{--
4. reverso :: [t] -> [t] según la siguiente especificación:
problema reverso (s: seq⟨T ⟩) : seq⟨T ⟩ {
requiere: { T rue }
asegura: { resultado tiene los mismos elementos que s pero en orden inverso.}
}
--}
reverso1 :: [t] -> [t]
reverso1 [] = []
reverso1 (x:xs) = reverso1 xs ++ [x]

-- solucion sin ++, no se me ocurrio a mi
reverso :: [t] -> [t]
reverso xs = aux xs []
    where 
        aux [] lista = lista
        aux (x:xs) lista = aux xs (x:lista)

-- Ejercicio 2. Definir las siguientes funciones sobre listas:
{--
1. pertenece :: (Eq t) => t -> [t] -> Bool según la siguiente especificación:
problema pertenece (e: T , s: seq⟨T ⟩) : B {
requiere: { True }
asegura: { resultado = true ↔ e ∈ s }
} --}

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs) | e == x = True
                   | otherwise = pertenece e xs 

{--
2. todosIguales :: (Eq t) => [t] -> Bool, que dada una lista devuelve verdadero sı́ y solamente sı́ todos sus ele-
mentos son iguales.
--}
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:xs) | x == y = todosIguales (y:xs)
                      | otherwise = False 

{--3. todosDistintos :: (Eq t) => [t] -> Bool según la siguiente especificación:
problema todosDistintos (s: seq⟨T ⟩) : B {
requiere: { True }
asegura: { resultado = false ↔ existen dos posiciones distintas de s con igual valor }
} --}

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos (x:xs) | pertenece x xs = False
                      | otherwise = todosDistintos xs 

{--
4. hayRepetidos :: (Eq t) => [t] -> Bool según la siguiente especificación:
problema hayRepetidos (s: seq⟨T ⟩) : B {
requiere: { True }
asegura: { resultado = true ↔ existen dos posiciones distintas de s con igual valor }
}
--}

hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos [x] = False
hayRepetidos (x:xs) | pertenece x xs = True
                      | otherwise = hayRepetidos xs 

{--
5. quitar :: (Eq t) => t -> [t] -> [t], que dados un entero x y una lista xs, elimina la primera aparición de x en
la lista xs (de haberla).
--}

quitar :: (Eq t) => t -> [t] -> [t]
quitar n [] = []
quitar n (x:xs) | n == x = xs
                | otherwise = x : quitar n xs

{--
6. quitarTodos :: (Eq t ) => t -> [t] -> [t], que dados un entero x y una lista xs, elimina todas las apariciones
de x en la lista xs (de haberlas). Es decir:
problema quitarTodos (e: T , s: seq⟨T ⟩) : seq⟨T ⟩ {
requiere: { T rue }
asegura: { resultado es igual a s pero sin el elemento e. }
}
--}

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos n [] = []
quitarTodos n (x:xs) | n == x = quitarTodos n xs
                | otherwise = x : quitarTodos n xs

{--
7. eliminarRepetidos :: (Eq t) => [t] -> [t] que deja en la lista una única aparición de cada elemento, eliminando
las repeticiones adicionales.
--}

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)

{--
8. mismosElementos :: (Eq t) => [t] -> [t] -> Bool, que dadas dos listas devuelve verdadero sı́ y solamente sı́
ambas listas contienen los mismos elementos, sin tener en cuenta repeticiones, es decir:
problema mismosElementos (s: seq⟨T ⟩, r: seq⟨T ⟩) : B {
requiere: { T rue }
asegura: { resultado = true ↔ todo elemento de s pertenece r y viceversa}
}
--}

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos xs ys = todosPertenecen xs ys && todosPertenecen ys xs

todosPertenecen :: (Eq t) => [t] -> [t] -> Bool
todosPertenecen [] _ = True
todosPertenecen (x:xs) ys | pertenece x ys = todosPertenecen xs ys
                          | otherwise = False  

{--
9. capicua :: (Eq t) => [t] -> Bool según la siguiente especificación:
problema capicua (s: seq⟨T ⟩) : B {
requiere: { T rue }
asegura: { (resultado = true) ↔ (s = reverso(s)) }
}
Por ejemplo capicua [á’,’c’, ’b’, ’b’, ’c’, á’] es true , capicua [á’, ’c’, ’b’, ’d’, á’] es false.
--} 

capicua :: (Eq t) => [t] -> Bool
capicua xs = xs == reverso xs

-- Ejercicio 3. Definir las siguientes funciones sobre listas de enteros:

{--
1. sumatoria :: [Int] -> Int según la siguiente especificación:
problema sumatoria (s: seq⟨Z⟩) : Z {
requiere: { T rue }
asegura: { resultado = sumatoria de todos los elementos de la lista
--}

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

{-- 
2. productoria :: [Int] -> Int según la siguiente especificación:
problema productoria (s: seq⟨Z⟩) : Z {
requiere: { T rue }
asegura: { resultado = producto de todos los elementos de la lista }
}
--}

productoria :: [Int] -> Int
productoria [] = 0
productoria [x] = x
productoria (x:xs) = x * productoria xs

{--
3. maximo :: [Int] -> Int según la siguiente especificación:
problema maximo (s: seq⟨Z⟩) : Z {
requiere: { |s| > 0 }
asegura: { resultado ∈ s ∧ todo elemento de s es menor o igual a resultado }
}
--}

maximo :: [Int] -> Int
maximo [] = 0
maximo (x:xs) | x >= maximo xs = x
              | otherwise = maximo xs  

{--
4. sumarN :: Int -> [Int] -> [Int] según la siguiente especificación:
problema sumarN (n: Z, s: seq⟨Z⟩) : seq⟨Z⟩ {
requiere: { T rue }
asegura: {|resultado| = |s| ∧ cada posición de resultado contiene el valor que hay en esa posición en s sumado
n}
}
--}

sumarN :: (Num t) => t -> [t] -> [t]
sumarN n xs = [x + n| x <- xs]

sumarM :: Int -> [Int] -> [Int]
sumarM _ [] = []
sumarM m (x:xs) = (m + x) : sumarM m xs

{--
5. sumarElPrimero :: [Int] -> [Int] según la siguiente especificación:
problema sumarElPrimero (s: seq⟨Z⟩) : seq⟨Z⟩ {
requiere: { |s| > 0 }
asegura: {resultado = sumarN (s[0], s) }
}
Por ejemplo sumarElPrimero [1,2,3] da [2,3,4]
--}

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero [] = []
sumarElPrimero (x:xs) = sumarM x (x:xs)

{--
6. sumarElUltimo :: [Int] -> [Int] según la siguiente especificación:
problema sumarElUltimo (s: seq⟨Z⟩) : seq⟨Z⟩ {
requiere: { |s| > 0 }
asegura: {resultado = sumarN (s[|s| − 1], s) }
}
Por ejemplo sumarElUltimo [1,2,3] da [4,5,6]
--}

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo [] = []
sumarElUltimo xs = sumarM (ultimo xs) xs

{--
7. pares :: [Int] -> [Int] según la siguiente especificación:
problema pares (s: seq⟨Z⟩) : seq⟨Z⟩ {
requiere: { T rue }
asegura: {resultado sólo tiene los elementos pares de s en el orden dado, respetando las repeticiones}
}
Por ejemplo pares [1,2,3,5,8,2] da [2,8,2]
--}

pares :: [Int] -> [Int]
pares xs = [x | x <- xs, mod x 2 == 0]

pares2 :: [Int] -> [Int]
pares2 [] = []
pares2 (x:xs) | mod x 2 == 0 = x:pares2 xs
             | otherwise = pares2 xs 

{--
8. multiplosDeN :: Int -> [Int] -> [Int] que dado un número n y una lista xs, devuelve una lista
con los elementos de xs múltiplos de n.
--}

multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN n xs = [x | x <- xs, mod x n == 0]

{--
9. ordenar :: [Int] -> [Int] que ordena los elementos de la lista en forma creciente. Sugerencia: Pensar
cómo pueden usar la función máximo para que ayude a generar la lista ordenada necesaria.
--}

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar xs = maximo xs : ordenar (quitar (maximo xs) xs)

ordenarR :: [Int] -> [Int]
ordenarR xs = reverso (ordenar xs)

-- Ejercicio 5: Definir las siguientes funciones sobre listas:

{--
1. sumaAcumulada :: (Num t) => [t] -> [t] según la siguiente especificación:
problema sumaAcumulada (s: seq⟨T ⟩) : seq⟨T ⟩ {
requiere: {T es un tipo numérico}
requiere: {cada elemento de s es mayor estricto que cero}
asegura: {|s| = |resultado| ∧ el valor en la posición i de resultado es
Pi
k=0 s[k]}
}
Por ejemplo sumaAcumulada [1, 2, 3, 4, 5] es [1, 3, 6, 10, 15].
--}

sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada [] = []
sumaAcumulada (x:xs) = x: sumaAcumulada (aux x xs)
        where 
            aux _ [] = []
            aux x (y:xs) = (x + y) : xs



{--
2. descomponerEnPrimos :: [Integer] -> [[Integer]] según la siguiente especificación:
problema descomponerEnPrimos (s: seq⟨Z⟩) : seq⟨seq⟨Z⟩⟩ {
requiere: { Todos los elementos de s son mayores a 2 }
asegura: { |resultado| = |s| }
asegura: {todos los valores en las listas de resultado son números primos}
asegura: {multiplicar todos los elementos en la lista en la posición i de resultado es igual al valor en la posición
i de s}
}
Por ejemplo descomponerEnPrimos [2, 10, 6] es [[2], [2, 5], [2, 3]].
--}
 
descomponerEnPrimos :: [Int] -> [[Int]]
descomponerEnPrimos [] = [[]]
descomponerEnPrimos (x:xs)| pertenece x listaPrimos = [x] : descomponerEnPrimos xs
                          | otherwise =  descomposicion x : descomponerEnPrimos xs
                        where 
                            listaPrimos = primosN x
 
-- saca todos los multiplos de un numero
sinDiv :: Int -> [Int] -> [Int]
sinDiv _ [] = []
sinDiv x xs = [n | n <- xs, mod n x /= 0] 

-- criba de eratostenes, le das un rango de numeros naturales del 2 a n y te devuelve todos los primos en una lista
criba :: [Int] -> [Int]
criba [] = []
criba (x:xs) = x:criba (sinDiv x xs)

-- le pasas un numero y devuelve todos los primos hasta ese numero
primosN :: Int -> [Int]
primosN n = criba [2..n]

-- dado n y una lista de primos, devuelve una lista con 2 primos q multiplicados hacen n
descomposicion x = [x]


{--
Ejercicio 6. En este ejercicio trabajaremos con la lista de contactos del teléfono.
a) Implementar una función que me diga si una persona aparece en mi lista de contactos del teléfono: enLosContactos ::
Nombre ->ContactosTel ->Bool
b) Implementar una función que agregue una nueva persona a mis contactos, si esa persona está ya en mis contactos entonces
actualiza el teléfono. agregarContacto :: Contacto ->ContactosTel ->ContactosTel
c) Implementar una función que dado un nombre, elimine un contacto de mis contactos. Si esa persona no está no hace
nada. eliminarContacto :: Nombre ->ContactosTel ->ContactosTel
Para esto definiremos los siguientes tipos:
type Texto = [Char]
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]
Sugerencia: Implementar las funciones auxiliares elNombre y elTelefono para que dado un Contacto devuelva el dato
del nombre y el teléfono respectivamente.
--}

type Texto = [Char]
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

enLosContactos :: Nombre -> ContactosTel -> Bool
enLosContactos n [] = False
enLosContactos n ((x, y):xs) = n == x || enLosContactos n xs

agregarContacto :: Contacto ->ContactosTel ->ContactosTel
agregarContacto n [] = [n]
agregarContacto n (x:xs)| fst n == fst x = n:xs 
                        | enLosContactos (fst n) (x:xs) = x : agregarContacto n xs
                        | otherwise = n:x:xs 


quitarContacto :: Nombre ->ContactosTel ->ContactosTel
quitarContacto n [] = []
quitarContacto n (x:xs)| n == fst x = xs 
                        | enLosContactos n (x:xs) = x : quitarContacto n xs
                        | otherwise = x:xs 


{--
Ejercicio 7. En este ejercicio trabajaremos con lockers de una facultad.
Para resolverlo usaremos un tipo MapaDelockers que será una secuencia de locker.
Cada locker es una tupla con la primera componente correspondiente al número de identificación, y la segunda componente
el estado.
El estado es a su vez una tupla cuya primera componente dice si esta ocupado (False) o libre (True), y la segunda
componente es un texto con el código de ubicación del locker.
type Identificacion = Integer
type Ubicacion = Texto
type Estado = (Disponibilidad, Ubicacion)
type Locker = (Identificacion, Estado)
type MapaDeLockers = [Locker]
type Disponibilidad = Bool
1. Implementar existeElLocker :: Identificacion ->MapaDeLockers ->Bool, una función para saber si un locker existe en la facultad.
2. Implementar ubicacionDelLocker :: Identificacion ->MapaDeLockers ->Ubicacion, una función que dado un locker que existe en la facultad, me dice la ubicación del mismo.
3. Implementar estaDisponibleElLocker :: Identificacion ->MapaDeLockers ->Bool, una función que dado un locker que existe en la facultad, me devuelve Verdadero si esta libre.
4. Implementar ocuparLocker :: Identificacion ->MapaDeLockers ->MapaDeLockers, una función que dado un locker que existe en la facultad, y está libre, lo ocupa.
--}

type Identificacion = Integer
type Ubicacion = Texto
type Estado = (Disponibilidad, Ubicacion)
type Locker = (Identificacion, Estado)
type MapaDeLockers = [Locker]
type Disponibilidad = Bool

-- Le pasas el id y the dice si existe en un mapa de lockers
existeElLocker :: Identificacion ->MapaDeLockers ->Bool
existeElLocker i [] = False
existeElLocker i ((x, y):xs) | i == x = True
                             | otherwise = existeElLocker i xs   

-- Le pasas el id y te dice donde esta el locker, solo funciona con lockers q existen
ubicacionDelLocker :: Identificacion ->MapaDeLockers ->Ubicacion
ubicacionDelLocker i ((x, (dis, ub)):xs) | i == x = ub
                                      | otherwise = ubicacionDelLocker i xs 


-- Le pasas el id y te dice si el locker esta disponible, solo funciona con lockers existentes
estaDisponibleElLocker :: Identificacion ->MapaDeLockers ->Bool
estaDisponibleElLocker i ((x, (dis, ub)):xs) | i == x = dis
                                          | otherwise = estaDisponibleElLocker i xs 

ocuparLocker :: Identificacion ->MapaDeLockers ->MapaDeLockers
ocuparLocker i [] = []
ocuparLocker i ((x, (dis, ub)):xs) | i == x = (x, (False, ub)):xs
                                   | otherwise = (x, (dis, ub)) : ocuparLocker i xs


-- ejemplo de mapa de lockers para testeos
lockersFadu :: MapaDeLockers
lockersFadu = 
  [ (1, (True, "Pasillo A")),    -- Locker 1 está libre en el Pasillo A
    (2, (False, "Pasillo B")),   -- Locker 2 está ocupado en el Pasillo B
    (3, (True, "Pasillo C")),    -- Locker 3 está libre en el Pasillo C
    (4, (False, "Pasillo D")),   -- Locker 4 está ocupado en el Pasillo D
    (5, (True, "Pasillo E"))     -- Locker 5 está libre en el Pasillo E
  ]