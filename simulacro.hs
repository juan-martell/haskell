-- Ejercicio 1 --------------
-- hayQueCodificar devuelve true si hay que codificar la letra
hayQueCodificar :: Char -> [(Char, Char)] -> Bool
hayQueCodificar c [] = False
hayQueCodificar c ((x, y):xs) = c == x || hayQueCodificar c xs

-- Ejercicio 2 --------------
-- cuantasVecesHayQueCodificar devuelve cuantas veces codificas una letra en una frase
cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char, Char)] -> Int
cuantasVecesHayQueCodificar _ [] _  = 0
cuantasVecesHayQueCodificar c frase mapeo |not (hayQueCodificar c mapeo) = 0 
                                          |otherwise = apariciones c frase 

apariciones :: Char -> [Char] -> Int
apariciones c [] = 0
apariciones c (x:xs) | c == x = 1 + apariciones c xs
                     | otherwise = apariciones c xs   


-- Ejercicio 3 --------------
-- laQueMasHayQueCodificar devuelve el caracter q mas hay que codificar
laQueMasHayQueCodificar :: [Char] -> [(Char, Char)] -> Char
laQueMasHayQueCodificar [c] _ = c
laQueMasHayQueCodificar (x:xs) mapeo | cuantoCodificar x >= cuantoCodificar recursion = x
                                     | otherwise = recursion 
                                    where
                                      cuantoCodificar n = cuantasVecesHayQueCodificar n (x:xs) mapeo
                                      recursion = laQueMasHayQueCodificar xs mapeo

 
-- Ejercicio 4 --------------
codificarFrase :: [Char] -> [(Char, Char)] -> [Char]
codificarFrase [] mapeo = []
codificarFrase (x:xs) mapeo | hayQueCodificar x mapeo= codificarLetra x mapeo : codificarFrase xs mapeo
                            | otherwise = x : codificarFrase xs mapeo

codificarLetra :: Char -> [(Char, Char)] -> Char
codificarLetra c [] = c
codificarLetra c ((x, y):xs) | c == x = y
                             | otherwise = codificarLetra c xs   


mapa :: [(Char, Char)]
mapa = [
    ('x', 'y'),
    ('a', 'b'),
    ('z', '0')
    ]
