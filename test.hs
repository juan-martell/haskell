f n = if n >= 100
        then n
        else n * 2

lenght x = sum [1 | _ <- x]
-- Cuando usamos _ le decimos a haskell que no nos importa el valor de la lista solo queremos ir ahi

rmmayus st = [ c | c <- st, c `elem` ['a'..'z']]
-- te saca todas las mayusculas del texto que le das, 
-- elem devuelve True solo si c esta en la lista proporcionada,
-- st es la variable que nosotros le damos al programa, esta toma el lugar de c,
-- cuando termina de correr la funcion nos queda un array con todos los valores de c que dan True

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
rectriangulo = [ (a,b,c)| c <- [1..10], b <- [1..c], a <- [1..b], a^2+b^2 == c^2 ]

