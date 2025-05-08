type Producto = [Char]
type Mercaderia = [Producto]
type Stock = [CantidadProducto]
type CantidadProducto = (Producto, Int)
type Precios = [PrecioProducto]
type PrecioProducto = (Producto, Float)

{--
Ejercicio 1. Implementar la funcion generarStock :: [String] ->[(String, Int)]
problema generarStock (mercaderıa: seq⟨String⟩) : seq⟨String × Z⟩ {
requiere: {True}
asegura: { La longitud de res es igual a la cantidad de productos distintos que hay en mercaderıa}
asegura: {Para cada producto que pertenece a mercaderıa, existe un i tal que 0 ≤ i < |res| y res[i]0=producto y res[i]1 es igual a la cantidad de veces que aparece producto en mercaderıa}
}
--}

generarStock :: Mercaderia -> Stock
generarStock [] = []
generarStock (x:xs) = (x, cuantoDe x (x:xs)) : generarStock (quitar x xs)


cuantoDe :: Producto -> Mercaderia -> Int
cuantoDe x [] = 0
cuantoDe x (y:ys) | x == y = 1 + cuantoDe x ys
                  | otherwise = cuantoDe x ys  

quitar :: Producto -> Mercaderia -> Mercaderia
quitar x [] = []
quitar x (y:ys) | x == y = quitar x ys
                | otherwise = y : quitar x ys


{--
Ejercicio 2. Implementar la funcion stockDeProducto :: [(String, Int))] ->String
problema stockDeProducto (stock: seq⟨String × Z⟩, producto: String ) : Z {
requiere: {No existen dos nombres de productos (primeras componentes) iguales en stock}
requiere: {Todas las cantidades (segundas componentes) de stock son mayores a cero}
asegura: {si no existe un i tal que 0 ≤ i < |stock| y producto = stock[i]0 entonces res es igual a 0 }
asegura: {si existe un i tal que 0 ≤ i < |stock| y producto = stock[i]0 entonces res es igual a stock[i]1 }
}
--}

stockDeProducto :: Stock -> Producto -> Int
stockDeProducto [] _ = 0
stockDeProducto ((prod, cant):xs) x | prod == x = cant
                                    | otherwise = stockDeProducto xs x


{--
Ejercicio 3. Implementar la funcion dineroEnStock :: [(String, Int))] ->[(String, Float)] ->Float
problema dineroEnStock (stock: seq⟨String × Z⟩, precios: seq⟨String × R⟩ ) : R {
requiere: {No existen dos nombres de productos (primeras componentes) iguales en stock}
requiere: {No existen dos nombres de productos (primeras componentes) iguales en precios}
requiere: {Todas las cantidades (segundas componentes) de stock son mayores a cero}
requiere: {Todos los precios (segundas componentes) de precios son mayores a cero}
requiere: {Todo producto de stock aparece en la lista de precios}
asegura: {res es igual a la suma de los precios de todos los productos que estan en stock multiplicado por la cantidad de cada producto que hay en stock}
}
--}

dineroEnStock :: Stock -> Precios -> Float
dineroEnStock [] _ = 0
dineroEnStock ((prod, cant):xs) ys = precioDe prod ys * fromIntegral cant + dineroEnStock xs ys

precioDe :: Producto -> Precios -> Float
precioDe _ [] = 0
precioDe x ((prod, precio):ys) | x == prod = precio
                               | otherwise = precioDe x ys 


{-- Ejercicio 4. Implementar la funcion aplicarOferta :: [(String, Int)] ->[(String, Float)] ->[(String,Float)]
problema aplicarOferta (stock: seq⟨String × Z⟩, precios: seq⟨String × R⟩ ) : seq⟨String × R⟩ {
requiere: {No existen dos nombres de productos (primeras componentes) iguales en stock}
requiere: {No existen dos nombres de productos (primeras componentes) iguales en precios}
requiere: {Todas las cantidades (segundas componentes) de stock son mayores a cero}
requiere: {Todos los precios (segundas componentes) de precios son mayores a cero}
requiere: {Todo producto de stock aparece en la lista de precios}
asegura: {|res| = |precios|}
asegura: {Para todo 0 ≤ i < |precios|, si stockDeProducto(stock, precios[i]0) > 10, entonces res[i]0 = precios[i]0 y
res[i]1 = precios[i]1∗ 0,80}
asegura: {Para todo 0 ≤ i < |precios|, si stockDeProducto(stock, precios[i]0) ≤ 10, entonces res[i]0 = precios[i]0 y res[i]1 = precios[i]1 }
}
--}

aplicarOferta :: Stock -> Precios -> Precios
aplicarOferta _ [] = []
aplicarOferta xs ((prod,precio):ys)|stockDeProducto xs prod > 10 = (prod, precio*0.80):aplicarOferta xs ys
                                   | otherwise = (prod, precio) : aplicarOferta xs ys




-- zona ejemplos para testear !!

ejemploM :: Mercaderia
ejemploM = [
    "manzana", 
    "manzana",
    "manzana",
    "pera",    
    "naranja", 
    "naranja",
    "",
    "uva",
    "uva",
    "uva"
    ]

ejemploP :: Precios
ejemploP =  [
    ("manzana", 1.5),
    ("pera", 2.0),
    ("naranja", 0.0),
    ("uva", -1.0),
    ("", 3.0)
    ]