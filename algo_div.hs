division :: Int -> Int -> ( Int, Int )
division a d | a<d = (0, a)
             | otherwise = (1 + k, r)
             where (k, r) = division (a - d) d   


mcd :: Int -> Int -> Int
mcd a b | abs b > abs a = mcd b a
mcd a 0 = abs a
mcd a b = mcd b (mod a b)