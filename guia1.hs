--
f :: Int -> Int
f x = 5 * x

duplica :: Int -> Int
duplica a = a + a

por2 :: Int -> Int
por2 y = 2 * y

multiplicar :: Int -> Int -> Int
multiplicar zz tt = zz * tt


promedio :: Float -> Float -> Float
promedio x y = (x + y) / 2

entre0y9 :: Int -> Bool
entre0y9 x | (0 <= x) && (x <= 9) = True
           | otherwise = False
           

-- nuevo tipo de datos, ej de clase
g :: (Int, Float) -> (Int,Float) -> (Int, Int, Float)
g (a,b) (c,d) = (a+c, a*c, b/d)

ge :: (Int, Float) -> (Int,Float) -> (Int, Int, Float)
ge t r = (fst t + fst r, fst t * fst r, snd t / snd r)

-- ejercicio 19
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | mod y x == 0 = True
                 | otherwise = False

-- ejercicio 20
esBisiesto :: Int -> Bool
esBisiesto x | (mod x 400 == 0) || (mod x 4 == 0) = True
             | otherwise = False

-- ejercicio 21
dispersion :: Float -> Float -> Float -> Float
--dispersion x y z = maximum[x,y,z] - minimum[x,y,z]
dispersion x y z = max(max x y) z - min(min x y) z

-- ejercicio 22
celsiusToFahr :: Float -> Float
celsiusToFahr c = c * 1.8 + 32  

--ejercicio 23
fahrToCelsius :: Float -> Float
fahrToCelsius f = (f - 32) / 1.8

--ejercicio 24
haceFrioF :: Float -> Bool
haceFrioF f | fahrToCelsius f < 8 = True
            | otherwise = False


--Tuplas
--ejercicio 25
--a)
segundo3 :: (Int, Int, Int) -> Int
segundo3 (a, b, c) = b


--b)
ordena :: (Int, Int) -> (Int, Int)
ordena (a,b) = (min a b, max a b)


--c)
rangoPrecioParametrizado :: Float -> (Float, Float) -> String
rangoPrecioParametrizado x (a,b) | x > (max a b)  = "demasiado caro"
                                 | (x <= (max a b)) && (x >= (min a b)) = "hay que verlo bien"
                                 | (x >= 0) && (x < (min a b)) = "muy barato"
                                 | x < 0 = "esto no puede ser"


--d)
mayor3 :: (Int, Int, Int) -> (Bool, Bool, Bool)
mayor3 (a,b,c) = (a > 3, b > 3, c > 3)

--e) 
todosIguales :: (Int, Int, Int) -> Bool
todosIguales (a, b, c) = (a == b) && (b == c)