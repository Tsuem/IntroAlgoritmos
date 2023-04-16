-- Funciones Recursivas
-- EJERCICIO DE CLASE
cuad :: [Int] -> [Int]
-- CASO BASE:
cuad [] = []
-- CASO RECURSIVO:
cuad (x:xs) = (x^2):(cuad xs)


--GUÍA 2
-- EJERCICIO 3
-- a)
soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | mod x 2 == 0 = x: (soloPares xs)
                 | mod x 2 /= 0 = soloPares xs


-- b)
mayoresQue10 :: [Int] -> [Int]
mayoresQue10 [] = []
mayoresQue10 (x:xs) | x > 10 = x : (mayoresQue10 xs)
                    | x <= 10 = mayoresQue10 xs


-- c)
mayoresQue :: Int -> [Int] -> [Int]
mayoresQue n [] =  []
mayoresQue n (x:xs) | x > n = x : (mayoresQue n xs)
                    | x <= n = mayoresQue n xs


-- EJERCICIO 4
-- a)
sumar1 :: [Int] -> [Int]
sumar1 [] = []
sumar1 (x:xs) = x + 1 : (sumar1 xs)

-- b)
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = x*2 : (duplica xs)

-- c)
multiplica :: Int -> [Int] -> [Int]
multiplica n [] = []
multiplica n (x:xs) = x*n : (multiplica n xs)


--EJERCICIO 5
-- a)
todosMenores10 :: [Int] -> Bool
todosMenores10 [] = True
todosMenores10 (x:xs) | x < 10 = todosMenores10 xs   -- tiene que recorrer toda la lista
                      | x >= 10 = False

-- b)
hay0 :: [Int] -> Bool
hay0 [] = True
hay0 (x:xs) | x == 0 = True                   -- basta que encuentre un 0 para que de True
            | x /= 0 = hay0 xs


-- c)
suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs