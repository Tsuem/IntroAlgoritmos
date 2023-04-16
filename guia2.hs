-- ---------------------- FUNCIONES RECURSIVAS ----------------------
-- EJERCICIO DE CLASE
cuad :: [Int] -> [Int]
-- CASO BASE:
cuad [] = []
-- CASO RECURSIVO:
cuad (x:xs) = (x^2):(cuad xs)


-- ----------------------------- GUÍA 2 -----------------------------
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


-- EJERCICIO 5
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


-- EJERCICIO 6
repartir :: [String] -> [String] -> [(String, String)]
repartir [] [] = []
repartir [] (y:ys) = []
repartir (x:xs) [] = []
repartir (x:xs) (y:ys) = (x,y) : repartir xs ys


-- EJERCICIO 7
apellidos :: [(String, String, Int)] -> [String]
apellidos [] = []
apellidos ((x, y, z):xs) = y : apellidos xs 


-- EJERCICIO 8
-- length recursivamente
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- !! recursivamente
posicion :: [a] -> Int -> a
posicion (x:xs) 0 = x
posicion (x:xs) n | n > 0 = posicion xs (n - 1)
                  | n < 0 = x 

indice :: [a] -> Int -> a
indice (x:xs) 0 = x 
indice (x:xs) n = indice xs (n-1)
                
-- take recursivamente
take' :: Int -> [a] -> [a]
take' n [] = []
take' 0 (x:xs) = []
take' n (x:xs) | n > 0 = x : take' (n - 1) xs
               | n < 0 = []

tomar :: Int -> [a] -> [a]
tomar 0 xs = []
tomar n [] = []
tomar n (x:xs) = x:(tomar (n-1) xs)

-- drop recursivamente
drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' 0 (x:xs) = (x:xs)
drop' n (x:xs) | n > 0 = drop' (n-1) xs
               | n < 0 = x:xs

-- ++ recursivamente
concatenar' :: [a] -> [a] -> [a]
concatenar' xs [] = xs
concatenar' [] ys = ys
concatenar' (x:xs) (y:ys) = x : y : concatenar' xs ys

concatenar :: [a] -> [a] -> [a]
concatenar (x:xs) [] = x:xs
concatenar [] (y:ys) = y:ys
concatenar (x:xs) (y:ys) = x : y : concatenar xs ys


-- EJERCICIO 9
--a)
maximo :: [Int] -> Int
maximo [] = minBound
maximo [x] = x
maximo (x:xs) | x >= maximo xs = x
              | x < maximo xs = maximo xs

-- b)
sumaPares :: [(Int, Int)] -> Int
sumaPares [] = 0
sumaPares ((x, y):xs) = x + y + sumaPares xs

-- c)
todos0y1 :: [Int] -> Bool
todos0y1 [] = True
todos0y1 (x:xs) | x == 0 || x==1 = todos0y1 xs
                | otherwise = False

-- d)
quitar0s :: [Int] -> [Int]

quitar0s [] = []
quitar0s (x:xs) | x == 0 = quitar0s xs
                | x /= 0 = x : quitar0s xs

-- e)
ultimo :: [a] -> a
ultimo [x] = x
ultimo (x:xs) = ultimo xs 

-- f)
repetir :: Int-> Int-> [Int]
repetir 0 b = []
repetir a b | a > 0 = b : repetir (a - 1) b
            | a < 0 = []

-- g)
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- h)
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]