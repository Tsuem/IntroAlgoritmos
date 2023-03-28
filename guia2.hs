-- Funciones Recursivas
-- a)
cuad :: [Int] -> [Int]
cuad (x:xs) = (x^2):(cuad xs)
cuad [] = []