-- Costo: O(1)
head' :: [a] -> a
head' (x:xs) = x

-- Costo: O(1)
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

-- Costo: O(n)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Costo: O(n) siendo n la cant de elementos de la lista
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- Costo: O(n*x), siendo n es la cant de nros y x la suma de todos los nros de la lista.
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

-- Costo: O(N), N es la cant de elementos de la lista
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

-- Costo: O(n^2), siendo n la cantidad de elementos de la lista.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =
    if pertenece x xs
        then sinRepetidos xs
        else x : sinRepetidos xs


-- equivalente a (++)
-- Costo: O(n), siendo n la cantidad de elementos de la primer lista.
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- Costo: O(n^2), siendo n la cantidad de elementos de la lista.
concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

-- Costo: O(n), siendo n el nro dado.
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

-- Costo: O(n), siendo n el nro dado.
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

-- Costo: O(n), siendo n el nro dado.
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

-- Costo: O(n), siendo n la cant de elementos de la lista.
minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

-- Costo: O(n), siendo n la cant de elementos de la lista.
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) =
    if n == x
        then xs
        else x : sacar n xs

-- Costo: O(n^2), siendo n la cantidad de elementos de la lista.
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =
    let m = minimo xs
        in m : ordenar (sacar m xs)