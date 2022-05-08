module Set2 (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where 

data Set a = S [a] 

-- Inv. de Representación
-- 

-- Crea un conjunto vacío.
-- Costo: O(1).
emptyS :: Set a
emptyS = S [] 

-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
-- Costo: O(1).
addS :: Eq a => a -> Set a -> Set a
addS x (S xs) = S (x:xs)

-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
-- Costo: O(n), siendo n la cant veces que se agregaron elementos al conjunto.
belongs :: Eq a => a -> Set a -> Bool
belongs x (S xs) = elem x xs

-- Devuelve la cantidad de elementos distintos de un conjunto.
-- Costo: O(n^2), siendo n la cant veces que se agregaron elementos al conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (S xs) = length (sinRepetidos xs)

-- Borra un elemento del conjunto.
-- Costo: O(n), siendo n la cant veces que se agregaron elementos al conjunto..
removeS :: Eq a => a -> Set a -> Set a
removeS x (S xs) = S (borrarTodos x xs)

-- dados un elemento y una lista borra todas las iteraciones del elemento de la lista. 
-- Costo: O(n), siendo n la cantidad de elementos de la lista.
borrarTodos :: Eq a => a -> [a] -> [a]
borrarTodos x []     = []
borrarTodos x (y:ys) = 
	if x == y
		then borrarTodos x ys
		else y : borrarTodos x ys

-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
-- Costo: O(n), siendo n la cant veces que se agregaron elementos al primer conjunto.
unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs) (S ys) = S (xs++ys)

-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
-- Costo: O(n^2), siendo n la cant veces que se agregaron elementos al conjunto.
setToList :: Eq a => Set a -> [a]
setToList (S xs) = sinRepetidos xs

-- dados una lista de elementos, la devuelve sin elementos repetidos.
-- Costo: O(n^2), siendo n la cant de elementos de la lista.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = agregarSiNoPertenece x (sinRepetidos xs)

-- dados un elemento y una lista de elementos, agrega el elemento si no se encuentra en la lista.
-- Costo: O(n), siendo n la cant de elementos de la lista.
agregarSiNoPertenece :: Eq a => a -> [a] -> [a]
agregarSiNoPertenece x []     = x : []
agregarSiNoPertenece x (y:ys) =
	if (x == y)
	  then y : ys
	  else y : agregarSiNoPertenece x ys