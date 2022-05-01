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
-- Costo: O(n).
belongs :: Eq a => a -> Set a -> Bool
belongs x (S xs) = elem x xs

-- Devuelve la cantidad de elementos distintos de un conjunto.
-- Costo: O(n).
sizeS :: Eq a => Set a -> Int
sizeS (S xs) = length xs

-- Borra un elemento del conjunto.
-- Costo: O(n).
removeS :: Eq a => a -> Set a -> Set a
removeS x s = 
	if belongs x s 
		then borrar x s
		else s 

-- Borra un elemento del conjunto.
-- precondición: el elemento pertenece al conjunto.
-- Costo: O(n).
borrar :: Eq a => a -> Set a -> Set a
borrar x (S xs) = S (deleteAll x xs)

-- dados un elemento y una lista borra todas las iteraciones del elemento de la lista. 
-- Costo: O(n).
deleteAll :: Eq a => a -> [a] -> [a]
deleteAll x []     = []
deleteAll x (y:ys) = 
	if x == y
		then deleteAll x ys
		else y : deleteAll x ys

-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
-- Costo: O(n).
unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs) (S ys) = S (xs++ys)

-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
-- -- Costo: O().
setToList :: Eq a => Set a -> [a]
setToList (S xs) = sinRepetidos xs

-- dados una lista de elementos, la devuelve sin elementos repetidos.
-- Costo: O(n^2).
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = agregarSiNoPertenece x (sinRepetidos xs)

-- dados un elemento y una lista de elementos, agrega el elemento si no se encuentra en la lista.
-- Costo: O(n).
agregarSiNoPertenece :: Eq a => a -> [a] -> [a]
agregarSiNoPertenece x []     = x : []
agregarSiNoPertenece x (y:ys) =
	if (x == y)
	  then y : ys
	  else y : agregarSiNoPertenece x ys