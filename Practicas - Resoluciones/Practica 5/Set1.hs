module Set1 (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where 

data Set a = S [a] Int 
--            elems longitud


-- Inv. de Representación
-- no existen elementos repetidos en la lista
-- el valor del Int es la longitud de la lista
-- el valor del Int no es negativo.

-- Crea un conjunto vacío.
-- Costo: O(1).
emptyS :: Set a
emptyS = S [] 0

-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
-- Costo: O(n).
addS :: Eq a => a -> Set a -> Set a
addS x s = 
	if belongs x s
		then s
		else agregar x s

-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
-- Precondición: el elemento dado no pertenece al set.
-- Costo: O(1).
agregar :: a -> Set a -> Set a
agregar x (S xs l) = S (x:xs) (l+1)

-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
-- Costo: O(n).
belongs :: Eq a => a -> Set a -> Bool
belongs x (S xs l) = elem x xs

-- Devuelve la cantidad de elementos distintos de un conjunto.
-- Costo: O(1).
sizeS :: Eq a => Set a -> Int
sizeS (S xs l) = l

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
borrar x (S xs l) = S (delete x xs) (l-1)

-- Borra la primera iteración del elemento dado de la lista dada.
-- precondición: el elemento pertenece a la lista.
delete :: Eq a => a -> [a] -> [a]
delete x (y:ys) = 
	if x == y
		then ys
		else y : delete x ys

-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
-- Costo: O(n^2).
unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs l1) (S ys l2) = 
	let zs = unirSinRepetidos xs ys 
	    in S zs (length zs)

-- dadas dos listas de elementos las une sin repetidos.
-- Costo: O(n^2).
unirSinRepetidos :: Eq a => [a] -> [a] -> [a]
unirSinRepetidos []     ys = ys
unirSinRepetidos (x:xs) ys = agregarSiNoPertenece x (unirSinRepetidos xs ys)

-- dados un elemento y una lista de elementos, agrega el elemento si no se encuentra en la lista.
-- Costo: O(n^2).
agregarSiNoPertenece :: Eq a => a -> [a] -> [a]
agregarSiNoPertenece e []     = e : []
agregarSiNoPertenece e (y:ys) =
	if (e == y)
	  then y : ys
	  else y : agregarSiNoPertenece e ys

-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
-- -- Costo: O(1).
setToList :: Eq a => Set a -> [a]
setToList (S xs l) = xs