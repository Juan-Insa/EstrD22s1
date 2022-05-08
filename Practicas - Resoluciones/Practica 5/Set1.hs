module Set1 (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where 

data Set a = S [a] Int 
--            elems longitud


-- Inv. de Representación
-- no existen elementos repetidos en la lista
-- el valor del Int es la longitud de la lista

-- Crea un conjunto vacío.
-- Costo: O(1).
emptyS :: Set a
emptyS = S [] 0

-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
-- Costo: O(n).
addS :: Eq a => a -> Set a -> Set a
addS x (S xs l) = S (agregarSiNoPertenece x xs) l

-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
-- Costo: O(n), siendo n la cant de elementos del conjunto.
belongs :: Eq a => a -> Set a -> Bool
belongs x (S xs l) = elem x xs

-- Devuelve la cantidad de elementos distintos de un conjunto.
-- Costo: O(1).
sizeS :: Eq a => Set a -> Int
sizeS (S xs l) = l

-- Borra un elemento del conjunto.
-- Costo: O(n), siendo n la cant de elementos del conjunto.
removeS :: Eq a => a -> Set a -> Set a
removeS x (S xs l) = S (borrar x xs) l 

-- Borra un elemento de la lista.
-- Costo: O(n), siendo n la cant de elementos del conjunto.
borrar :: Eq a => a -> [a] -> [a]
borrar x []     = []
borrar x (y:ys)	= 
	if x == y
		then ys
		else y : borrar x ys

-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
-- Costo: O(n*m), siendo n la cant de elementos del primer conjunto y m los del segundo.
unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs l1) (S ys l2) = 
	let zs = unirSinRepetidos xs ys 
	    in S zs (length zs)

-- dadas dos listas de elementos las une sin repetidos.
-- Costo: O(n*m), siendo n la cant de elementos la primer lista y m los de la segunda.
unirSinRepetidos :: Eq a => [a] -> [a] -> [a]
unirSinRepetidos []     ys = ys
unirSinRepetidos (x:xs) ys = agregarSiNoPertenece x (unirSinRepetidos xs ys)

-- dados un elemento y una lista de elementos, agrega el elemento si no se encuentra en la lista.
-- Costo: O(m), siendo m la cant de elementos de la lista.
agregarSiNoPertenece :: Eq a => a -> [a] -> [a]
agregarSiNoPertenece e []     = e : []
agregarSiNoPertenece e (y:ys) =
	if (e == y)
	  then y : ys
	  else y : agregarSiNoPertenece e ys

-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
-- Costo: O(1).
setToList :: Eq a => Set a -> [a]
setToList (S xs l) = xs