import Set2
{- 
emptyS:   Crea un conjunto vacío.
addS:      Dados un elemento y un conjunto, agrega el elemento al conjunto
belongs:   Dados un elemento y un conjunto indica si el elemento pertenece al conjunto
sizeS      Devuelve la cantidad de elementos distintos de un conjunto.
removeS:   Borra un elemento del conjunto.
unionS:    Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
setToList: Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.  
-} 

-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
-- al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen []     c = []
losQuePertenecen (x:xs) c =  
	if belongs x c 
		then x : losQuePertenecen xs c
        else losQuePertenecen xs c

-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (listToSet xs emptyS)

-- agrega los elementos de la lista dada a un set.
-- nota: el set dado deberia estar vacio.
listToSet :: Eq a => [a] -> Set a -> Set a
listToSet []     c = emptyS
listToSet (x:xs) c = addS x (listToSet xs c)


data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
-- del arbol.
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT          = emptyS
unirTodos (NodeT c ti td) = unionS c (unionS (unirTodos ti) (unirTodos td))


















