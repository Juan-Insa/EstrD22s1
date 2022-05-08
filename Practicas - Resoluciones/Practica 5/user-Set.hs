import Set2
{- 
emptyS:    Crea un conjunto vacío.
addS:      Dados un elemento y un conjunto, agrega el elemento al conjunto
belongs:   Dados un elemento y un conjunto indica si el elemento pertenece al conjunto
sizeS      Devuelve la cantidad de elementos distintos de un conjunto.
removeS:   Borra un elemento del conjunto.
unionS:    Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
setToList: Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.  
-} 

s1 =  addS 1
    $ addS 3
    $ addS 3
    $ addS 4
    $ emptyS


-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
-- al conjunto.
-- Costo S1: O(n*m), siendo n la cantidad de elementos de la lista y m la del conjunto.
-- Costo S2: O(n*m), siendo n la cantidad de elementos de la lista y m las veces que se
-- agregró un elemento al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen []     c = []
losQuePertenecen (x:xs) c =  
	if belongs x c 
		then x : losQuePertenecen xs c
        else losQuePertenecen xs c

-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar
-- Costo S1: O(n^2), siendo n la cantidad de elementos en la lista (setToList es constante).
-- Costo S2: O(n^2), siendo n la cantidad de elementos en la lista (seToList es cuadrática).
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (listToSet xs)

-- agrega los elementos de la lista dada a un set.
-- nota: el set dado deberia estar vacio.
-- Costo S1: O(n^2), siendo n la cant de elementos de la lista (addS es lienal).
-- Costo S2: O(n), siendo n la cant de elementos de la lista (addS es constante).
listToSet :: Eq a => [a] -> Set a
listToSet []     = emptyS
listToSet (x:xs) = addS x (listToSet xs)


data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
-- del arbol.
-- (no estoy muy seguro de estos costos).
-- Costo S1: O(n^3), siendo n la cantidad de elementos del arbol.
-- Costo S2: O(n^2), siendo n la cantida de elementos del arbol. 
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT          = emptyS
unirTodos (NodeT c ti td) = unionS c (unionS (unirTodos ti) (unirTodos td))


















