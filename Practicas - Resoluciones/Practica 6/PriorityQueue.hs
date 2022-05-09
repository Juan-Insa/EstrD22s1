module PriorityQueue1 (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ) where

data PriorityQueue a = P [a]

-- Inv. Rep.:
-- no hay.

p1 = insertPQ 5
   $ insertPQ 8
   $ insertPQ 4
   $ insertPQ 20
   $ insertPQ 6
   $ emptyPQ

-- Propósito: devuelve una priority queue vacía.
-- Costo: O(1).
emptyPQ :: PriorityQueue a
emptyPQ = P []

-- Propósito: indica si la priority queue está vacía.
-- Costo: O(1).
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (P xs) = null xs

-- Propósito: inserta un elemento en la priority queue.
-- Costo: O(1).
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (P xs) = P (x:xs)

-- Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
-- Precondición: parcial en caso de priority queue vacía.
-- Costo: O(n), siendo n la cant de elementos de la pq.
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (P xs) = minimum xs

-- Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
-- Precondición: parcial en caso de priority queue vacía
-- Costo: O(n), siendo n la cant de elementos de la pq.
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (P xs) = P (borrar (minimum xs) xs)

-- dado un elemento y una lista de elemento, borra el elemento de la lista.
-- Costo: O(n), siendo n la cant de elementos de la lista.
borrar :: Eq a => a -> [a] -> [a]
borrar x (y:ys) = 
	if x == y 
	    then ys 
	    else y : borrar x ys


--------------------------------------------------------------

-- Costo: O(n^2), siendo n la cantidad de elementos de la lista.
heapSort ::  Ord a => [a] -> [a]
heapSort xs = pqToList (listToPQ xs)

-- covierte una lista de elementos en una priorityQueue.
-- Costo: O(n), siendo n la cantidad de elementos de la lista.
listToPQ :: Ord a => [a] -> PriorityQueue a
listToPQ []     = emptyPQ
listToPQ (x:xs) = insertPQ x (listToPQ xs) 

-- convierte una priorityQueue en una lista con los elementos ordenados de menor a mayor.
-- Costo: O(n^2), siendo n la cantidad de elementos de la pq.
pqToList :: Ord a => PriorityQueue a -> [a]
pqToList p = 
   if isEmptyPQ p
      then []
      else findMinPQ p : pqToList (deleteMinPQ p) 

