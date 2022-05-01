import Queue3

{- interfaz
emptyQ   - Crea una cola vacía.
isEmptyQ - Dada una cola indica si la cola está vacía.
queue    - Dados un elemento y una cola, agrega ese elemento a la cola.
firstQ   - Dada una cola devuelve el primer elemento de la cola.
dequeue  - Dada una cola la devuelve sin su primer elemento.
-}

-- Cuenta la cantidad de elementos de la cola.
-- Costo Q1: O(n).
-- Costo Q2: O(n^2), porque dequeue es lineal.
-- Costo Q3: O(n^2), está amortizado pero en el peor de los casos dequeue es lineal.
lengthQ :: Queue a -> Int
lengthQ q = if isEmptyQ q
	            then 0
	            else 1 + lengthQ (dequeue q)

-- Dada una cola devuelve la lista con los mismos elementos, donde el orden de la lista es el de la cola.
-- Costo Q1: O(n).
-- Costo Q2: O(n^2), porque firstQ y dequeue son lineales.
-- Costo Q3: O(n^2).
queueToList :: Queue a -> [a]
queueToList q = if isEmptyQ q
	                then []
	                else firstQ q : queueToList (dequeue q) 

-- Inserta todos los elementos de la segunda cola en la primera.
-- Costo Q1: O(n^2), porque queue es lineal.
-- Costo Q2: O(n^2), porque dequeue es lineal.
-- Costo Q3: O(n^2).
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = 
	if isEmptyQ q2
	    then q1
	    else unionQ (queue (firstQ q2) q1) (dequeue q2)

