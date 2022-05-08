module Queue3(Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue) where

-- Implemente la interfaz de Queue pero en lugar de una lista utilice dos listas. Esto permitirá
-- que todas las operaciones sean constantes (aunque alguna/s de forma amortizada).

-- Invariantes de representación
-- Si fs se encuentra vacía, entonces la cola se encuentra vacía.

data Queue a = Q [a] [a]

-- Crea una cola vacía.
-- Costo: O(1).
emptyQ :: Queue a
emptyQ = Q [] []

-- Dada una cola indica si la cola está vacía.
-- Costo: O(1).
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q fs bs) = null fs

-- Dados un elemento y una cola, agrega ese elemento a la cola.
-- Costo: O(1).
queue :: a -> Queue a -> Queue a
queue x (Q fs bs) = 
	if null fs 
		then Q [x] bs
	    else Q fs  (x:bs)

-- Dada una cola devuelve el primer elemento de la cola.
-- Costo: O(1)
firstQ :: Queue a -> a
firstQ (Q fs bs) = head fs

-- Dada una cola la devuelve sin su primer elemento.
-- Costo: O(1) amortizado, en el caso que fs sea vacia O(n^2) con n siendo la cant
-- de elementos de la backstack.
dequeue :: Queue a -> Queue a
dequeue (Q fs bs) = 
	if hayUltimo fs
	    then Q (reverse bs) []
	    else Q (tail fs) bs

-- dada una lista indica si tiene solo un elemento.
hayUltimo :: [a] -> Bool
hayUltimo (x:[]) = True
hayUltimo _      = False