module Queue2(Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue) where

-- 2. Implemente ahora la versión que agrega por delante y quita por el final de la lista. Compare
--    la eficiencia entre ambas implementaciones
 
-- Invariantes de Representación:
-- no hay.

data Queue a = Q [a]

-- Crea una cola vacía.
-- Costo: O(1).
emptyQ :: Queue a
emptyQ = Q []

-- Dada una cola indica si la cola está vacía.
-- Costo: O(1).
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = null xs

-- Dados un elemento y una cola, agrega ese elemento a la cola.
-- Costo: O(1).
queue :: a -> Queue a -> Queue a
queue x (Q xs) = Q (x:xs)

-- Dada una cola devuelve el primer elemento de la cola.
-- Costo: O(n), siendo n la cant de elementos de la cola.
firstQ :: Queue a -> a
firstQ (Q xs) = last xs

-- Dada una cola la devuelve sin su primer elemento.
-- Costo: O(n), siendo n la cant de elementos de la cola.
dequeue :: Queue a -> Queue a
dequeue (Q xs) = Q (init xs)