module Queue1(Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue) where

-- 1. Implemente el tipo abstracto Queue utilizando listas. Los elementos deben encolarse por el
--    final de la lista y desencolarse por delante.

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
-- Costo: O(n), siendo n la cant de elementos de la cola.
queue :: a -> Queue a -> Queue a
queue x (Q xs) = Q (xs ++ [x])

-- Dada una cola devuelve el primer elemento de la cola.
-- Costo: O(1).
firstQ :: Queue a -> a
firstQ (Q xs) = head xs

-- Dada una cola la devuelve sin su primer elemento.
-- Costo: O(1).
dequeue :: Queue a -> Queue a
dequeue (Q xs) = Q (tail xs)




















