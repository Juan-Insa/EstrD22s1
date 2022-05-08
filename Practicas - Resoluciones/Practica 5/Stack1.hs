module Stack1(Stack, emptyS, isEmptyS, push, top, pop, lenS) where

data Stack a = S [a]

-- Crea una pila vacía.
-- Costo: O(1).
emptyS :: Stack a
emptyS = S []

-- Dada una pila indica si está vacía.
-- Costo: O(1).
isEmptyS :: Stack a -> Bool
isEmptyS (S xs) = null xs

-- Dados un elemento y una pila, agrega el elemento a la pila.
-- Costo: O(1).
push :: a -> Stack a -> Stack a
push x (S xs) = S (x:xs)

-- Dada un pila devuelve el elemento del tope de la pila.
-- Costo: O(1).
top :: Stack a -> a
top (S xs) = head xs

-- Dada una pila devuelve la pila sin el primer elemento.
-- Costo: O(1).
pop :: Stack a -> Stack a
pop (S xs) = S (tail xs)

-- Dada una pila devuelve su longitud.
-- Costo: O(n), siendo n la cant de elementos de la pila.
lenS :: Stack a -> Int
lenS (S xs) = length xs
