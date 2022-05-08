import Stack1

{- Interfaz Stack
emptyS   - Crea una pila vacía.
isEmptyS - Dada una pila indica si está vacía.
push     - Dados un elemento y una pila, agrega el elemento a la pila.
top      - Dada un pila devuelve el elemento del tope de la pila.
pop      - Dada una pila devuelve la pila sin el primer elemento.
lenS     - Dada una pila devuelve la pila sin el primer elemento.
-}

s1 = push 5
   $ push 4
   $ push 3
   $ push 2
   $ push 1
   $ emptyS


-- Dada una lista devuelve una pila sin alterar el orden de los elementos.
-- Costo: O(n), siendo n la cant de elementos de la lista.
apilar :: [a] -> Stack a
apilar []     = emptyS
apilar (x:xs) = push x (apilar xs)

-- Dada una pila devuelve una lista sin alterar el orden de los elementos.
-- Costo: O(n), siendo n la cant de elementos de la pila.
desapilar :: Stack a -> [a]
desapilar s = 
	if isEmptyS s
		then []
		else top s : desapilar (pop s) 

-- Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
-- posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
-- Costo: O(n), siendo n el numero dado.
insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos 0 x s = push x s
insertarEnPos n x s = push (top s) (insertarEnPos (n-1) x (pop s))


