import Stack1

{- Interfaz Stack
emptyS   - Crea una pila vacía.
isEmptyS - Dada una pila indica si está vacía.
push     - Dados un elemento y una pila, agrega el elemento a la pila.
top      - Dada un pila devuelve el elemento del tope de la pila.
pop      - Dada una pila devuelve la pila sin el primer elemento.
lenS     - Dada una pila devuelve la pila sin el primer elemento.
-}

-- Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a
apilar []     = emptyS
apilar (x:xs) =	push x (apilar xs)

-- Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar :: Stack a -> [a]
desapilar s = 
	if isEmptyS s
		then []
		else top s : desapilar (pop s) 

-- Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
-- posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos 0 x s = push x s
insertarEnPos n x s = insertarEnPos (n-1) x (pop s)


