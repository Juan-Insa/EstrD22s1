import Map1

{- INTERFAZ MAP

emptyM :: Map k v 
- devuelve un map vacío

assocM :: k -> v -> Map k v -> Map k v 
- agrega una asociación clave-valor al map.

lookupM :: k -> Map k v -> Maybe v
- encuentra un valor dado una clave.

deleteM :: k -> Map k v -> Map k v 
- borra una asociación dada una clave.

keys :: Map k v -> [k]
- devuelve las claves del map.

-}
m1 = assocM "juan" 26
   $ assocM "emma" 28
   $ assocM "abru" 22
   $ emptyM

m2 = assocM "juan" 26
   $ assocM "emma" 28
   $ emptyM

-- 1) Propósito: obtiene los valores asociados a cada clave del map
-- Costo M1, M2 y M3: O(n^2), siendo n la cantidad de elementos del map.
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = valoresConK (keys m) m 

-- dada una lista de claves y un map, devuelve una lista con todos los valores asociados.
-- Costo M1, M2 y M3: O(n*p), siendo n la cantidad de elementos de la lista y p la de pares del map.
valoresConK :: Eq k => [k] -> Map k v -> [Maybe v]
valoresConK []     m = []
valoresConK (k:ks) m = lookupM k m : valoresConK ks m

-- 2) Propósito: indica si en el map se encuentran todas las claves dadas.
-- Costo M1, M2 y M3: O(n^2), siendo n la cantidad de elementos de la lista.
-- nota: la unica dif. de costo es q para M3 keys es O(1).
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas ks m = estanAsociadasA ks (keys m) 

-- dadas una clave y un map, indica si se encuentra la clave.
-- Costo: O(n^2), siendo n la cantidad de elementos de la lista.
estanAsociadasA :: Eq k => [k] -> [k] -> Bool
estanAsociadasA []     ks' = True
estanAsociadasA (k:ks) ks' = elem k ks' && estanAsociadasA ks ks'

 
-- 3) Propósito: convierte una lista de pares clave valor en un map.
-- Costo M1 y M3: O(n^2), siendo n la cantidad de pares de la lista.
-- Costo M2: O(n), siendo n la cantidad de elementos de la lista.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap []         = emptyM
listToMap ((k,v):ps) = assocM k v (listToMap ps)

-- 4) Propósito: convierte un map en una lista de pares clave valor.
-- Costo M1, M2 y M3: O(n^2), siendo n la cantidad de elementos del map.
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m = asociarAValores (keys m) m

-- dada una lista de claves y un map, devuelve una lista de pares con las claves
-- asignadas a los valores correspondientes en el map.
-- precondición: todas las claves pertenecen al map.
-- Costo M1, M2 y M3: O(n*p), siendo n la cantidad de elementos de la lista y p 
-- la de pares del map.
asociarAValores :: Eq k => [k] -> Map k v -> [(k,v)]
asociarAValores []     m = []
asociarAValores (k:ks) m = (k, fromJust (lookupM k m)) : asociarAValores ks m

-- dado un maybe devuelve su valor.
-- precondición: no puede ser Nothing
-- Costo: O(1).
fromJust :: Maybe a -> a  
fromJust (Just x) = x 

-- 5) Propósito: dada una lista de pares clave/valor, agrupa los valores de los pares que compartan
--    la misma clave.
-- Costo M1, M2 y M3: O(n^2), siendo n la cantidad de elementos de la lista.
agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq []          = emptyM
agruparEq ((k,v):kvs) = agregarValorEn k v (agruparEq kvs)
	
-- dado un valor, una clave y un map, agrupa los valores del par correspondiente a la clave en el map.
-- Costo M1, M2 y M3: O(n), siendo n la cantidad de pares del map.
agregarValorEn :: Eq k => k -> v -> Map k [v] -> Map k [v]
agregarValorEn k v m = assocM k (agregarAValoresDe k v m) m 

-- dada una clave y una lista de pares clave/valor, agrupa los valores de los pares que compartan
-- la clave.
-- Costo M1, M2 y M3: O(n), siendo n la cantidad de pares del map.
agregarAValoresDe :: Eq k => k -> v -> Map k [v] -> [v]
agregarAValoresDe k v m = 
	let  
	    vs = lookupM k m
	in 
	    if existeValor vs 
	        then v : (fromJust vs)
		    else [v]

-- Costo: O(1).
existeValor :: Maybe a -> Bool
existeValor (Just _) = True
existeValor _        = False

-- 6) Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
--    cada número asociado con dichas claves.
-- Costo M1, M2 y M3: O(n*p), siendo n la cantidad de elementos de la lista y p la de pares del map. 
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar []     m = m
incrementar (k:ks) m = assocM k ((valorDe k m) + 1) (incrementar ks m)

-- dada una clave y un map, devuelve el valor asociado a la clave.
-- precondición: la clave es valida para el map.
-- Costo M1, M2 y M3: O(n), siendo n la cantidad de elementos del map.
valorDe :: Eq k => k -> Map k v -> v
valorDe k m = fromJust (lookupM k m)

-- 7) Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
--    una clave del primero existe en el segundo, es reemplazada por la del primero.	
-- Costo: O(n^2), siendo n la cantidad de elementos del primer map.
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = unirAlMap (keys m1) m1 m2

-- dos maps y una lista de claves del primer map, se agregan las claves y valores del primer map en el segundo.
-- Costo M1, M2 y M3: O(n*p), siendo n la cantidad de elementos de la lista y p los pares del map.
unirAlMap :: Eq k => [k] -> Map k v -> Map k v -> Map k v
unirAlMap []     m1 m2 = m2
unirAlMap (k:ks) m1 m2 = assocM k (valorDe k m1) (unirAlMap ks m1 m2)

-- EJERCICIO 5 --

-- Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
-- su posición en la lista.
-- Costo M1 y M3: O(n^2), siendo n la cantidad de elementos de la lista.
-- Costo M2: O(n), siendo n la cantidad de elementos de la lista.
indexar :: [a] -> Map Int a
indexar xs = indexarM 0 xs

-- dado un número de index y una lista de elementos construye un map que relaciona cada 
-- elemento con su posición a partir del index dado.
-- Costo M1 y M3: O(n^2), siendo n la cantidad de elementos de la lista.
-- Costo M2: O(n), siendo n la cantidad de elementos de la lista.
indexarM :: Int -> [a] -> Map Int a
indexarM n []     = emptyM
indexarM n (x:xs) = assocM n x (indexarM (n+1) xs)

-- Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
-- en el string, y los valores la cantidad de veces que aparecen en el mismo.
-- Costo M1, M2 y M3: O(n^2), siendo n la cantidad caracteres del string.
ocurrencias :: String -> Map Char Int
ocurrencias []     = emptyM
ocurrencias (c:cs) = assocM c (ocurrenciasDe c cs) (ocurrencias (sinLetra c cs))

-- devuelve el string sin el caracter dado.
-- Costo: O(n), siendo n la cantidad de caracteres del string.
ocurrenciasDe :: Char -> String -> Int
ocurrenciasDe c []      = 1
ocurrenciasDe c (c':cs) = unoSi (c==c') + ocurrenciasDe c cs

-- dado un Char, devuelve el string sin el mismo
-- Costo: O(n), siendo n la cantidad de caracteres del String.
sinLetra :: Char -> String -> String
sinLetra c []      = []
sinLetra c (c':cs) = 
	if c == c'
		then sinLetra c cs
	    else c' : sinLetra c cs

-- Dada una condición, devuelve 1 si se cumple y 0 de no hacerlo.
-- Costo: O(1).
unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0









