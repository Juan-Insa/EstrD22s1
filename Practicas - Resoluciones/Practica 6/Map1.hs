module Map1(Map, emptyM, assocM, lookupM, deleteM, keys) where

data Map k v = M [(k,v)]
-- INV.REP.: en M kvs, no hay claves repetidas en kvs

m1 = assocM "juan" 26
   $ assocM "emma" 28
   $ assocM "abru" 22
   $ emptyM

-- Propósito: devuelve un map vacío
-- Costo: O(1).
emptyM :: Map k v
emptyM = M []

-- Propósito: agrega una asociación clave-valor al map.
-- Costo: O(n), siendo n la cantidad de pares del map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M kvs) = M (asociar k v kvs)

-- agrega una asociación clave-valor a la lista.
-- Costo: O(n), siendo n la cantidad de pares de la lista.
asociar :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
asociar k v []            = [(k,v)]
asociar k v ((k',v'):kvs) = 
	if k == k'
		then (k',v) : kvs
		else (k',v') : asociar k v kvs

-- Propósito: encuentra un valor dado una clave.
-- Costo: O(n), siendo n la cantidad de pares del map.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M kvs) = buscar k kvs

-- encuentra un valor dado una clave en la lista de pares clave/valor.
-- Costo: O(n), siendo n la cantidad de pares de la lista.
buscar :: Eq k => k -> [(k,v)] -> Maybe v
buscar k []           = Nothing
buscar k ((k',v):kvs) = 
	if k == k' 
		then Just v
		else buscar k kvs

-- Propósito: borra una asociación dada una clave.
-- Costo: O(n), siendo n la cantidad de pares del map.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M kvs) = M (borrar k kvs)

-- borra un par clave/valor de la lista dada una clave.
-- Costo: O(n), siendo n la cantidad de pares de la lista.
borrar :: Eq k => k -> [(k,v)] -> [(k,v)]
borrar k []           = []
borrar k ((k',v):kvs) = 
	if k == k'
		then kvs
		else (k',v) : borrar k kvs

-- Propósito: devuelve las claves del map.
-- Costo: O(n), siendo n la cantidad de pares del map.
keys :: Map k v -> [k]
keys (M kvs) = claves kvs

-- devuelve las claves de la lista de pares clave/valor.
-- Costo: O(n), siendo n la cantidad de pares de la lista.
claves :: [(k,v)] -> [k]
claves []          = []
claves ((k,v):kvs) = k : claves kvs