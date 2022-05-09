module MultiSet1(MultiSet, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList) where

import Map1

data MultiSet a = MS (Map a Int)

-- INV.Rep.: no tiene. 

{- INTERFAZ MAP
emptyM :: Map k v
devuelve un map vacío

assocM :: Eq k => k -> v -> Map k v -> Map k v
agrega una asociación clave-valor al map.

lookupM :: Eq k => k -> Map k v -> Maybe v
encuentra un valor dado una clave.

deleteM :: Eq k => k -> Map k v -> Map k v
borra una asociación dada una clave.

keys :: Map k v -> [k]
devuelve las claves del map.
-}

ms1 = addMS "juan" 
   $ addMS "emma" 
   $ addMS "abru"
   $ addMS "abru"
   $ addMS "abru"
   $ addMS "juan" 
   $ emptyMS

ms2 = addMS "emma"
   $ addMS "facu" 
   $ emptyMS


-- Los costos se consideran con el uso del Map1

-- Propósito: denota un multiconjunto vacío.
-- Costo: O(1), porque el costo de emptyM es constante.
emptyMS :: MultiSet a
emptyMS = MS emptyM

-- Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
-- multiconjunto.
-- Costo: O(n), siendo n la cantidad de elementos del map del multiset.
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS x ms = armarMSCon x (ocurrencesMS x ms + 1) ms

-- Costo: O(p), siendo p la cantidad de elementos del map del multiset. 
armarMSCon :: Ord a => a -> Int -> MultiSet a -> MultiSet a
armarMSCon x n ms = MS (assocM x n (mapDeMS ms))

-- Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
-- elemento en el multiconjunto.
-- Costo: O(n), siendo n la cantidad de elementos del map del multiset.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS x (MS m) = 
    let 
        v = lookupM x m
    in 
        if existeValor v
            then fromJust v
            else 0

-- Costo: O(1)
existeValor :: Maybe a -> Bool
existeValor (Just _) = True
existeValor _        = False

-- dado un maybe devuelve su valor.
-- precondición: no puede ser Nothing
-- Costo: O(1), porque sólo devuelvo el valor x.
fromJust :: Maybe a -> a  
fromJust (Just x) = x 


-- Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
-- ambos multiconjuntos.
-- Costo: O(n*m), siendo n la cantidad claves de la lista y m la cant de elementos de multiset.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 
unionMS (MS m1) ms = MS (unionMSK (keys m1) m1 ms)

-- asocia los pares de las claves dadas del primer map con las del map del multiset con la suma de
-- las ocurrencias de ambos como valor. 
-- Costo: O(n*m), siendo n la cantidad claves de la lista y m la cant de elementos de multiset.
unionMSK :: Ord k => [k] -> Map k Int -> MultiSet k -> Map k Int
unionMSK []     m1 ms = mapDeMS ms
unionMSK (k:ks) m1 ms = 
    assocM k (valorDe k m1 + ocurrencesMS k ms) (unionMSK ks m1 ms) 

-- Costo: O(m), siendo m la cantidad de pares del map.
-- precondición: hay un par con la clave dada en el map.
valorDe :: Eq k => k -> Map k v -> v
valorDe k m = fromJust (lookupM k m)

-- dado un Multiset que implementa un map, devuelve su map.
-- Costo O(1).
mapDeMS :: MultiSet a -> Map a Int
mapDeMS (MS m) = m


-- Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
-- multiconjuntos tienen en común.
-- Costo: O(n*m), siendo n la cantidad de elementos del primer multiset y m los del segundo.
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 
intersectionMS (MS m1) (MS m2) = MS (intersectionM (keys m1) m1 m2)

-- Costo: O(n*m), siendo n la cantidad de elementos de la lista y m la cantidad de elementos
-- del segundo map.
intersectionM :: Ord k => [k] -> Map k Int -> Map k Int -> Map k Int
intersectionM []     m1 m2 = emptyM
intersectionM (k:ks) m1 m2 = 
      if existeValor (lookupM k m2)
          then assocM k (valorDe k m1 + valorDe k m2) (intersectionM ks m1 m2)
          else intersectionM ks m1 m2

-- Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
-- su cantidad de ocurrencias.
-- Costo: O(n^2), siendo n la cantidad de pares del map del multiset.
multiSetToList :: Eq a => MultiSet a -> [(a, Int)]
multiSetToList (MS m) = mapToList m   -- ya hecha como usuario de map.

-- Costo: O(n^2), siendo n la cantidad de pares del map.
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m = asociarAValores (keys m) m 

-- Costo: O(n*p), siendo n la cantidad de elementos de la lista y p los pares del map.
asociarAValores :: Eq k => [k] -> Map k v -> [(k,v)]
asociarAValores []     m = []
asociarAValores (k:ks) m = (k, fromJust (lookupM k m)) : asociarAValores ks m
