import Map1

type SectorId = Int

type CUIL = Int

data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)

-- Inv. Rep.: 
--    id de sectores y los legajos (CUIL) no pueden repetirse.

{- INTERFAZ EMPLEADO

Propósito: construye un empleado con dicho CUIL.
Costo: O(1)
consEmpleado :: CUIL -> Empleado


Propósito: indica el CUIL de un empleado.
Costo: O(1)
cuil :: Empleado -> CUIL

Propósito: agrega un sector a un empleado.
Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.
incorporarSector :: SectorId -> Empleado -> Empleado

Propósito: indica los sectores en los que el empleado trabaja.
Costo: O(N).
sectores :: Empleado -> SectorId

-}



-- Propósito: construye una empresa vacía.
-- Costo: O(1).
-- Justif: emptyM es de O(1).
consEmpresa :: Empresa
consEmpresa = ConsE emptyM emptyM

-- Propósito: devuelve el empleado con dicho CUIL.
-- Costo: O(log E), siendo E la cantidad de empleados de la empresa.
-- Justif: lookupM es de O(log E), el resto de las operacopmes O(1).
buscarPorCUIL :: CUIL -> Empresa -> Empleado
buscarPorCUIL c (ConsE m1 m2) = fromJust (lookupM c m2)

-- Propósito: indica los empleados que trabajan en un sector dado.
-- Costo: O(logS + E), siendo E la cantidad de empleados de la empresa y S los sectores.
-- Justif: se usa lookupM con O(log S) y seguido de setToList con O(E).
empleadosDelSector :: SectorId -> Empresa -> [Empleado]
empleadosDelSector s (ConseE m1 m2) = setToList (fromJust (lookupM s m1))

-- Propósito: indica todos los CUIL de empleados de la empresa.
-- Costo: O(E), siendo E la cantidad de empleados de la empresa.
-- Justif: keys es de O(E).
todosLosCUIL :: Empresa -> [CUIL]
todosLosCUIL (ConsE m1 m2) = keys m2

-- Propósito: indica todos los sectores de la empresa.
-- Costo: O(S), siendo S la cantidad de sectores de la empresa.
-- Justif: keys es de O(S).
todosLosSectores :: Empresa -> [SectorId]
todosLosSectores (ConsE m1 m2) = keys m1

-- Propósito: agrega un sector a la empresa, inicialmente sin empleados.
-- Costo: O(log S), siendo S la cantidad de sectores de la empresa.
-- Justif: agregarSectorM es de O(log S).
agregarSector :: SectorId -> Empresa -> Empresa
agregarSector s (ConsE m1 m2) = ConsE (agregarSectorM s m1) m2

-- agrega un sector al map dado, sin empleados.
-- Costo: O(log S), siendo S la cantidad de sectores del map.
-- Justif: assocM es de O(log S), como emptyS es O(1), no se tiene en cuenta.
agregarSectorM :: SectorId -> Map SectorId Set -> Map SectorId Set
agregarSectorM s m = assocM s emptyS m


-- Propósito: agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendrá
-- el CUIL dado.
-- Costo: O(L * (log S + log X + log N) + log E), donde L es la cantidad de sectores de la lista, N la de sectores del empleado,
-- S la de sectores de la empresa y X la de empleados del los sectores de la lista.
-- Justif: agregarSectores es O(L log N), agregarEmpleadoSE es O(L log (log M + log X)) y assocM es O(log E).
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarEmpleado ss c (ConsE m1 m2) = 
	let empleado = agregarSectores ss (consEmpleado c)
	  in ConsE (agregarEmpleadoSE ss empleado m1) 
	           (assocM c empleado m2) 

-- agrega los sectores dados al empleado.
-- Costo: O(S log N), siendo S la cant de sectores de la lista y N la de sectores del empleado.
-- Justif: se hace recursión sobre incorporarSector con O(log N).
agregarSectores :: [SectorId] -> Empleado -> Empleado
agregarSectores []     e = e
agregarSectores (s:ss) e = incorporarSector s (agregarSectores ss e)

-- agrega al empleado a cada sector de la lista dada en el map.
-- Costo: O(S * (log M + log X)), donde S es la cantidad de sectores de la lista, M la de sectores del map
-- y X la de empleados del sector dado. 
-- Justif: se hace recursión sobre addEmpleadoASector con O(log M + log X).
agregarEmpleadoSE :: [SectorId] -> Empleado -> Map SectorId Set -> Map SectorId Set
agregarEmpleadoSE []     e m = m
agregarEmpleadoSE (s:ss) e m = 
	addEmpleadoASector s e (agregarEmpleadoSE ss e m)

-- costo: O(log S + log X), siendo S la cantidad de sectores del map y X la de empleados del sector dado.
-- Justif: lookupM y assocM es de O(log S) y addS es de O(log X).
addEmpleadoASector :: SectorId -> Empleado -> Map SectorId Set -> Map SectorId Set
addEmpleadoASector s e m =
	let empleados = fromSet (lookupM s m)
      in assocM s (addS e empleados) m

-- Costo : O(1).
fromSet :: Maybe (Set Empleado) -> Set Empleado
fromSet Nothing   = emptyS
fromSet (Just es) = es

-- Costo: O(1).
fromJust :: Maybe a -> a
fromJust (Just x) = x

-- Propósito: agrega un sector al empleado con dicho CUIL.
-- Costo: O(log E + (log S + log M) + log X)), siendo E la cantidad de empleados de la empresa, S la 
-- de sectores de la empresa, X la de sectores que el empleado tiene asignado y M la de empleados del 
-- sector dado.
-- Justif: incorporarSector es de O(log X), lookupM y assocM son de O(log E) y addEmpleadoASector es
-- de O(log S + log M)
agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
agregarASector s c (ConsE m1 m2) = 
	let empleado = incorporarSector s (fromJust (lookupM c m2))
	  in ConsE (addEmpleadoASector s empleado m1) 
	           (assocM c empleado m1)

-- Propósito: elimina al empleado que posee dicho CUIL.
-- Costo: O(log E + (S log (Log S + Log X))), siendo E la cant de empleados de la empresa, S la de
-- sectores de la empresa, y X la empleados de los sectores a los que pertenece el empleado.
-- Justif: lookupM y deleteM son de O(log E) y borrarEmpleadoSE es de O(S log (Log M + Log X)).
borrarEmpleado :: CUIL -> Empresa -> Empresa
borrarEmpleado c (ConsE m1 m2) = 
    let empleado = fromJust (lookupM c m2)
	 in ConsE (borrarEmpleadoSE (sectores c) empleado m1)
	          (deleteM c m2) 

-- Costo: O(S * (Log M + Log X)), siendo S la cant de sectores de la lista y M la de sectores del map,
-- y X la empleados del sector.
-- precond: los sectores de la lista son validos para el map.
-- Justif: se hace rescursion de S sobre removeS de O(log X), lookupM y assocM de O(log M). 
borrarEmpleadoSE :: [SectorId] -> Empleado -> Map SectorId Set -> Map SectorId Set
borrarEmpleadoSE []     e m = m
borrarEmpleadoSE (s:ss) e m = 
    let sinEmpleado = removeS e (fromSet (lookupM s m))
	  in assocM s sinEmpleado (borrarEmpleadoSE ss e m)
