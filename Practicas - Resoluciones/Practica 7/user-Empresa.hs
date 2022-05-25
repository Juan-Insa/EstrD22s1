import Empresa

-- Propósito: construye una empresa con la informacion de empleados dada.
-- Costo: O(S * (log M + log X)), porque es el costo de utilizar agregarEmpleado 
comenzarCon :: [SectorId] -> [CUIL] -> Empresa
comenzarCon ss []     = consEmpresa
comenzarCon ss (c:cs) = 
    agregarEmpleado ss c (comenzarCon ss cs) 


-- Propósito dada una empresa elimina la mitad de sus empleados (sin importar quienes).
-- Costo: O(C * (S * (Log S + Log X))), porque se utiliza recorteDe.
recortePersonal :: Empresa -> Empresa
recortePersonal e = 
    let cuiles = todosLosCUIL e    
      in recorteDe (div (length cuiles) 2) cuiles e

-- Costo: O(C * (S * (Log S + Log X))), siendo C la cantidad de cuil de la lista, 
-- S la de sectores de la empresa, y X la empleados de los sectores a los que 
-- pertenece el empleado con el cuil dado.
-- Justif: se hace C recursiones sobre borrarEmpleado de O(S * (Log S + Log X)).
-- precond: el valor dado no es mayor a la longitud de la lista.
recorteDe :: Int -> [CUIL] -> Empresa -> Empresa
recorteDe 0 _      e = consEmpresa
recorteDe n (c:cs) e = borrarEmpleado c (recorteDe (n-1) cs e)

-- Propósito: dado un CUIL de empleado, le asigna todos los sectore de la empresa.
-- Costo: O(S + ).
-- Justificar: todosLosSectores O(S) y asignarASectores O(s*log ).
convertirEnComodinFEO :: CUIL -> Empresa -> Empresa
convertirEnComodinFEO c e = asignarASectores (todosLosSectores e) c e

-- Costo: O(L * (log E + log S + log M + log X)), L es la cant de sectores de la
-- lista y (log E + log S + log M + log X) el costo de agregarASector. Se hace
-- recursion de agregarASector, L cantidad de veces.
asignarASectores :: [SectorId] -> CUIL -> Empresa -> Empresa
asignarASectores []     c e = ConsEmpresa
asignarASectores (s:ss) c e = agregarASector s c (asignarASectores xs c e)

-- Costo: O(S * (log S + Log X)), donde S es la cantidad de sectores de la lista, 
-- Justificacion: utiliza agregarEmpleado de O(S *log S), borrarEmpleado de O(S * (Log S + Log X)).
-- y todosLosSectores de O(S)
convertirEnComodin :: CUIL -> Empresa -> Empresa
convertirEnComodin c e = agregarEmpleado (todosLosSectores e) c (borrarEmpleado c e)

-- Propósito: dado un CUIL de empleado indica si el empleado está en todos los sectores.
-- Costo: O(n), siendo n la cantidad de sectoresId en la empresa.
-- Justif: porque los costos de length, todosLosSectores, sectores, buscarPorCUIL
-- son los siguientes            O(S)       O(S)            O(i)       (log E)
-- es posible porque la invariante de representación me asegura que un empleado conoce todos los sectores
-- a los que está asignado en la estructura. 
esComodin :: CUIL -> Empresa -> Bool
esComodin c e = length (todosLosSectores e) == length (sectores (buscarPorCUIL c e))
-- esto es teniendo en cuenta que puedo acceder a las funciones de la interfaz de empleado

-- Propósito: dado un CUIL de empleado indica si el empleado está en todos los sectores.
-- Costo: O(S*E).
-- Justif: se utiliza estaEnTodosLosSectores con O(S*E), todosLosSectores con O(S) y buscarPorCUIL
-- no se tiene en cuenta al ser O(log E).
esComodin :: CUIL -> Empresa -> Bool
esComodin c e = estaEnTodosLosSectores (buscarPorCUIL c e) (todosLosSectores e) e


-- Costo: O(S*E), siend S la cantidad de sectores de la lista y E la cantidad de empleados
-- de cada sector.
-- Justif: se lleva a cabo la operacion elem con O(E) por cada sector de la lista.
estaEnTodosLosSectores :: Empleado -> [SectorId] -> Empresa -> Bool
estaEnTodosLosSectores empleado []     e = True
estaEnTodosLosSectores empleado (s:ss) e = 
    elem empleado (empleadosDelSector s e) && estaEnTodosLosSectores empleado ss e
 