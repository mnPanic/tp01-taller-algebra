-----------------------------------------------------------------------
-- Nombre y LU/DNI de los integrantes del grupo:
-- INTEGRANTE 1: Manuel Panichelli    LU: 72/18
-- INTEGRANTE 2: Vladimir Pomsztein   LU: 364/18
-- INTEGRANTE 3: Ignacio Alonso Rehor LU: 195/18
-----------------------------------------------------------------------

-----------------------------------------------------------------------
--                        Formato del archivo
-----------------------------------------------------------------------
-- El archivo tiene el siguiente formato:
--
--- | Definiciones y funciones dadas (por los profesores)
--- | Parte A
---- | Implementacion
----- | Funcion Xi
------ | Descripción
------ | Funciones auxiliares
------ | Funcion principal
--
--- | Parte B
---- | Implementacion
----- | Funcion Xi
------ | Descripción
------ | Funciones auxiliares
------ | Funcion principal
--
--- | Casos de test
---- | Parte A
---- | Parte B
---- | General

-----------------------------------------------------------------------
--                               Tests
-----------------------------------------------------------------------
-- Todos los casos de tests provistos por los profesores fueron
-- tomados en cuenta.
--
-- La mecánica de tests usada fue la de llamar a un test,
-- el cual si pasa retorna True y sino False
--
-- Cada función realizada (salvo algunas excepciones) tiene su propio 
-- caso de test, al igual que las funciones auxiliares utilizadas.
--
-- Ej. La función 1 de la parte A tiene su test, 'testCaminoValido', 
--     y una funcion auxiliar a la funcion 1, 'desplazar' tambien, 
--     'testDesplazar'.
--     Además, todas las funciones que pueden categorizarse como parte
--     de la función 1 se pueden testear con la funcion que las engloba
--     a todas, 'testA_i'.
-- 
-- Todos los tests pueden correrse con 'test'
-- Los de la parte A con 'testA',
-- y los de la parte B con 'testB'

-----------------------------------------------------------------------
--                   Definiciones y funciones dadas
-----------------------------------------------------------------------

data Desplazamiento = Arriba | Abajo | Izquierda | Derecha deriving (Show, Eq)

type Conjunto a = [a]
type Camino = [Desplazamiento]
type Posicion = (Integer,Integer)
type Tablero a = [[a]]
type CampoMinado = Tablero Bool
type TableroAF = Tablero Desplazamiento

-- Devuelve el tamaño de un tablero.
tamano :: Tablero a -> Integer
tamano t = fromIntegral(length t)

-- Devuelve el valor de una posición de un tablero.
-- Notar que la primera posición de arriba a la izquierda es la (1,1).
valor :: Tablero a -> Posicion -> a
valor t (i,j) = iesimo (iesimo t i) j

-- Devuelve el iésimo elemento de una lista. El primer elemento es el 1.
iesimo :: [a] -> Integer -> a
iesimo (x:xs) 1 = x
iesimo (x:xs) n = iesimo xs (n-1)

-- Determina si una posición está dentro de los límites de un tablero.
posValida :: Tablero a -> Posicion -> Bool
posValida t (i,j) = 1<=i && i<=n && 1<=j && j<=n
    where n = tamano t

-- Caminos de prueba.
camino1 = [Derecha, Abajo, Izquierda, Arriba, Abajo, Abajo, Derecha, Derecha]
camino2 = [Derecha, Abajo, Derecha, Abajo]
camino3 = [Derecha, Abajo, Derecha, Izquierda, Derecha, Abajo]

-- CampoMinado de prueba.
campo1 :: CampoMinado
campo1 = [ [False, False, True],
           [True,  False, False],
           [True,  True,  False] ]

-- TableroAF de prueba, sin ciclos.
taf1 :: TableroAF
taf1 = [ [Derecha,  Derecha, Abajo],
         [Arriba, Izquierda, Abajo],
         [Arriba, Izquierda, Abajo] ]

-- TableroAF de prueba, con ciclos.
taf2 :: TableroAF
taf2 = [ [Derecha,       Abajo, Abajo],
         [Arriba,    Izquierda, Abajo],
         [Izquierda, Izquierda, Izquierda] ]

-----------------------------------------------------------------------
--                              PARTE A
-----------------------------------------------------------------------
-------------------------- | Implementación | -------------------------

----------------------- FUNCION 1: caminoValido -----------------------
--- | DESCRIPCIÓN | ---
-- Determina si un camino se mantiene dentro de los límites del tablero 
-- a lo largo de su trayectoria, asumiendo que se comenzará 
-- por la posición (1, 1).


--- | AUXILIARES | ---
--- desplazar: Dada una posición y un desplazamiento devuelve la posición resultante.
desplazar :: Posicion -> Desplazamiento -> Posicion
desplazar (a, b) Arriba    = (a - 1, b)
desplazar (a, b) Derecha   = (a, b + 1)
desplazar (a, b) Abajo     = (a + 1, b)
desplazar (a, b) Izquierda = (a, b - 1)

--- caminoValidoDesde: Determina si un camino se mantiene dentro de los
--                     límites de un tablero a lo largo de su trayectoria,
--                     comenzando en la posición dada.
caminoValidoDesde :: Tablero a -> Camino -> Posicion -> Bool
caminoValidoDesde tablero [] pos = posValida tablero pos
caminoValidoDesde tablero (d:ds) pos = (posValida tablero pos) && (caminoValidoDesde tablero ds siguientePos)
    where siguientePos = desplazar pos d

--- | PRINCIPAL | ---
--- caminoValido: Determina si un camino se mantiene dentro de los límites
--                del tablero a lo largo de su trayectoria, 
--                asumiendo que se comenzará por la posición (1, 1).
caminoValido :: Tablero a -> Camino -> Bool
caminoValido tablero camino = caminoValidoDesde tablero camino (1, 1)

----------------------- FUNCION 2: caminoDeSalida -----------------------
--- | DESCRIPCIÓN | ---
-- Determina si un RAE, comenzando en la posición (1, 1), al seguir el
-- camino dado, llega a la posición (n, n) sin pisar ninguna mina.

--- | AUXILIARES | ---
--- posicionSalida: Dado un tablero, devuelve la posicion de salida.
posicionSalida :: Tablero a -> Posicion
posicionSalida tablero = (indiceFinal, indiceFinal)
    where indiceFinal = tamano tablero

--- hayMina: Dado un Campo Minado, dice si hay una mina en una posicion dada
hayMina :: CampoMinado -> Posicion -> Bool
hayMina campo pos = (valor campo pos)   -- La presencia de minas esta determinada por un "True"

--- caminoDeSalidaDesde: Determina si un RAE, comenzando en la posicion dada, al seguir el camino dado,
--                       llega a la posicion (n, n) sin pisar ninguna mina.
caminoDeSalidaDesde :: CampoMinado -> Camino -> Posicion -> Bool
caminoDeSalidaDesde campo [] pos = esPosicionSalida && (not (hayMina campo pos))
    where esPosicionSalida = (pos == (posicionSalida campo))
caminoDeSalidaDesde campo (d:ds) pos
    | not (caminoValidoDesde campo (d:ds) pos) = False    -- Si no es un camino valido (se sale del tablero), siempre será False
    | otherwise = (not (hayMina campo pos)) && caminoDeSalidaDesde campo ds siguientePos
    where siguientePos = desplazar pos d

--- | PRINCIPAL | ---
--- caminoDeSalida: Determina si un RAE, comenzando en la posición (1, 1), al seguir el camino dado, 
--                  llega a la posición (n, n) sin pisar ninguna mina.
caminoDeSalida :: CampoMinado -> Camino -> Bool
caminoDeSalida campo camino = caminoDeSalidaDesde campo camino (1, 1)
          
----------------------- FUNCION 3: caminoDeSalidaSinRepetidos -----------------------
--- | DESCRIPCIÓN | ---
-- Determina si un RAE, comenzando en la posición (1, 1), al seguir el camino dado, 
-- llega a la posición (n, n) sin pisar ninguna mina y sin pasar dos veces
-- por una misma posición.

--- | AUXILIARES | ---
--- contenidoEn: Dado una lista de elementos y un elemento, dice si ese elemento pertenece a la lista.
contenidoEn :: Eq a => a -> [a] -> Bool
contenidoEn n l = elem n l

--- caminoSinPosicionesRepetidasDesde: Dado un camino, una posicion desde la cual comenzar, 
--                                     y un conjunto de posiciones recorridas,
--                                     se fija si ese camino tendra posiciones repetidas.
caminoSinPosicionesRepetidasDesde :: Camino -> Posicion -> Conjunto Posicion -> Bool
caminoSinPosicionesRepetidasDesde [] _ _ = True
caminoSinPosicionesRepetidasDesde (d:ds) pos posiciones = (not (siguientePos `contenidoEn` posiciones)) && (caminoSinPosicionesRepetidasDesde ds siguientePos (pos:posiciones))
    where siguientePos = desplazar pos d

--- caminoSinPosicionesRepetidas: Dado un camino, se fija si tiene posiciones repetidas.
caminoSinPosicionesRepetidas :: Camino -> Bool
caminoSinPosicionesRepetidas camino = caminoSinPosicionesRepetidasDesde camino (1, 1) []

--- | PRINCIPAL | ---
--- caminoDeSalidaSinRepetidos: Determina si un RAE, comenzando en la posición (1, 1), al seguir el camino dado,
--                              llega a la posición (n, n) sin pisar ninguna mina y sin pasar dos veces por una
--                              misma posición.
caminoDeSalidaSinRepetidos :: CampoMinado -> Camino -> Bool
caminoDeSalidaSinRepetidos campo camino = (caminoDeSalida campo camino) && (caminoSinPosicionesRepetidas camino)

----------------------- FUNCION 4: salidasEnKDesp -----------------------
--- | DESCRIPCIÓN | ---
-- Dados un campo minado y un número natural k, devuelve el conjunto de 
-- todos los caminos de longitud k que lleven a un RAE desde (1, 1)
-- hasta (n, n), sin pisar ninguna mina.

--- | AUXILIARES | ---
--- listaDeElementos: Dado un conjunto de elementos, crea un conjunto de listas con dichos elementos.
--  Ej. listaDeElementos [1, 2, 3] ~> [[1], [2], [3]]
listaDeElementos :: Conjunto a -> Conjunto [a]
listaDeElementos [] = []
listaDeElementos (x:xs) = [[x]]++(listaDeElementos xs)

--- agregarATodasLasListas: Dado un elemento y una lista de listas, agrega ese elemento
--                          a todas las listas.
--  Ej. agregarATodasLasListas 1 [[0], [2]] ~> [[1,0],[1,2]]
agregarATodasLasListas :: a -> [[a]] -> [[a]] 
agregarATodasLasListas _ [] = []
agregarATodasLasListas n (x:xs) = [n:x]++(agregarATodasLasListas n xs)

--- agregarTodos: Dado una lista y un conjunto de listas, agrega por separado cada elemento de la lista
--                a todas las listas del conjunto.
-- Ej. agregarTodos [0, 1] [[2],[3],[4]] ~> [[0,2],[0,3],[0,4],[1,2],[1,3],[1,4]]
agregarTodos :: [a] -> Conjunto [a] -> [[a]]
agregarTodos [] conjunto = []
agregarTodos (x:xs) conjunto = (agregarATodasLasListas x conjunto)++(agregarTodos xs conjunto)

--- variaciones: Dado un conjunto de elementos y una longitud, crea todas las posibles
--               listas de dicha longitud con los elementos dados.
--  Ej. variaciones [4, 7] 2 ~> [[4,4], [4,7], [7,4], [7,7]]
variaciones :: Conjunto a -> Integer -> Conjunto [a]
variaciones _ 0 = []
variaciones elementos 1 = listaDeElementos elementos
variaciones elementos n = agregarTodos elementos variacionAnterior
                        where variacionAnterior = variaciones elementos (n - 1)

--- caminosPosiblesDeLongitud: Dado una longitud, devuelve un conjunto de todos los caminos
--                             posibles de dicha longitud.
caminosPosiblesDeLongitud :: Integer -> Conjunto Camino
caminosPosiblesDeLongitud k = variaciones [Arriba, Derecha, Abajo, Izquierda] k

--- cualesSonCaminosDeSalida: Dado un conjunto de caminos y un campo minado,
--                            devuelve todos los caminos del conjunto que son caminos de salida
--                            (al partir de la posición (1, 1), llegan a la posición (n, n) del tablero
--                            sin pisar ninguna mina)
cualesSonCaminosDeSalida :: CampoMinado -> Conjunto Camino -> Conjunto Camino
cualesSonCaminosDeSalida _ [] = []
cualesSonCaminosDeSalida campo (x:xs) 
    | caminoDeSalida campo x = x : caminosRestantes
    | otherwise = caminosRestantes
    where caminosRestantes = (cualesSonCaminosDeSalida campo xs)

--- | PRINCIPAL | ---
--- salidasEnKDesp: Dados un campo minado y un número natural k,
--                  devuelve el conjunto de todos los caminos de longitud k
--                  que lleven a un RAE desde (1, 1) hasta (n, n), sin pisar ninguna mina.
salidasEnKDesp :: CampoMinado -> Integer -> Conjunto Camino 
salidasEnKDesp campo k = cualesSonCaminosDeSalida campo (caminosPosiblesDeLongitud k)

-----------------------------------------------------------------------
--                              PARTE B
-----------------------------------------------------------------------
-------------------------- | Implementación | -------------------------

----------------------- FUNCION 1: recorrido -----------------------
--- | PRINCIPAL | ---
--- recorrido: Dado un tablero y una posición p, devuelve una lista que contiene
--             las posiciones por las que pasará un AF si se lo coloca inicialmente
--             sobre p. Tener en cuenta que puede tratarse de una lista infinita,
--             cuya head es p.
recorrido :: TableroAF -> Posicion -> [Posicion]
recorrido tablero pos 
        | not (posValida tablero pos) = []      -- Si el AF esta fuera del tablero, terminó su recorrido.
        | otherwise = pos : (recorrido tablero siguientePos)
        where flechaEnPosicion = valor tablero pos  
              siguientePos = desplazar pos flechaEnPosicion

----------------------- FUNCION 2: escapaDelTablero -----------------------
--- | DESCRIPCIÓN | ---
-- Dado un tablero y una posición p, determina si al colocar un AF en p, 
-- el AF escapará del tablero o entrará en un loop infinito.

--- | AUXILIARES | ---
-- tieneRepetidosAux: Guarda los elementos de una lista en una auxiliar, para ver si
--                    en la original hay repetidos
tieneRepetidosAux :: Eq a => [a] -> [a] -> Bool
tieneRepetidosAux [] _ = False          -- Una lista vacía nunca tiene repetidos
tieneRepetidosAux (x:xs) y = (x `contenidoEn` y) || (tieneRepetidosAux xs (x:y))

--- tieneRepetidos: Dada una lista de elementos, dice alguno está repetido.
--  Ej. tieneRepetidos [1, 2, 3]    ~> False
--      tieneRepetidos [1, 2, 3, 1] ~> True
tieneRepetidos :: Eq a => [a] -> Bool
tieneRepetidos lista = tieneRepetidosAux lista []

--- | PRINCIPAL | ---
--- escapaDelTablero: Dado un tablero y una posición p, determina si al colocar
--                    un AF en p, el AF escapará del tablero o entrará en un loop infinito.
escapaDelTablero :: TableroAF -> Posicion -> Bool
escapaDelTablero tablero pos = not (tieneRepetidos (recorrido tablero pos))

-- Nota: Con una lista infinita el unico caso en el cual tieneRepetidos no termina,
--       es en el que en esa lista no hay elementos repetidos.
--       En escapaDelTablero eso no se da nunca. Pues si un camino no tiene repetidos,
--       saldría del tablero en una cantidad finita de desplazamientos.

----------------------- FUNCION 3: cantidadDePasosParaSalir -----------------------
--- | DESCRIPCIÓN | ---
-- Dado un tablero y una posición p, devuelve cuántas veces tiene que desplazarse 
-- un AF para escapar del tablero si inicialmente lo colocamos en p. Esto incluye 
-- al último desplazamiento.

--- | AUXILIARES | ---
--- cambiarValor: Dada una lista, un elemento y una posición,
--                Reemplaza el elemento en dicha posición por el dado.
--  Nota: Si el indice es mayor al tamaño de la lista,
--        el elemento será agregado al final.
--  Ej. cambiarValor [1,2,3] 6 2 ~> [1,6,3]
cambiarValor :: [a] -> a -> Integer -> [a]
cambiarValor [] e _ = [e]
cambiarValor (x:xs) e 1 = e : xs
cambiarValor (x:xs) e i = x : (cambiarValor xs e (i - 1))

--- cambiarValorEnMatriz: Dada una matriz, un elemento y una posición,
--                        Reemplaza el elemento en dicha posición por el dado.
--  Ej. cambiarValorEnMatriz [[1, 2, 3],[4, 5, 6],[7, 8, 9]] 0 (2,3) ~> [[1,2,3],[4,5,0],[7,8,9]]
--  Nota: Sabemos que explota para posiciones fuera del tamaño de la matriz, pero no nos interesa
--        ya que no le damos ese uso. (Nunca será necesario cambiar el valor de una posición con
--        el AF fuera del tablero)
cambiarValorEnMatriz :: [[a]] -> a -> Posicion -> [[a]]
cambiarValorEnMatriz matriz e (i, j) = cambiarValor matriz (cambiarValor fila e j) i
    where fila = iesimo matriz i

--- rotarSentidoHorario: Dado un desplazamiento, devuelve ese desplazamiento rotado
--                       en sentido horario.
rotarSentidoHorario :: Desplazamiento -> Desplazamiento
rotarSentidoHorario Arriba    = Derecha
rotarSentidoHorario Derecha   = Abajo
rotarSentidoHorario Abajo     = Izquierda
rotarSentidoHorario Izquierda = Arriba

--- actualizarPosicionEnTablero: Dado un tablero y una posicion,
--                               devuelve un tablero con esa posicion actualizada
--                               (rotada en sentido horario)
actualizarPosicionEnTablero :: TableroAF -> Posicion -> TableroAF
actualizarPosicionEnTablero tablero pos = cambiarValorEnMatriz tablero valorActualizado pos
    where valorActualizado = rotarSentidoHorario (valor tablero pos)


--- | PRINCIPAL | ---
--- cantidadDePasosParaSalir: Dado un tablero y una posición p, devuelve cuántas
--                            veces tiene que desplazarse un AF para escapar del
--                            tablero si inicialmente lo colocamos en p. 
--                            Esto incluye al último desplazamiento.
cantidadDePasosParaSalir :: TableroAF -> Posicion -> Integer
cantidadDePasosParaSalir tablero pos 
    | not (posValida tablero pos) = 0           -- Si el AF ya esta fuera del tablero, termino el recorrido
    | otherwise = 1 + cantidadDePasosParaSalir tableroActualizado siguientePos
    where tableroActualizado = actualizarPosicionEnTablero tablero pos
          flechaEnPosicion = valor tablero pos
          siguientePos = desplazar pos flechaEnPosicion

-----------------------------------------------------------------------
--                               TESTS
-----------------------------------------------------------------------
----------------------- | Elementos de prueba | -----------------------

tableroPrueba :: Tablero Integer
tableroPrueba = [ [0, 0, 0],
                  [0, 0, 0],
                  [0, 0, 0] ]

caminoPrueba1 :: Camino
caminoPrueba1 = [Derecha, Abajo, Derecha, Izquierda, Abajo, Derecha]

campoPrueba :: CampoMinado
campoPrueba = [ [False, False, True],
                [False, True,  True],
                [False, False, False] ]

caminoPrueba2 :: Camino
caminoPrueba2 = [Abajo, Abajo, Derecha, Derecha]

taf3 = [ [Derecha,     Abajo, Abajo],
         [Arriba,  Izquierda, Abajo],
         [Abajo,   Izquierda, Izquierda] ]

----------------------------- | PARTE A | -----------------------------
------ | FUNCION 1 | ------
--- desplazar
testDesplazar :: Bool
testDesplazar =
    desplazar (2, 2) Arriba     == (1, 2) &&
    desplazar (2, 2) Derecha    == (2, 3) &&
    desplazar (2, 2) Abajo      == (3, 2) &&
    desplazar (2, 2) Izquierda  == (2, 1)


--- caminoValidoDesde
testCaminoValidoDesde :: Bool
testCaminoValidoDesde =
    caminoValidoDesde tableroPrueba [] (4, 1)                   == False && -- Comenzando fuera del tablero nunca es un camino valido
    caminoValidoDesde tableroPrueba caminoPrueba1 (1, 1)        == True  &&
    caminoValidoDesde tableroPrueba [Arriba] (1, 1)             == False &&
    caminoValidoDesde tableroPrueba [Izquierda] (1, 1)          == False &&
    caminoValidoDesde tableroPrueba [Abajo] (3, 3)              == False &&
    caminoValidoDesde tableroPrueba [Derecha] (3,3)             == False &&
    caminoValidoDesde tableroPrueba [Derecha, Izquierda] (1, 3) == False    -- Entrar y salir de los limites del tablero no es valido
    
--- caminoValido
testCaminoValido :: Bool
testCaminoValido =
    caminoValido campo1 []                 == True  &&
    caminoValido campo1 [Izquierda]        == False &&
    caminoValido campo1 camino1            == True  &&
    caminoValido campo1 camino2            == True  &&
    caminoValido campo1 (Arriba:camino1)   == False &&
    caminoValido campo1 (camino2++[Abajo]) == False

-- Funcion i) general
testA_i :: Bool
testA_i =
    testDesplazar         &&
    testCaminoValidoDesde &&
    testCaminoValido

------ | FUNCION 2 | ------
--- posicionSalida
testPosicionSalida :: Bool
testPosicionSalida =
    posicionSalida campoPrueba == (3, 3) &&
    posicionSalida []          == (0, 0)

--- hayMina
testHayMina :: Bool
testHayMina = 
    hayMina campoPrueba (1, 1) == False &&
    hayMina campoPrueba (2, 2) == True  &&
    hayMina campoPrueba (3, 3) == False

--- caminoDeSalidaDesde
testCaminoDeSalidaDesde :: Bool
testCaminoDeSalidaDesde =
    caminoDeSalidaDesde campoPrueba caminoPrueba2 (1, 1)                         == True  &&
    caminoDeSalidaDesde campoPrueba [] (3, 3)                                    == True  &&
    caminoDeSalidaDesde campoPrueba [Abajo] (2,3)                                == False &&
    caminoDeSalidaDesde campoPrueba [] (4, 4)                                    == False &&
    caminoDeSalidaDesde campoPrueba (caminoPrueba2++[Izquierda]) (1, 1)          == False &&
    caminoDeSalidaDesde campoPrueba (caminoPrueba2++[Derecha, Izquierda]) (1, 1) == False
    
--- caminoDeSalida
testCaminoDeSalida :: Bool
testCaminoDeSalida =
    caminoDeSalida campo1 camino1 == False &&
    caminoDeSalida campo1 camino2 == True  &&
    caminoDeSalida campo1 camino3 == True

--- Funcion ii) general
testA_ii :: Bool
testA_ii =
    testPosicionSalida      &&
    testHayMina             &&
    testCaminoDeSalidaDesde &&    
    testCaminoDeSalida

------ | FUNCION 3 | ------
--- contenidoEn
testContenidoEn :: Bool
testContenidoEn =
    contenidoEn 1 [1, 2, 3] == True &&
    contenidoEn 0 [1, 2, 3] == False

-- Nota: No hace falta testear caminoSinPosicionesRepetidasDesde ya que la posicion de inicio no afecta
--- caminoSinPosicionesRepetidas
testCaminoSinPosicionesRepetidas :: Bool
testCaminoSinPosicionesRepetidas =
    caminoSinPosicionesRepetidas caminoPrueba1 == False &&
    caminoSinPosicionesRepetidas caminoPrueba2 == True  &&
    caminoSinPosicionesRepetidas camino1       == False &&
    caminoSinPosicionesRepetidas camino2       == True  &&
    caminoSinPosicionesRepetidas camino3       == False

--- testCaminoDeSalidaSinRepetidos
testCaminoDeSalidaSinRepetidos :: Bool
testCaminoDeSalidaSinRepetidos =
    caminoDeSalidaSinRepetidos campo1 camino1 == False &&
    caminoDeSalidaSinRepetidos campo1 camino2 == True  &&
    caminoDeSalidaSinRepetidos campo1 camino3 == False

--- Funcion iii) general
testA_iii :: Bool
testA_iii = 
    testContenidoEn                  &&
    testCaminoSinPosicionesRepetidas &&
    testCaminoDeSalidaSinRepetidos

------ | FUNCION 4 | ------
--- listaDeElementos
testListaDeElementos :: Bool
testListaDeElementos =
    listaDeElementos [1, 2, 3]            == [[1], [2], [3]] &&
    listaDeElementos [Derecha, Izquierda] == [[Derecha], [Izquierda]]

--- agregarATodasLasListas
testAgregarATodasLasListas :: Bool
testAgregarATodasLasListas =
    agregarATodasLasListas 1 [[0], [2]]              == [[1,0], [1,2]] &&
    agregarATodasLasListas 1 [[]]                    == [[1]]          &&
    agregarATodasLasListas 1 []                      == []             &&
    agregarATodasLasListas Derecha [[Izquierda], []] == [[Derecha, Izquierda], [Derecha]]

--- agregarTodos
testAgregarTodos :: Bool
testAgregarTodos =
    agregarTodos [0, 1] [[2],[3],[4]]               == [[0,2],[0,3],[0,4],[1,2],[1,3],[1,4]] &&
    agregarTodos [Derecha, Izquierda] [[Izquierda]] == [[Derecha, Izquierda], [Izquierda, Izquierda]]

--- variaciones
testVariaciones :: Bool
testVariaciones =
    variaciones [4, 7] 2          == [[4,4], [4,7], [7,4], [7,7]] &&
    variaciones [1] 0             == []                           &&
    variaciones [Arriba, Abajo] 2 == [[Arriba,Arriba],[Arriba,Abajo],[Abajo,Arriba],[Abajo,Abajo]]

--- caminosPosiblesDeLongitud
testCaminosPosiblesDeLongitud :: Bool
testCaminosPosiblesDeLongitud =
    caminosPosiblesDeLongitud 1 == [[Arriba], [Derecha], [Abajo], [Izquierda]] &&
    caminosPosiblesDeLongitud 2 == [[Arriba,Arriba],[Arriba,Derecha],[Arriba,Abajo],[Arriba,Izquierda],
                                    [Derecha,Arriba],[Derecha,Derecha],[Derecha,Abajo],[Derecha,Izquierda],
                                    [Abajo,Arriba],[Abajo,Derecha],[Abajo,Abajo],[Abajo,Izquierda],
                                    [Izquierda,Arriba],[Izquierda,Derecha],[Izquierda,Abajo],[Izquierda,Izquierda]]

--- cualesSonCaminosDeSalida
testCualesSonCaminosDeSalida :: Bool
testCualesSonCaminosDeSalida =
    cualesSonCaminosDeSalida campoPrueba []                                                   == []              &&
    cualesSonCaminosDeSalida campoPrueba [caminoPrueba2, [Abajo]]                             == [caminoPrueba2] &&
    cualesSonCaminosDeSalida campoPrueba [caminoPrueba2, caminoPrueba2++[Izquierda, Derecha]] == [caminoPrueba2, caminoPrueba2++[Izquierda, Derecha]]

--- salidasEnKDesp
testSalidasEnKDesp :: Bool
testSalidasEnKDesp =
    salidasEnKDesp campo1 1 == []                              &&
    salidasEnKDesp campo1 2 == []                              &&
    salidasEnKDesp campo1 3 == []                              &&
    salidasEnKDesp campo1 4 == [[Derecha,Abajo,Derecha,Abajo]] &&
    salidasEnKDesp campo1 5 == []                              &&
    salidasEnKDesp campo1 6 == [[Derecha,Abajo,Arriba,Abajo,Derecha,Abajo], -- Nota, el orden de los caminos fue cambiado con respecto al del documento
                                [Derecha,Abajo,Derecha,Abajo,Arriba,Abajo],
                                [Derecha,Abajo,Derecha,Izquierda,Derecha,Abajo],
                                [Derecha,Izquierda,Derecha,Abajo,Derecha,Abajo]]

--- Funcion iv) general
testA_iv :: Bool
testA_iv =
    testSalidasEnKDesp            &&
    testCualesSonCaminosDeSalida  &&
    testCaminosPosiblesDeLongitud &&
    testVariaciones               &&
    testAgregarTodos              &&
    testAgregarATodasLasListas    &&
    testListaDeElementos


------ | GENERAL A | ------
testA :: Bool
testA =
    testA_i   &&
    testA_ii  &&
    testA_iii &&
    testA_iv

----------------------------- | PARTE B | -----------------------------
------ | FUNCION 1 | ------
--- recorrido
testRecorrido :: Bool
testRecorrido =
    recorrido taf1 (3,3)          == [(3,3)]                         &&
    recorrido taf1 (1,1)          == [(1,1),(1,2),(1,3),(2,3),(3,3)] &&
    recorrido taf2 (3,2)          == [(3,2),(3,1)]                   &&
    take 5 (recorrido taf2 (1,1)) == [(1,1),(1,2),(2,2),(2,1),(1,1)] &&
    recorrido taf1 (4,4)          == []                              &&
    recorrido taf2 (3,1)          == [(3,1)]                         &&
    recorrido taf1 (2,1)          == [(2,1), (1,1), (1,2), (1,3), (2,3), (3,3)]

--- Funcion i) general
testB_i :: Bool
testB_i =
    testRecorrido

------ | FUNCION 2 | ------
--- tieneRepetidos
testTieneRepetidos :: Bool
testTieneRepetidos =
    tieneRepetidos [1,2,3]                      == False &&
    tieneRepetidos [1, 2, 3, 1]                 == True  &&
    tieneRepetidos [(1,1), (2,1), (1,1)]        == True  &&
    tieneRepetidos [(1,1), (2,1), (2,2), (1,2)] == False
-- Consideramos que no es necesario testear tieneRepetidosAux

--- escapaDelTablero
testEscapaDelTablero :: Bool
testEscapaDelTablero =
    escapaDelTablero taf1 (2,2) == True  &&
    escapaDelTablero taf2 (1,3) == True  &&
    escapaDelTablero taf2 (1,1) == False &&
    escapaDelTablero taf3 (1,3) == True  &&
    escapaDelTablero taf3 (1,1) == False &&
    escapaDelTablero taf3 (4,4) == True  &&
    escapaDelTablero taf3 (2,2) == False

--- Funcion ii) general
testB_ii :: Bool
testB_ii =
    testTieneRepetidos &&
    testEscapaDelTablero

------ | FUNCION 3 | ------
--- cambiarValor
testCambiarValor :: Bool
testCambiarValor =
    cambiarValor [1,2,3] 6 2                          == [1,6,3]   &&
    cambiarValor [1,2,3] 5 100                        == [1,2,3,5] &&
    cambiarValor [] 5 2                               == [5]       &&
    cambiarValor [Derecha, Izquierda, Arriba] Abajo 2 == [Derecha, Abajo, Arriba]


--- cambiarValorEnMatriz
testCambiarValorEnMatriz :: Bool
testCambiarValorEnMatriz =
    cambiarValorEnMatriz [[1, 2, 3],[4, 5, 6],[7, 8, 9]] 0 (2,3) == [[1,2,3],[4,5,0],[7,8,9]]    &&
    cambiarValorEnMatriz [[1,1], [2,2], [3,3], [4,4]] 9 (3,2)    == [[1,1], [2,2], [3,9], [4,4]] &&
    cambiarValorEnMatriz taf1 Derecha (1,3) == [ [Derecha,  Derecha, Derecha],
                                                 [Arriba, Izquierda, Abajo],
                                                 [Arriba, Izquierda, Abajo] ]

--- rotarSentidoHorario
testRotarSentidoHorario :: Bool
testRotarSentidoHorario =
    rotarSentidoHorario Arriba    == Derecha   &&
    rotarSentidoHorario Derecha   == Abajo     &&
    rotarSentidoHorario Abajo     == Izquierda &&
    rotarSentidoHorario Izquierda == Arriba

--- actualizarPosicionEnTablero
testActualizarPosicionEnTablero :: Bool
testActualizarPosicionEnTablero =
    actualizarPosicionEnTablero taf1 (1,3) == [ [Derecha,  Derecha, Izquierda],
                                                [Arriba, Izquierda, Abajo],
                                                [Arriba, Izquierda, Abajo] ] &&
    actualizarPosicionEnTablero taf2 (2,1) == [ [Derecha,        Abajo, Abajo],
                                                [Derecha,    Izquierda, Abajo],
                                                [Izquierda,  Izquierda, Izquierda] ]
--- cantidadDePasosParaSalir
testCantidadDePasosParaSalir :: Bool
testCantidadDePasosParaSalir =
    cantidadDePasosParaSalir taf2 (3,3) == 3 &&
    cantidadDePasosParaSalir taf2 (1,1) == 9 &&
    cantidadDePasosParaSalir taf3 (1,3) == 5 &&
    cantidadDePasosParaSalir taf3 (1,1) == 9

--- Funcion iii) general
testB_iii :: Bool
testB_iii =
    testCambiarValor                &&
    testCambiarValorEnMatriz        &&
    testRotarSentidoHorario         &&
    testActualizarPosicionEnTablero &&
    testCantidadDePasosParaSalir

------ | GENERAL B | ------
testB :: Bool
testB =
    testB_i  &&
    testB_ii &&
    testB_iii

----------------------------- | GENERAL | -----------------------------
--- TODOS los tests
test :: Bool
test = 
    testA &&
    testB