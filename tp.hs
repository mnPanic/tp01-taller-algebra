-----------------------------------------------------------------------
-- Nombre y LU/DNI de los integrantes del grupo:
-- INTEGRANTE 1: Manuel Panichelli 72/18
-- INTEGRANTE 2: Vladimir Pomsztein 364/18
-- INTEGRANTE 3: Ignacio Alonso Rehor 195/18
-----------------------------------------------------------------------

-----------------------------------------------------------------------
-- Definiciones, funciones y ejemplos dados                          
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
    
    
-- Funciones de ejemplo, solo para ilustrar cómo usar los tipos definidos arriba.
-- Determina si un desplazamiento es vertical (Arriba o Abajo).
esVertical :: Desplazamiento -> Bool
esVertical Arriba = True
esVertical Abajo = True
esVertical _ = False

-- Cuenta la cantidad de Desplazamientos verticales en un Camino.
contarDesplazamientosVerticales :: Camino -> Integer
contarDesplazamientosVerticales [] = 0
contarDesplazamientosVerticales (x:xs) | esVertical x = 1 + resto
                                       | otherwise    = resto
  where resto = contarDesplazamientosVerticales xs

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
------------------- | Implementación de funciones | -------------------

----------------------- FUNCION 1: caminoValido -----------------------

-- desplazar: Dada una posición y un desplazamiento devuelve la posición resultante.
desplazar :: Posicion -> Desplazamiento -> Posicion
desplazar (a, b) Arriba    = (a - 1, b)
desplazar (a, b) Derecha   = (a, b + 1)
desplazar (a, b) Abajo     = (a + 1, b)
desplazar (a, b) Izquierda = (a, b - 1)

-- Test de ejemplo
testDesplazar :: Bool
testDesplazar =
    desplazar (2, 2) Arriba     == (1, 2) &&
    desplazar (2, 2) Derecha    == (2, 3) &&
    desplazar (2, 2) Abajo      == (3, 2) &&
    desplazar (2, 2) Izquierda  == (2, 1)

-- caminoValidoDesde: Determina si un camino se mantiene dentro de los
--                    límites de un tablero a lo largo de su trayectoria,
--                    comenzando en la posición dada.
caminoValidoDesde :: Tablero a -> Camino -> Posicion -> Bool
caminoValidoDesde tablero [] pos = posValida tablero pos
caminoValidoDesde tablero (d:ds) pos = (posValida tablero pos) && (caminoValidoDesde tablero ds siguientePos)
    where siguientePos = desplazar pos d

--- caminoValido: Determina si un camino se mantiene dentro de los límites
--                del tablero a lo largo de su trayectoria, 
--                asumiendo que se comenzará por la posición (1, 1).
caminoValido :: Tablero a -> Camino -> Bool
caminoValido tablero camino = caminoValidoDesde tablero camino (1, 1)


----------------------- FUNCION 2: caminoDeSalida -----------------------

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

--- caminoDeSalida: Determina si un RAE, comenzando en la posición (1, 1), al seguir el camino dado, 
--                  llega a la posición (n, n) sin pisar ninguna mina.
caminoDeSalida :: CampoMinado -> Camino -> Bool
caminoDeSalida campo camino = caminoDeSalidaDesde campo camino (1, 1)


----------------------- FUNCION 3: caminoDeSalidaSinRepetidos -----------------------

--- contiene: Dado una lista de elementos y un elemento, dice si ese elemento pertenece a la lista.
contenidoEn :: Eq a => a -> [a] -> Bool
contenidoEn n l = elem n l

--- caminoSinPosicionesRepetidasDesde: Dado un camino, una posicion desde la cual comenzar, 
--                                     y un conjunto de posiciones recorridas,
--                                     se fija si ese camino tendra posiciones repetidas.
caminoSinPosicionesRepetidasDesde :: Camino -> Posicion -> Conjunto Posicion -> Bool
caminoSinPosicionesRepetidasDesde [] _ _ = True
caminoSinPosicionesRepetidasDesde (d:ds) pos posiciones = (not (siguientePos `contenidoEn` posiciones)) && (caminoSinPosicionesRepetidasDesde ds siguientePos (pos:posiciones))
    where siguientePos = desplazar pos d
    -- TODO: ponerle un where

--- caminoSinPosicionesRepetidas: Dado un camino, se fija si tiene posiciones repetidas.
caminoSinPosicionesRepetidas :: Camino -> Bool
caminoSinPosicionesRepetidas camino = caminoSinPosicionesRepetidasDesde camino (1, 1) []

--- caminoDeSalidaSinRepetidos: Determina si un RAE, comenzando en la posición (1, 1), al seguir el camino dado,
--                              llega a la posición (n, n) sin pisar ninguna mina y sin pasar dos veces por una
--                              misma posición.
caminoDeSalidaSinRepetidos :: CampoMinado -> Camino -> Bool
caminoDeSalidaSinRepetidos campo camino = (caminoDeSalida campo camino) && (caminoSinPosicionesRepetidas camino)

-----------------------------------------------------------------------
--                              PARTE B
-----------------------------------------------------------------------
------------------- | Implementación de funciones | -------------------
