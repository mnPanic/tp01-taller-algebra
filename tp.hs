-----------------------------------------------------------------------
-- Nombre y LU/DNI de los integrantes del grupo:
-- INTEGRANTE 1: Manuel Panichelli 72/18
-- INTEGRANTE 2: Vladimir Pomsztein 364/18
-- INTEGRANTE 3: Ignacio Alonso Rehor 195/18
-----------------------------------------------------------------------

data Desplazamiento = Arriba | Abajo | Izquierda | Derecha deriving (Show, Eq)

type Conjunto a = [a]
type Camino = [Desplazamiento]
type Posicion = (Integer,Integer)
type Tablero a = [[a]]
type CampoMinado = Tablero Bool

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


-----------------------------------------------------------------------
-- PARTE A
-----------------------------------------------------------------------
-- funcion 1

-- desplazar: Dada una posición y un desplazamiento devuelve la posición resultante.
desplazar :: Posicion -> Desplazamiento -> Posicion
desplazar (a, b) Arriba    = (a - 1, b)
desplazar (a, b) Derecha   = (a, b + 1)
desplazar (a, b) Abajo     = (a + 1, b)
desplazar (a, b) Izquierda = (a, b - 1)

-- caminoValidoComenzandoEn: Determina si un camino se mantiene dentro de los límites de un tablero a lo largo de su trayectoria,
--                           comenzando en la posición dada.
caminoValidoComenzandoEn :: Tablero a -> Camino -> Posicion -> Bool
caminoValidoComenzandoEn tablero [] pos = posValida tablero pos
caminoValidoComenzandoEn tablero (d:ds) pos = (posValida tablero pos) && (caminoValidoComenzandoEn tablero ds siguientePosicion)
    where siguientePosicion = desplazar pos d

-- Determina si un camino se mantiene dentro de los límites del tablero a lo largo de su trayectoria, 
-- asumiendo que se comenzará por la posición (1, 1).
caminoValido :: Tablero a -> Camino -> Bool
caminoValido tablero camino = caminoValidoComenzandoEn tablero camino (1, 1)


-----------------------------------------------------------------------
-- PARTE B
-----------------------------------------------------------------------

type TableroAF = Tablero Desplazamiento

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