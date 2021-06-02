module Library where
import PdePreludat



data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b


--1) Modelado de palos de golf  y obstaculos 

type Palo = Habilidad->Tiro

paloPutter :: Habilidad->Tiro
paloPutter habilidad = UnTiro{velocidad=10, precision=2* (precisionJugador habilidad), altura=0}

paloMadera:: Habilidad->Tiro
paloMadera habilidad = UnTiro{velocidad=100, precision= (precisionJugador habilidad)/2, altura=5    }

paloHierro ::Number->Habilidad->Tiro
paloHierro n habilidad= UnTiro{velocidad= (fuerzaJugador habilidad)*3, precision= (precisionJugador habilidad)/n, altura= max 0 (n-3)}

--Para probar
paloPrueba = Habilidad {
  fuerzaJugador =4
, precisionJugador = 5
} 
--Otra forma es poner en consola ej : paloPutter (habilidad bart)...

--b) 

type Palos= [Palo]

--Fijara despues si se puede restringir n como las vocales



todosPalos :: Palos
todosPalos= [paloPutter, paloMadera] ++ map paloHierro [1..10] --uno las lista de palos q no requieren ingreso de otras variables
--con la lista de los diez palos de hierro ya que en este varia "n" (entre 1 y 10) map para crear una lista de 10 funciones.



--2)

golpe :: Jugador->Palo->Tiro

golpe aJugador aPalo = aPalo ( habilidad aJugador)


--Es diferente al tipo de lo q pide la consigana, puede ser una ventaja o no (todo depende de lo que te pidan desp)
golpe2 :: Palo->Jugador->Tiro
golpe2 aPalo = aPalo.habilidad


--3)

tiroDetenido = UnTiro {
  velocidad = 0,
  precision =0,
  altura =0
} 

tiroPrueba = UnTiro {
  velocidad = 15,
  precision =200,
  altura =6
} 

--SOLUCION PLANTEADA:

{-

type Obs= Tiro->Tiro
tunelRampa :: Tiro->Tiro
tunelRampa tiro | condicionTunelRampa tiro = efecto1 tiro
                | otherwise = tiroDetenido
condicionTunelRampa :: Tiro->Bool
condicionTunelRampa tiro = precision tiro > 90
efecto1:: Tiro->Tiro
efecto1 tiro=  UnTiro{velocidad= 2*(velocidad tiro), precision= 100, altura= 0}

laguna :: Number->Tiro->Tiro
laguna largo tiro  | condicionLaguna tiro = efecto2 largo tiro
                   | otherwise = tiroDetenido
condicionLaguna ::Tiro->Bool
condicionLaguna tiro =velocidad tiro >80 && between 1 5 (altura tiro)
efecto2 :: Number->Tiro->Tiro
efecto2 largo tiro = UnTiro{velocidad = velocidad tiro, precision = precision tiro, altura= (altura tiro)/ largo}

hoyo :: Tiro->Tiro
hoyo tiro | condicionHoyo tiro = UnTiro{velocidad= 0, precision = 0, altura=0}
          | otherwise= tiroDetenido
condicionHoyo :: Tiro->Bool
condicionHoyo tiro = between 5 20 (velocidad tiro) && precision tiro >95
-}

--CUMPLE PERO hay mucha logica repetida en las condiciones y efectos


--Parametrizamos la  condicion y el efecto (las guardas) q eran lo que se nos repetian en el laguna y tunelRampa
obstaculoSuperableSi :: (Tiro->Bool)->(Tiro->Tiro)->Tiro->Tiro
obstaculoSuperableSi condiciontuneles efecto tiro | condiciontuneles tiro = efecto tiro    
                                                  | otherwise = tiroDetenido

--Entonces ahora puedo escribir al tunelRampa y laguna de la siguiente manera:

--Aplicacion parcial con el tiro


--
tunelConRampa :: Tiro->Tiro
tunelConRampa  = obstaculoSuperableSi condicionTunelRampa efectoTunelRampa 

condicionTunelRampa :: Tiro->Bool
condicionTunelRampa tiro = precision tiro > 90 && rasdelSuelo tiro
rasdelSuelo = (0==).altura

efectoTunelRampa :: Tiro->Tiro
efectoTunelRampa tiro = UnTiro{velocidad= 2*(velocidad tiro), precision= 100, altura= 0}
--
laguna :: Number->Tiro->Tiro
laguna largo = obstaculoSuperableSi condicionLaguna (efectoLaguna largo)

condicionLaguna ::Tiro->Bool
condicionLaguna tiro =velocidad tiro >80 && between 1 5 (altura tiro)

efectoLaguna :: Number->Tiro->Tiro
efectoLaguna largo tiro = UnTiro{velocidad = velocidad tiro, precision = precision tiro, altura= (altura tiro)/ largo}

--
hoyo :: Tiro->Tiro
hoyo  = obstaculoSuperableSi condicionHoyo efectoHoyo

condicionHoyo :: Tiro->Bool
condicionHoyo tiro = between 5 20 (velocidad tiro) && precision tiro >95 && rasdelSuelo tiro
efectoHoyo:: Tiro->Tiro
efectoHoyo _ = tiroDetenido


--Nota: se puede crear un type Obstaculo = Tiro->Tiro



