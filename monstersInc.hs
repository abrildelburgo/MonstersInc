											-- PUNTO 1
type Grito = ( String , Int , Bool )
type Asustador = Nino -> Grito

energiaDeGrito (onomatopeya,intensidad,mojoLaCama) 
		| mojoLaCama = (nivelDeTerror onomatopeya) * intensidad ^ 2  
		| otherwise = (nivelDeTerror onomatopeya)* 3 + intensidad

nivelDeTerror :: String -> Int
nivelDeTerror = length

{-
energiaDeGrito (onomatopeya,intensidad,mojoLaCama) 
		| mojoLaCama = nivelDeTerror * intensidad ^ 2  
		| otherwise = nivelDeTerror * 3 + intensidad
			where nivelDeTerror = length onomatopeya
-}


											-- PUNTO 2
{-
type Nino = (String, Int, Float)

sullivan :: Nino -> Grito
sullivan (nombre,edad,altura) = ( generarGrito, generarIntensidad, edad < 3 )
	where generarGrito = replicate (length nombre) 'A' ++ "GH"
		  generarIntensidad = div 20 edad
-}

type Nino = (String, Int, Float)

nombre (nombreNino,_,_) = nombreNino
edad (_,edadNino,_) = edadNino
altura (_,_,alturaNino) = alturaNino

sullivan :: Asustador
sullivan chico = (generarGrito,generarIntensidad,generarMojaLaCama)
	where generarGrito = replicate (length nombre chico) 'A' ++ "GH"
		  generarIntensidad = div 20 edad chico
		  generarMojaLaCama = edad chico < 3      -- PUEDO PONERLO DIRECTO EN LA TERNA DE ARRIBA????????

randall :: Asustador
randall chico = ("MAMADERA", generarIntensidad, generarMojaLaCama) 
	where generarIntensidad = (length.listaDeVocales nombre) chico        -- SE PUEDE HACER UN WHERE DE OTRO WHERE?????????????
		  generarMojaLaCama = (altura chico >= 0.8) && (altura chico <= 1.2)

listaDeVocales :: String -> String  --ESTA BIEN TIPADO????????????
listaDeVocales = filter esVocal
	where esVocal = flip elem "AEIOUaeiou"

chuckNorris :: Asustador
chuckNorris chico = ("abcdefghijklmnopqrstuovwxyz",100,True)

osoCarinoso :: Asustador
osoCarinoso chico = ("uf", edad chico, False)


 										 -- PUNTO 3
{- 
pam :: [b->a] -> b -> [a]
pam [] _ = []
pam (x:xs) elemento = [x elemento] ++ pam xs elemento 
-}

pam :: [b->a] -> b -> [a]
pam listaFunciones elemento = map (/funcion -> funcion elemento) listaFunciones


 										 -- PUNTO 4
conjuntoDeGritos :: [Asustador] -> Nino -> [Grito]
conjuntoDeGritos = pam                             --    SI TENGO ESTO SE PUEDEN SACAR LOS DOS PARAMETROS?
												   --    funcionEjemplo elemento lista = filter elemento lista    ===>   funcion = filter    ?????


 										 -- PUNTO 5
produccionEnergeticaGritos :: [Asustador] -> [Nino] -> Int
produccionEnergeticaGritos = sumatoriaDeEnergias.asustarAlCampamento

asustarAlCampamento :: [Asustador] -> [Nino] -> [Grito]
asustarAlCampamento asustadores campamento = (concat.map (/nino -> pam asustadores nino)) campamento

sumatoriaDeEnergiasDeGrito :: [Grito] -> Int
sumatoriaDeEnergiasDeGrito listaDeGritosCampamento = (sum. map energiaDeGrito) listaDeGritosCampamento


 										 -- PUNTO 6
 type Risa = (Float,Int)
 duracion (duracionDeLaRisa,_) = duracionDeLaRisa     -- duracion = fst ?????????
 intensidad (_,intensidadDeLaRisa) = intensidadDeLaRisa

energiaDeUnaRisa :: Risa -> Int
energiaDeUnaRisa unaRisa = duracion unaRisa ^ intensidad unaRisa

type Comediante = Nino -> Risa

capusotto :: Comediante
capusotto nino = (generarDuracion, generarIntensidad)
	where generarDuracion = 2 * edad nino
		  generarIntensidad =                     -- QUE INTENTA DECIR ACA?????????

produccionEnergeticaRisas :: [Comediante] -> [Nino] -> Int
produccionEnergeticaRisas = sumatoriaDeRisas.risasDelCampamento

risasDelCampamento :: [Comediante] -> [Nino] -> [Risa]
risasDelCampamento comediantes campamento = (concat.map (/nino -> pam comediantes nino)) campamento

sumatoriaDeEnergiasDeRisas :: [Risa] -> Int
sumatoriaDeEnergiasDeRisas listaDeRisasCampamento = (sum.map energiaDeUnaRisa) listaDeRisasCampamento


										 -- PUNTO 7
produccionEnergetica :: -----       ??????????????????????????????
produccionEnergetica listaDeTrabajadores campamento = (sumatoriaDeEnergias.reaccionDelCampamento listaDeTrabajadores) campamento    --PF
reaccionDelCampamento listaDeTrabajadores campamento = (concat.map (/nino -> pam listaDeTrabajadores nino)) campamento
sumatoriaDeEnergias listaDeReacciones = (sum.map energiaDeUnaReaccion) listaDeReacciones        -- Y ENERGIADEUNAREACCION QUE ONDAAAAAAAAA