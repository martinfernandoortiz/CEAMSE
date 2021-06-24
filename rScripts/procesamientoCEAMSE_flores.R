library(tidyverse)
library(readxl)
library(splitstackshape)
setwd("~/Github/CTBA/CEAMSE")
#tabla <- read_excel("C:/Users/GCBA/Downloads/Copia de Monitoreo Social CEAMSE (Responses).xlsx")
#setwd("C:/Users/GCBA/Downloads/procesamiento")
tabla <- read_excel("Copia de Monitoreo Social CEAMSE (Responses).xlsx", 
                                                          sheet = "Sheet4")



#renombres
names(tabla)[1:79] <- c("Timestamp",
                        "edad",
                        "genero",
                        "estudiaActualmente",
                        "estudiosMaximos",
                        "situacionLaboral",
                        "situacionLaboral_DET",
                        "beneficiarioPlanSocial",
                        "beneficiarioPlanSocial_DET",
                        "organizacionesSociales",
                        "organizacionesSociales_DET",
                        "organizacionesParticipa",
                        "conoceETCeamse",
                        "distanciaET",
                        "actividadesETConoce",
                        "actividadesETConoce_DET",
                        "camionesMomentoTransito",
                        "camionesTipo",
                        "camionesMolestiaET",
                        "camionesMolestiaET_DET",
                        "oloresSintioET",
                        "oloresMomentoET",
                        "oloresMolestiaET",
                        "oloresMolestiaET_DET",
                        "ruidosET",
                        "ruidosETMomento",
                        "ruidosMolestiaET",
                        "ruidosMolestiaET_DET",
                        "vibraciones",
                        "vibracionesProveniencia",
                        "vibracionesProveniencia_DET",
                        "error",
                        "vibracionesMolestia",
                        "vibracionesMolestia_DET",
                        "beneficiosET",
                        "actividadesConsultadoET",
                        "actividadesConsultadoET_DET",
                        "problemasSaludFrecuentes",
                        "problemasSaludFrecuentes_DET",
                        "consultaMedicaAsiste",
                        "consultaMedicaAsiste_DET",
                        "aireCalidad",
                        "fuentesContaminacion",
                        "fuentesContaminacion_DET",
                        "eventoAfectadoCalidadAire",
                        "eventoAfectadoCalidadAire_DET",
                        "inundaciones",
                        "inundaciones_DET",
                        "olorZanjas",
                        "olorZanjas_DET",
                        "emergenciaInundacionesInstitucion",
                        "emergenciaInundacionesInstitucion_DET",
                        "contaminacionSonora",
                        "contaminacionSonoraMomento",
                        "transitoCongestion",
                        "transitoCongestion_DET",
                        "transitoCongestionMomento",
                        "transitoAccidentesFrecuencia",
                        "transitoAccidentesFrecuencia_DET",
                        "residuosManejo",
                        "residuosContenedores",
                        "residuosContenedoresDificultad",
                        "residuosContenedoresDificultad_DET",
                        "residuosRecoleccionFrecuencia",
                        "cartonerosCooperativa",
                        "cartonerosTarea",
                        "cartonerosTarea_DET",
                        "residuosReclamos",
                        "residuosReciclablesDonde",
                        "residuosSeparacion",
                        "ablCalidad",
                        "ablCalidad_DETalle",
                        "entrevistador",
                        "zona",
                        "zonacodigo",
                        "encuestado",
                        "describa",
                        "sabeQueEsET",
                        "sabeQueEsET_DET")
tabla %>% group_by(zona) %>% count()
tabla <- tabla %>% filter(zona== 'ET Flores')
#Tabla Principal
tablaPrincipal <- tabla[,c(1:9,13:16,73:76)]
#write_csv(tablaPrincipal,"principal.csv")

tablaPrueba <- tabla[,c(1,10,11)]

orgas <- cSplit(tablaPrueba, "organizacionesSociales", ",", "long")






#Camiones

camiones <- tabla[,c(1,17:20)]

camionesLong <- cSplit(camiones, "camionesMomentoTransito", ",", "long")

#write_csv(camiones,"camiones.csv")
#write_csv(camionesLong,"camionesLong.csv")

#Olores

olores <- tabla[,c(1,21:24)]

oloresLong <- cSplit(olores, "oloresMomentoET", ",", "long")

#write_csv(olores,"olores.csv")
#write_csv(oloresLong,"oloresLong.csv")


#Organizaciones

organizaciones <- tabla[,c(1,10:12)]

organizacionesLong <- cSplit(organizaciones, "organizacionesSociales", ",", "long")

#write_csv(organizaciones,"organizaciones.csv")
#write_csv(organizacionesLong,"organizacionesLong.csv")



#Ruidos

ruidos <- tabla[,c(1,25:28)]

ruidosLong <- cSplit(ruidos, "ruidosETMomento", ",", "long")

#write_csv(ruidos,"ruidos.csv")
#write_csv(ruidosLong,"ruidosLong.csv")


#Vibraciones

vibraciones <- tabla[,c(1,29:33)]

vibracionesLong <- cSplit(vibraciones, "vibracionesProveniencia", ",", "long")

#write_csv(vibraciones,"vibraciones.csv")
#write_csv(vibracionesLong,"vibracionesLong.csv")

#Beneficios

beneficios <- tabla[,c(1,35:37)]

beneficiosLong <- cSplit(beneficios, "beneficiosET", ",", "long")

#write_csv(beneficios,"beneficios.csv")
#write_csv(beneficiosLong,"beneficiosLong.csv")


#Salud

salud <- tabla[,c(1,38:41)]

saludLong <- cSplit(salud, "problemasSaludFrecuentes", ",", "long")
saludLong1 <- cSplit(salud, "consultaMedicaAsiste", ",", "long")
#write_csv(salud,"salud.csv")
#write_csv(saludLong,"saludLong.csv")
#write_csv(saludLong1,"saludLong1.csv")
#Aire



aire <- tabla[,c(1,42:46)]

aireLong <- cSplit(aire, "fuentesContaminacion", ",", "long")

#write_csv(aire,"aire.csv")
#write_csv(aireLong,"aireLong.csv")


#Inundaciones

inundaciones <- tabla[,c(1,47:50)]

#write_csv(inundaciones,"inundaciones.csv")

#Emergencias

emergencias <- tabla[,c(1,51:52)]

#emergenciasLong <- cSplit(emergencias, "fuentesContaminacion", ",", "long")

#write_csv(emergencias,"emergencias.csv")
##write_csv(emergenciasLong,"emergenciasLong.csv")




#Ruidos

sonora <- tabla[,c(1,53:54)]

sonoraLong <- cSplit(sonora, "contaminacionSonora", ",", "long")

#write_csv(sonora,"sonora.csv")
#write_csv(sonoraLong,"sonoraLong.csv")

#Transito

transito <- tabla[,c(1,55:59)]
#transitoLong <- cSplit(transito, "contaminacionSonora", ",", "long")

#write_csv(transito,"transito.csv")
##write_csv(transitoLong,"transitoLong.csv")


