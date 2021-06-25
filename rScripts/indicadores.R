library(tidyverse)
library(leaflet)
library(sf)

#capas geo. Dejar todo en 4326 y GKBA 
radios <- st_read("data/indicadores/radios.gpkg") #4326
et <- st_read("data/et.gpkg") #posgar 2007 faja 6

encuestas <- st_read("data/encuestas.gpkg") #Argentina GKBsAs
espaciosVerdes <- st_read("data/espaciosVerdes.gpkg")  #posgar 2007 faja 6
manzanas <- st_read("data/manzanas.gpkg") #4326


#Sistema de Coordenadas de Buenos Aires
ArgentinaGKBuenosAires <- st_crs(encuestas) #sirve para st_transform

radios_gk <- st_transform(radios,ArgentinaGKBuenosAires)
et <- st_transform(et, 4326)
et_gk <- st_transform(et,ArgentinaGKBuenosAires)
encuestas <- st_transform(encuestas,4326)
encuestas_gk <- st_transform(encuestas,ArgentinaGKBuenosAires)
espaciosVerdes <- st_transform(espaciosVerdes,4326)
espaciosVerdes_gk <- st_transform(espaciosVerdes,ArgentinaGKBuenosAires)
manzanas_gk <- st_transform(manzanas,ArgentinaGKBuenosAires)

##########################################################
#Esto lo hacemos para saber que radios pertenecen a cada et. Se calculo en base al centroide.
etBuffer_gk <- st_buffer(et_gk,1500) #BUFFER 1500 CHEQUEAR SI ES 1500 o 1200
radiosCentroides_gk <- st_centroid(radios_gk)

interseccion <- st_intersection(radiosCentroides_gk,etBuffer_gk)
interseccion <- interseccion[,c(84,5)]
interseccion <- interseccion %>% as.data.frame() %>% select(-geom)

radiosET <-left_join(radios,interseccion,  keep=TRUE) 

rm(interseccion, etBuffer_gk,radiosCentroides_gk) #Borramos Basura

#Visualizador
 leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=) 



#ESPACIOS VERDES
 
#SAU1 - Superficie de Espacios verdes en toda el área de influencia por cantidad de habitantes
 espaciosVerdesArea_gk <- st_area(espaciosVerdes_gk)
 espaciosVerdes_gk["area_2021"] <-as.numeric( espaciosVerdesArea_gk)
 verdes <- st_intersection(st_centroid(espaciosVerdes_gk),radios_gk[,5]) %>%
            as.data.frame() %>% select(-geom) %>% group_by(REDCODE) %>% summarise(verdes=sum(area_2021))

 SUA1 <- radiosET[,5]
 
 #################### SAU 2

#hay que calcular la distancia de cada centroide de manzana a espacio verde, luego promediar elcentroide de manzana segun el radio
#al que pertenecen

manzanasCentroides_gk <- st_centroid(manzanas_gk)
espaciosVerdesCentroides_gk <- st_centroid(espaciosVerdes_gk)


#Función para seleccionar la distancia mínima. Esto me sirve para cuando uso el st_distance
distancia1FUN <- function(x) {
  ave <- sort(x)[1]
  return(ave [1])
}



distanciaMinima <- st_distance(manzanasCentroides_gk, espaciosVerdesCentroides_gk)
distanciaMinimaMaxima <- apply(distanciaMinima,1,distancia1FUN)

manzanas["distanciaVerdes"] <- round(distanciaMinimaMaxima,2) 

a <- st_intersection(st_centroid(manzanas),radiosET[,c(5,83)] )%>% as.data.frame()%>% select(-geom)
manzanas <- left_join(manzanas,a)
rm(a)



distEspVerdes <- manzanas %>% as.data.frame() %>% select(-geom) %>% 
                          group_by(REDCODE.x,Nombre) %>% summarise(promedio=mean(distanciaVerdes)) %>%
                          mutate(SAU2= case_when(
                            promedio<500 ~1,
                            promedio<750 ~0.5,
                            promedio<1000 ~.25,
                            promedio>1000 ~0
                                                     ))





st_write(manzanas[,-1], 'manzanasBorrar.gpkg')



#ET FLORES

radios <- radios %>% filter(et== 1, nombre=='Flores')

iseDemografia <- radios[,c(5,12)]

iseDemografia <- iseDemografia %>% mutate(demografia= P_TOTAL/(max(P_TOTAL)))

