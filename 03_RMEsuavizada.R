#SUAVIZADO DE RME (RR suavizado)

#Se sigue la metodologia propuesta en: https://github.com/Epi-Emma/Firearm_Suicide_Disease_Mapping

#Librerias 
library(INLA)
library(sf)
library(spdep)
library(dplyr)
library(purrr)

#Carga de archivos
load("paper_1/data_rme.rda")

#Aplicar funcion a cada trienio
datos_trienio_1 <- suavizar_rme(data_trienio = data_rme[data_rme$TRIENIO == "2000-2002",], data_tdf = data_rme)
datos_trienio_2 <- suavizar_rme(data_trienio = data_rme[data_rme$TRIENIO == "2009-2011",], data_tdf = data_rme)
datos_trienio_3 <- suavizar_rme(data_trienio = data_rme[data_rme$TRIENIO == "2021-2023",], data_tdf = data_rme)

#Unir datos de los tres trienios

data_rme_suavizada <- rbind(datos_trienio_1,datos_trienio_2,datos_trienio_3)

#======================================================
#BASES DE RME SUAVIZADAS PARA MAPAS CON ESCALAS COMUNES
#======================================================

#Estimo cuartiles comunes a las RME de los tres trienios
data_rme_suavizada <- cuartiles(data_rme_suavizada, "RME_SUAVIZADA")

# Paleta de colores para cuartiles RME suavizada
paleta <- aplicar_paleta("cuartil_RME_SUAVIZADA")

#Asigno colores a los cuartiles

data_rme_suavizada <- data_rme_suavizada %>% 
  left_join(paleta, by = "cuartil_RME_SUAVIZADA")

#Calculo valores mínimos y máximos de cuartiles para leyenda del mapa
minMax_RME_SUAVIZADA = obtener_minmax(data_rme_suavizada, "RME_SUAVIZADA")

#Uno leyendas al dataframe
data_rme_suavizada <- data_rme_suavizada %>% 
  left_join(minMax_RME_SUAVIZADA, by = "cuartil_RME_SUAVIZADA")


#PALETA DE COLORES PARA PP de RR >1
#Intervalos para PP
data_rme_suavizada$INTERVALOS_PP<-cut(data_rme_suavizada$PROBABILIDAD_POSTERIOR,breaks =  c(0,0.05,0.2,0.8,0.95,1), include.lowest = T)

niveles_PP <- c("0 – 0.05", "0.05 – 0.2", "0.2 – 0.8", "0.8 – 0.95", "0.95 – 1")

#Asigno colores para cada intervalo

data_rme_suavizada <- data_rme_suavizada %>%
  mutate(
    leyenda_pp = factor(INTERVALOS_PP, labels = niveles_PP),
    color_PP = case_when(
      INTERVALOS_PP == levels(INTERVALOS_PP)[1] ~ "#feedde",
      INTERVALOS_PP == levels(INTERVALOS_PP)[2] ~ "#fdbe85",
      INTERVALOS_PP == levels(INTERVALOS_PP)[3] ~ "#fd8d3c",
      INTERVALOS_PP == levels(INTERVALOS_PP)[4] ~ "#e6550d",
      INTERVALOS_PP == levels(INTERVALOS_PP)[5] ~ "#a63603"
    )
  )


#Guardo base

save(data_rme_suavizada, file = "paper_1/data_rme_suavizada.rda")

message("Dataset generado data_rme_suavizada.rda")
