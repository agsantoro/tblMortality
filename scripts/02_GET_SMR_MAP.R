library(glue)
library(patchwork)
library(highcharter)
library(dplyr)
library(tidyr)
library(shiny)
library(sf)
library(tibble)
library(ggplot2)

sf_use_s2(FALSE)

#=============== MAPAS PARA GRAFICAR RME POR TUMORES DE TRÁQUEA, BRONQUIO Y PULMÓN ================#

# importa bases de datasets

load("outputs/data/data_trienio_1.rda")
load("outputs/data/data_trienio_2.rda")
load("outputs/data/data_trienio_3.rda")


# une datasets de los 3 trienios

trienios = 1:3

base_mapa_rme = lapply(trienios, function(i) {
  load(glue("outputs/data/data_trienio_{i}.rda"))
  RMEtable
})

base_mapa_rme = do.call(rbind,base_mapa_rme)

# transforma base a sf para mapear
base_mapa_rme <- st_as_sf(base_mapa_rme)

#Cuartiles de RME considerando todos los periodos
base_mapa_rme <- cuartiles(base_mapa_rme, "RME_CRUDA")
paleta <- aplicar_paleta("cuartil_RME_CRUDA")

#Asigno un color específico a cada color
base_mapa_rme <- base_mapa_rme %>% 
  left_join(paleta, by = "cuartil_RME_CRUDA")

#Calculo valores mínimos y máximos para cada cuartiles para la leyenda del mapa
minMax_RME_CRUDA = obtener_minmax(base_mapa_rme, "RME_CRUDA")

#Uno leyendas al dataframe
base_mapa_rme <- base_mapa_rme %>% 
  left_join(minMax_RME_CRUDA, by = "cuartil_RME_CRUDA")

#Guardo base
data_rme <- base_mapa_rme
save(data_rme, file = "outputs/data/data_rme.rda")


#===============================
#MAPA RME_CRUDA DIVIDIDA EN CUARTILES
#===============================

mapa <- mapas_rme_cuartiles(
  variable = "RME_CRUDA",
  titulo_var = "RME_CRUDA",
  trienios_seleccionados = c("2000-2002", "2009-2011", "2021-2023")
)

message("Mapas RME_CRUDA generados")

ggsave(
  filename = "outputs/figures/map_smr.jpg",
  plot = mapa,
  device = "jpeg",
  width = 30,
  height = 20,
  units = "cm",
  dpi = 300
)
