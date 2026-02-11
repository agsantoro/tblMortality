#Librerias

library(tidyverse)
library(sf)
library(RColorBrewer)
library(patchwork)

#Importo base
load("data/mortality/data_rme_suavizada.rda")

sf_use_s2(FALSE)

#===============================
#MAPA RME DIVIDIDA EN CUARTILES
#===============================

mapa_rme <- mapas_rme_cuartiles(
  variable = "RME_SUAVIZADA",
  titulo_var = "RME_SUAVIZADA",
  trienios_seleccionados = c("2000-2002", "2009-2011", "2021-2023")
)

#===================================================
#MAPA PP RR >1
#===================================================

mapa_pp <- mapas_PP_RR(
  variable = "Probabilidad Posterior (PP)" ,
  titulo_var = "Probabilidad posterior (PP)",
  trienios_seleccionados= c("2000-2002", "2009-2011", "2021-2023")
)

message("Mapas RME suavizada, RR suavizado y PP generados")


ggsave(
  filename = "data/mortality/mapa_rmesuav_grilla.jpg",
  plot = mapa_rme,
  device = "jpeg",
  width = 30,
  height = 20,
  units = "cm",
  dpi = 300
)

ggsave(
  filename = "data/mortality/mapa_pp_grilla.jpg",
  plot = mapa_pp,
  device = "jpeg",
  width = 30,
  height = 20,
  units = "cm",
  dpi = 300
)
