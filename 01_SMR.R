# ESTIMACION DE RME PARA TUMORES TRÁQUEA, BRONQUIO Y PULMÓN EN MUJERES
library(tidyverse)
library(epitools)
library(sf)
library(spdep)
library(glue)
library(rmapshaper)

#Aplico función para generar datasets espaciales de RME

trienios = 1:3

lapply(trienios, function(i) {
  trienio = i
  causa = "2350 TUMORES MALIGNOS DE TRAQUEA, BRONQUIO Y PULMON"
  generarRDA(trienio, causa)
})

message("Datasets generados data_trienio_1.rda, data_trienio_2.rda y data_trienio_3.rda")
