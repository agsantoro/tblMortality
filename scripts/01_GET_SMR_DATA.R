# SMR ESTIMATION FOR TRACHEA, BRONCHUS, AND LUNG TUMORS IN WOMEN
library(tidyverse)
library(epitools)
library(sf)
library(spdep)
library(glue)
library(rmapshaper)

# Apply function to generate SMR spatial datasets
trienios = 1:3

lapply(trienios, function(i) {
  trienio = i
  causa = "2350 TUMORES MALIGNOS DE TRAQUEA, BRONQUIO Y PULMON"
  generarRDA(trienio, causa)
})

message("Datasets generated: data_trienio_1.rda, data_trienio_2.rda, and data_trienio_3.rda")
