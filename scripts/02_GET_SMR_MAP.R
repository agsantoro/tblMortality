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

#=============== MAPS FOR PLOTTING SMR FOR TRACHEA, BRONCHUS, AND LUNG TUMORS ================#

# Import datasets
load("outputs/data/data_trienio_1.rda")
load("outputs/data/data_trienio_2.rda")
load("outputs/data/data_trienio_3.rda")


# Join datasets from all 3 trienniums
trienios = 1:3

base_mapa_rme = lapply(trienios, function(i) {
  load(glue("outputs/data/data_trienio_{i}.rda"))
  RMEtable
})

base_mapa_rme = do.call(rbind, base_mapa_rme)

# Transform base to sf object for mapping
base_mapa_rme <- st_as_sf(base_mapa_rme)

# SMR Quartiles considering all periods
base_mapa_rme <- cuartiles(base_mapa_rme, "RME_CRUDA")
paleta <- aplicar_paleta("cuartil_RME_CRUDA")

# Assign a specific color to each quartile
base_mapa_rme <- base_mapa_rme %>% 
  left_join(paleta, by = "cuartil_RME_CRUDA")

# Calculate minimum and maximum values for each quartile for the map legend
minMax_RME_CRUDA = obtener_minmax(base_mapa_rme, "RME_CRUDA")

# Join legends to the dataframe
base_mapa_rme <- base_mapa_rme %>% 
  left_join(minMax_RME_CRUDA, by = "cuartil_RME_CRUDA")

# Save base data
data_rme <- base_mapa_rme
save(data_rme, file = "outputs/data/data_rme.rda")


#===============================
# RAW SMR MAP DIVIDED BY QUARTILES
#===============================

mapa <- mapas_rme_cuartiles(
  variable = "RME_CRUDA",
  titulo_var = "RME_CRUDA",
  trienios_seleccionados = c("2000-2002", "2009-2011", "2021-2023")
)

message("RAW SMR Maps generated")

ggsave(
  filename = "outputs/figures/map_smr.jpg",
  plot = mapa,
  device = "jpeg",
  width = 30,
  height = 20,
  units = "cm",
  dpi = 300
)
