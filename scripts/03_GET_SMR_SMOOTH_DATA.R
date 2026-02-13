# SMR SMOOTHING (Smoothed RR)

# Following the methodology proposed in: https://github.com/Epi-Emma/Firearm_Suicide_Disease_Mapping

# Libraries 
library(INLA)
library(sf)
library(spdep)
library(dplyr)
library(purrr)

# Load files
load("outputs/data/data_rme.rda")

# Apply smoothing function to each triennium
datos_trienio_1 <- suavizar_rme(data_trienio = data_rme[data_rme$TRIENIO == "2000-2002", ], data_tdf = data_rme)
datos_trienio_2 <- suavizar_rme(data_trienio = data_rme[data_rme$TRIENIO == "2009-2011", ], data_tdf = data_rme)
datos_trienio_3 <- suavizar_rme(data_trienio = data_rme[data_rme$TRIENIO == "2021-2023", ], data_tdf = data_rme)

# Join data from all three trienniums
data_rme_suavizada <- rbind(datos_trienio_1, datos_trienio_2, datos_trienio_3)

#======================================================
# SMOOTHED SMR BASES FOR MAPS WITH COMMON SCALES
#======================================================

# Estimate common quartiles for SMR across all three trienniums
data_rme_suavizada <- cuartiles(data_rme_suavizada, "RME_SUAVIZADA")

# Color palette for smoothed SMR quartiles
paleta <- aplicar_paleta("cuartil_RME_SUAVIZADA")

# Assign colors to quartiles
data_rme_suavizada <- data_rme_suavizada %>% 
  left_join(paleta, by = "cuartil_RME_SUAVIZADA")

# Calculate min and max quartile values for the map legend
minMax_RME_SUAVIZADA = obtener_minmax(data_rme_suavizada, "RME_SUAVIZADA")

# Join legends to the dataframe
data_rme_suavizada <- data_rme_suavizada %>% 
  left_join(minMax_RME_SUAVIZADA, by = "cuartil_RME_SUAVIZADA")


# COLOR PALETTE FOR PP (Posterior Probability) of RR > 1
# Intervals for PP
data_rme_suavizada$INTERVALOS_PP <- cut(data_rme_suavizada$PROBABILIDAD_POSTERIOR, 
                                        breaks = c(0, 0.05, 0.2, 0.8, 0.95, 1), 
                                        include.lowest = TRUE)

niveles_PP <- c("0 – 0.05", "0.05 – 0.2", "0.2 – 0.8", "0.8 – 0.95", "0.95 – 1")

# Assign colors for each interval
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

# Save dataset
save(data_rme_suavizada, file = "outputs/data/data_rme_suavizada.rda")

message("Dataset generated: data_rme_suavizada.rda")