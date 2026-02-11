library(sf)
library(spdep)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(patchwork)
library(tibble)
library(leaflet)

#leaflet(mapa) %>% addTiles() %>% addPolygons()

# ------leaflet# -----------------------------
# 1) PARÁMETROS / PATHS
# -----------------------------

PATH_XLSX <- "paper_1/bivariado.xlsx"
PATH_SHP  <- "paper_1/shp/mapa.rda"

TRIENIOS  <- c("2000-2002", "2021-2023")
ALPHA     <- 0.05
NSIM      <- 999
SEED      <- 123

OUT_FIG <- "mapa_capeco.png"

# -----------------------------
# 2) CARGA DE DATOS
# -----------------------------
biv <- read_excel(PATH_XLSX) %>%
  mutate(in1 = sprintf("%05d", as.integer(in1))) %>%
  mutate(in1 = as.character(in1))

load(PATH_SHP)

# -----------------------------
# 4) ARMAR SF POR TRIENIO (SOLO CASOS COMPLETOS)
# -----------------------------
armar_sf_trienio <- function(trienio, depts_sf, biv_df) {
  depts_sf %>%
    left_join(biv_df %>% filter(TRIENIO == trienio), by = "in1") #%>%
    #dplyr::filter(!is.na(RME_SUAVIZADA), !is.na(PCT_CAPECO))
}

sf_2000 <- armar_sf_trienio(TRIENIOS[1], mapa, biv)
sf_2021 <- armar_sf_trienio(TRIENIOS[2], mapa, biv)

# -----------------------------
# 5) PESOS ESPACIALES (VECINOS + LISTW)
# -----------------------------
armar_pesos <- function(sf_obj, queen = TRUE, style = "W") {
  nb <- poly2nb(sf_obj, queen = queen)
  lw <- nb2listw(nb, style = style, zero.policy = TRUE)
  list(nb = nb, lw = lw)
}

sf_use_s2(FALSE)

w_2000 <- armar_pesos(sf_2000[!sf_2000$in1 %in% c("94000","95000"),], queen = TRUE, style = "W")
w_2021 <- armar_pesos(sf_2021[!sf_2021$in1 %in% c("94000","95000"),], queen = TRUE, style = "W")

# -----------------------------
# 6) MORAN GLOBAL BIVARIADO (PERMUTACIONES)
# -----------------------------
moran_bivariado_global_perm <- function(sf_obj, lw_obj, nsim = 999, seed = 123) {
  x <- as.numeric(scale(sf_obj$RME_SUAVIZADA))
  y <- as.numeric(scale(sf_obj$PCT_CAPECO))
  Wy <- lag.listw(lw_obj, y, zero.policy = TRUE)
  
  I_obs <- as.numeric(crossprod(x, Wy) / crossprod(x))
  
  set.seed(seed)
  I_perm <- replicate(nsim, {
    y_perm <- sample(y)
    Wy_perm <- lag.listw(lw_obj, y_perm, zero.policy = TRUE)
    as.numeric(crossprod(x, Wy_perm) / crossprod(x))
  })
  
  p_value <- (sum(abs(I_perm) >= abs(I_obs)) + 1) / (nsim + 1)
  list(I_obs = I_obs, p_value = p_value, I_perm = I_perm)
}

moran_2000 <- moran_bivariado_global_perm(sf_2000[!sf_2000$in1 %in% c("94000","95000"),], w_2000$lw, nsim = NSIM, seed = SEED)
moran_2021 <- moran_bivariado_global_perm(sf_2021[!sf_2021$in1 %in% c("94000","95000"),], w_2021$lw, nsim = NSIM, seed = SEED)

cat("\nMoran bivariado global 2000-2002: I =", moran_2000$I_obs, "p =", moran_2000$p_value, "\n")
cat("Moran bivariado global 2021-2023: I =", moran_2021$I_obs, "p =", moran_2021$p_value, "\n")

# -----------------------------
# 7) LISA LOCAL BIVARIADO (PERMUTACIONES) + CLUSTERS (TU ORIGINAL)
# -----------------------------
lisa_bivariado_local_perm <- function(sf_obj, lw_obj, alpha = 0.05, nsim = 999, seed = 123) {
  x <- as.numeric(scale(sf_obj$RME_SUAVIZADA))
  y <- as.numeric(scale(sf_obj$PCT_CAPECO))
  Wy <- lag.listw(lw_obj, y, zero.policy = TRUE)
  
  Ii_obs <- x * Wy
  
  set.seed(seed)
  Ii_perm <- replicate(nsim, {
    y_perm <- sample(y)
    Wy_perm <- lag.listw(lw_obj, y_perm, zero.policy = TRUE)
    x * Wy_perm
  })
  
  p_sim <- vapply(seq_along(Ii_obs), function(i) {
    (sum(abs(Ii_perm[i, ]) >= abs(Ii_obs[i])) + 1) / (nsim + 1)
  }, numeric(1))
  
  sf_obj %>%
    mutate(
      Ii_bv = Ii_obs,
      p_sim = p_sim,
      cluster = case_when(
        p_sim > alpha     ~ "No significativo",
        x >= 0 & Wy >= 0  ~ "High–High",
        x <= 0 & Wy <= 0  ~ "Low–Low",
        x >= 0 & Wy <= 0  ~ "High–Low",
        x <= 0 & Wy >= 0  ~ "Low–High"
      )
    )
}

lisa_sf_2000 <- lisa_bivariado_local_perm(sf_2000[!sf_2000$in1 %in% c("94000","95000"),], w_2000$lw, alpha = ALPHA, nsim = NSIM, seed = SEED)
lisa_sf_2021 <- lisa_bivariado_local_perm(sf_2021[!sf_2021$in1 %in% c("94000","95000"),], w_2021$lw, alpha = ALPHA, nsim = NSIM, seed = SEED)


sf_2000 = sf_2000 %>% left_join(lisa_sf_2000 %>% dplyr::select(cluster) %>% as.data.frame())
sf_2000$cluster[is.na(sf_2000$cluster)] = "Excluido"

sf_2021 = sf_2021 %>% left_join(lisa_sf_2021 %>% dplyr::select(cluster) %>% as.data.frame())
sf_2021$cluster[is.na(sf_2021$cluster)] = "Excluido"


# -----------------------------
# 8) COLORES / NIVELES (FIJOS)
# -----------------------------
niveles_cluster <- c("High–High", "Low–Low", "High–Low", "Low–High", "No significativo", "Excluido")

colores_lisa <- c(
  "High–High" = "#B2182B",
  "Low–Low" = "#2166AC",
  "High–Low" = "#EF8A62",
  "Low–High" = "#67A9CF",
  "No significativo" = "grey80",
  "Excluido" = "#636363"
  
)

sf_2000$cluster <- factor(sf_2000$cluster, levels = niveles_cluster)
sf_2021$cluster <- factor(sf_2021$cluster, levels = niveles_cluster)

# -----------------------------
# 9) MAPA LISA CON INSET CABA + TDF/MALVINAS EN GRIS
# -----------------------------
mapa_lisa_caba <- function(sf_lisa, trienio, depts_caba_sf, colores, mostrar_leyenda = FALSE) {
  # =========================
  # 1) MAPA PRINCIPAL (IGUAL AL ORIGINAL)
  # =========================
  
  bb <- st_bbox(sf_lisa)
  dx <- as.numeric(bb["xmax"] - bb["xmin"])
  dy <- as.numeric(bb["ymax"] - bb["ymin"])
  

  xlim_main <- c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"]))
  ylim_main <- c(as.numeric(bb["ymin"] - 0.18*dy), as.numeric(bb["ymax"]))
 
  mapa_nacional <- ggplot(sf_lisa) +
    geom_sf(aes(fill = cluster), color = "white", linewidth = 0.1) +
    scale_fill_manual(
      values = colores,
      drop = FALSE,
      name  = if (mostrar_leyenda) "Cluster" else NULL,
      guide = if (mostrar_leyenda) "legend" else "none"
    ) +
    coord_sf(xlim = xlim_main, ylim = ylim_main, expand = FALSE) +
   # theme_void() +
    theme_minimal() +
    labs(title = trienio) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      legend.position = if (mostrar_leyenda) "bottom" else "none"
    )
  
  
  # =========================
  # 2) INSET CABA (más chico y pegado, como tu original)
  # =========================
  caba <- sf_lisa %>%
    dplyr::filter(in1 == "02000") %>%
    left_join(sf_lisa %>% st_drop_geometry() %>% dplyr::select(in1, cluster), by = "in1") %>%
    rename(cluster = cluster.x)
  
  bb_c <- st_bbox(caba)
  dx_c <- as.numeric(bb_c["xmax"] - bb_c["xmin"])
  dy_c <- as.numeric(bb_c["ymax"] - bb_c["ymin"])
  pad  <- 0.6
  
  mapa_caba <- ggplot(caba) +
    geom_sf(aes(fill = cluster), color = "black", linewidth = 0.3) +
    scale_fill_manual(values = colores, guide = "none", drop = FALSE) +
    coord_sf(
      xlim = c(as.numeric(bb_c["xmin"] - pad*dx_c), as.numeric(bb_c["xmax"] + pad*dx_c)),
      ylim = c(as.numeric(bb_c["ymin"] - pad*dy_c), as.numeric(bb_c["ymax"] + pad*dy_c)),
      expand = FALSE
    ) +
    theme_void() +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4))
  
  # =========================
  # 3) INSET TDF + MALVINAS (gris = No significativo)
  # =========================
  # =========================
  # 3) INSET TDF (continental) + MALVINAS encima
  # =========================
  mapa_nacional +
    inset_element (mapa_caba, left = 0.86, bottom = 0.50, right = 0.98, top = 0.74)
}


mapa_2000 <- mapa_lisa_caba(sf_2000, "Triennium 2000–2002", depts_final, colores_lisa, mostrar_leyenda = TRUE)
mapa_2021 <- mapa_lisa_caba(sf_2021, "Triennium 2021–2023", depts_final, colores_lisa, mostrar_leyenda = FALSE)

figura_final <- (mapa_2000 | mapa_2021) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center"
  )

print(figura_final)

ggsave(
  filename = "paper_1/mapa_lisa.jpg",
  plot = figura_final,
  device = "jpeg",
  width = 30,
  height = 20,
  units = "cm",
  dpi = 300
)

