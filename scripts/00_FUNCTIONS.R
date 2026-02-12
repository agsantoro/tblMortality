#####mapa completo con Malvinas
mapaCompleto = function() {
  # carga mapa de polígonos de departamentos
  load("Cartografía/mapaSimple.rda")
  mapa2 = read_sf("data/shp/pruebaunion.shp")
  mapa2 = mapa2 %>% dplyr::select(in1 = FIRST_MI_1) %>% 
    dplyr::filter(in1 == "95") %>%
    mutate(in1 = "95000")
  
  # 1. Convertir mapa a MULTIPOLYGON
  mapa <- st_cast(mapa, "MULTIPOLYGON")
  
  # 2. Verificar que ambos tengan el mismo tipo de geometría
  st_geometry_type(mapa, by_geometry = FALSE)
  st_geometry_type(mapa2, by_geometry = FALSE)
  
  # 3. Unir los mapas
  mapa <- rbind(mapa, mapa2)
  mapa = rmapshaper::ms_simplify(mapa)
  
  save(mapa, file = "outputs/maps/mapa.rda")
}


analisisEspacialRME = function (trienio, causa) {
  ##### PREPARACION DE DATOS #####
  # cargar poblaciones por departamentos
  load("inputs/data/population/pobDeptosCenso.rda")
  
  
  # carga dataset de tasas de mortalidad de población estandar
  load("inputs/data/mortality/tasasEstandarizadas.rda")
  
  # carga mapa
  load("inputs/data/shp/mapa.rda")
  
  # crea lista para guardar resultados
  resultados = list()
  
  # carga lista de causas de mortalidad
  load("inputs/data/mortality/causasMortalidad.rda")
  
  causasMortalidad = causasMortalidad %>% 
    pivot_longer(cols= 1:ncol(causasMortalidad),names_to = "LISTA") %>% 
    dplyr::filter(is.na(value)==F) %>% 
    distinct(LISTA, value)
  
  # identifica lista de la causa seleccionada
  lista = max(causasMortalidad$LISTA[causasMortalidad$value==causa])
  
  # identifica el año del censo y los años de las defucniones según el trienio seleccionado
  if (trienio == 1) {
    anoCenso = "2001"
    anosDef = 2000:2002
    anosDef = substring(anosDef,3,4)
  } else if (trienio == 2) {
    anoCenso = "2010"
    anosDef = 2009:2011
    anosDef = substring(anosDef,3,4)
  } else if (trienio == 3) {
    anoCenso = "2022"
    anosDef = 2021:2023
    anosDef = substring(anosDef,3,4)
  }
  
  
  # formatea df de población:
  # - filtra mujeres
  # - agrupa edades
  
  pobDeptosCenso = pobDeptosCenso %>% 
    dplyr::filter(SEXO == 2) %>% 
    pivot_longer(cols = 5:(ncol(pobDeptosCenso)-1), names_to = "GRUPEDAD", values_to = "POBLACION")
  
  pobDeptosCenso$GRUPEDAD[pobDeptosCenso$GRUPEDAD %in% c("POB7579","POB8084","POB8589","POB9094","POB95MAS")] = "POB75MAS"
  
  pobDeptosCenso = pobDeptosCenso %>% mutate(GRUPEDAD_DEPTOS = case_when(
    GRUPEDAD == "POB0004" ~ "POB0004",
    GRUPEDAD == "POB0509" ~ "POB0514",
    GRUPEDAD == "POB1014" ~ "POB0514",
    GRUPEDAD == "POB1519" ~ "POB1524",
    GRUPEDAD == "POB2024" ~ "POB1524",
    GRUPEDAD == "POB2529" ~ "POB2534",
    GRUPEDAD == "POB3034" ~ "POB2534",
    GRUPEDAD == "POB3539" ~ "POB3544",
    GRUPEDAD == "POB4044" ~ "POB3544",
    GRUPEDAD == "POB4549" ~ "POB4554",
    GRUPEDAD == "POB5054" ~ "POB4554",
    GRUPEDAD == "POB5559" ~ "POB5564",
    GRUPEDAD == "POB6064" ~ "POB5564",
    GRUPEDAD == "POB6569" ~ "POB6574",
    GRUPEDAD == "POB7074" ~ "POB6574",
    GRUPEDAD == "POB75MAS" ~ "POB75MAS"
  )) %>% 
    dplyr::filter(is.na(GRUPEDAD_DEPTOS) == F) %>%
    group_by(ANO, CODIGODEPTO, SEXO, GRUPEDAD_DEPTOS) %>%
    summarise(POBLACION = sum(POBLACION))
  
  # carga defunciones del trienio seleccionado
  if (trienio == 1) {
    load("inputs/data/mortality/defTri1.rda")
  } else if (trienio == 2) {
    load("inputs/data/mortality/defTri2.rda")
  } else if (trienio == 3) {
    load("inputs/data/mortality/defTri3.rda")
  }
  
  # filtra según la causa seleccionada
  if (causa != "0000 TODAS LAS CAUSAS") {
    muertes = defTri %>% dplyr::filter(get(lista) == causa)
  } else {
    muertes = defTri
  }
  
  # formatea df de muertes
  muertes = muertes %>% 
    group_by(CODIGODEPTO, SEXO, GRUPEDAD_DEPTOS) %>%
    summarise(DEFUNCIONES= n())
  
  # crea tabla con información de población y muertes por departamento
  pobDeptosCenso$CODIGODEPTO = as.character(pobDeptosCenso$CODIGODEPTO)
  
  tablaDeptos = 
    pobDeptosCenso[pobDeptosCenso$ANO==anoCenso & pobDeptosCenso$SEXO %in% c(1,2),] %>% 
    left_join(muertes, by = c("CODIGODEPTO","GRUPEDAD_DEPTOS","SEXO"))
  
  tablaDeptos$DEFUNCIONES[is.na(tablaDeptos$DEFUNCIONES)] = 0
  
  # crea tabla de tasas de mortalidad estandar
  tasasEstandar = tasasEstandar[tasasEstandar$CAUSA == causa,]
  
  # crea tabla con RMEs y NBI por depto
  RMEtable = data.frame()
  
  for (i in unique(tablaDeptos$CODIGODEPTO)) {
    #for (i = "70126") {
    RME_CRUDA = ageadjust.indirect(
      count = tablaDeptos$DEFUNCIONES[tablaDeptos$CODIGODEPTO == i],
      pop = tablaDeptos$POBLACION[tablaDeptos$CODIGODEPTO == i]*3,
      stdcount = tasasEstandar$DEFUNCIONES,
      stdpop = tasasEstandar$POBLACION,
      stdrate = NULL, 
      conf.level = 0.95
    )
    
    append = data.frame(
      CODIGODEPTO = i,
      RME_CRUDA = RME_CRUDA$sir[["sir"]],
      STDRATE = RME_CRUDA$rate[["adj.rate"]],
      DEFUNCIONES = sum(tablaDeptos$DEFUNCIONES[tablaDeptos$CODIGODEPTO == i]),
      POBLACION = sum(tablaDeptos$POBLACION[tablaDeptos$CODIGODEPTO == i])*3,
      ESPERADAS = RME_CRUDA$sir[["exp"]]
    )
    
    RMEtable = rbind(
      RMEtable,
      append
    )
    
    
    
  }
  
  malvinas = c("95000",rep(NA,5))
  
  RMEtable = rbind(RMEtable,malvinas)
  
  RMEtable = RMEtable %>%
    mutate(
      RME_CRUDA = as.numeric(RME_CRUDA),
      DEFUNCIONES = as.numeric(DEFUNCIONES),
      POBLACION = as.numeric(POBLACION),
      ESPERADAS = as.numeric(ESPERADAS),
      STDRATE = as.numeric(STDRATE)
    )
  
  return(
    RMEtable
  )
  
}


# Suma indicadores al mapa e identifica el trienio al que pertenece el dataframe

indicadores_mapa_trienio <-function (df,trienio) {
  # carga mapa de polígonos de departamentos
  load("inputs/data/shp/mapa.rda")
  
  df<-df %>% rename("in1"="CODIGODEPTO") %>%
    left_join(mapa, by = "in1") %>%
    mutate (TRIENIO = trienio)
  
  return(df)
}

generarRDA = function(trienio,causa) {
  RMEtable = analisisEspacialRME(trienio,causa)
  
  anoCenso = switch (
    trienio,
    "1" = "2000-2002",
    "2" = "2009-2011",
    "3" = "2021-2023"
  )
  
  RMEtable = indicadores_mapa_trienio(RMEtable, anoCenso)
  
  save(RMEtable, file = glue("outputs/data/data_trienio_{trienio}.rda"))
  
}


#####Cálculo de cuartiles con nombre sufijo
cuartiles <- function(df, variable) {
  datos <- df[[variable]]
  
  # Calcular los quintiles (0%, 25%, 50%, 75%, 100%)
  quintiles <- quantile(datos, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  
  # Asignar etiquetas de cuartiles
  rangos_cuartiles <- cut(datos, breaks = quintiles, 
                          labels = c("Q1", "Q2", "Q3", "Q4"), 
                          include.lowest = TRUE)
  
  # Crear una nueva columna en el data.frame
  nueva_columna <- paste0("cuartil_", variable)
  df[[nueva_columna]] <- rangos_cuartiles
  
  return(df)
}

#####paletas mapas
aplicar_paleta <- function(tipo_variable) {
  
  # 1. Definimos los colores según la variable
  if (tipo_variable == "cuartil_RME_CRUDA") {
    colores <- c("#edf8fb", "#b2e2e2", "#66c2a4", "#238b45")
  } else if (tipo_variable == "cuartil_RME_SUAVIZADA") {
    colores <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")
  } else {
    return(NULL) # Si no coincide, no devuelve nada sin tirar error
  }
  
  # 2. Creamos el dataframe base
  paleta <- data.frame(
    c("Q1", "Q2", "Q3", "Q4"),
    colores
  )
  
  # 3. Renombramos las columnas dinámicamente
  # Esto creará: cuartiles_RME_CRUDA y color_RME_CRUDA (por ejemplo)
  colnames(paleta) <- c(paste0("", tipo_variable), 
                        paste0("color_", tipo_variable))
  
  return(paleta)
}


#####cálculo de minmax
obtener_minmax <- function(df, tipo_variable) {
  
  # Creamos el nombre de la columna de cuartiles que vamos a buscar
  nombre_cuartil <- paste0("cuartil_", tipo_variable)
  
  # 1. Calculamos min y max dinámicamente
  resumen <- df %>%
    st_drop_geometry() %>% 
    # Agrupamos por la columna dinámica 
    group_by(.data[[nombre_cuartil]]) %>%
    summarise(
      minimo = min(.data[[tipo_variable]], na.rm = TRUE),
      maximo = max(.data[[tipo_variable]], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    # Filtramos filas vacías (como las de TDF si no tienen datos)
    filter(!is.na(.data[[nombre_cuartil]]))
  
  # 2. Formateo de texto para la leyenda del paper
  resumen$rango <- paste0(
    format(resumen$minimo, digits = 2, nsmall = 2, big.mark = ".", decimal.mark = ","),
    " - ",
    format(resumen$maximo, digits = 2, nsmall = 2, big.mark = ".", decimal.mark = ",")
  )
  
  # 3. Renombramos las columnas para que no choquen al unir trienios
  colnames(resumen) <- c(
    nombre_cuartil, 
    paste0("minimo_", tipo_variable), 
    paste0("maximo_", tipo_variable), 
    paste0("rango_", tipo_variable)
  )
  
  return(resumen)
}


#####mapas cuartiles
mapas_rme_cuartiles <- function(variable, titulo_var, trienios_seleccionados) {
  
  if (variable == "RME_CRUDA") {
    base_a_usar <- base_mapa_rme
  } else if (variable == "RME_SUAVIZADA") {
    base_a_usar <- data_rme_suavizada
  } else {
    stop("La variable debe ser 'RME_CRUDA' o 'RME_SUAVIZADA'")
  }
  
  # Filtrar la base seleccionada por los trienios indicados
  df_filtrado <- base_a_usar %>%
    dplyr::filter(TRIENIO %in% trienios_seleccionados)
  
  # 1. Definir nombres de columnas dinámicas basándose en la variable
  # Si variable = "RME_SUAVIZADA", buscará "cuartil_RME_SUAVIZADA", etc.
  col_cuartil <- paste0("cuartil_", variable)
  col_color   <- paste0("color_cuartil_", variable)
  col_rango   <- paste0("rango_", variable)
  
  # 2. Función interna para crear cada mapa individual
  mapa_por_trienio <- function(trienio_actual, mostrar_leyenda = FALSE) {
    
    # Filtrar datos del trienio específico
    df_trienio <- df_filtrado %>% dplyr::filter(TRIENIO == trienio_actual)
    df_caba    <- df_trienio %>% dplyr::filter(in1 == "02000") # CABA para el inset
    
    # Extraer la combinación única de Cuartil y Color
    # Usamos .data[[...]] para que ggplot busque el nombre contenido en la variable
    colores_por_cuartil <- df_trienio %>%
      st_drop_geometry() %>%
      distinct(.data[[col_cuartil]], .data[[col_color]]) %>%
      drop_na() %>%
      deframe()
    
    # Extraer la combinación única de Cuartil y Rango de texto para la leyenda
    etiquetas_por_cuartil <- df_trienio %>%
      st_drop_geometry() %>%
      distinct(.data[[col_cuartil]], .data[[col_rango]]) %>%
      drop_na() %>%
      deframe()
    
    # Mapa Nacional
    mapa_nacional <- ggplot(df_trienio) +
      geom_sf(aes(fill = .data[[col_cuartil]]), color = "black", size = 0.1) +
      scale_fill_manual(
        values = colores_por_cuartil,
        labels = etiquetas_por_cuartil,
        name = if (mostrar_leyenda) titulo_var else NULL,
        guide = if (mostrar_leyenda) "legend" else "none"
      ) +
      theme_minimal() +
      labs(title = paste("Triennium", trienio_actual)) +
      theme(
        legend.position = if (mostrar_leyenda) "right" else "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10)#,
      #  axis.text = element_blank(),
      #  panel.grid = element_blank()
      )
    
    # Mapa CABA (Inset)
    mapa_caba <- ggplot(df_caba) +
      geom_sf(aes(fill = .data[[col_cuartil]]), color = "black", size = 0.2) +
      scale_fill_manual(values = colores_por_cuartil, guide = "none") +
      coord_sf(expand = FALSE) +
      theme_void() +
      theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))
    
    # Combinar mapa nacional con el zoom de CABA
    mapa_nacional + inset_element(
      mapa_caba,
      left = 0.9, bottom = 0.5, right = 0.98, top = 0.7
    )
  }
  
  # 3. Lógica para mostrar la leyenda solo en el último mapa
  n <- length(trienios_seleccionados)
  leyenda_vec <- rep(FALSE, n)
  leyenda_vec[n] <- TRUE
  
  # 4. Generar la lista de mapas usando mapply
  mapas_lista <- mapply(
    mapa_por_trienio,
    trienios_seleccionados,
    mostrar_leyenda = leyenda_vec,
    SIMPLIFY = FALSE
  )
  
  # 5. Unir todos los mapas en una sola fila
  wrap_plots(mapas_lista, ncol = n) #+
  #  plot_annotation(
     # caption = "Fuente: elaboración propia en base a datos de la DEIS.",
   #   theme = theme(plot.caption = element_text(size = 8, hjust = 0))
  #  )
}


#####FUNCION PARA ESTIMAR RR SUAVIZADO Y PP >1
suavizar_rme <- function(data_trienio, data_tdf, nombre_archivo_vecindad = "adjacency.graph") {
  
  datos_modelo <- data_trienio %>%
    dplyr::filter(in1 != "94000" & in1 != "95000") %>%
    st_as_sf()
  
  datos_modelo$nuevo_id <- 1:nrow(datos_modelo)
  
  # Crear matriz de vecindades
  deptos_nb <- poly2nb(datos_modelo, row.names = datos_modelo$nuevo_id, queen = TRUE)
  
  # Guardar y leer archivo de vecinos para INLA
  nb2INLA(file = nombre_archivo_vecindad, nb = deptos_nb)
  vecindad_inla <- inla.read.graph(filename = nombre_archivo_vecindad)
  
  # Preparar datos de TDF con NA para luego mapear
  datos_tdf <- data_tdf %>%
    dplyr::filter((in1 == "94000" | in1 == "95000") & TRIENIO == first(data_trienio$TRIENIO)) %>%
    mutate(
      nuevo_id = NA,          
      RME_SUAVIZADA = NA_real_,
      PROBABILIDAD_POSTERIOR = NA_real_
    ) %>% 
    dplyr::select(-c("cuartil_RME_CRUDA", "color_cuartil_RME_CRUDA", "minimo_RME_CRUDA", "maximo_RME_CRUDA", "rango_RME_CRUDA")) 
  
  # Fórmula BYM2
  formula <- DEFUNCIONES ~ 1 + f(nuevo_id,
                                 model = "bym2", 
                                 graph = vecindad_inla,
                                 scale.model = TRUE, 
                                 constr = TRUE, 
                                 hyper = list(
                                   phi = list(prior = "pc", param = c(0.5, 0.5), initial = -3),
                                   prec = list(prior = "pc.prec", param = c(1, 0.01), initial = 4)
                                 ))
  
  # Ajuste del modelo INLA
  fitmod <- inla(formula,
                 data = datos_modelo,
                 family = "poisson",
                 E = ESPERADAS,
                 control.predictor = list(compute = TRUE),
                 control.compute = list(return.marginals.predictor = TRUE), #para poder calcular PP
                 verbose = TRUE)
  
  # Agregar resultados al dataframe
  datos_modelo$RME_SUAVIZADA <- fitmod$summary.fitted.values$mean
  
  # Calcular probabilidades posteriores P(RR > 1)
  prob_post <- map_dbl(fitmod$marginals.fitted.values, ~ 1 - inla.pmarginal(1, .x))
  datos_modelo$PROBABILIDAD_POSTERIOR <- prob_post
  
  return(bind_rows(datos_modelo, datos_tdf))
  
}


#FUNCION PARA MAPA PROBABILIDAD POSTERIOR POR TRIENIOS 
mapas_PP_RR <- function(variable = "PROBABILIDAD_POSTERIOR", titulo_var, trienios_seleccionados) {
  
  # Filtrar base por trienios seleccionados
  df_filtrado <- data_rme_suavizada %>%
    dplyr::filter(TRIENIO %in% trienios_seleccionados)
  
  df_mapa <- df_filtrado
  
  # Función interna para crear un mapa por trienio
  mapa_por_trienio <- function(trienio_actual, mostrar_leyenda = FALSE) {
    df_trienio <- df_mapa %>% filter(TRIENIO == trienio_actual)
    df_caba <- df_trienio %>% filter(in1 == "02000")
    
    # Extraer colores por cuartil para este trienio
    colores_PP <- df_trienio %>%
      distinct(INTERVALOS_PP, color_PP) %>%
      deframe()
    
    #Etiquetas por cuartil
    etiquetas_por_intervalo <- df_trienio %>%
      distinct(color_PP, leyenda_pp) %>%
      deframe()
    
    # Mapa nacional
    mapa_nacional <- ggplot(df_trienio) +
      geom_sf(aes(fill = INTERVALOS_PP), color = "black", size = 0.2) +
      scale_fill_manual(
        values = colores_PP,
        na.value = "#cccccc",
        labels = etiquetas_por_intervalo,
        name = if (mostrar_leyenda) titulo_var else NULL,
        guide = if (mostrar_leyenda) "legend" else "none"
      ) +
      theme_minimal() +
      labs(title = paste("Triennium", trienio_actual)) +
      theme(
        legend.position = if (mostrar_leyenda) "right" else "none",
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    # Mapa CABA (inset)
    mapa_caba <- ggplot(df_caba) +
      geom_sf(aes(fill = INTERVALOS_PP), color = "black") +
      scale_fill_manual(
        values = colores_PP,
        guide = "none"
      ) +
      coord_sf(expand = FALSE) +
      theme_void() +
      theme(panel.border = element_rect(color = "black", fill = NA))
    
    # Insertar CABA en el mapa nacional
    mapa_nacional + inset_element(
      mapa_caba,
      left = 0.9, bottom = 0.5, right = 0.98, top = 0.7
    )
  }
  
  # Control de leyenda (solo en el último mapa)
  n <- length(trienios_seleccionados)
  leyenda_vec <- rep(FALSE, n)
  leyenda_vec[n] <- TRUE
  
  # Crear mapas por trienio
  mapas <- mapply(
    mapa_por_trienio,
    trienios_seleccionados,
    mostrar_leyenda = leyenda_vec,
    SIMPLIFY = FALSE
  )
  
  # Combinar los mapas y agregar título
  wrap_plots(mapas, ncol = n) #+
   # plot_annotation(
    #  title = paste(titulo_var, "por tumores de tráquea, bronquio y pulmón en mujeres. Argentina"),
    #  caption = "Fuente: elaboración propia en base a datos DEIS"
   # )
}
