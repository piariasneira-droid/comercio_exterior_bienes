#### Funciones auxiliares ----
instala_carga_librerias <- function(librerias_necesarias) {
  # Instalación depaquetes faltantes
  for (lib in librerias_necesarias) {
    if (!requireNamespace(lib, quietly = TRUE)) {
      message("Instalando ", lib, "...")
      utils::install.packages(lib, dependencies = TRUE)
    }
  }
  
  # Cargar todos los paquetes
  invisible(lapply(librerias_necesarias, function(lib) {
    message("Cargando ", lib, "...")
    base::library(lib, character.only = TRUE)
  }))
  
  message("Carga de librerías necesarias completada.")
}

#### Tema plotly y paleta -----
custom_theme_plotly <- function() {
  list(
    plot_bgcolor = 'rgba(255, 255, 255, 1)',
    paper_bgcolor = 'rgba(255, 255, 255, 1)',
    font = list(size = 14, family = "Calibri", color = "black"),
    xaxis = list(
      title = list(font = list(size = 14, family = "Calibri", color = "black")),
      tickfont = list(size = 14, family = "Calibri", color = "black"),
      ticks = "outside",
      tickcolor = "black",
      linecolor = "black",
      gridcolor = "lightgrey",
      gridwidth = 1,
      griddash = "dot",
      zeroline = FALSE
    ),
    yaxis = list(
      title = list(font = list(size = 14, family = "Calibri", color = "black")),
      tickfont = list(size = 14, family = "Calibri", color = "black"),
      ticks = "outside",
      tickcolor = "black",
      linecolor = "black",
      gridcolor = "lightgrey",
      gridwidth = 1,
      griddash = "dot",
      zeroline = FALSE
    ),
    legend = list(
      bgcolor = 'rgba(0, 0, 0, 0)',
      font = list(size = 12, family = "Calibri"),
      orientation = "h",
      x = 1,
      xanchor = "right",
      y = 1.02,
      yanchor = "bottom"
    ),
    margin = list(t = 40, b = 40, l = 40, r = 40)
  )
}

#### Renderizado DT ----
render_datatable_generico <- function(df, cols_semaforo = NULL, cols_barras = NULL,
                                      cols_barras_cien = NULL, cols_fecha = NULL,
                                      cols_enteros = NULL, pageLength = 25, decimales_defecto = 2) {
  
  # Convertir columnas de fecha al formato deseado antes de crear la tabla
  if(!is.null(cols_fecha)) {
    for(col in cols_fecha) {
      if(col %in% names(df)) {
        # Convertir a fecha si no lo es ya
        if(!inherits(df[[col]], "Date")) {
          df[[col]] <- as.Date(df[[col]])
        }
        # Formatear como YYYY-MM
        df[[col]] <- format(df[[col]], "%Y-%m")
      }
    }
  }
  
  # Crear la tabla base
  dt <- datatable(
    df,
    extensions = 'Buttons',
    options = list(
      pageLength = pageLength,
      lengthMenu = c(10, 25, 50, 100),
      dom = 'Bfrtip',
      buttons = list(
        list(extend = 'copy', text = 'Copiar', className = 'btn-sm'),
        list(extend = 'csv', text = 'CSV', className = 'btn-sm'),
        list(extend = 'excel', text = 'Excel', className = 'btn-sm')
      ),
      scrollX = TRUE,
      language = list(
        decimal = ",",
        thousands = ".",
        url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"
      ),
      columnDefs = list(
        list(
          targets = "_all",
          render = JS(
            "function(data, type, row) {
              if(type === 'display' && typeof data === 'number') {
                return data.toLocaleString('es-ES', {minimumFractionDigits: 1, maximumFractionDigits: 2});
              }
              return data;
            }"
          )
        )
      )
    ),
    class = 'display compact',
    rownames = FALSE
  )
  
  # Aplicar formato semáforo a las columnas especificadas
  if(!is.null(cols_semaforo)) {
    for(col in cols_semaforo) {
      if(col %in% names(df)) {
        dt <- dt %>%
          formatStyle(
            col,
            color = styleInterval(
              c(0),
              c("#dc3545", "#28a745")  # Rojo para negativos, verde para positivos
            ),
            fontWeight = 'bold'
          )
      }
    }
  }
  
  # Aplicar barras de progreso a las columnas especificadas
  if(!is.null(cols_barras)) {
    for(col in cols_barras) {
      if(col %in% names(df)) {
        # Obtener el máximo valor para la escala
        max_val <- max(abs(df[[col]]), na.rm = TRUE)
        
        dt <- dt %>%
          formatStyle(
            col,
            background = styleColorBar(c(0, max_val), 'lightblue'),
            backgroundSize = '98% 80%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'left center'
          )
      }
    }
  }
  
  # Aplicar barras de progreso con máximo 100 a las columnas especificadas
  if(!is.null(cols_barras_cien)) {
    for(col in cols_barras_cien) {
      if(col %in% names(df)) {
        dt <- dt %>%
          formatStyle(
            col,
            background = styleColorBar(c(0, 100), 'lightgreen'),
            backgroundSize = '98% 80%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'left center'
          )
      }
    }
  }
  
  # Formato numérico para todas las columnas numéricas
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  # Primero formatear columnas enteras especificadas sin decimales
  if(!is.null(cols_enteros)) {
    for(col in cols_enteros) {
      if(col %in% numeric_cols) {
        dt <- dt %>%
          formatRound(col, 0, mark = ".", dec.mark = ",")
        # Remover de numeric_cols para no procesarla de nuevo
        numeric_cols <- numeric_cols[numeric_cols != col]
      }
    }
  }
  
  # Luego formatear el resto de columnas numéricas
  for(col in numeric_cols) {
    # Determinar número de decimales según el tipo de columna
    if(grepl("p\\.p\\.|%", col)) {
      decimales <- 1
    } else if(grepl("M", col)) {
      decimales <- 1
    } else {
      decimales <- decimales_defecto
    }
    
    dt <- dt %>%
      formatRound(col, decimales, mark = ".", dec.mark = ",")
  }
  
  return(dt)
}


#### Carga TARIC, paises y periodo ----
cargar_taric <- function(path) {
  # Leer el fichero con vroom
  df_taric <- vroom::vroom(
    file       = path,
    delim      = "\t",
    col_names  = TRUE,
    locale     = vroom::locale(encoding = "UTF-16LE"),
    col_types  = vroom::cols(.default = vroom::col_character())
  )
  
  # Convertir a data.table
  dt_taric <- data.table::as.data.table(df_taric)
  
  # Transformaciones
  dt_taric[, nivel_taric := readr::parse_integer(nivel_taric, na = c("", "NA"))]
  dt_taric[, codint_taric := as.numeric(cod_taric)]
  dt_taric[codint_taric == as.numeric(0), codint_taric := NA]
  
  # Filtrar filas: eliminar cod_taric que empiezan con "00" y codint_taric NA
  dt_taric <- dt_taric[substr(cod_taric, 1, 2) != "00" & !is.na(codint_taric)]
  
  # Fila TOTAL
  fila_total <- data.table::data.table(
    cod_taric    = "0",
    nivel_taric  = 0L,
    taric        = "TOTAL",
    codint_taric = as.numeric(0)
  )
  
  # Combinar todo
  resultado <- data.table::rbindlist(list(fila_total, dt_taric), use.names = TRUE, fill = TRUE)
  resultado <- anade_padres_dt(resultado)
  
  return(resultado)
}

niveles_map <- c(
  "0" = "Total",
  "1" = "Capítulo",
  "2" = "Partida",
  "3" = "Subpartida",
  "4" = "Nomenclatura combinada",
  "5" = "Arancel"
)

cargar_pais <- function(path) {
  # Leer el archivo Excel
  df_pais <- openxlsx::read.xlsx(path)
  
  # Convertir a data.table
  setDT(df_pais)
  df_pais[, cod_pais := as.integer(cod_pais)]
  
  # Añadir la fila "Total"
  df_pais <- rbind(
    df_pais,
    data.table(
      pais = "000 - Total",
      region = "Total",
      continente = "Total",
      cod_pais = 0L,
      nombre = "Total"
    ),
    fill = TRUE
  )
  
  return(df_pais)
}

periodos_choices <- c(
  setNames(1:12,
           c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
             "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")),
  
  setNames(21:24,
           c("Trimestre 1","Trimestre 2","Trimestre 3","Trimestre 4")),
  
  setNames(31:32,
           c("Semestre 1","Semestre 2")),

  setNames(41, "Año entero"),
  
  setNames(51:58,
           c("Enero-Febrero","Enero-Abril","Enero-Mayo",
             "Enero-Julio","Enero-Agosto","Enero-Septiembre",
             "Enero-Octubre","Enero-Noviembre"))
)

# Mapeo de periodos a meses
periodos_map <- list(
  "1"  = 1L, "2"  = 2L, "3"  = 3L, "4"  = 4L, "5"  = 5L, "6"  = 6L,
  "7"  = 7L, "8"  = 8L, "9"  = 9L, "10" = 10L, "11" = 11L, "12" = 12L,
  "21" = 1L:3L, "22" = 4L:6L, "23" = 7L:9L, "24" = 10L:12L,
  "31" = 1L:6L, "32" = 7L:12L,
  "41" = 1L:12L,
  "51" = 1L:2L, "52" = 1L:4L, "53" = 1L:5L, "54" = 1L:7L, "55" = 1L:8L,   
  "56" = 1L:9L, "57" = 1L:10L, "58" = 1L:11L   
)

obtener_meses_periodo <- function(cod_periodo) {
  meses <- periodos_map[[as.character(cod_periodo)]]
  if (is.null(meses)) {
    return(NA_integer_) 
  }
  return(meses)
}

anade_padres_dt <- function(dt) {
  dt[, longitud := nchar(cod_taric)]
  
  # Asignar códigos padres solo si cod_taric ≠ "0"
  dt[, Capítulo := fifelse(
    cod_taric == "0", NA_character_,
    fifelse(longitud > 2, substr(cod_taric, 1, 2), NA_character_)
  )]
  
  dt[, Partida := fifelse(
    cod_taric == "0", NA_character_,
    fifelse(longitud > 4, substr(cod_taric, 1, 4), NA_character_)
  )]
  
  dt[, Subpartida := fifelse(
    cod_taric == "0", NA_character_,
    fifelse(longitud > 6, substr(cod_taric, 1, 6), NA_character_)
  )]
  
  dt[, NC := fifelse(
    cod_taric == "0", NA_character_,
    fifelse(longitud > 8, substr(cod_taric, 1, 8), NA_character_)
  )]
  
  dt[, longitud := NULL]
  
  # Crear tabla lookup con código -> descripción
  lookup <- unique(dt[, .(cod_taric, etiqueta = paste0(cod_taric, " - ", taric))])
  setkey(lookup, cod_taric)
  
  # Asignar descripciones concatenadas
  dt[, Tar := lookup[cod_taric, etiqueta, on = "cod_taric"]]
  dt[, Cap := lookup[Capítulo, etiqueta, on = "cod_taric"]]
  dt[, Par := lookup[Partida, etiqueta, on = "cod_taric"]]
  dt[, Sub := lookup[Subpartida, etiqueta, on = "cod_taric"]]
  dt[, N := lookup[NC, etiqueta, on = "cod_taric"]]
  
  return(dt)
}

cruce_taric_pais <- function(dfbase, dftar, dfpais) {
  # Join con df_taric
  df <- merge(dfbase, dftar, by.x = "cod_taric", by.y = "codint_taric", all.x = TRUE)
  setnames(df, "cod_taric.y", "cod_taric_char")
  
  # Join con df_paises
  df <- merge(df, dfpais, by.x = "pais", by.y = "cod_pais", all.x = TRUE)
  setnames(df, c("pais", "pais.y"), c("cod_pais", "pais"))
  
  return(df)
}

#### Cálculo variables ----
##### Funciones helper -----
# Función helper para procesar todos los subconjuntos de una vez
procesar_subconjuntos_periodo <- function(df_completo, v, a, var_base) {
  # Pre-filtrar solo los meses necesarios una vez
  df_meses <- df_completo[mes %in% a$meses]
  
  # Procesar todos los subconjuntos en una sola pasada usando split
  subconjuntos <- list(
    fil = df_meses[pais == v$pais & cod_taric == v$taric],
    taric = df_meses[pais == 0L & cod_taric == v$taric],
    pais = df_meses[pais == v$pais & cod_taric == 0L],
    region = df_meses[pais == 0L & cod_taric == 0L]
  )
  
  # Aplicar resumir_periodo a cada subconjunto
  list(
    df_fil = resumir_periodo(subconjuntos$fil, var_base, c("año","pais","cod_taric"), v$ano_ini, "fil"),
    total_taric = resumir_periodo(subconjuntos$taric, var_base, c("año","cod_taric"), v$ano_ini, "taric"),
    total_pais = resumir_periodo(subconjuntos$pais, var_base, c("año","pais"), v$ano_ini, "pais"),
    total_region = resumir_periodo(subconjuntos$region, var_base, c("año"), v$ano_ini, "region")
  )
}

# Función similar para datos mensuales
procesar_subconjuntos_mensual <- function(df_completo, v, a, var_base) {
  # Para mensual no filtramos por mes
  subconjuntos <- list(
    fil = df_completo[pais == v$pais & cod_taric == v$taric],
    taric = df_completo[pais == 0L & cod_taric == v$taric],
    pais = df_completo[pais == v$pais & cod_taric == 0L],
    region = df_completo[pais == 0L & cod_taric == 0L]
  )
  
  list(
    df_fil = resumir_mensual(subconjuntos$fil, var_base, c("año","mes","pais","cod_taric"), v$ano_ini, "fil"),
    total_taric = resumir_mensual(subconjuntos$taric, var_base, c("año","mes","cod_taric"), v$ano_ini, "taric"),
    total_pais = resumir_mensual(subconjuntos$pais, var_base, c("año","mes","pais"), v$ano_ini, "pais"),
    total_region = resumir_mensual(subconjuntos$region, var_base, c("año","mes"), v$ano_ini, "region")
  )
}

##### Calculo volumenes -----
# Función para resumir a nivel anual y añadir columna del año previo
resumir_periodo <- function(dt, var_base, group_vars, ano_ini, contexto = "fil") {
  # Definir sufijo en base al contexto
  suf <- switch(contexto,
                "fil"    = "",
                "taric"  = "_taric",
                "pais"   = "_pais",
                "region" = "_region",
                stop("contexto no reconocido")
  )
  
  var_anual    <- paste0(var_base, "_periodo", suf)
  var_anterior <- paste0(var_base, "_periodo_prev", suf)
  
  # Agregado anual 
  df_anual <- dt[, .(tmp_val = sum(get(var_base), na.rm = TRUE)), by = group_vars]
  setnames(df_anual, "tmp_val", var_anual)
  
  # Crear tabla del año anterior
  df_anual_prev <- copy(df_anual)
  df_anual_prev[, año := año + 1]
  setnames(df_anual_prev, var_anual, var_anterior)
  
  # Merge: añadir columna previo
  df_res <- merge(df_anual, df_anual_prev,
                  by = group_vars, all.x = TRUE)
  
  # Filtrar: quitar año_ini -1
  df_res <- df_res[año >= ano_ini]
  
  return(df_res[])
}

resumir_mensual <- function(dt, var_base, group_vars, ano_ini, contexto = "fil") {
  # Definir sufijo
  suf <- switch(contexto,
                "fil"    = "",
                "taric"  = "_taric",
                "pais"   = "_pais",
                "region" = "_region",
                stop("contexto no reconocido")
  )
  
  var_anual    <- paste0(var_base, "_periodo", suf)
  var_anterior <- paste0(var_base, "_periodo_prev", suf)
  
  # --- Seleccionar solo claves + variable ---
  df_anual <- unique(dt[, c(group_vars, var_base), with = FALSE])
  setnames(df_anual, var_base, var_anual)
  
  # --- Crear tabla del año anterior ---
  df_anual_prev <- copy(df_anual)
  df_anual_prev[, año := año + 1]
  setnames(df_anual_prev, var_anual, var_anterior)
  
  # --- Merge ---
  df_res <- merge(df_anual, df_anual_prev, by = group_vars, all.x = TRUE)
  
  # --- Filtrar ---
  df_res <- df_res[año >= ano_ini]
  
  return(df_res[])
}

##### Calculo valores referencia -----
# Función para calcular valores de referencia del índice
valores_referencia_indice_periodo <- function(dataframe, anoindex, meses, pais, taric, var_base) {
  df_idx <- dataframe[
    año == anoindex &
      mes %in% meses &
      pais == pais &
      cod_taric == taric,
    .(valor_refidx = sum(get(var_base), na.rm = TRUE)),
    by = .(pais, cod_taric)
  ]
  setnames(df_idx, "valor_refidx", paste0(var_base, "_refidx"))
  
  df_idxtaric <- dataframe[
    año == anoindex &
      mes %in% meses &
      pais == 0L &
      cod_taric == taric,
    .(valor_refidx = sum(get(var_base), na.rm = TRUE)),
    by = .(cod_taric)
  ]
  setnames(df_idxtaric, "valor_refidx", paste0(var_base, "_refidx_taric"))
  
  df_idxpais <- dataframe[
    año == anoindex &
      mes %in% meses &
      pais == pais &
      cod_taric == 0L,
    .(valor_refidx = sum(get(var_base), na.rm = TRUE)),
    by = .(pais)
  ]
  setnames(df_idxpais, "valor_refidx", paste0(var_base, "_refidx_pais"))
  
  df_idxregion <- dataframe[
    año == anoindex &
      mes %in% meses &
      pais == 0L &
      cod_taric == 0L,
    .(valor_refidx = sum(get(var_base), na.rm = TRUE))
  ]
  setnames(df_idxregion, "valor_refidx", paste0(var_base, "_refidx_region"))
  
  return(list(
    df_idx = df_idx,
    df_idxtaric = df_idxtaric,
    df_idxpais = df_idxpais,
    df_idxregion = df_idxregion
  ))
}

valores_referencia_indice_mensual <- function(dataframe, anoindex, mesindex, cpais, taric, var_base) {
  df_idx <- unique(dataframe[
    año == anoindex &
      mes == mesindex &
      pais == cpais &
      cod_taric == taric,
    c("pais", "cod_taric", var_base), with = FALSE
  ])
  setnames(df_idx, var_base, paste0(var_base, "_refidx"))
  
  df_idxtaric <- unique(dataframe[
    año == anoindex &
      mes == mesindex &
      pais == 0L &
      cod_taric == taric,
    c("cod_taric", var_base), with = FALSE
  ])
  setnames(df_idxtaric, var_base, paste0(var_base, "_refidx_taric"))
  
  df_idxpais <- unique(dataframe[
    año == anoindex &
      mes == mesindex &
      pais == cpais &
      cod_taric == 0L,
    c("pais", var_base), with = FALSE
  ])
  setnames(df_idxpais, var_base, paste0(var_base, "_refidx_pais"))
  
  df_idxregion <- unique(dataframe[
    año == anoindex &
      mes == mesindex &
      pais == 0L &
      cod_taric == 0L,
    c(var_base), with = FALSE
  ])
  setnames(df_idxregion, var_base, paste0(var_base, "_refidx_region"))
  
  return(list(
    df_idx = df_idx,
    df_idxtaric = df_idxtaric,
    df_idxpais = df_idxpais,
    df_idxregion = df_idxregion
  ))
}

##### Calculo variables adicionales -----
preparacion_dataplot <- function(dt, var_base, factor) {
  # Construimos nombres de columnas dinámicas
  col_periodo      <- paste0(var_base, "_periodo")
  col_periodo_prev <- paste0(var_base, "_periodo_prev")
  col_refidx       <- paste0(var_base, "_refidx")
  col_region       <- paste0(var_base, "_periodo_region")
  col_region_prev  <- paste0(var_base, "_periodo_prev_region")
  col_taric        <- paste0(var_base, "_periodo_taric")
  col_taric_prev   <- paste0(var_base, "_periodo_prev_taric")
  col_pais         <- paste0(var_base, "_periodo_pais")
  col_pais_prev    <- paste0(var_base, "_periodo_prev_pais")
  col_refidx_taric  <- paste0(var_base, "_refidx_taric")
  col_refidx_pais   <- paste0(var_base, "_refidx_pais")
  col_refidx_region <- paste0(var_base, "_refidx_region")
  col_periodo_taric   <- paste0(var_base, "_periodo_taric")
  col_periodo_pais    <- paste0(var_base, "_periodo_pais")
  col_periodo_region  <- paste0(var_base, "_periodo_region")
  
  # Calcular y añadir diferencias, TVA, contribuciones, índice y ranking
  dt[, diferencia := (get(col_periodo) - get(col_periodo_prev)), 
     by = .(año, pais, cod_taric)]
  
  dt[, tva := (diferencia / get(col_periodo_prev) * 100), 
     by = .(año, pais, cod_taric)]
  
  dt[, con := (diferencia / get(col_region_prev) * 100), 
     by = .(año, pais, cod_taric)]
  
  # Índices
  dt[, idx := (get(col_periodo) / get(col_refidx) * 100), 
     by = .(año, pais, cod_taric)]
  
  dt[, idx_taric := (get(col_periodo_taric) / get(col_refidx_taric) * 100), 
     by = .(año, cod_taric)]
  
  dt[, idx_pais := (get(col_periodo_pais) / get(col_refidx_pais) * 100), 
     by = .(año, pais)]
  
  dt[, idx_region := (get(col_periodo_region) / get(col_refidx_region) * 100), 
     by = año]
  
  # Pesos relativos
  dt[, peso := ((get(col_periodo) / get(col_region)) * 100), 
     by = .(año, pais, cod_taric)]
  
  dt[, pesotar := ((get(col_periodo) / get(col_taric)) * 100), 
     by = .(año, pais, cod_taric)]
  
  dt[, pesopais := ((get(col_periodo) / get(col_pais)) * 100), 
     by = .(año, pais, cod_taric)]
  
  dt[, rank := frank(-get(col_periodo), ties.method = "min")]
  
  # Modificar columnas dividiendo por el factor
  cols_a_modificar <- c(
    col_periodo, col_periodo_prev, col_refidx,
    col_region, col_region_prev, col_taric,
    col_taric_prev, col_pais, col_pais_prev,
    col_refidx_taric, col_refidx_pais, col_refidx_region,
    "diferencia"
  )
  
  for (col in cols_a_modificar) {
    if (col %in% names(dt)) {
      dt[, (col) := get(col) / factor]
    }
  }
  
  # Eliminar columnas que contienen "prev" y refidx ya usados
  dt[, c(
    col_refidx, col_refidx_taric, col_refidx_pais, col_refidx_region,
    col_region_prev, col_taric_prev, col_pais_prev,
    col_periodo_taric, col_periodo_pais, col_periodo_region
  ) := NULL]
  
  return(dt)
}

# Cálculo de las medias móviles
obtencion_medias_moviles <- function(df) {
  # Crear copia
  df_copy <- copy(df)
  setorder(df_copy, fecha)
  
  # Calcular media móvil 12 meses para euros_periodo
  df_copy[, mm12 := {
    n <- .N
    resultado <- numeric(n)
    
    for (i in 1:n) {
      fecha_actual <- fecha[i]
      fecha_limite <- fecha_actual %m-% months(11)
      
      resultado[i] <- sum(euros_periodo[fecha >= fecha_limite & fecha <= fecha_actual], 
                          na.rm = TRUE) / 12
    }
    
    resultado
  }]
  
  # Calcular media móvil 12 meses para euros_periodo_prev
  df_copy[, mm12prev := {
    n <- .N
    resultado <- numeric(n)
    
    for (i in 1:n) {
      fecha_actual <- fecha[i]
      fecha_limite <- fecha_actual %m-% months(11)
      
      resultado[i] <- sum(euros_periodo_prev[fecha >= fecha_limite & fecha <= fecha_actual], 
                          na.rm = TRUE) / 12
    }
    
    resultado
  }]
  
  # Calcular tasa de variación de las medias móviles
  df_copy[, mm12tva := (mm12 - mm12prev) / mm12prev * 100]
  df_copy[, mm12prev := NULL]
  
  return(df_copy)
}

#### Plots ----
grafica_lineas_periodo_periodo <- function(datafr, nom_regi, nom_flujo, nivel_taric, var, varud, colorplot){
  
  # nombres dinámicos de columnas
  col_volumen <- paste0(var, "_periodo")
  col_volumen_previo <- paste0(var, "_periodo_prev")
  
  # etiquetas dinámicas
  nombre_volumen <- paste0("Volumen (", varud, "): ")
  nombre_volumen_previo <- paste0("Volumen previo (", varud, "): ")
  nombre_diferencia <- paste0("Diferencia absoluta (", varud, "): ")
  
  # generar hover_text
  datafr[, hover_text := paste0(
    "Región: ", nom_regi, "<br>",
    "Flujo: ", nom_flujo, "<br>",
    "Año: ", año, "<br>",
    "País: ", pais, "<br>",
    "TARIC: ", Tar, "<br>",
    
    # jerarquía TARIC
    ifelse(nivel_taric <= 1, "",
           ifelse(nivel_taric == 2, paste0("Capítulo: ", Cap, "<br>"),
                  ifelse(nivel_taric == 3, paste0("Capítulo: ", Cap, "<br>",
                                                  "Partida: ", Par, "<br>"),
                         ifelse(nivel_taric == 4, paste0("Capítulo: ", Cap, "<br>",
                                                         "Partida: ", Par, "<br>",
                                                         "Subpartida: ", Sub, "<br>"),
                                paste0("Capítulo: ", Cap, "<br>",
                                       "Partida: ", Par, "<br>",
                                       "Subpartida: ", Sub, "<br>",
                                       "NC: ", N, "<br>"))))),
    
    # variables dinámicas
    nombre_volumen, format(round(get(col_volumen), 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    "Index: ", round(idx, 1), "<br>",
    "Ranking: ", rank, "<br>",
    nombre_volumen_previo, format(round(get(col_volumen_previo), 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    nombre_diferencia, format(round(diferencia, 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    "Tasa de variación: ", round(tva, 1), "%<br>",
    "Contribución: ", round(con, 1), " p.p.<br>",
    "Peso (%): ", round(peso, 1), "%<br>",
    "Peso en país (%): ", round(pesopais, 1), "%<br>",
    "Peso en TARIC (%): ", round(pesotar, 1), "%<br>"
  )]
  
  # Crear el gráfico de líneas
  fig <- plot_ly(data = datafr,
                 x = ~año,
                 y = ~get(col_volumen),
                 color = I(colorplot),
                 type = 'scatter',
                 mode = 'lines+markers',
                 text = ~hover_text,
                 hovertemplate = '%{text}<extra></extra>',
                 line = list(width = 3),
                 marker = list(size = 6)) %>%
    layout(
      xaxis = list(
        title = "Año",
        showgrid = TRUE,
        gridcolor = 'lightgray'
      ),
      yaxis = list(
        title = col_volumen,
        showgrid = TRUE,
        gridcolor = 'lightgray'
      ),
      hovermode = 'x unified'
    )
  
  # aplicar tema
  fig <- fig %>% layout(custom_theme_plotly())
  
  return(fig)
}
