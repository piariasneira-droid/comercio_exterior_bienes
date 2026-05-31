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

# Formatear valores con separadores de miles y decimales
formatear_numero <- function(x) {
  format(round(x, 1), decimal.mark = ",", big.mark = ".", nsmall = 1)
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
                                      cols_enteros = NULL, pageLength = 25, decimales_defecto = 2, ...) {
  
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
    rownames = FALSE,
    ... # Aquí se pasa el argumento 'escape = FALSE' y otros si los hay
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

elimina_cols_sobrante <- function(DT, cols = character()) {
  cols_base <- c("cod_taric_char", "taric", "Capítulo", "Partida",
                 "Subpartida", "NC", "region", "continente", "nombre")
  cols_all <- unique(c(cols_base, cols))
  cols_drop <- intersect(cols_all, names(DT))
  if (length(cols_drop)) DT[, (cols_drop) := NULL]
  DT
}

#### Procesamiento de datos ----
preparacion_dataplot_volumenes <- function(dt, var_base, agrupacion, ndata, factor, ref, ref_act) {
  
  # Validar data.table vacío
  if (nrow(dt) == 0) {
    warning("El data.table está vacío")
    return(dt[])
  }
  
  # Validar que el factor no sea cero, si es NA o 0 se usa 1.
  if (is.na(factor) || factor == 0) {
    warning("El factor es 0 o NA, no se aplicará normalización")
    factor <- 1
  }
  
  # Crear nombres de columnas dinámicos
  col_periodo <- paste0(var_base, "_periodo")
  col_periodo_prev <- paste0(var_base, "_periodo_prev")
  
  # Validar existencia de columnas
  if (!(col_periodo %in% names(dt)) || !(col_periodo_prev %in% names(dt))) {
    stop("Las columnas necesarias no existen en el data.table.")
  }
  
  # Ranking volumen
  dt[, rank := frank(-get(col_periodo), ties.method = "min")]
  dt <- dt[order(rank)][1:min(.N, ndata)]
  
  # Cálculos de diferencia, TVA y contribución
  dt[, diferencia := get(col_periodo) - get(col_periodo_prev), by = agrupacion]
  dt[, tva := diferencia / get(col_periodo_prev) * 100, by = agrupacion]
  dt[, con := ifelse(ref != 0, diferencia / ref * 100, NA), by = agrupacion]
  dt[, peso := get(col_periodo) / ref_act * 100]
  
  # Normalización con verificaciones
  cols_a_modificar <- c(col_periodo, col_periodo_prev, "diferencia")
  for (col in cols_a_modificar) {
    if (col %in% names(dt)) {
      dt[, (col) := get(col) / factor]
    }
  }
  
  return(dt[])
}

preparacion_dataplot_contribuciones <- function(dt, var_base, agrupacion, ndata, ordenacion, factor, ref) {
  
  # Validar data.table vacío
  if (nrow(dt) == 0) {
    warning("El data.table está vacío")
    return(dt[])
  }
  
  # Validar que el factor no sea cero, si es NA o 0 se usa 1.
  if (is.na(factor) || factor == 0) {
    warning("El factor es 0 o NA, no se aplicará normalización")
    factor <- 1
  }
  
  # Crear nombres de columnas dinámicos
  col_periodo <- paste0(var_base, "_periodo")
  col_periodo_prev <- paste0(var_base, "_periodo_prev")
  
  # Validar existencia de columnas
  if (!(col_periodo %in% names(dt)) || !(col_periodo_prev %in% names(dt))) {
    stop("Las columnas necesarias no existen en el data.table.")
  }
  
  # Cálculos de diferencia, TVA y contribución
  dt[, diferencia := get(col_periodo) - get(col_periodo_prev), by = agrupacion]
  dt[, tva := diferencia / get(col_periodo_prev) * 100, by = agrupacion]
  dt[, con := ifelse(ref != 0, diferencia / ref * 100, NA), by = agrupacion]
  
  # Ranking y ordenación según el parámetro "ordenacion"
  if (ordenacion == "positivas") {
    # Ranking para diferencias positivas: 1 = diferencia más alta
    dt[, rank := frank(-diferencia, ties.method = "min")]
    # Filtra solo las diferencias positivas y toma las de mejor ranking
    dt <- dt[!is.na(diferencia) & diferencia > 0][order(rank)][1:min(.N, ndata)]
  } else if (ordenacion == "negativas") {
    # Ranking para diferencias negativas: 1 = diferencia más baja (más negativo)
    dt[, rank := frank(diferencia, ties.method = "min")]
    # Filtra solo las diferencias negativas y toma las de mejor ranking
    dt <- dt[!is.na(diferencia) & diferencia < 0][order(rank)][1:min(.N, ndata)]
  } else {
    # Si la ordenación es otra (o no está definida), usa el valor absoluto
    dt[, rank := frank(-abs(diferencia), ties.method = "min")]
    # Filtra y toma las de mejor ranking en valor absoluto
    dt <- dt[!is.na(diferencia)][order(rank)][1:min(.N, ndata)]
  }
  
  # Normalización con verificaciones
  cols_a_modificar <- c(col_periodo, col_periodo_prev, "diferencia")
  for (col in cols_a_modificar) {
    if (col %in% names(dt)) {
      dt[, (col) := get(col) / factor]
    }
  }
  
  return(dt[])
}

# Función para crear expresión dinámica
crear_expresion_dinamica <- function(nivel_taric) {
  switch(as.character(nivel_taric),
         "2" = quote(list("Capítulo" = Cap)),
         "3" = quote(list("Capítulo" = Cap, "Partida" = Par)),
         "4" = quote(list("Capítulo" = Cap, "Partida" = Par, "Subpartida" = Sub)),
         "5" = quote(list("Capítulo" = Cap, "Partida" = Par, "Subpartida" = Sub, "Nomenclatura Combinada" = N)),
         quote(list())
  )
}

# Función para crear el texto descriptivo
crear_texto_descriptivo_fila <- function(i, dt, aux, vals, nombre_volumen_prev, nombre_volumen, nombre_diferencia) {
  
  # Verificar si las columnas "Taric" y "País" existen
  tiene_taric <- "Taric" %in% names(dt)
  tiene_pais <- "País" %in% names(dt)
  
  # Crear el texto base dinámicamente
  texto_base <- ""
  
  if (tiene_pais) {
    pais_completo <- as.character(dt[[i, "País"]])
    pais_sin_codigo <- substr(pais_completo, 5, nchar(pais_completo))
    texto_base <- pais_sin_codigo
  }
  
  if (tiene_taric) {
    taric_valor <- as.character(dt[[i, "Taric"]])
    if (tiene_pais) {
      texto_base <- paste0(aux$texto_ini, " <i>\"", taric_valor, "\"</i> de ", pais_sin_codigo)
    } else {
      texto_base <- paste0(aux$texto_ini, " <i>\"", taric_valor, "\"</i>")
    }
  }
  
  # Poner la primera letra en mayúscula
  if (nchar(texto_base) > 0) {
    texto_base <- paste0(toupper(substr(texto_base, 1, 1)), substr(texto_base, 2, nchar(texto_base)))
  }
  
  # Extraer valores de la fila i
  vol_prev <- as.numeric(dt[[i, nombre_volumen_prev]])
  vol_act <- as.numeric(dt[[i, nombre_volumen]])
  dif <- as.numeric(dt[[i, nombre_diferencia]])
  tva <- as.numeric(dt[[i, "TVA (%)"]])
  contrib <- as.numeric(dt[[i, "Contribución (p.p.)"]])
  
  # Formatear valores
  vol_prev_fmt <- formatear_numero(vol_prev)
  vol_act_fmt <- formatear_numero(vol_act)
  dif_fmt <- formatear_numero(abs(dif))
  tva_fmt <- formatear_numero(tva)
  contrib_fmt <- formatear_numero(contrib)
  
  # Determinar si es incremento o disminución
  texto_cambio <- ifelse(dif >= 0, "un incremento", "una disminución")
  
  # Determinar el signo de la contribución
  signo_contrib <- ifelse(contrib < 0, "", "+")
  
  # Construir el texto
  texto <- paste0(
    texto_base,
    " ha pasado de ", vol_prev_fmt, " ", aux$texud, " en ", aux$texto_periodo, " ", vals$ano - 1,
    " a ", vol_act_fmt, " ", aux$texud, " en ", aux$texto_periodo, " ", vals$ano,
    ". Esto supone ", texto_cambio,
    " de ", dif_fmt, " ", aux$texud, " (Δ <b>",
    tva_fmt, "%</b>)",
    " con una repercusión de <b>",
    signo_contrib, contrib_fmt,
    " puntos porcentuales.</b>"
  )
  
  return(texto)
}

crear_tabla_renderizada <- function(df, aux, vals, decimales_defecto, orden_descendente = TRUE) {
  # Crear el nombre dinámico de la columna
  col_dinamica <- paste0(aux$var, "_periodo")
  col_dinamica_prev <- paste0(aux$var, "_periodo_prev")
  
  # Crear nombres de columnas con unidades
  nombre_volumen <- paste0("Volumen (", aux$varud, ")")
  nombre_volumen_prev <- paste0("Volumen previo (", aux$varud, ")")
  nombre_diferencia <- paste0("Diferencia (", aux$varud, ")")
  
  # Determinar las columnas de identificación basadas en el df
  columnas_id <- c()
  nombres_id <- c()
  
  # Columnas básicas de identificación
  if ("pais" %in% names(df)) {
    columnas_id <- c(columnas_id, "pais")
    nombres_id <- c(nombres_id, "País")
  }
  if ("Tar" %in% names(df)) {
    columnas_id <- c(columnas_id, "Tar")
    nombres_id <- c(nombres_id, "Taric")
  }
  
  # Columnas jerárquicas TARIC según nivel
  if (vals$nivel >= 2 && "Cap" %in% names(df)) {
    columnas_id <- c(columnas_id, "Cap")
    nombres_id <- c(nombres_id, "Capítulo")
  }
  if (vals$nivel >= 3 && "Par" %in% names(df)) {
    columnas_id <- c(columnas_id, "Par")
    nombres_id <- c(nombres_id, "Partida")
  }
  if (vals$nivel >= 4 && "Sub" %in% names(df)) {
    columnas_id <- c(columnas_id, "Sub")
    nombres_id <- c(nombres_id, "Subpartida")
  }
  if (vals$nivel == 5 && "N" %in% names(df)) {
    columnas_id <- c(columnas_id, "N")
    nombres_id <- c(nombres_id, "Nomenclatura Combinada")
  }
  
  # Columnas adicionales y sus nombres
  columnas_adicionales <- c(
    col_dinamica,
    col_dinamica_prev,
    "diferencia",
    "tva",
    "con",
    "rank"
  )
  nombres_adicionales <- c(
    "Volumen",
    "Volumenp",
    "Diferencia",
    "TVA (%)",
    "Contribución (p.p.)",
    "Ranking"
  )
  
  # Seleccionar las columnas en el data.frame
  tabla <- df[, c(columnas_id, columnas_adicionales), with = FALSE]
  
  # Renombrar las columnas
  setnames(tabla, c(columnas_id, columnas_adicionales), c(nombres_id, nombres_adicionales))
  
  # Ordenar tabla y calcular columnas acumuladas
  if (orden_descendente) {
    tabla <- tabla[order(-get("Diferencia"))]
  } else {
    tabla <- tabla[order(get("Diferencia"))]
  }
  
  tabla[, `Diferencia (acumulada)` := cumsum(get("Diferencia"))]
  tabla[, `Contribución (acumulada)` := cumsum(`Contribución (p.p.)`)]
  
  # Renombrar columnas con unidades para que coincidan con la función `crear_texto_descriptivo`
  setnames(tabla, "Volumen", nombre_volumen)
  setnames(tabla, "Volumenp", nombre_volumen_prev)
  setnames(tabla, "Diferencia", nombre_diferencia)
  
  # Crear índice temporal
  tabla[, idx := .I]
  
  # Aplicar función
  for(i in 1:nrow(tabla)) {
    tabla[i, texto := crear_texto_descriptivo_fila(
      i = i,
      dt = tabla,
      aux = aux,
      vals = vals,
      nombre_volumen_prev = nombre_volumen_prev,
      nombre_volumen = nombre_volumen,
      nombre_diferencia = nombre_diferencia
    )]
  }
  
  # Eliminar índice temporal
  tabla[, idx := NULL]
  
  # Renderizar la tabla
  render_datatable_generico(
    df = tabla,
    cols_semaforo = c("TVA (%)", "Contribución (p.p.)"),
    cols_barras = c(""),
    cols_barras_cien = c(""),
    cols_enteros = c("Ranking"),
    decimales_defecto = decimales_defecto,
    escape = FALSE
  )
}

#### Plots ----
##### Volumen -----
grafica_volumen_taric <- function(datafr, aux, vals) {
  
  # Extraer valores de las listas
  nom_flujo <- aux$nombre_flujo
  nivel_taric <- vals$nivel
  var <- aux$var
  varud <- aux$varud
  colorplot1 <- aux$paleta["col1"] 
  colorplot2 <- aux$paleta["col2"]
  
  # nombres dinámicos de columnas
  col_volumen <- paste0(var, "_periodo")
  col_volumen_previo <- paste0(var, "_periodo_prev")
  
  # etiquetas dinámicas
  nombre_volumen <- paste0("Volumen ", vals$ano, " (", varud, ")")
  nombre_volumen_previo <- paste0("Volumen ", vals$ano - 1, " (", varud, ")")
  nombre_diferencia <- paste0("Diferencia absoluta (", varud, ")")
  
  # Ordenar datafr por volumen actual (descendente)
  datafr <- datafr[order(-get(col_volumen))]
  
  # Crear factor ordenado para el eje X basado en el orden de volumen actual
  datafr[, cod_taric_factor := factor(cod_taric, levels = unique(cod_taric))]
  
  # Jerarquía TARIC (parte común)
  datafr[, jerarquia_taric := ifelse(nivel_taric <= 1, "",
                                     ifelse(nivel_taric == 2, paste0("Capítulo: ", Cap, "<br>"),
                                            ifelse(nivel_taric == 3, paste0("Capítulo: ", Cap, "<br>",
                                                                            "Partida: ", Par, "<br>"),
                                                   ifelse(nivel_taric == 4, paste0("Capítulo: ", Cap, "<br>",
                                                                                   "Partida: ", Par, "<br>",
                                                                                   "Subpartida: ", Sub, "<br>"),
                                                          paste0("Capítulo: ", Cap, "<br>",
                                                                 "Partida: ", Par, "<br>",
                                                                 "Subpartida: ", Sub, "<br>",
                                                                 "NC: ", N, "<br>")))))]
  
  # Hover para año ACTUAL (con todos los datos)
  datafr[, hover_text_actual := paste0(
    "Flujo: ", nom_flujo, "<br>",
    "Región: ", aux$nombre_region, "<br>",
    "TARIC: ", Tar, "<br>",
    jerarquia_taric,
    nombre_volumen, ": ", format(round(get(col_volumen), 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    "Ranking: ", rank, "<br>",
    nombre_diferencia, ": ", format(round(diferencia, 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    "Tasa de variación: ", round(tva, 1), "%<br>",
    "Contribución: ", round(con, 1), " p.p."
  )]
  
  # Hover para año PREVIO (solo volumen)
  datafr[, hover_text_previo := paste0(
    "Flujo: ", nom_flujo, "<br>",
    "Región: ", aux$nombre_region, "<br>",
    "TARIC: ", Tar, "<br>",
    jerarquia_taric,
    nombre_volumen_previo, ": ", format(round(get(col_volumen_previo), 1), big.mark = ".", decimal.mark = ",", nsmall = 1)
  )]
  
  # Figura con barras agrupadas
  fig <- plot_ly() %>%
    add_bars(
      data = datafr,
      x = ~cod_taric_factor,
      y = ~get(col_volumen),
      name = as.character(vals$ano),
      marker = list(color = colorplot1),
      hovertext = ~hover_text_actual,
      hoverinfo = 'text'
    ) %>%
    add_bars(
      data = datafr,
      x = ~cod_taric_factor,
      y = ~get(col_volumen_previo),
      name = as.character(vals$ano - 1),
      marker = list(color = colorplot2),
      hovertext = ~hover_text_previo,
      hoverinfo = 'text'
    ) %>%
    layout(
      barmode = 'group',
      xaxis = list(
        title = "Código TARIC",
        tickfont = list(size = 10),
        showspikes = TRUE,
        spikemode = "across",
        spikesnap = "cursor",
        spikecolor = "grey",
        spikethickness = 1,
        spikedash = "dash"
      ),
      yaxis = list(
        title = list(text = paste0("Volumen (", varud, ")"), standoff = 20),
        automargin = TRUE,
        tickformat = ",.0f"  # Formato entero con separador de miles
      ),
      margin = list(l = 80, b = 100),
      showlegend = FALSE,
      hovermode = "closest",
      hoverdistance = 20,
      spikedistance = -1
    )
  
  fig <- fig %>% layout(custom_theme_plotly())
  
  return(fig)
}

grafica_volumen_paises <- function(datafr, aux, vals) {
  
  # Extraer valores de las listas
  nom_flujo <- aux$nombre_flujo
  var <- aux$var
  varud <- aux$varud
  colorplot1 <- aux$paleta["col1"] 
  colorplot2 <- aux$paleta["col2"]
  
  # nombres dinámicos de columnas
  col_volumen <- paste0(var, "_periodo")
  col_volumen_previo <- paste0(var, "_periodo_prev")
  
  # etiquetas dinámicas
  nombre_volumen <- paste0("Volumen ", vals$ano, " (", varud, ")")
  nombre_volumen_previo <- paste0("Volumen ", vals$ano - 1, " (", varud, ")")
  nombre_diferencia <- paste0("Diferencia absoluta (", varud, ")")
  
  # Ordenar datafr por volumen actual (descendente)
  datafr <- datafr[order(-get(col_volumen))]
  
  # Crear código de país con 3 dígitos y factor ordenado
  datafr[, cod_pais_3dig := sprintf("%03d", as.integer(cod_pais))]
  datafr[, cod_pais_factor := factor(cod_pais_3dig, levels = unique(cod_pais_3dig))]
  
  # Hover para año ACTUAL (con todos los datos)
  datafr[, hover_text_actual := paste0(
    "Flujo: ", nom_flujo, "<br>",
    "Región: ", aux$nombre_region, "<br>",
    "País: ", pais, "<br>",
    nombre_volumen, ": ", format(round(get(col_volumen), 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    "Ranking: ", rank, "<br>",
    nombre_diferencia, ": ", format(round(diferencia, 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    "Tasa de variación: ", round(tva, 1), "%<br>",
    "Contribución: ", round(con, 1), " p.p."
  )]
  
  # Hover para año PREVIO (solo volumen)
  datafr[, hover_text_previo := paste0(
    "Flujo: ", nom_flujo, "<br>",
    "Región: ", aux$nombre_region, "<br>",
    "País: ", pais, "<br>",
    nombre_volumen_previo, ": ", format(round(get(col_volumen_previo), 1), big.mark = ".", decimal.mark = ",", nsmall = 1)
  )]
  
  # Figura con barras agrupadas
  fig <- plot_ly() %>%
    add_bars(
      data = datafr,
      x = ~cod_pais_factor,
      y = ~get(col_volumen),
      name = as.character(vals$ano),
      marker = list(color = colorplot1),
      hovertext = ~hover_text_actual,
      hoverinfo = 'text'
    ) %>%
    add_bars(
      data = datafr,
      x = ~cod_pais_factor,
      y = ~get(col_volumen_previo),
      name = as.character(vals$ano - 1),
      marker = list(color = colorplot2),
      hovertext = ~hover_text_previo,
      hoverinfo = 'text'
    ) %>%
    layout(
      barmode = 'group',
      xaxis = list(
        title = "Código País",
        tickfont = list(size = 10),
        showspikes = TRUE,
        spikemode = "across",
        spikesnap = "cursor",
        spikecolor = "grey",
        spikethickness = 1,
        spikedash = "dash"
      ),
      yaxis = list(
        title = list(text = paste0("Volumen (", varud, ")"), standoff = 20),
        automargin = TRUE,
        tickformat = ",.0f"  # Formato entero con separador de miles
      ),
      margin = list(l = 80, b = 100),
      showlegend = FALSE,
      hovermode = "closest",
      hoverdistance = 20,
      spikedistance = -1
    )
  
  fig <- fig %>% layout(custom_theme_plotly())
  
  return(fig)
}

grafica_volumen_pt <- function(datafr, aux, vals) {
  
  # Extraer valores de las listas
  nom_flujo <- aux$nombre_flujo
  nivel_taric <- vals$nivel
  var <- aux$var
  varud <- aux$varud
  colorplot1 <- aux$paleta["col1"] 
  colorplot2 <- aux$paleta["col2"]
  
  # nombres dinámicos de columnas
  col_volumen <- paste0(var, "_periodo")
  col_volumen_previo <- paste0(var, "_periodo_prev")
  
  # etiquetas dinámicas
  nombre_volumen <- paste0("Volumen ", vals$ano, " (", varud, ")")
  nombre_volumen_previo <- paste0("Volumen ", vals$ano - 1, " (", varud, ")")
  nombre_diferencia <- paste0("Diferencia absoluta (", varud, ")")
  
  # Ordenar datafr por volumen actual (descendente)
  datafr <- datafr[order(-get(col_volumen))]
  
  # Crear etiquetas combinadas para el eje X
  datafr[, cod_pais_3dig := sprintf("%03d", as.integer(cod_pais))]
  datafr[, cod_taric_str := paste0("T:", cod_taric)]
  datafr[, cod_pais_str := paste0("P:", cod_pais_3dig)]
  datafr[, cod_concat := paste0(cod_taric_str, " - ", cod_pais_str)]
  datafr[, cod_concat_factor := factor(cod_concat, levels = unique(cod_concat))]
  
  # Jerarquía TARIC (parte común)
  datafr[, jerarquia_taric := ifelse(nivel_taric <= 1, "",
                                     ifelse(nivel_taric == 2, paste0("Capítulo: ", Cap, "<br>"),
                                            ifelse(nivel_taric == 3, paste0("Capítulo: ", Cap, "<br>",
                                                                            "Partida: ", Par, "<br>"),
                                                   ifelse(nivel_taric == 4, paste0("Capítulo: ", Cap, "<br>",
                                                                                   "Partida: ", Par, "<br>",
                                                                                   "Subpartida: ", Sub, "<br>"),
                                                          paste0("Capítulo: ", Cap, "<br>",
                                                                 "Partida: ", Par, "<br>",
                                                                 "Subpartida: ", Sub, "<br>",
                                                                 "NC: ", N, "<br>")))))]
  
  # Hover para año ACTUAL (con todos los datos)
  datafr[, hover_text_actual := paste0(
    "Flujo: ", nom_flujo, "<br>",
    "Región: ", aux$nombre_region, "<br>",
    "TARIC: ", Tar, "<br>",
    "País: ", pais, "<br>",
    jerarquia_taric,
    nombre_volumen, ": ", format(round(get(col_volumen), 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    "Ranking: ", rank, "<br>",
    nombre_diferencia, ": ", format(round(diferencia, 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    "Tasa de variación: ", round(tva, 1), "%<br>",
    "Contribución: ", round(con, 1), " p.p."
  )]
  
  # Hover para año PREVIO (solo volumen)
  datafr[, hover_text_previo := paste0(
    "Flujo: ", nom_flujo, "<br>",
    "Región: ", aux$nombre_region, "<br>",
    "TARIC: ", Tar, "<br>",
    "País: ", pais, "<br>",
    jerarquia_taric,
    nombre_volumen_previo, ": ", format(round(get(col_volumen_previo), 1), big.mark = ".", decimal.mark = ",", nsmall = 1)
  )]
  
  # Figura con barras agrupadas
  fig <- plot_ly() %>%
    add_bars(
      data = datafr,
      x = ~cod_concat_factor,
      y = ~get(col_volumen),
      name = as.character(vals$ano),
      marker = list(color = colorplot1),
      hovertext = ~hover_text_actual,
      hoverinfo = 'text'
    ) %>%
    add_bars(
      data = datafr,
      x = ~cod_concat_factor,
      y = ~get(col_volumen_previo),
      name = as.character(vals$ano - 1),
      marker = list(color = colorplot2),
      hovertext = ~hover_text_previo,
      hoverinfo = 'text'
    ) %>%
    layout(
      barmode = 'group',
      xaxis = list(
        title = "TARIC - País",
        tickfont = list(size = 9),
        tickangle = -45,
        showspikes = TRUE,
        spikemode = "across",
        spikesnap = "cursor",
        spikecolor = "grey",
        spikethickness = 1,
        spikedash = "dash"
      ),
      yaxis = list(
        title = list(text = paste0("Volumen (", varud, ")"), standoff = 20),
        automargin = TRUE,
        tickformat = ",.0f"  # Formato entero con separador de miles
      ),
      margin = list(l = 80, b = 120),
      showlegend = FALSE,
      hovermode = "closest",
      hoverdistance = 20,
      spikedistance = -1
    )
  
  fig <- fig %>% layout(custom_theme_plotly())
  
  return(fig)
}

##### Contribuciones -----
grafica_contribuciones_taric <- function(datafr, aux, vals) {
  
  # Extraer valores de las listas
  nom_flujo <- aux$nombre_flujo
  nivel_taric <- vals$nivel
  var <- aux$var
  varud <- aux$varud
  colorplot <- aux$paleta["col1"] 
  
  # nombres dinámicos de columnas
  col_volumen <- paste0(var, "_periodo")
  col_volumen_previo <- paste0(var, "_periodo_prev")
  
  # etiquetas dinámicas
  nombre_volumen <- paste0("Volumen (", varud, "): ")
  nombre_volumen_previo <- paste0("Volumen previo (", varud, "): ")
  nombre_diferencia <- paste0("Diferencia absoluta (", varud, "): ")
  
  # Ordenar datafr por ranking
  datafr <- datafr[order(rank)]
  
  # generar hover_text
  datafr[, hover_text := paste0(
    "Flujo: ", nom_flujo, "<br>",
    "Región: ", aux$nombre_region, "<br>",
    "TARIC: ", Tar, "<br>",
    
    # jerarquía TARIC (simplificada ya que nivel_taric es 1)
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
    "Ranking: ", rank, "<br>",
    nombre_volumen_previo, format(round(get(col_volumen_previo), 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    nombre_diferencia, format(round(diferencia, 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    "Tasa de variación: ", round(tva, 1), "%<br>",
    "Contribución: ", round(con, 1), " p.p."
  )]
  
  # Crear factor ordenado para el eje y basado en el ranking
  datafr[, cod_taric_factor := factor(cod_taric, levels = cod_taric[order(-rank)])]
  
  # Figura
  fig <- plot_ly() %>%
    add_bars(
      data = datafr,
      x = ~con,
      y = ~cod_taric_factor,
      marker = list(color = colorplot),
      orientation = 'h',
      hovertext = ~hover_text,
      hoverinfo = 'text'
    ) %>%
    layout(
      xaxis = list(
        title = "Contribución (p.p.)",
        tickformat = ".1f",
        ticksuffix = " p.p.",
        showspikes = TRUE,
        spikemode = "across",
        spikesnap = "cursor",
        spikecolor = "grey",
        spikethickness = 1,
        spikedash = "dash"
      ),
      yaxis = list(
        title = list(text = "Código TARIC", standoff = 20),
        automargin = TRUE,
        tickfont = list(size = 10)
      ),
      margin = list(l = 80),
      showlegend = FALSE,
      hovermode = "closest",
      hoverdistance = 20,
      spikedistance = -1
    )
  
  fig <- fig %>% layout(custom_theme_plotly())
  
  return(fig)
}

grafica_contribuciones_paises <- function(datafr, aux, vals) {
  
  # Extraer valores de las listas
  nom_flujo <- aux$nombre_flujo
  nivel_taric <- vals$nivel
  var <- aux$var
  varud <- aux$varud
  colorplot <- aux$paleta["col2"] 
  
  # nombres dinámicos de columnas
  col_volumen <- paste0(var, "_periodo")
  col_volumen_previo <- paste0(var, "_periodo_prev")
  
  # etiquetas dinámicas
  nombre_volumen <- paste0("Volumen (", varud, "): ")
  nombre_volumen_previo <- paste0("Volumen previo (", varud, "): ")
  nombre_diferencia <- paste0("Diferencia absoluta (", varud, "): ")
  
  # Ordenar datafr por ranking
  datafr <- datafr[order(rank)]
  
  # generar hover_text
  datafr[, hover_text := paste0(
    "Flujo: ", nom_flujo, "<br>",
    "Región: ", aux$nombre_region, "<br>",
    "País: ", pais, "<br>",
    
    # jerarquía TARIC (simplificada ya que nivel_taric es 1)
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
    "Ranking: ", rank, "<br>",
    nombre_volumen_previo, format(round(get(col_volumen_previo), 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    nombre_diferencia, format(round(diferencia, 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    "Tasa de variación: ", round(tva, 1), "%<br>",
    "Contribución: ", round(con, 1), " p.p."
  )]
  
  # Crear factor para eje
  datafr[, cod_pais_3dig := sprintf("%03d", as.integer(cod_pais))]
  datafr[, cod_pais_factor := factor(cod_pais_3dig, levels = cod_pais_3dig[order(-rank)])]
  
  # Figura
  fig <- plot_ly() %>%
    add_bars(
      data = datafr,
      x = ~con,
      y = ~cod_pais_factor,
      marker = list(color = colorplot),
      orientation = 'h',
      hovertext = ~hover_text,
      hoverinfo = 'text'
    ) %>%
    layout(
      xaxis = list(
        title = "Contribución (p.p.)",
        tickformat = ".1f",
        ticksuffix = " p.p.",
        showspikes = TRUE,
        spikemode = "across",
        spikesnap = "cursor",
        spikecolor = "grey",
        spikethickness = 1,
        spikedash = "dash"
      ),
      yaxis = list(
        title = list(text = "Código TARIC", standoff = 20),
        automargin = TRUE,
        tickfont = list(size = 10)
      ),
      margin = list(l = 80),
      showlegend = FALSE,
      hovermode = "closest",
      hoverdistance = 20,
      spikedistance = -1
    )
  
  fig <- fig %>% layout(custom_theme_plotly())
  
  return(fig)
}

grafica_contribuciones_pt <- function(datafr, aux, vals) {
  
  # Extraer valores de las listas
  nom_flujo <- aux$nombre_flujo
  nivel_taric <- vals$nivel
  var <- aux$var
  varud <- aux$varud
  colorplot <- aux$paleta["col3"] 
  
  # nombres dinámicos de columnas
  col_volumen <- paste0(var, "_periodo")
  col_volumen_previo <- paste0(var, "_periodo_prev")
  
  # etiquetas dinámicas
  nombre_volumen <- paste0("Volumen (", varud, "): ")
  nombre_volumen_previo <- paste0("Volumen previo (", varud, "): ")
  nombre_diferencia <- paste0("Diferencia absoluta (", varud, "): ")
  
  # Ordenar datafr por ranking
  datafr <- datafr[order(rank)]
  
  # generar hover_text
  datafr[, hover_text := paste0(
    "Flujo: ", nom_flujo, "<br>",
    "Región: ", aux$nombre_region, "<br>",
    "TARIC: ", Tar, "<br>",
    "País: ", pais, "<br>",
    
    # jerarquía TARIC (simplificada ya que nivel_taric es 1)
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
    "Ranking: ", rank, "<br>",
    nombre_volumen_previo, format(round(get(col_volumen_previo), 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    nombre_diferencia, format(round(diferencia, 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    "Tasa de variación: ", round(tva, 1), "%<br>",
    "Contribución: ", round(con, 1), " p.p."
  )]
  
  # Crear factor ordenado 
  datafr[, cod_pais_3dig := sprintf("%03d", as.integer(cod_pais))]
  datafr[, cod_taric_str := paste0("T:", cod_taric)]
  datafr[, cod_pais_str := paste0("P:", cod_pais_3dig)]
  datafr[, cod_concat := paste0(cod_taric_str, " - ", cod_pais_str)]
  datafr[, cod_concat_factor := factor(cod_concat, levels = cod_concat[order(-rank)])]
  
  # Figura
  fig <- plot_ly() %>%
    add_bars(
      data = datafr,
      x = ~con,
      y = ~cod_concat_factor,
      marker = list(color = colorplot),
      orientation = 'h',
      hovertext = ~hover_text,
      hoverinfo = 'text'
    ) %>%
    layout(
      xaxis = list(
        title = "Contribución (p.p.)",
        tickformat = ".1f",
        ticksuffix = " p.p.",
        showspikes = TRUE,
        spikemode = "across",
        spikesnap = "cursor",
        spikecolor = "grey",
        spikethickness = 1,
        spikedash = "dash"
      ),
      yaxis = list(
        title = list(text = "Código TARIC", standoff = 20),
        automargin = TRUE,
        tickfont = list(size = 10)
      ),
      margin = list(l = 80),
      showlegend = FALSE,
      hovermode = "closest",
      hoverdistance = 20,
      spikedistance = -1
    )
  
  fig <- fig %>% layout(custom_theme_plotly())
  
  return(fig)
}