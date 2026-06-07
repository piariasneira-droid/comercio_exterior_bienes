# funciones.R
# Funciones para el procesamiento de datos de comercio exterior
# Madrid vs España por sectores económicos

## Tratamiento exceles----
# Lee y combina múltiples hojas de un fichero Excel en un data.table
.leer_excel_sheets <- function(path, sheets) {
  data.table::rbindlist(
    lapply(sheets, function(s) {
      data.table::as.data.table(openxlsx::read.xlsx(path, sheet = s))
    }),
    use.names = TRUE,
    fill = TRUE
  )
}

# write_formatted_xlsx
# Exporta un data.table a Excel aplicando transformaciones numéricas y
# formatos específicos por tipo de columna.
#
# Parámetros:
#   data        : data.table con los datos a exportar
#   parametros  : lista de parámetros del proyecto (paramets)
#   file_name   : nombre del fichero de salida (.xlsx); la ruta se construye desde parametros$path_outx
#   int_cols    : columnas enteras — sin decimales, sin transformación        (ej: orden, niv, cod)
#   idx_cols    : columnas de índice base 100 — decimales, sin transformación (ej: exp_mad_idx_2019)
#   pct_cols    : columnas de porcentaje — se multiplican x100, formato decimal
#   num_cols    : columnas numéricas absolutas — se dividen por varfactor, formato decimal
#                 (si es NULL se infiere automáticamente como el resto de columnas numéricas)
.write_formatted_xlsx <- function(data, parametros, file_name,
                                  int_cols     = NULL,
                                  idx_cols     = NULL,
                                  pct_cols     = NULL,
                                  num_cols     = NULL,
                                  extra_sheets = NULL) {
  
  # 1. Preparación y ruta de salida
  df_temp   <- data.table::as.data.table(data.table::copy(data))
  all_names <- names(df_temp)
  file_path <- file.path(parametros$path_outx, file_name)
  
  # 2. Identificación automática de num_cols
  # Son numéricas que no pertenecen a ninguno de los otros tres grupos
  if (is.null(num_cols)) {
    potenciales <- setdiff(all_names, c(int_cols, idx_cols, pct_cols))
    num_cols    <- potenciales[sapply(df_temp[, ..potenciales], is.numeric)]
  }
  
  # 3. Transformaciones de datos
  # Porcentajes: multiplicar x100
  if (length(pct_cols) > 0) {
    df_temp[, (pct_cols) := lapply(.SD, function(x) x * 100), .SDcols = pct_cols]
  }
  
  # Numéricas absolutas: dividir por varfactor
  if (length(num_cols) > 0) {
    df_temp[, (num_cols) := lapply(.SD, function(x) x / parametros$varfactor), .SDcols = num_cols]
  }
  
  # idx_cols y int_cols: sin transformación
  
  # 4. Configuración de Excel (openxlsx)
  wb         <- openxlsx::createWorkbook()
  sheet_name <- "Datos"
  openxlsx::addWorksheet(wb, sheet_name)
  
  # Formatos numéricos
  fmt_num <- paste0("#,##0.", paste(rep("0", parametros$dec_num), collapse = ""))
  fmt_pct <- paste0("#,##0.", paste(rep("0", parametros$dec_per), collapse = ""))
  
  style_num <- openxlsx::createStyle(numFmt = fmt_num)
  style_pct <- openxlsx::createStyle(numFmt = fmt_pct)
  style_idx <- openxlsx::createStyle(numFmt = fmt_num)   # mismo decimal que num, sin divisor
  style_int <- openxlsx::createStyle(numFmt = "0")
  
  # 5. Escritura y diseño
  openxlsx::writeData(wb, sheet_name, df_temp)
  openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
  openxlsx::addFilter(wb, sheet_name, row = 1, cols = 1:ncol(df_temp))
  openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df_temp), widths = "auto")
  
  rows_range <- 2:(nrow(df_temp) + 1)
  
  # 6. Aplicación de estilos
  if (length(pct_cols) > 0) {
    openxlsx::addStyle(wb, sheet_name, style = style_pct,
                       cols = which(all_names %in% pct_cols),
                       rows = rows_range, gridExpand = TRUE)
  }
  
  if (length(num_cols) > 0) {
    openxlsx::addStyle(wb, sheet_name, style = style_num,
                       cols = which(all_names %in% num_cols),
                       rows = rows_range, gridExpand = TRUE)
  }
  
  if (length(idx_cols) > 0) {
    openxlsx::addStyle(wb, sheet_name, style = style_idx,
                       cols = which(all_names %in% idx_cols),
                       rows = rows_range, gridExpand = TRUE)
  }
  
  if (length(int_cols) > 0) {
    openxlsx::addStyle(wb, sheet_name, style = style_int,
                       cols = which(all_names %in% int_cols),
                       rows = rows_range, gridExpand = TRUE)
  }
  
  # 7. Hojas adicionales (acu, anopas, etc.)
  if (!is.null(extra_sheets)) {
    for (sheet_nm in names(extra_sheets)) {
      df_extra   <- data.table::as.data.table(data.table::copy(extra_sheets[[sheet_nm]]))
      ex_names   <- names(df_extra)
      
      # Inferir num_cols para la hoja extra
      if (is.null(num_cols)) {
        potenciales_ex <- setdiff(ex_names, c(int_cols, idx_cols, pct_cols))
        num_cols_ex    <- potenciales_ex[sapply(df_extra[, ..potenciales_ex], is.numeric)]
      } else {
        num_cols_ex <- num_cols
      }
      
      # Transformaciones
      pct_ex <- pct_cols[pct_cols %in% ex_names]
      num_ex <- num_cols_ex[num_cols_ex %in% ex_names]
      idx_ex <- if (!is.null(idx_cols)) idx_cols[idx_cols %in% ex_names] else character(0)
      int_ex <- if (!is.null(int_cols)) int_cols[int_cols %in% ex_names] else character(0)
      
      if (length(pct_ex) > 0)
        df_extra[, (pct_ex) := lapply(.SD, function(x) x * 100), .SDcols = pct_ex]
      if (length(num_ex) > 0)
        df_extra[, (num_ex) := lapply(.SD, function(x) x / parametros$varfactor), .SDcols = num_ex]
      
      openxlsx::addWorksheet(wb, sheet_nm)
      openxlsx::writeData(wb, sheet_nm, df_extra)
      openxlsx::freezePane(wb, sheet_nm, firstRow = TRUE)
      openxlsx::addFilter(wb, sheet_nm, row = 1, cols = 1:ncol(df_extra))
      openxlsx::setColWidths(wb, sheet_nm, cols = 1:ncol(df_extra), widths = "auto")
      
      rows_ex <- 2:(nrow(df_extra) + 1)
      
      if (length(pct_ex) > 0)
        openxlsx::addStyle(wb, sheet_nm, style = style_pct,
                           cols = which(ex_names %in% pct_ex),
                           rows = rows_ex, gridExpand = TRUE)
      if (length(num_ex) > 0)
        openxlsx::addStyle(wb, sheet_nm, style = style_num,
                           cols = which(ex_names %in% num_ex),
                           rows = rows_ex, gridExpand = TRUE)
      if (length(idx_ex) > 0)
        openxlsx::addStyle(wb, sheet_nm, style = style_idx,
                           cols = which(ex_names %in% idx_ex),
                           rows = rows_ex, gridExpand = TRUE)
      if (length(int_ex) > 0)
        openxlsx::addStyle(wb, sheet_nm, style = style_int,
                           cols = which(ex_names %in% int_ex),
                           rows = rows_ex, gridExpand = TRUE)
    }
  }
  
  # 8. Guardado
  if (!dir.exists(parametros$path_outx)) dir.create(parametros$path_outx, recursive = TRUE)
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
}

.cargar_taric <- function(path) {
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
  resultado <- .anade_padres_dt(resultado)
  
  return(resultado)
}

.anade_padres_dt <- function(dt) {
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

.top_bottom_rep <- function(df, n = 1000L, flujo = "exp") {
  # Ordenar por 'dif' de mayor a menor y extraer top/bottom
  df_ord <- df[order(-dif)]
  res <- rbind(head(df_ord, n), tail(df_ord, n))
  
  # Definir el término del flujo (exportaciones o importaciones)
  flujo_limpio <- tolower(trimws(flujo))
  txt_flujo <- ifelse(grepl("^exp", flujo_limpio), "exportaciones", "importaciones")
  
  # Determinar columna de texto y tipo de clasificación
  # Evaluamos ambas variantes: 'codconnombre' (tu df real) y 'cod_con_nombre'
  if ("codconnombre" %in% names(res) || "cod_con_nombre" %in% names(res)) {
    # Clasificación sectorial (incluye nivel 0 como total)
    tipo_concepto <- fcase(
      res$niv_sec == 0, "total",
      res$niv_sec == 1, "sector",
      default = "subsector"
    )
    # Asignamos la columna que realmente exista en el df
    concepto <- if ("codconnombre" %in% names(res)) res$codconnombre else res$cod_con_nombre
    
  } else if ("Tar" %in% names(res)) {
    # Clasificación por nivel TARIC
    tipo_concepto <- fcase(
      res$nivel_taric == 0, "total",
      res$nivel_taric == 1, "capítulo",
      res$nivel_taric == 2, "partida",
      res$nivel_taric == 3, "subpartida",
      res$nivel_taric == 4, "nomenclatura combinada",
      res$nivel_taric == 5, "arancel",
      default = "concepto"
    )
    concepto <- res$Tar
  } else {
    stop("El dataframe no contiene las columnas necesarias ('codconnombre', 'cod_con_nombre' o 'Tar').")
  }
  
  # Determinar el sentido del cambio de forma natural
  verbo_cambio <- ifelse(res$dif >= 0, "un incremento", "una disminución")
  
  # Construir la columna 'texto' utilizando tus funciones de formato
  res[, texto := sprintf(
    "El volumen de las %s del %s %s al mercado %s ha pasado de %s a %s millones de euros, lo que supone %s de %s millones de euros con una tva de %s%% y aporta una contribución de %s puntos porcentuales.",
    txt_flujo,
    tipo_concepto, 
    concepto, 
    res$paisconcod, 
    .fmt_num(res$euros_prev), 
    .fmt_num(res$euros), 
    verbo_cambio,
    .fmt_num(abs(res$dif)), 
    .fmt_pct(res$tva * 100), 
    .fmt_pp(res$rep * 100)
  )]
  
  return(res)
}

## Totales anuales ----
### Serie histórica ----
# procesar_totales_anuales
# Genera series anuales de exportaciones e importaciones Madrid vs España
# con tasas de variación, cuota y saldo
#
# Parámetros:
#   ds_mad     : Arrow dataset Madrid
#   ds_esp     : Arrow dataset España
#   parametros : lista con los siguientes campos:
#     $ano_ini    : año de inicio de la serie
#     $meses      : vector de meses a incluir
#     $cod_sector : código de sector económico (character)
#     $cod_pais   : código de país (integer)
.procesar_totales_anuales <- function(ds_mad, ds_esp, parametros) {
  
  # --- Filtrado en origen (Arrow, sin collect) ---
  query_mad <- ds_mad |>
    dplyr::filter(
      año >= parametros$ano_ini,
      mes %in% parametros$mes,
      cod_sector_economico == parametros$cod_sector,
      pais == parametros$cod_pais
    )
  
  query_esp <- ds_esp |>
    dplyr::filter(
      año >= parametros$ano_ini,
      mes %in% parametros$mes,
      cod_sector_economico == parametros$cod_sector,
      pais == parametros$cod_pais
    )
  
  # --- Materialización ---
  df_mad <- data.table::as.data.table(dplyr::collect(query_mad))
  df_esp <- data.table::as.data.table(dplyr::collect(query_esp))
  
  # --- Preparación: agrega por año y flujo, pivota a columnas exp/imp ---
  prep <- function(dt) {
    dt <- data.table::copy(dt)
    dt <- dt[, .(euros = sum(euros)), by = .(año, flujo)]
    data.table::dcast(dt, año ~ flujo, value.var = "euros")[,
                                                            .(año, exp = `1`, imp = `0`)
    ]
  }
  
  mad <- prep(df_mad)
  esp <- prep(df_esp)
  
  data.table::setnames(mad, c("exp", "imp"), c("exp_mad", "imp_mad"))
  data.table::setnames(esp, c("exp", "imp"), c("exp_esp", "imp_esp"))
  
  final <- merge(mad, esp, by = "año")
  
  # --- Tasas de variación (requieren año previo) ---
  final[, exp_mad_tva := exp_mad / data.table::shift(exp_mad) - 1]
  final[, imp_mad_tva := imp_mad / data.table::shift(imp_mad) - 1]
  final[, exp_esp_tva := exp_esp / data.table::shift(exp_esp) - 1]
  final[, imp_esp_tva := imp_esp / data.table::shift(imp_esp) - 1]
  
  # --- Filtrar tras calcular tasas ---
  final <- final[año >= (parametros$ano_ini + 1)]
  
  # --- Indicadores ---
  final[, exp_mad_pct          := exp_mad / exp_esp]
  final[, imp_mad_pct          := imp_mad / imp_esp]
  final[, saldo_mad            := exp_mad - imp_mad]
  final[, tasa_cobertura_mad   := exp_mad / imp_mad]
  final[, saldo_esp            := exp_mad - imp_mad]
  final[, tasa_cobertura_esp   := exp_esp / imp_esp]
  
  final[, .(
    año,
    exp_mad, exp_mad_tva, exp_mad_pct,
    imp_mad, imp_mad_tva, imp_mad_pct,
    saldo_mad, tasa_cobertura_mad,
    exp_esp, exp_esp_tva,
    imp_esp, imp_esp_tva,
    saldo_esp, tasa_cobertura_esp
  )]
}

### Totales año ----
# extraer_totales_de_tabla
# Extrae los totales Madrid y España de la fila de total (orden 65, niv 9)
# directamente desde la tabla ya calculada por tabla_sectores_datacomex
# Evita un pull adicional a los datos
#
# Parámetros:
#   tabla_sectores : salida de tabla_sectores_datacomex
.extraer_totales_de_tabla <- function(tabla_sectores) {
  
  mad <- tabla_sectores[region == "Madrid" & orden == 65L]
  esp <- tabla_sectores[region == "España"  & orden == 65L]
  
  # Guard: fail loudly instead of returning NA silently
  if (nrow(mad) == 0L) stop("Fila subtotal (orden 65) no encontrada para Madrid.")
  if (nrow(esp) == 0L) stop("Fila subtotal (orden 65) no encontrada para España.")
  
  list(
    exp_mad      = mad$exp,
    exp_prev_mad = mad$exp_prev,
    imp_mad      = mad$imp,
    imp_prev_mad = mad$imp_prev,
    exp_esp      = esp$exp,
    exp_prev_esp = esp$exp_prev,
    imp_esp      = esp$imp,
    imp_prev_esp = esp$imp_prev
  )
}

## Sectores ----
### Réplica datacomex ----
# tabla_sectores_datacomex
# Agrega exportaciones e importaciones por sector económico para Madrid y España
# en el año indicado y el año anterior
#
# Parámetros:
#   ds_mad     : Arrow dataset Madrid
#   ds_esp     : Arrow dataset España
#   df_sec     : data.table de metadatos de sectores
#   parametros : lista con los siguientes campos:
#     $anho      : año de referencia (integer)
#     $meses     : vector de meses a incluir
#     $cod_pais  : código de país (default 0L = total)
.tabla_sectores_datacomex <- function(ds_mad, ds_esp, df_sec, parametros) {
  # --- Sectores a incluir ---
  lista_nivel3 <- c(
    "431", "432", "433", "434", "435", "436", "437", "438",
    "441", "4421", "4422", "4423", "443", "444",
    "511", "512", "513", "514", "515", "516",
    "521", "522", "523",
    "531", "532", "533", "534",
    "541", "542", "543", "544",
    "814"
  )
  
  df_sec <- df_sec[nivel_sec %in% c(1L, 2L) | cod_sec %in% c("0", lista_nivel3)]
  
  # --- Extracción desde Arrow ---
  extraer_micro <- function(ds) {
    ds |>
      dplyr::filter(
        año == parametros$anho | año == parametros$anho - 1L,
        mes %in% parametros$mes,
        pais == parametros$cod_pais
      ) |>
      dplyr::group_by(flujo, año, cod_sector_economico) |>
      dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
      dplyr::collect() |>
      data.table::as.data.table()
  }
  
  micro_mad <- extraer_micro(ds_mad)
  micro_esp <- extraer_micro(ds_esp)
  
  # --- Join con metadatos de sectores ---
  micro_mad <- df_sec[micro_mad, on = .(cod_sec = cod_sector_economico), nomatch = 0]
  micro_esp <- df_sec[micro_esp, on = .(cod_sec = cod_sector_economico), nomatch = 0]
  
  # --- Agregación por sector ---
  agregar <- function(df) {
    df[, .(
      exp      = sum(euros[flujo == 1 & año == parametros$anho],       na.rm = TRUE),
      exp_prev = sum(euros[flujo == 1 & año == parametros$anho - 1L],  na.rm = TRUE),
      imp      = sum(euros[flujo == 0 & año == parametros$anho],       na.rm = TRUE),
      imp_prev = sum(euros[flujo == 0 & año == parametros$anho - 1L],  na.rm = TRUE)
    ), by = .(orden, niv, nombre)]
  }
  
  df_mad <- agregar(micro_mad)
  df_esp <- agregar(micro_esp)
  
  # --- Fila 18: suma de órdenes 19-23 ---
  añadir_fila18 <- function(df) {
    fila_18_val <- df[orden %in% c(19, 20, 21, 22, 23), .(
      exp      = sum(exp,      na.rm = TRUE),
      exp_prev = sum(exp_prev, na.rm = TRUE),
      imp      = sum(imp,      na.rm = TRUE),
      imp_prev = sum(imp_prev, na.rm = TRUE)
    )]
    
    fila_18 <- data.table::data.table(
      orden    = 18L,
      niv      = 1L,
      nombre   = "Semifacturas no químicas",
      exp      = fila_18_val$exp,
      exp_prev = fila_18_val$exp_prev,
      imp      = fila_18_val$imp,
      imp_prev = fila_18_val$imp_prev
    )
    
    data.table::setorder(
      data.table::rbindlist(list(df, fila_18), use.names = TRUE, fill = TRUE),
      orden
    )
  }
  
  # --- Fila 65: suma de filas con niv == 1 ---
  añadir_fila65 <- function(df) {
    fila_65_val <- df[niv == 1L, .(
      exp      = sum(exp,      na.rm = TRUE),
      exp_prev = sum(exp_prev, na.rm = TRUE),
      imp      = sum(imp,      na.rm = TRUE),
      imp_prev = sum(imp_prev, na.rm = TRUE)
    )]
    
    fila_65 <- data.table::data.table(
      orden    = 65L,
      niv      = 9L,
      nombre   = "Subtotal",
      exp      = fila_65_val$exp,
      exp_prev = fila_65_val$exp_prev,
      imp      = fila_65_val$imp,
      imp_prev = fila_65_val$imp_prev
    )
    
    data.table::setorder(
      data.table::rbindlist(list(df, fila_65), use.names = TRUE, fill = TRUE),
      orden
    )
  }
  
  # Fila 18 primero — fila 65 la incluye en la suma
  df_mad <- añadir_fila18(df_mad)
  df_esp <- añadir_fila18(df_esp)
  
  df_mad <- añadir_fila65(df_mad)
  df_esp <- añadir_fila65(df_esp)
  
  # --- Combinar regiones ---
  df_mad[, region := "Madrid"]
  df_esp[, region := "España"]
  
  df_out <- data.table::rbindlist(list(df_mad, df_esp), use.names = TRUE)
  df_out <- df_out[!is.na(niv) & niv != ""]
  
  # --- Indicadores ---
  df_out[, exp_dif    := exp - exp_prev]
  df_out[, imp_dif    := imp - imp_prev]
  df_out[, tva_exp    := ifelse(exp_prev != 0, exp_dif / exp_prev, 0)]
  df_out[, tva_imp    := ifelse(imp_prev != 0, imp_dif / imp_prev, 0)]
  df_out[, saldo      := exp - imp]
  df_out[, saldo_prev := exp_prev - imp_prev]
  
  df_out[]
}




# procesar_salida_sectores
# Combina la tabla de sectores de Madrid y España, calcula cuotas,
# tasas de variación y contribuciones relativas al total Madrid
#
# Parámetros:
#   tabla        : salida de tabla_sectores_datacomex
#   listatotales : salida de extraer_totales_de_tabla
.procesar_salida_sectores <- function(tabla, listatotales) {
  
  stopifnot(
    data.table::is.data.table(tabla),
    is.list(listatotales)
  )
  
  # 1. Separar por regiones
  mad <- tabla[region == "Madrid"]
  esp <- tabla[region == "España"]
  
  # 2. Merge de datos base
  out <- merge(
    mad,
    esp[, .(orden, 
            exp_esp      = exp, 
            exp_prev_esp = exp_prev, 
            imp_esp      = imp, 
            imp_prev_esp = imp_prev,
            exp_dif_esp  = exp_dif,
            imp_dif_esp  = imp_dif)],
    by    = "orden",
    all.x = TRUE
  )
  
  # 3. Cálculo de indicadores y saldos por región
  out[, `:=`(
    # --- MADRID ---
    exp_mad_pct      = exp / listatotales$exp_mad,
    exp_mad_tva      = exp_dif / exp_prev,
    exp_mad_contrib  = exp_dif / listatotales$exp_prev_mad,
    exp_mad_vs_esp   = exp / exp_esp,
    
    imp_mad_pct      = imp / listatotales$imp_mad,
    imp_mad_tva      = imp_dif / imp_prev,
    imp_mad_contrib  = imp_dif / listatotales$imp_prev_mad,
    imp_mad_vs_esp   = imp / imp_esp,
    
    tasa_cob_mad     = exp / imp,
    saldo_mad        = exp - imp,         # Renombramos saldo genérico
    saldo_mad_prev   = exp_prev - imp_prev,
    
    # --- ESPAÑA ---
    exp_esp_pct      = exp_esp / listatotales$exp_esp,
    exp_esp_tva      = exp_dif_esp / exp_prev_esp,
    exp_esp_contrib  = exp_dif_esp / listatotales$exp_prev_esp,
    
    imp_esp_pct      = imp_esp / listatotales$imp_esp,
    imp_esp_tva      = imp_dif_esp / imp_prev_esp,
    imp_esp_contrib  = imp_dif_esp / listatotales$imp_prev_esp,
    
    tasa_cob_esp     = exp_esp / imp_esp,
    saldo_esp        = exp_esp - imp_esp,
    saldo_esp_prev   = exp_prev_esp - imp_prev_esp
  )]
  
  # 4. Selección y Orden final por bloques
  out_final <- out[, .(
    orden, niv, nombre,
    
    # BLOQUE MADRID
    exp_mad = exp, exp_mad_pct, exp_mad_tva, exp_mad_contrib, exp_mad_vs_esp,
    imp_mad = imp, imp_mad_pct, imp_mad_tva, imp_mad_contrib, imp_mad_vs_esp,
    saldo_mad, saldo_mad_prev, tasa_cob_mad,
    
    # BLOQUE ESPAÑA
    exp_esp, exp_esp_pct, exp_esp_tva, exp_esp_contrib,
    imp_esp, imp_esp_pct, imp_esp_tva, imp_esp_contrib,
    saldo_esp, saldo_esp_prev, tasa_cob_esp
  )]
  
  return(out_final)
}

### Completa ----
.tabla_sectores_f <- function(ds_mad, ds_esp, df_sec, parametros) {
  
  # --- 1. Extracción desde Arrow ---
  extraer_micro <- function(ds) {
    ds |>
      dplyr::filter(
        año == parametros$anho | año == parametros$anho - 1L,
        mes %in% parametros$mes,
        pais == parametros$cod_pais
      ) |>
      dplyr::group_by(flujo, año, cod_sector_economico) |>
      dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
      dplyr::collect() |>
      data.table::as.data.table()
  }
  
  micro_mad <- extraer_micro(ds_mad)
  micro_esp <- extraer_micro(ds_esp)
  
  # --- 2. Join con metadatos ---
  micro_mad <- df_sec[micro_mad,
                      .(cod_sec, nombre, niv, orden, codconnombre,
                        flujo, año, euros),
                      on = .(cod_sec = cod_sector_economico),
                      nomatch = 0]
  
  micro_esp <- df_sec[micro_esp,
                      .(cod_sec, nombre, niv, orden, codconnombre,
                        flujo, año, euros),
                      on = .(cod_sec = cod_sector_economico),
                      nomatch = 0]
  
  # --- 3. Agregación ---
  agregar <- function(df) {
    df[, .(
      exp      = sum(euros[flujo == 1 & año == parametros$anho],      na.rm = TRUE),
      exp_prev = sum(euros[flujo == 1 & año == parametros$anho - 1L], na.rm = TRUE),
      imp      = sum(euros[flujo == 0 & año == parametros$anho],      na.rm = TRUE),
      imp_prev = sum(euros[flujo == 0 & año == parametros$anho - 1L], na.rm = TRUE)
    ), by = .(orden, niv, nombre, codconnombre)]
  }
  
  df_mad <- agregar(micro_mad)
  df_esp <- agregar(micro_esp)
  
  # --- 4. Combinar regiones ---
  df_mad[, region := "Madrid"]
  df_esp[, region := "España"]
  
  df_out <- rbindlist(list(df_mad, df_esp), use.names = TRUE)
  df_out <- df_out[!is.na(niv)]
  
  # --- 5. Indicadores básicos ---
  df_out[, exp_dif := exp - exp_prev]
  df_out[, imp_dif := imp - imp_prev]
  
  df_out[, tva_exp := ifelse(exp_prev != 0, exp_dif / exp_prev, 0)]
  df_out[, tva_imp := ifelse(imp_prev != 0, imp_dif / imp_prev, 0)]
  
  df_out[, saldo := exp - imp]
  df_out[, saldo_prev := exp_prev - imp_prev]
  
  # --- 6. Orden final ---
  setorder(df_out, region, orden)
  
  return(df_out[])
}


## Países ----
### Replica datacomex ----
# tabla_paises_datacomex
# Agrega exportaciones e importaciones por país para Madrid y España
# en el año indicado y el año anterior
#
# Parámetros:
#   ds_mad     : Arrow dataset Madrid
#   ds_esp     : Arrow dataset España
#   df_paises  : data.table de metadatos de países y regiones
#   parametros : lista con los siguientes campos:
#     $anho      : año de referencia (integer)
#     $meses     : vector de meses a incluir
#     $cod_sector: código de sector económico (character)
#   totales    : salida de extraer_totales_de_tabla
.tabla_paises_datacomex <- function(ds_mad, ds_esp, df_paises, parametros, totales) {
  
  # --- Extracción desde Arrow ---
  extraer_micro <- function(ds) {
    ds |>
      dplyr::filter(
        año == parametros$anho | año == parametros$anho - 1L,
        mes %in% parametros$mes,
        cod_sector_economico == parametros$cod_sector
      ) |>
      dplyr::group_by(flujo, año, pais) |>
      dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
      dplyr::collect() |>
      data.table::as.data.table()
  }
  
  micro_mad <- extraer_micro(ds_mad)
  micro_esp <- extraer_micro(ds_esp)
  
  # --- Join con metadatos de países ---
  micro_mad <- df_paises[micro_mad, on = .(cod = pais), nomatch = 0]
  micro_esp <- df_paises[micro_esp, on = .(cod = pais), nomatch = 0]
  
  # --- Agregación por país ---
  agregar <- function(df) {
    df[, .(
      exp      = sum(euros[flujo == 1 & año == parametros$anho],      na.rm = TRUE),
      exp_prev = sum(euros[flujo == 1 & año == parametros$anho - 1L], na.rm = TRUE),
      imp      = sum(euros[flujo == 0 & año == parametros$anho],      na.rm = TRUE),
      imp_prev = sum(euros[flujo == 0 & año == parametros$anho - 1L], na.rm = TRUE)
    ), by = .(cod, pais, reg)]
  }
  
  df_mad <- agregar(micro_mad)
  df_esp <- agregar(micro_esp)
  
  # --- Añadir filas de regiones geográficas ---
  # Tabla de códigos de región
  df_codigos_reg <- data.table::data.table(
    pais = c(
      "ZONA EURO", "RESTO DE EUROPA", "RESTO UE",
      "ASIA (excl. Oriente Medio)", "ÁFRICA",
      "AMÉRICA DEL NORTE", "AMÉRICA LATINA", "RESTO DE AMÉRICA",
      "ORIENTE MEDIO", "OCEANÍA", "OTROS"
    ),
    cod = c(1001, 1003, 1002, 1007, 1009, 1004, 1005, 1006, 1008, 1010, 1011)
  )
  
  añadir_regiones <- function(df) {
    df_regiones <- df[!is.na(reg), .(
      exp      = sum(exp,      na.rm = TRUE),
      exp_prev = sum(exp_prev, na.rm = TRUE),
      imp      = sum(imp,      na.rm = TRUE),
      imp_prev = sum(imp_prev, na.rm = TRUE)
    ), by = reg]
    
    data.table::setnames(df_regiones, "reg", "pais")
    df_regiones <- df_codigos_reg[df_regiones, on = "pais"]
    
    data.table::rbindlist(list(df, df_regiones), use.names = TRUE, fill = TRUE)
  }
  
  df_mad <- añadir_regiones(df_mad)
  df_esp <- añadir_regiones(df_esp)
  
  # --- Join con metadatos de orden/niv y filtrar ---
  ordenar_paises <- function(df) {
    df[, c("pais", "reg") := NULL]
    
    df_paises[, .(cod, pais, orden, niv)][df, on = "cod", nomatch = 0][
      !is.na(orden)
    ][
      order(orden)
    ]
  }
  
  df_mad <- ordenar_paises(df_mad)
  df_esp <- ordenar_paises(df_esp)
  
  # --- Añadir filas agregadas: UE27, América, Asia, Europa ---
  
  añadir_fila_agregada <- function(df, ordenes_origen, cod, pais, orden, niv) {
    df_sub <- df[orden %in% ordenes_origen]
    fila <- data.table::data.table(
      cod      = cod,
      pais     = pais,
      orden    = orden,
      niv      = niv,
      exp      = sum(df_sub$exp,      na.rm = TRUE),
      exp_prev = sum(df_sub$exp_prev, na.rm = TRUE),
      imp      = sum(df_sub$imp,      na.rm = TRUE),
      imp_prev = sum(df_sub$imp_prev, na.rm = TRUE)
    )
    fila
  }
  
  procesar_region <- function(df) {
    fila_ue27    <- añadir_fila_agregada(df, c(3, 23),      1013L, "UE 27",   2L,  2L)
    fila_america <- añadir_fila_agregada(df, c(38, 41, 47), 1014L, "AMERICA", 37L, 1L)
    fila_asia    <- añadir_fila_agregada(df, c(49, 59),     1015L, "ASIA",    48L, 1L)
    
    df <- data.table::rbindlist(
      list(df, fila_ue27, fila_america, fila_asia),
      use.names = TRUE, fill = TRUE
    )[order(orden)]
    
    fila_europa <- añadir_fila_agregada(df, c(2, 31), 1012L, "EUROPA", 1L, 1L)
    
    data.table::rbindlist(
      list(df, fila_europa),
      use.names = TRUE, fill = TRUE
    )[order(orden)]
  }
  
  df_mad <- procesar_region(df_mad)
  df_esp <- procesar_region(df_esp)
  
  # --- Fila 71: subtotal (suma de filas con niv == 1) ---
  añadir_fila71 <- function(df) {
    sub_val <- df[niv == 1L, .(
      exp      = sum(exp,      na.rm = TRUE),
      exp_prev = sum(exp_prev, na.rm = TRUE),
      imp      = sum(imp,      na.rm = TRUE),
      imp_prev = sum(imp_prev, na.rm = TRUE)
    )]
    
    fila_sub <- data.table::data.table(
      cod      = 1100L,
      pais     = "Subtotal",
      orden    = 71L,
      niv      = 9L,
      exp      = sub_val$exp,
      exp_prev = sub_val$exp_prev,
      imp      = sub_val$imp,
      imp_prev = sub_val$imp_prev
    )
    
    data.table::rbindlist(
      list(df, fila_sub),
      use.names = TRUE, fill = TRUE
    )[order(orden)]
  }
  
  df_mad <- añadir_fila71(df_mad)
  df_esp <- añadir_fila71(df_esp)
  
  # --- Fila 72: total territorial (valores desde totalesanho) ---
  fila72_mad <- data.table::data.table(
    cod      = 0L,
    pais     = "Total territorial",
    orden    = 72L,
    niv      = 9L,
    exp      = totales$exp_mad,
    exp_prev = totales$exp_prev_mad,
    imp      = totales$imp_mad,
    imp_prev = totales$imp_prev_mad
  )
  
  fila72_esp <- data.table::data.table(
    cod      = 0L,
    pais     = "Total territorial",
    orden    = 72L,
    niv      = 9L,
    exp      = totales$exp_esp,
    exp_prev = totales$exp_prev_esp,
    imp      = totales$imp_esp,
    imp_prev = totales$imp_prev_esp
  )
  
  df_mad <- data.table::rbindlist(list(df_mad, fila72_mad), use.names = TRUE, fill = TRUE)[order(orden)]
  df_esp <- data.table::rbindlist(list(df_esp, fila72_esp), use.names = TRUE, fill = TRUE)[order(orden)]
  
  # --- Combinar regiones ---
  df_mad[, region := "Madrid"]
  df_esp[, region := "España"]
  
  df_out <- data.table::rbindlist(list(df_mad, df_esp), use.names = TRUE, fill = TRUE)
  df_out <- df_out[!is.na(niv) & niv != ""]
  
  # --- Indicadores ---
  df_out[, exp_dif    := exp - exp_prev]
  df_out[, imp_dif    := imp - imp_prev]
  df_out[, tva_exp    := ifelse(exp_prev != 0, exp_dif / exp_prev, 0)]
  df_out[, tva_imp    := ifelse(imp_prev != 0, imp_dif / imp_prev, 0)]
  df_out[, saldo      := exp - imp]
  df_out[, saldo_prev := exp_prev - imp_prev]
  
  df_out[]
}

# procesar_salida_paises
# Combina la tabla de países de Madrid y España, calcula cuotas,
# tasas de variación y contribuciones relativas al total Madrid
#
# Parámetros:
#   tabla        : salida de tabla_paises_datacomex
#   listatotales : salida de extraer_totales_de_tabla
.procesar_salida_paises <- function(tabla, listatotales) {
  
  stopifnot(
    data.table::is.data.table(tabla),
    is.list(listatotales),
    all(c("exp_mad", "imp_mad", "exp_prev_mad", "imp_prev_mad",
          "exp_esp", "imp_esp", "exp_prev_esp", "imp_prev_esp") %in% names(listatotales))
  )
  
  # 1. Separar regiones
  mad <- tabla[region == "Madrid"]
  esp <- tabla[region == "España"]
  
  # 2. Merge de datos base por orden/país
  out <- merge(
    mad,
    esp[, .(orden, 
            exp_esp      = exp, 
            exp_prev_esp = exp_prev, 
            imp_esp      = imp, 
            imp_prev_esp = imp_prev,
            exp_dif_esp  = exp_dif,
            imp_dif_esp  = imp_dif)],
    by    = "orden",
    all.x = TRUE
  )
  
  # 3. Cálculo de indicadores y saldos por bloque regional
  out[, `:=`(
    # --- BLOQUE MADRID ---
    exp_mad_pct      = exp / listatotales$exp_mad,
    exp_mad_tva      = exp_dif / exp_prev,
    exp_mad_contrib  = exp_dif / listatotales$exp_prev_mad,
    exp_mad_vs_esp   = exp / exp_esp,
    
    imp_mad_pct      = imp / listatotales$imp_mad,
    imp_mad_tva      = imp_dif / imp_prev,
    imp_mad_contrib  = imp_dif / listatotales$imp_prev_mad,
    imp_mad_vs_esp   = imp / imp_esp,
    
    saldo_mad        = exp - imp,
    saldo_mad_prev   = exp_prev - imp_prev,
    tasa_cob_mad     = exp / imp,
    
    # --- BLOQUE ESPAÑA ---
    exp_esp_pct      = exp_esp / listatotales$exp_esp,
    exp_esp_tva      = exp_dif_esp / exp_prev_esp,
    exp_esp_contrib  = exp_dif_esp / listatotales$exp_prev_esp,
    
    imp_esp_pct      = imp_esp / listatotales$imp_esp,
    imp_esp_tva      = imp_dif_esp / imp_prev_esp,
    imp_esp_contrib  = imp_dif_esp / listatotales$imp_prev_esp,
    
    saldo_esp        = exp_esp - imp_esp,
    saldo_esp_prev   = exp_prev_esp - imp_prev_esp,
    tasa_cob_esp     = exp_esp / imp_esp
  )]
  
  # 4. Selección final: ID -> Bloque MAD -> Bloque ESP
  out_final <- out[, .(
    orden, niv, pais,
    
    # MADRID
    exp_mad = exp, exp_mad_prev = exp_prev,
    exp_mad_pct, exp_mad_tva, exp_mad_contrib, exp_mad_vs_esp,
    imp_mad = imp, imp_mad_prev = imp_prev,
    imp_mad_pct, imp_mad_tva, imp_mad_contrib, imp_mad_vs_esp,
    saldo_mad, saldo_mad_prev, tasa_cob_mad,
    
    # ESPAÑA
    exp_esp, exp_esp_prev = exp_prev_esp,
    exp_esp_pct, exp_esp_tva, exp_esp_contrib,
    imp_esp, imp_esp_prev = imp_prev_esp,
    imp_esp_pct, imp_esp_tva, imp_esp_contrib,
    saldo_esp, saldo_esp_prev, tasa_cob_esp
  )]
  
  return(out_final)
}

# sectores_evol
# Genera una tabla wide con exportaciones e importaciones por sector y año,
# desde ano_ini hasta anho (ambos inclusive).
#
# Columnas resultantes por cada año Y:
#   exp_mad_Y, imp_mad_Y, exp_esp_Y, imp_esp_Y
#
# Parámetros:
#   ds_mad     : Arrow dataset Madrid
#   ds_esp     : Arrow dataset España
#   df_sec     : data.table de metadatos de sectores (salida de leer_excel_sheets)
#   parametros : lista de parámetros del proyecto
#                (usa $anho, $ano_ini, $meses, $cod_pais)

.sectores_evol <- function(ds_mad, ds_esp, df_sec, parametros) {
  
  # ── 1. Extracción desde Arrow (todos los años de una vez) ───────────────────
  extraer_micro <- function(ds) {
    ds |>
      dplyr::filter(
        año <= parametros$anho & año >= parametros$ano_ini,
        mes %in% parametros$mes,
        pais == parametros$cod_pais
      ) |>
      dplyr::group_by(flujo, año, cod_sector_economico) |>
      dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
      dplyr::collect() |>
      data.table::as.data.table()
  }
  
  micro_mad <- extraer_micro(ds_mad)
  micro_esp <- extraer_micro(ds_esp)
  
  # ── 2. Join con metadatos de sectores ───────────────────────────────────────
  micro_mad <- df_sec[micro_mad, on = .(cod_sec = cod_sector_economico), nomatch = 0]
  micro_esp <- df_sec[micro_esp, on = .(cod_sec = cod_sector_economico), nomatch = 0]
  
  # ── 3. Agregación por sector y año ──────────────────────────────────────────
  agregar <- function(df) {
    df[, .(euros = sum(euros, na.rm = TRUE)),
       by = .(orden, niv, nombre, flujo, año)]
  }
  
  df_mad <- agregar(micro_mad)
  df_esp <- agregar(micro_esp)
  
  # ── 4. Pivot wide: una columna por flujo x año ───────────────────────────────
  # flujo 1 = exp, flujo 0 = imp
  pivotar <- function(df, prefijo) {
    df[, flujo_label := ifelse(flujo == 1L,
                               paste0("exp_", prefijo, "_", año),
                               paste0("imp_", prefijo, "_", año))]
    data.table::dcast(df, orden + niv + nombre ~ flujo_label,
                      value.var = "euros", fill = 0)
  }
  
  wide_mad <- pivotar(df_mad, "mad")
  wide_esp <- pivotar(df_esp, "esp")
  
  # ── 5. Añadir fila 18 (Semifacturas no químicas = suma órdenes 19-23) ───────
  # Se aplica antes de fila 65 para que la incluya en el subtotal
  anos <- sort(unique(df_mad$año))
  
  añadir_fila18 <- function(df, prefijo) {
    cols_val <- names(df)[names(df) %in%
                            c(paste0("exp_", prefijo, "_", anos),
                              paste0("imp_", prefijo, "_", anos))]
    fila_val <- df[orden %in% c(19, 20, 21, 22, 23),
                   lapply(.SD, sum, na.rm = TRUE),
                   .SDcols = cols_val]
    fila_18  <- data.table::data.table(
      orden  = 18L,
      niv    = 1L,
      nombre = "Semifacturas no qu\u00edmicas"
    )
    fila_18 <- cbind(fila_18, fila_val)
    data.table::setorder(
      data.table::rbindlist(list(df, fila_18), use.names = TRUE, fill = TRUE),
      orden
    )
  }
  
  # ── 6. Añadir fila 65 (Subtotal = suma de filas niv == 1) ───────────────────
  añadir_fila65 <- function(df, prefijo) {
    cols_val <- names(df)[names(df) %in%
                            c(paste0("exp_", prefijo, "_", anos),
                              paste0("imp_", prefijo, "_", anos))]
    fila_val <- df[niv == 1L,
                   lapply(.SD, sum, na.rm = TRUE),
                   .SDcols = cols_val]
    fila_65  <- data.table::data.table(
      orden  = 65L,
      niv    = 9L,
      nombre = "Subtotal"
    )
    fila_65 <- cbind(fila_65, fila_val)
    data.table::setorder(
      data.table::rbindlist(list(df, fila_65), use.names = TRUE, fill = TRUE),
      orden
    )
  }
  
  wide_mad <- añadir_fila18(wide_mad, "mad")
  wide_esp <- añadir_fila18(wide_esp, "esp")
  
  wide_mad <- añadir_fila65(wide_mad, "mad")
  wide_esp <- añadir_fila65(wide_esp, "esp")
  
  # ── 7. Join Madrid + España ──────────────────────────────────────────────────
  out <- merge(wide_mad, wide_esp[, !c("niv", "nombre")], by = "orden", all.x = TRUE)
  
  # ── 8. Ordenar columnas: id | exp_mad_Y imp_mad_Y exp_esp_Y imp_esp_Y por año
  id_cols  <- c("orden", "niv", "nombre")
  val_cols <- unlist(lapply(anos, function(y) c(
    paste0("exp_mad_", y), paste0("imp_mad_", y),
    paste0("exp_esp_", y), paste0("imp_esp_", y)
  )))
  val_cols <- val_cols[val_cols %in% names(out)]
  
  data.table::setcolorder(out, c(id_cols, val_cols))
  data.table::setorder(out, orden)
  out[]
}

### Completa ----
.tabla_paises_f <- function(ds_mad, ds_esp, df_paises, parametros, totales) {
  
  # --- Extracción desde Arrow ---
  extraer_micro <- function(ds) {
    ds |>
      dplyr::filter(
        año == parametros$anho | año == parametros$anho - 1L,
        mes %in% parametros$mes,
        cod_sector_economico %in% parametros$cod_sector
      ) |>
      dplyr::group_by(flujo, año, pais) |>
      dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
      dplyr::collect() |>
      data.table::as.data.table()
  }
  
  micro_mad <- extraer_micro(ds_mad)
  micro_esp <- extraer_micro(ds_esp)
  
  # --- Join con metadatos de países ---
  micro_mad <- df_paises[micro_mad, on = .(cod = pais), nomatch = 0]
  micro_esp <- df_paises[micro_esp, on = .(cod = pais), nomatch = 0]
  
  # --- Agregación por país ---
  agregar <- function(df) {
    df[, .(
      exp      = sum(euros[flujo == 1 & año == parametros$anho],      na.rm = TRUE),
      exp_prev = sum(euros[flujo == 1 & año == parametros$anho - 1L], na.rm = TRUE),
      imp      = sum(euros[flujo == 0 & año == parametros$anho],      na.rm = TRUE),
      imp_prev = sum(euros[flujo == 0 & año == parametros$anho - 1L], na.rm = TRUE)
    ), by = .(cod, pais, reg)]
  }
  
  df_mad <- agregar(micro_mad)
  df_esp <- agregar(micro_esp)
  
  # --- Combinar regiones ---
  df_mad[, region := "Madrid"]
  df_esp[, region := "España"]
  
  df_out <- data.table::rbindlist(list(df_mad, df_esp), use.names = TRUE, fill = TRUE)
  
  # --- Indicadores ---
  df_out[, exp_dif    := exp - exp_prev]
  df_out[, imp_dif    := imp - imp_prev]
  df_out[, tva_exp    := ifelse(exp_prev != 0, exp_dif / exp_prev, 0)]
  df_out[, tva_imp    := ifelse(imp_prev != 0, imp_dif / imp_prev, 0)]
  df_out[, saldo      := exp - imp]
  df_out[, saldo_prev := exp_prev - imp_prev]
  
  df_out[]
}

# sectores_evol
# Genera una tabla wide con exportaciones e importaciones por sector y año,
# desde ano_ini hasta anho (ambos inclusive).
#
# Columnas resultantes por cada año Y:
#   exp_mad_Y, imp_mad_Y, exp_esp_Y, imp_esp_Y
#
# Parámetros:
#   ds_mad     : Arrow dataset Madrid
#   ds_esp     : Arrow dataset España
#   df_sec     : data.table de metadatos de sectores (salida de leer_excel_sheets)
#   parametros : lista de parámetros del proyecto
#                (usa $anho, $ano_ini, $meses, $cod_pais)

## Evol ----
# sectores_evol
# Genera una tabla wide con exportaciones e importaciones por sector y año,
# desde ano_ini hasta anho (ambos inclusive).
#
# Columnas resultantes por cada año Y:
#   exp_mad_Y, imp_mad_Y, exp_esp_Y, imp_esp_Y
#
# Parámetros:
#   ds_mad     : Arrow dataset Madrid
#   ds_esp     : Arrow dataset España
#   df_sec     : data.table de metadatos de sectores (salida de leer_excel_sheets)
#   parametros : lista de parámetros del proyecto
#                (usa $anho, $ano_ini, $meses, $cod_pais)

.sectores_evol <- function(ds_mad, ds_esp, df_sec, parametros) {
  
  # ── 1. Extracción desde Arrow (todos los años de una vez) ───────────────────
  extraer_micro <- function(ds) {
    ds |>
      dplyr::filter(
        año <= parametros$anho & año >= parametros$anho_idx,
        mes %in% parametros$mes,
        pais == parametros$cod_pais
      ) |>
      dplyr::group_by(flujo, año, cod_sector_economico) |>
      dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
      dplyr::collect() |>
      data.table::as.data.table()
  }
  
  micro_mad <- extraer_micro(ds_mad)
  micro_esp <- extraer_micro(ds_esp)
  
  # ── 2. Join con metadatos de sectores ───────────────────────────────────────
  micro_mad <- df_sec[micro_mad, on = .(cod_sec = cod_sector_economico), nomatch = 0]
  micro_esp <- df_sec[micro_esp, on = .(cod_sec = cod_sector_economico), nomatch = 0]
  
  # ── 3. Agregación por sector y año ──────────────────────────────────────────
  agregar <- function(df) {
    df[, .(euros = sum(euros, na.rm = TRUE)),
       by = .(orden, niv, nombre, flujo, año)]
  }
  
  df_mad <- agregar(micro_mad)
  df_esp <- agregar(micro_esp)
  
  # ── 4. Pivot wide: una columna por flujo x año ───────────────────────────────
  # flujo 1 = exp, flujo 0 = imp
  pivotar <- function(df, prefijo) {
    df[, flujo_label := ifelse(flujo == 1L,
                               paste0("exp_", prefijo, "_", año),
                               paste0("imp_", prefijo, "_", año))]
    data.table::dcast(df, orden + niv + nombre ~ flujo_label,
                      value.var = "euros", fill = 0)
  }
  
  wide_mad <- pivotar(df_mad, "mad")
  wide_esp <- pivotar(df_esp, "esp")
  
  # ── 5. Añadir fila 18 (Semifacturas no químicas = suma órdenes 19-23) ───────
  # Se aplica antes de fila 65 para que la incluya en el subtotal
  anos <- sort(unique(df_mad$año))
  
  añadir_fila18 <- function(df, prefijo) {
    cols_val <- names(df)[names(df) %in%
                            c(paste0("exp_", prefijo, "_", anos),
                              paste0("imp_", prefijo, "_", anos))]
    fila_val <- df[orden %in% c(19, 20, 21, 22, 23),
                   lapply(.SD, sum, na.rm = TRUE),
                   .SDcols = cols_val]
    fila_18  <- data.table::data.table(
      orden  = 18L,
      niv    = 1L,
      nombre = "Semifacturas no qu\u00edmicas"
    )
    fila_18 <- cbind(fila_18, fila_val)
    data.table::setorder(
      data.table::rbindlist(list(df, fila_18), use.names = TRUE, fill = TRUE),
      orden
    )
  }
  
  # ── 6. Añadir fila 65 (Subtotal = suma de filas niv == 1) ───────────────────
  añadir_fila65 <- function(df, prefijo) {
    cols_val <- names(df)[names(df) %in%
                            c(paste0("exp_", prefijo, "_", anos),
                              paste0("imp_", prefijo, "_", anos))]
    fila_val <- df[niv == 1L,
                   lapply(.SD, sum, na.rm = TRUE),
                   .SDcols = cols_val]
    fila_65  <- data.table::data.table(
      orden  = 65L,
      niv    = 9L,
      nombre = "Subtotal"
    )
    fila_65 <- cbind(fila_65, fila_val)
    data.table::setorder(
      data.table::rbindlist(list(df, fila_65), use.names = TRUE, fill = TRUE),
      orden
    )
  }
  
  wide_mad <- añadir_fila18(wide_mad, "mad")
  wide_esp <- añadir_fila18(wide_esp, "esp")
  
  wide_mad <- añadir_fila65(wide_mad, "mad")
  wide_esp <- añadir_fila65(wide_esp, "esp")
  
  # ── 7. Join Madrid + España ──────────────────────────────────────────────────
  out <- merge(wide_mad, wide_esp[, !c("niv", "nombre")], by = "orden", all.x = TRUE)
  out <- out[!is.na(niv) & niv != ""]
  
  # ── 8. Ordenar columnas: id | exp_mad_Y imp_mad_Y exp_esp_Y imp_esp_Y por año
  id_cols  <- c("orden", "niv", "nombre")
  val_cols <- unlist(lapply(anos, function(y) c(
    paste0("exp_mad_", y), paste0("imp_mad_", y),
    paste0("exp_esp_", y), paste0("imp_esp_", y)
  )))
  val_cols <- val_cols[val_cols %in% names(out)]
  
  data.table::setcolorder(out, c(id_cols, val_cols))
  data.table::setorder(out, orden)
  out[]
}

# paises_evol
# Genera una tabla wide con exportaciones e importaciones por país y año,
# desde ano_ini hasta anho (ambos inclusive).
#
# Columnas resultantes por cada año Y:
#   exp_mad_Y, imp_mad_Y, exp_esp_Y, imp_esp_Y
#
# Parámetros:
#   ds_mad     : Arrow dataset Madrid
#   ds_esp     : Arrow dataset España
#   df_paises  : data.table de metadatos de países (salida de leer_excel_sheets)
#   parametros : lista de parámetros del proyecto
#                (usa $anho, $ano_ini, $meses, $cod_sector)

# paises_evol
# Genera una tabla wide con exportaciones e importaciones por país y año,
# desde ano_ini hasta anho (ambos inclusive).
#
# Columnas resultantes por cada año Y:
#   exp_mad_Y, imp_mad_Y, exp_esp_Y, imp_esp_Y
#
# Parámetros:
#   ds_mad     : Arrow dataset Madrid
#   ds_esp     : Arrow dataset España
#   df_paises  : data.table de metadatos de países (salida de leer_excel_sheets)
#   parametros : lista de parámetros del proyecto
#                (usa $anho, $ano_ini, $meses, $cod_sector)

.paises_evol <- function(ds_mad, ds_esp, df_paises, parametros) {
  
  # ── 1. Extracción desde Arrow (todos los años de una vez) ───────────────────
  extraer_micro <- function(ds) {
    ds |>
      dplyr::filter(
        año <= parametros$anho & año >= parametros$anho_idx,
        mes %in% parametros$mes,
        cod_sector_economico == parametros$cod_sector
      ) |>
      dplyr::group_by(flujo, año, pais) |>
      dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
      dplyr::collect() |>
      data.table::as.data.table()
  }
  
  micro_mad <- extraer_micro(ds_mad)
  micro_esp <- extraer_micro(ds_esp)
  
  # ── 1b. Extraer fila 72 (Total territorial, cod_pais == 0) antes del join ───
  # cod 0 no existe en df_paises → lo capturamos aquí antes de que nomatch lo elimine
  extraer_fila72 <- function(df, prefijo) {
    tot <- df[pais == 0L, .(euros = sum(euros, na.rm = TRUE)), by = .(flujo, año)]
    tot[, flujo_label := ifelse(flujo == 1L,
                                paste0("exp_", prefijo, "_", año),
                                paste0("imp_", prefijo, "_", año))]
    wide <- data.table::dcast(tot, 1 ~ flujo_label, value.var = "euros", fill = 0)
    wide[, `1` := NULL]
    wide
  }
  
  fila72_mad_vals <- extraer_fila72(micro_mad, "mad")
  fila72_esp_vals <- extraer_fila72(micro_esp, "esp")
  
  # ── 2. Join con metadatos de países ─────────────────────────────────────────
  micro_mad <- df_paises[micro_mad, on = .(cod = pais), nomatch = 0]
  micro_esp <- df_paises[micro_esp, on = .(cod = pais), nomatch = 0]
  
  # ── 3. Agregación por país y año ────────────────────────────────────────────
  agregar <- function(df) {
    df[, .(euros = sum(euros, na.rm = TRUE)),
       by = .(cod, pais, reg, flujo, año)]
  }
  
  df_mad <- agregar(micro_mad)
  df_esp <- agregar(micro_esp)
  
  anos <- sort(unique(df_mad$año))
  
  # ── 4. Añadir filas de regiones geográficas ─────────────────────────────────
  df_codigos_reg <- data.table::data.table(
    pais = c(
      "ZONA EURO", "RESTO DE EUROPA", "RESTO UE",
      "ASIA (excl. Oriente Medio)", "ÁFRICA",
      "AMÉRICA DEL NORTE", "AMÉRICA LATINA", "RESTO DE AMÉRICA",
      "ORIENTE MEDIO", "OCEANÍA", "OTROS"
    ),
    cod = c(1001, 1003, 1002, 1007, 1009, 1004, 1005, 1006, 1008, 1010, 1011)
  )
  
  añadir_regiones <- function(df) {
    df_regiones <- df[!is.na(reg), .(euros = sum(euros, na.rm = TRUE)),
                      by = .(reg, flujo, año)]
    data.table::setnames(df_regiones, "reg", "pais")
    df_regiones <- df_codigos_reg[df_regiones, on = "pais"]
    data.table::rbindlist(list(df, df_regiones), use.names = TRUE, fill = TRUE)
  }
  
  df_mad <- añadir_regiones(df_mad)
  df_esp <- añadir_regiones(df_esp)
  
  # ── 5. Pivot wide: una columna por flujo x año ───────────────────────────────
  pivotar <- function(df, prefijo) {
    df[, flujo_label := ifelse(flujo == 1L,
                               paste0("exp_", prefijo, "_", año),
                               paste0("imp_", prefijo, "_", año))]
    data.table::dcast(df, cod + pais ~ flujo_label,
                      value.var = "euros", fill = 0)
  }
  
  wide_mad <- pivotar(df_mad, "mad")
  wide_esp <- pivotar(df_esp, "esp")
  
  # ── 6. Join con metadatos de orden/niv ──────────────────────────────────────
  ordenar_paises <- function(df) {
    df[, pais := NULL]
    df_paises[, .(cod, pais, orden, niv)][df, on = "cod", nomatch = 0][
      !is.na(orden)
    ][order(orden)]
  }
  
  wide_mad <- ordenar_paises(wide_mad)
  wide_esp <- ordenar_paises(wide_esp)
  
  # ── 7. Helper: añadir fila agregada (suma de ordenes_origen) ────────────────
  cols_mad <- paste0(rep(c("exp_mad_", "imp_mad_"), each = length(anos)), anos)
  cols_esp <- paste0(rep(c("exp_esp_", "imp_esp_"), each = length(anos)), anos)
  
  añadir_fila_agregada <- function(df, ordenes_origen, cod, pais, orden, niv, cols_val) {
    fila_val <- df[orden %in% ordenes_origen,
                   lapply(.SD, sum, na.rm = TRUE),
                   .SDcols = cols_val]
    fila <- data.table::data.table(cod = cod, pais = pais, orden = orden, niv = niv)
    cbind(fila, fila_val)
  }
  
  # ── 8. procesar_region: UE27, América, Asia — luego Europa ──────────────────
  procesar_region <- function(df, cols_val) {
    fila_ue27    <- añadir_fila_agregada(df, c(3, 23),      1013L, "UE 27",   2L,  2L, cols_val)
    fila_america <- añadir_fila_agregada(df, c(38, 41, 47), 1014L, "AMERICA", 37L, 1L, cols_val)
    fila_asia    <- añadir_fila_agregada(df, c(49, 59),     1015L, "ASIA",    48L, 1L, cols_val)
    
    df <- data.table::rbindlist(
      list(df, fila_ue27, fila_america, fila_asia),
      use.names = TRUE, fill = TRUE
    )[order(orden)]
    
    # Europa se calcula después de añadir UE27 (orden 2) y Resto Europa (orden 31)
    fila_europa <- añadir_fila_agregada(df, c(2, 31), 1012L, "EUROPA", 1L, 1L, cols_val)
    
    data.table::rbindlist(
      list(df, fila_europa),
      use.names = TRUE, fill = TRUE
    )[order(orden)]
  }
  
  wide_mad <- procesar_region(wide_mad, cols_mad[cols_mad %in% names(wide_mad)])
  wide_esp <- procesar_region(wide_esp, cols_esp[cols_esp %in% names(wide_esp)])
  
  # ── 9. Añadir fila 71 (Subtotal = suma de filas niv == 1) ───────────────────
  añadir_fila71 <- function(df, cols_val) {
    fila_val <- df[niv == 1L,
                   lapply(.SD, sum, na.rm = TRUE),
                   .SDcols = cols_val]
    fila_71  <- data.table::data.table(
      cod   = 1100L,
      pais  = "Subtotal",
      orden = 71L,
      niv   = 9L
    )
    fila_71 <- cbind(fila_71, fila_val)
    data.table::rbindlist(
      list(df, fila_71),
      use.names = TRUE, fill = TRUE
    )[order(orden)]
  }
  
  wide_mad <- añadir_fila71(wide_mad, cols_mad[cols_mad %in% names(wide_mad)])
  wide_esp <- añadir_fila71(wide_esp, cols_esp[cols_esp %in% names(wide_esp)])
  
  # ── 10. Añadir fila 72 (Total territorial — cod_pais == 0) ──────────────────
  fila72_mad <- cbind(
    data.table::data.table(cod = 0L, pais = "Total territorial", orden = 72L, niv = 9L),
    fila72_mad_vals
  )
  fila72_esp <- cbind(
    data.table::data.table(cod = 0L, pais = "Total territorial", orden = 72L, niv = 0L),
    fila72_esp_vals
  )
  
  wide_mad <- data.table::rbindlist(list(wide_mad, fila72_mad), use.names = TRUE, fill = TRUE)[order(orden)]
  wide_esp <- data.table::rbindlist(list(wide_esp, fila72_esp), use.names = TRUE, fill = TRUE)[order(orden)]
  
  # ── 11. Join Madrid + España ─────────────────────────────────────────────────
  out <- merge(wide_mad, wide_esp[, !c("niv", "pais")], by = c("orden", "cod"), all.x = TRUE)
  out <- out[!is.na(niv) & niv != ""]
  
  # ── 12. Ordenar columnas: id | exp_mad_Y imp_mad_Y exp_esp_Y imp_esp_Y por año
  id_cols  <- c("orden", "niv", "cod", "pais")
  val_cols <- unlist(lapply(anos, function(y) c(
    paste0("exp_mad_", y), paste0("imp_mad_", y),
    paste0("exp_esp_", y), paste0("imp_esp_", y)
  )))
  val_cols <- val_cols[val_cols %in% names(out)]
  
  data.table::setcolorder(out, c(id_cols, val_cols))
  data.table::setorder(out, orden)
  out[]
}

# procesar_evol.R
# Funciones de post-proceso para tablas de evolución anual wide
# Calcula para cada año Y:
#   _pct     : cuota sobre el total (orden 65 / orden 71)
#   _vs_esp  : ratio Madrid / España   (solo sectores/países con ambas cols)
#   _idx     : índice base año_base = 100
#   _contrib : contribución a la variación total año_base → anho
#              = (val_Y - val_base) / total_base

# ── Helper compartido ─────────────────────────────────────────────────────────
# fila_total: función que recibe `out` y devuelve la fila de totales (1 fila)
#   Por defecto usa orden == orden_total (tablas datacomex/evol estándar)
#   Para tablas _f se pasa e.g. function(x) x[cod_sector_economico == "0"]
.calcular_indicadores_evol <- function(out, flujos, territorios, anos,
                                       orden_total = NULL, ano_base,
                                       fila_total  = NULL) {
  ano_base  <- as.character(ano_base)
  ano_final <- as.character(max(as.integer(anos)))
  
  # Construir función de filtro si no se proporcionó explícitamente
  if (is.null(fila_total)) {
    stopifnot(!is.null(orden_total))
    fila_total <- function(x) x[orden == orden_total]
  }
  
  for (fl in flujos) {           # "exp" / "imp"
    for (ter in territorios) {   # "mad" / "esp"
      
      col_base   <- paste0(fl, "_", ter, "_", ano_base)
      if (!col_base %in% names(out)) next
      
      total_base <- fila_total(out)[[col_base]][1]
      if (is.null(total_base) || is.na(total_base) || total_base == 0) total_base <- NA_real_
      
      for (yr in anos) {
        col_y <- paste0(fl, "_", ter, "_", yr)
        if (!col_y %in% names(out)) next
        
        total_y <- fila_total(out)[[col_y]][1]
        if (is.null(total_y) || is.na(total_y) || total_y == 0) total_y <- NA_real_
        
        pct_col <- paste0(fl, "_", ter, "_pct_", yr)
        idx_col <- paste0(fl, "_", ter, "_idx_", yr)
        
        out[, (pct_col) := get(col_y) / total_y]
        out[, (idx_col) := (get(col_y) / get(col_base)) * 100]
        
        if (yr == ano_final) {
          contrib_col <- paste0(fl, "_", ter, "_contrib")
          out[, (contrib_col) := (get(col_y) - get(col_base)) / total_base]
        }
      }
      
      if (ter == "mad") {
        for (yr in anos) {
          col_mad <- paste0(fl, "_mad_", yr)
          col_esp <- paste0(fl, "_esp_", yr)
          vs_col  <- paste0(fl, "_mad_vs_esp_", yr)
          if (col_mad %in% names(out) && col_esp %in% names(out)) {
            out[, (vs_col) := get(col_mad) / get(col_esp)]
          }
        }
      }
    }
  }
  return(out)
}


# ── 1. procesar_evol_sectores ─────────────────────────────────────────────────
.procesar_evol_sectores <- function(tabla, ano_base = 2019L) {
  out  <- data.table::copy(tabla)
  
  # Limpiar columnas basura de merges de forma segura
  garbage_cols <- grep("^\\.\\.", names(out), value = TRUE)
  if (length(garbage_cols) > 0) out[, (garbage_cols) := NULL]
  
  anos <- sort(unique(as.integer(
    sub("exp_mad_", "", grep("^exp_mad_\\d{4}$", names(out), value = TRUE))
  )))
  
  out <- .calcular_indicadores_evol(
    out         = out,
    flujos      = c("exp", "imp"),
    territorios = c("mad", "esp"),
    anos        = anos,
    orden_total = 65L,      # Fila subtotal sectores
    ano_base    = ano_base
  )
  
  # Ordenar columnas
  id_cols      <- intersect(c("orden", "niv", "nombre"), names(out))
  contrib_cols <- grep("_contrib$", names(out), value = TRUE)
  yr_cols      <- setdiff(names(out), c(id_cols, contrib_cols))
  
  data.table::setcolorder(out, c(id_cols, yr_cols, contrib_cols))
  data.table::setorder(out, orden)
  
  return(out[])
}

.procesar_evol_paises <- function(tabla, ano_base = 2019L) {
  out <- data.table::copy(tabla)
  
  # Limpiar columnas basura de merges de forma segura
  garbage_cols <- grep("^\\.\\.", names(out), value = TRUE)
  if (length(garbage_cols) > 0) out[, (garbage_cols) := NULL]
  
  anos <- sort(unique(as.integer(
    sub("exp_mad_", "", grep("^exp_mad_\\d{4}$", names(out), value = TRUE))
  )))
  
  out <- .calcular_indicadores_evol(
    out         = out,
    flujos      = c("exp", "imp"),
    territorios = c("mad", "esp"),
    anos        = anos,
    orden_total = 71L,      # Fila subtotal países
    ano_base    = ano_base
  )
  
  # Ordenar columnas
  id_cols      <- intersect(c("orden", "niv", "cod", "pais"), names(out))
  contrib_cols <- grep("_contrib$", names(out), value = TRUE)
  yr_cols      <- setdiff(names(out), c(id_cols, contrib_cols))
  
  data.table::setcolorder(out, c(id_cols, yr_cols, contrib_cols))
  data.table::setorder(out, orden)
  
  return(out[])
}

# ── sectores_evol_f ───────────────────────────────────────────────────────────
# Parámetros:
#   ds_mad     : Arrow dataset Madrid
#   ds_esp     : Arrow dataset España
#   df_sec     : data.table de metadatos de sectores
#   parametros : lista con $anho, $anho_idx, $meses, $cod_pais
.sectores_evol_f <- function(ds_mad, ds_esp, df_sec, parametros) {
  
  # ── 1. Extracción desde Arrow ────────────────────────────────────────────────
  extraer_micro <- function(ds) {
    ds |>
      dplyr::filter(
        año <= parametros$anho & año >= parametros$anho_idx,
        mes %in% parametros$mes,
        pais == parametros$cod_pais
      ) |>
      dplyr::group_by(flujo, año, cod_sector_economico) |>
      dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
      dplyr::collect() |>
      data.table::as.data.table()
  }
  
  micro_mad <- extraer_micro(ds_mad)
  micro_esp <- extraer_micro(ds_esp)
  
  # ── 2. Join con metadatos ────────────────────────────────────────────────────
  micro_mad <- df_sec[micro_mad,
                      .(cod_sec, nombre, niv, orden, codconnombre,
                        flujo, año, euros),
                      on = .(cod_sec = cod_sector_economico),
                      nomatch = 0]
  
  micro_esp <- df_sec[micro_esp,
                      .(cod_sec, nombre, niv, orden, codconnombre,
                        flujo, año, euros),
                      on = .(cod_sec = cod_sector_economico),
                      nomatch = 0]
  
  # ── 3. Agregación ────────────────────────────────────────────────────────────
  agregar <- function(df) {
    df[, .(euros = sum(euros, na.rm = TRUE)),
       by = .(cod_sec, codconnombre, orden, niv, nombre, flujo, año)]
  }
  
  df_mad <- agregar(micro_mad)
  df_esp <- agregar(micro_esp)
  
  # ── 4. Pivot wide ────────────────────────────────────────────────────────────
  pivotar <- function(df, prefijo) {
    df[, flujo_label := ifelse(flujo == 1L,
                               paste0("exp_", prefijo, "_", año),
                               paste0("imp_", prefijo, "_", año))]
    data.table::dcast(df,
                      cod_sec + nombre + niv + orden + codconnombre ~ flujo_label,
                      value.var = "euros", fill = 0)
  }
  
  wide_mad <- pivotar(df_mad, "mad")
  wide_esp <- pivotar(df_esp, "esp")
  
  # ── 5. Join Madrid + España ──────────────────────────────────────────────────
  out <- merge(wide_mad, wide_esp[, !c("nombre", "niv", "codconnombre")],
               by = c("cod_sec", "orden"), all = TRUE)
  out <- out[!is.na(niv)]
  
  # ── 6. Ordenar columnas ──────────────────────────────────────────────────────
  anos    <- seq(parametros$anho_idx, parametros$anho)
  id_cols <- c("cod_sec", "nombre", "niv", "orden", "codconnombre")
  val_cols <- unlist(lapply(anos, function(y) c(
    paste0("exp_mad_", y), paste0("imp_mad_", y),
    paste0("exp_esp_", y), paste0("imp_esp_", y)
  )))
  val_cols <- val_cols[val_cols %in% names(out)]
  
  data.table::setcolorder(out, c(id_cols, val_cols))
  data.table::setorder(out, orden)
  out[]
}


# ── paises_evol_f ─────────────────────────────────────────────────────────────
# Parámetros:
#   ds_mad     : Arrow dataset Madrid
#   ds_esp     : Arrow dataset España
#   df_paises  : data.table de metadatos de países
#   parametros : lista con $anho, $anho_idx, $meses, $cod_sector
.paises_evol_f <- function(ds_mad, ds_esp, df_paises, parametros) {
  
  # ── 1. Extracción desde Arrow ────────────────────────────────────────────────
  extraer_micro <- function(ds) {
    ds |>
      dplyr::filter(
        año <= parametros$anho & año >= parametros$anho_idx,
        mes %in% parametros$mes,
        cod_sector_economico == parametros$cod_sector
      ) |>
      dplyr::group_by(flujo, año, pais) |>
      dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
      dplyr::collect() |>
      data.table::as.data.table()
  }
  
  micro_mad <- extraer_micro(ds_mad)
  micro_esp <- extraer_micro(ds_esp)
  
  # ── 2. Agregación ────────────────────────────────────────────────────────────
  agregar <- function(df) {
    df[, .(euros = sum(euros, na.rm = TRUE)),
       by = .(pais, flujo, año)]
  }
  
  df_mad <- agregar(micro_mad)
  df_esp <- agregar(micro_esp)
  
  # ── 3. Pivot wide ────────────────────────────────────────────────────────────
  pivotar <- function(df, prefijo) {
    df[, flujo_label := ifelse(flujo == 1L,
                               paste0("exp_", prefijo, "_", año),
                               paste0("imp_", prefijo, "_", año))]
    data.table::dcast(df, pais ~ flujo_label,
                      value.var = "euros", fill = 0)
  }
  
  wide_mad <- pivotar(df_mad, "mad")
  wide_esp <- pivotar(df_esp, "esp")
  
  # ── 4. Join Madrid + España ──────────────────────────────────────────────────
  out <- merge(wide_mad, wide_esp, by = "pais", all = TRUE)
  
  # ── 5. Cruce con metadatos de países ─────────────────────────────────────────
  meta <- df_paises[, .(cod, pais, paisconcod, reg)]
  out  <- meta[out, on = .(cod = pais)]
  
  # ── 6. Ordenar columnas ──────────────────────────────────────────────────────
  anos    <- seq(parametros$anho_idx, parametros$anho)
  id_cols <- c("cod", "pais", "paisconcod", "reg")
  val_cols <- unlist(lapply(anos, function(y) c(
    paste0("exp_mad_", y), paste0("imp_mad_", y),
    paste0("exp_esp_", y), paste0("imp_esp_", y)
  )))
  val_cols <- val_cols[val_cols %in% names(out)]
  
  data.table::setcolorder(out, c(id_cols, val_cols))
  out[]
}

# CCAAs ----
.dataframe_general <- function(df, para, meta){
  # Month
  df_act <- df[
    mes %in% para$mes & 
      año == para$anho, 
    .(euros = sum(euros)), 
    by = .(flujo, cod_comunidad, año)
  ]
  
  df_pas <- df[
    mes %in% para$mes & 
      año == para$anho - 1, 
    .(euros_ant = sum(euros)), 
    by = .(flujo, cod_comunidad, año = año + 1)
  ]
  
  # YTM
  df_acu_act <- df[
    mes %in% 1L:para$mes & 
      año == para$anho, 
    .(euros_acu = sum(euros)), 
    by = .(flujo, cod_comunidad, año)
  ]
  
  df_acu_pas <- df[
    mes %in% 1L:para$mes & 
      año == para$anho - 1, 
    .(euros_acu_ant = sum(euros)), 
    by = .(flujo, cod_comunidad, año = año + 1)
  ]
  
  # Finished years
  df_ano_ant <- df[
    año == para$anho - 1, 
    .(euros_anoant = sum(euros)),
    by = .(flujo, cod_comunidad, año = año + 1)
  ]
  
  df_ano_ant2 <- df[
    año == para$anho - 2, 
    .(euros_anoant2 = sum(euros)), 
    by = .(flujo, cod_comunidad, año = año + 2)
  ]
  
  # Merge df
  df <- merge(df_act, df_pas, by = c("flujo", "cod_comunidad", "año"), all.x = TRUE)
  df <- merge(df, df_acu_act, by = c("flujo", "cod_comunidad", "año"), all.x = TRUE)
  df <- merge(df, df_acu_pas, by = c("flujo", "cod_comunidad", "año"), all.x = TRUE)
  df <- merge(df, df_ano_ant, by = c("flujo", "cod_comunidad", "año"), all.x = TRUE)
  df <- merge(df, df_ano_ant2, by = c("flujo", "cod_comunidad", "año"), all.x = TRUE)
  
  # Pivot wider flujo
  df_wide <- data.table::dcast(
    df, 
    cod_comunidad ~ flujo, 
    value.var = c("euros", "euros_ant", "euros_acu", "euros_acu_ant", "euros_anoant", "euros_anoant2")
  )
  
  data.table::setnames(
    df_wide, 
    old = c("euros_0", "euros_ant_0", "euros_acu_0", "euros_acu_ant_0", "euros_anoant_0", "euros_anoant2_0",
            "euros_1", "euros_ant_1", "euros_acu_1", "euros_acu_ant_1", "euros_anoant_1", "euros_anoant2_1"),
    new = c("imp_euros", "imp_euros_ant", "imp_euros_acu", "imp_euros_acu_ant", "imp_euros_anoant", "imp_euros_anoant2",
            "exp_euros", "exp_euros_ant", "exp_euros_acu", "exp_euros_acu_ant", "exp_euros_anoant", "exp_euros_anoant2")
  )
  
  # Calculate differences and variation rates
  df_wide[, `:=`(
    imp_euros_dif = imp_euros - imp_euros_ant,
    exp_euros_dif = exp_euros - exp_euros_ant,
    imp_euros_tva = ((imp_euros - imp_euros_ant) / imp_euros_ant) * 100,
    exp_euros_tva = ((exp_euros - exp_euros_ant) / exp_euros_ant) * 100,
    imp_euros_acu_dif = imp_euros_acu - imp_euros_acu_ant,
    exp_euros_acu_dif = exp_euros_acu - exp_euros_acu_ant,
    imp_euros_acu_tva = ((imp_euros_acu - imp_euros_acu_ant) / imp_euros_acu_ant) * 100,
    exp_euros_acu_tva = ((exp_euros_acu - exp_euros_acu_ant) / exp_euros_acu_ant) * 100,
    imp_euros_anopas = imp_euros_anoant - imp_euros_anoant2,
    exp_euros_anopas = exp_euros_anoant - exp_euros_anoant2,
    imp_euros_tva2 = ((imp_euros_anoant - imp_euros_anoant2) / imp_euros_anoant2) * 100,
    exp_euros_tva2 = ((exp_euros_anoant - exp_euros_anoant2) / exp_euros_anoant2) * 100
  )]
  
  # Extract Spain totals
  lista_esp <- as.list(df_wide[cod_comunidad == 99])
  
  # Calculate repercussions, weights and rankings
  df_wide[, `:=`(
    imp_euros_rep = (imp_euros_dif / lista_esp$imp_euros_ant) * 100,
    exp_euros_rep = (exp_euros_dif / lista_esp$exp_euros_ant) * 100,
    imp_euros_acu_rep = (imp_euros_acu_dif / lista_esp$imp_euros_acu_ant) * 100,
    exp_euros_acu_rep = (exp_euros_acu_dif / lista_esp$exp_euros_acu_ant) * 100,
    imp_euros_tva2_rep = (imp_euros_anopas / lista_esp$imp_euros_anoant2) * 100,
    exp_euros_tva2_rep = (exp_euros_anopas / lista_esp$exp_euros_anoant2) * 100,
    imp_euros_peso = (imp_euros / lista_esp$imp_euros) * 100,
    exp_euros_peso = (exp_euros / lista_esp$exp_euros) * 100,
    imp_euros_acu_peso = (imp_euros_acu / lista_esp$imp_euros_acu) * 100,
    exp_euros_acu_peso = (exp_euros_acu / lista_esp$exp_euros_acu) * 100,
    imp_euros_anoant_peso = (imp_euros_anoant / lista_esp$imp_euros_anoant) * 100,
    exp_euros_anoant_peso = (exp_euros_anoant / lista_esp$exp_euros_anoant) * 100,
    imp_euros_rank = frank(-imp_euros, ties.method = "min"),
    exp_euros_rank = frank(-exp_euros, ties.method = "min"),
    imp_euros_acu_rank = frank(-imp_euros_acu, ties.method = "min"),
    exp_euros_acu_rank = frank(-exp_euros_acu, ties.method = "min"),
    imp_euros_anoant_rank = frank(-imp_euros_anoant, ties.method = "min"),
    exp_euros_anoant_rank = frank(-exp_euros_anoant, ties.method = "min")
  )]
  
  # Adjust ranks and apply Spain conditions
  df_wide[, `:=`(
    imp_euros_rank = if_else(imp_euros_rank == 1, NA_integer_, imp_euros_rank - 1L),
    exp_euros_rank = if_else(exp_euros_rank == 1, NA_integer_, exp_euros_rank - 1L),
    imp_euros_acu_rank = if_else(imp_euros_acu_rank == 1, NA_integer_, imp_euros_acu_rank - 1L),
    exp_euros_acu_rank = if_else(exp_euros_acu_rank == 1, NA_integer_, exp_euros_acu_rank - 1L),
    imp_euros_anoant_rank = if_else(imp_euros_anoant_rank == 1, NA_integer_, imp_euros_anoant_rank - 1L),
    exp_euros_anoant_rank = if_else(exp_euros_anoant_rank == 1, NA_integer_, exp_euros_anoant_rank - 1L),
    imp_euros_peso = if_else(cod_comunidad == 99, NA_real_, imp_euros_peso),
    exp_euros_peso = if_else(cod_comunidad == 99, NA_real_, exp_euros_peso),
    imp_euros_acu_peso = if_else(cod_comunidad == 99, NA_real_, imp_euros_acu_peso),
    exp_euros_acu_peso = if_else(cod_comunidad == 99, NA_real_, exp_euros_acu_peso),
    imp_euros_anoant_peso = if_else(cod_comunidad == 99, NA_real_, imp_euros_anoant_peso),
    exp_euros_anoant_peso = if_else(cod_comunidad == 99, NA_real_, exp_euros_anoant_peso),
    imp_euros_rep = if_else(cod_comunidad == 99, NA_real_, imp_euros_rep),
    exp_euros_rep = if_else(cod_comunidad == 99, NA_real_, exp_euros_rep),
    imp_euros_acu_rep = if_else(cod_comunidad == 99, NA_real_, imp_euros_acu_rep),
    exp_euros_acu_rep = if_else(cod_comunidad == 99, NA_real_, exp_euros_acu_rep),
    imp_euros_tva2_rep = if_else(cod_comunidad == 99, NA_real_, imp_euros_tva2_rep),
    exp_euros_tva2_rep = if_else(cod_comunidad == 99, NA_real_, exp_euros_tva2_rep)
  )]
  
  # Merge with metadata
  df_final <- merge(df_wide, meta, by.x = "cod_comunidad", by.y = "Condn")
  setorder(df_final, Coddax)
  
  # Select and order columns
  df_final <- df_final[, .(
    Coddax, Región, Etiqueta,
    exp_euros, exp_euros_peso, exp_euros_rank, 
    exp_euros_ant, exp_euros_dif, exp_euros_tva, exp_euros_rep,
    imp_euros, imp_euros_peso, imp_euros_rank, 
    imp_euros_ant, imp_euros_dif, imp_euros_tva, imp_euros_rep,
    exp_euros_acu, exp_euros_acu_peso, exp_euros_acu_rank,
    exp_euros_acu_ant, exp_euros_acu_dif, exp_euros_acu_tva, exp_euros_acu_rep,
    imp_euros_acu, imp_euros_acu_peso, imp_euros_acu_rank,
    imp_euros_acu_ant, imp_euros_acu_dif, imp_euros_acu_tva, imp_euros_acu_rep,
    exp_euros_anoant, exp_euros_anoant_peso, exp_euros_anoant_rank,
    exp_euros_anoant2, exp_euros_anopas, exp_euros_tva2, exp_euros_tva2_rep,
    imp_euros_anoant, imp_euros_anoant_peso, imp_euros_anoant_rank,
    imp_euros_anoant2, imp_euros_anopas, imp_euros_tva2, imp_euros_tva2_rep
  )]
  
  df_final
}

## Read data from ccaapais ----
.read_processed_data <- function(file_path, data_type) {
  # Leer el archivo directamente como data.table
  df <- fread(file_path)
  
  if (data_type == "mes") {
    
    df[, Fecha := as.IDate(Fecha)] 
  } else if (data_type == "trim") {
    df[, Trimestre := as.character(Trimestre)]
  } else if (data_type == "anos") {
    df[, Año := as.numeric(Año)]
  }
  
  return(df)
}

# Dataframe Plots contribuciones datacomex style ----
.df_plot_barras_contribucion_sectores_datacomex <- function(
    df               = df_evol_countryfull,
    para             = paramets,
    totalesf         = totalesanho,  
    flujo            = "exp",
    region           = "esp",
    meta             = meta_sec,
    dss_mad          = ds_mad,
    dss_esp          = ds_esp) {
  
  # ── Variables auxiliares ───────────────────────────────────────────────────
  N           <- para$max_bars_con
  col_ano     <- paste0(flujo, "_", region, "_", para$anho)
  col_anop    <- paste0(flujo, "_", region, "_", para$anho - 1)
  fil_flujo   <- ifelse(flujo == "exp", 1L, 0L)
  reg_buscada <- ifelse(region == "esp", "España", "Madrid")
  
  col_val_procesado  <- paste0(flujo, "_", region)
  col_total_bueno    <- paste0(flujo, "_prev_", region)
  
  # Extraemos el total previo global correcto desde totalesf
  val_total_prev_bueno <- totalesf[[col_total_bueno]]
  
  # ── Dataframes base ────────────────────────────────────────────────────────
  df <- df[, .(cod, pais, paisconcod, reg,
               valano  = get(col_ano),
               valanop = get(col_anop))]
  
  totales   <- as.list(df[cod == 0L])
  df_paises <- df[cod != 0L][, dif := valano - valanop]
  
  paises_pos <- df_paises[order(-dif)][1:N][
    , `:=`(
      tva    = 100 * dif / valanop,
      rep    = 100 * dif / totales$valanop,
      peso   = 100 * valano / totales$valano
    )
  ]
  
  paises_neg <- df_paises[order(dif)][1:N][
    , `:=`(
      tva    = 100 * dif / valanop,
      rep    = 100 * dif / totales$valanop,
      peso   = 100 * valano / totales$valano
    )
  ]
  
  lista_pos <- as.list(paises_pos$cod)
  lista_neg <- as.list(paises_neg$cod)
  
  para_pais <- para
  
  # ── Procesar Países Positivos ──────────────────────────────────────────────
  res_pos_list <- list()
  for (i in lista_pos) {
    para_pais$cod_pais <- i
    
    tabla_sectores_aux_paispos <- .tabla_sectores_datacomex(
      ds_mad     = dss_mad,
      ds_esp     = dss_esp,
      df_sec     = meta,
      parametros = para_pais
    )
    
    df_secpaispos <- .procesar_salida_sectores(
      tabla        = tabla_sectores_aux_paispos,
      listatotales = totalesf
    )
    
    col_contrib_mecanica <- paste0(flujo, "_", region, "_contrib")
    
    df_filtrado <- df_secpaispos[
      !orden %in% para$fil_sectores_plot
    ][order(-get(col_contrib_mecanica))][1:para$n_subsec_plotpais]
    
    # Mapeo y suma de diferencias asegurando correspondencia por código de sector
    reg_tabla_aux <- ifelse(region == "esp", "España", "Madrid")
    col_dif_aux   <- paste0(flujo, "_dif")
    
    total_dif_calc <- sum(
      tabla_sectores_aux_paispos[region == reg_tabla_aux & orden %in% df_filtrado$orden, get(col_dif_aux)], 
      na.rm = TRUE
    )
    
    res_pos_list[[as.character(i)]] <- df_filtrado[, .(
      cod           = i,
      sectores      = paste(nombre, collapse = ", "),
      total_dif     = total_dif_calc,
      rep_sectores  = 100 * total_dif_calc / val_total_prev_bueno
    )]
  }
  df_sectores_pos <- data.table::rbindlist(res_pos_list)
  
  # ── Procesar Países Negativos ──────────────────────────────────────────────
  res_neg_list <- list()
  for (i in lista_neg) {
    para_pais$cod_pais <- i
    
    tabla_sectores_aux_paisneg <- .tabla_sectores_datacomex(
      ds_mad     = dss_mad,
      ds_esp     = dss_esp,
      df_sec     = meta,
      parametros = para_pais
    )
    
    # CORRECCIÓN: Cambiado 'totalef' por el parámetro correcto 'totalesf'
    df_secpaisneg <- .procesar_salida_sectores(
      tabla        = tabla_sectores_aux_paisneg,
      listatotales = totalesf
    )
    
    col_contrib_mecanica <- paste0(flujo, "_", region, "_contrib")
    
    df_filtrado_neg <- df_secpaisneg[
      orden < 65 & niv >= 2
    ][order(get(col_contrib_mecanica))][1:para$n_subsec_plotpais]
    
    reg_tabla_aux <- ifelse(region == "esp", "España", "Madrid")
    col_dif_aux   <- paste0(flujo, "_dif")
    
    total_dif_calc_neg <- sum(
      tabla_sectores_aux_paisneg[region == reg_tabla_aux & orden %in% df_filtrado_neg$orden, get(col_dif_aux)], 
      na.rm = TRUE
    )
    
    res_neg_list[[as.character(i)]] <- df_filtrado_neg[, .(
      cod           = i,
      sectores      = paste(nombre, collapse = ", "),
      total_dif     = total_dif_calc_neg,
      rep_sectores  = 100 * total_dif_calc_neg / val_total_prev_bueno
    )]
  }
  df_sectores_neg <- data.table::rbindlist(res_neg_list)
  
  # ── Uniones Finales ────────────────────────────────────────────────────────
  paises_pos_final <- df_sectores_pos[paises_pos, on = "cod"]
  paises_neg_final <- df_sectores_neg[paises_neg, on = "cod"]
  
  chart_final <- data.table::rbindlist(list(paises_pos_final, paises_neg_final))
  chart_final <- chart_final[order(-rep)]
  
  return(chart_final)
}

.df_plot_barras_contribucion_paises_datacomex <- function(
    df               = df_sectores[!orden %in% paramets$fil_sectores_plot],
    para             = paramets,
    totalesf         = totalesanho,  
    flujo            = "exp",
    region           = "esp",
    metas            = meta_sec,
    metap            = meta_pais,
    dss_mad          = ds_mad,
    dss_esp          = ds_esp) {
  
  # Helper 
  clean_listasec <- function(x) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
    x <- lapply(x, function(z) {
      if (is.character(z) && grepl("^c\\(", z)) return(eval(parse(text = z)))
      return(z)
    })
    unique(as.character(unlist(x)))
  }
  
  # Vars aux
  N            <- para$max_bars_con
  col_dif      <- paste0(flujo, "_dif")
  col_tva      <- paste0(flujo, "_", region, "_tva")
  col_rep      <- paste0(flujo, "_", region, "_contrib")
  
  val_total_prev_bueno <- totalesf[[paste0(flujo, "_prev_", region)]]
  
  # Sectores pos/neg desde df_sectores
  df_base <- df[, .(
    orden, niv, nombre,
    tva = 100 * get(col_tva),
    rep = 100 * get(col_rep)
  )]
  
  sectores_pos <- df_base[order(-rep)][1:N]
  sectores_neg <- df_base[order(rep)][1:N]
  
  lista_pos <- sectores_pos$nombre
  lista_neg <- sectores_neg$nombre
  
  # Diccionario nombre → vector de cod_sec
  map_nombre_sec <- metas[
    !is.na(nombre),
    .(listasec = list(clean_listasec(listasec))),
    by = nombre
  ]
  dict_sec <- setNames(map_nombre_sec$listasec, map_nombre_sec$nombre)
  
  # Helper: llama a .tabla_paises_f para un nombre de sector y devuelve
  # los n_subsec_plotpais países más destacados (pos o neg) con sus métricas
  procesar_sector <- function(nombre_sec, orden_sec, dir_pos) {
    
    parasec <- para
    parasec$cod_sector <- as.character(unlist(dict_sec[[nombre_sec]]))
    
    df_country <- .tabla_paises_f(
      ds_mad     = dss_mad,
      ds_esp     = dss_esp,
      df_paises  = metap,
      parametros = parasec
    )
    
    reg_label <- ifelse(region == "esp", "España", "Madrid")
    
    df_paises_fil <- df_country[region == reg_label & cod > 0 & cod < 1000]
    
    df_top <- if (dir_pos) {
      df_paises_fil[order(-get(col_dif))][1:para$n_paises_plotsectores]
    } else {
      df_paises_fil[order(get(col_dif))][1:para$n_paises_plotsectores]
    }
    
    total_dif <- sum(df_top[[col_dif]], na.rm = TRUE)
    
    data.table::data.table(
      orden      = orden_sec,
      nombre     = nombre_sec,
      paises     = paste(df_top$pais, collapse = ", "),
      total_dif  = total_dif,
      rep_paises = 100 * total_dif / val_total_prev_bueno
    )
  }
  
  # ── Bucle positivos ───────────────────────────────────────────────────────
  res_pos_list <- vector("list", length(lista_pos))
  for (k in seq_along(lista_pos)) {
    res_pos_list[[k]] <- procesar_sector(lista_pos[k], sectores_pos$orden[k], dir_pos = TRUE)
  }
  
  # ── Bucle negativos ───────────────────────────────────────────────────────
  res_neg_list <- vector("list", length(lista_neg))
  for (k in seq_along(lista_neg)) {
    res_neg_list[[k]] <- procesar_sector(lista_neg[k], sectores_neg$orden[k], dir_pos = FALSE)
  }
  
  df_res_pos <- data.table::rbindlist(res_pos_list)
  df_res_neg <- data.table::rbindlist(res_neg_list)
  
  # ── Join con métricas del sector y unión final ────────────────────────────
  sec_pos_final <- df_res_pos[sectores_pos, on = "orden"]
  sec_neg_final <- df_res_neg[sectores_neg, on = "orden"]
  
  chart_final <- data.table::rbindlist(list(sec_pos_final, sec_neg_final))
  chart_final[order(-rep)]
}

.dataframe_pares_contribuciones <- function(ds = dsmad, 
                                            metap = meta_pais, 
                                            metas = meta_sec, 
                                            para = paramets, 
                                            tot = totalesanho, 
                                            reg = "mad", 
                                            flujo = "exp") {
  
  # Variables auxiliares
  totalanop <- tot[[paste0(flujo, "_prev_", reg)]]
  f_flujo   <- dplyr::if_else(flujo == "exp", 1L, 0L)
  
  # Extracción y agregación de microdatos
  df <- ds |>
    dplyr::filter(
      año == para$anho | año == para$anho - 1L,
      flujo == f_flujo,
      mes %in% para$mes
    ) |>
    dplyr::group_by(año, pais, cod_sector_economico) |>
    dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect() |>
    data.table::as.data.table()
  
  # Separación de periodos
  df_act <- df[año == para$anho]
  df_pas <- df[año == para$anho - 1L]
  
  df_pas[, `:=`(
    año = año + 1L,
    euros_prev = euros,
    euros = NULL
  )]
  
  # Unión de periodos en un único data.table base
  df_con <- merge(df_act, df_pas, by = c("año", "pais", "cod_sector_economico"), all.x = TRUE)
  df_con[is.na(euros), euros := 0]
  df_con[is.na(euros_prev), euros_prev := 0]
  
  # Agregación de regiones geográficas básicas basadas en la columna reg de países
  df_codigos_reg <- data.table::data.table(
    pais = c("ZONA EURO", "RESTO DE EUROPA", "RESTO UE", "ASIA (excl. Oriente Medio)", "ÁFRICA", 
             "AMÉRICA DEL NORTE", "AMÉRICA LATINA", "RESTO DE AMÉRICA", "ORIENTE MEDIO", "OCEANÍA", "OTROS"),
    cod  = c(1001L, 1003L, 1002L, 1007L, 1009L, 1004L, 1005L, 1006L, 1008L, 1010L, 1011L)
  )
  
  df_reg_geo <- merge(df_con, metap[, .(cod, reg)], by.x = "pais", by.y = "cod", all.x = TRUE)
  df_reg_geo <- df_reg_geo[!is.na(reg), .(
    euros      = sum(euros, na.rm = TRUE),
    euros_prev = sum(euros_prev, na.rm = TRUE)
  ), by = .(año, reg, cod_sector_economico)]
  
  df_reg_geo <- merge(df_reg_geo, df_codigos_reg, by.x = "reg", by.y = "pais", all.x = TRUE)
  df_reg_geo <- df_reg_geo[, .(año, pais = cod, cod_sector_economico, euros, euros_prev)]
  
  df_con <- data.table::rbindlist(list(df_con, df_reg_geo), use.names = TRUE, fill = TRUE)
  
  # Función auxiliar para añadir regiones compuestas utilizando órdenes de países
  añadir_fila_agregada_pais <- function(df_base, ordenes_origen, nuevo_cod) {
    df_temp <- merge(df_base, metap[, .(cod, orden)], by.x = "pais", by.y = "cod", all.x = TRUE)
    res <- df_temp[orden %in% ordenes_origen, .(
      euros      = sum(euros, na.rm = TRUE),
      euros_prev = sum(euros_prev, na.rm = TRUE)
    ), by = .(año, cod_sector_economico)]
    res[, pais := nuevo_cod]
    return(res[, .(año, pais, cod_sector_economico, euros, euros_prev)])
  }
  
  # Construcción de regiones compuestas geográficas
  f_ue27     <- añadir_fila_agregada_pais(df_con, c(3, 23), 1013L)
  f_america  <- añadir_fila_agregada_pais(df_con, c(38, 41, 47), 1014L)
  f_asia     <- añadir_fila_agregada_pais(df_con, c(49, 59), 1015L)
  df_con     <- data.table::rbindlist(list(df_con, f_ue27, f_america, f_asia), use.names = TRUE, fill = TRUE)
  
  f_europa   <- añadir_fila_agregada_pais(df_con, c(2, 31), 1012L)
  df_con     <- data.table::rbindlist(list(df_con, f_europa), use.names = TRUE, fill = TRUE)
  
  # Agregación de sectores especiales por cada país o región generada
  df_temp_sec <- merge(df_con, metas[, .(cod_sec, orden)], by.x = "cod_sector_economico", by.y = "cod_sec", all.x = TRUE)
  f_sec18 <- df_temp_sec[orden %in% c(19, 20, 21, 22, 23), .(
    euros      = sum(euros, na.rm = TRUE),
    euros_prev = sum(euros_prev, na.rm = TRUE)
  ), by = .(año, pais)]
  
  # Asignamos únicamente la clave primaria aquí para evitar conflictos en los merges
  f_sec18[, cod_sector_economico := "F18"]
  df_con <- data.table::rbindlist(list(df_con, f_sec18), use.names = TRUE, fill = TRUE)
  
  # Cruce final con los metadatos completos de países
  df_con <- merge(df_con, metap, by.x = "pais", by.y = "cod", all.x = TRUE)
  data.table::setnames(df_con, old = c("pais", "pais.y"), new = c("cod", "pais"))
  
  # Cruce final con los metadatos completos de sectores
  df_con <- merge(
    df_con, 
    metas, 
    by.x = "cod_sector_economico", 
    by.y = "cod_sec", 
    all.x = TRUE, 
    suffixes = c("", "_sec")
  )
  data.table::setnames(df_con, old = "cod_sector_economico", new = "cod_sec")
  
  # Forzamos la inyección segura de metadatos para nuestro sector sintético F18
  df_con[cod_sec == "F18", `:=`(
    orden_sec = 18L,
    nombre = "Semifacturas no químicas",
    niv_sec = 1L,
    codconnombre = "F18 - Semifacturas no químicas"
  )]
  
  # Cálculo de diferencias e indicadores macroeconómicos
  df_con[, dif := euros - euros_prev][, `:=`(
    tva = dif / euros_prev,
    rep = dif / totalanop
  )]
  
  # Selección estricta de las columnas más importantes para el reporte final
  df_final <- df_con[!is.na(nombre), .(
    año,
    orden,
    cod,
    paisconcod,
    orden_sec,
    cod_sec,
    codconnombre,
    niv_sec,
    euros,
    euros_prev,
    dif,
    tva,
    rep
  )]
  
  # Devolvemos el resultado perfectamente ordenado por su jerarquía oficial
  data.table::setorder(df_final, año, orden, orden_sec)
  
  return(df_final[])
}

.dataframe_pares_taric_contribuciones <- function(ds = dsmadt, 
                                                  metap = meta_pais, 
                                                  metat = meta_taric, 
                                                  para = paramets, 
                                                  tot = totalesanho, 
                                                  reg = "mad", 
                                                  flujo = "exp") {
  
  # Variables auxiliares
  totalanop <- tot[[paste0(flujo, "_prev_", reg)]]
  f_flujo   <- dplyr::if_else(flujo == "exp", 1L, 0L)
  
  # Extracción y agregación de microdatos
  df <- ds |>
    dplyr::filter(
      año == para$anho | año == para$anho - 1L,
      flujo == f_flujo,
      mes %in% para$mes
    ) |>
    dplyr::group_by(año, pais, cod_taric) |>
    dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect() |>
    data.table::as.data.table()
  
  # Separación de periodos
  df_act <- df[año == para$anho]
  df_pas <- df[año == para$anho - 1L]
  
  df_pas[, `:=`(
    año = año + 1L,
    euros_prev = euros,
    euros = NULL
  )]
  
  df_con <- merge(df_act, df_pas, by = c("año", "pais", "cod_taric"), all.x = TRUE)
  df_con[is.na(euros), euros := 0]
  df_con[is.na(euros_prev), euros_prev := 0]
  
  # NUEVO: Cruces con Metadatos
  setDT(df_con)
  metap_dt <- data.table::as.data.table(metap)
  metat_dt <- data.table::as.data.table(metat)
  
  df_con[, pais_num := as.numeric(pais)]
  metap_dt[, cod_num := as.numeric(cod)]
  
  df_con[metap_dt, on = .(pais_num = cod_num), `:=`(
    nombre_pais = i.pais,      
    paisconcod  = i.paisconcod,
    reg_pais    = i.reg
  )]
  
  df_con[, cod_taric_num := as.numeric(cod_taric)]
  metat_dt[, codint_taric_num := as.numeric(codint_taric)]
  
  df_con[metat_dt, on = .(cod_taric_num = codint_taric_num), `:=`(
    meta_cod_taric_chr = i.cod_taric,
    nivel_taric        = i.nivel_taric,
    NC                 = i.NC,
    Tar                = i.Tar,
    Cap                = i.Cap,
    Par                = i.Par,
    Sub                = i.Sub,
    N                  = i.N
  )]
  
  # Limpieza de columnas auxiliares generadas para los cruces
  df_con[, `:=`(pais_num = NULL, cod_taric_num = NULL)]
  df_con[, dif := euros - euros_prev][, `:=`(
    tva = dif / euros_prev,
    rep = dif / totalanop
  )]
  
  return(df_con)
  
}