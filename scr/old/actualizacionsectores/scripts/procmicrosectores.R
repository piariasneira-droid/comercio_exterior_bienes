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

##### Manipulación parquets -----
combinar_parquets <- function(folder_path, output_path, years = NULL) {
  # Obtener lista de archivos .parquet en la carpeta
  archivos_parquet <- list.files(path = folder_path, pattern = "\\.parquet$", full.names = TRUE)
  
  # Filtrar por años si se especifica el parámetro
  if (!is.null(years)) {
    # Crear patrón para filtrar archivos por años
    pattern_years <- paste0("_(", paste(years, collapse = "|"), ")_")
    archivos_parquet <- archivos_parquet[grepl(pattern_years, archivos_parquet)]
  }
  
  if (length(archivos_parquet) > 0) {
    message("Archivos encontrados: ", length(archivos_parquet))
    message("Archivos a combinar:")
    message(paste(basename(archivos_parquet), collapse = "\n"))
    
    # Leer y combinar todos los Parquets, extrayendo año y mes del nombre de fichero
    lista_dfs <- lapply(archivos_parquet, function(file) {
      df <- arrow::read_parquet(file)
      
      # Extraer año y mes del nombre del archivo con regex
      nombre <- basename(file)
      match <- stringr::str_match(nombre, "_(\\d{4})_(\\d{2})\\.parquet$")
      
      if (!is.na(match[1,1])) {
        df$año <- as.integer(match[1,2])
        df$mes <- as.integer(match[1,3])
      } else {
        df$año <- NA_integer_
        df$mes <- NA_integer_
      }
      
      return(df)
    })
    
    df_combinado <- data.table::rbindlist(lista_dfs, use.names = TRUE, fill = TRUE)
    
    # Guardar el archivo combinado
    arrow::write_parquet(df_combinado, sink = output_path)
    message("Archivo combinado guardado en: ", output_path)
    
    return(df_combinado)
  } else {
    if (!is.null(years)) {
      message("No se encontraron archivos Parquet para los años especificados: ", paste(years, collapse = ", "))
    } else {
      message("No se encontraron archivos Parquet en la carpeta especificada.")
    }
    return(NULL)
  }
}

unir_parquets <- function(input_paths, output_path) {
  # Leer todos los archivos especificados
  lista_dfs <- lapply(input_paths, function(path) {
    if (file.exists(path)) {
      arrow::read_parquet(path)
    } else {
      warning(sprintf("Archivo no encontrado: %s", path))
      NULL
    }
  })
  
  # Eliminar elementos NULL (archivos no encontrados)
  lista_dfs <- Filter(Negate(is.null), lista_dfs)
  
  if (length(lista_dfs) > 0) {
    df_combinado <- data.table::rbindlist(lista_dfs, use.names = TRUE, fill = TRUE)
    arrow::write_parquet(df_combinado, sink = output_path)
    message("Archivo combinado guardado en: ", output_path)
    return(df_combinado)
  } else {
    message("No se pudo combinar: no se encontraron archivos válidos.")
    return(NULL)
  }
}

#### Lectura csv----
read_csv_utf16_file_sectores <- function(estado = "prov", año, mes) {
  # Validar estado
  if (!estado %in% c("prov", "def")) {
    message("Estado inválido. Debe ser 'prov' o 'def'.")
    return(NULL)
  }
  
  # Validar año y mes
  if (!grepl("^\\d{4}$", año)) {
    message("Año inválido. Debe estar en formato 'aaaa'.")
    return(NULL)
  }
  if (!grepl("^\\d{2}$", mes)) {
    message("Mes inválido. Debe estar en formato 'mm'.")
    return(NULL)
  }
  
  # Construir ruta
  file_path <- file.path(
    ".", "datos", "historicosectores", estado,
    paste0("comex_sec_", año, mes, ".csv")
  )
  
  if (!file.exists(file_path)) {
    message("El archivo no existe: ", file_path)
    return(NULL)
  }
  
  message("Leyendo el archivo: ", file_path)
  
  # Leer el archivo con vroom
  df <- vroom::vroom(
    file_path,
    delim = "\t",
    col_names = TRUE,
    locale = readr::locale(encoding = "UTF-16LE"),
    col_types = readr::cols(.default = "c")
  )
  
  # Tratamiento post-lectura
  data.table::setDT(df)
  df[, c("año", "mes", "estado") := NULL]
  df[, flujo := ifelse(flujo == "E", 1L, ifelse(flujo == "I", 0L, NA_integer_))]
  
  numeric_cols_int <- c("pais", "provincia", "nivel_sector_economico")
  for (col in numeric_cols_int) {
    df[, (col) := as.integer(gsub("\\s+", "", get(col)))]
  }
  
  numeric_cols_decimal <- c("euros", "dolares")
  for (col in numeric_cols_decimal) {
    df[, (col) := as.numeric(gsub(",", ".", gsub("\\s+", "", get(col))))]
  }
  
  message("Archivo procesado. Filas: ", nrow(df))
  return(df)
}



#### Procesamiento microdatos----
##### Inicialización -----
inicializacion_completa_sectores <- function(
    fyea = 1995,
    yea = 2025,
    yeardefi = 2023,
    mes_actual = "12",
    ruta_mad_dir  = "./datos/sectores_mad",
    ruta_esp_dir  = "./datos/sectores_esp",
    ruta_final    = "./datos/totales_mad_esp",
    prov_cod      = 28L
) {
  
  message("=== INICIO DE INICIALIZACIÓN COMPLETA ===")
  # Crear carpetas de salida -
  message("Creando estructura de carpetas...")
  dir.create(file.path(ruta_mad_dir, "def"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(ruta_mad_dir, "prov"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(ruta_esp_dir, "def"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(ruta_esp_dir, "prov"), recursive = TRUE, showWarnings = FALSE)
  dir.create(ruta_final, recursive = TRUE, showWarnings = FALSE)
  
  # Bucle de procesamiento total
  message(sprintf("Procesando datos desde %s hasta %s...", fyea, yea))
  for (anio in fyea:yea) {
    est <- if (anio <= yeardefi) "def" else "prov"
    message(sprintf("  Año %s (%s)", anio, est))
    
    for (mes in 1:12) {
      mes_str <- sprintf("%02d", mes)
      procesar_mes_sectores(
        est = est,
        anio = as.character(anio),
        mesio = mes_str,
        carpeta_mad = file.path(ruta_mad_dir, est),
        carpeta_esp = file.path(ruta_esp_dir, est),
        prov_cod = prov_cod
      )
    }
  }
  
  # Uniones finales
  message("=== Iniciando uniones finales ===")
  # Microdatos Madrid
  message("1/2: Procesando microdatos Madrid...")
  
  combinar_parquets(
    folder_path = file.path(ruta_mad_dir, "def"),
    output_path = file.path(ruta_mad_dir, "total_mad_sectores_def.parquet"),
    years = fyea:yeardefi
  )
  
  combinar_parquets(
    folder_path = file.path(ruta_mad_dir, "prov"),
    output_path = file.path(ruta_mad_dir, "total_mad_sectores_prov.parquet"),
    years = (yeardefi + 1):yea
  )
  
  unir_parquets(
    input_paths = c(
      file.path(ruta_mad_dir, "total_mad_sectores_def.parquet"),
      file.path(ruta_mad_dir, "total_mad_sectores_prov.parquet")
    ),
    output_path = file.path(ruta_mad_dir, paste0("total_mad_sectores_hasta_", yea, "_", mes_actual, ".parquet"))
  )
  
  # Microdatos España
  message("2/2: Procesando microdatos España...")
  
  combinar_parquets(
    folder_path = file.path(ruta_esp_dir, "def"),
    output_path = file.path(ruta_esp_dir, "total_esp_sectores_def.parquet"),
    years = fyea:yeardefi
  )
  
  combinar_parquets(
    folder_path = file.path(ruta_esp_dir, "prov"),
    output_path = file.path(ruta_esp_dir, "total_esp_sectores_prov.parquet"),
    years = (yeardefi + 1):yea
  )
  
  unir_parquets(
    input_paths = c(
      file.path(ruta_esp_dir, "total_esp_sectores_def.parquet"),
      file.path(ruta_esp_dir, "total_esp_sectores_prov.parquet")
    ),
    output_path = file.path(ruta_esp_dir, paste0("total_esp_sectores_hasta_", yea, "_", mes_actual, ".parquet"))
  )
  
  # Separación de columnas y añadimos totales -
  message("Generando archivos finales con totales...")
  
  # Madrid - euros
  message("  - Madrid euros")
  anadir_totales_sectores_parquet(
    file.path(ruta_mad_dir, paste0("total_mad_sectores_hasta_", yea, "_", mes_actual, ".parquet")),
    file.path(ruta_final, paste0("de_mad_sectores_euros_", yea, "_", mes_actual, ".parquet")),
    columna = "euros"
  )
  
  # España - euros
  message("  - España euros")
  anadir_totales_sectores_parquet(
    file.path(ruta_esp_dir, paste0("total_esp_sectores_hasta_", yea, "_", mes_actual, ".parquet")),
    file.path(ruta_final, paste0("de_esp_sectores_euros_", yea, "_", mes_actual, ".parquet")),
    columna = "euros"
  )
  
  message("=== INICIALIZACIÓN COMPLETA FINALIZADA ===")
  invisible(TRUE)
}

##### Procesamiento mensual-----
procesar_mes_sectores <- function(est, anio, mesio, prov_cod = 28L,
                        carpeta_mad, carpeta_esp) {
  # Lectura CSV
  df <- read_csv_utf16_file_sectores(estado = est, año = anio, mes = mesio)
  if (is.null(df)) return(invisible(NULL))
  
  # Microdatos Madrid
  df_mad <- filtra_provincia_sectores(df, prov_cod)
  if (!is.null(df_mad)) {
    nombre_archivo <- sprintf("de_mad_sectores_%s_%s_%s.parquet", est, anio, mesio)
    arrow::write_parquet(df_mad, sink = file.path(carpeta_mad, nombre_archivo))
  }
  
  # Microdatos España
  df_esp <- agrega_provincias_sectores(df, provincias_a_sumar = 0L:52L, valor_provincia_suma = 99L)
  if (!is.null(df_esp)) {
    nombre_archivo <- sprintf("de_esp_sectores_%s_%s_%s.parquet", est, anio, mesio)
    arrow::write_parquet(df_esp, sink = file.path(carpeta_esp, nombre_archivo))
  }
  
  message(sprintf("Procesado %s-%s (%s)", anio, mesio, est))
}

filtra_provincia_sectores <- function(dataframe, provincia_filtro) {
  # Filtrar por provincia
  df_filtrado <- dataframe[provincia == provincia_filtro]
  
  if (nrow(df_filtrado) == 0) {
    message(sprintf("No se encontraron datos para la provincia %d.", provincia_filtro))
    return(NULL)
  }
  
  # Eliminar la columna 'provincia'
  df_filtrado[, provincia := NULL]
  
  message(sprintf("Filtrado completado. Filas encontradas: %d", nrow(df_filtrado)))
  return(df_filtrado)
}

agrega_provincias_sectores <- function(dataframe, provincias_a_sumar = 0L:52L, valor_provincia_suma = 99L){
  # Columnas para agrupar y sumar
  columnas_grupo <- c("flujo", "pais", "nivel_sector_economico", "cod_sector_economico")
  columnas_suma <- c("euros", "dolares")
  
  # Filtrar provincias
  df_filtrado <- dataframe[provincia %in% provincias_a_sumar]
  
  if (nrow(df_filtrado) == 0) {
    message("No se encontraron datos para las provincias especificadas.")
    return(NULL)
  }
  
  # Agrupar y sumar
  df_agregado <- df_filtrado[, lapply(.SD, sum, na.rm = TRUE),
                             by = columnas_grupo,
                             .SDcols = columnas_suma]
  
  # Asignar nueva provincia
  # df_agregado[, provincia := valor_provincia_suma]
  df_agregado[, provincia := NULL]
  
  message(sprintf("Agregación completada. Filas agregadas: %d", nrow(df_agregado)))
  return(df_agregado)
}

#### Funciones de actualización ----
##### Funciones de actualización mensual -----
generacion_ficheros_mes_sectores <- function(
    year, yeardef, monthss, 
    ruta_mad_salida, ruta_esp_salida) {
  
  # Determinar estado (definitivo o provisional)
  est <- if (year <= yeardef) "def" else "prov"
  mes_str <- sprintf("%02d", monthss)
  
  # Leer el archivo base
  df <- read_csv_utf16_file_sectores(estado = est, año = as.character(year), mes = mes_str)
  if (is.null(df)) {
    message("No se pudo leer el archivo. Actualización cancelada.")
    return(invisible(NULL))
  }
  
  message(sprintf("Procesando datos para %s-%s (%s)...", year, mes_str, est))
  
  
  ## === MADRID ===
  df_mad <- filtra_provincia_sectores(df, provincia_filtro = 28L)
  if (!is.null(df_mad)) {
    carpeta_mad_est <- file.path(ruta_mad_salida, est)
    dir.create(carpeta_mad_est, showWarnings = FALSE, recursive = TRUE)
    
    nombre_archivo_mad <- sprintf("de_mad_sectores_%s_%s_%s.parquet", est, year, mes_str)
    ruta_completa_mad <- file.path(carpeta_mad_est, nombre_archivo_mad)
    
    arrow::write_parquet(df_mad, sink = ruta_completa_mad)
    message("Archivo Madrid mensual guardado: ", ruta_completa_mad)
  }
  gc(verbose = FALSE)
  
  ## === ESPAÑA ===
  df_esp <- agrega_provincias_sectores(df, provincias_a_sumar = 0L:52L, valor_provincia_suma = 99L)
  if (!is.null(df_esp)) {
    carpeta_esp_est <- file.path(ruta_esp_salida, est)
    dir.create(carpeta_esp_est, showWarnings = FALSE, recursive = TRUE)
    
    nombre_archivo_esp <- sprintf("de_esp_sectores_%s_%s_%s.parquet", est, year, mes_str)
    ruta_completa_esp <- file.path(carpeta_esp_est, nombre_archivo_esp)
    
    arrow::write_parquet(df_esp, sink = ruta_completa_esp)
    message("Archivo España mensual guardado: ", ruta_completa_esp)
  }
  gc(verbose = FALSE)
  
  message(sprintf("=== Generación de archivos mensuales finalizada para %s-%s (%s) ===", year, mes_str, est))
}

actualizacion_sectores_mensual_totales_mad_esp <- function(
    ruta_mad_mes, ruta_esp_mes,
    ruta_mad_euros_entrada, ruta_esp_euros_entrada, 
    ruta_mad_euros_salida, ruta_esp_euros_salida,
    year, mes) {
  
  message("=== Iniciando actualización de totales MAD/ESP ===")
  
  # Leer datos del mes actual
  df_mad_mes <- as.data.table(arrow::read_parquet(ruta_mad_mes))
  df_esp_mes <- as.data.table(arrow::read_parquet(ruta_esp_mes))
  
  # Añadir año y mes si no existen
  if (!"año" %in% names(df_mad_mes)) df_mad_mes[, año := as.integer(year)]
  if (!"mes" %in% names(df_mad_mes)) df_mad_mes[, mes := as.integer(mes)]
  if (!"año" %in% names(df_esp_mes)) df_esp_mes[, año := as.integer(year)]
  if (!"mes" %in% names(df_esp_mes)) df_esp_mes[, mes := as.integer(mes)]
  
  # --- Madrid: euros ---
  message("Procesando Madrid (euros)...")
  fixed_cols <- c("flujo", "año", "mes", "pais", "nivel_sector_economico", "cod_sector_economico")
  select_cols_euros <- c(fixed_cols, "euros")
  
  df_mad_mes_euros <- df_mad_mes[, ..select_cols_euros]
  df_mad_euros <- calculo_totales_y_subtotales_sectores_dt(df_mad_mes_euros, columna = "euros")
  
  if (file.exists(ruta_mad_euros_entrada)) {
    df_mad_ant <- as.data.table(arrow::read_parquet(ruta_mad_euros_entrada))
    df_mad_union <- rbindlist(list(df_mad_ant, df_mad_euros), use.names = TRUE, fill = TRUE)
  } else {
    df_mad_union <- df_mad_euros
  }
  
  setorder(df_mad_union, año, mes)
  arrow::write_parquet(df_mad_union, ruta_mad_euros_salida)
  message("  [OK] Madrid euros actualizado -> ", ruta_mad_euros_salida)
  
  # --- España: euros ---
  message("Procesando España (euros)...")
  df_esp_mes_euros <- df_esp_mes[, ..select_cols_euros]
  df_esp_euros <- calculo_totales_y_subtotales_sectores_dt(df_esp_mes_euros, columna = "euros")
  
  if (file.exists(ruta_esp_euros_entrada)) {
    df_esp_ant <- as.data.table(arrow::read_parquet(ruta_esp_euros_entrada))
    df_esp_union <- rbindlist(list(df_esp_ant, df_esp_euros), use.names = TRUE, fill = TRUE)
  } else {
    df_esp_union <- df_esp_euros
  }
  
  setorder(df_esp_union, año, mes)
  arrow::write_parquet(df_esp_union, ruta_esp_euros_salida)
  message("  [OK] España euros actualizado -> ", ruta_esp_euros_salida)
  
  message("=== Actualización completada correctamente ===")
}

actualizacion_sectores_mensual_datos_provisionales <- function(
    yea,
    yeardefi,
    mon,
    ruta_mad_dir  = "./datos/total_mad",
    ruta_esp_dir  = "./datos/total_esp",
    ruta_final    = "./datos/totales_mad_esp",
    ruta_mad_euros_salida = NULL,
    ruta_esp_euros_salida = NULL) {
  
  message("=== INICIO DE ACTUALIZACIÓN MENSUAL DATOS PROVISIONALES ===")
  
  # Calcular mes anterior y actual
  if (mon == 1) {
    mes_formateado <- "12"
    año_fichero_origen <- yea - 1
  } else {
    mes_formateado <- sprintf("%02d", mon - 1)
    año_fichero_origen <- yea
  }
  mes_actual <- sprintf("%02d", mon)
  
  message(sprintf("Procesando actualización para %s-%s (origen %s-%s)", yea, mes_actual, año_fichero_origen, mes_formateado))
  
  # Crear carpeta de salida si no existe
  dir.create(ruta_final, showWarnings = FALSE, recursive = TRUE)
  
  ## === 0. Renombrar archivos existentes si están presentes ===
  message("Paso 0/2: Verificando y renombrando archivos existentes...")
  
  archivos_a_renombrar <- list(
    list(
      actual = file.path(ruta_final, "de_mad_sectores_euros.parquet"),
      nuevo  = file.path(ruta_final, paste0("de_mad_sectores_euros_", año_fichero_origen, "_", mes_formateado, ".parquet"))
    ),
    list(
      actual = file.path(ruta_final, "de_esp_sectores_euros.parquet"),
      nuevo  = file.path(ruta_final, paste0("de_esp_sectores_euros_", año_fichero_origen, "_", mes_formateado, ".parquet"))
    )
  )
  
  for (archivo in archivos_a_renombrar) {
    if (file.exists(archivo$actual)) {
      message(sprintf("  Renombrando: %s -> %s", basename(archivo$actual), basename(archivo$nuevo)))
      file.rename(archivo$actual, archivo$nuevo)
    }
  }
  
  ## === 1. Generación de ficheros mensuales ===
  message("Paso 1/2: Generación de ficheros mensuales...")
  generacion_ficheros_mes_sectores(
    year    = yea,
    yeardef = yeardefi,
    monthss = mon,
    
    # Madrid (solo genera archivo mensual)
    ruta_mad_salida = ruta_mad_dir,
    
    # España (solo genera archivo mensual)
    ruta_esp_salida = ruta_esp_dir
  )
  
  gc(verbose = FALSE)
  
  ## === 2. Actualización de totales MAD/ESP ===
  message("Paso 2/2: Unión dinámica de totales MAD/ESP...")
  
  # Establecer valores por defecto solo si no se proporcionan
  if (is.null(ruta_mad_euros_salida)) {
    ruta_mad_euros_salida <- file.path(ruta_final, "de_mad_sectores_euros.parquet")
  }
  if (is.null(ruta_esp_euros_salida)) {
    ruta_esp_euros_salida <- file.path(ruta_final, "de_esp_sectores_euros.parquet")
  }
  
  actualizacion_sectores_mensual_totales_mad_esp(
    # Nuevos ficheros del mes actual (provisionales)
    ruta_mad_mes = file.path(ruta_mad_dir, "prov", paste0("de_mad_sectores_prov_", yea, "_", mes_actual, ".parquet")),
    ruta_esp_mes = file.path(ruta_esp_dir, "prov", paste0("de_esp_sectores_prov_", yea, "_", mes_actual, ".parquet")),
    
    # Entradas: totales del mes anterior (de referencia)
    ruta_mad_euros_entrada = file.path(ruta_final, paste0("de_mad_sectores_euros_", año_fichero_origen, "_", mes_formateado, ".parquet")),
    ruta_esp_euros_entrada = file.path(ruta_final, paste0("de_esp_sectores_euros_", año_fichero_origen, "_", mes_formateado, ".parquet")),
    
    # Salidas actualizadas (se pasan tal cual, YA vienen con ruta completa)
    ruta_mad_euros_salida = ruta_mad_euros_salida,
    ruta_esp_euros_salida = ruta_esp_euros_salida,
    
    # Año y mes del nuevo dato
    year = yea,
    mes = mon
  )
  
  message("=== ACTUALIZACIÓN MENSUAL FINALIZADA CORRECTAMENTE ===")
  invisible(TRUE)
}

#### Calculo totales y subtotales -----
anadir_totales_sectores_parquet <- function(path_entrada, path_salida, columna = "euros", 
                                   fixed_cols = c("flujo", "año", "mes", "pais", "nivel_sector_economico", "cod_sector_economico")) {
  
  select_cols <- c(fixed_cols, columna)
  
  # Leer parquet con columnas seleccionadas
  df <- as.data.table(read_parquet(path_entrada))
  df <- df[, ..select_cols]
  
  # Calcular totales y subtotales
  df_resultado <- calculo_totales_y_subtotales_sectores_dt(df, columna = columna)
  
  # Guardar en parquet
  write_parquet(df_resultado, path_salida)
  message(paste0("Proceso completado. El archivo procesado se ha guardado en: ", path_salida, 
                 " usando la columna '", columna, "'."))
  
  return(df_resultado)
}

calculo_totales_y_subtotales_sectores_dt <- function(df_input, columna = "euros") {
  df_processed <- copy(df_input)
  setDT(df_processed)
  
  # Pass the 'columna' argument to all sub-functions
  df_total <- calcular_totales_sectores_dt(df_processed, columna = columna)
  df_subpais <- calcular_subtotal_sectores_pais_dt(df_processed, columna = columna)
  df_subtaric <- calcular_subtotal_sectores_taric_dt(df_processed, columna = columna)
  
  combined_df <- rbindlist(list(df_processed, df_total, df_subpais, df_subtaric), use.names = TRUE, fill = TRUE)
  return(combined_df)
}

calcular_totales_sectores_dt <- function(df, columna = "euros") {
  setDT(df)
  
  totales <- df[nivel_sector_economico == 1L,
                .(valor_sumado = sum(get(columna), na.rm = TRUE)),
                by = .(flujo, año, mes)]
  totales[, ':=' (pais = 0L, cod_sector_economico = "0", nivel_sector_economico =0L)]
  setnames(totales, "valor_sumado", columna)
  return(totales)
}

calcular_subtotal_sectores_pais_dt <- function(df, columna = "euros") {
  setDT(df)
  
  subtotal <- df[nivel_sector_economico == 1,
                 .(valor_sumado = sum(get(columna), na.rm = TRUE)),
                 by = .(flujo, año, mes, pais)]
  subtotal[, ':=' (cod_sector_economico = "0", nivel_sector_economico =0L)]
  setnames(subtotal, "valor_sumado", columna)
  return(subtotal)
}

calcular_subtotal_sectores_taric_dt <- function(df, columna = "euros") {
  setDT(df)
  
  subtotal <- df[, .(valor_sumado = sum(get(columna), na.rm = TRUE)),
                 by = .(flujo, año, mes, cod_sector_economico, nivel_sector_economico)]
  subtotal[, pais := 0L]
  setnames(subtotal, "valor_sumado", columna)
  return(subtotal)
}