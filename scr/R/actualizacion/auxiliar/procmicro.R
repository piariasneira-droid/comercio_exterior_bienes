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

##### Manipulación csvs -----
combinar_csvs <- function(folder_path, output_path, years = NULL) {
  # Obtener lista de archivos CSV en la carpeta
  archivos_csv <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Filtrar por años si se especifica el parámetro
  if (!is.null(years)) {
    # Crear patrón para filtrar archivos por años
    pattern_years <- paste0("_(", paste(years, collapse = "|"), ")_")
    archivos_csv <- archivos_csv[grepl(pattern_years, archivos_csv)]
  }
  
  # Leer y combinar todos los CSVs
  if (length(archivos_csv) > 0) {
    message("Archivos encontrados: ", length(archivos_csv))
    message("Archivos a combinar:")
    message(paste(basename(archivos_csv), collapse = "\n"))
    
    lista_dfs <- lapply(archivos_csv, fread)
    df_combinado <- rbindlist(lista_dfs, use.names = TRUE, fill = TRUE)
    
    # Guardar el archivo combinado
    fwrite(df_combinado, file = output_path)
    message("Archivo combinado guardado en: ", output_path)
    
    return(df_combinado)
  } else {
    if (!is.null(years)) {
      message("No se encontraron archivos CSV para los años especificados: ", paste(years, collapse = ", "))
    } else {
      message("No se encontraron archivos CSV en la carpeta especificada.")
    }
    return(NULL)
  }
}

unir_csvs <- function(input_paths, output_path) {
  # Leer todos los archivos especificados
  lista_dfs <- lapply(input_paths, function(path) {
    if (file.exists(path)) {
      fread(path)
    } else {
      warning(sprintf("Archivo no encontrado: %s", path))
      NULL
    }
  })
  
  # Eliminar elementos NULL (archivos no encontrados)
  lista_dfs <- Filter(Negate(is.null), lista_dfs)
  
  # Combinar si hay al menos un archivo válido
  if (length(lista_dfs) > 0) {
    df_combinado <- rbindlist(lista_dfs, use.names = TRUE, fill = TRUE)
    fwrite(df_combinado, file = output_path)
    message("Archivo combinado guardado en: ", output_path)
    return(df_combinado)
  } else {
    message("No se pudo combinar: no se encontraron archivos válidos.")
    return(NULL)
  }
}

unir_csv_si_subfila_presente <- function(csv_origen, csv_a_cargar, csv_salida) {
  library(data.table)
  
  # Leer los archivos
  df_origen <- fread(csv_origen)
  df_nuevo <- fread(csv_a_cargar)
  
  # Verificar si la columna 'subfila' existe en ambos
  if (!("subfila" %in% names(df_origen)) || !("subfila" %in% names(df_nuevo))) {
    stop("Ambos archivos deben contener la columna 'subfila'.")
  }
  
  # Verificar que el archivo nuevo tenga solo un valor único en 'subfila'
  subfila_nueva <- unique(df_nuevo$subfila)
  
  if (length(subfila_nueva) != 1) {
    stop("El archivo a cargar debe contener exactamente un valor único en la columna 'subfila'.")
  }
  
  # Comprobar si ese valor ya está presente en el archivo original
  if (subfila_nueva %in% df_origen$subfila) {
    message(sprintf("El fichero con subfila '%s' ya está cargado. No se realizó la unión.", subfila_nueva))
    return(NULL)
  } else {
    # Unir los datos
    df_combinado <- rbindlist(list(df_origen, df_nuevo), use.names = TRUE, fill = TRUE)
    fwrite(df_combinado, file = csv_salida)
    message("Archivo combinado guardado en: ", csv_salida)
    return(df_combinado)
  }
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

unir_parquets_actualizacion <- function(fichero_origen, fichero_a_actualizar, fichero_salida) {
  # Leer archivo origen si existe
  df_origen <- if (file.exists(fichero_origen)) {
    arrow::read_parquet(fichero_origen)
  } else {
    warning(sprintf("Archivo origen no encontrado: %s", fichero_origen))
    NULL
  }
  
  # Leer archivo a actualizar si existe
  if (!file.exists(fichero_a_actualizar)) {
    warning(sprintf("Archivo a actualizar no encontrado: %s", fichero_a_actualizar))
    return(NULL)
  }
  df_actualizar <- arrow::read_parquet(fichero_a_actualizar)
  
  # Extraer año y mes del nombre del archivo
  patron <- ".*_(\\d{4})_(\\d{2})\\.parquet$"
  matches <- regmatches(fichero_a_actualizar, regexec(patron, fichero_a_actualizar))[[1]]
  if (length(matches) == 3) {
    año <- as.integer(matches[2])
    mes <- as.integer(matches[3])
  } else {
    stop("No se pudo extraer año y mes del nombre del archivo: ", fichero_a_actualizar)
  }
  
  # Añadir columnas año y mes
  df_actualizar[, año := año]
  df_actualizar[, mes := mes]
  
  # Combinar con origen
  lista_dfs <- Filter(Negate(is.null), list(df_origen, df_actualizar))
  df_combinado <- data.table::rbindlist(lista_dfs, use.names = TRUE, fill = TRUE)
  
  # Guardar resultado
  arrow::write_parquet(df_combinado, sink = fichero_salida)
  message("Archivo combinado guardado en: ", fichero_salida)
  
  return(df_combinado)
}


#### Lectura csv----
read_csv_utf16_file <- function(estado = "prov", año, mes) {
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
    ".", "datos", "historico", estado,
    paste0("comex_taric_", año, mes, ".csv")
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
  
  numeric_cols_int <- c("pais", "provincia", "nivel_taric")
  for (col in numeric_cols_int) {
    df[, (col) := as.integer(gsub("\\s+", "", get(col)))]
  }
  
  numeric_cols_decimal <- c("cod_taric", "euros", "dolares", "kilogramos")
  for (col in numeric_cols_decimal) {
    df[, (col) := as.numeric(gsub(",", ".", gsub("\\s+", "", get(col))))]
  }
  
  message("Archivo procesado. Filas: ", nrow(df))
  return(df)
}

#### Procesamiento microdatos----
##### Inicialización -----
inicializacion_completa <- function(
    fyea = 1995,
    yea = 2025,
    yeardefi = 2023,
    mes_actual = "12",
    ruta_ccaa_dir = "./datos/total_ccaas",
    ruta_mad_dir  = "./datos/total_mad",
    ruta_esp_dir  = "./datos/total_esp",
    ruta_final    = "./datos/totales_mad_esp",
    prov_cod      = 28L
) {
  
  message("=== INICIO DE INICIALIZACIÓN COMPLETA ===")
  # Crear carpetas de salida -
  message("Creando estructura de carpetas...")
  dir.create(file.path(ruta_ccaa_dir, "def"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(ruta_ccaa_dir, "prov"), recursive = TRUE, showWarnings = FALSE)
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
      procesar_mes(
        est = est,
        anio = as.character(anio),
        mesio = mes_str,
        carpeta_ccaa = file.path(ruta_ccaa_dir, est),
        carpeta_mad = file.path(ruta_mad_dir, est),
        carpeta_esp = file.path(ruta_esp_dir, est),
        prov_cod = prov_cod
      )
    }
  }
  
  # Uniones finales
  message("=== Iniciando uniones finales ===")
  
  # Totales CCAA
  message("1/3: Procesando totales CCAA...")
  
  # Combinar archivos definitivos
  combinar_csvs(
    folder_path = file.path(ruta_ccaa_dir, "def"),
    output_path = file.path(ruta_ccaa_dir, "total_ccaa_def.csv"),
    years = fyea:yeardefi
  )
  
  # Combinar archivos provisionales
  combinar_csvs(
    folder_path = file.path(ruta_ccaa_dir, "prov"),
    output_path = file.path(ruta_ccaa_dir, "total_ccaa_prov.csv"),
    years = (yeardefi + 1):yea
  )
  
  # Unir los dos archivos combinados
  unir_csvs(
    input_paths = c(
      file.path(ruta_ccaa_dir, "total_ccaa_def.csv"),
      file.path(ruta_ccaa_dir, "total_ccaa_prov.csv")
    ),
    output_path = file.path(ruta_ccaa_dir, paste0("total_ccaa_hasta_", yea, "_", mes_actual, ".csv"))
  )
  
  # Microdatos Madrid
  message("2/3: Procesando microdatos Madrid...")
  
  combinar_parquets(
    folder_path = file.path(ruta_mad_dir, "def"),
    output_path = file.path(ruta_mad_dir, "total_mad_def.parquet"),
    years = fyea:yeardefi
  )
  
  combinar_parquets(
    folder_path = file.path(ruta_mad_dir, "prov"),
    output_path = file.path(ruta_mad_dir, "total_mad_prov.parquet"),
    years = (yeardefi + 1):yea
  )
  
  unir_parquets(
    input_paths = c(
      file.path(ruta_mad_dir, "total_mad_def.parquet"),
      file.path(ruta_mad_dir, "total_mad_prov.parquet")
    ),
    output_path = file.path(ruta_mad_dir, paste0("total_mad_hasta_", yea, "_", mes_actual, ".parquet"))
  )
  
  # Microdatos España
  message("3/3: Procesando microdatos España...")
  
  combinar_parquets(
    folder_path = file.path(ruta_esp_dir, "def"),
    output_path = file.path(ruta_esp_dir, "total_esp_def.parquet"),
    years = fyea:yeardefi
  )
  
  combinar_parquets(
    folder_path = file.path(ruta_esp_dir, "prov"),
    output_path = file.path(ruta_esp_dir, "total_esp_prov.parquet"),
    years = (yeardefi + 1):yea
  )
  
  unir_parquets(
    input_paths = c(
      file.path(ruta_esp_dir, "total_esp_def.parquet"),
      file.path(ruta_esp_dir, "total_esp_prov.parquet")
    ),
    output_path = file.path(ruta_esp_dir, paste0("total_esp_hasta_", yea, "_", mes_actual, ".parquet"))
  )
  
  # Separación de columnas y añadimos totales -
  message("Generando archivos finales con totales...")
  
  # Madrid - euros
  message("  - Madrid euros")
  anadir_totales_parquet(
    file.path(ruta_mad_dir, paste0("total_mad_hasta_", yea, "_", mes_actual, ".parquet")),
    file.path(ruta_final, paste0("de_mad_euros_", yea, "_", mes_actual, ".parquet")),
    columna = "euros"
  )
  
  # Madrid - kilogramos
  message("  - Madrid kilogramos")
  anadir_totales_parquet(
    file.path(ruta_mad_dir, paste0("total_mad_hasta_", yea, "_", mes_actual, ".parquet")),
    file.path(ruta_final, paste0("de_mad_kg_", yea, "_", mes_actual, ".parquet")),
    columna = "kilogramos"
  )
  
  # España - euros
  message("  - España euros")
  anadir_totales_parquet(
    file.path(ruta_esp_dir, paste0("total_esp_hasta_", yea, "_", mes_actual, ".parquet")),
    file.path(ruta_final, paste0("de_esp_euros_", yea, "_", mes_actual, ".parquet")),
    columna = "euros"
  )
  
  # España - kilogramos
  message("  - España kilogramos")
  anadir_totales_parquet(
    file.path(ruta_esp_dir, paste0("total_esp_hasta_", yea, "_", mes_actual, ".parquet")),
    file.path(ruta_final, paste0("de_esp_kg_", yea, "_", mes_actual, ".parquet")),
    columna = "kilogramos"
  )
  
  message("=== INICIALIZACIÓN COMPLETA FINALIZADA ===")
  invisible(TRUE)
}

##### Procesamiento mensual-----
procesar_mes <- function(est, anio, mesio, prov_cod = 28L,
                         carpeta_ccaa, carpeta_mad, carpeta_esp) {
  # Lectura CSV
  df <- read_csv_utf16_file(estado = est, año = anio, mes = mesio)
  if (is.null(df)) return(invisible(NULL))
  
  #Totales CCAA
  df_ccaa <- totales_ccaas(df)
  if (!is.null(df_ccaa)) {
    df_ccaa[, c("dolares", "cod_comunidad", "kilogramos") := NULL]

    # Subfila con mes y año
    meses_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                  "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
    mes_nombre <- meses_es[as.integer(mesio)]
    df_ccaa[, subfila := paste(mes_nombre, "de", anio)]

    # Reordenar columnas
    setcolorder(df_ccaa, c("fila", "subfila", "columna", "subcolumna", "valor"))

    # Guardar CSV
    nombre_archivo <- sprintf("totales_ccaa_%s_%s_%s.csv", est, anio, mesio)
    fwrite(df_ccaa, file = file.path(carpeta_ccaa, nombre_archivo))
  }
  
  # Microdatos Madrid
  df_mad <- filtra_provincia(df, prov_cod)
  if (!is.null(df_mad)) {
    nombre_archivo <- sprintf("de_mad_%s_%s_%s.parquet", est, anio, mesio)
    arrow::write_parquet(df_mad, sink = file.path(carpeta_mad, nombre_archivo))
  }
  
  # Microdatos España
  df_esp <- agrega_provincias(df, provincias_a_sumar = 0L:52L, valor_provincia_suma = 99L)
  if (!is.null(df_esp)) {
    nombre_archivo <- sprintf("de_esp_%s_%s_%s.parquet", est, anio, mesio)
    arrow::write_parquet(df_esp, sink = file.path(carpeta_esp, nombre_archivo))
  }
  
  message(sprintf("Procesado %s-%s (%s)", anio, mesio, est))
}

totales_ccaas <- function(dataframe) {
  # Mapeo de provincia a comunidad autónoma (todo como string)
  mapa_prov_ccaa <- data.table::data.table(
    cod_provincia = as.character(0:52),
    cod_comunidad = c(
      "0", "14", "7", "13", "1", "6", "9", "4", "8", "6", "9", "1", "13", "7", "1", "10", "7", "8", "1", "7",
      "14", "1", "2", "1", "6", "8", "16", "10", "15", "1", "11", "12", "10", "3", "6", "5", "10", "6", "5",
      "17", "6", "1", "6", "8", "2", "7", "13", "6", "14", "6", "2", "18", "19"
    )
  )
  
  # Mapeo de cod_comunidad a nombre (todo como string)
  mapa_nombre_ccaa <- data.table::data.table(
    cod_comunidad = c(as.character(0:19), "99"),
    fila = c(
      "No determinado", "Andalucía", "Aragón", "Asturias, Principado de", "Balears, Illes",
      "Canarias", "Castilla y León", "Castilla-La Mancha", "Cataluña", "Extremadura",
      "Galicia", "Murcia, Región de", "Navarra, Comunidad Foral de", "Comunitat Valenciana",
      "País Vasco", "Madrid, Comunidad de", "Rioja, La", "Cantabria", "Ceuta", "Melilla",
      "Total seleccionado"
    )
  )
  
  # Filtrar por nivel_taric == 1
  df_filtrado <- dataframe[nivel_taric == 1]
  
  if (nrow(df_filtrado) == 0) {
    message("No hay datos con nivel_taric == 1.")
    return(NULL)
  }
  
  # Convertir provincia a string para el merge
  df_filtrado[, provincia := as.character(provincia)]
  
  # Unir el mapeo al dataframe
  df_filtrado <- merge(df_filtrado, mapa_prov_ccaa, by.x = "provincia", by.y = "cod_provincia", all.x = TRUE)
  
  # Agregación por CCAA
  columnas_suma <- c("euros", "dolares", "kilogramos")
  df_ccaa <- df_filtrado[, lapply(.SD, sum, na.rm = TRUE),
                         by = .(flujo, cod_comunidad),
                         .SDcols = columnas_suma]
  
  # Agregación total nacional por flujo
  df_total <- df_ccaa[, lapply(.SD, sum, na.rm = TRUE), by = flujo, .SDcols = columnas_suma]
  df_total[, cod_comunidad := "99"]  # String para total nacional
  
  # Combinar resultados
  df_final <- data.table::rbindlist(list(df_ccaa, df_total), use.names = TRUE)
  df_final[, cod_comunidad := as.character(cod_comunidad)]
  df_final <- merge(df_final, mapa_nombre_ccaa, by = "cod_comunidad", all.x = TRUE)
  df_final[, columna := fifelse(flujo == 1L, "EXPORT", "IMPORT")]
  df_final[, subcolumna := ""]  
  df_final[, valor := euros]
  
  # Seleccionar y reordenar columnas finales
  df_final <- df_final[, .(columna, subcolumna, valor, fila, cod_comunidad, dolares, kilogramos)]
  data.table::setorder(df_final, columna, cod_comunidad)
  
  message(sprintf("Agregación por CCAA y total nacional completada. Filas resultantes: %d", nrow(df_final)))
  return(df_final)
}

filtra_provincia <- function(dataframe, provincia_filtro) {
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

agrega_provincias <- function(dataframe, provincias_a_sumar = 0L:52L, valor_provincia_suma = 99L){
  # Columnas para agrupar y sumar
  columnas_grupo <- c("flujo", "pais", "nivel_taric", "cod_taric")
  columnas_suma <- c("euros", "dolares", "kilogramos")
  
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

##### Procesamiento totales CCAAs-----
procesamiento_ccaa_mes <- function(est, anio, mesio) {
  # Leer el archivo CSV
  df <- read_csv_utf16_file(estado = est, año = anio, mes = mesio)
  if (is.null(df)) return(NULL)
  
  # Calculo df comunidades autónomas
  df_ccaa <- totales_ccaas(dataframe = df)
  
  # Eliminar columnas innecesarias
  df_ccaa[, c("dolares", "cod_comunidad", "kilogramos") := NULL]
  
  # Crear columna subfila con el nombre del mes en español y el año
  meses_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  mes_nombre <- meses_es[as.integer(mesio)]
  df_ccaa[, subfila := paste(mes_nombre, "de", anio)]
  
  # Reordenar columnas
  setcolorder(df_ccaa, c("fila", "subfila", "columna", "subcolumna", "valor"))
  
  return(df_ccaa)
}

guardar_csvs_total_ccaa <- function(carpeta_salida,
                                    ultimo_anio_def = 2023,
                                    anios = 1995:2025,
                                    meses = 1:12) {
  
  dir.create(carpeta_salida, showWarnings = FALSE, recursive = TRUE)
  
  for (anio in anios) {
    est <- if (anio <= ultimo_anio_def) "def" else "prov"
    
    for (mes in meses) {
      mes_str <- sprintf("%02d", mes)
      df_mes <- procesamiento_ccaa_mes(est = est, anio = as.character(anio), mesio = mes_str)
      
      if (!is.null(df_mes)) {
        nombre_archivo <- sprintf("totales_ccaa_%s_%s_%s.csv", est, anio, mes_str)
        ruta_completa <- file.path(carpeta_salida, nombre_archivo)
        fwrite(df_mes, file = ruta_completa)
      }
    }
  }
  
  message("Todos los archivos han sido guardados en: ", carpeta_salida)
}

##### Procesamiento microdatos Madrid-----
procesamiento_microdatos_mad <- function(est, anio, mesio) {
  # Leer el archivo CSV
  df <- read_csv_utf16_file(estado = est, año = anio, mes = mesio)
  if (is.null(df)) return(NULL)
  
  df_mad <- filtra_provincia(
    dataframe = df, 
    provincia_filtro = 28L)
  
  return(df_mad)
}

guardar_parquets_mad <- function(carpeta_salida,
                                 ultimo_anio_def = 2023,
                                 anios = 1995:2025,
                                 meses = 1:12) {
  
  dir.create(carpeta_salida, showWarnings = FALSE, recursive = TRUE)
  
  for (anio in anios) {
    est <- if (anio <= ultimo_anio_def) "def" else "prov"
    
    for (mes in meses) {
      mes_str <- sprintf("%02d", mes)
      df_mes <- procesamiento_microdatos_mad(est = est, anio = as.character(anio), mesio = mes_str)
      
      if (!is.null(df_mes)) {
        nombre_archivo <- sprintf("de_mad_%s_%s_%s.parquet", est, anio, mes_str)
        ruta_completa <- file.path(carpeta_salida, nombre_archivo)
        arrow::write_parquet(df_mes, sink = ruta_completa)
      }
    }
  }
  
  message("Todos los archivos Parquet han sido guardados en: ", carpeta_salida)
}

##### Procesamiento microdatos España-----
procesamiento_microdatos_esp <- function(est, anio, mesio) {
  # Leer el archivo CSV
  df <- read_csv_utf16_file(estado = est, año = anio, mes = mesio)
  if (is.null(df)) return(NULL)
  
  df_esp <- agrega_provincias(
    dataframe = df, 
    provincias_a_sumar = 0L:52L, 
    valor_provincia_suma = 99L)
  
  return(df_esp)
}

guardar_parquets_esp <- function(carpeta_salida,
                                 ultimo_anio_def = 2023,
                                 anios = 1995:2025,
                                 meses = 1:12) {
  
  dir.create(carpeta_salida, showWarnings = FALSE, recursive = TRUE)
  
  for (anio in anios) {
    est <- if (anio <= ultimo_anio_def) "def" else "prov"
    
    for (mes in meses) {
      mes_str <- sprintf("%02d", mes)
      df_mes <- procesamiento_microdatos_esp(est = est, anio = as.character(anio), mesio = mes_str)
      
      if (!is.null(df_mes)) {
        nombre_archivo <- sprintf("de_esp_%s_%s_%s.parquet", est, anio, mes_str)
        ruta_completa <- file.path(carpeta_salida, nombre_archivo)
        arrow::write_parquet(df_mes, sink = ruta_completa)
      }
    }
  }
  
  message("Todos los archivos Parquet han sido guardados en: ", carpeta_salida)
}

#### Funciones de actualización ----
##### Funciones de actualización mensual -----
generacion_ficheros_mes <- function(
    year, yeardef, monthss, 
    ruta_ccaa_salida, ruta_ccaa_origen, ruta_ccaa_final,
    ruta_mad_salida, ruta_esp_salida) {
  
  # Determinar estado (definitivo o provisional)
  est <- if (year <= yeardef) "def" else "prov"
  mes_str <- sprintf("%02d", monthss)
  
  # Leer el archivo base
  df <- read_csv_utf16_file(estado = est, año = as.character(year), mes = mes_str)
  if (is.null(df)) {
    message("No se pudo leer el archivo. Actualización cancelada.")
    return(invisible(NULL))
  }
  
  message(sprintf("Procesando datos para %s-%s (%s)...", year, mes_str, est))
  
  ## === CCAA ===
  df_ccaa <- totales_ccaas(df)
  if (!is.null(df_ccaa)) {
    df_ccaa[, c("dolares", "cod_comunidad", "kilogramos") := NULL]
    
    # Añadir etiqueta de subfila (mes/año)
    meses_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                  "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
    mes_nombre <- meses_es[as.integer(mes_str)]
    df_ccaa[, subfila := paste(mes_nombre, "de", year)]
    
    setcolorder(df_ccaa, c("fila", "subfila", "columna", "subcolumna", "valor"))
    
    # Guardar archivo mensual
    carpeta_ccaa_est <- file.path(ruta_ccaa_salida, est)
    dir.create(carpeta_ccaa_est, showWarnings = FALSE, recursive = TRUE)
    nombre_archivo_ccaa <- sprintf("totales_ccaa_%s_%s_%s.csv", est, year, mes_str)
    ruta_completa_ccaa <- file.path(carpeta_ccaa_est, nombre_archivo_ccaa)
    
    fwrite(df_ccaa, file = ruta_completa_ccaa)
    message("Archivo CCAA mensual guardado: ", ruta_completa_ccaa)
    
    # Unir con histórico
    unir_csv_si_subfila_presente(
      csv_origen = ruta_ccaa_origen,
      csv_a_cargar = ruta_completa_ccaa,
      csv_salida = ruta_ccaa_final
    )
  }
  gc(verbose = FALSE)
  
  ## === MADRID ===
  df_mad <- filtra_provincia(df, provincia_filtro = 28L)
  if (!is.null(df_mad)) {
    carpeta_mad_est <- file.path(ruta_mad_salida, est)
    dir.create(carpeta_mad_est, showWarnings = FALSE, recursive = TRUE)
    
    nombre_archivo_mad <- sprintf("de_mad_%s_%s_%s.parquet", est, year, mes_str)
    ruta_completa_mad <- file.path(carpeta_mad_est, nombre_archivo_mad)
    
    arrow::write_parquet(df_mad, sink = ruta_completa_mad)
    message("Archivo Madrid mensual guardado: ", ruta_completa_mad)
  }
  gc(verbose = FALSE)
  
  ## === ESPAÑA ===
  df_esp <- agrega_provincias(df, provincias_a_sumar = 0L:52L, valor_provincia_suma = 99L)
  if (!is.null(df_esp)) {
    carpeta_esp_est <- file.path(ruta_esp_salida, est)
    dir.create(carpeta_esp_est, showWarnings = FALSE, recursive = TRUE)
    
    nombre_archivo_esp <- sprintf("de_esp_%s_%s_%s.parquet", est, year, mes_str)
    ruta_completa_esp <- file.path(carpeta_esp_est, nombre_archivo_esp)
    
    arrow::write_parquet(df_esp, sink = ruta_completa_esp)
    message("Archivo España mensual guardado: ", ruta_completa_esp)
  }
  gc(verbose = FALSE)
  
  message(sprintf("=== Generación de archivos mensuales finalizada para %s-%s (%s) ===", year, mes_str, est))
}

actualizacion_mensual_totales_mad_esp <- function(
    ruta_mad_mes, ruta_esp_mes,
    ruta_mad_euros_entrada, ruta_mad_kg_entrada,
    ruta_esp_euros_entrada, ruta_esp_kg_entrada,
    ruta_mad_euros_salida, ruta_mad_kg_salida,
    ruta_esp_euros_salida, ruta_esp_kg_salida,
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
  fixed_cols <- c("flujo", "año", "mes", "pais", "nivel_taric", "cod_taric")
  select_cols_euros <- c(fixed_cols, "euros")
  
  df_mad_mes_euros <- df_mad_mes[, ..select_cols_euros]
  df_mad_euros <- calculo_totales_y_subtotales_dt(df_mad_mes_euros, columna = "euros")
  df_mad_euros[, nivel_taric := NULL]
  
  if (file.exists(ruta_mad_euros_entrada)) {
    df_mad_ant <- as.data.table(arrow::read_parquet(ruta_mad_euros_entrada))
    df_mad_union <- rbindlist(list(df_mad_ant, df_mad_euros), use.names = TRUE, fill = TRUE)
  } else {
    df_mad_union <- df_mad_euros
  }
  
  setorder(df_mad_union, año, mes)
  arrow::write_parquet(df_mad_union, ruta_mad_euros_salida)
  message("  [OK] Madrid euros actualizado -> ", ruta_mad_euros_salida)
  
  # --- Madrid: kilogramos ---
  message("Procesando Madrid (kg)...")
  select_cols_kg <- c(fixed_cols, "kilogramos")
  
  df_mad_mes_kg <- df_mad_mes[, ..select_cols_kg]
  df_mad_kg <- calculo_totales_y_subtotales_dt(df_mad_mes_kg, columna = "kilogramos")
  df_mad_kg[, nivel_taric := NULL]
  
  if (file.exists(ruta_mad_kg_entrada)) {
    df_mad_ant_kg <- as.data.table(arrow::read_parquet(ruta_mad_kg_entrada))
    df_mad_union_kg <- rbindlist(list(df_mad_ant_kg, df_mad_kg), use.names = TRUE, fill = TRUE)
  } else {
    df_mad_union_kg <- df_mad_kg
  }
  
  setorder(df_mad_union_kg, año, mes)
  arrow::write_parquet(df_mad_union_kg, ruta_mad_kg_salida)
  message("  [OK] Madrid kg actualizado -> ", ruta_mad_kg_salida)
  
  # --- España: euros ---
  message("Procesando España (euros)...")
  df_esp_mes_euros <- df_esp_mes[, ..select_cols_euros]
  df_esp_euros <- calculo_totales_y_subtotales_dt(df_esp_mes_euros, columna = "euros")
  df_esp_euros[, nivel_taric := NULL]
  
  if (file.exists(ruta_esp_euros_entrada)) {
    df_esp_ant <- as.data.table(arrow::read_parquet(ruta_esp_euros_entrada))
    df_esp_union <- rbindlist(list(df_esp_ant, df_esp_euros), use.names = TRUE, fill = TRUE)
  } else {
    df_esp_union <- df_esp_euros
  }
  
  setorder(df_esp_union, año, mes)
  arrow::write_parquet(df_esp_union, ruta_esp_euros_salida)
  message("  [OK] España euros actualizado -> ", ruta_esp_euros_salida)
  
  # --- España: kilogramos ---
  message("Procesando España (kg)...")
  df_esp_mes_kg <- df_esp_mes[, ..select_cols_kg]
  df_esp_kg <- calculo_totales_y_subtotales_dt(df_esp_mes_kg, columna = "kilogramos")
  df_esp_kg[, nivel_taric := NULL]
  
  if (file.exists(ruta_esp_kg_entrada)) {
    df_esp_ant_kg <- as.data.table(arrow::read_parquet(ruta_esp_kg_entrada))
    df_esp_union_kg <- rbindlist(list(df_esp_ant_kg, df_esp_kg), use.names = TRUE, fill = TRUE)
  } else {
    df_esp_union_kg <- df_esp_kg
  }
  
  setorder(df_esp_union_kg, año, mes)
  arrow::write_parquet(df_esp_union_kg, ruta_esp_kg_salida)
  message("  [OK] España kg actualizado -> ", ruta_esp_kg_salida)
  
  message("=== Actualización completada correctamente ===")
}

actualizacion_mensual_datos_provisionales <- function(
    yea,
    yeardefi,
    mon,
    ruta_ccaa_dir = "./datos/total_ccaas",
    ruta_mad_dir  = "./datos/total_mad",
    ruta_esp_dir  = "./datos/total_esp",
    ruta_final    = "./datos/totales_mad_esp",
    ruta_mad_euros_salida = NULL,
    ruta_mad_kg_salida    = NULL,
    ruta_esp_euros_salida = NULL,
    ruta_esp_kg_salida    = NULL) {
  
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
      actual = file.path(ruta_final, "de_mad_euros.parquet"),
      nuevo  = file.path(ruta_final, paste0("de_mad_euros_", año_fichero_origen, "_", mes_formateado, ".parquet"))
    ),
    list(
      actual = file.path(ruta_final, "de_mad_kg.parquet"),
      nuevo  = file.path(ruta_final, paste0("de_mad_kg_", año_fichero_origen, "_", mes_formateado, ".parquet"))
    ),
    list(
      actual = file.path(ruta_final, "de_esp_euros.parquet"),
      nuevo  = file.path(ruta_final, paste0("de_esp_euros_", año_fichero_origen, "_", mes_formateado, ".parquet"))
    ),
    list(
      actual = file.path(ruta_final, "de_esp_kg.parquet"),
      nuevo  = file.path(ruta_final, paste0("de_esp_kg_", año_fichero_origen, "_", mes_formateado, ".parquet"))
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
  generacion_ficheros_mes(
    year    = yea,
    yeardef = yeardefi,
    monthss = mon,
    
    # CCAA (mantiene unión con histórico)
    ruta_ccaa_salida = ruta_ccaa_dir,
    ruta_ccaa_origen = file.path(ruta_ccaa_dir, paste0("total_ccaa_hasta_", año_fichero_origen, "_", mes_formateado, ".csv")),
    ruta_ccaa_final  = file.path(ruta_ccaa_dir, paste0("total_ccaa_hasta_", yea, "_", mes_actual, ".csv")),
    
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
    ruta_mad_euros_salida <- file.path(ruta_final, "de_mad_euros.parquet")
  }
  if (is.null(ruta_mad_kg_salida)) {
    ruta_mad_kg_salida <- file.path(ruta_final, "de_mad_kg.parquet")
  }
  if (is.null(ruta_esp_euros_salida)) {
    ruta_esp_euros_salida <- file.path(ruta_final, "de_esp_euros.parquet")
  }
  if (is.null(ruta_esp_kg_salida)) {
    ruta_esp_kg_salida <- file.path(ruta_final, "de_esp_kg.parquet")
  }
  
  actualizacion_mensual_totales_mad_esp(
    # Nuevos ficheros del mes actual (provisionales)
    ruta_mad_mes = file.path(ruta_mad_dir, "prov", paste0("de_mad_prov_", yea, "_", mes_actual, ".parquet")),
    ruta_esp_mes = file.path(ruta_esp_dir, "prov", paste0("de_esp_prov_", yea, "_", mes_actual, ".parquet")),
    
    # Entradas: totales del mes anterior (de referencia)
    ruta_mad_euros_entrada = file.path(ruta_final, paste0("de_mad_euros_", año_fichero_origen, "_", mes_formateado, ".parquet")),
    ruta_mad_kg_entrada    = file.path(ruta_final, paste0("de_mad_kg_", año_fichero_origen, "_", mes_formateado, ".parquet")),
    ruta_esp_euros_entrada = file.path(ruta_final, paste0("de_esp_euros_", año_fichero_origen, "_", mes_formateado, ".parquet")),
    ruta_esp_kg_entrada    = file.path(ruta_final, paste0("de_esp_kg_", año_fichero_origen, "_", mes_formateado, ".parquet")),
    
    # Salidas actualizadas (se pasan tal cual, YA vienen con ruta completa)
    ruta_mad_euros_salida = ruta_mad_euros_salida,
    ruta_mad_kg_salida    = ruta_mad_kg_salida,
    ruta_esp_euros_salida = ruta_esp_euros_salida,
    ruta_esp_kg_salida    = ruta_esp_kg_salida,
    
    # Año y mes del nuevo dato
    year = yea,
    mes = mon
  )
  
  message("=== ACTUALIZACIÓN MENSUAL FINALIZADA CORRECTAMENTE ===")
  invisible(TRUE)
}

##### Función de actualización anual cuando datos pasan a definitivos -----
actualizacion_anual <- function(
    year_nuevo_def,      # Año que pasa de provisional a definitivo
    year_actual,         # Año actual (último año con datos)
    mes_actual,          # Mes actual en formato "01", "02", etc.
    fyea = 1995,         # Primer año histórico
    ruta_ccaa_dir = "./datos/total_ccaas",
    ruta_mad_dir = "./datos/total_mad",
    ruta_esp_dir = "./datos/total_esp",
    ruta_final = "./datos/totales_mad_esp"
) {
  
  message("========================================")
  message("ACTUALIZACIÓN ANUAL - DATOS DEFINITIVOS")
  message("========================================")
  message(sprintf("Año que pasa a definitivo: %d", year_nuevo_def))
  message(sprintf("Rango provisional: %d-%d", year_nuevo_def + 1, year_actual))
  message("")
  
  # TOTALES CCAA
  message(">>> [1/3] Procesando TOTALES CCAA...")
  
  # Cargar fichero definitivo anterior
  archivo_ccaa_def_anterior <- file.path(ruta_ccaa_dir, sprintf("total_ccaa_def_hasta_%d.csv", year_nuevo_def - 1))
  archivo_ccaa_def_nuevo <- file.path(ruta_ccaa_dir, sprintf("total_ccaa_def_hasta_%d.csv", year_nuevo_def))
  
  if (file.exists(archivo_ccaa_def_anterior)) {
    message(sprintf("  - Cargando definitivos anteriores hasta %d...", year_nuevo_def - 1))
    df_ccaa_def <- fread(archivo_ccaa_def_anterior)
  } else {
    message("  - No hay fichero definitivo anterior. Combinando desde el inicio...")
    combinar_csvs(
      folder_path = file.path(ruta_ccaa_dir, "def"),
      output_path = archivo_ccaa_def_anterior,
      years = fyea:(year_nuevo_def - 1)
    )
    df_ccaa_def <- fread(archivo_ccaa_def_anterior)
  }
  
  # Añadir el nuevo año definitivo
  message(sprintf("  - Añadiendo año %d a definitivos...", year_nuevo_def))
  combinar_csvs(
    folder_path = file.path(ruta_ccaa_dir, "def"),
    output_path = file.path(ruta_ccaa_dir, sprintf("total_ccaa_%d.csv", year_nuevo_def)),
    years = year_nuevo_def
  )
  df_ccaa_nuevo <- fread(file.path(ruta_ccaa_dir, sprintf("total_ccaa_%d.csv", year_nuevo_def)))
  
  # Unir y guardar
  df_ccaa_def_completo <- rbindlist(list(df_ccaa_def, df_ccaa_nuevo), use.names = TRUE, fill = TRUE)
  fwrite(df_ccaa_def_completo, archivo_ccaa_def_nuevo)
  message(sprintf("  ✓ Fichero definitivo guardado: %s", archivo_ccaa_def_nuevo))
  
  # Recalcular provisionales
  if (year_nuevo_def + 1 <= year_actual) {
    message(sprintf("  - Recalculando provisionales %d-%d...", year_nuevo_def + 1, year_actual))
    combinar_csvs(
      folder_path = file.path(ruta_ccaa_dir, "prov"),
      output_path = file.path(ruta_ccaa_dir, "total_ccaa_prov.csv"),
      years = (year_nuevo_def + 1):year_actual
    )
    
    # Unir definitivos + provisionales
    unir_csvs(
      input_paths = c(
        archivo_ccaa_def_nuevo,
        file.path(ruta_ccaa_dir, "total_ccaa_prov.csv")
      ),
      output_path = file.path(ruta_ccaa_dir, sprintf("total_ccaa_hasta_%d_%s.csv", year_actual, mes_actual))
    )
  } else {
    # Si no hay provisionales, copiar directamente
    file.copy(
      archivo_ccaa_def_nuevo,
      file.path(ruta_ccaa_dir, sprintf("total_ccaa_hasta_%d_%s.csv", year_actual, mes_actual)),
      overwrite = TRUE
    )
  }
  message(sprintf("  ✓ Total CCAA hasta %d_%s generado\n", year_actual, mes_actual))
  
  
  # MICRODATOS MADRID
  message(">>> [2/3] Procesando MICRODATOS MADRID...")
  
  archivo_mad_def_anterior <- file.path(ruta_mad_dir, sprintf("total_mad_def_hasta_%d.parquet", year_nuevo_def - 1))
  archivo_mad_def_nuevo <- file.path(ruta_mad_dir, sprintf("total_mad_def_hasta_%d.parquet", year_nuevo_def))
  
  if (file.exists(archivo_mad_def_anterior)) {
    message(sprintf("  - Cargando definitivos anteriores hasta %d...", year_nuevo_def - 1))
    df_mad_def <- as.data.table(arrow::read_parquet(archivo_mad_def_anterior))
  } else {
    message("  - No hay fichero definitivo anterior. Combinando desde el inicio...")
    combinar_parquets(
      folder_path = file.path(ruta_mad_dir, "def"),
      output_path = archivo_mad_def_anterior,
      years = fyea:(year_nuevo_def - 1)
    )
    df_mad_def <- as.data.table(arrow::read_parquet(archivo_mad_def_anterior))
  }
  
  # Añadir nuevo año
  message(sprintf("  - Añadiendo año %d a definitivos...", year_nuevo_def))
  combinar_parquets(
    folder_path = file.path(ruta_mad_dir, "def"),
    output_path = file.path(ruta_mad_dir, sprintf("total_mad_%d.parquet", year_nuevo_def)),
    years = year_nuevo_def
  )
  df_mad_nuevo <- as.data.table(arrow::read_parquet(file.path(ruta_mad_dir, sprintf("total_mad_%d.parquet", year_nuevo_def))))
  
  df_mad_def_completo <- rbindlist(list(df_mad_def, df_mad_nuevo), use.names = TRUE, fill = TRUE)
  arrow::write_parquet(df_mad_def_completo, archivo_mad_def_nuevo)
  message(sprintf("  ✓ Fichero definitivo guardado: %s", archivo_mad_def_nuevo))
  
  # Recalcular provisionales y unir
  if (year_nuevo_def + 1 <= year_actual) {
    message(sprintf("  - Recalculando provisionales %d-%d...", year_nuevo_def + 1, year_actual))
    combinar_parquets(
      folder_path = file.path(ruta_mad_dir, "prov"),
      output_path = file.path(ruta_mad_dir, "total_mad_prov.parquet"),
      years = (year_nuevo_def + 1):year_actual
    )
    
    unir_parquets(
      input_paths = c(
        archivo_mad_def_nuevo,
        file.path(ruta_mad_dir, "total_mad_prov.parquet")
      ),
      output_path = file.path(ruta_mad_dir, sprintf("total_mad_hasta_%d_%s.parquet", year_actual, mes_actual))
    )
  } else {
    file.copy(
      archivo_mad_def_nuevo,
      file.path(ruta_mad_dir, sprintf("total_mad_hasta_%d_%s.parquet", year_actual, mes_actual)),
      overwrite = TRUE
    )
  }
  message(sprintf("  ✓ Total Madrid hasta %d_%s generado\n", year_actual, mes_actual))
  
  
  # MICRODATOS ESPAÑA
  message(">>> [3/3] Procesando MICRODATOS ESPAÑA...")
  
  archivo_esp_def_anterior <- file.path(ruta_esp_dir, sprintf("total_esp_def_hasta_%d.parquet", year_nuevo_def - 1))
  archivo_esp_def_nuevo <- file.path(ruta_esp_dir, sprintf("total_esp_def_hasta_%d.parquet", year_nuevo_def))
  
  if (file.exists(archivo_esp_def_anterior)) {
    message(sprintf("  - Cargando definitivos anteriores hasta %d...", year_nuevo_def - 1))
    df_esp_def <- as.data.table(arrow::read_parquet(archivo_esp_def_anterior))
  } else {
    message("  - No hay fichero definitivo anterior. Combinando desde el inicio...")
    combinar_parquets(
      folder_path = file.path(ruta_esp_dir, "def"),
      output_path = archivo_esp_def_anterior,
      years = fyea:(year_nuevo_def - 1)
    )
    df_esp_def <- as.data.table(arrow::read_parquet(archivo_esp_def_anterior))
  }
  
  # Añadir nuevo año
  message(sprintf("  - Añadiendo año %d a definitivos...", year_nuevo_def - 1))
  combinar_parquets(
    folder_path = file.path(ruta_esp_dir, "def"),
    output_path = file.path(ruta_esp_dir, sprintf("total_esp_%d.parquet", year_nuevo_def)),
    years = year_nuevo_def
  )
  df_esp_nuevo <- as.data.table(arrow::read_parquet(file.path(ruta_esp_dir, sprintf("total_esp_%d.parquet", year_nuevo_def))))
  
  df_esp_def_completo <- rbindlist(list(df_esp_def, df_esp_nuevo), use.names = TRUE, fill = TRUE)
  arrow::write_parquet(df_esp_def_completo, archivo_esp_def_nuevo)
  message(sprintf("  ✓ Fichero definitivo guardado: %s", archivo_esp_def_nuevo))
  
  # Recalcular provisionales y unir
  if (year_nuevo_def + 1 <= year_actual) {
    message(sprintf("  - Recalculando provisionales %d-%d...", year_nuevo_def + 1, year_actual))
    combinar_parquets(
      folder_path = file.path(ruta_esp_dir, "prov"),
      output_path = file.path(ruta_esp_dir, "total_esp_prov.parquet"),
      years = (year_nuevo_def + 1):year_actual
    )
    
    unir_parquets(
      input_paths = c(
        archivo_esp_def_nuevo,
        file.path(ruta_esp_dir, "total_esp_prov.parquet")
      ),
      output_path = file.path(ruta_esp_dir, sprintf("total_esp_hasta_%d_%s.parquet", year_actual, mes_actual))
    )
  } else {
    file.copy(
      archivo_esp_def_nuevo,
      file.path(ruta_esp_dir, sprintf("total_esp_hasta_%d_%s.parquet", year_actual, mes_actual)),
      overwrite = TRUE
    )
  }
  message(sprintf("  ✓ Total España hasta %d_%s generado\n", year_actual, mes_actual))
  
  
  # GENERAR FICHEROS FINALES CON TOTALES
  message(">>> [4/4] Generando ficheros finales con totales...")
  
  # Madrid - euros
  anadir_totales_parquet(
    file.path(ruta_mad_dir, sprintf("total_mad_hasta_%d_%s.parquet", year_actual, mes_actual)),
    file.path(ruta_final, sprintf("de_mad_euros_%d_%s.parquet", year_actual, mes_actual)),
    columna = "euros"
  )
  
  # Madrid - kilogramos
  anadir_totales_parquet(
    file.path(ruta_mad_dir, sprintf("total_mad_hasta_%d_%s.parquet", year_actual, mes_actual)),
    file.path(ruta_final, sprintf("de_mad_kg_%d_%s.parquet", year_actual, mes_actual)),
    columna = "kilogramos"
  )
  
  # España - euros
  anadir_totales_parquet(
    file.path(ruta_esp_dir, sprintf("total_esp_hasta_%d_%s.parquet", year_actual, mes_actual)),
    file.path(ruta_final, sprintf("de_esp_euros_%d_%s.parquet", year_actual, mes_actual)),
    columna = "euros"
  )
  
  # España - kilogramos
  anadir_totales_parquet(
    file.path(ruta_esp_dir, sprintf("total_esp_hasta_%d_%s.parquet", year_actual, mes_actual)),
    file.path(ruta_final, sprintf("de_esp_kg_%d_%s.parquet", year_actual, mes_actual)),
    columna = "kilogramos"
  )
  
  message("  ✓ Ficheros finales con totales generados\n")
  
  message("========================================")
  message("✓ ACTUALIZACIÓN ANUAL COMPLETADA")
  message("========================================")
  message(sprintf("Nuevos ficheros definitivos hasta: %d", year_nuevo_def))
  message(sprintf("Ficheros acumulados hasta: %d_%s", year_actual, mes_actual))
  message("")
}

#### Calculo totales y subtotales -----
calcular_totales_dt <- function(df, columna = "euros") {
  setDT(df)
  
  totales <- df[nivel_taric == 1L,
                .(valor_sumado = sum(get(columna), na.rm = TRUE)),
                by = .(flujo, año, mes)]
  totales[, ':=' (pais = 0L, cod_taric = 0, nivel_taric =0L)]
  setnames(totales, "valor_sumado", columna)
  return(totales)
}

calcular_subtotal_pais_dt <- function(df, columna = "euros") {
  setDT(df)
  
  subtotal <- df[nivel_taric == 1,
                 .(valor_sumado = sum(get(columna), na.rm = TRUE)),
                 by = .(flujo, año, mes, pais)]
  subtotal[, ':=' (cod_taric = 0, nivel_taric =0L)]
  setnames(subtotal, "valor_sumado", columna)
  return(subtotal)
}

calcular_subtotal_taric_dt <- function(df, columna = "euros") {
  setDT(df)
  
  subtotal <- df[, .(valor_sumado = sum(get(columna), na.rm = TRUE)),
                 by = .(flujo, año, mes, cod_taric, nivel_taric)]
  subtotal[, pais := 0L]
  setnames(subtotal, "valor_sumado", columna)
  return(subtotal)
}

calculo_totales_y_subtotales_dt <- function(df_input, columna = "euros") {
  df_processed <- copy(df_input)
  setDT(df_processed)
  
  # Pass the 'columna' argument to all sub-functions
  df_total <- calcular_totales_dt(df_processed, columna = columna)
  df_subpais <- calcular_subtotal_pais_dt(df_processed, columna = columna)
  df_subtaric <- calcular_subtotal_taric_dt(df_processed, columna = columna)
  
  combined_df <- rbindlist(list(df_processed, df_total, df_subpais, df_subtaric), use.names = TRUE, fill = TRUE)
  return(combined_df)
}

anadir_totales_parquet <- function(path_entrada, path_salida, columna = "euros", 
                                   fixed_cols = c("flujo", "año", "mes", "pais", "nivel_taric", "cod_taric")) {
  
  select_cols <- c(fixed_cols, columna)
  
  # Leer parquet con columnas seleccionadas
  df <- as.data.table(read_parquet(path_entrada))
  df <- df[, ..select_cols]
  
  # Calcular totales y subtotales
  df_resultado <- calculo_totales_y_subtotales_dt(df, columna = columna)
  df_resultado[, nivel_taric := NULL]
  
  # Guardar en parquet
  write_parquet(df_resultado, path_salida)
  message(paste0("Proceso completado. El archivo procesado se ha guardado en: ", path_salida, 
                 " usando la columna '", columna, "'."))
  
  return(df_resultado)
}
