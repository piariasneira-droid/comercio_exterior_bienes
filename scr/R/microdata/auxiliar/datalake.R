..validate_meses <- function(meses) {
  if (length(meses) == 0L) stop("La lista de meses no puede estar vacía")
  bad <- meses[meses < 1L | meses > 12L]
  if (length(bad) > 0L) stop(paste("Mes inválido:", paste(bad, collapse = ", ")))
  meses
}

.available_months <- function(anio, ultimo_anio_prov, ultimo_mes_disponible) {
  if (!is.null(ultimo_mes_disponible) && anio == ultimo_anio_prov)
    return(seq_len(ultimo_mes_disponible))
  1:12
}

# arrow Table -> data.table
.arrow_to_dt <- function(tbl) {
  as.data.table(as.data.frame(tbl))
}

# data.table -> arrow Table
.dt_to_arrow <- function(dt) {
  arrow::as_arrow_table(dt)
}


#### CLASE ParquetStore ----
ParquetStore <- function(base_dir) {
  
  base_dir <- fs::path(base_dir)
  
  # Métodos básicos ----
  
  build_path <- function(ambito, dominio, estado, anio, mes,
                         filename = "data_0.parquet") {
    fs::path(base_dir, ambito, dominio,
             paste0("estado=", estado),
             paste0("anio=",   anio),
             paste0("mes=",    mes),
             filename)
  }
  
  exists_parquet <- function(ambito, dominio, estado, anio, mes,
                             filename = "data_0.parquet") {
    fs::file_exists(build_path(ambito, dominio, estado, anio, mes, filename))
  }
  
  # Lee parquet -> data.table (collect inmediato)
  read_parquet_dt <- function(ambito, dominio, estado, anio, mes,
                              filename = "data_0.parquet") {
    path <- build_path(ambito, dominio, estado, anio, mes, filename)
    if (!fs::file_exists(path)) stop(paste("No existe:", path))
    log_debug("Leyendo: {path}")
    dt <- .arrow_to_dt(arrow::read_parquet(as.character(path), as_data_frame = FALSE))
    dt[, estado := as.integer(estado)]
    dt[, anio   := as.integer(anio)]
    dt[, mes    := as.integer(mes)]
    dt[]
  }
  
  # Escribe data.table -> parquet
  write_parquet_dt <- function(dt, ambito, dominio, estado, anio, mes,
                               filename = "data_0.parquet", overwrite = TRUE) {
    path <- build_path(ambito, dominio, estado, anio, mes, filename)
    fs::dir_create(fs::path_dir(path), recurse = TRUE)
    if (fs::file_exists(path) && !overwrite) stop(paste("Ya existe:", path))
    log_debug("Escribiendo: {path}")
    arrow::write_parquet(.dt_to_arrow(dt), as.character(path))
    invisible(path)
  }
  
  # ETL desde CSV ----
  
  read_and_transform_csv <- function(csv_file, nivel, estado, anio, mes) {
    # Arrow lee el TSV con encoding; devolvemos data.table para transformar con :=
    tbl <- arrow::read_delim_arrow(
      as.character(csv_file),
      delim         = "\t",
      read_options  = arrow::CsvReadOptions$create(encoding = "UTF-16"),
      schema        = arrow::schema(
        .list = setNames(
          rep(list(arrow::utf8()), if (nivel == "taric") 10L else 9L),
          if (nivel == "taric")
            c("flujo","pais","provincia","euros","dolares","kilogramos","nivel_taric","cod_taric","estado","anio")
          else
            c("flujo","pais","provincia","nivel_sector_economico","cod_sector_economico","euros","dolares","estado","anio")
        )
      ),
      as_data_frame = FALSE
    )
    dt <- .arrow_to_dt(tbl)
    
    parse_num <- function(x) as.numeric(gsub(",", ".", x, fixed = TRUE))
    
    if (nivel == "taric") {
      dt[, `:=`(
        flujo       = fifelse(flujo == "E", 1L, fifelse(flujo == "I", 0L, NA_integer_)),
        pais        = as.integer(pais),
        provincia   = as.integer(provincia),
        euros       = parse_num(euros),
        dolares     = parse_num(dolares),
        kilogramos  = parse_num(kilogramos),
        nivel_taric = as.integer(nivel_taric),
        cod_taric   = as.numeric(cod_taric),
        estado      = as.integer(estado),
        anio        = as.integer(anio),
        mes         = as.integer(mes)
      )]
    } else {
      dt[, `:=`(
        flujo                  = fifelse(flujo == "E", 1L, fifelse(flujo == "I", 0L, NA_integer_)),
        pais                   = as.integer(pais),
        provincia              = as.integer(provincia),
        nivel_sector_economico = as.integer(nivel_sector_economico),
        cod_sector_economico   = as.character(cod_sector_economico),
        euros                  = parse_num(euros),
        dolares                = parse_num(dolares),
        estado                 = as.integer(estado),
        anio                   = as.integer(anio),
        mes                    = as.integer(mes)
      )]
    }
    dt[]
  }
  
  aggregate_spain <- function(dt, nivel) {
    if (nivel == "taric") {
      dt[, .(euros      = sum(euros,      na.rm = TRUE),
             dolares    = sum(dolares,    na.rm = TRUE),
             kilogramos = sum(kilogramos, na.rm = TRUE)),
         by = .(flujo, pais, nivel_taric, cod_taric, estado, anio, mes)]
    } else {
      dt[, .(euros   = sum(euros,   na.rm = TRUE),
             dolares = sum(dolares, na.rm = TRUE)),
         by = .(flujo, pais, nivel_sector_economico, cod_sector_economico, estado, anio, mes)]
    }
  }
  
  process_ccaa_totals <- function(dt, estado_val, anio_val, mes_val) {
    provincias <- data.table(read.csv(
      "./data/raw/metadatos/PROVINCIAS.csv",
      sep      = "\t",
      fileEncoding = "UTF-16",
      stringsAsFactors = FALSE
    ))
    setnames(provincias, c("cod_provincia", "provincia_nombre", "cod_comunidad", "comunidad_nombre"))
    provincias[, cod_provincia := as.integer(cod_provincia)]
    provincias[, cod_comunidad := as.integer(cod_comunidad)]
    lookup <- provincias[, .(provincia = cod_provincia, cod_comunidad)]
    
    provincias <- data.table(read.csv(
      "./data/raw/metadatos/PROVINCIAS.csv",
      sep      = "\t",
      fileEncoding = "UTF-16",
      stringsAsFactors = FALSE
    ))
    setnames(provincias, c("cod_provincia", "provincia_nombre", "cod_comunidad", "comunidad_nombre"))
    provincias[, cod_provincia := as.integer(cod_provincia)]
    provincias[, cod_comunidad := as.integer(cod_comunidad)]
    lookup <- provincias[, .(provincia = cod_provincia, cod_comunidad)]
    
    # Limpiar columnas duplicadas y tipos
    dt <- copy(dt)
    dt[, estado := as.integer(estado_val)]
    dt[, anio   := as.integer(anio_val)]
    dt[, mes    := as.integer(mes_val)]
    if ("año" %in% names(dt)) dt[, año := NULL]
    if ("kilogramos" %in% names(dt) && is.character(dt$kilogramos))
      dt[, kilogramos := as.numeric(gsub(",", ".", kilogramos, fixed = TRUE))]
    
    # Filtrar nivel raiz
    dt_n1 <- dt[nivel_sector_economico == 1L]
    
    # Total nacional (cod_comunidad = 99): suma TODAS las provincias incluida la 0
    dt_nac <- dt_n1[, .(euros      = sum(euros,      na.rm = TRUE),
                        dolares    = sum(dolares,    na.rm = TRUE)),
                    by = .(flujo, estado, anio, mes)]
    dt_nac[, cod_comunidad := 99L]
    
    # Totales por CCAA: join con lookup
    dt_n1 <- lookup[dt_n1, on = "provincia"]
    dt_n1 <- dt_n1[!is.na(cod_comunidad)]
    
    dt_ccaa <- dt_n1[, .(euros      = sum(euros,      na.rm = TRUE),
                         dolares    = sum(dolares,    na.rm = TRUE)),
                     by = .(flujo, cod_comunidad, estado, anio, mes)]
    
    cols <- c("flujo", "cod_comunidad", "estado", "anio", "mes", "euros", "dolares")
    result <- rbindlist(list(dt_ccaa[, ..cols], dt_nac[, ..cols]))
    setorder(result, flujo, cod_comunidad)
    result[]
  }
  
  csv_to_parquet <- function(year, month, version,
                             raw_base_dir      = "data/raw",
                             filtros_provincia = list(madrid = 28L)) {
    if (!version %in% c("def", "prov")) stop("version debe ser 'def' o 'prov'")
    if (month < 1L || month > 12L)      stop("month debe estar entre 1 y 12")
    
    estado_num <- if (version == "def") 1L else 0L
    month_str  <- sprintf("%02d", month)
    
    for (nivel in c("taric", "sectores")) {
      csv_prefix <- if (nivel == "taric") "taric" else "sec"
      csv_file   <- fs::path(raw_base_dir, nivel, version,
                             paste0("comex_", csv_prefix, "_", year, month_str, ".csv"))
      
      if (!fs::file_exists(csv_file)) {
        log_warn("No existe {csv_file}, saltando...")
        next
      }
      log_info("Procesando {fs::path_file(csv_file)}")
      
      dt_completo <- read_and_transform_csv(csv_file, nivel, estado_num, year, month)
      
      # 1. Completo
      write_parquet_dt(dt_completo, "", nivel, estado_num, year, month)
      log_info("  OK Completo: {nivel}/estado={estado_num}/anio={year}/mes={month_str}/")
      
      # 2. Filtros por provincia
      for (nombre in names(filtros_provincia)) {
        provs <- filtros_provincia[[nombre]]
        dt_f  <- dt_completo[provincia %in% provs, -"provincia"]
        write_parquet_dt(dt_f, nombre, nivel, estado_num, year, month)
        log_info("  OK {nombre}: {nivel} (provincias: {paste(provs, collapse=',')})")
      }
      
      # 3. España (agregado nacional)
      write_parquet_dt(aggregate_spain(dt_completo, nivel), "espana", nivel, estado_num, year, month)
      log_info("  OK España: {nivel} agregado nacional")
      
      # 4. Totales CCAA (solo sectores)
      if (nivel == "sectores") {
        write_parquet_dt(process_ccaa_totals(dt_completo, estado_num, year, month),
                         "", "totalesccaa", estado_num, year, month)
        log_info("  OK Totales CCAA")
      }
    }
    invisible(NULL)
  }
  
  # Procesamiento de rangos ----
  
  process_range <- function(year_start, year_end, version = "def",
                            ultimo_mes        = NULL,
                            raw_base_dir      = "data/raw",
                            filtros_provincia = NULL,
                            skip_existing     = FALSE) {
    if (is.null(filtros_provincia)) filtros_provincia <- list(madrid = 28L)
    env <- environment()
    env$stats <- list(procesados = 0L, saltados = 0L, errores = 0L)
    log_info("Procesando rango {year_start}-{year_end} ({version})")
    t0 <- proc.time()
    
    for (year in year_start:year_end) {
      meses  <- .available_months(year, year_end, if (year == year_end) ultimo_mes else NULL)
      estado <- if (version == "def") 1L else 0L
      
      for (month in meses) {
        if (skip_existing && exists_parquet("", "taric", estado, year, month)) {
          env$stats$saltados <- env$stats$saltados + 1L; next
        }
        tryCatch({
          csv_to_parquet(year, month, version, raw_base_dir, filtros_provincia)
          env$stats$procesados <- env$stats$procesados + 1L
        }, error = function(e) {
          if (grepl("No existe|cannot open", conditionMessage(e))) {
            env$stats$saltados <- env$stats$saltados + 1L
          } else {
            log_error("{year}-{sprintf('%02d',month)}: {conditionMessage(e)}")
            env$stats$errores <- env$stats$errores + 1L
          }
        })
      }
    }
    
    elapsed <- round((proc.time() - t0)["elapsed"], 1L)
    log_info("Completado en {elapsed}s — proc={env$stats$procesados} salt={env$stats$saltados} err={env$stats$errores}")
    env$stats
  }
  
  # Dominios derivados ----
  
  process_derived_domain <- function(ambito, dominio_in, dominio_out, estado,
                                     anio, mes, transform_fn, skip_if_exists = FALSE) {
    if (skip_if_exists && exists_parquet(ambito, dominio_out, estado, anio, mes)) {
      log_debug("Saltando {ambito}/{dominio_out} {anio}-{sprintf('%02d', mes)}")
      return(invisible(NULL))
    }
    log_debug("{ambito}/{dominio_in} -> {dominio_out} ({anio}-{sprintf('%02d', mes)})")
    dt_out <- transform_fn(read_parquet_dt(ambito, dominio_in, estado, anio, mes))
    write_parquet_dt(dt_out, ambito, dominio_out, estado, anio, mes)
  }
  
  process_derived_pipeline <- function(pipeline, ambitos, year_start, year_end,
                                       estado, ultimo_mes = NULL, skip_if_exists = TRUE) {
    env <- environment()
    env$stats <- list(procesados = 0L, saltados = 0L, errores = 0L)
    log_info("Pipeline: {length(pipeline)} transf x {length(ambitos)} ambitos x {year_end-year_start+1} años")
    t0 <- proc.time()
    
    for (ambito in ambitos) {
      for (step in pipeline) {
        for (anio in year_start:year_end) {
          meses <- .available_months(anio, year_end, if (anio == year_end) ultimo_mes else NULL)
          for (mes in meses) {
            tryCatch({
              process_derived_domain(ambito, step[[1L]], step[[2L]], estado,
                                     anio, mes, step[[3L]], skip_if_exists)
              env$stats$procesados <- env$stats$procesados + 1L
            }, error = function(e) {
              if (grepl("No existe|cannot open", conditionMessage(e)))
                env$stats$saltados <- env$stats$saltados + 1L
              else {
                log_error("{ambito}/{step[[2L]]} {anio}-{sprintf('%02d',mes)}: {conditionMessage(e)}")
                env$stats$errores <- env$stats$errores + 1L
              }
            })
          }
        }
      }
    }
    
    elapsed <- round((proc.time() - t0)["elapsed"], 1L)
    log_info("Pipeline completado en {elapsed}s — proc={env$stats$procesados} salt={env$stats$saltados} err={env$stats$errores}")
    env$stats
  }
  
  process_month_pipeline <- function(year, month, estado, pipeline, ambitos,
                                     skip_if_exists = FALSE) {
    env <- environment()
    env$stats <- list(procesados = 0L, saltados = 0L, errores = 0L)
    version <- if (estado == 1L) "def" else "prov"
    log_info(strrep("=", 60))
    log_info("Pipeline mes: {year}-{sprintf('%02d', month)} ({version})")
    log_info(strrep("=", 60))
    t0 <- proc.time()
    
    for (ambito in ambitos) {
      log_info("Ambito: {ambito}")
      for (step in pipeline) {
        tryCatch({
          process_derived_domain(ambito, step[[1L]], step[[2L]], estado,
                                 year, month, step[[3L]], skip_if_exists)
          env$stats$procesados <- env$stats$procesados + 1L
        }, error = function(e) {
          if (grepl("No existe|cannot open", conditionMessage(e)))
            env$stats$saltados <- env$stats$saltados + 1L
          else {
            log_error("{step[[1L]]} -> {step[[2L]]}: {conditionMessage(e)}")
            env$stats$errores <- env$stats$errores + 1L
          }
        })
      }
    }
    
    elapsed <- round((proc.time() - t0)["elapsed"], 1L)
    log_info("Completado en {elapsed}s — proc={env$stats$procesados} salt={env$stats$saltados} err={env$stats$errores}")
    if (env$stats$errores == 0L) log_info("Mes OK sin errores") else log_warn("{env$stats$errores} errores")
    env$stats
  }
  
  # Junta múltiples parquets leyendo fichero a fichero y añadiendo columnas temporales
  # lazy = TRUE/FALSE ambos devuelven data.table (arrow no tiene LazyFrame equivalente)
  merge_parquet_range <- function(ambito, dominio, anio_inicio, anio_definitivo,
                                  anio_fin, meses = NULL, filename = "data_0.parquet",
                                  lazy = FALSE) {
    if (anio_inicio > anio_fin) stop("anio_inicio no puede ser mayor que anio_fin")
    if (is.null(meses)) meses <- 1:12 else meses <- .validate_meses(meses)
    
    label <- if (nchar(ambito) == 0L) "nacional" else ambito
    log_info("Agregando {label}/{dominio} {anio_inicio}-{anio_fin} (def hasta {anio_definitivo})")
    
    paths <- character(0)
    meta  <- list()   # guardamos estado/anio/mes para añadirlos tras collect
    for (anio in anio_inicio:anio_fin) {
      estado <- if (anio <= anio_definitivo) 1L else 0L
      for (mes in meses) {
        p <- build_path(ambito, dominio, estado, anio, mes, filename)
        if (!fs::file_exists(p)) next
        paths <- c(paths, as.character(p))
        meta  <- c(meta, list(list(estado=estado, anio=anio, mes=mes)))
      }
    }
    
    if (length(paths) == 0L)
      stop(paste("No se encontró ningún parquet para", label, dominio))
    
    log_info("OK {length(paths)} parquets en la agregación")
    
    # Leer fichero a fichero y añadir columnas temporales (estado/anio/mes garantizados)
    dts <- vector("list", length(paths))
    for (i in seq_along(paths)) {
      d <- .arrow_to_dt(arrow::read_parquet(paths[i], as_data_frame = FALSE))
      d[, `:=`(estado = meta[[i]]$estado,
               anio   = meta[[i]]$anio,
               mes    = meta[[i]]$mes)]
      dts[[i]] <- d
    }
    dt <- rbindlist(dts, use.names = TRUE, fill = TRUE)
    
    if (lazy) return(dt)   # con arrow no hay LazyFrame real; devolvemos data.table igualmente
    dt[]
  }
  
  # API pública del store ----
  list(
    build_path               = build_path,
    exists                   = exists_parquet,
    read                     = read_parquet_dt,
    write                    = write_parquet_dt,
    csv_to_parquet           = csv_to_parquet,
    process_range            = process_range,
    process_derived_domain   = process_derived_domain,
    process_derived_pipeline = process_derived_pipeline,
    process_month_pipeline   = process_month_pipeline,
    merge_parquet_range      = merge_parquet_range
  )
}


#### Funciones de agregación ----

euros_taric         <- function(dt) dt[, .(euros      = sum(euros,      na.rm=TRUE)), by=.(flujo, nivel_taric, cod_taric)]
euros_pais          <- function(dt) dt[nivel_sector_economico==1L, .(euros=sum(euros,na.rm=TRUE)), by=.(flujo,pais)]
euros_taric_pais    <- function(dt) dt[, .(euros      = sum(euros,      na.rm=TRUE)), by=.(flujo, nivel_taric, cod_taric, pais)]
kg_taric            <- function(dt) dt[, .(kilogramos = sum(kilogramos, na.rm=TRUE)), by=.(flujo, nivel_taric, cod_taric)]
kg_pais             <- function(dt) dt[nivel_taric==1L, .(kilogramos=sum(kilogramos,na.rm=TRUE)), by=.(flujo,pais)]
kg_taric_pais       <- function(dt) dt[, .(kilogramos = sum(kilogramos, na.rm=TRUE)), by=.(flujo, nivel_taric, cod_taric, pais)]
euros_sectores      <- function(dt) dt[, .(euros      = sum(euros,      na.rm=TRUE)), by=.(flujo, nivel_sector_economico, cod_sector_economico)]
euros_sectores_pais <- function(dt) dt[, .(euros      = sum(euros,      na.rm=TRUE)), by=.(flujo, nivel_sector_economico, cod_sector_economico, pais)]


#### Esquemas para alinear columnas ----

TARIC_EUROS_SCHEMA <- list(
  flujo=integer(), anio=integer(), mes=integer(), estado=integer(), pais=integer(),
  nivel_taric=integer(), cod_taric=numeric(), euros=numeric()
)
TARIC_KG_SCHEMA <- list(
  flujo=integer(), anio=integer(), mes=integer(), estado=integer(), pais=integer(),
  nivel_taric=integer(), cod_taric=numeric(), kilogramos=numeric()
)
SECTORES_EUROS_SCHEMA <- list(
  flujo=integer(), anio=integer(), mes=integer(), estado=integer(), pais=integer(),
  nivel_sector_economico=integer(), cod_sector_economico=character(), euros=numeric()
)

# Alinea data.table al esquema: añade columnas faltantes, fuerza tipos, reordena
.align_schema_dt <- function(dt, schema) {
  dt <- copy(dt)
  for (col in names(schema)) {
    if (!col %in% names(dt))
      set(dt, j = col, value = vector(class(schema[[col]]), nrow(dt)))
    target <- class(schema[[col]])
    if (target == "integer")   set(dt, j = col, value = as.integer(dt[[col]]))
    if (target == "numeric")   set(dt, j = col, value = as.numeric(dt[[col]]))
    if (target == "character") set(dt, j = col, value = as.character(dt[[col]]))
  }
  dt[, names(schema), with = FALSE]
}


#### CSV to Parquet ----

run_csv_to_parquet <- function(
    raw_base_dir       = "data/raw",
    out_dir            = "data/rawparquet",
    ano_ini            = 1995L,
    ano_fin_def        = 2023L,
    ano_fin            = 2025L,
    ultimo_mes_prov    = 12L,
    filtros_provincia  = list(
      madrid           = 28L
    ),
    skip_existing = FALSE
) {
  ambitos  <- c("espana", names(filtros_provincia))
  pipeline <- list(
    list("taric",    "euros_taric",        euros_taric),
    list("taric",    "euros_taric_pais",    euros_taric_pais),
    list("sectores", "euros_pais",          euros_pais),
    list("taric",    "kg_pais",             kg_pais),
    list("taric",    "kg_taric",            kg_taric),
    list("taric",    "kg_taric_pais",       kg_taric_pais),
    list("sectores", "euros_sectores",      euros_sectores),
    list("sectores", "euros_sectores_pais", euros_sectores_pais)
  )
  store <- ParquetStore(out_dir)
  
  # 1. CSV -> parquet base
  s1 <- store$process_range(year_start=ano_ini,        year_end=ano_fin_def, version="def",
                            raw_base_dir=raw_base_dir, filtros_provincia=filtros_provincia,
                            skip_existing=skip_existing)
  s2 <- store$process_range(year_start=ano_fin_def+1L, year_end=ano_fin,     version="prov",
                            ultimo_mes=ultimo_mes_prov, raw_base_dir=raw_base_dir,
                            filtros_provincia=filtros_provincia, skip_existing=skip_existing)
  
  # 2. Dominios derivados
  s3 <- store$process_derived_pipeline(pipeline=pipeline, ambitos=ambitos,
                                       year_start=ano_ini,        year_end=ano_fin_def,
                                       estado=1L, skip_if_exists=skip_existing)
  s4 <- store$process_derived_pipeline(pipeline=pipeline, ambitos=ambitos,
                                       year_start=ano_fin_def+1L, year_end=ano_fin,
                                       estado=0L, ultimo_mes=ultimo_mes_prov,
                                       skip_if_exists=skip_existing)
  
  cat("\n", strrep("=",60), "\nRESUMEN FINAL\n", strrep("=",60), "\n")
  cat(sprintf("Base def  (%d-%d):  proc=%d  salt=%d  err=%d\n", ano_ini,        ano_fin_def, s1$procesados, s1$saltados, s1$errores))
  cat(sprintf("Base prov (%d-%d): proc=%d  salt=%d  err=%d\n",  ano_fin_def+1L, ano_fin,     s2$procesados, s2$saltados, s2$errores))
  cat(sprintf("Derivados def:      proc=%d  salt=%d  err=%d\n",                              s3$procesados, s3$saltados, s3$errores))
  cat(sprintf("Derivados prov:     proc=%d  salt=%d  err=%d\n",                              s4$procesados, s4$saltados, s4$errores))
  cat("Data lake completado\n")
  invisible(list(s1=s1, s2=s2, s3=s3, s4=s4))
}


#### Join parquet ----

run_join_parquet <- function(
    datalake_dir = "data/rawparquet",
    dataoutput   = "data/interim",
    anio_ini     = 1995L,
    anio_def     = 2023L,
    anio_fin     = 2025L,
    meses_list   = NULL,
    mapeo_ambito_cod_comunidad = list(
      madrid=15L,
      espana=99L
    )
) {
  ambitos <- names(mapeo_ambito_cod_comunidad)
  datalake_dir <- fs::path(datalake_dir)
  fs::dir_create(dataoutput, recurse=TRUE)
  
  # DuckDB procesa todo en disco, sin cargar en RAM
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(duckdb::dbDisconnect(con, shutdown=TRUE), add=TRUE)
  
  # Configurar memoria limite para ser conservadores (ajustar segun RAM disponible)
  DBI::dbExecute(con, "SET memory_limit = '12GB'")
  DBI::dbExecute(con, "SET threads = 4")
  
  nombres_guardados <- character(0)
  
  # Helper: construye glob de paths para un dominio/ambito dado el rango de anios
  # Devuelve un string SQL listo para usar en read_parquet([...])
  build_glob <- function(ambito, dominio, anio_inicio, anio_fin, anio_definitivo, meses=NULL) {
    paths <- character(0)
    for (anio in anio_inicio:anio_fin) {
      estado <- if (anio <= anio_definitivo) 1L else 0L
      meses_iter <- if (is.null(meses)) 1:12 else meses
      for (mes in meses_iter) {
        p <- fs::path(datalake_dir, ambito, dominio,
                      paste0("estado=", estado),
                      paste0("anio=", anio),
                      paste0("mes=", mes),
                      "data_0.parquet")
        if (fs::file_exists(p)) paths <- c(paths, as.character(p))
      }
    }
    if (length(paths) == 0L) return(NULL)
    # SQL array literal: ['path1', 'path2', ...]
    paste0("[", paste0("'", paths, "'", collapse=", "), "]")
  }
  
  # Helper: ejecuta una query DuckDB y escribe resultado directo a parquet
  # Envuelve la query en un subselect que renombra anio -> año
  query_to_parquet <- function(sql_query, out_path) {
    fs::dir_create(fs::path_dir(out_path), recurse=TRUE)
    write_sql <- sprintf(
      "COPY (SELECT * RENAME (anio AS \"año\") FROM (%s)) TO '%s' (FORMAT PARQUET)",
      sql_query, out_path
    )
    DBI::dbExecute(con, write_sql)
  }
  
  # Helper: query de un dominio con columnas extra y alias anio->año
  domain_query <- function(glob, extra_cols_sql, schema_cols) {
    # extra_cols_sql: vector de strings como "0 AS pais", "0.0 AS cod_taric"
    base_select <- paste(c(schema_cols, extra_cols_sql), collapse=", ")
    sprintf("SELECT %s FROM read_parquet(%s)", base_select, glob)
  }
  
  # Totales CCAA 
  log_info("Procesando totales CCAA...")
  glob_ccaa <- build_glob("", "totalesccaa", anio_ini, anio_fin, anio_def, meses_list)
  if (!is.null(glob_ccaa)) {
    out_path <- as.character(fs::path(dataoutput, "totalesccaa", "totalesccaa.csv"))
    fs::dir_create(fs::path_dir(out_path), recurse=TRUE)
    sql <- sprintf(
      "COPY (SELECT flujo, cod_comunidad, estado, anio AS año, mes, euros, dolares
             FROM read_parquet(%s)) TO '%s' (FORMAT CSV, HEADER TRUE)",
      glob_ccaa, out_path
    )
    DBI::dbExecute(con, sql)
    log_info("  OK totalesccaa.csv")
    nombres_guardados <- c(nombres_guardados, "totalesccaa")
  }
  
  # Columnas finales por schema - version CON pais (parquet ya lo contiene)
  taric_euros_cols        <- "CAST(flujo AS INTEGER) AS flujo, CAST(anio AS INTEGER) AS anio,
                               CAST(mes AS INTEGER) AS mes, CAST(estado AS INTEGER) AS estado,
                               CAST(pais AS INTEGER) AS pais, CAST(nivel_taric AS INTEGER) AS nivel_taric,
                               CAST(cod_taric AS DOUBLE) AS cod_taric, CAST(euros AS DOUBLE) AS euros"
  # Version SIN pais (se anade 0 AS pais fuera para no duplicar la columna)
  taric_euros_cols_nopais <- "CAST(flujo AS INTEGER) AS flujo, CAST(anio AS INTEGER) AS anio,
                               CAST(mes AS INTEGER) AS mes, CAST(estado AS INTEGER) AS estado,
                               CAST(nivel_taric AS INTEGER) AS nivel_taric,
                               CAST(cod_taric AS DOUBLE) AS cod_taric, CAST(euros AS DOUBLE) AS euros"
  
  taric_kg_cols        <- "CAST(flujo AS INTEGER) AS flujo, CAST(anio AS INTEGER) AS anio,
                            CAST(mes AS INTEGER) AS mes, CAST(estado AS INTEGER) AS estado,
                            CAST(pais AS INTEGER) AS pais, CAST(nivel_taric AS INTEGER) AS nivel_taric,
                            CAST(cod_taric AS DOUBLE) AS cod_taric, CAST(kilogramos AS DOUBLE) AS kilogramos"
  taric_kg_cols_nopais <- "CAST(flujo AS INTEGER) AS flujo, CAST(anio AS INTEGER) AS anio,
                            CAST(mes AS INTEGER) AS mes, CAST(estado AS INTEGER) AS estado,
                            CAST(nivel_taric AS INTEGER) AS nivel_taric,
                            CAST(cod_taric AS DOUBLE) AS cod_taric, CAST(kilogramos AS DOUBLE) AS kilogramos"
  
  sectores_euros_cols        <- "CAST(flujo AS INTEGER) AS flujo, CAST(anio AS INTEGER) AS anio,
                                  CAST(mes AS INTEGER) AS mes, CAST(estado AS INTEGER) AS estado,
                                  CAST(pais AS INTEGER) AS pais,
                                  CAST(nivel_sector_economico AS INTEGER) AS nivel_sector_economico,
                                  CAST(cod_sector_economico AS VARCHAR) AS cod_sector_economico,
                                  CAST(euros AS DOUBLE) AS euros"
  sectores_euros_cols_nopais <- "CAST(flujo AS INTEGER) AS flujo, CAST(anio AS INTEGER) AS anio,
                                  CAST(mes AS INTEGER) AS mes, CAST(estado AS INTEGER) AS estado,
                                  CAST(nivel_sector_economico AS INTEGER) AS nivel_sector_economico,
                                  CAST(cod_sector_economico AS VARCHAR) AS cod_sector_economico,
                                  CAST(euros AS DOUBLE) AS euros"
  
  # Obtener glob de totalesccaa para usar en unions por ambito
  glob_totales <- build_glob("", "totalesccaa", anio_ini, anio_fin, anio_def, meses_list)
  
  # Por ambito 
  for (ambito in ambitos) {
    log_info("Procesando ambito: {ambito} ({which(ambitos==ambito)}/{length(ambitos)})")
    cod_com <- mapeo_ambito_cod_comunidad[[ambito]]
    
    # globs por dominio
    g_euros_taric      <- build_glob(ambito, "euros_taric",      anio_ini, anio_fin, anio_def, meses_list)
    g_euros_taric_pais <- build_glob(ambito, "euros_taric_pais", anio_ini, anio_fin, anio_def, meses_list)
    g_euros_pais       <- build_glob(ambito, "euros_pais",       anio_ini, anio_fin, anio_def, meses_list)
    g_kg_taric         <- build_glob(ambito, "kg_taric",         anio_ini, anio_fin, anio_def, meses_list)
    g_kg_taric_pais    <- build_glob(ambito, "kg_taric_pais",    anio_ini, anio_fin, anio_def, meses_list)
    g_kg_pais          <- build_glob(ambito, "kg_pais",          anio_ini, anio_fin, anio_def, meses_list)
    g_euros_sectores   <- build_glob(ambito, "euros_sectores",   anio_ini, anio_fin, anio_def, meses_list)
    g_euros_sec_pais   <- build_glob(ambito, "euros_sectores_pais", anio_ini, anio_fin, anio_def, meses_list)
    
    # TARIC euros 
    parts <- list()
    if (!is.null(g_euros_taric))
      parts <- c(parts, sprintf(
        "SELECT %s, 0 AS pais FROM read_parquet(%s)", taric_euros_cols_nopais, g_euros_taric))
    if (!is.null(g_euros_taric_pais))
      parts <- c(parts, sprintf(
        "SELECT %s FROM read_parquet(%s)", taric_euros_cols, g_euros_taric_pais))
    if (!is.null(g_euros_pais))
      parts <- c(parts, sprintf(
        "SELECT %s, 0 AS nivel_taric, 0.0 AS cod_taric FROM read_parquet(%s)",
        "CAST(flujo AS INTEGER) AS flujo, CAST(anio AS INTEGER) AS anio,
         CAST(mes AS INTEGER) AS mes, CAST(estado AS INTEGER) AS estado,
         CAST(pais AS INTEGER) AS pais, CAST(euros AS DOUBLE) AS euros",
        g_euros_pais))
    # totales CCAA para este ambito
    if (!is.null(glob_totales))
      parts <- c(parts, sprintf(
        "SELECT %s, 0 AS pais, 0 AS nivel_taric, 0.0 AS cod_taric
         FROM read_parquet(%s) WHERE cod_comunidad = %d",
        "CAST(flujo AS INTEGER) AS flujo, CAST(anio AS INTEGER) AS anio,
         CAST(mes AS INTEGER) AS mes, CAST(estado AS INTEGER) AS estado,
         CAST(euros AS DOUBLE) AS euros",
        glob_totales, cod_com))
    
    if (length(parts) > 0) {
      out <- as.character(fs::path(dataoutput, ambito, paste0(ambito, "_euros_taric.parquet")))
      query_to_parquet(paste(parts, collapse=" UNION ALL BY NAME "), out)
      log_info("  OK {ambito}_euros_taric.parquet")
      nombres_guardados <- c(nombres_guardados, paste0(ambito, "_euros_taric"))
    }
    
    # TARIC kg 
    parts <- list()
    if (!is.null(g_kg_taric))
      parts <- c(parts, sprintf(
        "SELECT %s, 0 AS pais FROM read_parquet(%s)", taric_kg_cols_nopais, g_kg_taric))
    if (!is.null(g_kg_taric_pais))
      parts <- c(parts, sprintf(
        "SELECT %s FROM read_parquet(%s)", taric_kg_cols, g_kg_taric_pais))
    if (!is.null(g_kg_pais))
      parts <- c(parts, sprintf(
        "SELECT %s, 0 AS nivel_taric, 0.0 AS cod_taric FROM read_parquet(%s)",
        "CAST(flujo AS INTEGER) AS flujo, CAST(anio AS INTEGER) AS anio,
         CAST(mes AS INTEGER) AS mes, CAST(estado AS INTEGER) AS estado,
         CAST(pais AS INTEGER) AS pais, CAST(kilogramos AS DOUBLE) AS kilogramos",
        g_kg_pais))
    if (!is.null(glob_totales))
      parts <- c(parts, sprintf(
        "SELECT %s, 0 AS pais, 0 AS nivel_taric, 0.0 AS cod_taric, 0.0 AS kilogramos
         FROM read_parquet(%s) WHERE cod_comunidad = %d",
        "CAST(flujo AS INTEGER) AS flujo, CAST(anio AS INTEGER) AS anio,
         CAST(mes AS INTEGER) AS mes, CAST(estado AS INTEGER) AS estado",
        glob_totales, cod_com))
    
    if (length(parts) > 0) {
      out <- as.character(fs::path(dataoutput, ambito, paste0(ambito, "_kg_taric.parquet")))
      query_to_parquet(paste(parts, collapse=" UNION ALL BY NAME "), out)
      log_info("  OK {ambito}_kg_taric.parquet")
      nombres_guardados <- c(nombres_guardados, paste0(ambito, "_kg_taric"))
    }
    
    # Sectores euros
    parts <- list()
    if (!is.null(g_euros_sectores))
      parts <- c(parts, sprintf(
        "SELECT %s, 0 AS pais FROM read_parquet(%s)", sectores_euros_cols_nopais, g_euros_sectores))
    if (!is.null(g_euros_sec_pais))
      parts <- c(parts, sprintf(
        "SELECT %s FROM read_parquet(%s)", sectores_euros_cols, g_euros_sec_pais))
    if (!is.null(g_euros_pais))
      parts <- c(parts, sprintf(
        "SELECT %s, 0 AS nivel_sector_economico, '0' AS cod_sector_economico FROM read_parquet(%s)",
        "CAST(flujo AS INTEGER) AS flujo, CAST(anio AS INTEGER) AS anio,
         CAST(mes AS INTEGER) AS mes, CAST(estado AS INTEGER) AS estado,
         CAST(pais AS INTEGER) AS pais, CAST(euros AS DOUBLE) AS euros",
        g_euros_pais))
    if (!is.null(glob_totales))
      parts <- c(parts, sprintf(
        "SELECT %s, 0 AS pais, 0 AS nivel_sector_economico, '0' AS cod_sector_economico
         FROM read_parquet(%s) WHERE cod_comunidad = %d",
        "CAST(flujo AS INTEGER) AS flujo, CAST(anio AS INTEGER) AS anio,
         CAST(mes AS INTEGER) AS mes, CAST(estado AS INTEGER) AS estado,
         CAST(euros AS DOUBLE) AS euros",
        glob_totales, cod_com))
    
    if (length(parts) > 0) {
      out <- as.character(fs::path(dataoutput, ambito, paste0(ambito, "_euros_sectores.parquet")))
      query_to_parquet(paste(parts, collapse=" UNION ALL BY NAME "), out)
      log_info("  OK {ambito}_euros_sectores.parquet")
      nombres_guardados <- c(nombres_guardados, paste0(ambito, "_euros_sectores"))
    }
    
    gc()
  }
  
  log_info("Proceso completado. {length(nombres_guardados)} archivos en {dataoutput}")
  invisible(nombres_guardados)
}

aplicar_filtro <- function(dt, col, valores) {
  if (!is.null(valores) && col %in% names(dt)) {
    list(
      dt    = dt[get(col) %in% valores],
      label = sprintf("%s: [%s]", col, paste(valores, collapse = ", "))
    )
  } else {
    list(dt = dt, label = NULL)
  }
}