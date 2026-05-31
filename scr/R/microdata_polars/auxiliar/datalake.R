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

# polars DataFrame -> polars DataFrame (identity; kept for API compat)
.pl_to_pl <- function(lf) lf

# Helpers de conversión (por si algún sitio externo aún necesita data.frame)
.pl_to_df  <- function(lf) lf$collect()$to_data_frame()
.df_to_pl  <- function(df) pl$DataFrame(df)


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
  
  # Lee parquet -> polars DataFrame (collect inmediato)
  read_parquet_pl <- function(ambito, dominio, estado, anio, mes,
                              filename = "data_0.parquet") {
    path <- build_path(ambito, dominio, estado, anio, mes, filename)
    if (!fs::file_exists(path)) stop(paste("No existe:", path))
    log_debug("Leyendo: {path}")
    pl$read_parquet(as.character(path))
  }
  
  # Escribe polars DataFrame -> parquet
  write_parquet_pl <- function(df, ambito, dominio, estado, anio, mes,
                               filename = "data_0.parquet", overwrite = TRUE) {
    path <- build_path(ambito, dominio, estado, anio, mes, filename)
    fs::dir_create(fs::path_dir(path), recurse = TRUE)
    if (fs::file_exists(path) && !overwrite) stop(paste("Ya existe:", path))
    log_debug("Escribiendo: {path}")
    df$write_parquet(as.character(path))
    invisible(path)
  }
  
  # ETL desde CSV ----
  
  read_and_transform_csv <- function(csv_file, nivel, estado, anio, mes) {
    # Definir schema según nivel
    if (nivel == "taric") {
      col_names <- c("flujo","pais","provincia","euros","dolares","kilogramos",
                     "nivel_taric","cod_taric","estado","anio")
    } else {
      col_names <- c("flujo","pais","provincia","nivel_sector_economico",
                     "cod_sector_economico","euros","dolares","estado","anio")
    }
    
    # Polars lee el TSV; encoding UTF-16 se maneja leyendo con readr y pasando a polars
    # (polars en R aún no soporta UTF-16 directamente en scan_csv)
    raw_lines <- readr::read_tsv(
      as.character(csv_file),
      col_names = col_names,
      col_types = readr::cols(.default = readr::col_character()),
      locale    = readr::locale(encoding = "UTF-16"),
      show_col_types = FALSE
    )
    df <- pl$DataFrame(raw_lines)
    
    parse_num_expr <- function(col) {
      # "1.234,56" -> "1234.56" -> Float64
      pl$col(col)$str$replace_all(",", ".", literal = TRUE)$cast(pl$Float64)
    }
    
    if (nivel == "taric") {
      df <- df$with_columns(
        pl$when(pl$col("flujo") == "E")$then(1L)$
          when(pl$col("flujo") == "I")$then(0L)$
          otherwise(NA_integer_)$cast(pl$Int32)$alias("flujo"),
        pl$col("pais")$cast(pl$Int32),
        pl$col("provincia")$cast(pl$Int32),
        parse_num_expr("euros"),
        parse_num_expr("dolares"),
        parse_num_expr("kilogramos"),
        pl$col("nivel_taric")$cast(pl$Int32),
        pl$col("cod_taric")$str$replace_all(",", ".", literal = TRUE)$cast(pl$Float64),
        pl$lit(as.integer(estado))$cast(pl$Int32)$alias("estado"),
        pl$lit(as.integer(anio))$cast(pl$Int32)$alias("anio"),
        pl$lit(as.integer(mes))$cast(pl$Int32)$alias("mes")
      )
    } else {
      df <- df$with_columns(
        pl$when(pl$col("flujo") == "E")$then(1L)$
          when(pl$col("flujo") == "I")$then(0L)$
          otherwise(NA_integer_)$cast(pl$Int32)$alias("flujo"),
        pl$col("pais")$cast(pl$Int32),
        pl$col("provincia")$cast(pl$Int32),
        pl$col("nivel_sector_economico")$cast(pl$Int32),
        pl$col("cod_sector_economico")$cast(pl$Utf8),
        parse_num_expr("euros"),
        parse_num_expr("dolares"),
        pl$lit(as.integer(estado))$cast(pl$Int32)$alias("estado"),
        pl$lit(as.integer(anio))$cast(pl$Int32)$alias("anio"),
        pl$lit(as.integer(mes))$cast(pl$Int32)$alias("mes")
      )
    }
    df
  }
  
  aggregate_spain <- function(df, nivel) {
    if (nivel == "taric") {
      df$group_by(c("flujo","pais","nivel_taric","cod_taric","estado","anio","mes"))$
        agg(
          pl$col("euros")$sum()$alias("euros"),
          pl$col("dolares")$sum()$alias("dolares"),
          pl$col("kilogramos")$sum()$alias("kilogramos")
        )
    } else {
      df$group_by(c("flujo","pais","nivel_sector_economico","cod_sector_economico","estado","anio","mes"))$
        agg(
          pl$col("euros")$sum()$alias("euros"),
          pl$col("dolares")$sum()$alias("dolares")
        )
    }
  }
  
  process_ccaa_totals <- function(df, estado, anio, mes) {
    mapeo_ccaa <- c(
      0L,14L, 7L,13L, 1L, 6L, 9L, 4L, 8L, 6L, 9L, 1L,13L, 7L, 1L,10L,
      7L, 8L, 1L, 7L,14L, 1L, 2L, 1L, 6L, 8L,16L,10L,15L, 1L,11L,12L,
      10L, 3L, 6L, 5L,10L, 6L, 5L,17L, 6L, 1L, 6L, 8L, 2L, 7L,13L, 6L,
      14L, 6L, 2L,18L,19L
    )
    lookup <- pl$DataFrame(
      provincia     = seq_len(length(mapeo_ccaa) - 1L),
      cod_comunidad = mapeo_ccaa[seq_len(length(mapeo_ccaa) - 1L)]
    )
    
    df_n1 <- df$filter(pl$col("nivel_sector_economico") == 1L)
    df_n1 <- df_n1$join(lookup, on = "provincia", how = "left")
    df_n1 <- df_n1$filter(
      pl$col("cod_comunidad")$is_not_null() & (pl$col("cod_comunidad") >= 0L)
    )
    
    df_ccaa <- df_n1$
      group_by(c("flujo","cod_comunidad","estado","anio","mes"))$
      agg(
        pl$col("euros")$sum()$alias("euros"),
        pl$col("dolares")$sum()$alias("dolares")
      )
    
    df_nac <- df_n1$
      group_by(c("flujo","estado","anio","mes"))$
      agg(
        pl$col("euros")$sum()$alias("euros"),
        pl$col("dolares")$sum()$alias("dolares")
      )$
      with_columns(pl$lit(99L)$cast(pl$Int32)$alias("cod_comunidad"))
    
    cols <- c("flujo","cod_comunidad","estado","anio","mes","euros","dolares")
    result <- df_ccaa$select(cols)$vstack(df_nac$select(cols))
    result$sort(c("flujo","cod_comunidad"))
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
      
      df_completo <- read_and_transform_csv(csv_file, nivel, estado_num, year, month)
      
      # 1. Completo
      write_parquet_pl(df_completo, "", nivel, estado_num, year, month)
      log_info("  OK Completo: {nivel}/estado={estado_num}/anio={year}/mes={month_str}/")
      
      # 2. Filtros por provincia
      for (nombre in names(filtros_provincia)) {
        provs <- filtros_provincia[[nombre]]
        df_f  <- df_completo$
          filter(pl$col("provincia")$is_in(provs))$
          select(setdiff(df_completo$columns, "provincia"))
        write_parquet_pl(df_f, nombre, nivel, estado_num, year, month)
        log_info("  OK {nombre}: {nivel} (provincias: {paste(provs, collapse=',')})")
      }
      
      # 3. España (agregado nacional)
      write_parquet_pl(aggregate_spain(df_completo, nivel), "espana", nivel, estado_num, year, month)
      log_info("  OK España: {nivel} agregado nacional")
      
      # 4. Totales CCAA (solo sectores)
      if (nivel == "sectores") {
        write_parquet_pl(process_ccaa_totals(df_completo, estado_num, year, month),
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
    df_out <- transform_fn(read_parquet_pl(ambito, dominio_in, estado, anio, mes))
    write_parquet_pl(df_out, ambito, dominio_out, estado, anio, mes)
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
  merge_parquet_range <- function(ambito, dominio, anio_inicio, anio_definitivo,
                                  anio_fin, meses = NULL, filename = "data_0.parquet",
                                  lazy = FALSE) {
    if (anio_inicio > anio_fin) stop("anio_inicio no puede ser mayor que anio_fin")
    if (is.null(meses)) meses <- 1:12 else meses <- .validate_meses(meses)
    
    label <- if (nchar(ambito) == 0L) "nacional" else ambito
    log_info("Agregando {label}/{dominio} {anio_inicio}-{anio_fin} (def hasta {anio_definitivo})")
    
    paths <- character(0)
    meta  <- list()
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
    
    # Leer fichero a fichero, añadir columnas garantizadas y apilar
    dfs <- vector("list", length(paths))
    for (i in seq_along(paths)) {
      d <- pl$read_parquet(paths[[i]])$
        with_columns(
          pl$lit(as.integer(meta[[i]]$estado))$cast(pl$Int32)$alias("estado"),
          pl$lit(as.integer(meta[[i]]$anio))$cast(pl$Int32)$alias("anio"),
          pl$lit(as.integer(meta[[i]]$mes))$cast(pl$Int32)$alias("mes")
        )
      dfs[[i]] <- d
    }
    
    # vstack requiere columnas compatibles; usar diagonal para fill = TRUE
    result <- do.call(function(...) pl$concat(list(...), how = "diagonal"), dfs)
    
    # lazy = TRUE devuelve LazyFrame, FALSE devuelve DataFrame
    if (lazy) result$lazy() else result
  }
  
  # API pública del store ----
  list(
    build_path               = build_path,
    exists                   = exists_parquet,
    read                     = read_parquet_pl,
    write                    = write_parquet_pl,
    csv_to_parquet           = csv_to_parquet,
    process_range            = process_range,
    process_derived_domain   = process_derived_domain,
    process_derived_pipeline = process_derived_pipeline,
    process_month_pipeline   = process_month_pipeline,
    merge_parquet_range      = merge_parquet_range
  )
}


#### Funciones de agregación ----
# Reciben y devuelven polars DataFrame

euros_taric <- function(df) {
  df$group_by(c("flujo","nivel_taric","cod_taric"))$
    agg(pl$col("euros")$sum()$alias("euros"))
}

euros_pais <- function(df) {
  df$filter(pl$col("nivel_sector_economico") == 1L)$
    group_by(c("flujo","pais"))$
    agg(pl$col("euros")$sum()$alias("euros"))
}

euros_taric_pais <- function(df) {
  df$group_by(c("flujo","nivel_taric","cod_taric","pais"))$
    agg(pl$col("euros")$sum()$alias("euros"))
}

kg_taric <- function(df) {
  df$group_by(c("flujo","nivel_taric","cod_taric"))$
    agg(pl$col("kilogramos")$sum()$alias("kilogramos"))
}

kg_pais <- function(df) {
  df$filter(pl$col("nivel_taric") == 1L)$
    group_by(c("flujo","pais"))$
    agg(pl$col("kilogramos")$sum()$alias("kilogramos"))
}

kg_taric_pais <- function(df) {
  df$group_by(c("flujo","nivel_taric","cod_taric","pais"))$
    agg(pl$col("kilogramos")$sum()$alias("kilogramos"))
}

euros_sectores <- function(df) {
  df$group_by(c("flujo","nivel_sector_economico","cod_sector_economico"))$
    agg(pl$col("euros")$sum()$alias("euros"))
}

euros_sectores_pais <- function(df) {
  df$group_by(c("flujo","nivel_sector_economico","cod_sector_economico","pais"))$
    agg(pl$col("euros")$sum()$alias("euros"))
}


#### Esquemas para alinear columnas ----

TARIC_EUROS_SCHEMA <- list(
  flujo=pl$Int32, anio=pl$Int32, mes=pl$Int32, estado=pl$Int32, pais=pl$Int32,
  nivel_taric=pl$Int32, cod_taric=pl$Float64, euros=pl$Float64
)
TARIC_KG_SCHEMA <- list(
  flujo=pl$Int32, anio=pl$Int32, mes=pl$Int32, estado=pl$Int32, pais=pl$Int32,
  nivel_taric=pl$Int32, cod_taric=pl$Float64, kilogramos=pl$Float64
)
SECTORES_EUROS_SCHEMA <- list(
  flujo=pl$Int32, anio=pl$Int32, mes=pl$Int32, estado=pl$Int32, pais=pl$Int32,
  nivel_sector_economico=pl$Int32, cod_sector_economico=pl$Utf8, euros=pl$Float64
)

# Alinea polars DataFrame al esquema: añade columnas faltantes, fuerza tipos, reordena
.align_schema_pl <- function(df, schema) {
  col_names <- names(schema)
  existing  <- df$columns
  
  # Añadir columnas faltantes como nulls con tipo correcto
  exprs_add <- lapply(setdiff(col_names, existing), function(col) {
    pl$lit(NULL)$cast(schema[[col]])$alias(col)
  })
  if (length(exprs_add) > 0L)
    df <- df$with_columns(exprs_add)
  
  # Forzar tipos de todas las columnas del schema
  exprs_cast <- lapply(col_names, function(col) {
    pl$col(col)$cast(schema[[col]])
  })
  df <- df$with_columns(exprs_cast)
  
  # Reordenar columnas
  df$select(col_names)
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
      nodeterminado=0L,  andalucia=1L,   aragon=2L,    asturias=3L,
      baleares=4L,       canarias=5L,    cantabria=17L, castillalamancha=7L,
      castillayleon=6L,  cataluna=8L,    galicia=10L,   madrid=15L,
      murcia=11L,        navarra=12L,    paisvasco=14L, rioja=16L,
      valencia=13L,      ceuta=51L,      melilla=52L,   espana=99L
    )
) {
  library(duckdb)
  
  ambitos <- names(mapeo_ambito_cod_comunidad)
  datalake_dir <- fs::path(datalake_dir)
  fs::dir_create(dataoutput, recurse=TRUE)
  
  # DuckDB procesa todo en disco, sin cargar en RAM
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(duckdb::dbDisconnect(con, shutdown=TRUE), add=TRUE)
  
  DBI::dbExecute(con, "SET memory_limit = '4GB'")
  DBI::dbExecute(con, "SET threads = 4")
  
  nombres_guardados <- character(0)
  
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
    paste0("[", paste0("'", paths, "'", collapse=", "), "]")
  }
  
  query_to_parquet <- function(sql_query, out_path) {
    fs::dir_create(fs::path_dir(out_path), recurse=TRUE)
    write_sql <- sprintf(
      "COPY (SELECT * RENAME (anio AS \"año\") FROM (%s)) TO '%s' (FORMAT PARQUET)",
      sql_query, out_path
    )
    DBI::dbExecute(con, write_sql)
  }
  
  domain_query <- function(glob, extra_cols_sql, schema_cols) {
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
  
  # Columnas finales por schema
  taric_euros_cols        <- "CAST(flujo AS INTEGER) AS flujo, CAST(anio AS INTEGER) AS anio,
                               CAST(mes AS INTEGER) AS mes, CAST(estado AS INTEGER) AS estado,
                               CAST(pais AS INTEGER) AS pais, CAST(nivel_taric AS INTEGER) AS nivel_taric,
                               CAST(cod_taric AS DOUBLE) AS cod_taric, CAST(euros AS DOUBLE) AS euros"
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
  
  glob_totales <- build_glob("", "totalesccaa", anio_ini, anio_fin, anio_def, meses_list)
  
  for (ambito in ambitos) {
    log_info("Procesando ambito: {ambito} ({which(ambitos==ambito)}/{length(ambitos)})")
    cod_com <- mapeo_ambito_cod_comunidad[[ambito]]
    
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

aplicar_filtro <- function(df, col, valores) {
  if (!is.null(valores) && col %in% df$columns) {
    list(
      df    = df$filter(pl$col(col)$is_in(valores)),
      label = sprintf("%s: [%s]", col, paste(valores, collapse = ", "))
    )
  } else {
    list(df = df, label = NULL)
  }
}