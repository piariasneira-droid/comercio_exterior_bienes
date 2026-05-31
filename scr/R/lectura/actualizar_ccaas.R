library(arrow)
library(data.table)
library(fs)
library(logger)
source("./scr/R/microdata/auxiliar/datalake.R")

out_dir <- "./data/rawparquet"
out_dir_interim <- "./data/interim"
store   <- ParquetStore(out_dir)

ano_ini         <- 1995L
ano_fin_def     <- 2023L
ano_fin         <- 2025L
ultimo_mes_prov <- 12L

# ---- Mapeo provincia -> cod_comunidad desde PROVINCIAS.csv ----
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

# ---- Función ----
process_ccaa_totals_ext <- function(dt, estado_val, anio_val, mes_val) {
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

# ---- Ejecución: definitivos ----
for (anio in ano_ini:ano_fin_def) {
  for (mes in 1:12) {
    tryCatch({
      dt      <- store$read("", "sectores", 1L, anio, mes)
      dt_ccaa <- process_ccaa_totals_ext(dt, 1L, anio, mes)
      store$write(dt_ccaa, "", "totalesccaa", 1L, anio, mes)
      log_info("OK def {anio}-{sprintf('%02d', mes)}")
    }, error = function(e) log_warn("Saltando def {anio}-{sprintf('%02d',mes)}: {conditionMessage(e)}"))
  }
}

# ---- Ejecución: provisionales ----
for (anio in (ano_fin_def + 1L):ano_fin) {
  for (mes in 1:ultimo_mes_prov) {
    tryCatch({
      dt      <- store$read("", "sectores", 0L, anio, mes)
      dt_ccaa <- process_ccaa_totals_ext(dt, 0L, anio, mes)
      store$write(dt_ccaa, "", "totalesccaa", 0L, anio, mes)
      log_info("OK prov {anio}-{sprintf('%02d', mes)}")
    }, error = function(e) log_warn("Saltando prov {anio}-{sprintf('%02d',mes)}: {conditionMessage(e)}"))
  }
}

# ---- Join final ----
run_join_parquet(
  datalake_dir               = out_dir,
  dataoutput                 = out_dir_interim,
  anio_ini                   = ano_ini,
  anio_def                   = ano_fin_def,
  anio_fin                   = ano_fin,
  mapeo_ambito_cod_comunidad = mapeo_ambito_cod_comunidad
)