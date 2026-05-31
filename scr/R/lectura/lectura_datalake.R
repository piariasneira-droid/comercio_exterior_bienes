  #### Entorno ----
  library(arrow)
  library(data.table)
  library(fs)
  library(logger)
  source("./scr/R/microdata/auxiliar/datalake.R")
  
  out_dir <- "./data/rawparquet"
  store   <- ParquetStore(out_dir)
  
  #### Parámetros ----
  amb     <- "totalesccaa"
  dom     <- ""
  ano_ini <- 1995L
  ano_fin <- 2025L
  ano_def <- 2023L
  meses   <- c(9L, 10L, 11L, 12L) 
  
  # Filtros (NULL = sin filtro)
  filtro_flujo                <- c(1L)
  filtro_pais                 <- c(0L, 1L, 400L)
  filtro_cod_taric            <- c(0L)
  filtro_cod_sector_economico <- NULL
  
  #### Lectura Parquet individual ----
  df_individual <- store$read(
    ambito  = amb,
    dominio = dom,
    estado  = 0L,
    anio    = ano_fin,
    mes     = 12L
  )
  
  str(df_individual)

#### Lectura rango temporal ----
df <- store$merge_parquet_range(
  ambito          = amb,
  dominio         = dom,
  anio_inicio     = ano_ini,
  anio_definitivo = ano_def,
  anio_fin        = ano_fin,
  meses           = meses,
  lazy            = FALSE
)


#### Lectura con filtro
cols              <- names(df)
df_filtered       <- copy(df)
filtros_aplicados <- character(0)

for (par in list(
  list(df_filtered, "flujo",                filtro_flujo),
  list(df_filtered, "pais",                 filtro_pais),
  list(df_filtered, "cod_taric",            filtro_cod_taric),
  list(df_filtered, "cod_sector_economico", filtro_cod_sector_economico)
)) {
  res               <- aplicar_filtro(par[[1L]], par[[2L]], par[[3L]])
  df_filtered       <- res$dt
  filtros_aplicados <- c(filtros_aplicados, res$label)
}

#### Resumen ----
cat(sprintf("\nColumnas: %s\n", paste(cols, collapse = ", ")))
cat("\nFiltros aplicados:\n")
if (length(filtros_aplicados) > 0) {
  for (f in filtros_aplicados) cat(sprintf("  ✓ %s\n", f))
} else {
  cat("  ⊘ Ninguno\n")
}

cat(sprintf("\n%s\n📊 RESUMEN\n%s\n", strrep("=", 60), strrep("=", 60)))
cat(sprintf("Registros totales:   %s\n", format(nrow(df),          big.mark = ",")))
cat(sprintf("Registros filtrados: %s\n", format(nrow(df_filtered), big.mark = ",")))
cat(sprintf("Periodo:             %d-%02d a %d-%02d\n",
            min(df_filtered$anio), min(df_filtered$mes),
            max(df_filtered$anio), max(df_filtered$mes)))

cat("\n🔍 Primeras 10 filas:\n")
print(head(df_filtered, 10))