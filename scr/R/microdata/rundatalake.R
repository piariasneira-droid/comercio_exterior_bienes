#### Entorno trabajo ----
library(arrow)
library(data.table)
library(fs)
library(logger)
library(duckdb)

source("./scr/R/microdata/auxiliar/datalake.R")
source("./scr/R/microdata/auxiliar/variables.R")

#### Parámetros ----
# Rutas
raw_base_dir <- "data/raw"
out_dir      <- "data/rawparquet"
dataoutput   <- "data/interim"

# Años
ano_ini         <- 1995L
ano_fin_def     <- 2023L
ano_ini_join    <- 2021L
ano_fin         <- 2026L
ultimo_mes_prov <- 3L

##### Rango completo: CSV -> parquet base + dominios derivados ----
run_csv_to_parquet(
  raw_base_dir      = raw_base_dir,
  out_dir           = out_dir,
  ano_ini           = ano_ini,
  ano_fin_def       = ano_fin_def,
  ano_fin           = ano_fin,
  ultimo_mes_prov   = ultimo_mes_prov,
  filtros_provincia = filtros_provincia
)

#### Un solo mes (base + derivados) ----
store <- ParquetStore(out_dir)
store$csv_to_parquet(
  year=ano_fin, 
  month=ultimo_mes_prov, 
  version="prov",
  raw_base_dir=raw_base_dir, 
  filtros_provincia=filtros_provincia)

store$process_month_pipeline(
  year=ano_fin, 
  month=ultimo_mes_prov, 
  estado=0L,
  pipeline=pipeline, 
  ambitos=ambitos
)

##### Un solo año ----
store$process_range(
  year_start=ano_fin_def, 
  year_end=ano_fin_def, 
  version="def",
  raw_base_dir=raw_base_dir, 
  filtros_provincia=filtros_provincia
)
store$process_derived_pipeline(
  pipeline=pipeline, 
  ambitos=ambitos,
  year_start=ano_fin_def, 
  year_end=ano_fin_def, 
  estado=1L
)

#### Unir parquets -> archivos finales para análisis ----
run_join_parquet(
  datalake_dir               = out_dir,
  dataoutput                 = dataoutput,
  anio_ini                   = ano_ini,
  anio_def                   = ano_fin_def,
  anio_fin                   = ano_fin,
  mapeo_ambito_cod_comunidad = mapeo_ambito_cod_comunidad
)