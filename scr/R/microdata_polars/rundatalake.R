#### rundatalake.R ─ Ejecuta el pipeline completo en R puro con Polars ────────
#
#  Estructura de ficheros:
#    scr/R/microdata_polars/auxiliar/datalake.R   ← lógica Polars en R
#    scr/R/microdata_polars/auxiliar/variables.R  ← parámetros de negocio
# ─────────────────────────────────────────────────────────────────────────────
library(polars)
source("./scr/R/microdata_polars/auxiliar/datalake.R")
source("./scr/R/microdata_polars/auxiliar/variables.R")

#### Parámetros ────────────────────────────────────────────────────────────────
raw_base_dir    <- "data/raw"
out_dir         <- "data/rawparquetpy"
dataoutput      <- "data/interimpy"
ano_ini         <- 1995L
ano_fin_def     <- 2023L
ano_fin         <- 2025L
ultimo_mes_prov <- 12L

#### 1. Rango completo: CSV → parquet base + dominios derivados ────────────────
run_csv_to_parquet(
  raw_base_dir      = raw_base_dir,
  out_dir           = out_dir,
  ano_ini           = ano_ini,
  ano_fin_def       = ano_fin_def,
  ano_fin           = ano_fin,
  ultimo_mes_prov   = ultimo_mes_prov,
  filtros_provincia = filtros_provincia,
  skip_existing     = FALSE
)

#### 2. Un solo mes (base + derivados) ─────────────────────────────────────────
store <- ParquetStore(out_dir)

store$csv_to_parquet(
  year              = ano_fin,
  month             = ultimo_mes_prov,
  version           = "prov",
  raw_base_dir      = raw_base_dir,
  filtros_provincia = filtros_provincia
)

store$process_month_pipeline(
  year     = ano_fin,
  month    = ultimo_mes_prov,
  estado   = 0L,
  pipeline = pipeline,
  ambitos  = ambitos
)

#### 3. Un solo año ────────────────────────────────────────────────────────────
store <- ParquetStore(out_dir)

store$process_range(
  year_start        = ano_fin_def,
  year_end          = ano_fin_def,
  version           = "def",
  raw_base_dir      = raw_base_dir,
  filtros_provincia = filtros_provincia
)

store$process_derived_pipeline(
  pipeline   = pipeline,
  ambitos    = ambitos,
  year_start = ano_fin_def,
  year_end   = ano_fin_def,
  estado     = 1L
)

#### 4. Unir parquets → archivos finales de análisis ───────────────────────────
run_join_parquet(
  datalake_dir               = out_dir,
  dataoutput                 = dataoutput,
  anio_ini                   = ano_ini,
  anio_def                   = ano_fin_def,
  anio_fin                   = ano_fin,
  mapeo_ambito_cod_comunidad = mapeo_ambito_cod_comunidad
)
