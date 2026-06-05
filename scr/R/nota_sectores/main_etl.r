# main_ETL.R
# Procesamiento de datos de comercio exterior Madrid vs España por sectores

# Entorno ----
# source("./scr/R/nota_sectores/procfun/parametros.r")
source("./scr/R/nota_sectores/procfun/funciones_etl.r")

# Metadatos ----
meta_sec  <- .leer_excel_sheets(paramets$path_sec,  c("sectores",  "agregaciones"))
meta_pais <- .leer_excel_sheets(paramets$path_pais, c("paises",    "regiones"))
meta_ccaa <- as.data.table(read.xlsx(paramets$path_mccaa))

# Carga datos ----
dsmad <- arrow::open_dataset(paramets$path_mad)
dsesp <- arrow::open_dataset(paramets$path_esp)
dfccaas <- data.table::fread(paramets$path_ccaa, drop= c("estado","dolares"))

# CCAAs ----
df_ccaas <- .dataframe_general(
  df = dfccaas,
  para = paramets,
  meta = meta_ccaa)
lista_esp <- as.list(df_ccaas[Etiqueta =="ESP"])
lista_mad <- as.list(df_ccaas[Etiqueta =="CM"])
df_ccaa_amp <- .read_processed_data(paramets$path_ccaafull, "mes") %>% 
  filter(ccaa %in% c(paramets$reg1, paramets$reg2))

# Procesamiento mes ----

## Totales anuales ----
totales_anuales <- .procesar_totales_anuales(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  parametros = paramets
)

## Sectores ----
### Datacomex ----
tabla_sectores_aux <- .tabla_sectores_datacomex(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_sec     = meta_sec,
  parametros = paramets
)

totalesanho <- .extraer_totales_de_tabla(tabla_sectores_aux)

df_sectores <- .procesar_salida_sectores(
  tabla        = tabla_sectores_aux,
  listatotales = totalesanho
)

### Full ----
df_sec <- .tabla_sectores_f(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_sec     = meta_sec,
  parametros = paramets
)

### Evol ----
tabla_evol_sec_raw <- .sectores_evol(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_sec     = meta_sec,
  parametros = paramets
)

df_evol_sec <- .procesar_evol_sectores(
  tabla_evol_sec_raw,
  ano_base = paramets$anho_idx
)

## Paises ----
### Datacomex ----
tabla_paises_aux <- .tabla_paises_datacomex(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_paises  = meta_pais,
  totales    = totalesanho,
  parametros = paramets
)

df_paises <- .procesar_salida_paises(
  tabla        = tabla_paises_aux,
  listatotales = totalesanho
)

### Full ----
df_country <- .tabla_paises_f(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_paises  = meta_pais,
  parametros = paramets
)

### Evol ----
tabla_evol_pais_raw <- .paises_evol(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_paises  = meta_pais,
  parametros = paramets
)

df_evol_pais <- .procesar_evol_paises(
  tabla_evol_pais_raw,
  ano_base = paramets$anho_idx
)

### Bump ----
df_evol_secfull <- .sectores_evol_f(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_sec     = meta_sec,
  parametros = paramets
)

df_evol_countryfull <- .paises_evol_f(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_paises  = meta_pais,
  parametros = paramets
)

## Contribuciones datacomex ----
### Madrid ----
df_contrib_paises_exp_informe <- .df_plot_barras_contribucion_sectores_datacomex(
  df         = df_evol_countryfull,
  para       = paramets,
  totalesf   = totalesanho,
  flujo      = "exp",
  region     = "mad",
  meta       = meta_sec,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_paises_imp_informe <- .df_plot_barras_contribucion_sectores_datacomex(
  df         = df_evol_countryfull,
  para       = paramets,
  totalesf   = totalesanho,
  flujo      = "imp",
  region     = "mad",
  meta       = meta_sec,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_sec_exp_informe <- .df_plot_barras_contribucion_paises_datacomex(
  df         = df_sectores[!orden %in% paramets$fil_sectores_plot],
  para       = paramets,
  totalesf   = totalesanho,
  flujo      = "exp",
  region     = "mad",
  metas      = meta_sec,
  metap      = meta_pais,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_sec_imp_informe <- .df_plot_barras_contribucion_paises_datacomex(
  df         = df_sectores[!orden %in% paramets$fil_sectores_plot],
  para       = paramets,
  totalesf   = totalesanho,
  flujo      = "imp",
  region     = "mad",
  metas      = meta_sec,
  metap      = meta_pais,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

### España ----
df_contrib_paises_exp_informe_esp <- .df_plot_barras_contribucion_sectores_datacomex(
  df         = df_evol_countryfull,
  para       = paramets,
  totalesf   = totalesanho,
  flujo      = "exp",
  region     = "esp",
  meta       = meta_sec,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_paises_imp_informe_esp <- .df_plot_barras_contribucion_sectores_datacomex(
  df         = df_evol_countryfull,
  para       = paramets,
  totalesf   = totalesanho,
  flujo      = "imp",
  region     = "esp",
  meta       = meta_sec,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_sec_exp_informe_esp <- .df_plot_barras_contribucion_paises_datacomex(
  df         = df_sectores[!orden %in% paramets$fil_sectores_plot],
  para       = paramets,
  totalesf   = totalesanho,
  flujo      = "exp",
  region     = "esp",
  metas      = meta_sec,
  metap      = meta_pais,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_sec_imp_informe_esp <- .df_plot_barras_contribucion_paises_datacomex(
  df         = df_sectores[!orden %in% paramets$fil_sectores_plot],
  para       = paramets,
  totalesf   = totalesanho,
  flujo      = "imp",
  region     = "esp",
  metas      = meta_sec,
  metap      = meta_pais,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

# Procesamiento acumulado ----
paux_mes <- paramets$mes
paramets$mes <- 1:max(paux_mes)

## Totales anuales ----
totales_anuales_acu <- .procesar_totales_anuales(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  parametros = paramets
)

## Sectores ----
### Datacomex ----
tabla_sectores_aux_acu <- .tabla_sectores_datacomex(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_sec     = meta_sec,
  parametros = paramets
)

totalesanho_acu <- .extraer_totales_de_tabla(tabla_sectores_aux_acu)

df_sectores_acu <- .procesar_salida_sectores(
  tabla        = tabla_sectores_aux_acu,
  listatotales = totalesanho_acu
)

### Full ----
df_sec_acu <- .tabla_sectores_f(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_sec     = meta_sec,
  parametros = paramets
)

### Evol ----
tabla_evol_sec_raw_acu <- .sectores_evol(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_sec     = meta_sec,
  parametros = paramets
)

df_evol_sec_acu <- .procesar_evol_sectores(
  tabla_evol_sec_raw_acu,
  ano_base = paramets$anho_idx
)

## Paises ----
### Datacomex ----
tabla_paises_aux_acu <- .tabla_paises_datacomex(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_paises  = meta_pais,
  totales    = totalesanho_acu,
  parametros = paramets
)

df_paises_acu <- .procesar_salida_paises(
  tabla        = tabla_paises_aux_acu,
  listatotales = totalesanho_acu
)

### Full ----
df_country_acu <- .tabla_paises_f(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_paises  = meta_pais,
  parametros = paramets
)

### Evol ----
tabla_evol_pais_raw_acu <- .paises_evol(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_paises  = meta_pais,
  parametros = paramets
)

df_evol_pais_acu <- .procesar_evol_paises(
  tabla_evol_pais_raw_acu,
  ano_base = paramets$anho_idx
)

### Bump ----
df_evol_secfull_acu <- .sectores_evol_f(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_sec     = meta_sec,
  parametros = paramets
)

df_evol_countryfull_acu <- .paises_evol_f(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_paises  = meta_pais,
  parametros = paramets
)

## Contribuciones datacomex ----
### Madrid ----
df_contrib_paises_exp_informe_acu <- .df_plot_barras_contribucion_sectores_datacomex(
  df         = df_evol_countryfull_acu,
  para       = paramets,
  totalesf   = totalesanho_acu,
  flujo      = "exp",
  region     = "mad",
  meta       = meta_sec,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_paises_imp_informe_acu <- .df_plot_barras_contribucion_sectores_datacomex(
  df         = df_evol_countryfull_acu,
  para       = paramets,
  totalesf   = totalesanho_acu,
  flujo      = "imp",
  region     = "mad",
  meta       = meta_sec,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_sec_exp_informe_acu <- .df_plot_barras_contribucion_paises_datacomex(
  df         = df_sectores_acu[!orden %in% paramets$fil_sectores_plot],
  para       = paramets,
  totalesf   = totalesanho_acu,
  flujo      = "exp",
  region     = "mad",
  metas      = meta_sec,
  metap      = meta_pais,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_sec_imp_informe_acu <- .df_plot_barras_contribucion_paises_datacomex(
  df         = df_sectores_acu[!orden %in% paramets$fil_sectores_plot],
  para       = paramets,
  totalesf   = totalesanho_acu,
  flujo      = "imp",
  region     = "mad",
  metas      = meta_sec,
  metap      = meta_pais,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

### España ----
df_contrib_paises_exp_informe_esp_acu <- .df_plot_barras_contribucion_sectores_datacomex(
  df         = df_evol_countryfull_acu,
  para       = paramets,
  totalesf   = totalesanho_acu,
  flujo      = "exp",
  region     = "esp",
  meta       = meta_sec,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_paises_imp_informe_esp_acu <- .df_plot_barras_contribucion_sectores_datacomex(
  df         = df_evol_countryfull_acu,
  para       = paramets,
  totalesf   = totalesanho_acu,
  flujo      = "imp",
  region     = "esp",
  meta       = meta_sec,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_sec_exp_informe_esp_acu <- .df_plot_barras_contribucion_paises_datacomex(
  df         = df_sectores_acu[!orden %in% paramets$fil_sectores_plot],
  para       = paramets,
  totalesf   = totalesanho_acu,
  flujo      = "exp",
  region     = "esp",
  metas      = meta_sec,
  metap      = meta_pais,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_sec_imp_informe_esp_acu <- .df_plot_barras_contribucion_paises_datacomex(
  df         = df_sectores_acu[!orden %in% paramets$fil_sectores_plot],
  para       = paramets,
  totalesf   = totalesanho_acu,
  flujo      = "imp",
  region     = "esp",
  metas      = meta_sec,
  metap      = meta_pais,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

# Procesamiento año pasado ----
paramets$mes  <- 1:12
paramets$anho <- paramets$anho - 1L

## Totales anuales ----
totales_anuales_anopas <- .procesar_totales_anuales(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  parametros = paramets
)

## Sectores ----
### Datacomex ----
tabla_sectores_aux_anopas <- .tabla_sectores_datacomex(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_sec     = meta_sec,
  parametros = paramets
)

totalesanho_anopas <- .extraer_totales_de_tabla(tabla_sectores_aux_anopas)

df_sectores_anopas <- .procesar_salida_sectores(
  tabla        = tabla_sectores_aux_anopas,
  listatotales = totalesanho_anopas
)

### Full ----
df_sec_anopas <- .tabla_sectores_f(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_sec     = meta_sec,
  parametros = paramets
)

### Evol ----
tabla_evol_sec_raw_anopas <- .sectores_evol(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_sec     = meta_sec,
  parametros = paramets
)

df_evol_sec_anopas <- .procesar_evol_sectores(
  tabla_evol_sec_raw_anopas,
  ano_base = paramets$anho_idx
)

## Paises ----
### Datacomex ----
tabla_paises_aux_anopas <- .tabla_paises_datacomex(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_paises  = meta_pais,
  totales    = totalesanho_anopas,
  parametros = paramets
)

df_paises_anopas <- .procesar_salida_paises(
  tabla        = tabla_paises_aux_anopas,
  listatotales = totalesanho_anopas
)

### Full ----
df_country_anopas <- .tabla_paises_f(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_paises  = meta_pais,
  parametros = paramets
)

### Evol ----
tabla_evol_pais_raw_anopas <- .paises_evol(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_paises  = meta_pais,
  parametros = paramets
)

df_evol_pais_anopas <- .procesar_evol_paises(
  tabla_evol_pais_raw_anopas,
  ano_base = paramets$anho_idx
)

### Bump ----
df_evol_secfull_anopas <- .sectores_evol_f(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_sec     = meta_sec,
  parametros = paramets
)

df_evol_countryfull_anopas <- .paises_evol_f(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_paises  = meta_pais,
  parametros = paramets
)

## Contribuciones datacomex ----
### Madrid ----
df_contrib_paises_exp_informe_anopas <- .df_plot_barras_contribucion_sectores_datacomex(
  df         = df_evol_countryfull_anopas,
  para       = paramets,
  totalesf   = totalesanho_anopas,
  flujo      = "exp",
  region     = "mad",
  meta       = meta_sec,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_paises_imp_informe_anopas <- .df_plot_barras_contribucion_sectores_datacomex(
  df         = df_evol_countryfull_anopas,
  para       = paramets,
  totalesf   = totalesanho_anopas,
  flujo      = "imp",
  region     = "mad",
  meta       = meta_sec,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_sec_exp_informe_anopas <- .df_plot_barras_contribucion_paises_datacomex(
  df         = df_sectores_anopas[!orden %in% paramets$fil_sectores_plot],
  para       = paramets,
  totalesf   = totalesanho_anopas,
  flujo      = "exp",
  region     = "mad",
  metas      = meta_sec,
  metap      = meta_pais,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_sec_imp_informe_anopas <- .df_plot_barras_contribucion_paises_datacomex(
  df         = df_sectores_anopas[!orden %in% paramets$fil_sectores_plot],
  para       = paramets,
  totalesf   = totalesanho_anopas,
  flujo      = "imp",
  region     = "mad",
  metas      = meta_sec,
  metap      = meta_pais,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

### España ----
df_contrib_paises_exp_informe_esp_anopas <- .df_plot_barras_contribucion_sectores_datacomex(
  df         = df_evol_countryfull_anopas,
  para       = paramets,
  totalesf   = totalesanho_anopas,
  flujo      = "exp",
  region     = "esp",
  meta       = meta_sec,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_paises_imp_informe_esp_anopas <- .df_plot_barras_contribucion_sectores_datacomex(
  df         = df_evol_countryfull_anopas,
  para       = paramets,
  totalesf   = totalesanho_anopas,
  flujo      = "imp",
  region     = "esp",
  meta       = meta_sec,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_sec_exp_informe_esp_anopas <- .df_plot_barras_contribucion_paises_datacomex(
  df         = df_sectores_anopas[!orden %in% paramets$fil_sectores_plot],
  para       = paramets,
  totalesf   = totalesanho_anopas,
  flujo      = "exp",
  region     = "esp",
  metas      = meta_sec,
  metap      = meta_pais,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

df_contrib_sec_imp_informe_esp_anopas <- .df_plot_barras_contribucion_paises_datacomex(
  df         = df_sectores_anopas[!orden %in% paramets$fil_sectores_plot],
  para       = paramets,
  totalesf   = totalesanho_anopas,
  flujo      = "imp",
  region     = "esp",
  metas      = meta_sec,
  metap      = meta_pais,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)

# Restaurar paramets al estado original ----
paramets$anho <- paramets$anho + 1L
paramets$mes  <- paux_mes

# Salidas Excel ----
## Nombres de archivo ----
nombre_totales   <- sprintf("evolucion_anual_%d_%d_%02d.xlsx",        paramets$ano_ini, paramets$anho, paramets$mes)
nombre_sec_dcx   <- sprintf("analisis_comercio_sectores_%d_%02d.xlsx", paramets$anho, paramets$mes)
nombre_pais_dcx  <- sprintf("analisis_comercio_paises_%d_%02d.xlsx",   paramets$anho, paramets$mes)
nombre_evol_sec  <- sprintf("evolucion_sectores_%d_%d.xlsx",           paramets$anho_idx, paramets$anho)
nombre_evol_pais <- sprintf("evolucion_paises_%d_%d.xlsx",             paramets$anho_idx, paramets$anho)
nombre_full_sec  <- sprintf("sectores_full_%d_%02d.xlsx",              paramets$anho, paramets$mes)
nombre_full_pais <- sprintf("paises_full_%d_%02d.xlsx",                paramets$anho, paramets$mes)

## Totales anuales ----
.write_formatted_xlsx(
  data       = totales_anuales,
  parametros = paramets,
  file_name  = nombre_totales,
  int_cols   = c("año"),
  pct_cols   = c(
    "exp_mad_pct", "imp_mad_pct", "tasa_cobertura_mad", "tasa_cobertura_esp",
    "exp_mad_tva", "imp_mad_tva", "exp_esp_tva", "imp_esp_tva"
  ),
  extra_sheets = list(
    acu     = totales_anuales_acu,
    anopas  = totales_anuales_anopas
  )
)

## Sectores datacomex ----
pct_cols_sec_dcx <- c(
  "exp_mad_pct", "exp_mad_tva", "exp_mad_contrib", "exp_mad_vs_esp",
  "imp_mad_pct", "imp_mad_tva", "imp_mad_contrib", "imp_mad_vs_esp",
  "tasa_cob_mad",
  "exp_esp_pct", "exp_esp_tva", "exp_esp_contrib",
  "imp_esp_pct", "imp_esp_tva", "imp_esp_contrib",
  "tasa_cob_esp"
)

.write_formatted_xlsx(
  data       = df_sectores,
  parametros = paramets,
  file_name  = nombre_sec_dcx,
  int_cols   = c("orden", "niv"),
  pct_cols   = pct_cols_sec_dcx,
  extra_sheets = list(
    acu     = df_sectores_acu,
    anopas  = df_sectores_anopas
  )
)

## Paises datacomex ----
pct_cols_pais_dcx <- c(
  "exp_mad_pct", "exp_mad_tva", "exp_mad_contrib", "exp_mad_vs_esp",
  "imp_mad_pct", "imp_mad_tva", "imp_mad_contrib", "imp_mad_vs_esp",
  "tasa_cob_mad",
  "exp_esp_pct", "exp_esp_tva", "exp_esp_contrib",
  "imp_esp_pct", "imp_esp_tva", "imp_esp_contrib",
  "tasa_cob_esp"
)

.write_formatted_xlsx(
  data       = df_paises,
  parametros = paramets,
  file_name  = nombre_pais_dcx,
  int_cols   = c("orden", "niv"),
  pct_cols   = pct_cols_pais_dcx,
  extra_sheets = list(
    acu     = df_paises_acu,
    anopas  = df_paises_anopas
  )
)

## Evolución sectores y países ----
pct_cols_evol_sec  <- grep("_pct_|_vs_esp_", names(df_evol_sec),  value = TRUE)
idx_cols_evol_sec  <- grep("_idx_",           names(df_evol_sec),  value = TRUE)

pct_cols_evol_pais <- grep("_pct_|_vs_esp_", names(df_evol_pais), value = TRUE)
idx_cols_evol_pais <- grep("_idx_",           names(df_evol_pais), value = TRUE)

.write_formatted_xlsx(
  data       = df_evol_sec,
  parametros = paramets,
  file_name  = nombre_evol_sec,
  int_cols   = c("orden", "niv"),
  idx_cols   = idx_cols_evol_sec,
  pct_cols   = pct_cols_evol_sec,
  extra_sheets = list(
    acu     = df_evol_sec_acu,
    anopas  = df_evol_sec_anopas
  )
)

.write_formatted_xlsx(
  data       = df_evol_pais,
  parametros = paramets,
  file_name  = nombre_evol_pais,
  int_cols   = c("orden", "niv", "cod"),
  idx_cols   = idx_cols_evol_pais,
  pct_cols   = pct_cols_evol_pais,
  extra_sheets = list(
    acu     = df_evol_pais_acu,
    anopas  = df_evol_pais_anopas
  )
)

## Tablas full sectores y países ----
.write_formatted_xlsx(
  data       = df_sec,
  parametros = paramets,
  file_name  = nombre_full_sec,
  int_cols   = c("orden", "niv"),
  pct_cols   = c("tva_exp", "tva_imp"),
  extra_sheets = list(
    acu     = df_sec_acu,
    anopas  = df_sec_anopas
  )
)

.write_formatted_xlsx(
  data       = df_country,
  parametros = paramets,
  file_name  = nombre_full_pais,
  int_cols   = c("cod"),
  pct_cols   = c("tva_exp", "tva_imp"),
  extra_sheets = list(
    acu     = df_country_acu,
    anopas  = df_country_anopas
  )
)

## CCAAs ----
nombre_ccaas <- sprintf("comercio_exterior_bienes_ccaas_%02d_%d.xlsx", paramets$mes, paramets$anho)

.write_formatted_xlsx(
  data       = df_ccaas,
  parametros = paramets,
  file_name  = nombre_ccaas,
  int_cols   = c("Coddax",
                 "exp_euros_rank", "imp_euros_rank",
                 "exp_euros_acu_rank", "imp_euros_acu_rank",
                 "exp_euros_anoant_rank", "imp_euros_anoant_rank"),
  pct_cols   = c(
    "exp_euros_peso", "exp_euros_tva", "exp_euros_rep",
    "imp_euros_peso", "imp_euros_tva", "imp_euros_rep",
    "exp_euros_acu_peso", "exp_euros_acu_tva", "exp_euros_acu_rep",
    "imp_euros_acu_peso", "imp_euros_acu_tva", "imp_euros_acu_rep",
    "exp_euros_anoant_peso", "exp_euros_tva2", "exp_euros_tva2_rep",
    "imp_euros_anoant_peso", "imp_euros_tva2", "imp_euros_tva2_rep"
  )
)

# Limpieza memoria ----
.limpiar_memoria()