# main_ETL.R
# Procesamiento de datos de comercio exterior Madrid vs España por sectores

# Entorno ----
source("./scr/R/recuadro_anual_sectores/parametros.r")
source("./scr/R/recuadro_anual_sectores/auxiliar/funciones_etl.r")

# Metadatos ----
meta_sec  <- leer_excel_sheets(params$path_sec,  c("sectores",  "agregaciones"))
meta_pais <- leer_excel_sheets(params$path_pais, c("paises",    "regiones"))

# Carga datos ----
dsmad <- arrow::open_dataset(params$path_mad)
dsesp <- arrow::open_dataset(params$path_esp)

# Procesamiento ----

## Totales anuales ----
totales_anuales <- procesar_totales_anuales(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  parametros = params
)

## Sectores ----

### Datacomex ----
tabla_sectores_aux <- tabla_sectores_datacomex(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_sec     = meta_sec,
  parametros = params
)

totalesanho <- extraer_totales_de_tabla(tabla_sectores_aux)

df_sectores <- procesar_salida_sectores(
  tabla        = tabla_sectores_aux,
  listatotales = totalesanho
)

### Full ----
df_sec <- tabla_sectores_f(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_sec     = meta_sec,
  parametros = params
)

### Evol ----
tabla_evol_sec_raw <- sectores_evol(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_sec     = meta_sec,
  parametros = params
)

df_evol_sec <- procesar_evol_sectores(
  tabla_evol_sec_raw,
  ano_base = params$anho_idx
)

## Paises ----
### Datacomex ----
tabla_paises_aux <- tabla_paises_datacomex(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_paises  = meta_pais,
  totales    = totalesanho,
  parametros = params
)

df_paises <- procesar_salida_paises(
  tabla        = tabla_paises_aux,
  listatotales = totalesanho
)

### Full ----
df_country <- tabla_paises_f(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_paises  = meta_pais,
  parametros = params
)

### Evol ----
tabla_evol_pais_raw <- paises_evol(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_paises  = meta_pais,
  parametros = params
)

df_evol_pais <- procesar_evol_paises(
  tabla_evol_pais_raw,
  ano_base = params$anho_idx
)

# Bump ----
df_evol_secfull <- sectores_evol_f(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_sec     = meta_sec,
  parametros = params
)

df_evol_countryfull <- paises_evol_f(
  ds_mad     = dsmad,
  ds_esp     = dsesp,
  df_paises  = meta_pais,
  parametros = params
)

# Salidas Excel ----
## Nombres de archivo ----
m_start <- min(params$meses)
m_end   <- max(params$meses)

nombre_totales   <- if (m_start == m_end) {
  sprintf("evolucion_anual_%d_%d_%02d.xlsx",        params$ano_ini, params$anho, m_start)
} else {
  sprintf("evolucion_anual_%d_%d_%02d-%02d.xlsx",   params$ano_ini, params$anho, m_start, m_end)
}

nombre_sec_dcx   <- if (m_start == m_end) {
  sprintf("analisis_comercio_sectores_%d_%02d.xlsx",      params$anho, m_start)
} else {
  sprintf("analisis_comercio_sectores_%d_%02d-%02d.xlsx", params$anho, m_start, m_end)
}

nombre_pais_dcx  <- if (m_start == m_end) {
  sprintf("analisis_comercio_paises_%d_%02d.xlsx",        params$anho, m_start)
} else {
  sprintf("analisis_comercio_paises_%d_%02d-%02d.xlsx",   params$anho, m_start, m_end)
}

nombre_evol_sec  <- sprintf("evolucion_sectores_%d_%d.xlsx", params$anho_idx, params$anho)
nombre_evol_pais <- sprintf("evolucion_paises_%d_%d.xlsx",   params$anho_idx, params$anho)

nombre_full_sec  <- if (m_start == m_end) {
  sprintf("sectores_full_%d_%02d.xlsx",       params$anho, m_start)
} else {
  sprintf("sectores_full_%d_%02d-%02d.xlsx",  params$anho, m_start, m_end)
}

nombre_full_pais <- if (m_start == m_end) {
  sprintf("paises_full_%d_%02d.xlsx",         params$anho, m_start)
} else {
  sprintf("paises_full_%d_%02d-%02d.xlsx",    params$anho, m_start, m_end)
}

## Totales anuales ----
write_formatted_xlsx(
  data       = totales_anuales,
  parametros = params,
  file_name  = nombre_totales,
  int_cols   = c("año"),
  pct_cols   = c(
    "exp_mad_pct", "imp_mad_pct", "tasa_cobertura_mad", "tasa_cobertura_esp",
    "exp_mad_tva", "imp_mad_tva", "exp_esp_tva", "imp_esp_tva"
  )
)

## Sectores datacomex ----
write_formatted_xlsx(
  data       = df_sectores,
  parametros = params,
  file_name  = nombre_sec_dcx,
  int_cols   = c("orden", "niv"),
  pct_cols   = c(
    "exp_mad_pct", "exp_mad_tva", "exp_mad_contrib", "exp_mad_vs_esp",
    "imp_mad_pct", "imp_mad_tva", "imp_mad_contrib", "imp_mad_vs_esp",
    "tasa_cob_mad",
    "exp_esp_pct", "exp_esp_tva", "exp_esp_contrib",
    "imp_esp_pct", "imp_esp_tva", "imp_esp_contrib",
    "tasa_cob_esp"
  )
)

## Paises datacomex ----
write_formatted_xlsx(
  data       = df_paises,
  parametros = params,
  file_name  = nombre_pais_dcx,
  int_cols   = c("orden", "niv"),
  pct_cols   = c(
    "exp_mad_pct", "exp_mad_tva", "exp_mad_contrib", "exp_mad_vs_esp",
    "imp_mad_pct", "imp_mad_tva", "imp_mad_contrib", "imp_mad_vs_esp",
    "tasa_cob_mad",
    "exp_esp_pct", "exp_esp_tva", "exp_esp_contrib",
    "imp_esp_pct", "imp_esp_tva", "imp_esp_contrib",
    "tasa_cob_esp"
  )
)

## Evolución sectores y países ----
pct_cols_evol_sec  <- grep("_pct_|_vs_esp_", names(df_evol_sec),  value = TRUE)
idx_cols_evol_sec  <- grep("_idx_",           names(df_evol_sec),  value = TRUE)

pct_cols_evol_pais <- grep("_pct_|_vs_esp_", names(df_evol_pais), value = TRUE)
idx_cols_evol_pais <- grep("_idx_",           names(df_evol_pais), value = TRUE)

write_formatted_xlsx(
  data       = df_evol_sec,
  parametros = params,
  file_name  = nombre_evol_sec,
  int_cols   = c("orden", "niv"),
  idx_cols   = idx_cols_evol_sec,
  pct_cols   = pct_cols_evol_sec
)

write_formatted_xlsx(
  data       = df_evol_pais,
  parametros = params,
  file_name  = nombre_evol_pais,
  int_cols   = c("orden", "niv", "cod"),
  idx_cols   = idx_cols_evol_pais,
  pct_cols   = pct_cols_evol_pais
)

## Tablas full sectores y países ----
write_formatted_xlsx(
  data       = df_sec,
  parametros = params,
  file_name  = nombre_full_sec,
  int_cols   = c("orden", "niv"),
  pct_cols   = c("tva_exp", "tva_imp")
)

write_formatted_xlsx(
  data       = df_country,
  parametros = params,
  file_name  = nombre_full_pais,
  int_cols   = c("cod"),
  pct_cols   = c("tva_exp", "tva_imp")
)

# Limpieza memoria ----
limpiar_memoria()