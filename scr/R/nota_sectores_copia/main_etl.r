# main_etl.r
# ============================================================
# ETL principal — Madrid vs España por sectores
# ============================================================
# CAMBIOS respecto a main_etl.r:
#   1. Pipeline envuelto en .run_etl_periodo() → sin duplicación
#   2. paramets NUNCA se muta: cada periodo recibe su propia
#      copia local (modifyList), por lo que un error a mitad
#      no corrompe el estado global
#   3. Los sufijos de archivo vienen de parametros_bis.r
#      (sufijo_mes, sufijo_ytm, sufijo_anopas) → coherentes
#      tanto con mes escalar como con rango trimestral
# ============================================================

# Entorno ----
source("./scr/R/nota_sectores_bis/procfun/funciones_etl.r")

# Metadatos ----
meta_sec  <- .leer_excel_sheets(paramets$path_sec,  c("sectores",   "agregaciones"))
meta_pais <- .leer_excel_sheets(paramets$path_pais, c("paises",     "regiones"))
meta_ccaa <- as.data.table(read.xlsx(paramets$path_mccaa))

# Carga datasets Arrow (apertura en streaming, sin cargar en RAM) ----
dsmad   <- arrow::open_dataset(paramets$path_mad)
dsesp   <- arrow::open_dataset(paramets$path_esp)
dfccaas <- data.table::fread(paramets$path_ccaa, drop = c("estado", "dolares"))

# ============================================================
# Función interna: ejecuta el pipeline completo para un periodo
# ============================================================
# Parámetros:
#   para_periodo  : lista paramets con mes y anho YA ajustados
#   ds_mad, ds_esp: datasets Arrow (compartidos, no se copian)
#   meta_sec, meta_pais: metadatos (compartidos)
#
# Devuelve una lista con todos los data.frames del periodo.
# ============================================================
.run_etl_periodo <- function(para_periodo, ds_mad, ds_esp, meta_sec, meta_pais) {

  ## Totales anuales ----
  totales_anuales <- .procesar_totales_anuales(
    ds_mad     = ds_mad,
    ds_esp     = ds_esp,
    parametros = para_periodo
  )

  ## Sectores ----
  ### Datacomex ----
  tabla_sectores_aux <- .tabla_sectores_datacomex(
    ds_mad     = ds_mad,
    ds_esp     = ds_esp,
    df_sec     = meta_sec,
    parametros = para_periodo
  )
  totalesanho  <- .extraer_totales_de_tabla(tabla_sectores_aux)
  df_sectores  <- .procesar_salida_sectores(tabla_sectores_aux, totalesanho)

  ### Full ----
  df_sec <- .tabla_sectores_f(
    ds_mad     = ds_mad,
    ds_esp     = ds_esp,
    df_sec     = meta_sec,
    parametros = para_periodo
  )

  ### Evol ----
  tabla_evol_sec_raw <- .sectores_evol(
    ds_mad     = ds_mad,
    ds_esp     = ds_esp,
    df_sec     = meta_sec,
    parametros = para_periodo
  )
  df_evol_sec <- .procesar_evol_sectores(tabla_evol_sec_raw, ano_base = para_periodo$anho_idx)

  ## Países ----
  ### Datacomex ----
  tabla_paises_aux <- .tabla_paises_datacomex(
    ds_mad     = ds_mad,
    ds_esp     = ds_esp,
    df_paises  = meta_pais,
    totales    = totalesanho,
    parametros = para_periodo
  )
  df_paises <- .procesar_salida_paises(tabla_paises_aux, totalesanho)

  ### Full ----
  df_country <- .tabla_paises_f(
    ds_mad     = ds_mad,
    ds_esp     = ds_esp,
    df_paises  = meta_pais,
    parametros = para_periodo
  )

  ### Evol ----
  tabla_evol_pais_raw <- .paises_evol(
    ds_mad     = ds_mad,
    ds_esp     = ds_esp,
    df_paises  = meta_pais,
    parametros = para_periodo
  )
  df_evol_pais <- .procesar_evol_paises(tabla_evol_pais_raw, ano_base = para_periodo$anho_idx)

  ### Bump ----
  df_evol_secfull <- .sectores_evol_f(
    ds_mad     = ds_mad,
    ds_esp     = ds_esp,
    df_sec     = meta_sec,
    parametros = para_periodo
  )
  df_evol_countryfull <- .paises_evol_f(
    ds_mad     = ds_mad,
    ds_esp     = ds_esp,
    df_paises  = meta_pais,
    parametros = para_periodo
  )

  ## Contribuciones datacomex ----
  ### Madrid ----
  df_contrib_paises_exp_informe <- .df_plot_barras_contribucion_sectores_datacomex(
    df       = df_evol_countryfull, para = para_periodo, totalesf = totalesanho,
    flujo    = "exp", region = "mad", meta = meta_sec, dss_mad = ds_mad, dss_esp = ds_esp
  )
  df_contrib_paises_imp_informe <- .df_plot_barras_contribucion_sectores_datacomex(
    df       = df_evol_countryfull, para = para_periodo, totalesf = totalesanho,
    flujo    = "imp", region = "mad", meta = meta_sec, dss_mad = ds_mad, dss_esp = ds_esp
  )
  df_contrib_sec_exp_informe <- .df_plot_barras_contribucion_paises_datacomex(
    df       = df_sectores[!orden %in% para_periodo$fil_sectores_plot],
    para     = para_periodo, totalesf = totalesanho,
    flujo    = "exp", region = "mad", metas = meta_sec, metap = meta_pais,
    dss_mad  = ds_mad, dss_esp = ds_esp
  )
  df_contrib_sec_imp_informe <- .df_plot_barras_contribucion_paises_datacomex(
    df       = df_sectores[!orden %in% para_periodo$fil_sectores_plot],
    para     = para_periodo, totalesf = totalesanho,
    flujo    = "imp", region = "mad", metas = meta_sec, metap = meta_pais,
    dss_mad  = ds_mad, dss_esp = ds_esp
  )

  ### España ----
  df_contrib_paises_exp_informe_esp <- .df_plot_barras_contribucion_sectores_datacomex(
    df       = df_evol_countryfull, para = para_periodo, totalesf = totalesanho,
    flujo    = "exp", region = "esp", meta = meta_sec, dss_mad = ds_mad, dss_esp = ds_esp
  )
  df_contrib_paises_imp_informe_esp <- .df_plot_barras_contribucion_sectores_datacomex(
    df       = df_evol_countryfull, para = para_periodo, totalesf = totalesanho,
    flujo    = "imp", region = "esp", meta = meta_sec, dss_mad = ds_mad, dss_esp = ds_esp
  )
  df_contrib_sec_exp_informe_esp <- .df_plot_barras_contribucion_paises_datacomex(
    df       = df_sectores[!orden %in% para_periodo$fil_sectores_plot],
    para     = para_periodo, totalesf = totalesanho,
    flujo    = "exp", region = "esp", metas = meta_sec, metap = meta_pais,
    dss_mad  = ds_mad, dss_esp = ds_esp
  )
  df_contrib_sec_imp_informe_esp <- .df_plot_barras_contribucion_paises_datacomex(
    df       = df_sectores[!orden %in% para_periodo$fil_sectores_plot],
    para     = para_periodo, totalesf = totalesanho,
    flujo    = "imp", region = "esp", metas = meta_sec, metap = meta_pais,
    dss_mad  = ds_mad, dss_esp = ds_esp
  )

  ## Devolver todo empaquetado ----
  list(
    totales_anuales              = totales_anuales,
    totalesanho                  = totalesanho,
    df_sectores                  = df_sectores,
    df_sec                       = df_sec,
    df_evol_sec                  = df_evol_sec,
    df_paises                    = df_paises,
    df_country                   = df_country,
    df_evol_pais                 = df_evol_pais,
    df_evol_secfull              = df_evol_secfull,
    df_evol_countryfull          = df_evol_countryfull,
    df_contrib_paises_exp_informe    = df_contrib_paises_exp_informe,
    df_contrib_paises_imp_informe    = df_contrib_paises_imp_informe,
    df_contrib_sec_exp_informe       = df_contrib_sec_exp_informe,
    df_contrib_sec_imp_informe       = df_contrib_sec_imp_informe,
    df_contrib_paises_exp_informe_esp = df_contrib_paises_exp_informe_esp,
    df_contrib_paises_imp_informe_esp = df_contrib_paises_imp_informe_esp,
    df_contrib_sec_exp_informe_esp   = df_contrib_sec_exp_informe_esp,
    df_contrib_sec_imp_informe_esp   = df_contrib_sec_imp_informe_esp
  )
}

# ============================================================
# CCAAs  (no cambian entre periodos, se procesan una sola vez)
# ============================================================
df_ccaas <- .dataframe_general(df = dfccaas, para = paramets, meta = meta_ccaa)

df_ccaa_amp <- .read_processed_data(paramets$path_ccaafull, "mes") %>%
  filter(ccaa %in% c(paramets$reg1, paramets$reg2))

df_mad_rank <- df_ccaa_amp[
  ccaa == "Madrid, Comunidad de" &
    flujo %in% c("EXPORT", "IMPORT") &
    var == "mes" &
    temp %in% c("datoper", "acumulado") &
    Mes %in% paramets$mes
][, rank := frank(-valor, ties.method = "min"),
  by = .(flujo, temp, Mes)
][, .(
  valor_mes = valor[temp == "datoper"],
  rank_mes  = rank[temp == "datoper"],
  valor_ytd = valor[temp == "acumulado"],
  rank_ytd  = rank[temp == "acumulado"]
), by = .(ccaa, flujo, Mes, Año)]

df_esp_rank <- df_ccaa_amp[
  ccaa == "España" &
    flujo %in% c("EXPORT", "IMPORT") &
    var == "mes" &
    temp %in% c("datoper", "acumulado") &
    Mes %in% paramets$mes
][, rank := frank(-valor, ties.method = "min"),
  by = .(flujo, temp, Mes)
][, .(
  valor_mes = valor[temp == "datoper"],
  rank_mes  = rank[temp == "datoper"],
  valor_ytd = valor[temp == "acumulado"],
  rank_ytd  = rank[temp == "acumulado"]
), by = .(ccaa, flujo, Mes, Año)]

# ============================================================
# Ejecutar los tres periodos con copias limpias de paramets
# ============================================================

# --- Periodo "mes" (el análisis principal) ---
message("[ETL] Procesando periodo mes: ", paste(paramets$mes, collapse = ":"))
p_mes  <- modifyList(paramets, list(mes = paramets$mes))
res_mes <- .run_etl_periodo(p_mes, dsmad, dsesp, meta_sec, meta_pais)

# --- Periodo "acumulado" (enero → max(mes)) ---
message("[ETL] Procesando periodo acumulado: 1:", max(paramets$mes))
p_acu  <- modifyList(paramets, list(mes = 1L:max(paramets$mes)))
res_acu <- .run_etl_periodo(p_acu, dsmad, dsesp, meta_sec, meta_pais)

# --- Periodo "año pasado" (año-1, meses 1:12) ---
message("[ETL] Procesando anho pasado: ", paramets$anho - 1L)
p_anop <- modifyList(paramets, list(anho = paramets$anho - 1L, mes = 1L:12L))
res_anop <- .run_etl_periodo(p_anop, dsmad, dsesp, meta_sec, meta_pais)

# ============================================================
# Exponer los data.frames en el entorno global con los mismos
# nombres que usaban main_tablas.R, main_phtmls.R y main_texts.R
# → compatibilidad total: esos scripts NO necesitan cambios
# ============================================================

# -- Periodo mes --
list2env(res_mes,  envir = .GlobalEnv)

# -- Periodo acumulado (sufijo _acu) --
totales_anuales_acu              <- res_acu$totales_anuales
totalesanho_acu                  <- res_acu$totalesanho
df_sectores_acu                  <- res_acu$df_sectores
df_sec_acu                       <- res_acu$df_sec
df_evol_sec_acu                  <- res_acu$df_evol_sec
df_paises_acu                    <- res_acu$df_paises
df_country_acu                   <- res_acu$df_country
df_evol_pais_acu                 <- res_acu$df_evol_pais
df_evol_secfull_acu              <- res_acu$df_evol_secfull
df_evol_countryfull_acu          <- res_acu$df_evol_countryfull
df_contrib_paises_exp_informe_acu    <- res_acu$df_contrib_paises_exp_informe
df_contrib_paises_imp_informe_acu    <- res_acu$df_contrib_paises_imp_informe
df_contrib_sec_exp_informe_acu       <- res_acu$df_contrib_sec_exp_informe
df_contrib_sec_imp_informe_acu       <- res_acu$df_contrib_sec_imp_informe
df_contrib_paises_exp_informe_esp_acu <- res_acu$df_contrib_paises_exp_informe_esp
df_contrib_paises_imp_informe_esp_acu <- res_acu$df_contrib_paises_imp_informe_esp
df_contrib_sec_exp_informe_esp_acu   <- res_acu$df_contrib_sec_exp_informe_esp
df_contrib_sec_imp_informe_esp_acu   <- res_acu$df_contrib_sec_imp_informe_esp

# -- Periodo año pasado (sufijo _anopas) --
totales_anuales_anopas              <- res_anop$totales_anuales
totalesanho_anopas                  <- res_anop$totalesanho
df_sectores_anopas                  <- res_anop$df_sectores
df_sec_anopas                       <- res_anop$df_sec
df_evol_sec_anopas                  <- res_anop$df_evol_sec
df_paises_anopas                    <- res_anop$df_paises
df_country_anopas                   <- res_anop$df_country
df_evol_pais_anopas                 <- res_anop$df_evol_pais
df_evol_secfull_anopas              <- res_anop$df_evol_secfull
df_evol_countryfull_anopas          <- res_anop$df_evol_countryfull
df_contrib_paises_exp_informe_anopas    <- res_anop$df_contrib_paises_exp_informe
df_contrib_paises_imp_informe_anopas    <- res_anop$df_contrib_paises_imp_informe
df_contrib_sec_exp_informe_anopas       <- res_anop$df_contrib_sec_exp_informe
df_contrib_sec_imp_informe_anopas       <- res_anop$df_contrib_sec_imp_informe
df_contrib_paises_exp_informe_esp_anopas <- res_anop$df_contrib_paises_exp_informe_esp
df_contrib_paises_imp_informe_esp_anopas <- res_anop$df_contrib_paises_imp_informe_esp
df_contrib_sec_exp_informe_esp_anopas   <- res_anop$df_contrib_sec_exp_informe_esp
df_contrib_sec_imp_informe_esp_anopas   <- res_anop$df_contrib_sec_imp_informe_esp

# ============================================================
# Salidas Excel
# ============================================================

## Nombres de archivo (usan sufijos del parametros_bis.r) ----
nombre_totales   <- sprintf("evolucion_anual_%d_%d_%s.xlsx",   paramets$ano_ini, paramets$anho, sufijo_mes)
nombre_sec_dcx   <- sprintf("analisis_comercio_sectores_%s.xlsx", sufijo_mes)
nombre_pais_dcx  <- sprintf("analisis_comercio_paises_%s.xlsx",   sufijo_mes)
nombre_evol_sec  <- sprintf("evolucion_sectores_%d_%d.xlsx",   paramets$anho_idx, paramets$anho)
nombre_evol_pais <- sprintf("evolucion_paises_%d_%d.xlsx",     paramets$anho_idx, paramets$anho)
nombre_full_sec  <- sprintf("sectores_full_%s.xlsx",           sufijo_mes)
nombre_full_pais <- sprintf("paises_full_%s.xlsx",             sufijo_mes)
nombre_ccaas     <- sprintf("comercio_exterior_bienes_ccaas_%s.xlsx", sufijo_mes)

## Totales anuales ----
.write_formatted_xlsx(
  data       = totales_anuales,
  parametros = paramets,
  file_name  = nombre_totales,
  int_cols   = c("año"),
  pct_cols   = c(
    "exp_mad_pct", "imp_mad_pct", "tasa_cobertura_mad", "tasa_cobertura_esp",
    "exp_mad_tva", "imp_mad_tva", "exp_esp_tva",        "imp_esp_tva"
  ),
  extra_sheets = list(acu = totales_anuales_acu, anopas = totales_anuales_anopas)
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
  extra_sheets = list(acu = df_sectores_acu, anopas = df_sectores_anopas)
)

## Países datacomex ----
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
  extra_sheets = list(acu = df_paises_acu, anopas = df_paises_anopas)
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
  extra_sheets = list(acu = df_evol_sec_acu, anopas = df_evol_sec_anopas)
)
.write_formatted_xlsx(
  data       = df_evol_pais,
  parametros = paramets,
  file_name  = nombre_evol_pais,
  int_cols   = c("orden", "niv", "cod"),
  idx_cols   = idx_cols_evol_pais,
  pct_cols   = pct_cols_evol_pais,
  extra_sheets = list(acu = df_evol_pais_acu, anopas = df_evol_pais_anopas)
)

## Tablas full ----
.write_formatted_xlsx(
  data       = df_sec,
  parametros = paramets,
  file_name  = nombre_full_sec,
  int_cols   = c("orden", "niv"),
  pct_cols   = c("tva_exp", "tva_imp"),
  extra_sheets = list(acu = df_sec_acu, anopas = df_sec_anopas)
)
.write_formatted_xlsx(
  data       = df_country,
  parametros = paramets,
  file_name  = nombre_full_pais,
  int_cols   = c("cod"),
  pct_cols   = c("tva_exp", "tva_imp"),
  extra_sheets = list(acu = df_country_acu, anopas = df_country_anopas)
)

## CCAAs ----
.write_formatted_xlsx(
  data       = df_ccaas,
  parametros = paramets,
  file_name  = nombre_ccaas,
  int_cols   = c("Coddax",
                 "exp_euros_rank",     "imp_euros_rank",
                 "exp_euros_acu_rank", "imp_euros_acu_rank",
                 "exp_euros_anoant_rank", "imp_euros_anoant_rank"),
  pct_cols   = c(
    "exp_euros_peso",     "exp_euros_tva",     "exp_euros_rep",
    "imp_euros_peso",     "imp_euros_tva",     "imp_euros_rep",
    "exp_euros_acu_peso", "exp_euros_acu_tva", "exp_euros_acu_rep",
    "imp_euros_acu_peso", "imp_euros_acu_tva", "imp_euros_acu_rep",
    "exp_euros_anoant_peso", "exp_euros_tva2",  "exp_euros_tva2_rep",
    "imp_euros_anoant_peso", "imp_euros_tva2",  "imp_euros_tva2_rep"
  )
)

# Limpieza memoria ----
.limpiar_memoria()
message("[ETL] Completado.")
