# parametros_bis.r
# ============================================================
# ÚNICO FICHERO QUE TOCA TU JEFE PARA LANZAR EL ANÁLISIS
# ============================================================
#
# Para un mes suelto:        mes = 3L
# Para un trimestre:         mes = 4L:6L   (Q2)
# Para un semestre:          mes = 1L:6L
# Para el año completo:      mes = 1L:12L
#
# El resto del código NO necesita tocar este fichero.
# ============================================================

paramets <- list(

  ## ----------------------------------------------------------
  ## 1. PERIODO A ANALIZAR  <-- SOLO CAMBIAR ESTO
  ## ----------------------------------------------------------
  anho                  = 2026L,
  mes                   = 3L,        
  ano_ini               = 2018L,
  anho_idx              = 2019L,

  ## ----------------------------------------------------------
  ## 2. Rutas de datos
  ## ----------------------------------------------------------
  path_mad              = "./data/interim/madrid/madrid_euros_sectores.parquet",
  path_madt             = "./data/interim/madrid/madrid_euros_taric.parquet",
  path_esp              = "./data/interim/espana/espana_euros_sectores.parquet",
  path_sec              = "./data/metatratado/sectores.xlsx",
  path_pais             = "./data/metatratado/paises_zonas.xlsx",
  path_taric            = "./data/raw/metadatos/TARIC.csv",
  path_ccaa             = "./data/interim/totalesccaa/totalesccaa.csv",
  path_mccaa            = "./data/metatratado/regiones.xlsx",
  path_ccaafull         = "./data/output/ccaacappais/df_ccaa_mes_amp.csv",

  ## ----------------------------------------------------------
  ## 3. Parámetros de cálculo
  ## ----------------------------------------------------------
  cod_pais              = 0L,
  cod_sector            = "0",
  varfactor             = 1e6,
  varud                 = "M",
  dec_num               = 1L,
  dec_per               = 1L,

  ## ----------------------------------------------------------
  ## 4. Flags de plots (TRUE = generar, FALSE = saltar)
  ## ----------------------------------------------------------
  flagmadmes            = TRUE,
  flagespmes            = TRUE,
  flagmadytm            = TRUE,
  flagespytm            = TRUE,
  flagmadanop           = TRUE,
  flagespanop           = TRUE,
  flag_ccaa             = TRUE,   # TRUE = incluir análisis de CC.AA.

  ## ----------------------------------------------------------
  ## 5. Opciones de plots
  ## ----------------------------------------------------------
  n_subsec_plotpais     = 3L,
  n_paises_plotsectores = 4L,
  fil_sectores_plot     = c(1, 11, 15, 18, 24, 33, 34, 37, 40, 45, 50, 53, 58, 59, 64, 65, 66),
  colpal1               = "#2d5532",
  colpal2               = "#b4d7b4",
  colpal3               = "#2d5532",
  colpal4               = "#b4d7b4",
  colorbf               = "#FFFFFF",
  palette_treemap_exp   = c(negativo = "#E47F56", neutro = "lightgrey", positivo = "#2d5532"),
  palette_treemap_imp   = c(negativo = "#E47F56", neutro = "lightgrey", positivo = "#b4d7b4"),
  font_title            = 10,
  font_axis             = 8,
  fuente_texto          = "Calibri",
  max_nivel_sec         = 3L,
  max_nivel_pai         = 3L,
  max_bars_con          = 3L,
  max_bars_vol          = 8L,
  n_pares_con           = 1000L,
  reg1                  = "Madrid, Comunidad de",
  reg2                  = "España",
  dpi                   = 300,
  w1                    = 7,
  h1                    = 4.5,
  w2                    = 9,
  h2                    = 5.5,
  mv                    = 0.3,
  mh                    = 0.3,
  ws_width_cm           = 18,
  ws_height_cm          = 8,
  ws_width_cm_alt       = 10,
  ws_height_cm_alt      = 8,


  ## ----------------------------------------------------------
  ## 6. Paleta tablas gt
  ## ----------------------------------------------------------
  gt_col_heading_bg     = "#2d5532",
  gt_col_heading_fg     = "#FFFFFF",
  gt_col_labels_bg      = "#2d5532",
  gt_col_labels_fg      = "#FFFFFF",
  gt_col_border         = "#AAAAAA",
  gt_col_niv0_bg        = "#59a75b",
  gt_col_niv0_fg        = "black",
  gt_col_niv1_bg        = "#b4d7b4",
  gt_col_niv1_fg        = "black",
  gt_col_niv2_bg        = "#F2F2F2",
  gt_col_niv2_fg        = "black",
  gt_col_niv3_bg        = "#FFFFFF",
  gt_col_niv3_fg        = "black",
  gt_col_niv4_bg        = "#FFFFFF",
  gt_col_niv4_fg        = "black",
  gt_col_bar_pos        = "#2d5532",
  gt_col_bar_neg        = "#C0392B",
  gt_col_spark_pos      = "#2f5532",
  gt_col_spark_neg      = "#C0392B",
  gt_col_spark_ref      = "#AAAAAA",

  ## ----------------------------------------------------------
  ## 7. Dimensiones y tipografía gt
  ## ----------------------------------------------------------
  gt_tam_fuente         = 8L,
  gt_ancho_tbl          = 18L,
  gt_ancho_tbl_mad      = 18L,
  gt_ancho_evol         = 18L,
  gt_alto_tbl           = 26L,

  ## ----------------------------------------------------------
  ## 8. Exclusiones para anexos
  ## ----------------------------------------------------------
  sectores_a_excluir    = c(3L, 4L, 5L, 6L, 7L, 8L, 9L, 16L, 17L, 19L, 20L, 21L, 22L, 23L,
                            26L, 29L, 30L, 32L, 35L, 36L, 38L, 39L, 41L, 43L, 55L, 56L, 57L,
                            61L, 62L, 65L),
  paises_a_excluir      = c(7L:12L, 14L, 17L:20L, 24L, 26L, 29L, 32L, 39L, 46L,
                            52L, 54L:57L, 60L, 64L, 66L:67L, 71L),

  ## ----------------------------------------------------------
  ## 9. Textos
  ## ----------------------------------------------------------
  caption               = "Elaboraci\u00f3n propia a partir de microdatos de DataComex (MITECO)."

)

# ----------------------------------------------------------
# Paleta gt consolidada (se construye automáticamente)
# ----------------------------------------------------------
paramets$gt_col_pal <- with(paramets, list(
  heading_bg = gt_col_heading_bg, heading_fg = gt_col_heading_fg,
  labels_bg  = gt_col_labels_bg,  labels_fg  = gt_col_labels_fg,
  border     = gt_col_border,
  niv0_bg    = gt_col_niv0_bg,    niv0_fg    = gt_col_niv0_fg,
  niv1_bg    = gt_col_niv1_bg,    niv1_fg    = gt_col_niv1_fg,
  niv2_bg    = gt_col_niv2_bg,    niv2_fg    = gt_col_niv2_fg,
  niv3_bg    = gt_col_niv3_bg,    niv3_fg    = gt_col_niv3_fg,
  niv4_bg    = gt_col_niv4_bg,    niv4_fg    = gt_col_niv4_fg,
  bar_pos    = gt_col_bar_pos,    bar_neg    = gt_col_bar_neg,
  spark_pos  = gt_col_spark_pos,  spark_neg  = gt_col_spark_neg,
  spark_ref  = gt_col_spark_ref
))

# ----------------------------------------------------------
# Sufijos de archivo (se calculan UNA SOLA VEZ aquí,
# disponibles en todos los scripts que hacen source de este)
# ----------------------------------------------------------
.mes_max   <- max(paramets$mes)
.mes_min   <- min(paramets$mes)
.n_meses   <- length(paramets$mes)

# ¿Es un trimestre natural? (3 meses consecutivos empezando en mes 1, 4, 7 o 10)
.es_trimestre <- .n_meses == 3L &&
                 all(diff(paramets$mes) == 1L) &&
                 (.mes_min %% 3L) == 1L

trimestre_num <- if (.es_trimestre) ceiling(.mes_max / 3L) else NA_integer_

# sufijo_mes  : usado para los archivos del periodo concreto analizado
# sufijo_ytm  : usado para los acumulados (ene → mes_max)
# sufijo_anopas: usado para el año anterior completo
sufijo_mes    <- if (.es_trimestre) {
  sprintf("%04d_Q%d", paramets$anho, trimestre_num)
} else if (.n_meses == 1L) {
  sprintf("%04d_%02d", paramets$anho, .mes_max)
} else {
  sprintf("%04d_%02d_%02d", paramets$anho, .mes_min, .mes_max)
}

sufijo_ytm    <- sprintf("%04d_ytm%02d", paramets$anho, .mes_max)
sufijo_anopas <- sprintf("%04d_anual",   paramets$anho - 1L)

# ----------------------------------------------------------
# Validación rápida — detecta errores de configuración
# antes de lanzar el ETL
# ----------------------------------------------------------
.validate_paramets <- function(p) {
  errores <- character(0)

  if (!is.integer(p$anho) || p$anho < 2000L)
    errores <- c(errores, "anho debe ser un entero >= 2000 (usar 2026L, no 2026)")

  if (!is.integer(p$mes) || any(p$mes < 1L) || any(p$mes > 12L))
    errores <- c(errores, "mes debe ser entero(s) entre 1 y 12 (usar 3L o 4L:6L)")

  if (!is.integer(p$ano_ini) || p$ano_ini >= p$anho)
    errores <- c(errores, "ano_ini debe ser entero y menor que anho")

  if (!file.exists(p$path_mad))
    errores <- c(errores, paste("No se encuentra path_mad:", p$path_mad))

  if (!file.exists(p$path_esp))
    errores <- c(errores, paste("No se encuentra path_esp:", p$path_esp))

  if (length(errores) > 0) {
    stop(
      "\n[parametros_bis.r] Errores de configuracion detectados:\n",
      paste0("  - ", errores, collapse = "\n"), "\n",
      call. = FALSE
    )
  }
  message("[parametros_bis.r] Configuracion validada OK: anho=", p$anho,
          " | mes=", paste(p$mes, collapse = ":"),
          " | sufijo=", sufijo_mes)
  invisible(p)
}

paramets <- .validate_paramets(paramets)
