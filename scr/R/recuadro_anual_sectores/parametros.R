# parametros.R
Sys.setenv(CHOREO_NO_SANDBOX = "1")

PYTHON_PATH <- "/home/pivan/Proyectos/comercio_exterior_bienes/.pixi/envs/default/bin/python3.11"

Sys.setenv(RETICULATE_PYTHON          = PYTHON_PATH)
Sys.setenv(RETICULATE_PYTHON_FALLBACK = PYTHON_PATH)

# reticulate
library(reticulate)
# use_python(PYTHON_PATH, required = TRUE)
# reticulate::py_config()

# Resto de librerías
library(arrow)
library(dplyr)
library(data.table)
library(writexl)
library(openxlsx)
library(gt)
library(gtExtras)
library(plotly)

# Funciones auxiliares ----
limpiar_memoria <- function() {
  conservar_fns <- ls(envir = .GlobalEnv)[
    sapply(ls(envir = .GlobalEnv), function(x) is.function(get(x, envir = .GlobalEnv)))
  ]
  rm(list = setdiff(ls(envir = .GlobalEnv), c(.conservar, conservar_fns)), envir = .GlobalEnv)
  gc(verbose = FALSE)
}

# Parámetros ----
params <- list(
  
  ## Rutas de datos ----
  path_mad              = "./data/interim/madrid/madrid_euros_sectores.parquet",
  path_esp              = "./data/interim/espana/espana_euros_sectores.parquet",
  path_sec              = "./data/metatratado/sectores.xlsx",
  path_pais             = "./data/metatratado/paises_zonas.xlsx",
  
  ## Parámetros de análisis ----
  ano_ini               = 2017L,
  anho                  = 2026L,
  meses                 = 2L:2L,
  cod_pais              = 0L,
  cod_sector            = "0",
  varfactor             = 1e6,
  varud                 = "M",
  dec_num               = 1L,
  dec_per               = 1L,
  anho_idx              = 2019L,
  
  ## Plots (común) ----
  dpi                   = 300,
  font_title            = 10,
  font_axis             = 8,
  font_caption          = 7,
  fuente_texto          = "Calibri",
  #colorbf              = "rgba(0,0,0,0)",
  #colorbf              = "rgba(255, 244, 202, 0.5)",
  colorbf               = "#FFFAE5",
  caption               = "Elaboración propia a partir de microdatos obtenidos de DataComex",
  
  ## Treemap ----
  palette_treemap_exp   = c(negativo = "#E47F56", neutro = "lightgrey", positivo = "#526DB0"),
  palette_treemap_imp   = c(negativo = "#E47F56", neutro = "lightgrey", positivo = "#F5C201"),
  plot_width_treemap    = 18,
  plot_height_treemap   = 13,
  plot_width            = 8.8,
  plot_height           = 7.49,
  plot_units            = "cm",
  px_per_cm             = 37.79527,
  plotly_dpi_ref        = 96,
  max_nivel_sec         = 3L,
  max_nivel_pai         = 4L,
  
  ## Vol-com ----
  max_bars_con          = 4L,
  max_bars_vol          = 8L,
  colpal1               = "#526DB0",
  colpal2               = "#96A6CF",
  colpal3               = "#F5C201",
  colpal4               = "#FEDE61",
  p_vol                 = list(title_y = 0.98, legend_y = 1.05, margin_t = 80, margin_b = 80, caption_y = -0.26),
  p_con                 = list(title_y = 0.98, margin_t = 60, margin_b = 60, caption_y = -0.26),
  
  ## Filtros evolución
  filpais               = c(),
  filsec                = c(),
  
  ## Tablas gt — paleta de colores ----
  gt_col_heading_bg     = "#526DB0",   # fondo cabecera (título de tabla)
  gt_col_heading_fg     = "#F5C201",   # texto del título
  gt_col_labels_bg      = "#F5C201",   # fondo cabecera de columnas / spanners / stubhead
  gt_col_labels_fg      = "black",     # texto cabecera de columnas
  gt_col_border         = "#AAAAAA",   # color bordes separadores verticales
  gt_col_niv0_bg        = "#F5C201",   # fondo nivel 0  (Total general)
  gt_col_niv0_fg        = "black",     # texto nivel 0
  gt_col_niv1_bg        = "#B9C4DF",   # fondo nivel 1 / 9 (grandes agregados)
  gt_col_niv1_fg        = "black",     # texto nivel 1 / 9
  gt_col_niv2_bg        = "#F2F2F2",   # fondo nivel 2  (zonas / regiones)
  gt_col_niv2_fg        = "black",     # texto nivel 2
  gt_col_niv3_bg        = "#FFFAE5",   # fondo nivel 3  (subzonas / subsectores)
  gt_col_niv3_fg        = "black",     # texto nivel 3
  gt_col_niv4_bg        = "#FFFAE5",   # fondo nivel 4  (países individuales)
  gt_col_niv4_fg        = "black",     # texto nivel 4
  gt_col_bar_pos        = "#2E7D5E",   # barras de contribución positivas
  gt_col_bar_neg        = "#C0392B",   # barras de contribución negativas
  gt_col_spark_pos      = "#2E7D5E",   # sparkline positivo (valor final >= 100)
  gt_col_spark_neg      = "#C0392B",   # sparkline negativo (valor final <  100)
  gt_col_spark_ref      = "#AAAAAA",   # línea de referencia sparkline
  
  ## Tablas gt — dimensiones y tipografía ----
  gt_tam_fuente         = 8L,          # tamaño base de fuente en tablas
  gt_ancho_tbl          = 18L,         # ancho por defecto tablas país/España (cm)
  gt_ancho_tbl_mad      = 18L,         # ancho tabla sectores Madrid (cm)
  gt_ancho_evol         = 18L,         # ancho tabla evolución anual (cm)
  gt_alto_tbl           = 26L          # alto en cm (NULL = automático)
)

# Paleta de colores ----
params$gt_col_pal <- with(params, list(
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

# Rutas ----
m_start <- min(params$meses)
m_end   <- max(params$meses)

nompath <- if (m_start == m_end) {
  sprintf("anal_sec_%d_%02d",      params$anho, m_start)
} else {
  sprintf("anal_sec_%d_%02d-%02d", params$anho, m_start, m_end)
}

params$path_out  <- file.path("./data/output", nompath)
params$path_outx <- file.path(params$path_out, "exceles")
params$path_outp <- file.path(params$path_out, "plots")
params$path_outt <- file.path(params$path_out, "tablas")

# Creación de directorios ----
invisible(lapply(
  c(params$path_out, params$path_outx, params$path_outp, params$path_outt),
  function(dir) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
))

# Limpieza de variables auxiliares de rutas ----
rm(m_start, m_end, nompath)

# Objetos a conservar en memoria ----
.conservar <- c(
  
  # Parámetros y utilidades
  "params",
  ".conservar",
  
  # Datos de main_etl ----
  "df_sectores",
  "df_paises",
  "df_sec",
  "df_country",
  "df_evol_sec",
  "df_evol_pais",
  "df_evol_secfull",
  "df_evol_countryfull",
  
  # Plots treemap — main_plots ----
  "treemap_exp_mad_sec",
  "treemap_exp_mad_pais",
  "treemap_imp_mad_sec",
  "treemap_imp_mad_pais",
  "treemap_exp_esp_sec",
  "treemap_exp_esp_pais",
  "treemap_imp_esp_sec",
  "treemap_imp_esp_pais",
  
  # Plots volumen — main_plots ----
  "vol_exp_mad_sec",
  "vol_exp_mad_pais",
  "vol_imp_mad_sec",
  "vol_imp_mad_pais",
  "vol_exp_esp_sec",
  "vol_exp_esp_pais",
  "vol_imp_esp_sec",
  "vol_imp_esp_pais",
  
  # Plots contribuciones — main_plots ----
  "contrib_exp_mad_sec",
  "contrib_exp_mad_pais",
  "contrib_imp_mad_sec",
  "contrib_imp_mad_pais",
  "contrib_exp_esp_sec",
  "contrib_exp_esp_pais",
  "contrib_imp_esp_sec",
  "contrib_imp_esp_pais",
  
  # Bump charts — main_plots ----
  "bump_exp_mad_paises",
  "bump_exp_mad_sec",
  "bump_imp_mad_paises",
  "bump_imp_mad_sec",
  "bump_exp_esp_paises",
  "bump_exp_esp_sec",
  "bump_imp_esp_paises",
  "bump_imp_esp_sec",
  
  # Tablas GT datacomex — main_tablas ----
  "tbl_sec_mad",
  "tbl_sec_esp",
  "tbl_pais_mad",
  "tbl_pais_esp",
  
  # Tablas GT evolución — main_tablas ----
  "tbl_evol_sec_mad_exp",
  "tbl_evol_sec_mad_imp",
  "tbl_evol_sec_esp_exp",
  "tbl_evol_sec_esp_imp",
  "tbl_evol_pais_mad_exp",
  "tbl_evol_pais_mad_imp",
  "tbl_evol_pais_esp_exp",
  "tbl_evol_pais_esp_imp",
  
  # Tablas GT evolución porcentual — main_tablas ----
  "tbl_evol_pct_sec_exp",
  "tbl_evol_pct_sec_imp",
  "tbl_evol_pct_pais_exp",
  "tbl_evol_pct_pais_imp"
)