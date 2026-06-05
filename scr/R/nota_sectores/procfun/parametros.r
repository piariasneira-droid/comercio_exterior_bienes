# Parámetros ----
paramets <- list(
  
  ## Rutas de datos ----
  path_mad              = "./data/interim/madrid/madrid_euros_sectores.parquet",
  path_esp              = "./data/interim/espana/espana_euros_sectores.parquet",
  path_sec              = "./data/metatratado/sectores.xlsx",
  path_pais             = "./data/metatratado/paises_zonas.xlsx",
  path_ccaa             = "./data/interim/totalesccaa/totalesccaa.csv",
  path_mccaa            = "./data/metatratado/regiones.xlsx",
  path_ccaafull         = "./data/output/ccaacappais/df_ccaa_mes_amp.csv",
  
  ## Parámetros de análisis ----
  ano_ini               = 2017L,
  anho                  = 2026L,
  mes                   = 3L,
  cod_pais              = 0L,
  cod_sector            = "0",
  varfactor             = 1e6,
  varud                 = "M",
  dec_num               = 1L,
  dec_per               = 1L,
  anho_idx              = 2019L,
  
  ## Plots
  flagmadmes            = TRUE,
  flagespmes            = TRUE,
  flagmadytm            = TRUE,
  flagespytm            = TRUE,
  flagmadanop           = TRUE,
  flagespanop           = TRUE,
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
  max_nivel_pai         = 4L,
  max_bars_con          = 4L,
  max_bars_vol          = 8L,
  reg1                  = "Madrid, Comunidad de",
  reg2                  = "España",
  ano_ini               = 2018,
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
  
  ## Tablas gt — paleta de colores ----
  gt_col_heading_bg     = "#2d5532",   # fondo cabecera (título de tabla)
  gt_col_heading_fg     = "#FFFFFF",   # texto del título
  gt_col_labels_bg      = "#2d5532",   # fondo cabecera de columnas / spanners / stubhead
  gt_col_labels_fg      = "#FFFFFF",     # texto cabecera de columnas
  gt_col_border         = "#AAAAAA",   # color bordes separadores verticales
  gt_col_niv0_bg        = "#59a75b",  # fondo nivel 0  (Total general)
  gt_col_niv0_fg        = "black",     # texto nivel 0
  gt_col_niv1_bg        = "#b4d7b4",   # fondo nivel 1 / 9 (grandes agregados)
  gt_col_niv1_fg        = "black",     # texto nivel 1 / 9
  gt_col_niv2_bg        = "#F2F2F2",   # fondo nivel 2  (zonas / regiones)
  gt_col_niv2_fg        = "black",     # texto nivel 2
  gt_col_niv3_bg        = "#FFFFFF",   # fondo nivel 3  (subzonas / subsectores)
  gt_col_niv3_fg        = "black",     # texto nivel 3
  gt_col_niv4_bg        = "#FFFFFF",   # fondo nivel 4  (países individuales)
  gt_col_niv4_fg        = "black",     # texto nivel 4
  gt_col_bar_pos        = "#2d5532",   # barras de contribución positivas
  gt_col_bar_neg        = "#C0392B",   # barras de contribución negativas
  gt_col_spark_pos      = "#2f5532",   # sparkline positivo (valor final >= 100)
  gt_col_spark_neg      = "#C0392B",   # sparkline negativo (valor final <  100)
  gt_col_spark_ref      = "#AAAAAA",   # línea de referencia sparkline
  
  ## Tablas gt — dimensiones y tipografía ----
  gt_tam_fuente         = 8L,          # tamaño base de fuente en tablas
  gt_ancho_tbl          = 18L,         # ancho por defecto tablas país/España (cm)
  gt_ancho_tbl_mad      = 18L,         # ancho tabla sectores Madrid (cm)
  gt_ancho_evol         = 18L,         # ancho tabla evolución anual (cm)
  gt_alto_tbl           = 26L,          # alto en cm (NULL = automático)
  
  ## Tabla anexos
  sectores_a_excluir    = c(3L, 4L, 5L, 6L, 7L, 8L, 9L, 16L, 17L, 19L, 20L, 21L, 22L, 23L, 26L, 29L,
                            30L, 32L, 35L, 36L, 38L, 39L, 41L, 43L, 55L, 56L, 57L, 61L, 62L, 65L),
  
  paises_a_excluir  = c(7L:12L, 14L, 17L:20L, 24L, 26L, 29L, 32L, 39L, 46L,
                        52L, 54L:57L, 60L, 64L, 66L:67L, 71L),
  
  ## Textos ----
  caption           = "Elaboraci\u00f3n propia a partir de microdatos de DataComex (MITECO)."
  
)

paramets$fecha <- as.Date(paste(paramets$anho, paramets$mes, "01", sep = "-"))
paramets$fecha_ini <- as.Date(paste(paramets$ano_ini, "01", "01", sep = "-"))

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