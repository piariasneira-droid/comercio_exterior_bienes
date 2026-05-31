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
  mh                    = 0.3  
)

paramets$fecha <- as.Date(paste(paramets$anho, paramets$mes, "01", sep = "-"))
paramets$fecha_ini <- as.Date(paste(paramets$ano_ini, "01", "01", sep = "-"))