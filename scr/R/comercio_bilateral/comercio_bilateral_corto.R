#### Establecimiento de ruta de trabajo----

library(data.table)
library(arrow)
library(dplyr)
library(tidyr)
library(writexl)
library(openxlsx)
library(plotly)
library(vroom)
library(htmltools)

source("./scr/R/comercio_bilateral/auxiliar/funproc_bilateral.R")

#### Parámetros ----
path_mad <- "./data/interim/madrid/madrid_euros_taric.parquet"
path_esp <- "./data/interim/espana/espana_euros_taric.parquet"

variable       <- "euros"
anios          <- 2025L
meses          <- 1L:11L
fil_porcentaje <- 0.01
n_max_n1       <- 6L
n_max_n2       <- 15L
n_paises       <- 5L
fil_aniotop    <- 2025L
for_paises     <- 400L
for_flujo      <- 0L:1L
for_niveles    <- c(1L, 5L)
for_aniotop    <- 2025L

##### Lista de regiones ----
regiones <- list(
  "TOTAL"             = 0,
  "AMÉRICA DEL NORTE" = c(400, 404, 406, 408, 413),
  "EEUU"              = for_paises,
  "UE27"              = c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 17, 18, 30, 32, 38,
                          40, 46, 53, 54, 55, 60, 61, 63, 64, 66, 68, 91, 92,
                          600, 951, 959, 975, 978)
)
orden <- c("TOTAL", "AMÉRICA DEL NORTE", "EEUU", "UE27")

#### Carga de metadatos ----
df_tarics <- cargar_taric("./data/raw/metadatos/TARIC.csv")
df_paises <- cargar_pais("./data/metatratado/paises.xlsx")

#### Carga de datos (lazy) ----
dfmad <- arrow::open_dataset(path_mad, format = "parquet")
dfesp <- arrow::open_dataset(path_esp, format = "parquet")
rm(path_mad, path_esp)

#### Procesamiento principal ----
for (pais in for_paises) {
  cat("\n============================\n")
  cat("Iniciando procesamiento para país:", pais, "\n")
  cat("============================\n")
  
  # Directorio raíz dinámico para este país + periodo
  outdir <- build_outdir_pais(pais, meses, anios, df_paises)
  crear_dirs_pais(outdir)
  cat("  Directorio de salida:", outdir, "\n")
  
  ##### Resumen anual bilateral
  cat("  [1/3] Resumen anual bilateral...\n")
  
  df_mad_anual <- comercio_bilateral_anual(dfmad, meses, regiones, orden, "Madrid")
  df_esp_anual <- comercio_bilateral_anual(dfesp, meses, regiones, orden, "España")
  
  name_anual <- build_name_anual(pais, meses, anios, df_paises, outdir)
  
  exportar_dataframes_anuales(
    dfmad    = df_mad_anual,
    dfesp    = df_esp_anual,
    savepath = name_anual
  )
  
  # rm(df_mad_anual, df_esp_anual)
  cat("    Guardado:", name_anual, "\n")
  
  #
  
  cat("  [2/3] Top TARICs nivel 1 y nivel 5...\n")
  
  # --- Nivel 1 ---
  top_exp_n1 <- top_tarics_exposicion(
    df_mad                = dfmad,
    df_esp                = dfesp,
    filtro_nivel          = 1L,
    filtro_ano            = anios,
    filtro_mes            = meses,
    n_max                 = n_max_n1,
    filtro_flujo          = 1L,
    filtro_pais           = pais,
    col_var               = variable,
    df_taric              = df_tarics,
    incluir_ranking_total = TRUE
  )
  
  top_imp_n1 <- top_tarics_exposicion(
    df_mad                = dfmad,
    df_esp                = dfesp,
    filtro_nivel          = 1L,
    filtro_ano            = anios,
    filtro_mes            = meses,
    n_max                 = n_max_n1,
    filtro_flujo          = 0L,
    filtro_pais           = pais,
    col_var               = variable,
    df_taric              = df_tarics,
    incluir_ranking_total = TRUE
  )
  
  ruta_top_n1 <- exportar_top_tarics(
    df_exp      = top_exp_n1,
    df_imp      = top_imp_n1,
    pais_code   = pais,
    flujo_label = "nivel_1",
    outdir      = outdir
  )
  cat("    Guardado nivel 1:", ruta_top_n1, "\n")
  
  # --- Nivel 5 ---
  top_exp_n5 <- top_tarics_exposicion(
    df_mad                = dfmad,
    df_esp                = dfesp,
    filtro_nivel          = 5L,
    filtro_ano            = anios,
    filtro_mes            = meses,
    n_max                 = n_max_n2,
    filtro_flujo          = 1L,
    filtro_pais           = pais,
    col_var               = variable,
    df_taric              = df_tarics,
    incluir_ranking_total = FALSE
  )
  
  top_imp_n5 <- top_tarics_exposicion(
    df_mad                = dfmad,
    df_esp                = dfesp,
    filtro_nivel          = 5L,
    filtro_ano            = anios,
    filtro_mes            = meses,
    n_max                 = n_max_n2,
    filtro_flujo          = 0L,
    filtro_pais           = pais,
    col_var               = variable,
    df_taric              = df_tarics,
    incluir_ranking_total = FALSE
  )
  
  ruta_top_n5 <- exportar_top_tarics(
    df_exp      = top_exp_n5,
    df_imp      = top_imp_n5,
    pais_code   = pais,
    flujo_label = "nivel_5",
    outdir      = outdir
  )
  cat("    Guardado nivel 5:", ruta_top_n5, "\n")
  
  ##### Dispersión y evolución
  cat("  [3/3] Dispersión y evolución...\n")
  
  for (flujo in for_flujo) {
    
    ##### Dispersión 
    for (nivel in for_niveles) {
      for (anio in for_aniotop) {
        
        df_dispersion <- top_exposicion_asimetria(
          df_mad            = dfmad,
          df_esp            = dfesp,
          filtro_nivel      = nivel,
          filtro_ano        = anio,
          filtro_mes        = meses,
          filtro_flujo      = flujo,
          filtro_pais       = pais,
          col_var           = variable,
          df_taric          = df_tarics,
          filtro_porcentaje = fil_porcentaje,
          ordenar_por       = "Grado dependencia"
        )
        
        ruta_excel_dis <- file.path(
          outdir, "dispersion/exceles",
          paste0("dispersion",
                 "_nivel_", nivel,
                 "_flujo_", flujo,
                 "_anio_", anio, ".xlsx")
        )
        openxlsx::write.xlsx(df_dispersion, ruta_excel_dis)
        
        plotdis1 <- plot_dispersion_conchy(
          df    = df_dispersion,
          nivel = nivel,
          x_var = "Peso país",
          y_var = "Grado dependencia"
        )
        plotdis2 <- plot_dispersion_conchy(
          df    = df_dispersion,
          nivel = nivel,
          x_var = "Grado dependencia",
          y_var = "Asimetría regional"
        )
        
        htmltools::save_html(plotdis1, file.path(
          outdir, "dispersion/htmls",
          paste0("dispersion_peso_dependencia",
                 "_nivel_", nivel,
                 "_flujo_", flujo,
                 "_anio_", anio, ".html")
        ))
        htmltools::save_html(plotdis2, file.path(
          outdir, "dispersion/htmls",
          paste0("dispersion_dependencia_asimetria",
                 "_nivel_", nivel,
                 "_flujo_", flujo,
                 "_anio_", anio, ".html")
        ))
        
        cat("    Dispersión - Flujo:", flujo, "| Nivel:", nivel, "| Año:", anio, "\n")
      } # closes: for (anio in f
    } # closes: for (nivel in for_niveles)
    
  } # closes: for (flujo in for_flujo)
  
  cat("Finalizado procesamiento para país:", pais, "\n")
} # closes: for (pais in for_paises)

cat("\n=== Proceso completado ===\n")