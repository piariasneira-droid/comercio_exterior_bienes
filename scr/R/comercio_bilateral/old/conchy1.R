#### Establecimiento marco de trabajo ----
library(data.table)
library(arrow)
library(dplyr)
library(tidyr)
library(writexl)
library(plotly)

# Carga funciones
source("./scr/R/comercio_bilateral/auxiliar/funproc_bilateral.R")

#### Parámetros ----
path_mad <- "./data/interim/madrid/madrid_euros_taric.parquet"
path_esp <- "./data/interim/espana/espana_euros_taric.parquet"
lista_meses <- 1:11
pais <- 400L

##### Lista regiones ----
regiones <- list(
  "TOTAL" = c(0),
  "AMÉRICA DEL NORTE" = c(400, 404, 406, 408, 413),
  "EEUU" = pais,
  "UE27" = c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 17, 18, 30, 32, 38, 40, 46, 53, 54, 
             55, 60, 61, 63, 64, 66, 68, 91, 92, 600, 951, 959, 975, 978)
)

orden <- c("TOTAL", "AMÉRICA DEL NORTE", "EEUU", "UE27")

##### Paŕametros ----
anios <- 2025L
meses <- 1L:11L
variable = "euros"
fil_porcentaje <- 0.01
n_max_n1 <- 6L

name_anual <- "./data/output/comercio_bilateral/eeuu_ene-nov_2025.xlsx"

#### Carga de metadatos ----
df_tarics <- cargar_taric("./data/raw/metadatos/TARIC.csv")
df_paises <- cargar_pais("./data/metatratado/paises.xlsx")

#### Carga de datos ----
dfmad <- arrow::open_dataset(path_mad, format = "parquet")
dfesp <- arrow::open_dataset(path_esp, format = "parquet")
rm(path_mad, path_esp)

#### Procesamiento ----
##### Resumen anual ----
df_mad <- comercio_bilateral_anual(dfmad, lista_meses, regiones, orden)
df_esp <- comercio_bilateral_anual(dfesp, lista_meses, regiones, orden)
exportar_dataframes_anuales(
  dfmad    = df_mad,
  dfesp    = df_esp,
  savepath = name_anual
)
rm(df_esp, df_mad)

##### Productos más vendidos -----
topn_vol_nivel_tarics_exposicion_exp <- top_tarics_exposicion(
  df_mad = dfmad,
  df_esp = dfesp,
  filtro_nivel = 1L,
  filtro_ano = anios,
  filtro_mes = meses,
  n_max = n_max_n1,
  filtro_flujo = 1L,
  filtro_pais = pais,
  col_var = variable,
  df_taric = df_tarics,
  incluir_ranking_total = TRUE)

topn_vol_nivel_tarics_exposicion_imp <- top_tarics_exposicion(
  df_mad = dfmad,
  df_esp = dfesp,
  filtro_nivel = 1L,
  filtro_ano = anios,
  filtro_mes = meses,
  n_max = n_max_n1,
  filtro_flujo = 0L,
  filtro_pais = pais,
  col_var = variable,
  df_taric = df_tarics,
  incluir_ranking_total = TRUE)


