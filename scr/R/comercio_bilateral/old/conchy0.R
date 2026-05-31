#### Establecimiento marco de trabajo ----
library(data.table)
library(arrow)
library(dplyr)
library(tidyr)
library(writexl)
library(plotly)
options(scipen = 999)
source("./scr/R/comercio_bilateral/auxiliar/funproc_bilateral.R")

#### Parámetros ----
path_mad <- "./data/interim/madrid/madrid_euros_taric.parquet"
path_esp <- "./data/interim/espana/espana_euros_taric.parquet"
lista_meses <- 1:11

##### Lista regiones ----
pais <- 400L
regiones <- list(
  "TOTAL" = c(0),
  "AMÉRICA DEL NORTE" = c(400, 404, 406, 408, 413),
  "EEUU" = pais,
  "UE27" = c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 17, 18, 30, 32, 38, 40, 46, 53, 54, 
             55, 60, 61, 63, 64, 66, 68, 91, 92, 600, 951, 959, 975, 978)
)

orden <- c("TOTAL", "AMÉRICA DEL NORTE", "EEUU", "UE27")

##### Paŕametros ----
nivel_taric <- 1L
anios <- 2025L
meses <- 1L:9L
fil_taric <- 300490
top_datos <- 15L
top_datos_bis <- 50L
flujo <- 1L
variable = "euros"
fil_porcentaje <- 0.01
n_paises <- 5L

name_anual <- "./data/output/comercio_bilateral/eeuu_ene-nov_2025.xlsx"

#### Carga de metadatos ----
df_tarics <- cargar_taric("./data/raw/metadatos/TARIC.csv")
df_paises <- cargar_pais("./data/metatratado/paises.xlsx")

#### Carga de datos ----
dfmad <- arrow::open_dataset(path_mad, format = "parquet")
dfesp <- arrow::open_dataset(path_esp, format = "parquet")
rm(path_mad, path_esp)

#### Dataframe anual ----
df_mad <- comercio_bilateral_anual(dfmad, lista_meses, regiones, orden)
df_esp <- comercio_bilateral_anual(dfesp, lista_meses, regiones, orden)
exportar_dataframes_anuales(
  dfmad    = df_mad,
  dfesp    = df_esp,
  savepath = name_anual
)

#### Productos ----
# Top n datos
topn_vol_nivel_tarics_exposicion <- top_tarics_exposicion(
  df_mad = dfmad,
  df_esp = dfesp,
  filtro_nivel = nivel_taric,
  filtro_ano = anios,
  filtro_mes = meses,
  n_max = top_datos,
  filtro_flujo = flujo,
  filtro_pais = pais,
  col_var = variable,
  df_taric = df_tarics)

# Filtro porcentaje
topper_vol_nivel_tarics_exposicion <- top_exposicion_asimetria(
  df_mad = dfmad,
  df_esp = dfesp,
  filtro_nivel = nivel_taric,
  filtro_ano = anios,
  filtro_mes = meses,
  filtro_flujo = flujo,
  filtro_pais = pais,
  col_var = variable,
  df_taric = df_tarics,
  filtro_porcentaje = fil_porcentaje,
  ordenar_por = "Grado dependencia")

#### Plots dispersión----
plotdis1 <- plot_dispersion_conchy(df = topper_vol_nivel_tarics_exposicion,
                                   nivel = nivel_taric,
                                   x_var = "Peso país",
                                   y_var = "Grado dependencia")

plotdis2 <- plot_dispersion_conchy(df = topper_vol_nivel_tarics_exposicion,
                                   nivel = nivel_taric,
                                   x_var = "Grado dependencia",
                                   y_var = "Asimetría regional")

#### Df----
df_evolucion_mensual_dependencia_taric <- df_evolucion_exposicion_asimetria(
  df_mad = dfmad,
  df_esp = dfesp,
  filtro_flujo = flujo,
  filtro_taric = fil_taric,
  col_var = variable,
  top_paises = n_paises,
  pais_analisis = pais,
  filtro_ano_top = anios,
  filtro_mes_top = meses,
  df_pais = df_paises,
  df_taric = df_tarics)

nombre_archivo <- paste0(
  "./comercio_exterior_evol_mensual_taric_", fil_taric, 
  "_del_grado_de_dependencia_asimetria_regional_top", n_paises, "_paises+", pais, ".xlsx"
)
openxlsx::write.xlsx(df_evolucion_mensual_dependencia_taric, nombre_archivo)

df_evolucion_periodo_dependencia_taric <- df_periodo_exposicion_asimetria(
  df_mad = dfmad,
  df_esp = dfesp,
  filtro_flujo = flujo,
  filtro_taric = fil_taric,
  filtro_mes = meses,
  col_var = variable,
  top_paises = n_paises,
  pais_analisis = pais,
  filtro_ano_top = anios,
  df_pais = df_paises,
  df_taric = df_tarics
)

nombre_archivo <- paste0(
  "./comercio_exterior_evol_anual_taric_", fil_taric, 
  "_del_grado_de_dependencia_asimetria_regional_top", n_paises, "_paises+", pais, ".xlsx"
)
openxlsx::write.xlsx(df_evolucion_periodo_dependencia_taric, nombre_archivo)

#### Calculos top tacis y países----
top_tarics <- top_tarics_mercado(
  df = dfmad,
  filtro_nivel = nivel_taric,
  filtro_ano = anios,
  filtro_mes = meses,
  filtro_porcentaje = fil_porcentaje,
  filtro_flujo = flujo,
  filtro_pais = pais,
  col_var = variable,
  df_pais = df_paises,
  df_taric = df_tarics)

top_paises <- top_paises_mercado(
  df = dfmad,
  filtro_ano = anios,
  filtro_mes = meses,
  filtro_porcentaje = fil_porcentaje,
  filtro_flujo = flujo,
  filtro_taric = fil_taric,
  col_var = variable,
  df_pais = df_paises,
  df_taric = df_tarics)

#### Plots evolución dependencia----
plot_evol_mensual_dependencia <- crear_grafico_lineas_dependencia_evolucion(
  df = df_evolucion_mensual_dependencia_taric, 
  y_var = "Grado dependencia", 
  ctaric = fil_taric, 
  df_taric = df_tarics)

plot_evol_periodo_dependencia <- crear_grafico_lineas_dependencia_periodo(
  df = df_evolucion_periodo_dependencia_taric, 
  y_var = "Grado dependencia", 
  ctaric = fil_taric, 
  df_taric = df_tarics)

