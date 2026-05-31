#### Entorno ----
library(arrow)
library(dplyr) 

#### Configuración de Parámetros ----
pathmadsec <- "./data/interim/madrid/madrid_euros_sectores.parquet"
f_anio  <- 2026L
f_mes   <- 2L


##### Lectura ----
ds <- arrow::open_dataset(pathmadsec)
df_ano <- ds %>%
  filter(
    año   == f_anio,
    mes   %in% f_mes
  ) %>%
  collect()

df_ant <- ds %>%
  filter(
    año   == (f_anio - 1L),
    mes   %in% f_mes
  ) %>%
  collect()