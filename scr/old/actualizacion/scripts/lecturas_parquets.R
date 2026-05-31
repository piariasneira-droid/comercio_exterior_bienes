setwd(dirname(dirname(this.path::this.dir())))

#### Parámetros ----
ano_def <- 2023
ano <- 2025
mes <- 8
fil_mes <- mes
fil_flujo <- 1L

# Variables auxiliares
ano_def_ant <- ano_def - 1
mes_formateado <- sprintf("%02d", mes)

if (mes == 1) {
  mes_ant_formateado <- "12"
  ano_ant <- ano - 1
} else {
  mes_ant_formateado <- sprintf("%02d", mes - 1)
  ano_ant <- ano
}

#### Construcción de rutas dinámicas ----
base_path <- "./datos"

# Arhivos finales
path_mad_euros_ant <- file.path(base_path, "totales_mad_esp", paste0("de_mad_euros_", ano_ant, "_", mes_ant_formateado, ".parquet"))
path_mad_kg_ant    <- file.path(base_path, "totales_mad_esp", paste0("de_mad_kg_", ano_ant, "_", mes_ant_formateado, ".parquet"))
path_esp_euros_ant <- file.path(base_path, "totales_mad_esp", paste0("de_esp_euros_", ano_ant, "_", mes_ant_formateado, ".parquet"))
path_esp_kg_ant    <- file.path(base_path, "totales_mad_esp", paste0("de_esp_kg_", ano_ant, "_", mes_ant_formateado, ".parquet"))

path_mad_euros_act <- file.path(base_path, "totales_mad_esp", "de_mad_euros.parquet")
path_mad_kg_act    <- file.path(base_path, "totales_mad_esp", "de_mad_kg.parquet")
path_esp_euros_act <- file.path(base_path, "totales_mad_esp", "de_esp_euros.parquet")
path_esp_kg_act    <- file.path(base_path, "totales_mad_esp", "de_esp_kg.parquet")

path_mad_euros_act <- file.path(base_path, "totales_mad_esp", paste0("de_mad_euros_", ano, "_", mes_formateado, ".parquet"))
path_mad_kg_act    <- file.path(base_path, "totales_mad_esp", paste0("de_mad_kg_", ano, "_", mes_formateado, ".parquet"))
path_esp_euros_act <- file.path(base_path, "totales_mad_esp", paste0("de_esp_euros_", ano, "_", mes_formateado, ".parquet"))
path_esp_kg_act    <- file.path(base_path, "totales_mad_esp", paste0("de_esp_kg_", ano, "_", mes_formateado, ".parquet"))

# Archivo mes
path_mes_mad <- file.path(base_path, "total_mad", "prov", paste0("de_mad_prov_", ano, "_", mes_formateado, ".parquet"))
path_mes_esp <- file.path(base_path, "total_esp", "prov", paste0("de_esp_prov_", ano, "_", mes_formateado, ".parquet"))


#### Lectura de archivos ----
df_mad_euros <- arrow::read_parquet(path_mad_euros_act)
df_mad_kg <- arrow::read_parquet(path_mad_kg_act)
df_esp_euros <- arrow::read_parquet(path_esp_euros_act)
df_esp_kg <- arrow::read_parquet(path_esp_kg_act)

##### Filtrado
df <- dplyr::filter(df_mad_euros,
              flujo == fil_flujo,
              año == ano,
              mes == fil_mes)