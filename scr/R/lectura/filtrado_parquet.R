#### Establecimiento marco de trabajo (Optimizado para Big Data) ----
library(arrow)
library(dplyr)

# Rutas y filtros
ruta_eurosmad <- "./data/interim/madrid/madrid_euros_taric.parquet"
ruta_eurosesp <- "./data/interim/espana/espana_euros_taric.parquet"
ano_ini <- 2024L
ano_fin <- 2026L

# Carga datos
ds_mad <- arrow::open_dataset(ruta_eurosmad)
ds_esp <- arrow::open_dataset(ruta_eurosesp)

# Generación rutas
ruta_out_mad <- gsub(".parquet", paste0("_", ano_ini, "_", ano_fin, ".parquet"), ruta_eurosmad)
ruta_out_esp <- gsub(".parquet", paste0("_", ano_ini, "_", ano_fin, ".parquet"), ruta_eurosesp)

# Filtrado y guardado
ds_mad %>%
  filter(año >= ano_ini & año <= ano_fin) %>%
  write_parquet(ruta_out_mad)

ds_esp %>%
  filter(año >= ano_ini & año <= ano_fin) %>%
  write_parquet(ruta_out_esp)

message("Proceso completado eficientemente.")