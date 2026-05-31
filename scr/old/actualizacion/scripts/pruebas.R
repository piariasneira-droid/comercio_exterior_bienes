#### Establecimiento marco de trabajo de trabajo ----
setwd(dirname(dirname(this.path::this.dir())))
source("./actualizacion/scripts/procmicro.R")
instala_carga_librerias(c("arrow", "data.table", "dplyr", "purrr", "readr", "vroom"))

#### Definición de parámetros ----
fyea <- 1995L
yea <- 2025L
yeardefi <- 2023L
prov_codi <- 28L
mes_actual <-"07"

ruta_ccaa_dir <- "./datos/total_ccaas"
ruta_esp_dir <- "./datos/total_esp"
ruta_mad_dir <- "./datos/total_mad"
ruta_final <- "./datos/totales_mad_esp"


# Formatear mes con dos dígitos
mes_ftdo <- sprintf("%02d", fil_mes)

for (anio in fyea:yea) {
  est <- if (anio <= yeardefi) "def" else "prov"
  message(sprintf("  Año %s (%s)", anio, est))
  
  for (mes in 1:12) {
    mes_str <- sprintf("%02d", mes)
    procesar_mes(
      est = est,
      anio = as.character(anio),
      mesio = mes_str,
      carpeta_ccaa = file.path(ruta_ccaa_dir, est),
      carpeta_mad = file.path(ruta_mad_dir, est),
      carpeta_esp = file.path(ruta_esp_dir, est),
      prov_cod = prov_codi
    )
  }
}

combinar_csvs(
  folder_path = file.path(ruta_ccaa_dir, "def"),
  output_path = file.path(ruta_ccaa_dir, "total_ccaa_def.csv"),
  years = fyea:yea
)

combinar_csvs(
  folder_path = file.path(ruta_ccaa_dir, "prov"),
  output_path = file.path(ruta_ccaa_dir, "total_ccaa_prov.csv"),
  years = (yeardefi + 1):yea
)

combinar_parquets(
  folder_path = file.path(ruta_mad_dir, "def"),
  output_path = file.path(ruta_mad_dir, "total_mad_def.parquet"),
  years = fyea:yea
)

combinar_parquets(
  folder_path = file.path(ruta_mad_dir, "prov"),
  output_path = file.path(ruta_mad_dir, "total_mad_prov.parquet"),
  years = (yeardefi + 1):yea
)

combinar_parquets(
  folder_path = file.path(ruta_esp_dir, "def"),
  output_path = file.path(ruta_esp_dir, "total_esp_def.parquet"),
  years = fyea:yea
)

combinar_parquets(
  folder_path = file.path(ruta_esp_dir, "prov"),
  output_path = file.path(ruta_esp_dir, "total_esp_prov.parquet"),
  years = (yeardefi + 1):yea
)

unir_csvs(
  input_paths = c(
    file.path(ruta_ccaa_dir, "total_ccaa_def_hasta_2023.csv"),
    file.path(ruta_ccaa_dir, "total_ccaa_prov.csv")
  ),
  output_path = file.path(ruta_ccaa_dir, paste0("total_ccaa_hasta_", yea, "_", mes_actual, ".csv"))
  # output_path = file.path(ruta_ccaa_dir, "total_ccaa.csv")
)

unir_parquets(
  input_paths = c(
    file.path(ruta_mad_dir, "total_mad_def_hasta_2023.parquet"),
    file.path(ruta_mad_dir, "total_mad_prov.parquet")
  ),
  output_path = file.path(ruta_mad_dir, paste0("total_mad_hasta_", yea, "_", mes_actual, ".parquet"))
)

unir_parquets(
  input_paths = c(
    file.path(ruta_esp_dir, "total_esp_def_hasta_2023.parquet"),
    file.path(ruta_esp_dir, "total_esp_prov.parquet")
  ),
  output_path = file.path(ruta_esp_dir, paste0("total_esp_hasta_", yea, "_", mes_actual, ".parquet"))
)

# Separación de columnas y añadimos totales -
message("Generando archivos finales con totales...")

# Madrid - euros
message("  - Madrid euros")
anadir_totales_parquet(
  file.path(ruta_mad_dir, paste0("total_mad_hasta_", yea, "_", mes_actual, ".parquet")),
  file.path(ruta_final, paste0("de_mad_euros_", yea, "_", mes_actual, ".parquet")),
  columna = "euros"
)

# Madrid - kilogramos
message("  - Madrid kilogramos")
anadir_totales_parquet(
  file.path(ruta_mad_dir, paste0("total_mad_hasta_", yea, "_", mes_actual, ".parquet")),
  file.path(ruta_final, paste0("de_mad_kg_", yea, "_", mes_actual, ".parquet")),
  columna = "kilogramos"
)

# España - euros
message("  - España euros")
anadir_totales_parquet(
  file.path(ruta_esp_dir, paste0("total_esp_hasta_", yea, "_", mes_actual, ".parquet")),
  file.path(ruta_final, paste0("de_esp_euros_", yea, "_", mes_actual, ".parquet")),
  columna = "euros"
)

# España - kilogramos
message("  - España kilogramos")
anadir_totales_parquet(
  file.path(ruta_esp_dir, paste0("total_esp_hasta_", yea, "_", mes_actual, ".parquet")),
  file.path(ruta_final, paste0("de_esp_kg_", yea, "_", mes_actual, ".parquet")),
  columna = "kilogramos"
)