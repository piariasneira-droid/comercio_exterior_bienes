#### Establecimiento marco de trabajo de trabajo ----
setwd(dirname(dirname(this.path::this.dir())))
source("./actualizacion/scripts/procmicro.R")
instala_carga_librerias(c("arrow", "data.table", "dplyr", "purrr", "readr", "vroom"))

#### Paramétros actualización (año y último año con datos definitivos) ----
fyea <- 1995
yea <- 2025
yeardefi <- 2023
mes_actual <- "06"

# rutas base
ruta_ccaa_dir <- "./datos/total_ccaas"
ruta_mad_dir  <- "./datos/total_mad"
ruta_esp_dir  <- "./datos/total_esp"
ruta_final    <- "./datos/totales_mad_esp"

#### Bucle de procesamiento total ----
# Crear carpetas de salida
dir.create("./datos/total_ccaas/def", recursive = TRUE, showWarnings = FALSE)
dir.create("./datos/total_ccaas/prov", recursive = TRUE, showWarnings = FALSE)
dir.create("./datos/total_mad/def", recursive = TRUE, showWarnings = FALSE)
dir.create("./datos/total_mad/prov", recursive = TRUE, showWarnings = FALSE)
dir.create("./datos/total_esp/def", recursive = TRUE, showWarnings = FALSE)
dir.create("./datos/total_esp/prov", recursive = TRUE, showWarnings = FALSE)

# Procesamiento año a año, mes a mes
for (anio in fyea:yea) {
  est <- if (anio <= yeardefi) "def" else "prov"
  for (mes in 1:12) {
    mes_str <- sprintf("%02d", mes)
    procesar_mes(
      est = est,
      anio = as.character(anio),
      mesio = mes_str,
      carpeta_ccaa = file.path("./datos/total_ccaas", est),
      carpeta_mad = file.path("./datos/total_mad", est),
      carpeta_esp = file.path("./datos/total_esp", est),
      prov_cod = 28L
    )
  }
}

#### Uniones finales ----
##### Totales CCAA -----
# Combinar archivos definitivos (desde fyea hasta yeardefi)
combinar_csvs(
  folder_path = "./datos/total_ccaas/def",
  output_path = "./datos/total_ccaas/total_ccaa_def.csv",
  years = fyea:yeardefi
  # years = fyea:yea
)

# Combinar archivos provisionales (desde yeardefi+1 hasta yea)
combinar_csvs(
  folder_path = "./datos/total_ccaas/prov",
  output_path = "./datos/total_ccaas/total_ccaa_prov.csv",
  years = (yeardefi + 1):yea
)

# Unir los dos archivos combinados
unir_csvs(
  input_paths = c(
    "./datos/total_ccaas/total_ccaa_def.csv",
    "./datos/total_ccaas/total_ccaa_prov.csv"
  ),
  output_path = "./datos/total_ccaas/total_csv_join.csv"
)

##### Microdatos Madrid -----
combinar_parquets(
  folder_path = "./datos/total_mad/def",
  output_path = "./datos/total_mad/total_mad_def.parquet",
  years = fyea:yeardefi
  # years = fyea:yea
)

combinar_parquets(
  folder_path = "./datos/total_mad/prov",
  output_path = "./datos/total_mad/total_mad_prov.parquet",
  years = (yeardefi + 1):yea
)

unir_parquets(
  input_paths = c(
    "./datos/total_mad/total_mad_def.parquet",
    "./datos/total_mad/total_mad_prov.parquet"
  ),
  output_path = "./datos/total_mad/total_mad_join.parquet"
)

##### Microdatos España -----
combinar_parquets(
  folder_path = "./datos/total_esp/def",
  output_path = "./datos/total_esp/total_esp_def.parquet",
  years = fyea:yeardefi
  # years = fyea:yea
)

combinar_parquets(
  folder_path = "./datos/total_esp/prov",
  output_path = "./datos/total_esp/total_esp_prov.parquet",
  years = (yeardefi + 1):yea)

unir_parquets(
  input_paths = c(
    "./datos/total_esp/total_esp_def.parquet",
    "./datos/total_esp/total_esp_prov.parquet"
  ),
  output_path = "./datos/total_esp/total_esp_join.parquet"
)

##### Separación columnas y añadimos totales -----
# Madrid - euros
anadir_totales_parquet(
  file.path(ruta_mad_dir, paste0("total_mad_hasta_", yea, "_", mes_actual, ".parquet")),
  file.path(ruta_final, paste0("de_mad_euros_", yea, "_", mes_actual, ".parquet")),
  columna = "euros"
)

# Madrid - kilogramos
anadir_totales_parquet(
  file.path(ruta_mad_dir, paste0("total_mad_hasta_", yea, "_", mes_actual, ".parquet")),
  file.path(ruta_final, paste0("de_mad_kg_", yea, "_", mes_actual, ".parquet")),
  columna = "kilogramos"
)

# España - euros
anadir_totales_parquet(
  file.path(ruta_esp_dir, paste0("total_esp_hasta_", yea, "_", mes_actual, ".parquet")),
  file.path(ruta_final, paste0("de_esp_euros_", yea, "_", mes_actual, ".parquet")),
  columna = "euros"
)

# España - kilogramos
anadir_totales_parquet(
  file.path(ruta_esp_dir, paste0("total_esp_hasta_", yea, "_", mes_actual, ".parquet")),
  file.path(ruta_final, paste0("de_esp_kg_", yea, "_", mes_actual, ".parquet")),
  columna = "kilogramos"
)