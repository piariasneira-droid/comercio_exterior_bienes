#### Establecimiento marco de trabajo ----
setwd(dirname(this.path::this.dir()))
source("./actualizacion/scripts/procmicro.R")
instala_carga_librerias(c("arrow", "data.table", "dplyr", "purrr", "readr", "vroom"))

#### Parámetros de actualización ----
yea <- 2025
yeardefi <- 2023
mon <- 7

# rutas base
ruta_ccaa_dir <- "./datos/total_ccaas"
ruta_mad_dir  <- "./datos/total_mad"
ruta_esp_dir  <- "./datos/total_esp"

# Calcular mes y año del fichero origen
if (mon == 1) {
  mes_formateado <- "12"
  año_fichero_origen <- yea - 1
} else {
  mes_formateado <- sprintf("%02d", mon - 1)
  año_fichero_origen <- yea
}
mes_actual <- sprintf("%02d", mon)

#### Generación de los ficheros del mes ----
generacion_ficheros_mes(
  year    = yea,
  yeardef = yeardefi,
  monthss = mon,
  
  # CCAA (mantiene unión con histórico)
  ruta_ccaa_salida = ruta_ccaa_dir,
  ruta_ccaa_origen = file.path(ruta_ccaa_dir, paste0("total_ccaa_hasta_", año_fichero_origen, "_", mes_formateado, ".csv")),
  ruta_ccaa_final  = file.path(ruta_ccaa_dir, paste0("total_ccaa_hasta_", yea, "_", mes_actual, ".csv")),
  
  # Madrid (solo genera archivo mensual)
  ruta_mad_salida = ruta_mad_dir,
  
  # España (solo genera archivo mensual)
  ruta_esp_salida = ruta_esp_dir
)

gc(verbose = FALSE)

#### Unión dinámica de totales MAD/ESP ----
actualizacion_mensual_totales_mad_esp(
  # Nuevos ficheros del mes actual
  ruta_mad_mes = file.path(ruta_mad_dir, "prov", paste0("de_mad_prov_", yea, "_", mes_actual, ".parquet")),
  ruta_esp_mes = file.path(ruta_esp_dir, "prov", paste0("de_esp_prov_", yea, "_", mes_actual, ".parquet")),
  
  # Entradas: totales del mes anterior
  ruta_mad_euros_entrada = file.path(ruta_final, paste0("de_mad_euros_", año_fichero_origen, "_", mes_formateado, ".parquet")),
  ruta_mad_kg_entrada    = file.path(ruta_final, paste0("de_mad_kg_", año_fichero_origen, "_", mes_formateado, ".parquet")),
  ruta_esp_euros_entrada = file.path(ruta_final, paste0("de_esp_euros_", año_fichero_origen, "_", mes_formateado, ".parquet")),
  ruta_esp_kg_entrada    = file.path(ruta_final, paste0("de_esp_kg_", año_fichero_origen, "_", mes_formateado, ".parquet")),
  
  # Salidas: acumulados actualizados
  ruta_mad_euros_salida = file.path(ruta_final, "de_mad_euros.parquet"),
  ruta_mad_kg_salida    = file.path(ruta_final, "de_mad_kg.parquet"),
  ruta_esp_euros_salida = file.path(ruta_final, "de_esp_euros.parquet"),
  ruta_esp_kg_salida    = file.path(ruta_final, "de_esp_kg.parquet"),
  
  # Salidas: acumulados actualizados
  # ruta_mad_euros_salida = paste0("de_mad_euros_", yea, "_", mes_actual, ".parquet"),
  # ruta_mad_kg_salida    = paste0("de_mad_kg_", yea, "_", mes_actual, ".parquet"),
  # ruta_esp_euros_salida = paste0("de_esp_euros_", yea, "_", mes_actual, ".parquet"),
  # ruta_esp_kg_salida    = paste0("de_esp_kg_", yea, "_", mes_actual, ".parquet"),
  
  # Año y mes del nuevo dato
  year = yea,
  mes = mon
)