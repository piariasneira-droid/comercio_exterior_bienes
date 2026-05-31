#### Establecimiento marco de trabajo de trabajo ----
setwd(dirname(this.path::this.dir()))
source("./actualizacionsectores/scripts/procmicrosectores.R")
instala_carga_librerias(c("arrow", "data.table", "dplyr", "purrr", "readr", "vroom"))

#### Definición de parámetros ----
fil_ano <- 2025L
fil_mes <- 10L
fil_anodefinito <- 2023L
fil_anoini <- 1995L
dir_esp <- "./datos/sectores_esp"
dir_mad <- "./datos/sectores_mad"
dir_final <- "./datos/totales_mad_esp"

# Formatear mes con dos dígitos
mes_ftdo <- sprintf("%02d", fil_mes)

##### Rutas archivo de salida provional -----
## OPCIÓN 1: Nombres genéricos (sin año/mes)
ruta_madrid_euros <- file.path(dir_final, "de_mad_sectores_euros.parquet")
ruta_espana_euros <- file.path(dir_final, "de_esp_sectores_euros.parquet")

## OPCIÓN 2: Nombres dinámicos (con año/mes)
# ruta_madrid_euros <- file.path(dir_final, sprintf("de_mad_sectores_euros_%s_%s.parquet", fil_ano, mes_ftdo))
# ruta_espana_euros <- file.path(dir_final, sprintf("de_esp_sectores_euros_%s_%s.parquet", fil_ano, mes_ftdo))

#### Inicialización data ----
inicializacion_completa_sectores(
  fyea = fil_anoini,
  yea = fil_ano,
  yeardefi = fil_anodefinito,
  mes_actual = mes_ftdo,
  ruta_mad_dir  = dir_mad,
  ruta_esp_dir  = dir_esp,
  ruta_final    = dir_final
)

#### Actualizaciones microdatos ----
##### Mensual provisional -----
actualizacion_sectores_mensual_datos_provisionales(
  yea = fil_ano,
  yeardefi = fil_anodefinito,
  mon = fil_mes,
  ruta_mad_dir  = dir_mad,
  ruta_esp_dir  = dir_esp,
  ruta_final    = dir_final,
  ruta_mad_euros_salida = ruta_madrid_euros,
  ruta_esp_euros_salida = ruta_espana_euros
)