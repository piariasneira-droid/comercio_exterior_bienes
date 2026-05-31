#### Establecimiento marco de trabajo de trabajo ----
source("./scr/R/actualizacion/auxiliar/procmicroexcel.R")

library(arrow)
library(data.table)
library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(tidyr)
library(vroom)
library(zoo)
library(writexl)
library(openxlsx)

#### Parámetros ----
##### Fechas -----
fechainicial <- as.Date("2024-01-01")
fechafinal <- as.Date("2026-03-01")

##### Rutas -----
data_path <- "./data/interim/madrid/madrid_euros_taric.parquet"
estados_path <- "./data/metatratado/estados.xlsx"
tarics_path <- "./data/metatratado/capitulos.xlsx"
ccaas_path <- "./data/interim/totalesccaa/totalesccaa.csv"
regiones_path <- "./data/metatratado/regiones.xlsx"
salida_path <- "./data/output/ccaacappais"
actualizador <- "./data/output/ccaacappais/actualizador_exceles.xlsx"

#### Mapeos ----
map_regiones <- data.table::as.data.table(readxl::read_excel(regiones_path))
map_tarics <- data.table::as.data.table(readxl::read_excel(tarics_path))
map_paises <- data.table::as.data.table(readxl::read_excel(estados_path))


#### Lectura arrow ----
datafil <- data.table::as.data.table(
  dplyr::collect(
    dplyr::filter(
      arrow::open_dataset(data_path),
      (pais == 0L & cod_taric >= 1 & cod_taric <= 99) |
        (pais != 0L & cod_taric == 0)
    )
  )
)

#### Data handling----
##### Tarics -----
df_tarics <- obtener_df_taric(
  df = datafil,
  mapeo_tarics = map_tarics
)

df_tarcarexp <- pivot_fechas(
  dt = df_tarics,
  id_cols = c("taric"),
  fechaini = fechainicial,
  fechafin = fechafinal,
  flujoval = "EXPORT"
)

df_tarcarimp <- pivot_fechas(
  dt = df_tarics,
  id_cols = c("taric"),
  fechaini = fechainicial,
  fechafin = fechafinal,
  flujoval = "IMPORT"
)

##### Países -----
df_paises <- obtener_df_pais(
  df = datafil,
  mapeo_paises = map_paises
)

df_paisescarexp <- pivot_fechas(
  dt = df_paises,
  id_cols = c("pais"),
  fechaini = fechainicial,
  fechafin = fechafinal,
  flujoval = "EXPORT"
)

df_paisescarimp <- pivot_fechas(
  dt = df_paises,
  id_cols = c("pais"),
  fechaini = fechainicial,
  fechafin = fechafinal,
  flujoval = "IMPORT"
)

##### CCAAs -----
df_ccaas <- obtener_df_ccaas(
  path_ccaa = ccaas_path,
  mapeo_regiones = map_regiones
)

df_ccaacarexp <- pivot_fechas(
  dt = df_ccaas,
  id_cols = c("ccaa", "Condn"),
  fechaini = fechainicial,
  fechafin = fechafinal,
  flujoval = "EXPORT"
)

df_ccaacarimp <- pivot_fechas(
  dt = df_ccaas,
  id_cols = c("ccaa", "Condn"),
  fechaini = fechainicial,
  fechafin = fechafinal,
  flujoval = "IMPORT"
)

data.table::setorder(df_ccaacarexp, Condn)
data.table::setorder(df_ccaacarimp, Condn)


#### Guardado datos ----
##### Excel con DAX -----
guardar_datos_brutos(
  dt_ccaas = df_ccaas,
  dt_tarics = df_tarics,
  dt_paises = df_paises,
  dir_salida = salida_path
)

##### Exceles originales -----
actualizar_exceles(
  excel_path = actualizador,
  dt_ccaas = map_regiones,
  dt_tarics = map_tarics,
  dt_paises = map_paises,
  ccaa_exp = df_ccaacarexp,
  ccaa_imp = df_ccaacarimp,
  taric_exp = df_tarcarexp,
  taric_imp = df_tarcarimp,
  paises_exp = df_paisescarexp,
  paises_imp = df_paisescarimp
)

#### Calculo variables datos ----
path_data <- file.path(salida_path, "datos_brutos_de.xlsx")
df_ccaa <- cargar_datos_brutos(path_data, "df_ccaa", "ccaa")
df_taric <- cargar_datos_brutos(path_data, "df_taric", "taric")
df_paises <- cargar_datos_brutos(path_data, "df_paises", "pais")

##### Mensual -----
datos_procesados <- procesa_datos(df_ccaa, df_taric, df_paises, "mes")
df_ccaa_amp <- datos_procesados$ccaa %>% formato_mes
df_taric_amp <- datos_procesados$taric %>% formato_mes
df_paises_amp <- datos_procesados$paises %>% formato_mes
rm(datos_procesados)

##### Trimestral -----
datos_trimestrales <- agrupar_trimestre(df_ccaa, df_taric, df_paises)
datos_trim_procesados <- datos_trimestrales %>%
  map(~mutate(., var = "trim")) %>%
  {procesa_datos(.$ccaa, .$taric, .$paises, "trim")}

# Asignación de resultados trimestrales
df_ccaa_trim_amp <- datos_trim_procesados$ccaa %>% formato_trimestre()
df_taric_trim_amp <- datos_trim_procesados$taric %>% formato_trimestre()
df_paises_trim_amp <- datos_trim_procesados$paises %>% formato_trimestre()

##### Anual -----
df_ccaa_anos_amp <- crear_datos_anuales(df_ccaa_amp)
df_taric_anos_amp <- crear_datos_anuales(df_taric_amp)
df_paises_anos_amp <- crear_datos_anuales(df_paises_amp)

##### Salvado datos -----
guardar_datos_procesados(
  df_ccaa_amp, df_taric_amp, df_paises_amp,
  df_ccaa_trim_amp, df_taric_trim_amp, df_paises_trim_amp,
  df_ccaa_anos_amp, df_taric_anos_amp, df_paises_anos_amp,
  path = salida_path
)