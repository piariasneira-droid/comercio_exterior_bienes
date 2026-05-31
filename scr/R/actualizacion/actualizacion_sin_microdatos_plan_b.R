#### Establecimiento marco de trabajo de trabajo ----
source("./scr/R/actualizacion/auxiliar/procactsinmicro.R")
library(dplyr)
library(data.table)

#### Definición de parámetros ----
dirdataccaa <- "./data/rawplanb/CCAA"
dirdatataric <- "./data/rawplanb/TARIC"
dirdatapaises <- "./data/rawplanb/Paises"
pathsalida <- "./data/output/ccaacappais/datos_brutos_de_alt.xlsx"
salida_path <- "./data/output/ccaacappais"

#### Carga de datos ----
dfccaa <- leer_csv_directorio(dirdataccaa)
dftaric <- leer_csv_directorio(dirdatataric)
dfpaises <- leer_csv_directorio(dirdatapaises)

mapregiones <-  data.table::as.data.table(readxl::read_excel("./data/metatratado/regiones.xlsx"))
mappaises <-  data.table::as.data.table(readxl::read_excel("./data/metatratado/estados.xlsx"))
maptarics <-  data.table::as.data.table(readxl::read_excel("./data/metatratado/capitulos.xlsx"))

#### Data handling ----
##### CCAA ----
dfccaa <- dfccaa[subfila != "Total seleccionado"]
dfccaa[, subcolumna := NULL]

data.table::setnames(dfccaa,
         old = c("fila", "subfila", "columna"),
         new = c("ccaa", "Fecha", "flujo"))

dfccaa <- mapregiones[dfccaa,
                      on = .(Cod = ccaa),
                      .(Región, Condn, flujo, Fecha, valor)]

dfccaa[, valor := as.numeric(gsub(",", ".", valor))]
dfccaa[, valor := valor / 1e6]
data.table::setnames(dfccaa, "Región", "ccaa")
dfccaa <- convertir_fecha(dfccaa, "Fecha")


##### TARIC ----
dftaric <- dftaric[subfila != "Total seleccionado"]
dftaric <- dftaric[fila != "Total seleccionado"]
dftaric[, subcolumna := NULL]

data.table::setnames(dftaric,
         old = c("fila", "subfila", "columna"),
         new = c("taric", "Fecha", "flujo"))

dftaric[, valor := as.numeric(gsub(",", ".", valor))]
dftaric[, valor := valor / 1e6]
dftaric <- convertir_fecha(dftaric, "Fecha")

##### Paises ----
dfpaises <- dfpaises[fila != "Total seleccionado"]
dfpaises[, subcolumna := NULL]

data.table(setnames(dfpaises,
         old = c("fila", "subfila", "columna"),
         new = c("pais", "Fecha", "flujo")))

dfpaises[, valor := as.numeric(gsub(",", ".", valor))]
dfpaises[, valor := valor / 1e6]
dfpaises <- convertir_fecha(dfpaises, "Fecha")

#### Guardado ----
sheets <- list(
  df_ccaa  = dfccaa,
  df_taric = dftaric,
  df_paises = dfpaises
)

# Guardar
writexl::write_xlsx(sheets, path = pathsalida)

#### Calculo variables datos ----
df_ccaa <- cargar_datos_brutos(pathsalida, "df_ccaa", "ccaa")
df_taric <- cargar_datos_brutos(pathsalida, "df_taric", "taric")
df_paises <- cargar_datos_brutos(pathsalida, "df_paises", "pais")

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