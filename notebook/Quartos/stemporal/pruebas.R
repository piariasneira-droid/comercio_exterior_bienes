#### Carga wd y librerías ----
setwd(dirname(this.path::this.path()))
source("./Raux/procfunappst.R")

# Carga de librerías
librerias <- c("arrow", "data.table", "dplyr", "DT", "lubridate", "openxlsx", "plotly", "readr", "shiny", "vroom")
instala_carga_librerias(librerias)
rm(librerias)

#### Carga metadatos ----
df_taric <- cargar_taric("../../datos/metadatos/TARIC.csv")
df_pais  <- cargar_pais("../../datos/metadatos/paises.xlsx")

#### Parámetros ----
# Base
unidades <- "euros"
region   <- "mad"
cflujo    <- 0L                           
ano_ini  <- 2019L
ano_fin  <- 2025L
ctaric   <- 27             
cpais     <- 1L
per      <- 41L
index    <- "2019-01-01"

#### Variables auxiliares ----
ctaric <- as.numeric(ctaric)
# Nombre del flujo
nombre_flujo <- if (cflujo == 1L) {
  "Exportaciones"
} else if (cflujo == 0L) {
  "Importaciones"
} else {
  NA_character_
}

# Nombre región legible
nombre_region <- if (region == "mad") {
  "C. de Madrid"
} else if (region == "esp") {
  "España"
} else {
  NA_character_
}

# Nombre de TARIC si el código existe
nombre_taric <- NA_character_
if (!is.na(ctaric)) {
  fila <- df_taric[df_taric$codint_taric == ctaric]
  if (nrow(fila) > 0) {
    nombre_taric <- paste0(fila$cod_taric, " - ", fila$taric)
  }
}

# Nivel TARIC
nivel_taric <- NA_integer_
if (!is.na(ctaric)) {
  fila <- df_taric[df_taric$codint_taric == ctaric]
  if (nrow(fila) > 0) {
    nivel_taric <- as.integer(fila$nivel_taric)
  }
}

# Mapeo de nivel TARIC a texto (asegúrate de que niveles_map esté definido)
nivel_taric_char <- niveles_map[as.character(nivel_taric)]

# Paleta de colores según flujo
paleta <- if (cflujo == 1L) {
  c(col1 = "#2d5532", col2 = "#b4d7b4", col3 = "#6f6f4e", col4 = "#ddd9c3")
} else if (cflujo == 0L) {
  c(col1 = "#2d3535", col2 = "#b4c7d7", col3 = "#4f6f8f", col4 = "#c3d9dd")
} else {
  c(col1 = "#2d5532", col2 = "#b4d7b4", col3 = "#6f6f4e", col4 = "#ddd9c3")
}

# Nombre país
nombre_pais <- NA_character_
if (!is.na(cpais)) {
  fila <- df_pais[df_pais$cod_pais == cpais]
  if (nrow(fila) > 0) {
    nombre_pais <- fila$pais
  }
}

# Meses según periodo seleccionado
meses <- obtener_meses_periodo(per)
rm(periodos_map)

# Nombre variable
var <- if (unidades == "euros") {
  "euros"
} else if (unidades == "kg") {
  "kilogramos"
} else {
  NA_character_
}

# Factor de conversión
varfactor <- if (unidades == "euros") {
  as.integer(1e6)
} else if (unidades == "kg") {
  as.integer(1e3)
} else {
  NA_integer_
}

# Unidad de medida
varud <- if (unidades == "euros") {
  "mill. €"
} else if (unidades == "kg") {
  "Tm"
} else {
  NA_character_
}

# Cálculo de año y mes del índice
anoindex <- as.integer(substr(index, 1, 4))
mesindex <- as.integer(substr(index, 6, 7))

rm(fila)

#### Carga data ----
# Selección de archivo según región y unidades
archivo <- switch(
  paste(region, unidades, sep = "_"),
  "mad_euros" = "../../datos/totales_mad_esp/de_mad_euros.parquet",
  "esp_euros" = "../../datos/totales_mad_esp/de_esp_euros.parquet",
  "mad_kg" = "../../datos/totales_mad_esp/de_mad_kg.parquet",
  "esp_kg" = "../../datos/totales_mad_esp/de_esp_kg.parquet",
  stop("Combinación región/unidades no válida")
)

cat("Conectando con archivo Parquet:", archivo, "\n")

# Abrir dataset con arrow
dataset <- arrow::open_dataset(archivo)

# Lectura y filtrado inicial de datos
datos_filtrados <- dataset %>%
  filter(
    flujo == cflujo,
    año >= (ano_ini - 1),
    año <= ano_fin,
    mes %in% meses,
    ((pais == cpais & cod_taric == ctaric) |
       (pais == 0L & cod_taric == ctaric) |
       (pais == cpais & cod_taric == 0L) |
       (pais == 0L & cod_taric == 0L))
  ) %>%
  select(-flujo) %>%
  collect() %>%
  as.data.table()

# df_per: datos agregados por año
df_per <- datos_filtrados[año >= (ano_ini - 1) & año <= ano_fin & mes %in% meses,
                          .(tmp = sum(get(var), na.rm = TRUE)),
                          by = .(año, pais, cod_taric)]
setnames(df_per, "tmp", var)

# Aplicar pivot_data para df_per
df_per <- pivot_data(
  df = df_per,
  anoini = ano_ini,
  anofin = ano_fin,
  var = var,
  codtaric = ctaric,
  codpais = cpais
)

# df_mes: datos mensuales
df_mes <- pivot_data(
  df = datos_filtrados[año >= (ano_ini - 1) & año <= ano_fin],
  anoini = ano_ini,
  anofin = ano_fin,
  var = var,
  codtaric = ctaric,
  codpais = cpais
)

# df_per_idx: año índice
df_per_idx <- datos_filtrados[año == anoindex & mes %in% meses,
                              .(tmp = sum(get(var), na.rm = TRUE)),
                              by = .(año, pais, cod_taric)]

# Cálculo de referencias índice anuales
ref_idx <- if (nrow(df_per_idx[pais == cpais & cod_taric == ctaric]) > 0) 
  df_per_idx[pais == cpais & cod_taric == ctaric, tmp[1]] else 0
ref_idx_pais <- if (nrow(df_per_idx[pais == 0L & cod_taric == ctaric]) > 0) 
  df_per_idx[pais == 0L & cod_taric == ctaric, tmp[1]] else 0
ref_idx_taric <- if (nrow(df_per_idx[pais == cpais & cod_taric == 0L]) > 0) 
  df_per_idx[pais == cpais & cod_taric == 0L, tmp[1]] else 0
ref_idx_region <- if (nrow(df_per_idx[pais == 0L & cod_taric == 0L]) > 0) 
  df_per_idx[pais == 0L & cod_taric == 0L, tmp[1]] else 0

# df_mes_idx: mes índice
df_mes_idx <- datos_filtrados[año == anoindex & mes == mesindex]

# Cálculo de referencias índice mensuales
ref_idxmes <- if (nrow(df_mes_idx[pais == cpais & cod_taric == ctaric]) > 0) 
  df_mes_idx[pais == cpais & cod_taric == ctaric, get(var)[1]] else 0
ref_idxmes_pais <- if (nrow(df_mes_idx[pais == 0L & cod_taric == ctaric]) > 0) 
  df_mes_idx[pais == 0L & cod_taric == ctaric, get(var)[1]] else 0
ref_idxmes_taric <- if (nrow(df_mes_idx[pais == cpais & cod_taric == 0L]) > 0) 
  df_mes_idx[pais == cpais & cod_taric == 0L, get(var)[1]] else 0
ref_idxmes_region <- if (nrow(df_mes_idx[pais == 0L & cod_taric == 0L]) > 0) 
  df_mes_idx[pais == 0L & cod_taric == 0L, get(var)[1]] else 0

cat("Datos cargados\n")

# Preparación df_per
df_per <- preparacion_dataplot(
  dt = df_per,
  var_base = var,
  factor = varfactor,
  refidx = ref_idx,
  refidx_pais = ref_idx_pais,
  refidx_taric = ref_idx_taric,
  refidx_region = ref_idx_region
)

df_per <- cruce_taric_pais(df_per, df_taric, df_pais)
df_per[, c("cod_taric_char", "taric", "Capítulo", "Partida",
           "Subpartida", "NC", "region", "continente", "nombre") := NULL]

# Preparación df_mes
df_mes <- preparacion_dataplot(
  dt = df_mes,
  var_base = var,
  factor = varfactor,
  refidx = ref_idxmes,
  refidx_pais = ref_idxmes_pais,
  refidx_taric = ref_idxmes_taric,
  refidx_region = ref_idxmes_region
)

df_mes <- cruce_taric_pais(df_mes, df_taric, df_pais)
df_mes[, fecha := as.Date(paste(año, mes, "01", sep = "-"))]
df_mes[, c("año", "mes", "cod_taric_char", "taric", "Capítulo", "Partida",
           "Subpartida", "NC", "region", "continente", "nombre") := NULL]
df_mes <- obtencion_medias_moviles(df_mes)

# Limpiamos RAM
rm(archivo, dataset, datos_filtrados, df_mes_idx, df_per_idx)

cat("Procesamiento completado\n")