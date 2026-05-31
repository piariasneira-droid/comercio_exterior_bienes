#### Carga wd y librerías ----
setwd(dirname(this.path::this.path()))
source("./Raux/procfunapptreemap.R")

# Carga de librerías
librerias <- c("arrow", "data.table", "dplyr", "DT", "lubridate", "openxlsx", "plotly", "readr", "shiny", "vroom")
instala_carga_librerias(librerias)
rm(librerias)

#### Carga metadatos ----
df_taric <- cargar_taric("../../datos/metadatos/TARIC.csv")
df_pais  <- cargar_pais("../../datos/metadatos/paises.xlsx")

#### Parámetros (simulando inputs del Shiny) ----
factoreuros <- as.integer(1e6)
factorkg <- as.integer(1e3)
uneuros <- "mill. €"
unekg <- "Tm"
texeuros <- "millones de euros"
texkg <- "toneladas métricas"
decdefecto <- 1L

# Puedes cambiar estos valores para probar diferentes escenarios
region <- "mad"       
unidades <- "euros"    
cflujo <- 1L          
ano_ini <- 2020L
ano_fin <- 2025L
ctaric <- 0          
cpais <- 0L           
per <- 54L             
n_lineas <- 5L

valores <- list(
  unidades = unidades,
  region   = region,
  flujo    = cflujo,                           
  ano_ini  = ano_ini,
  ano_fin  = ano_fin,
  taric    = ctaric,              
  pais     = cpais,
  per      = per,
  n_lineas = n_lineas
)

#### Variables auxiliares (equivalente a aux_vars reactive) ----
# Convertir ctaric a numérico
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
  fila <- df_taric[df_taric$codint_taric == ctaric, ]
  if (nrow(fila) > 0) {
    nombre_taric <- paste0(fila$cod_taric, " - ", fila$taric)
  }
}

# Nivel TARIC
nivel_taric <- NA_integer_
if (!is.na(ctaric)) {
  fila <- df_taric[df_taric$codint_taric == ctaric, ]
  if (nrow(fila) > 0) {
    nivel_taric <- as.integer(fila$nivel_taric)
  }
}

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
  fila <- df_pais[df_pais$cod_pais == cpais, ]
  if (nrow(fila) > 0) {
    nombre_pais <- fila$pais
  }
}

# Meses según periodo seleccionado
meses <- obtener_meses_periodo(per)

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
  factoreuros
} else if (unidades == "kg") {
  factorkg
} else {
  NA_integer_
}

# Unidad de medida
varud <- if (unidades == "euros") {
  uneuros
} else if (unidades == "kg") {
  unekg
} else {
  NA_character_
}

res <- list(
  nombre_region    = nombre_region,
  nombre_flujo     = nombre_flujo,
  nombre_taric     = nombre_taric,
  nivel_taric      = nivel_taric,
  paleta           = paleta,
  nombre_pais      = nombre_pais,
  meses            = meses,
  var              = var,
  varfactor        = varfactor,
  varud            = varud
)

# Limpieza
rm(fila)

#### Carga data (equivalente a obtencion_data reactive) ----
# Selección de archivo según región y unidades
archivo <- switch(
  paste(region, unidades, sep = "_"),
  "mad_euros" = "../../datos/totales_mad_esp/de_mad_euros.parquet",
  "esp_euros" = "../../datos/totales_mad_esp/de_esp_euros.parquet",
  "mad_kg" = "../../datos/totales_mad_esp/de_mad_kg.parquet",
  "esp_kg" = "../../datos/totales_mad_esp/de_esp_kg.parquet",
  stop("Combinación región/unidades no válida")
)

cat("========================================\n")
cat("PARÁMETROS DE LA CONSULTA:\n")
cat("========================================\n")
cat("Archivo:", archivo, "\n")
cat("Región:", nombre_region, "\n")
cat("Flujo:", nombre_flujo, "\n")
cat("Años:", ano_ini, "-", ano_fin, "\n")
cat("TARIC:", ctaric, "-", nombre_taric, "\n")
cat("País:", cpais, "-", nombre_pais, "\n")
cat("Variable:", var, "\n")
cat("Unidades:", varud, "\n")
cat("Meses período:", paste(meses, collapse = ", "), "\n")
cat("========================================\n\n")

# Lectura y filtrado inicial de datos
datos_completos <- arrow::open_dataset(archivo) %>%
  filter(
    flujo == cflujo,
    año >= ano_ini,
    año <= ano_fin,
    cod_taric == ctaric |
      cod_taric == 0 |
      pais == cpais |
      pais == 0L
  ) %>%
  select(-flujo) %>%
  collect() %>%
  as.data.table()

cat("Datos cargados:", nrow(datos_completos), "filas\n\n")

# Crear subconjuntos
df_mes_tar <- datos_completos[cod_taric == ctaric & pais != 0L]
df_mes_paises <- datos_completos[pais == cpais & cod_taric != 0L]
df_total_general <- datos_completos[cod_taric == 0 & pais == 0L]
df_total_taric <- datos_completos[cod_taric == ctaric & pais == 0L]
df_total_pais <- datos_completos[cod_taric == 0 & pais == cpais]

# Merge de totales para df_mes_tar
df_mes_tar <- merge(
  df_mes_tar,
  df_total_general[, .(año, mes, total_general = get(var))],
  by = c("año", "mes"),
  all.x = TRUE
)

df_mes_tar <- merge(
  df_mes_tar,
  df_total_taric[, .(año, mes, total_taric = get(var))],
  by = c("año", "mes"),
  all.x = TRUE
)

# Merge de totales para df_mes_paises
df_mes_paises <- merge(
  df_mes_paises,
  df_total_general[, .(año, mes, total_general = get(var))],
  by = c("año", "mes"),
  all.x = TRUE
)

df_mes_paises <- merge(
  df_mes_paises,
  df_total_pais[, .(año, mes, total_pais = get(var))],
  by = c("año", "mes"),
  all.x = TRUE
)

# DATAFRAMES DE PERIODO (agrupados por año, sumando los meses del periodo)
df_periodo_tar <- df_mes_tar[mes %in% meses, 
                             .(valor = sum(get(var), na.rm = TRUE),
                               total_general = sum(total_general, na.rm = TRUE),
                               total_taric = sum(total_taric, na.rm = TRUE)),
                             by = .(año, pais, cod_taric)]

df_periodo_paises <- df_mes_paises[mes %in% meses,
                                   .(valor = sum(get(var), na.rm = TRUE),
                                     total_general = sum(total_general, na.rm = TRUE),
                                     total_pais = sum(total_pais, na.rm = TRUE)),
                                   by = .(año, pais, cod_taric)]

# Renombrar columna 'valor' con el nombre de la unidad
setnames(df_periodo_tar, "valor", var)
setnames(df_periodo_paises, "valor", var)

#### Calcular valores año anterior y métricas ----

# Para df_mes_tar
if (nrow(df_mes_tar) > 0) {
  setkey(df_mes_tar, pais, cod_taric, mes, año)
  
  # Crear tabla temporal con año siguiente
  dt_ant <- df_mes_tar[, .(pais, cod_taric, mes, 
                           año_sig = año + 1L,
                           valor_ant = get(var), 
                           total_general_ant = total_general,
                           total_taric_ant = total_taric)]
  
  # Join con año anterior
  df_mes_tar[dt_ant, 
             c(paste0(var, "_ant"), "total_general_ant", "total_taric_ant") :=
               .(i.valor_ant, i.total_general_ant, i.total_taric_ant),
             on = .(pais, cod_taric, mes, año = año_sig)]
  
  # Reemplazar NAs con 0
  cols_ant <- c(paste0(var, "_ant"), "total_general_ant", "total_taric_ant")
  df_mes_tar[, (cols_ant) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = cols_ant]
  
  # Paso 1: Calcular diferencia, peso y tva
  df_mes_tar[, `:=`(
    diferencia = get(var) - get(paste0(var, "_ant")),
    peso = fifelse(total_general > 0, get(var) / total_general * 100, 0),
    tva = fifelse(get(paste0(var, "_ant")) > 0,
                  (get(var) - get(paste0(var, "_ant"))) / get(paste0(var, "_ant")) * 100,
                  NA_real_)
  )]
  
  # Paso 2: Calcular contribución
  df_mes_tar[, contribucion := fifelse(total_general_ant > 0,
                                       diferencia / total_general_ant * 100,
                                       0)]
}

# Para df_periodo_tar
if (nrow(df_periodo_tar) > 0) {
  setkey(df_periodo_tar, pais, cod_taric, año)
  
  dt_ant <- df_periodo_tar[, .(pais, cod_taric, 
                               año_sig = año + 1L,
                               valor_ant = get(var), 
                               total_general_ant = total_general,
                               total_taric_ant = total_taric)]
  
  df_periodo_tar[dt_ant, 
                 c(paste0(var, "_ant"), "total_general_ant", "total_taric_ant") :=
                   .(i.valor_ant, i.total_general_ant, i.total_taric_ant),
                 on = .(pais, cod_taric, año = año_sig)]
  
  cols_ant <- c(paste0(var, "_ant"), "total_general_ant", "total_taric_ant")
  df_periodo_tar[, (cols_ant) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = cols_ant]
  
  # Paso 1: Calcular diferencia, peso y tva
  df_periodo_tar[, `:=`(
    diferencia = get(var) - get(paste0(var, "_ant")),
    peso = fifelse(total_general > 0, get(var) / total_general * 100, 0),
    tva = fifelse(get(paste0(var, "_ant")) > 0, 
                  (get(var) - get(paste0(var, "_ant"))) / get(paste0(var, "_ant")) * 100, 
                  NA_real_)
  )]
  
  # Paso 2: Calcular contribución
  df_periodo_tar[, contribucion := fifelse(total_general_ant > 0, 
                                           diferencia / total_general_ant * 100, 
                                           0)]
}

# Para df_periodo_paises
if (nrow(df_periodo_paises) > 0) {
  setkey(df_periodo_paises, pais, cod_taric, año)
  
  dt_ant <- df_periodo_paises[, .(pais, cod_taric, 
                                  año_sig = año + 1L,
                                  valor_ant = get(var), 
                                  total_general_ant = total_general,
                                  total_pais_ant = total_pais)]
  
  df_periodo_paises[dt_ant, 
                    c(paste0(var, "_ant"), "total_general_ant", "total_pais_ant") :=
                      .(i.valor_ant, i.total_general_ant, i.total_pais_ant),
                    on = .(pais, cod_taric, año = año_sig)]
  
  cols_ant <- c(paste0(var, "_ant"), "total_general_ant", "total_pais_ant")
  df_periodo_paises[, (cols_ant) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = cols_ant]
  
  # Paso 1: Calcular diferencia, peso y tva
  df_periodo_paises[, `:=`(
    diferencia = get(var) - get(paste0(var, "_ant")),
    peso = fifelse(total_general > 0, get(var) / total_general * 100, 0),
    tva = fifelse(get(paste0(var, "_ant")) > 0, 
                  (get(var) - get(paste0(var, "_ant"))) / get(paste0(var, "_ant")) * 100, 
                  NA_real_)
  )]
  
  # Paso 2: Calcular contribución
  df_periodo_paises[, contribucion := fifelse(total_general_ant > 0, 
                                              diferencia / total_general_ant * 100, 
                                              0)]
}

cat("Métricas calculadas (diferencia, tva, contribución, peso)\n\n")

# Cruces con catálogos
df_mes_tar         <- cruce_taric_pais(df_mes_tar, df_taric, df_pais)
df_periodo_tar     <- cruce_taric_pais(df_periodo_tar, df_taric, df_pais)
df_mes_paises      <- cruce_taric_pais(df_mes_paises, df_taric, df_pais)
df_periodo_paises  <- cruce_taric_pais(df_periodo_paises, df_taric, df_pais)

rm(df_total_general, df_total_taric, df_total_pais)


plot <- grafica_treemap_taric(
  dt = df_periodo_paises[año == ano_fin], 
  vars = valores, 
  aux = res, 
  tipo_plot = "treemap"
)

plot <- grafica_treemap_paises(
  dt = df_periodo_tar[año == ano_fin],
  vars = valores,
  aux = res,
  tipo_plot = "treemap"
)

plot

#### Resumen de subconjuntos ----
cat("========================================\n")
cat("SUBCONJUNTOS CREADOS:\n")
cat("========================================\n")
cat("df_mes_tar (TARIC todos meses):", nrow(df_mes_tar), "filas\n")
cat("df_periodo_tar (TARIC período):", nrow(df_periodo_tar), "filas\n")
cat("df_mes_paises (País todos meses):", nrow(df_mes_paises), "filas\n")
cat("df_periodo_paises (País período):", nrow(df_periodo_paises), "filas\n")
cat("----------------------------------------\n")
cat("df_total_general (Total general todos):", nrow(df_total_general), "filas\n")
cat("df_total_taric (Total TARIC todos):", nrow(df_total_taric), "filas\n")
cat("df_total_pais (Total país todos):", nrow(df_total_pais), "filas\n")
cat("========================================\n\n")