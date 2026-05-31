#### Carga wd y librerías ----
setwd(this.path::this.dir())
source("./Raux/procfunapprepercusiones.R")

# Carga de librerías
librerias <- c("arrow", "data.table", "dplyr", "DT", "lubridate", "openxlsx", "plotly", "readr", "shiny", "vroom")
instala_carga_librerias(librerias)
rm(librerias)

#### Carga metadatos ----
df_taric <- cargar_taric("../../datos/metadatos/TARIC.csv")
df_pais  <- cargar_pais("../../datos/metadatos/paises.xlsx")

#### Constantes (del setup del .qmd) ----
factoreuros <- as.integer(1e6)
factorkg <- as.integer(1e3)
uneuros <- "mill. €"
unekg <- "Tm"
texeuros <- "millones de euros"
texkg <- "toneladas métricas"
nbarras <- 10L

#### Parámetros ----
# Base
unidades    <- "euros"
region      <- "mad"
cflujo      <- 0L                           
ano         <- 2019L
per         <- 6L 
nivel_tar   <- 1L
n_barras    <- nbarras

# Lista de valores/parámetros (equivalente a valores() del .qmd)
vals <- list(
  unidades  = unidades,
  region    = region,
  flujo     = cflujo,
  ano       = ano,
  per       = per,
  nivel     = nivel_tar,
  n_barras  = n_barras
)

##### Variables auxiliares -----
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

# Texto del nivel TARIC
nivel_taric_char <- switch(as.character(nivel_tar),
                           "0" = "Todos los niveles",
                           "1" = "Capítulos", 
                           "2" = "Partidas",
                           "3" = "Subpartidas",
                           "4" = "Nomenclaturas combinadas",
                           "5" = "Aranceles",
                           "Todos los niveles"
)

# Texto inicial según nivel
texto_ini <- switch(as.character(nivel_tar),
                    "0" = "el producto",
                    "1" = "el capítulo", 
                    "2" = "la partidas",
                    "3" = "la subpartida",
                    "4" = "la nomenclatura combinada",
                    "5" = "el arancel",
                    "el producto"
)

# Texto periodo
texto_periodo <- switch(as.character(per),
                        "1" = "enero de",
                        "2" = "febrero de",
                        "3" = "marzo de",
                        "4" = "abril de",
                        "5" = "mayo de",
                        "6" = "junio de",
                        "7" = "julio de",
                        "8" = "agosto de",
                        "9" = "septiembre de",
                        "10" = "octubre de",
                        "11" = "noviembre de",
                        "12" = "diciembre de",
                        "21" = "el primer trimestre de",
                        "22" = "el segundo trimestre de",
                        "23" = "el tercer trimestre de",
                        "24" = "el cuarto trimestre de",
                        "31" = "el primer semestre de",
                        "32" = "el segundo semestre de",
                        "41" = "año completo",
                        "51" = "el periodo enero-febrero de",
                        "52" = "el periodo enero-abril de",
                        "53" = "el periodo enero-mayo de",
                        "54" = "el periodo enero-julio de",
                        "55" = "el periodo enero-agosto de",
                        "56" = "el periodo enero-septiembre de",
                        "57" = "el periodo enero-octubre de",
                        "58" = "el periodo enero-noviembre de",
                        "Período no válido"
)

# Paleta de colores según flujo
paleta <- if (cflujo == 1L) {
  c(col1 = "#2d5532", col2 = "#b4d7b4", col3 = "#6f6f4e", col4 = "#ddd9c3")
} else if (cflujo == 0L) {
  c(col1 = "#2d3535", col2 = "#b4c7d7", col3 = "#4f6f8f", col4 = "#c3d9dd")
} else {
  c(col1 = "#2d5532", col2 = "#b4d7b4", col3 = "#6f6f4e", col4 = "#ddd9c3")
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

# Texto unidades de medida
texud <- if (unidades == "euros") {
  texeuros
} else if (unidades == "kg") {
  texkg
} else {
  NA_character_
}

# Lista de variables auxiliares (equivalente a aux_vars() del .qmd)
aux <- list(
  nombre_region    = nombre_region,
  nombre_flujo     = nombre_flujo,
  paleta           = paleta,
  meses            = meses,
  var              = var,
  varfactor        = varfactor,
  varud            = varud,
  nivel_taric_char = nivel_taric_char,
  texto_ini        = texto_ini,
  texto_periodo    = texto_periodo,
  texud            = texud
)

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

# Filtrado según nivel TARIC
if (nivel_tar == 0) {
  datos_filtrados <- arrow::open_dataset(archivo) %>%
    dplyr::filter(
      flujo == cflujo,
      año >= (ano) - 1,
      año <= ano,
      mes %in% meses
    ) %>%
    dplyr::select(-flujo) %>%
    collect() %>%
    as.data.table()
} else {
  rango_min <- switch(as.character(nivel_tar),
                      "1" = 1, "2" = 100, "3" = 10000, "4" = 1000000, "5" = 100000000
  )
  rango_max <- switch(as.character(nivel_tar),
                      "1" = 99, "2" = 9999, "3" = 999999, "4" = 99999999, "5" = 9999999999
  )
  
  datos_filtrados <- arrow::open_dataset(archivo) %>%
    dplyr::filter(
      flujo == cflujo,
      año >= (ano) - 1,
      año <= ano,
      mes %in% meses,
      cod_taric == 0L | (cod_taric >= rango_min & cod_taric <= rango_max)
    ) %>%
    dplyr::select(-flujo) %>%
    collect() %>%
    as.data.table()
}

cat("Datos filtrados:", nrow(datos_filtrados), "registros\n")

# Agregar directamente con nombre var
datos_agg <- datos_filtrados[, .(vol = sum(get(var), na.rm = TRUE)), 
                             by = .(año, pais, cod_taric)]

# Separar en actual y previo, eliminando columna `año`
df_actual <- datos_agg[año == ano][, -"año", with = FALSE]
df_previo <- datos_agg[año == (ano - 1)][, -"año", with = FALSE]

# Renombrar columnas
setnames(df_actual, "vol", paste0(var, "_periodo"))
setnames(df_previo, "vol", paste0(var, "_periodo_prev"))

# Full outer join
df_final <- merge(df_actual, df_previo, by = c("pais", "cod_taric"), all = TRUE)

# Reemplazar NAs con ceros
col_actual <- paste0(var, "_periodo")
col_previo <- paste0(var, "_periodo_prev")

df_final[is.na(get(col_actual)), (col_actual) := 0]
df_final[is.na(get(col_previo)), (col_previo) := 0]

# Valor de referencia
ref_val <- df_final[pais == 0L & cod_taric == 0L, get(paste0(var, "_periodo_prev"))]
ref_val_act <- df_final[pais == 0L & cod_taric == 0L, get(paste0(var, "_periodo"))]

cat("Valor de referencia:", ref_val, "\n")

# Subconjuntos
df_nivel <- df_final[pais != 0L & cod_taric != 0L]
df_tar   <- df_final[pais == 0L & cod_taric != 0L]
df_paises  <- df_final[pais != 0L & cod_taric == 0L]

#### Preparación datasets para contribuciones ----
df_nivel_pos <- preparacion_dataplot_contribuciones(df_nivel, var, c("pais", "cod_taric"), n_barras, 
                                                    "positivas", varfactor, ref_val)
df_nivel_neg <- preparacion_dataplot_contribuciones(df_nivel, var, c("pais", "cod_taric"), n_barras, 
                                                    "negativas", varfactor, ref_val)

df_tar_pos <- preparacion_dataplot_contribuciones(df_tar, var, "cod_taric", n_barras, 
                                                  "positivas", varfactor, ref_val)
df_tar_neg <- preparacion_dataplot_contribuciones(df_tar, var, "cod_taric", n_barras, 
                                                  "negativas", varfactor, ref_val)

df_paises_pos <- preparacion_dataplot_contribuciones(df_paises, var, "pais", n_barras, 
                                                     "positivas", varfactor, ref_val)
df_paises_neg <- preparacion_dataplot_contribuciones(df_paises, var, "pais", n_barras, 
                                                     "negativas", varfactor, ref_val)

cat("Datasets de contribuciones preparados\n")

#### Cruce con catálogo de TARIC y país ----
df_nivel_pos <- cruce_taric_pais(df_nivel_pos, df_taric, df_pais)
df_nivel_neg <- cruce_taric_pais(df_nivel_neg, df_taric, df_pais)
df_tar_pos   <- cruce_taric_pais(df_tar_pos, df_taric, df_pais)
df_tar_neg   <- cruce_taric_pais(df_tar_neg, df_taric, df_pais)
df_paises_pos  <- cruce_taric_pais(df_paises_pos, df_taric, df_pais)
df_paises_neg  <- cruce_taric_pais(df_paises_neg, df_taric, df_pais)

cat("Cruces con catálogos completados\n")

#### Limpiar columnas sobrantes ----
df_nivel_pos <- elimina_cols_sobrante(df_nivel_pos)
df_nivel_neg <- elimina_cols_sobrante(df_nivel_neg)
df_tar_pos   <- elimina_cols_sobrante(df_tar_pos, c("cod_pais", "pais"))
df_tar_neg   <- elimina_cols_sobrante(df_tar_neg, c("cod_pais", "pais"))
df_paises_pos  <- elimina_cols_sobrante(df_paises_pos, c("cod_taric", "Tar"))
df_paises_neg  <- elimina_cols_sobrante(df_paises_neg, c("cod_taric", "Tar"))

cat("Limpieza completada\n")

# Limpieza de memoria
gc(verbose = FALSE)

#### Preparación datasets para volumenes ----
df_vol_taric <- preparacion_dataplot_volumenes(df_tar, var, c("cod_taric"), n_barras, varfactor, ref_val, ref_val_act)
df_vol_paises <- preparacion_dataplot_volumenes(df_paises, var, c("pais"), n_barras, varfactor, ref_val, ref_val_act)
df_nivel <- preparacion_dataplot_volumenes(df_nivel, var, c("cod_taric", "pais"), n_barras, varfactor, ref_val, ref_val_act)

df_vol_taric <- cruce_taric_pais(df_vol_taric, df_taric, df_pais)
df_vol_paises <- cruce_taric_pais(df_vol_paises, df_taric, df_pais)
df_nivel <- cruce_taric_pais(df_nivel, df_taric, df_pais)

df_vol_taric <- elimina_cols_sobrante(df_vol_taric, c("cod_pais", "pais"))
df_vol_paises <- elimina_cols_sobrante(df_vol_paises, c("cod_taric", "Tar"))
df_nivel <- elimina_cols_sobrante(df_nivel)

#### Generar gráficos ----
plot_vol_taric <- grafica_volumen_taric(df_vol_taric, aux, vals)
plot_vol_taric

plot_vol_paises <- grafica_volumen_paises(df_vol_paises, aux, vals)
plot_vol_paises

plot_vol_pt <- grafica_volumen_pt(df_nivel, aux, vals)
plot_vol_pt
