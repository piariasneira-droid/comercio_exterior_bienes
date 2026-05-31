# main_tablas.R
# Generación de tablas Flextable y Excel de comercio exterior Madrid vs España

# Entorno ----
source("./scr/R/nota_sectores/main_etl.r")
source("./scr/R/nota_sectores/procfun/funciones_flextable.r")

.solo_imagen  <- c("col_contrib_bar", "ancho_cm", "alto_cm", "dpi", "subtitulo", "fuente")
.solo_img_evol <- c("ancho_cm", "dpi", "subtitulo", "fuente")
.filtrar_args <- function(args, excluir) args[!names(args) %in% excluir]

# Nombres de archivo ----
m_start  <- min(params$meses)
m_end    <- max(params$meses)

sufijo_m <- if (m_start == m_end) {
  sprintf("%d_%02d",         params$anho, m_start)
} else {
  sprintf("%d_%02d-%02d",    params$anho, m_start, m_end)
}

sufijo_evol <- if (m_start == m_end) {
  sprintf("%d_%d_%02d",      params$anho_idx, params$anho, m_start)
} else {
  sprintf("%d_%d_%02d-%02d", params$anho_idx, params$anho, m_start, m_end)
}

rm(m_start, m_end)

# Argumentos tablas ----

## Sectores ----
### Madrid ----
args_sec_mad <- list(
  tabla           = df_sec_mad,
  omitir_orden    = c(99, 100),
  col_contrib_bar = "exp_mad_contrib",
  titulo          = paste0("Comercio exterior de la Comunidad de Madrid por sectores económicos. ", params$texto_meses),
  subtitulo       = "Valores acumulados, peso sobre el total, tasas de variación e implicación al crecimiento",
  caption         = params$caption,
  tam_fuente      = params$gt_tam_fuente,
  col_pal         = params$gt_col_pal,
  dec_num         = params$dec_eur,
  dec_pct         = params$dec_per
)

### España ----
args_sec_esp <- args_sec_mad
args_sec_esp$tabla           <- df_sec_esp
args_sec_esp$col_contrib_bar <- "exp_esp_contrib"
args_sec_esp$titulo          <- paste0("Comercio exterior de España por sectores económicos. ", params$texto_meses)
args_sec_esp$cols_exp        <- c("exp_esp", "exp_esp_pct", "exp_esp_tva", "exp_esp_contrib")
args_sec_esp$cols_imp        <- c("imp_esp", "imp_esp_pct", "imp_esp_tva", "imp_esp_contrib")
args_sec_esp$cols_extra      <- c("saldo_esp", "tasa_cob_esp")

## Países ----
### Madrid ----
args_pais_mad <- list(
  tabla           = df_pais_mad,
  col_contrib_bar = "exp_mad_contrib",
  titulo          = paste0("Comercio exterior de la Comunidad de Madrid por áreas geográficas y países. ", params$texto_meses),
  subtitulo       = "Valores acumulados, peso sobre el total, tasas de variación e implicación al crecimiento",
  caption         = params$caption,
  tam_fuente      = params$gt_tam_fuente,
  col_pal         = params$gt_col_pal,
  dec_num         = params$dec_eur,
  dec_pct         = params$dec_per
)

### España ----
args_pais_esp <- args_pais_mad
args_pais_esp$tabla           <- df_pais_esp
args_pais_esp$col_contrib_bar <- "exp_esp_contrib"
args_pais_esp$titulo          <- paste0("Comercio exterior de España por áreas geográficas y países. ", params$texto_meses)
args_pais_esp$cols_exp        <- c("exp_esp", "exp_esp_pct", "exp_esp_tva", "exp_esp_contrib")
args_pais_esp$cols_imp        <- c("imp_esp", "imp_esp_pct", "imp_esp_tva", "imp_esp_contrib")
args_pais_esp$cols_extra      <- c("saldo_esp", "tasa_cob_esp")


# Ejecución de exportaciones ----

## 1. IMÁGENES (FLEXTABLE) ----

### Sectores Madrid ----
do.call(exportar_sectores_imagen, c(
  args_sec_mad,
  list(ruta_salida = file.path(params$path_outx, sprintf("tabla_sectores_madrid_%s.png", sufijo_m)))
))

### Sectores España ----
do.call(exportar_sectores_imagen, c(
  args_sec_esp,
  list(ruta_salida = file.path(params$path_outx, sprintf("tabla_sectores_espana_%s.png", sufijo_m)))
))

### Países Madrid ----
do.call(exportar_paises_imagen, c(
  args_pais_mad,
  list(ruta_salida = file.path(params$path_outx, sprintf("tabla_paises_madrid_%s.png", sufijo_m)))
))

### Países España ----
do.call(exportar_paises_imagen, c(
  args_pais_esp,
  list(ruta_salida = file.path(params$path_outx, sprintf("tabla_paises_espana_%s.png", sufijo_m)))
))


## 2. EXCEL (OPENXLSX) ----

### Sectores Madrid ----
do.call(exportar_sectores_excel, c(
  .filtrar_args(args_sec_mad, .solo_imagen),
  list(ruta_salida = file.path(params$path_outx, sprintf("tabla_sectores_madrid_%s.xlsx", sufijo_m)))
))

### Sectores España ----
do.call(exportar_sectores_excel, c(
  .filtrar_args(args_sec_esp, .solo_imagen),
  list(ruta_salida = file.path(params$path_outx, sprintf("tabla_sectores_espana_%s.xlsx", sufijo_m)))
))

### Países Madrid ----
do.call(exportar_paises_excel, c(
  .filtrar_args(args_pais_mad, .solo_imagen),
  list(ruta_salida = file.path(params$path_outx, sprintf("tabla_paises_madrid_%s.xlsx", sufijo_m)))
))

### Países España ----
do.call(exportar_paises_excel, c(
  .filtrar_args(args_pais_esp, .solo_imagen),
  list(ruta_salida = file.path(params$path_outx, sprintf("tabla_paises_espana_%s.xlsx", sufijo_m)))
))


# 3. EVOLUTIVOS (EXCEL) ----

## Evolución porcentual sectores ----
exportar_evol_pct_excel(
  tabla       = df_evol_aux_sec,
  flujo       = "exp", tipo = "sectores",
  titulo      = paste0("Exportaciones de Madrid por sectores — Estructura porcentual ", params$anho_idx, "–", params$anho),
  anos_mostrar = params$anho_idx:params$anho,
  dec_pct = params$dec_per, caption = params$caption,
  tam_fuente = params$gt_tam_fuente, col_pal = params$gt_col_pal,
  ruta_salida = file.path(params$path_outx, sprintf("tablagt_evol_pct_exp_sec_%s.xlsx", sufijo_evol))
)

exportar_evol_pct_excel(
  tabla       = df_evol_aux_sec,
  flujo       = "imp", tipo = "sectores",
  titulo      = paste0("Importaciones de Madrid por sectores — Estructura porcentual ", params$anho_idx, "–", params$anho),
  anos_mostrar = params$anho_idx:params$anho,
  dec_pct = params$dec_per, caption = params$caption,
  tam_fuente = params$gt_tam_fuente, col_pal = params$gt_col_pal,
  ruta_salida = file.path(params$path_outx, sprintf("tablagt_evol_pct_imp_sec_%s.xlsx", sufijo_evol))
)

## Evolución porcentual países ----
exportar_evol_pct_excel(
  tabla       = df_evol_aux_pais,
  flujo       = "exp", tipo = "paises",
  titulo      = paste0("Exportaciones de Madrid por países — Estructura porcentual ", params$anho_idx, "–", params$anho),
  anos_mostrar = params$anho_idx:params$anho,
  dec_pct = params$dec_per, caption = params$caption,
  tam_fuente = params$gt_tam_fuente, col_pal = params$gt_col_pal,
  ruta_salida = file.path(params$path_outx, sprintf("tablagt_evol_pct_exp_pais_%s.xlsx", sufijo_evol))
)

exportar_evol_pct_excel(
  tabla       = df_evol_aux_pais,
  flujo       = "imp", tipo = "paises",
  titulo      = paste0("Importaciones de Madrid por países — Estructura porcentual ", params$anho_idx, "–", params$anho),
  anos_mostrar = params$anho_idx:params$anho,
  dec_pct = params$dec_per, caption = params$caption,
  tam_fuente = params$gt_tam_fuente, col_pal = params$gt_col_pal,
  ruta_salida = file.path(params$path_outx, sprintf("tablagt_evol_pct_imp_pais_%s.xlsx", sufijo_evol))
)