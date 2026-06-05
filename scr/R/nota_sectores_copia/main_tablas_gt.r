# main_tablas.R
# Generación de tablas GT de comercio exterior Madrid vs España

# Entorno ----
source("./scr/R/nota_sectores/procfun/funciones_gt.r")

.solo_imagen  <- c("col_contrib_bar", "ancho_cm", "alto_cm", "dpi", "subtitulo", "fuente")
.solo_img_evol <- c("ancho_cm", "dpi", "subtitulo", "fuente")
.filtrar_args <- function(args, excluir) args[!names(args) %in% excluir]

# Nombres de archivo ----
m_start  <- min(paramets$mes)
m_end    <- max(paramets$mes)

sufijo_m <- if (m_start == m_end) {
  sprintf("%d_%02d",         paramets$anho, m_start)
} else {
  sprintf("%d_%02d-%02d",    paramets$anho, m_start, m_end)
}

sufijo_evol <- if (m_start == m_end) {
  sprintf("%d_%d_%02d",      paramets$anho_idx, paramets$anho, m_start)
} else {
  sprintf("%d_%d_%02d-%02d", paramets$anho_idx, paramets$anho, m_start, m_end)
}

rm(m_start, m_end)

# Argumentos tablas ----

## Sectores ----
### Madrid ----
arg_mad_sec <- list(
  tabla           = df_sectores %>%
    filter(orden != 65L) %>%
    mutate(
      nombre = if_else(orden == 66L, "TOTAL", nombre)
    ),
  cols_exp        = c("exp_mad", "exp_mad_pct", "exp_mad_tva", "exp_mad_contrib", "exp_mad_vs_esp"),
  cols_imp        = c("imp_mad", "imp_mad_pct", "imp_mad_tva", "imp_mad_contrib", "imp_mad_vs_esp"),
  cols_extra      = c("saldo_mad", "tasa_cob_mad"),
  omitir_orden    = NULL,
  label_exp       = "Exportaciones",
  label_imp       = "Importaciones",
  label_extra     = "Saldo",
  col_contrib_bar = c("exp_mad_contrib", "imp_mad_contrib"),
  cols_millones   = c("exp_mad", "imp_mad", "saldo_mad"),
  cols_pct        = c("exp_mad_pct", "exp_mad_tva", "imp_mad_pct", "imp_mad_tva",
                      "tasa_cob_mad", "exp_mad_vs_esp", "imp_mad_vs_esp"),
  cols_contrib    = c("exp_mad_contrib", "imp_mad_contrib"),
  header_cols     = c(
    exp_mad        = "Mill. \u20ac",  exp_mad_pct     = "% s/total",
    exp_mad_tva    = "TVA (%)",       exp_mad_contrib = "Con. (p.p.)",
    exp_mad_vs_esp = "% s/Esp.",
    imp_mad        = "Mill. \u20ac",  imp_mad_pct     = "% s/total",
    imp_mad_tva    = "TVA (%)",       imp_mad_contrib = "Con. (p.p.)",
    imp_mad_vs_esp = "% s/Esp.",
    saldo_mad      = "Saldo (M\u20ac)", tasa_cob_mad  = "T. cob. (%)"
  ),
  titulo     = paste0("Comercio exterior de Madrid por sectores \u2014 ", paramets$anho),
  subtitulo  = paste0("Volumen (Mill.\u20ac), estructura porcentual, variaci\u00f3n anual y contribuci\u00f3n al crecimiento. Acumulado enero\u2013diciembre ", paramets$anho, "."),
  caption    = paramets$caption,
  fuente     = paramets$fuente_texto,
  tam_fuente = paramets$gt_tam_fuente,
  dec_num    = paramets$dec_num,
  dec_pct    = paramets$dec_per,
  col_pal    = paramets$gt_col_pal
)

### España ----
arg_esp_sec <- arg_mad_sec
arg_esp_sec$cols_exp        <- c("exp_esp", "exp_esp_pct", "exp_esp_tva", "exp_esp_contrib")
arg_esp_sec$cols_imp        <- c("imp_esp", "imp_esp_pct", "imp_esp_tva", "imp_esp_contrib")
arg_esp_sec$cols_extra      <- c("saldo_esp", "tasa_cob_esp")
arg_esp_sec$col_contrib_bar <- c("exp_esp_contrib", "imp_esp_contrib")
arg_esp_sec$cols_millones   <- c("exp_esp", "imp_esp", "saldo_esp")
arg_esp_sec$cols_pct        <- c("exp_esp_pct", "exp_esp_tva", "imp_esp_pct", "imp_esp_tva", "tasa_cob_esp")
arg_esp_sec$cols_contrib    <- c("exp_esp_contrib", "imp_esp_contrib")
arg_esp_sec$header_cols     <- c(
  exp_esp     = "Mill. \u20ac", exp_esp_pct     = "% s/total",
  exp_esp_tva = "TVA (%)",      exp_esp_contrib = "Con. (p.p.)",
  imp_esp     = "Mill. \u20ac", imp_esp_pct     = "% s/total",
  imp_esp_tva = "TVA (%)",      imp_esp_contrib = "Con. (p.p.)",
  saldo_esp   = "Saldo (M\u20ac)", tasa_cob_esp = "T. cob. (%)"
)
arg_esp_sec$titulo    <- paste0("Comercio exterior de Espa\u00f1a por sectores \u2014 ", paramets$anho)
arg_esp_sec$subtitulo <- paste0("Volumen (Mill.\u20ac), estructura porcentual, variaci\u00f3n anual y contribuci\u00f3n al crecimiento. Datos enero\u2013diciembre ", paramets$anho, ".")

## Países ----
### Madrid ----
arg_mad_pais           <- arg_mad_sec
arg_mad_pais$tabla     <- df_paises %>%
  filter(orden != 71L) %>%
  mutate(
    pais = if_else(orden == 72L, "TOTAL", pais),
    niv = if_else(orden == 72L, 0L, niv)
  )
arg_mad_pais$titulo    <- paste0("Comercio exterior de Madrid por pa\u00edses \u2014 ", paramets$anho)
arg_mad_pais$subtitulo <- paste0("Volumen (Mill.\u20ac), estructura porcentual, variaci\u00f3n anual y contribuci\u00f3n al crecimiento por pa\u00eds y zona geogr\u00e1fica. Datos enero\u2013diciembre ", paramets$anho, ".")

### España ----
arg_esp_pais           <- arg_mad_pais
arg_esp_pais$tabla     <- df_paises %>%
  filter(orden != 71L) %>%
  mutate(
    pais = if_else(orden == 72L, "TOTAL", pais),
    niv = if_else(orden == 72L, 0L, niv)
  )
arg_esp_pais$cols_exp        <- c("exp_esp", "exp_esp_pct", "exp_esp_tva", "exp_esp_contrib")
arg_esp_pais$cols_imp        <- c("imp_esp", "imp_esp_pct", "imp_esp_tva", "imp_esp_contrib")
arg_esp_pais$cols_extra      <- c("saldo_esp", "tasa_cob_esp")
arg_esp_pais$col_contrib_bar <- c("exp_esp_contrib", "imp_esp_contrib")
arg_esp_pais$cols_millones   <- c("exp_esp", "imp_esp", "saldo_esp")
arg_esp_pais$cols_pct        <- c("exp_esp_pct", "exp_esp_tva", "imp_esp_pct", "imp_esp_tva", "tasa_cob_esp")
arg_esp_pais$cols_contrib    <- c("exp_esp_contrib", "imp_esp_contrib")
arg_esp_pais$header_cols     <- c(
  exp_esp     = "Mill. \u20ac", exp_esp_pct     = "% s/total",
  exp_esp_tva = "TVA (%)",      exp_esp_contrib = "Con. (p.p.)",
  imp_esp     = "Mill. \u20ac", imp_esp_pct     = "% s/total",
  imp_esp_tva = "TVA (%)",      imp_esp_contrib = "Con. (p.p.)",
  saldo_esp   = "Saldo (M\u20ac)", tasa_cob_esp = "T. cob. (%)"
)
arg_esp_pais$titulo    <- paste0("Comercio exterior de Espa\u00f1a por pa\u00edses \u2014 ", paramets$anho)
arg_esp_pais$subtitulo <- paste0("Volumen (Mill.\u20ac), estructura porcentual, variaci\u00f3n anual y contribuci\u00f3n al crecimiento por pa\u00eds y zona geogr\u00e1fica. Datos enero\u2013diciembre ", paramets$anho, ".")

# Argumentos comunes evol ----
.arg_evol_base <- list(
  ano_base             = paramets$anho_idx,
  ano_final            = paramets$anho,
  anos_mostrar         = paramets$anho_idx:paramets$anho,
  cols_millones_factor = paramets$varfactor,
  dec_num              = paramets$dec_num,
  dec_pct              = paramets$dec_per,
  caption              = paramets$caption,
  fuente               = paramets$fuente_texto,
  tam_fuente           = paramets$gt_tam_fuente,
  ancho_cm             = paramets$gt_ancho_evol,
  dpi                  = paramets$dpi,
  col_pal              = paramets$gt_col_pal
)

# Argumentos comunes evol_pct ----
.arg_evol_pct_base <- list(
  anos_mostrar = paramets$anho_idx:paramets$anho,
  dec_pct      = paramets$dec_per,
  caption      = paramets$caption,
  fuente       = paramets$fuente_texto,
  tam_fuente   = paramets$gt_tam_fuente,
  ancho_cm     = paramets$gt_ancho_evol,
  dpi          = paramets$dpi,
  col_pal      = paramets$gt_col_pal
)

# Imágenes tabla GT ----

## Sectores ----
### Madrid ----
tbl_sec_mad <- do.call(exportar_sectores_imagen, c(arg_mad_sec, list(
  ruta_salida = file.path(paramets$path_outt, sprintf("tabla_sectores_mad_%s.png", sufijo_m)),
  ancho_cm    = paramets$gt_ancho_tbl_mad,
  alto_cm     = paramets$gt_alto_tbl,
  dpi         = paramets$dpi
)))

### España ----
tbl_sec_esp <- do.call(exportar_sectores_imagen, c(arg_esp_sec, list(
  ruta_salida = file.path(paramets$path_outt, sprintf("tabla_sectores_esp_%s.png", sufijo_m)),
  ancho_cm    = paramets$gt_ancho_tbl,
  alto_cm     = paramets$gt_alto_tbl,
  dpi         = paramets$dpi
)))

## Países ----
### Madrid ----
tbl_pais_mad <- do.call(exportar_paises_imagen, c(arg_mad_pais, list(
  ruta_salida = file.path(paramets$path_outt, sprintf("tabla_paises_mad_%s.png",   sufijo_m)),
  ancho_cm    = paramets$gt_ancho_tbl,
  alto_cm     = paramets$gt_alto_tbl,
  dpi         = paramets$dpi
)))

### España ----
tbl_pais_esp <- do.call(exportar_paises_imagen, c(arg_esp_pais, list(
  ruta_salida = file.path(paramets$path_outt, sprintf("tabla_paises_esp_%s.png",   sufijo_m)),
  ancho_cm    = paramets$gt_ancho_tbl,
  alto_cm     = paramets$gt_alto_tbl,
  dpi         = paramets$dpi
)))

# Evol ----
df_evol_aux_sec <- df_evol_sec %>%
  filter(!orden %in% c(66L, paramets$filsec)) %>%
  mutate(
    nombre = if_else(orden == 65L, "TOTAL", nombre),
    niv    = if_else(orden == 65L, 0L, niv)
  )

df_evol_aux_pais <- df_evol_pais %>%
  filter(!orden %in% c(72L, paramets$filpais)) %>%
  mutate(
    pais = if_else(orden == 71L, "TOTAL", pais),
    niv  = if_else(orden == 71L, 0L, niv)
  )

## Sectores ----
### Madrid ----
tbl_evol_sec_mad_exp <- do.call(exportar_evol_imagen, c(.arg_evol_base, list(
  tabla       = df_evol_aux_sec,
  flujo       = "exp", territorio = "mad", tipo = "sectores",
  titulo      = paste0("Exportaciones de Madrid por sectores \u2014 Evoluci\u00f3n ", paramets$anho_idx, "\u2013", paramets$anho),
  subtitulo   = NULL,
  ruta_salida = file.path(paramets$path_outt, sprintf("evol_exp_mad_sec_%s.png", sufijo_evol))
)))

tbl_evol_sec_mad_imp <- do.call(exportar_evol_imagen, c(.arg_evol_base, list(
  tabla       = df_evol_aux_sec,
  flujo       = "imp", territorio = "mad", tipo = "sectores",
  titulo      = paste0("Importaciones de Madrid por sectores \u2014 Evoluci\u00f3n ", paramets$anho_idx, "\u2013", paramets$anho),
  subtitulo   = NULL,
  ruta_salida = file.path(paramets$path_outt, sprintf("evol_imp_mad_sec_%s.png", sufijo_evol))
)))

### España ----
tbl_evol_sec_esp_exp <- do.call(exportar_evol_imagen, c(.arg_evol_base, list(
  tabla       = df_evol_aux_sec,
  flujo       = "exp", territorio = "esp", tipo = "sectores",
  titulo      = paste0("Exportaciones de Espa\u00f1a por sectores \u2014 Evoluci\u00f3n ", paramets$anho_idx, "\u2013", paramets$anho),
  subtitulo   = NULL,
  ruta_salida = file.path(paramets$path_outt, sprintf("evol_exp_esp_sec_%s.png", sufijo_evol))
)))

tbl_evol_sec_esp_imp <- do.call(exportar_evol_imagen, c(.arg_evol_base, list(
  tabla       = df_evol_aux_sec,
  flujo       = "imp", territorio = "esp", tipo = "sectores",
  titulo      = paste0("Importaciones de Espa\u00f1a por sectores \u2014 Evoluci\u00f3n ", paramets$anho_idx, "\u2013", paramets$anho),
  subtitulo   = NULL,
  ruta_salida = file.path(paramets$path_outt, sprintf("evol_imp_esp_sec_%s.png", sufijo_evol))
)))

## Países ----
### Madrid ----
tbl_evol_pais_mad_exp <- do.call(exportar_evol_imagen, c(.arg_evol_base, list(
  tabla       = df_evol_aux_pais,
  flujo       = "exp", territorio = "mad", tipo = "paises",
  titulo      = paste0("Exportaciones de Madrid por pa\u00edses \u2014 Evoluci\u00f3n ", paramets$anho_idx, "\u2013", paramets$anho),
  subtitulo   = NULL,
  ruta_salida = file.path(paramets$path_outt, sprintf("evol_exp_mad_pais_%s.png", sufijo_evol))
)))

tbl_evol_pais_mad_imp <- do.call(exportar_evol_imagen, c(.arg_evol_base, list(
  tabla       = df_evol_aux_pais,
  flujo       = "imp", territorio = "mad", tipo = "paises",
  titulo      = paste0("Importaciones de Madrid por pa\u00edses \u2014 Evoluci\u00f3n ", paramets$anho_idx, "\u2013", paramets$anho),
  subtitulo   = NULL,
  ruta_salida = file.path(paramets$path_outt, sprintf("evol_imp_mad_pais_%s.png", sufijo_evol))
)))

### España ----
tbl_evol_pais_esp_exp <- do.call(exportar_evol_imagen, c(.arg_evol_base, list(
  tabla       = df_evol_aux_pais,
  flujo       = "exp", territorio = "esp", tipo = "paises",
  titulo      = paste0("Exportaciones de Espa\u00f1a por pa\u00edses \u2014 Evoluci\u00f3n ", paramets$anho_idx, "\u2013", paramets$anho),
  subtitulo   = NULL,
  ruta_salida = file.path(paramets$path_outt, sprintf("evol_exp_esp_pais_%s.png", sufijo_evol))
)))

tbl_evol_pais_esp_imp <- do.call(exportar_evol_imagen, c(.arg_evol_base, list(
  tabla       = df_evol_aux_pais,
  flujo       = "imp", territorio = "esp", tipo = "paises",
  titulo      = paste0("Importaciones de Espa\u00f1a por pa\u00edses \u2014 Evoluci\u00f3n ", paramets$anho_idx, "\u2013", paramets$anho),
  subtitulo   = NULL,
  ruta_salida = file.path(paramets$path_outt, sprintf("evol_imp_esp_pais_%s.png", sufijo_evol))
)))

rm(.arg_evol_base)

# Evol Pct ----
# Estructura: % s/total Madrid | sparkline | % s/total España | sparkline
# Solo Madrid. Flujos: exp + imp. Tipos: sectores + países.

## Sectores ----
### Exportaciones ----
tbl_evol_pct_sec_exp <- do.call(exportar_evol_pct_imagen, c(.arg_evol_pct_base, list(
  tabla       = df_evol_aux_sec,
  flujo       = "exp", tipo = "sectores",
  titulo      = paste0("Exportaciones de Madrid por sectores \u2014 Estructura porcentual ", paramets$anho_idx, "\u2013", paramets$anho),
  subtitulo   = NULL,
  ruta_salida = file.path(paramets$path_outt, sprintf("evol_pct_exp_sec_%s.png", sufijo_evol))
)))

### Importaciones ----
tbl_evol_pct_sec_imp <- do.call(exportar_evol_pct_imagen, c(.arg_evol_pct_base, list(
  tabla       = df_evol_aux_sec,
  flujo       = "imp", tipo = "sectores",
  titulo      = paste0("Importaciones de Madrid por sectores \u2014 Estructura porcentual ", paramets$anho_idx, "\u2013", paramets$anho),
  subtitulo   = NULL,
  ruta_salida = file.path(paramets$path_outt, sprintf("evol_pct_imp_sec_%s.png", sufijo_evol))
)))

## Países ----
### Exportaciones ----
tbl_evol_pct_pais_exp <- do.call(exportar_evol_pct_imagen, c(.arg_evol_pct_base, list(
  tabla       = df_evol_aux_pais,
  flujo       = "exp", tipo = "paises",
  titulo      = paste0("Exportaciones de Madrid por pa\u00edses \u2014 Estructura porcentual ", paramets$anho_idx, "\u2013", paramets$anho),
  subtitulo   = NULL,
  ruta_salida = file.path(paramets$path_outt, sprintf("evol_pct_exp_pais_%s.png", sufijo_evol))
)))

### Importaciones ----
tbl_evol_pct_pais_imp <- do.call(exportar_evol_pct_imagen, c(.arg_evol_pct_base, list(
  tabla       = df_evol_aux_pais,
  flujo       = "imp", tipo = "paises",
  titulo      = paste0("Importaciones de Madrid por pa\u00edses \u2014 Estructura porcentual ", paramets$anho_idx, "\u2013", paramets$anho),
  subtitulo   = NULL,
  ruta_salida = file.path(paramets$path_outt, sprintf("evol_pct_imp_pais_%s.png", sufijo_evol))
)))

rm(.arg_evol_pct_base)

# Exceles GT ----

## Datacomex sectores y países ----
do.call(exportar_sectores_excel, c(
  .filtrar_args(arg_mad_sec,  .solo_imagen),
  list(ruta_salida = file.path(paramets$path_outx, sprintf("tablagt_sec_mad_%s.xlsx",  sufijo_m)))
))

do.call(exportar_sectores_excel, c(
  .filtrar_args(arg_esp_sec,  .solo_imagen),
  list(ruta_salida = file.path(paramets$path_outx, sprintf("tablagt_sec_esp_%s.xlsx",  sufijo_m)))
))

do.call(exportar_paises_excel, c(
  .filtrar_args(arg_mad_pais, .solo_imagen),
  list(ruta_salida = file.path(paramets$path_outx, sprintf("tablagt_pais_mad_%s.xlsx", sufijo_m)))
))

do.call(exportar_paises_excel, c(
  .filtrar_args(arg_esp_pais, .solo_imagen),
  list(ruta_salida = file.path(paramets$path_outx, sprintf("tablagt_pais_esp_%s.xlsx", sufijo_m)))
))

rm(arg_mad_sec, arg_esp_sec, arg_mad_pais, arg_esp_pais, .solo_imagen, .filtrar_args, sufijo_m)

## Evolución sectores ----
exportar_evol_excel(
  tabla       = df_evol_aux_sec,
  flujo       = "exp", territorio = "mad", tipo = "sectores",
  titulo      = paste0("Exportaciones de Madrid por sectores \u2014 Evoluci\u00f3n ", paramets$anho_idx, "\u2013", paramets$anho),
  ano_base    = paramets$anho_idx, ano_final = paramets$anho, anos_mostrar = paramets$anho_idx:paramets$anho,
  cols_millones_factor = paramets$varfactor,
  dec_num = paramets$dec_num, dec_pct = paramets$dec_per,
  caption = paramets$caption, tam_fuente = paramets$gt_tam_fuente, col_pal = paramets$gt_col_pal,
  ruta_salida = file.path(paramets$path_outx, sprintf("tablagt_evol_exp_mad_sec_%s.xlsx",  sufijo_evol))
)

exportar_evol_excel(
  tabla       = df_evol_aux_sec,
  flujo       = "imp", territorio = "mad", tipo = "sectores",
  titulo      = paste0("Importaciones de Madrid por sectores \u2014 Evoluci\u00f3n ", paramets$anho_idx, "\u2013", paramets$anho),
  ano_base    = paramets$anho_idx, ano_final = paramets$anho, anos_mostrar = paramets$anho_idx:paramets$anho,
  cols_millones_factor = paramets$varfactor,
  dec_num = paramets$dec_num, dec_pct = paramets$dec_per,
  caption = paramets$caption, tam_fuente = paramets$gt_tam_fuente, col_pal = paramets$gt_col_pal,
  ruta_salida = file.path(paramets$path_outx, sprintf("tablagt_evol_imp_mad_sec_%s.xlsx",  sufijo_evol))
)

exportar_evol_excel(
  tabla       = df_evol_aux_sec,
  flujo       = "exp", territorio = "esp", tipo = "sectores",
  titulo      = paste0("Exportaciones de Espa\u00f1a por sectores \u2014 Evoluci\u00f3n ", paramets$anho_idx, "\u2013", paramets$anho),
  ano_base    = paramets$anho_idx, ano_final = paramets$anho, anos_mostrar = paramets$anho_idx:paramets$anho,
  cols_millones_factor = paramets$varfactor,
  dec_num = paramets$dec_num, dec_pct = paramets$dec_per,
  caption = paramets$caption, tam_fuente = paramets$gt_tam_fuente, col_pal = paramets$gt_col_pal,
  ruta_salida = file.path(paramets$path_outx, sprintf("tablagt_evol_exp_esp_sec_%s.xlsx",  sufijo_evol))
)

exportar_evol_excel(
  tabla       = df_evol_aux_sec,
  flujo       = "imp", territorio = "esp", tipo = "sectores",
  titulo      = paste0("Importaciones de Espa\u00f1a por sectores \u2014 Evoluci\u00f3n ", paramets$anho_idx, "\u2013", paramets$anho),
  ano_base    = paramets$anho_idx, ano_final = paramets$anho, anos_mostrar = paramets$anho_idx:paramets$anho,
  cols_millones_factor = paramets$varfactor,
  dec_num = paramets$dec_num, dec_pct = paramets$dec_per,
  caption = paramets$caption, tam_fuente = paramets$gt_tam_fuente, col_pal = paramets$gt_col_pal,
  ruta_salida = file.path(paramets$path_outx, sprintf("tablagt_evol_imp_esp_sec_%s.xlsx",  sufijo_evol))
)

## Evolución países ----
exportar_evol_excel(
  tabla       = df_evol_aux_pais,
  flujo       = "exp", territorio = "mad", tipo = "paises",
  titulo      = paste0("Exportaciones de Madrid por pa\u00edses \u2014 Evoluci\u00f3n ", paramets$anho_idx, "\u2013", paramets$anho),
  ano_base    = paramets$anho_idx, ano_final = paramets$anho, anos_mostrar = paramets$anho_idx:paramets$anho,
  cols_millones_factor = paramets$varfactor,
  dec_num = paramets$dec_num, dec_pct = paramets$dec_per,
  caption = paramets$caption, tam_fuente = paramets$gt_tam_fuente, col_pal = paramets$gt_col_pal,
  ruta_salida = file.path(paramets$path_outx, sprintf("tablagt_evol_exp_mad_pais_%s.xlsx", sufijo_evol))
)

exportar_evol_excel(
  tabla       = df_evol_aux_pais,
  flujo       = "imp", territorio = "mad", tipo = "paises",
  titulo      = paste0("Importaciones de Madrid por pa\u00edses \u2014 Evoluci\u00f3n ", paramets$anho_idx, "\u2013", paramets$anho),
  ano_base    = paramets$anho_idx, ano_final = paramets$anho, anos_mostrar = paramets$anho_idx:paramets$anho,
  cols_millones_factor = paramets$varfactor,
  dec_num = paramets$dec_num, dec_pct = paramets$dec_per,
  caption = paramets$caption, tam_fuente = paramets$gt_tam_fuente, col_pal = paramets$gt_col_pal,
  ruta_salida = file.path(paramets$path_outx, sprintf("tablagt_evol_imp_mad_pais_%s.xlsx", sufijo_evol))
)

exportar_evol_excel(
  tabla       = df_evol_aux_pais,
  flujo       = "exp", territorio = "esp", tipo = "paises",
  titulo      = paste0("Exportaciones de Espa\u00f1a por pa\u00edses \u2014 Evoluci\u00f3n ", paramets$anho_idx, "\u2013", paramets$anho),
  ano_base    = paramets$anho_idx, ano_final = paramets$anho, anos_mostrar = paramets$anho_idx:paramets$anho,
  cols_millones_factor = paramets$varfactor,
  dec_num = paramets$dec_num, dec_pct = paramets$dec_per,
  caption = paramets$caption, tam_fuente = paramets$gt_tam_fuente, col_pal = paramets$gt_col_pal,
  ruta_salida = file.path(paramets$path_outx, sprintf("tablagt_evol_exp_esp_pais_%s.xlsx", sufijo_evol))
)

exportar_evol_excel(
  tabla       = df_evol_aux_pais,
  flujo       = "imp", territorio = "esp", tipo = "paises",
  titulo      = paste0("Importaciones de Espa\u00f1a por pa\u00edses \u2014 Evoluci\u00f3n ", paramets$anho_idx, "\u2013", paramets$anho),
  ano_base    = paramets$anho_idx, ano_final = paramets$anho, anos_mostrar = paramets$anho_idx:paramets$anho,
  cols_millones_factor = paramets$varfactor,
  dec_num = paramets$dec_num, dec_pct = paramets$dec_per,
  caption = paramets$caption, tam_fuente = paramets$gt_tam_fuente, col_pal = paramets$gt_col_pal,
  ruta_salida = file.path(paramets$path_outx, sprintf("tablagt_evol_imp_esp_pais_%s.xlsx", sufijo_evol))
)

## Evolución porcentual sectores ----
exportar_evol_pct_excel(
  tabla       = df_evol_aux_sec,
  flujo       = "exp", tipo = "sectores",
  titulo      = paste0("Exportaciones de Madrid por sectores \u2014 Estructura porcentual ", paramets$anho_idx, "\u2013", paramets$anho),
  anos_mostrar = paramets$anho_idx:paramets$anho,
  dec_pct = paramets$dec_per, caption = paramets$caption,
  tam_fuente = paramets$gt_tam_fuente, col_pal = paramets$gt_col_pal,
  ruta_salida = file.path(paramets$path_outx, sprintf("tablagt_evol_pct_exp_sec_%s.xlsx",  sufijo_evol))
)

exportar_evol_pct_excel(
  tabla       = df_evol_aux_sec,
  flujo       = "imp", tipo = "sectores",
  titulo      = paste0("Importaciones de Madrid por sectores \u2014 Estructura porcentual ", paramets$anho_idx, "\u2013", paramets$anho),
  anos_mostrar = paramets$anho_idx:paramets$anho,
  dec_pct = paramets$dec_per, caption = paramets$caption,
  tam_fuente = paramets$gt_tam_fuente, col_pal = paramets$gt_col_pal,
  ruta_salida = file.path(paramets$path_outx, sprintf("tablagt_evol_pct_imp_sec_%s.xlsx",  sufijo_evol))
)

## Evolución porcentual países ----
exportar_evol_pct_excel(
  tabla       = df_evol_aux_pais,
  flujo       = "exp", tipo = "paises",
  titulo      = paste0("Exportaciones de Madrid por pa\u00edses \u2014 Estructura porcentual ", paramets$anho_idx, "\u2013", paramets$anho),
  anos_mostrar = paramets$anho_idx:paramets$anho,
  dec_pct = paramets$dec_per, caption = paramets$caption,
  tam_fuente = paramets$gt_tam_fuente, col_pal = paramets$gt_col_pal,
  ruta_salida = file.path(paramets$path_outx, sprintf("tablagt_evol_pct_exp_pais_%s.xlsx", sufijo_evol))
)

exportar_evol_pct_excel(
  tabla       = df_evol_aux_pais,
  flujo       = "imp", tipo = "paises",
  titulo      = paste0("Importaciones de Madrid por pa\u00edses \u2014 Estructura porcentual ", paramets$anho_idx, "\u2013", paramets$anho),
  anos_mostrar = paramets$anho_idx:paramets$anho,
  dec_pct = paramets$dec_per, caption = paramets$caption,
  tam_fuente = paramets$gt_tam_fuente, col_pal = paramets$gt_col_pal,
  ruta_salida = file.path(paramets$path_outx, sprintf("tablagt_evol_pct_imp_pais_%s.xlsx", sufijo_evol))
)

rm(.solo_img_evol, sufijo_evol)

# Limpieza de memoria ----
.limpiar_memoria()