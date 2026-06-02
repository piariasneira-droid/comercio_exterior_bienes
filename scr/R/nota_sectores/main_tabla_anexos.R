# main_tabla_anexos.R
# Genera tablas combinadas (datos + minigráficas de tendencia)
#   · Sectores — Madrid
#   · Países   — Madrid

# Entorno ----
source("./scr/R/nota_sectores/procfun/funciones_tabla_anexos.R")

# Sufijo de nombre de archivo ----
# Usa paramets$mes (escalar o vector; definido en parametros.r como mes = 3L)
sufijo_m <- .sufijo_mes(paramets)

# Madrid · Sectores ----
tbl_sec_spark_mad <- .exportar_sec_spark_imagen(
  
  # Datos (mes actual)
  tabla_sec  = df_sectores %>% filter(!orden %in% paramets$sectores_a_excluir),
  tabla_evol = df_evol_sec %>% filter(!orden %in% paramets$sectores_a_excluir),
  
  # Territorio y años del sparkline
  territorio = "mad",
  anos_spark = paramets$anho_idx:paramets$anho,
  
  # Columnas de la tabla base
  cols_exp        = c("exp_mad", "exp_mad_pct", "exp_mad_tva",
                      "exp_mad_contrib", "exp_mad_vs_esp"),
  cols_imp        = c("imp_mad", "imp_mad_pct", "imp_mad_tva",
                      "imp_mad_contrib", "imp_mad_vs_esp"),
  cols_extra      = c(),
  omitir_orden    = NULL,
  label_exp       = "Exportaciones",
  label_imp       = "Importaciones",
  col_contrib_bar = c("exp_mad_contrib", "imp_mad_contrib"),
  cols_millones   = c("exp_mad", "imp_mad", "saldo_mad"),
  cols_pct        = c("exp_mad_pct", "exp_mad_tva", "imp_mad_pct", "imp_mad_tva",
                      "tasa_cob_mad", "exp_mad_vs_esp", "imp_mad_vs_esp"),
  cols_contrib    = c("exp_mad_contrib", "imp_mad_contrib"),
  header_cols     = c(
    exp_mad        = "Mill. \u20ac", exp_mad_pct     = "%",
    exp_mad_tva    = "TVA",          exp_mad_contrib = "Con.",
    exp_mad_vs_esp = "% s/E",
    imp_mad        = "Mill. \u20ac", imp_mad_pct     = "%",
    imp_mad_tva    = "TVA",          imp_mad_contrib = "Con.",
    imp_mad_vs_esp = "% s/E"
  ),
  
  # Textos
  # .per_label() está definida en funciones_text.r
  titulo    = paste0(
    "ANEXO I \u2014 Comercio exterior de la C. Madrid por sectores y subsectores. ",
    tools::toTitleCase(tolower(.build_period_labels(paramets)$mes_label))
  ),
  subtitulo = paste0(
    "Volumen (Mill.\u20ac), estructura porcentual, variaci\u00f3n anual,",
    " contribuci\u00f3n al crecimiento y evoluci\u00f3n \u00edndice desde ",
    paramets$anho_idx, "."
  ),
  caption   = paramets$caption,
  
  # Tipografía y dimensiones
  ancho_cm   = paramets$gt_ancho_tbl_mad,
  ancho_px   = 2126L,
  alto_px    = 3071L,
  tam_fuente = paramets$gt_tam_fuente,
  fuente     = paramets$fuente_texto,
  dec_num    = paramets$dec_num,
  dec_pct    = paramets$dec_per,
  dpi        = paramets$dpi,
  col_pal    = paramets$gt_col_pal,
  
  # Ruta de salida
  ruta_salida = file.path(
    paramets$path_outt,
    sprintf("tabla_sec_spark_mad_%s.png", sufijo_m)
  )
)

# Madrid · Países ----
tbl_pais_spark_mad <- .exportar_pais_spark_imagen(
  
  # Datos (mes actual)
  # orden 71 → fila TOTAL (niv 0); los paises_a_excluir vienen de parametros.r
  tabla_sec = df_paises %>%
    filter(!orden %in% paramets$paises_a_excluir) %>%
    mutate(
      pais = if_else(orden == 71L, "TOTAL", pais),
      niv  = if_else(orden == 71L, 0L,      niv)
    ),
  
  # cod_pais == 0 → sin filtro por país concreto (todos los países)
  # Si cod_pais > 0, se filtra a ese país específico en otros contextos (etl, textos)
  tabla_evol = df_evol_pais %>%
    filter(!orden %in% paramets$paises_a_excluir) %>%
    mutate(
      pais = if_else(orden == 71L, "TOTAL", pais),
      niv  = if_else(orden == 71L, 0L,      niv)
    ),
  
  # Territorio y años del sparkline
  territorio = "mad",
  anos_spark = paramets$anho_idx:paramets$anho,
  
  # Columnas de la tabla base
  cols_exp        = c("exp_mad", "exp_mad_pct", "exp_mad_tva",
                      "exp_mad_contrib", "exp_mad_vs_esp"),
  cols_imp        = c("imp_mad", "imp_mad_pct", "imp_mad_tva",
                      "imp_mad_contrib", "imp_mad_vs_esp"),
  cols_extra      = c("saldo_mad", "tasa_cob_mad"),
  omitir_orden    = NULL,
  label_exp       = "Exportaciones",
  label_imp       = "Importaciones",
  col_contrib_bar = c("exp_mad_contrib", "imp_mad_contrib"),
  cols_millones   = c("exp_mad", "imp_mad", "saldo_mad"),
  cols_pct        = c("exp_mad_pct", "exp_mad_tva", "imp_mad_pct", "imp_mad_tva",
                      "exp_mad_vs_esp", "imp_mad_vs_esp", "tasa_cob_mad"),
  cols_contrib    = c("exp_mad_contrib", "imp_mad_contrib"),
  header_cols     = c(
    exp_mad        = "Mill. \u20ac", exp_mad_pct     = "%",
    exp_mad_tva    = "TVA",          exp_mad_contrib = "Con.",
    exp_mad_vs_esp = "% s/E",
    imp_mad        = "Mill. \u20ac", imp_mad_pct     = "%",
    imp_mad_tva    = "TVA",          imp_mad_contrib = "Con.",
    imp_mad_vs_esp = "% s/E",
    saldo_mad      = "Saldo (M\u20ac)", tasa_cob_mad = "T. cob. (%)"
  ),
  
  # Textos
  titulo    = paste0("Comercio exterior de Madrid por pa\u00edses \u2014 ",
                     paramets$anho),
  subtitulo = paste0(
    "Volumen (Mill.\u20ac), estructura porcentual, variaci\u00f3n anual,",
    " contribuci\u00f3n al crecimiento y evoluci\u00f3n desde ", paramets$anho_idx,
    ". ", tools::toTitleCase(.per_label(paramets$mes, paramets$anho)), "."
  ),
  caption   = paramets$caption,
  
  # Tipografía y dimensiones
  ancho_cm   = paramets$gt_ancho_tbl_mad,
  ancho_px   = 2126L,
  alto_px    = 3071L,
  tam_fuente = paramets$gt_tam_fuente,
  fuente     = paramets$fuente_texto,
  dec_num    = paramets$dec_num,
  dec_pct    = paramets$dec_per,
  dpi        = paramets$dpi,
  col_pal    = paramets$gt_col_pal,
  
  # Ruta de salida
  ruta_salida = file.path(
    paramets$path_outt,
    sprintf("tabla_pais_spark_mad_%s.png", sufijo_m)
  )
)

rm(sufijo_m)

# Limpieza de memoria ----
.limpiar_memoria()