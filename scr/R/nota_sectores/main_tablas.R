# main_tabla_anexos.R
# Genera tablas combinadas (datos + minigráficas de tendencia)
#   · Sectores — Madrid
#   · Países   — Madrid

# Entorno ----
source("./scr/R/nota_sectores/procfun/funciones_tabla_anexos.R")
source("./scr/R/nota_sectores/procfun/funciones_flextable.r")
source("./scr/R/nota_sectores/procfun/funciones_plot.r")

# Tabla CCAA ----
df_tp1 <- .prepare_flextable_ccaas(df_ccaas)[c1 != "ND",
                                             .(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)]

ft_ccaa <- .make_flextable_ccaa(
  df   = df_tp1,
  para = paramets
)

save_as_image(ft_ccaa, path = file.path(paramets$path_outp , "table_p1_t1.png"), zoom = 3)
wb <- wb_workbook()$add_worksheet("Resultados CCAA")
wb <- wb_add_flextable(wb, sheet = "Resultados CCAA", ft = ft_ccaa, dims = "A1")
wb_save(wb, file = file.path(paramets$path_outx, "table_p1_t1.xlsx"))

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
    paramets$path_outp,
    sprintf("tabla_sec_spark_mad_%s.png", sufijo_mes)
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
    paramets$path_outp,
    sprintf("tabla_pais_spark_mad_%s.png", sufijo_mes)
  )
)

# Plots ----
## Página 1 ----
plot_mad_evo_mes <- .grafica_flujos_ccaa(
  df        = df_ccaa_amp, 
  flujo_fil = c("EXPORT", "IMPORT"), 
  var_fil   = c("mes"),
  temp_fil  = c("datoper"), 
  ccaa_fil  = paramets$reg1, 
  ano_fil   = paramets$ano_ini, 
  mes_fil   = paramets$mes,
  colde1    = paramets$colpal1,
  colde3    = paramets$colpal2)

## Página 2 (mm12 y años) -----
plot_mad_exp_mm12 <- .grafica_mm(
  df        = df_ccaa_amp, 
  flujo_fil = c("EXPORT"), 
  temp_fil  = c("MM12"), 
  ccaa_fil  = paramets$reg1, 
  fecha_ini = paramets$fecha_ini, 
  fecha_fin = paramets$fecha,
  colde1    = paramets$colpal1,
  colde3    = paramets$colpal2)

plot_mad_imp_mm12 <- .grafica_mm(
  df = df_ccaa_amp, 
  flujo_fil = c("IMPORT"), 
  temp_fil  = c("MM12"), 
  ccaa_fil  = paramets$reg1, 
  fecha_ini = paramets$fecha_ini, 
  fecha_fin = paramets$fecha,
  colde1    = paramets$colpal1,
  colde3    = paramets$colpal2)

plot_mad_exp_anos <-  .grafica_anos(
  dataframe  = df_ccaa_amp, 
  ccaa_fil   = c("Madrid, Comunidad de"), 
  flujo_fil  = c("EXPORT"),
  temp_fil   = c("datoper", "acumulado"), 
  var_fil    = c("mes"), 
  mes_filtro = paramets$mes, 
  ano_filtro = paramets$ano_ini,
  colde1     = paramets$colpal1,
  colde3     = paramets$colpal2,
  colde5     = paramets$colpal3)

plot_mad_imp_anos <-  .grafica_anos(
  dataframe   = df_ccaa_amp, 
  ccaa_fil    = c("Madrid, Comunidad de"), 
  flujo_fil   = c("IMPORT"),
  temp_fil    = c("datoper", "acumulado"), 
  var_fil     = c("mes"), 
  mes_filtro  = paramets$mes, 
  ano_filtro  = paramets$ano_ini,
  colde1      = paramets$colpal1,
  colde3      = paramets$colpal2,
  colde5      = paramets$colpal3)

plot_mad_mm12_anos <- (
  (plot_mad_exp_anos + theme(plot.margin = unit(c(0, paramets$mh/2, paramets$mv/2, 0), "cm"))) + 
    (plot_mad_exp_mm12 + theme(plot.margin = unit(c(0, 0, paramets$mv/2, paramets$mh/2), "cm"))) +
    (plot_mad_imp_anos + theme(plot.margin = unit(c(paramets$mv/2, paramets$mh/2, 0, 0), "cm"))) + 
    (plot_mad_imp_mm12 + theme(plot.margin = unit(c(paramets$mv/2, 0, 0, paramets$mh/2), "cm")))
) + 
  patchwork::plot_layout(ncol = 2, nrow = 2)

# Salvado plots ----
ggsave(
  file.path(paramets$path_outp, "plot1_mad_evo_mes.png"), 
  plot = plot_mad_evo_mes,
  width = paramets$w1, 
  height = paramets$h1, 
  units = "cm", 
  dpi = paramets$dpi
)

ggsave(
  file.path(paramets$path_outp, "plot211_mad_exp_anos.png"), 
  plot = plot_mad_exp_anos,
  width = paramets$w2, 
  height = paramets$h2, 
  units = "cm", 
  dpi = paramets$dpi
)

ggsave(
  file.path(paramets$path_outp, "plot212_mad_exp_mm12.png"), 
  plot = plot_mad_exp_mm12,
  width = paramets$w2, 
  height = paramets$h2, 
  units = "cm", 
  dpi = paramets$dpi
)

ggsave(
  file.path(paramets$path_outp, "plot221_mad_exp_anos.png"), 
  plot = plot_mad_imp_anos,
  width = paramets$w2, 
  height = paramets$h2, 
  units = "cm", 
  dpi = paramets$dpi
)

ggsave(
  file.path(paramets$path_outp, "plot222_mad_imp_mm12.png"), 
  plot = plot_mad_imp_mm12,
  width = paramets$w2, 
  height = paramets$h2, 
  units = "cm", 
  dpi = paramets$dpi
)

ggsave(
  file.path(paramets$path_outp, "plot2_mad_mm12_anos.png"),
  plot = plot_mad_mm12_anos,
  width = paramets$w2 * 2 + paramets$mh,
  height = paramets$h2 * 2 + paramets$mv,
  units = "cm",
  dpi = paramets$dpi
)


# Limpieza de memoria ----
.limpiar_memoria()