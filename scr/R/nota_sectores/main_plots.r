# main_plots.R
# Generación de gráficos de comercio exterior Madrid vs España

# Entorno ----
source("./scr/R/nota_sectores/procfun/funciones_plot.r")

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