#### Establecimiento marco de trabajo de trabajo ----
library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(scales)
library(openxlsx)

source("./scr/R/actualizacion/auxiliar/procplot.R")
source("./scr/R/actualizacion/auxiliar/temaggplot.R")

#### Parametros ----
fecha <- as.Date("2026-03-01")
fecha_ini <- as.Date("2018-01-01")
fecha_ini_alt <- as.Date("2019-01-01")
fecha_ini_alt2 <- as.Date("1995-01-01")
fecha_ini_alt3 <- as.Date("1996-01-01")
ano_ini <- 2017
mes_filtro <- as.integer(format(fecha, "%m"))
reg1 <- "Madrid, Comunidad de"
reg2 <- "España"

##### Rutas -----
data_path <- "./data/output/ccaacappais/"
plot_path <- "./data/output/plots/"

##### Tamaños -----
w1 <- 7;          h1 <- 4.5
w2 <- 9;          h2 <- 5.5
mv <- 0.3;        mh <- 0.3   
w3 <- 10.8;       h3 <- 5.4   
w4 <- 10.8;       h4 <- 5.4
w5 <- 9;          h5 <- 4.5   
w6 <- 9;          h6 <- 4.5  
ppp <- 300

#### Cargar datos ----
data_list <- load_all_data(data_path)
data_list <- lapply(data_list, setDT)
list2env(data_list, .GlobalEnv)
# verify_data(data_list)
rm(data_list)

#### Var auxiliares ----
output_year <- format(fecha, "%Y")
output_month <- format(fecha, "%m")
output_dir <- file.path(plot_path, paste0("plots_", output_year, "_", output_month))

# Crear el directorio si no existe
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

#### Plots ----
##### Página 1 (evolución) -----
plot_mad_evo_mes <-grafica_flujos_ccaa(
  df = ccaa_amp, 
  flujo_fil = c("EXPORT", "IMPORT"), 
  var_fil = c("mes"),
  temp_fil = c("datoper"), 
  ccaa_fil = reg1, 
  ano_fil = ano_ini, 
  mes_fil = mes_filtro)

##### Página 2 (mm12 y años) -----
plot_mad_exp_mm12 <- grafica_mm(
  df = ccaa_amp, 
  flujo_fil = c("EXPORT"), 
  temp_fil = c("MM12"), 
  ccaa_fil = reg1, 
  fecha_ini = fecha_ini_alt, 
  fecha_fin = fecha)

plot_mad_imp_mm12 <- grafica_mm(
  df = ccaa_amp, 
  flujo_fil = c("IMPORT"), 
  temp_fil = c("MM12"), 
  ccaa_fil = reg1, 
  fecha_ini = fecha_ini_alt, 
  fecha_fin = fecha)

plot_mad_exp_anos <-  grafica_anos(
  dataframe = ccaa_amp, 
  ccaa_fil = c("Madrid, Comunidad de"), 
  flujo_fil = c("EXPORT"),
  temp_fil = c("datoper", "acumulado"), 
  var_fil = c("mes"), 
  mes_filtro = mes_filtro, 
  ano_filtro = ano_ini)

plot_mad_imp_anos <-  grafica_anos(
  dataframe = ccaa_amp, 
  ccaa_fil = c("Madrid, Comunidad de"), 
  flujo_fil = c("IMPORT"),
  temp_fil = c("datoper", "acumulado"), 
  var_fil = c("mes"), 
  mes_filtro = mes_filtro, 
  ano_filtro = ano_ini)

###### Cuadricula 2x2 ------
plot_mad_mm12_anos <- (
  (plot_mad_exp_anos + theme(plot.margin = unit(c(0, mh/2, mv/2, 0), "cm"))) + 
    (plot_mad_exp_mm12 + theme(plot.margin = unit(c(0, 0, mv/2, mh/2), "cm"))) +
    (plot_mad_imp_anos + theme(plot.margin = unit(c(mv/2, mh/2, 0, 0), "cm"))) + 
    (plot_mad_imp_mm12 + theme(plot.margin = unit(c(mv/2, 0, 0, mh/2), "cm")))
) + 
  patchwork::plot_layout(ncol = 2, nrow = 2)


##### Página 3-6 (Contribuciones) -----
plot_con_taric_exp <- grafica_contribuciones(
  df = taric_amp, 
  fecha_ini = fecha, 
  fecha_fin = fecha,
  flujo_fil = c("EXPORT"), 
  var_fil = c("con_tva"), 
  col = "taric")

plot_con_taric_imp <- grafica_contribuciones(
  df = taric_amp, 
  fecha_ini = fecha, 
  fecha_fin = fecha,
  flujo_fil = c("IMPORT"), 
  var_fil = c("con_tva"), 
  col = "taric")

plot_con_pais_exp <- grafica_contribuciones(
  df = paises_amp, 
  fecha_ini = fecha, 
  fecha_fin = fecha,
  flujo_fil = c("EXPORT"), 
  var_fil = c("con_tva"), 
  col = "pais")

plot_con_pais_imp <- grafica_contribuciones(
  df = paises_amp, 
  fecha_ini = fecha, 
  fecha_fin = fecha,
  flujo_fil = c("IMPORT"), 
  var_fil = c("con_tva"), 
  col = "pais")

#### Guardado de plots ----
ggsave(
  file.path(output_dir, "plot1_mad_evo_mes.png"), 
  plot = plot_mad_evo_mes,
  width = w1, 
  height =h1, 
  units = "cm", 
  dpi = ppp)

ggsave(
  file.path(output_dir, "plot211_mad_exp_anos.png"), 
  plot = plot_mad_exp_anos,
  width = w2, 
  height = h2, 
  units = "cm", 
  dpi = ppp)

ggsave(
  file.path(output_dir, "plot212_mad_exp_mm12.png"), 
  plot = plot_mad_exp_mm12,
  width = w2, 
  height = h2, 
  units = "cm", 
  dpi = ppp)

ggsave(
  file.path(output_dir, "plot221_mad_exp_anos.png"), 
  plot = plot_mad_imp_anos,
  width = w2, 
  height = h2, 
  units = "cm", 
  dpi = ppp)

ggsave(
  file.path(output_dir, "plot222_mad_imp_mm12.png"), 
  plot = plot_mad_imp_mm12,
  width = w2, 
  height = h2, 
  units = "cm", 
  dpi = ppp)

ggsave(
  file.path(output_dir, "plot2_mad_mm12_anos.png"),
  plot = plot_mad_mm12_anos,
  width = w2 * 2 + mh,
  height = h2 * 2 + mv,
  units = "cm",
  dpi = ppp
)

ggsave(
  file.path(output_dir, "plot3_con_taric_exp.png"), 
  plot = plot_con_taric_exp,
  width = w3, 
  height = h3, 
  units = "cm", 
  dpi = ppp)

ggsave(
  file.path(output_dir, "plot4_con_taric_imp.png"), 
  plot = plot_con_taric_imp,
  width = w4, 
  height = h4, 
  units = "cm", 
  dpi = ppp)

ggsave(
  file.path(output_dir, "plot5_con_paises_exp.png"), 
  plot = plot_con_pais_exp,
  width = w5, 
  height = h5, 
  units = "cm", 
  dpi = ppp)

ggsave(
  file.path(output_dir, "plot6_con_paises_imp.png"), 
  plot = plot_con_pais_imp,
  width = w6, 
  height = h6, 
  units = "cm", 
  dpi = ppp)

#### Plots no graficados ----
# plot_taric_exp <- grafica_barras_detalle_bis(taric_amp, fecha_ini = fecha, fecha_fin = fecha,
#                                              flujo_fil = c("EXPORT"), var_fil = c("mes", "peso"), "taric")
# 
# plot_taric_imp <- grafica_barras_detalle_bis(taric_amp, fecha_ini = fecha, fecha_fin = fecha,
#                                              flujo_fil = c("IMPORT"), var_fil = c("mes", "peso"), "taric")
# 
# plot_pais_exp <- grafica_barras_detalle_bis(paises_amp, fecha_ini = fecha, fecha_fin = fecha,
#                                             flujo_fil = c("EXPORT"), var_fil = c("mes", "peso"), "pais")
# 
# plot_pais_imp <- grafica_barras_detalle_bis(paises_amp, fecha_ini = fecha, fecha_fin = fecha,
#                                             flujo_fil = c("IMPORT"), var_fil = c("mes", "peso"), "pais")
# 
# 
# 
# plot_mad_evo_mes_tva <- grafica_flujos_ccaa_con_tva(df = ccaa_amp, flujo_fil = c("EXPORT", "IMPORT"),
#                                                     temp_fil = c("datoper"), ccaa_fil = reg1, ano_fil = ano_ini, mes_fil = mes_filtro
# )
# 
# plot_mad_imp <- grafica_flujo(ccaa_amp, c("IMPORT"), c("datoper"), reg1, fecha_ini_alt2, fecha, mes_filtro)
# plot_mad_imp_mes <- grafica_flujo_mes(ccaa_amp, c("IMPORT"), c("datoper"), reg1, fecha_ini, fecha)
# 
# ggsave(file.path(output_dir, "plot_pais_exp.png"), plot = plot_pais_exp,
#        width = 13, height = 8, units = "cm", dpi = 300)
# 
# ggsave(file.path(output_dir, "plot_pais_imp.png"), plot = plot_pais_imp,
#        width = 13, height = 8, units = "cm", dpi = 300)
# 
# ggsave(file.path(output_dir, "plot_taric_exp.png"), plot = plot_taric_exp,
#        width = 11, height = 6, units = "cm", dpi = 300)
# 
# ggsave(file.path(output_dir, "plot_taric_imp.png"), plot = plot_taric_imp,
#        width = 11, height = 6, units = "cm", dpi = 300)
# 
# ggsave(file.path(output_dir, "plot_mad_evo_mes_tva.png"), plot = plot_mad_evo_mes_tva,
#        width = 7, height = 4.5, units = "cm", dpi = 300)
# 
