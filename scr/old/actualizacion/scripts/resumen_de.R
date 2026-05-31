# Establecer ruta de trabajo
source("./scripts/procesamiento_resumen_de.R")

# Carga de librerias e instalación si es necesario
instala_carga_librerias(c("data.table", "dplyr", "DT", "ggplot2", "htmltools", "knitr", "lubridate", "plotly", "stringr",
                              "tibble", "tidyr", "vroom"))
source("./scripts/theme.R")
paleta_de <- c(
  colde1 = "#2d5532",  # verde oscuro
  colde2 = "#6f6f4e",  # gris verdoso
  colde3 = "#b4d7b4",  # verde claro
  colde4 = "#ddd9c3",  # beige
  colde5 = "#a6a6a6",  # gris
  colde6 = "#d9d9d9",  # gris claro
  colde7 = "#a1a17a"   # otro tono de verde
)

# Lectura de datos
base_path <- "../datos/ccaacappais"
data_list <- load_all_data(base_path)

ccaa_amp <- data_list$ccaa_amp
taric_amp <- data_list$taric_amp
paises_amp <- data_list$paises_amp
ccaa_trim_amp <- data_list$ccaa_trim_amp
taric_trim_amp <- data_list$taric_trim_amp
paises_trim_amp <- data_list$paises_trim_amp
ccaa_anos_amp <- data_list$ccaa_anos_amp
taric_anos_amp <- data_list$taric_anos_amp
paises_anos_amp <- data_list$paises_anos_amp
rm(data_list)

# Parámetros
# fecha <- '2025-05-01'
# fecha_ini <- '2015-01-01'
# fecha_ini_alt <- '1995-01-01'
# fecha_ini_alt2 <- '2018-01-01'
# ano_ini <- 1995
# reg <- "Madrid, Comunidad de"
# valores_con <- 5
# valores_con_bis <- 20

# Variables auxiliares
ano_filtro <- as.integer(substr(fecha, 1, 4))
mes_filtro <- as.integer(substr(fecha, 6, 7))
# 
tabla_resultado <- crear_tabla_de(ccaa_amp, ano_filtro, mes_filtro)

# Plots
plot_trade_mad_ts <- plot_lines_exp_imp(
  ccaa_amp, 
  reg, 
  fecha_ini_alt, 
  fecha, 
  'mes', 
  'datoper', 
  "la Comunidad de Madrid")

plot_mad_evo_mes <- grafica_flujos_ccaa(
  df=ccaa_amp, 
  flujo_fil=c("EXPORT", "IMPORT"), 
  var_fil="mes",
  temp_fil="datoper", 
  ccaa_fil=reg, 
  ano_fil=ano_ini, 
  mes_fil=mes_filtro)

plot_mad_evo_mes_tva <- grafica_flujos_ccaa_con_tva(
  df=ccaa_amp, 
  flujo_fil=c("EXPORT", "IMPORT"), 
  var_fil="mes",
  temp_fil="datoper", 
  ccaa_fil=reg, 
  ano_fil=ano_ini, 
  mes_fil=mes_filtro)

plot_mad_exp_anos <- grafica_anos(
  dataframe=ccaa_amp, 
  ccaa_fil= reg, 
  flujo_fil= "EXPORT",
  temp_fil=c("datoper", "acumulado"), 
  var_fil= "mes", 
  mes_filtro=mes_filtro, 
  ano_filtro=ano_ini)

plot_mad_imp_anos <- grafica_anos(
  dataframe=ccaa_amp, 
  ccaa_fil= reg, 
  flujo_fil= "IMPORT",
  temp_fil=c("datoper", "acumulado"), 
  var_fil= "mes", 
  mes_filtro=mes_filtro, 
  ano_filtro=ano_ini)

plot_mad_exp_mm12 = grafica_mm(
  df=ccaa_amp, 
  flujo_fil='EXPORT', 
  temp_fil='MM12', 
  ccaa_fil=reg, 
  fecha_ini=fecha_ini, 
  fecha_fin=fecha)

plot_mad_imp_mm12 = grafica_mm(
  df=ccaa_amp, 
  flujo_fil='IMPORT', 
  temp_fil='MM12', 
  ccaa_fil=reg, 
  fecha_ini=fecha_ini, 
  fecha_fin=fecha)

plot_mad_exp_toptaric <- grafico_barras_detalle(
  taric_amp, 
  fecha, fecha, 
  'taric', 
  flujo = "EXPORT", 
  top_n = valores_con_bis)

plot_mad_exp_toppais <- grafico_barras_detalle(
  paises_amp, 
  fecha, fecha, 
  'pais', 
  flujo = "EXPORT", 
  top_n = valores_con_bis)

plot_mad_imp_toptaric <- grafico_barras_detalle(
  taric_amp, 
  fecha, fecha, 
  'taric', 
  flujo = "IMPORT", 
  top_n = valores_con_bis)

plot_mad_imp_toppais <- grafico_barras_detalle(
  paises_amp, 
  fecha, fecha, 
  'pais', 
  flujo = "IMPORT", 
  top_n = valores_con_bis)

plot_mad_exp_con_mes_taric = grafico_contribuciones(
  taric_amp,
  fecha,
  fecha,
  'taric',
  flujo_fil = "EXPORT",
  temporal="datoper",
  top_n=valores_con)
print(plot_mad_exp_con_mes_taric)

plot_mad_exp_con_mes_pais = grafico_contribuciones(
  paises_amp,
  fecha,
  fecha,
  'pais',
  flujo_fil = "EXPORT",
  temporal="datoper",
  top_n=valores_con)

plot_mad_imp_con_mes_taric = grafico_contribuciones(
  taric_amp,
  fecha,
  fecha,
  'taric',
  flujo_fil = "IMPORT",
  temporal="datoper",
  top_n=valores_con)

plot_mad_imp_con_mes_pais = grafico_contribuciones(
  paises_amp,
  fecha,
  fecha,
  'pais',
  flujo_fil = "IMPORT",
  temporal="datoper",
  top_n=valores_con)

plot_mad_exp_con_acu_taric = grafico_contribuciones(
  taric_amp,
  fecha,
  fecha,
  'taric',
  flujo_fil = "EXPORT",
  temporal="acumulado",
  top_n=valores_con)

plot_mad_exp_con_acu_pais = grafico_contribuciones(
  paises_amp,
  fecha,
  fecha,
  'pais',
  flujo_fil = "EXPORT",
  temporal="acumulado",
  top_n=valores_con)

plot_mad_imp_con_acu_taric = grafico_contribuciones(
  taric_amp,
  fecha,
  fecha,
  'taric',
  flujo_fil = "IMPORT",
  temporal="acumulado",
  top_n=valores_con)

plot_mad_imp_con_acu_pais = grafico_contribuciones(
  paises_amp,
  fecha,
  fecha,
  'pais',
  flujo_fil = "IMPORT",
  temporal="acumulado",
  top_n=valores_con)

# print(plot_trade_mad_ts)
# print(plot_mad_evo_mes)
# print(plot_mad_evo_mes_tva)
# print(plot_mad_exp_anos)
# print(plot_mad_imp_anos)
# print(plot_mad_exp_mm12)
# print(plot_mad_imp_mm12)
# print(plot_mad_exp_toptaric)
# print(plot_mad_exp_toppais)
# print(plot_mad_imp_toptaric)
# print(plot_mad_imp_toppais)
# print(plot_mad_exp_con_mes_taric)
# print(plot_mad_exp_con_mes_pais)
# print(plot_mad_imp_con_mes_taric)
# print(plot_mad_imp_con_mes_pais)
# print(plot_mad_exp_con_acu_taric)
# print(plot_mad_exp_con_acu_pais)
# print(plot_mad_imp_con_acu_taric)
# print(plot_mad_imp_con_acu_pais)