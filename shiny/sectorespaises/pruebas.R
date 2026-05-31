#### Marco de trabajo ----
setwd(this.path::this.dir())
source("./procfunshinysectores.R")
options(scipen = 99)

#### Metadatos ----
##### Sectores y agrupaciones ----
df_sectores <- data.table::rbindlist(
  list(
    data.table::as.data.table(openxlsx::read.xlsx("../../data/metatratado/sectores.xlsx", sheet = "sectores")),
    data.table::as.data.table(openxlsx::read.xlsx("../../data/metatratado/sectores.xlsx", sheet = "agregaciones"))
  ),
  use.names = TRUE,
  fill = TRUE
)

##### Paises y regiones ----
df_pais <- data.table::rbindlist(
  list(
    data.table::as.data.table(openxlsx::read.xlsx("../../data/metatratado/paises_zonas.xlsx", sheet = "paises")),
    data.table::as.data.table(openxlsx::read.xlsx("../../data/metatratado/paises_zonas.xlsx", sheet = "regiones"))
  ),
  use.names = TRUE,
  fill = TRUE
)

#### Parámetros ----
fil_region <- "mad"
fil_ano <- 2025L
fil_per <- 57L
fil_pais <- 0L
fil_sectores <- "0"

#### Variables auxiliares ----
parametros <- crear_listas_parametros(
  region = fil_region,
  ano = fil_ano,
  per = fil_per,
  fpais = fil_pais,
  fsec = fil_sectores,
  mapeo_pais = df_pais,
  mapeo_sectores = df_sectores)

rm(fil_region, fil_ano, fil_per, fil_pais, fil_sectores)

#### Carga datos ----
dataset <- arrow::open_dataset(parametros$archivo)

#### Datasets ----
##### Totales ----
totales <- calculo_totales(
  df_query = dataset, 
  param = parametros)

##### Sectores ----
df_tabla_sectores <- tabla_sectores_datacomex(
  datas = dataset,
  tot = totales,
  df_sec = df_sectores, 
  parametros = parametros)

##### Paises ----
df_tabla_paises <- tabla_paises_datacomex(
  datas = dataset,
  tot = totales,
  df_paises = df_pais, 
  parametros = parametros)

##### Treemap sectores ----
df_treemap_sectores <- treemap_data(
  datas = dataset,
  tot = totales,
  df_sec = df_sectores, 
  parametros = parametros)

#### Tablas ----
##### Sectores ----
tabla_salida_sectores_datacomex_desp <- render_datatable_datacomexsec_desplegable(
  df = df_tabla_sectores[, .(orden, niv, nombre, exp, exp_per_reg, tva_exp, con_exp,
                    imp, imp_per_reg, tva_imp, con_imp, saldo, saldo_prev)],
  param = parametros,
  cols_semaforo = c("tva_exp", "tva_imp", "saldo", "saldo_prev"),
  cols_barras_cien = c("exp_per_reg", "imp_per_reg"),
  cols_barras_con = c("con_exp", "con_imp"),
  cols_enteros = c("orden")
)

##### Países ----
tabla_salida_paises_datacomex_desp <- render_datatable_datacomexpaises_desplegable(
  df = df_tabla_paises[, .(orden, niv, pais, exp, exp_per_reg, tva_exp, con_exp,
                             imp, imp_per_reg, tva_imp, con_imp, saldo, saldo_prev)],
  param = parametros,
  cols_semaforo = c("tva_exp", "tva_imp", "saldo", "saldo_prev"),
  cols_barras_cien = c("exp_per_reg", "imp_per_reg"),
  cols_barras_con = c("con_exp", "con_imp"),
  cols_enteros = c("orden")
)

#### Visualizaciones ----
##### Treemap sectores ----
plot_treemap_sectores_alt_exp <- grafica_treemap_informe(
  dt =df_tabla_sectores,
  tipo = "sectores",
  flujo = "exp", 
  para = parametros)

plot_treemap_sectores_alt_imp <- grafica_treemap_informe(
  dt =df_tabla_sectores,
  tipo = "sectores",
  flujo = "imp", 
  para = parametros)

##### Treemap paises ----
plot_treemap_paises_alt_exp <- grafica_treemap_informe(
  dt =df_tabla_paises,
  tipo = "paises",
  flujo = "exp", 
  para = parametros)

plot_treemap_paises_alt_imp <- grafica_treemap_informe(
  dt =df_tabla_paises,
  tipo = "paises",
  flujo = "imp", 
  para = parametros)

##### Sectores temporales----
# Ejemplo de llamada
listaplotsecttemporales <- graficas_temporales_sectores(
  datas = dataset,
  tot = totales,
  para = parametros
)

plot_sec_animado <- listaplotsecttemporales$animado
plot_sec_facetado <- listaplotsecttemporales$spaghetti

lista_temporal <- graficas_evolucion_secpais(
  datas = dataset,
  para = parametros
)

plot_temporal_anual <- lista_temporal$fig_anual
plot_temporal_mensual <- lista_temporal$fig_mensual

plot_vol_subsectores_exp <- grafica_volumen_subsectores(
  df = df_tabla_sectores, 
  para = parametros, 
  nmax = 8, 
  flujo = "exp")

plot_vol_subsectores_imp <- grafica_volumen_subsectores(
  df = df_tabla_sectores, 
  para = parametros, 
  nmax = 8, 
  flujo = "imp")

plot_vol_paises_exp <- grafica_volumen_paises(
  df = df_tabla_paises, 
  para = parametros, 
  nmax = 8, 
  flujo = "exp")

plot_vol_paises_imp <- grafica_volumen_paises(
  df = df_tabla_paises, 
  para = parametros, 
  nmax = 8, 
  flujo = "imp")

plot_con_subsectores_exp <- grafica_contribuciones_subsectores(
  df = df_tabla_sectores,
  para = parametros,
  nmax = 4,
  flujo = "exp"
)

plot_con_subsectores_imp <- grafica_contribuciones_subsectores(
  df = df_tabla_sectores,
  para = parametros,
  nmax = 4,
  flujo = "imp"
)

plot_con_paises_exp <- grafica_contribuciones_paises(
  df = df_tabla_paises,
  para = parametros,
  nmax = 4,
  flujo = "exp"
)

plot_con_paises_imp <- grafica_contribuciones_paises(
  df = df_tabla_paises,
  para = parametros,
  nmax = 4,
  flujo = "imp"
)

#### Prints ----
print(tabla_salida_sectores_datacomex_desp)
print(tabla_salida_paises_datacomex_desp)
print(plot_treemap_sectores_alt_exp)
print(plot_treemap_sectores_alt_imp)
print(plot_treemap_paises_alt_exp)
print(plot_treemap_paises_alt_imp)
# print(plot_sec_animado)
# print(plot_sec_facetado)
# print(plot_temporal_anual)
# print(plot_temporal_mensual)
print(plot_vol_subsectores_exp)
print(plot_vol_subsectores_imp)
print(plot_vol_paises_exp)
print(plot_vol_paises_imp)
print(plot_con_subsectores_exp)
print(plot_con_subsectores_imp)
print(plot_con_paises_exp)
print(plot_con_paises_imp)

