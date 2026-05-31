# main_plots.R
# Generación de gráficos de comercio exterior Madrid vs España

# Entorno ----
source("./scr/R/recuadro_anual_sectores/main_etl.R")
# source("./scr/R/recuadro_anual_sectores/auxiliar/funciones_plot.R")


# Nombres de archivo ----
m_start  <- min(params$meses)
m_end    <- max(params$meses)

sufijo_p <- if (m_start == m_end) {
  sprintf("%d_%02d",      params$anho, m_start)
} else {
  # sprintf("%d_%02d-%02d", params$anho, m_start, m_end)
}

# Dimensiones comunes ----
w_tree  <- params$plot_width_treemap  * params$px_per_cm

h_tree  <- params$plot_height_treemap * params$px_per_cm

w_vol   <- params$plot_width          * params$px_per_cm

h       <- params$plot_height         * params$px_per_cm

sc      <- params$dpi / params$plotly_dpi_ref

# Treemaps ----

## Madrid ----
### Exportaciones ----
#### Sectores ----
treemap_exp_mad_sec <- grafica_treemap_plotly(
  dt         = df_sectores,
  flujo      = "exp",
  territorio = "mad",
  tipo       = "sectores",
  parametros = params,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.00)
)
plotly::save_image(
  treemap_exp_mad_sec,
  file  = file.path(params$path_outp, sprintf("treemap_exp_mad_sec_%s.png",   sufijo_p)),
  width = w_tree, height = h_tree, scale = sc
)

#### Países ----
treemap_exp_mad_pais <- grafica_treemap_plotly(
  dt         = df_paises,
  flujo      = "exp",
  territorio = "mad",
  tipo       = "paises",
  parametros = params,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
plotly::save_image(
  treemap_exp_mad_pais,
  file  = file.path(params$path_outp, sprintf("treemap_exp_mad_pais_%s.png",  sufijo_p)),
  width = w_tree, height = h_tree, scale = sc
)

### Importaciones ----
#### Sectores ----
treemap_imp_mad_sec <- grafica_treemap_plotly(
  dt         = df_sectores,
  flujo      = "imp",
  territorio = "mad",
  tipo       = "sectores",
  parametros = params,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
plotly::save_image(
  treemap_imp_mad_sec,
  file  = file.path(params$path_outp, sprintf("treemap_imp_mad_sec_%s.png",   sufijo_p)),
  width = w_tree, height = h_tree, scale = sc
)

#### Países ----
treemap_imp_mad_pais <- grafica_treemap_plotly(
  dt         = df_paises,
  flujo      = "imp",
  territorio = "mad",
  tipo       = "paises",
  parametros = params,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
plotly::save_image(
  treemap_imp_mad_pais,
  file  = file.path(params$path_outp, sprintf("treemap_imp_mad_pais_%s.png",  sufijo_p)),
  width = w_tree, height = h_tree, scale = sc
)

## España ----
### Exportaciones ----
#### Sectores ----
treemap_exp_esp_sec <- grafica_treemap_plotly(
  dt         = df_sectores,
  flujo      = "exp",
  territorio = "esp",
  tipo       = "sectores",
  parametros = params,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
plotly::save_image(
  treemap_exp_esp_sec,
  file  = file.path(params$path_outp, sprintf("treemap_exp_esp_sec_%s.png",   sufijo_p)),
  width = w_tree, height = h_tree, scale = sc
)

#### Países ----
treemap_exp_esp_pais <- grafica_treemap_plotly(
  dt         = df_paises,
  flujo      = "exp",
  territorio = "esp",
  tipo       = "paises",
  parametros = params,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
plotly::save_image(
  treemap_exp_esp_pais,
  file  = file.path(params$path_outp, sprintf("treemap_exp_esp_pais_%s.png",  sufijo_p)),
  width = w_tree, height = h_tree, scale = sc
)

### Importaciones ----
#### Sectores ----
treemap_imp_esp_sec <- grafica_treemap_plotly(
  dt         = df_sectores,
  flujo      = "imp",
  territorio = "esp",
  tipo       = "sectores",
  parametros = params,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
plotly::save_image(
  treemap_imp_esp_sec,
  file  = file.path(params$path_outp, sprintf("treemap_imp_esp_sec_%s.png",   sufijo_p)),
  width = w_tree, height = h_tree, scale = sc
)

#### Países ----
treemap_imp_esp_pais <- grafica_treemap_plotly(
  dt         = df_paises,
  flujo      = "imp",
  territorio = "esp",
  tipo       = "paises",
  parametros = params,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
plotly::save_image(
  treemap_imp_esp_pais,
  file  = file.path(params$path_outp, sprintf("treemap_imp_esp_pais_%s.png",  sufijo_p)),
  width = w_tree, height = h_tree, scale = sc
)

# Plots Volumen y Contribuciones ----

## Madrid ----
### Exportaciones ----
#### Sectores ----
vol_exp_mad_sec <- grafica_volumen_sectores_com(
  dt = df_sec, flujo = "exp", region = "mad", parametros = params
)
plotly::save_image(
  vol_exp_mad_sec,
  file  = file.path(params$path_outp, sprintf("vol_exp_mad_sec_%s.png",       sufijo_p)),
  width = w_vol, height = h, scale = sc
)

contrib_exp_mad_sec <- grafica_contribuciones_sectores_com(
  dt = df_sec, flujo = "exp", region = "mad", parametros = params
)
plotly::save_image(
  contrib_exp_mad_sec,
  file  = file.path(params$path_outp, sprintf("contrib_exp_mad_sec_%s.png",   sufijo_p)),
  width = w_vol, height = h, scale = sc
)

#### Países ----
vol_exp_mad_pais <- grafica_volumen_paises_com(
  dt = df_country, flujo = "exp", region = "mad", parametros = params
)
plotly::save_image(
  vol_exp_mad_pais,
  file  = file.path(params$path_outp, sprintf("vol_exp_mad_pais_%s.png",      sufijo_p)),
  width = w_vol, height = h, scale = sc
)

contrib_exp_mad_pais <- grafica_contribuciones_paises_com(
  dt = df_country, flujo = "exp", region = "mad", parametros = params
)
plotly::save_image(
  contrib_exp_mad_pais,
  file  = file.path(params$path_outp, sprintf("contrib_exp_mad_pais_%s.png",  sufijo_p)),
  width = w_vol, height = h, scale = sc
)

### Importaciones ----
#### Sectores ----
vol_imp_mad_sec <- grafica_volumen_sectores_com(
  dt = df_sec, flujo = "imp", region = "mad", parametros = params
)
plotly::save_image(
  vol_imp_mad_sec,
  file  = file.path(params$path_outp, sprintf("vol_imp_mad_sec_%s.png",       sufijo_p)),
  width = w_vol, height = h, scale = sc
)

contrib_imp_mad_sec <- grafica_contribuciones_sectores_com(
  dt = df_sec, flujo = "imp", region = "mad", parametros = params
)
plotly::save_image(
  contrib_imp_mad_sec,
  file  = file.path(params$path_outp, sprintf("contrib_imp_mad_sec_%s.png",   sufijo_p)),
  width = w_vol, height = h, scale = sc
)

#### Países ----
vol_imp_mad_pais <- grafica_volumen_paises_com(
  dt = df_country, flujo = "imp", region = "mad", parametros = params
)
plotly::save_image(
  vol_imp_mad_pais,
  file  = file.path(params$path_outp, sprintf("vol_imp_mad_pais_%s.png",      sufijo_p)),
  width = w_vol, height = h, scale = sc
)

contrib_imp_mad_pais <- grafica_contribuciones_paises_com(
  dt = df_country, flujo = "imp", region = "mad", parametros = params
)
plotly::save_image(
  contrib_imp_mad_pais,
  file  = file.path(params$path_outp, sprintf("contrib_imp_mad_pais_%s.png",  sufijo_p)),
  width = w_vol, height = h, scale = sc
)

## España ----
### Exportaciones ----
#### Sectores ----
vol_exp_esp_sec <- grafica_volumen_sectores_com(
  dt = df_sec, flujo = "exp", region = "esp", parametros = params
)
plotly::save_image(
  vol_exp_esp_sec,
  file  = file.path(params$path_outp, sprintf("vol_exp_esp_sec_%s.png",       sufijo_p)),
  width = w_vol, height = h, scale = sc
)

contrib_exp_esp_sec <- grafica_contribuciones_sectores_com(
  dt = df_sec, flujo = "exp", region = "esp", parametros = params
)
plotly::save_image(
  contrib_exp_esp_sec,
  file  = file.path(params$path_outp, sprintf("contrib_exp_esp_sec_%s.png",   sufijo_p)),
  width = w_vol, height = h, scale = sc
)

#### Países ----
vol_exp_esp_pais <- grafica_volumen_paises_com(
  dt = df_country, flujo = "exp", region = "esp", parametros = params
)
plotly::save_image(
  vol_exp_esp_pais,
  file  = file.path(params$path_outp, sprintf("vol_exp_esp_pais_%s.png",      sufijo_p)),
  width = w_vol, height = h, scale = sc
)

contrib_exp_esp_pais <- grafica_contribuciones_paises_com(
  dt = df_country, flujo = "exp", region = "esp", parametros = params
)
plotly::save_image(
  contrib_exp_esp_pais,
  file  = file.path(params$path_outp, sprintf("contrib_exp_esp_pais_%s.png",  sufijo_p)),
  width = w_vol, height = h, scale = sc
)

### Importaciones ----
#### Sectores ----
vol_imp_esp_sec <- grafica_volumen_sectores_com(
  dt = df_sec, flujo = "imp", region = "esp", parametros = params
)
plotly::save_image(
  vol_imp_esp_sec,
  file  = file.path(params$path_outp, sprintf("vol_imp_esp_sec_%s.png",       sufijo_p)),
  width = w_vol, height = h, scale = sc
)

contrib_imp_esp_sec <- grafica_contribuciones_sectores_com(
  dt = df_sec, flujo = "imp", region = "esp", parametros = params
)
plotly::save_image(
  contrib_imp_esp_sec,
  file  = file.path(params$path_outp, sprintf("contrib_imp_esp_sec_%s.png",   sufijo_p)),
  width = w_vol, height = h, scale = sc
)

#### Países ----
vol_imp_esp_pais <- grafica_volumen_paises_com(
  dt = df_country, flujo = "imp", region = "esp", parametros = params
)
plotly::save_image(
  vol_imp_esp_pais,
  file  = file.path(params$path_outp, sprintf("vol_imp_esp_pais_%s.png",      sufijo_p)),
  width = w_vol, height = h, scale = sc
)

contrib_imp_esp_pais <- grafica_contribuciones_paises_com(
  dt = df_country, flujo = "imp", region = "esp", parametros = params
)
plotly::save_image(
  contrib_imp_esp_pais,
  file  = file.path(params$path_outp, sprintf("contrib_imp_esp_pais_%s.png",  sufijo_p)),
  width = w_vol, height = h, scale = sc
)

# Bump charts ----

## Madrid ----
### Exportaciones ----
#### Países ----
bump_exp_mad_paises <- grafica_bump_chart(
  dt         = df_evol_countryfull[cod != 0],
  flujo      = "exp",
  region     = "mad",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = params
)
plotly::save_image(
  bump_exp_mad_paises,
  file  = file.path(params$path_outp, sprintf("bump_exp_mad_paises_%s.png", sufijo_p)),
  width = w_vol, height = h, scale = sc
)

#### Sectores ----
bump_exp_mad_sec <- grafica_bump_chart(
  dt         = df_evol_secfull[niv >= 2],
  flujo      = "exp",
  region     = "mad",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = params
)
plotly::save_image(
  bump_exp_mad_sec,
  file  = file.path(params$path_outp, sprintf("bump_exp_mad_sec_%s.png",    sufijo_p)),
  width = w_vol, height = h, scale = sc
)

### Importaciones ----
#### Países ----
bump_imp_mad_paises <- grafica_bump_chart(
  dt         = df_evol_countryfull[cod != 0],
  flujo      = "imp",
  region     = "mad",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = params
)
plotly::save_image(
  bump_imp_mad_paises,
  file  = file.path(params$path_outp, sprintf("bump_imp_mad_paises_%s.png", sufijo_p)),
  width = w_vol, height = h, scale = sc
)

#### Sectores ----
bump_imp_mad_sec <- grafica_bump_chart(
  dt         = df_evol_secfull[niv >= 2],
  flujo      = "imp",
  region     = "mad",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = params
)
plotly::save_image(
  bump_imp_mad_sec,
  file  = file.path(params$path_outp, sprintf("bump_imp_mad_sec_%s.png",    sufijo_p)),
  width = w_vol, height = h, scale = sc
)

## España ----
### Exportaciones ----
#### Países ----
bump_exp_esp_paises <- grafica_bump_chart(
  dt         = df_evol_countryfull[cod != 0],
  flujo      = "exp",
  region     = "esp",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = params
)
plotly::save_image(
  bump_exp_esp_paises,
  file  = file.path(params$path_outp, sprintf("bump_exp_esp_paises_%s.png", sufijo_p)),
  width = w_vol, height = h, scale = sc
)

#### Sectores ----
bump_exp_esp_sec <- grafica_bump_chart(
  dt         = df_evol_secfull[niv >= 2],
  flujo      = "exp",
  region     = "esp",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = params
)
plotly::save_image(
  bump_exp_esp_sec,
  file  = file.path(params$path_outp, sprintf("bump_exp_esp_sec_%s.png",    sufijo_p)),
  width = w_vol, height = h, scale = sc
)

### Importaciones ----
#### Países ----
bump_imp_esp_paises <- grafica_bump_chart(
  dt         = df_evol_countryfull[cod != 0],
  flujo      = "imp",
  region     = "esp",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = params
)
plotly::save_image(
  bump_imp_esp_paises,
  file  = file.path(params$path_outp, sprintf("bump_imp_esp_paises_%s.png", sufijo_p)),
  width = w_vol, height = h, scale = sc
)

#### Sectores ----
bump_imp_esp_sec <- grafica_bump_chart(
  dt         = df_evol_secfull[niv >= 2],
  flujo      = "imp",
  region     = "esp",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = params
)
plotly::save_image(
  bump_imp_esp_sec,
  file  = file.path(params$path_outp, sprintf("bump_imp_esp_sec_%s.png",    sufijo_p)),
  width = w_vol, height = h, scale = sc
)

# Limpieza de memoria ----
limpiar_memoria()