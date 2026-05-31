# main_phtmls.R
# Generación de gráficos interactivos HTML de comercio exterior Madrid vs España

# Entorno ----
source("./scr/R/nota_sectores/procfun/funciones_phtmls.R")

# Subcarpetas de salida ----
.subdirs_html <- c(
  "madrid_mes", "madrid_ytm", "madrid_anopasado",
  "espana_mes", "espana_ytm", "espana_anopasado"
)
invisible(lapply(.subdirs_html, function(d) {
  p <- file.path(paramets$path_outh, d)
  if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
}))

# Sufijos de archivo ----
sufijo_mes    <- sprintf("%d_%02d",  paramets$anho, paramets$mes)
sufijo_ytm    <- sprintf("%d_ytm%02d", paramets$anho, paramets$mes)
sufijo_anopas <- sprintf("%d_anual", paramets$anho - 1L)

# Plots mes ----

## Treemaps mes ----

### Madrid ----
#### Exportaciones ----
treemap_exp_mad_sec <- .grafica_treemap_plotly(
  dt         = df_sectores,
  flujo      = "exp",
  territorio = "mad",
  tipo       = "sectores",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.00)
)
.guardar_html(treemap_exp_mad_sec,
              file.path(paramets$path_outh, "madrid_mes",
                        sprintf("treemap_exp_mad_sec_%s.html", sufijo_mes)))

treemap_exp_mad_pais <- .grafica_treemap_plotly(
  dt         = df_paises,
  flujo      = "exp",
  territorio = "mad",
  tipo       = "paises",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_exp_mad_pais,
              file.path(paramets$path_outh, "madrid_mes",
                        sprintf("treemap_exp_mad_pais_%s.html", sufijo_mes)))

#### Importaciones ----
treemap_imp_mad_sec <- .grafica_treemap_plotly(
  dt         = df_sectores,
  flujo      = "imp",
  territorio = "mad",
  tipo       = "sectores",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_imp_mad_sec,
              file.path(paramets$path_outh, "madrid_mes",
                        sprintf("treemap_imp_mad_sec_%s.html", sufijo_mes)))

treemap_imp_mad_pais <- .grafica_treemap_plotly(
  dt         = df_paises,
  flujo      = "imp",
  territorio = "mad",
  tipo       = "paises",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_imp_mad_pais,
              file.path(paramets$path_outh, "madrid_mes",
                        sprintf("treemap_imp_mad_pais_%s.html", sufijo_mes)))

### España ----
#### Exportaciones ----
treemap_exp_esp_sec <- .grafica_treemap_plotly(
  dt         = df_sectores,
  flujo      = "exp",
  territorio = "esp",
  tipo       = "sectores",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_exp_esp_sec,
              file.path(paramets$path_outh, "espana_mes",
                        sprintf("treemap_exp_esp_sec_%s.html", sufijo_mes)))

treemap_exp_esp_pais <- .grafica_treemap_plotly(
  dt         = df_paises,
  flujo      = "exp",
  territorio = "esp",
  tipo       = "paises",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_exp_esp_pais,
              file.path(paramets$path_outh, "espana_mes",
                        sprintf("treemap_exp_esp_pais_%s.html", sufijo_mes)))

#### Importaciones ----
treemap_imp_esp_sec <- .grafica_treemap_plotly(
  dt         = df_sectores,
  flujo      = "imp",
  territorio = "esp",
  tipo       = "sectores",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_imp_esp_sec,
              file.path(paramets$path_outh, "espana_mes",
                        sprintf("treemap_imp_esp_sec_%s.html", sufijo_mes)))

treemap_imp_esp_pais <- .grafica_treemap_plotly(
  dt         = df_paises,
  flujo      = "imp",
  territorio = "esp",
  tipo       = "paises",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_imp_esp_pais,
              file.path(paramets$path_outh, "espana_mes",
                        sprintf("treemap_imp_esp_pais_%s.html", sufijo_mes)))

## Volumen y Contribuciones mes ----

### Madrid ----
#### Exportaciones ----
vol_exp_mad_sec <- .grafica_volumen_sectores_com(
  dt = df_sec, flujo = "exp", region = "mad", parametros = paramets
)
.guardar_html(vol_exp_mad_sec,
              file.path(paramets$path_outh, "madrid_mes",
                        sprintf("vol_exp_mad_sec_%s.html", sufijo_mes)))

contrib_exp_mad_sec <- .grafica_contribuciones_sectores_com(
  dt = df_sec, flujo = "exp", region = "mad", parametros = paramets
)
.guardar_html(contrib_exp_mad_sec,
              file.path(paramets$path_outh, "madrid_mes",
                        sprintf("contrib_exp_mad_sec_%s.html", sufijo_mes)))

vol_exp_mad_pais <- .grafica_volumen_paises_com(
  dt = df_country, flujo = "exp", region = "mad", parametros = paramets
)
.guardar_html(vol_exp_mad_pais,
              file.path(paramets$path_outh, "madrid_mes",
                        sprintf("vol_exp_mad_pais_%s.html", sufijo_mes)))

contrib_exp_mad_pais <- .grafica_contribuciones_paises_com(
  dt = df_country, flujo = "exp", region = "mad", parametros = paramets
)
.guardar_html(contrib_exp_mad_pais,
              file.path(paramets$path_outh, "madrid_mes",
                        sprintf("contrib_exp_mad_pais_%s.html", sufijo_mes)))

#### Importaciones ----
vol_imp_mad_sec <- .grafica_volumen_sectores_com(
  dt = df_sec, flujo = "imp", region = "mad", parametros = paramets
)
.guardar_html(vol_imp_mad_sec,
              file.path(paramets$path_outh, "madrid_mes",
                        sprintf("vol_imp_mad_sec_%s.html", sufijo_mes)))

contrib_imp_mad_sec <- .grafica_contribuciones_sectores_com(
  dt = df_sec, flujo = "imp", region = "mad", parametros = paramets
)
.guardar_html(contrib_imp_mad_sec,
              file.path(paramets$path_outh, "madrid_mes",
                        sprintf("contrib_imp_mad_sec_%s.html", sufijo_mes)))

vol_imp_mad_pais <- .grafica_volumen_paises_com(
  dt = df_country, flujo = "imp", region = "mad", parametros = paramets
)
.guardar_html(vol_imp_mad_pais,
              file.path(paramets$path_outh, "madrid_mes",
                        sprintf("vol_imp_mad_pais_%s.html", sufijo_mes)))

contrib_imp_mad_pais <- .grafica_contribuciones_paises_com(
  dt = df_country, flujo = "imp", region = "mad", parametros = paramets
)
.guardar_html(contrib_imp_mad_pais,
              file.path(paramets$path_outh, "madrid_mes",
                        sprintf("contrib_imp_mad_pais_%s.html", sufijo_mes)))

### España ----
#### Exportaciones ----
vol_exp_esp_sec <- .grafica_volumen_sectores_com(
  dt = df_sec, flujo = "exp", region = "esp", parametros = paramets
)
.guardar_html(vol_exp_esp_sec,
              file.path(paramets$path_outh, "espana_mes",
                        sprintf("vol_exp_esp_sec_%s.html", sufijo_mes)))

contrib_exp_esp_sec <- .grafica_contribuciones_sectores_com(
  dt = df_sec, flujo = "exp", region = "esp", parametros = paramets
)
.guardar_html(contrib_exp_esp_sec,
              file.path(paramets$path_outh, "espana_mes",
                        sprintf("contrib_exp_esp_sec_%s.html", sufijo_mes)))

vol_exp_esp_pais <- .grafica_volumen_paises_com(
  dt = df_country, flujo = "exp", region = "esp", parametros = paramets
)
.guardar_html(vol_exp_esp_pais,
              file.path(paramets$path_outh, "espana_mes",
                        sprintf("vol_exp_esp_pais_%s.html", sufijo_mes)))

contrib_exp_esp_pais <- .grafica_contribuciones_paises_com(
  dt = df_country, flujo = "exp", region = "esp", parametros = paramets
)
.guardar_html(contrib_exp_esp_pais,
              file.path(paramets$path_outh, "espana_mes",
                        sprintf("contrib_exp_esp_pais_%s.html", sufijo_mes)))

#### Importaciones ----
vol_imp_esp_sec <- .grafica_volumen_sectores_com(
  dt = df_sec, flujo = "imp", region = "esp", parametros = paramets
)
.guardar_html(vol_imp_esp_sec,
              file.path(paramets$path_outh, "espana_mes",
                        sprintf("vol_imp_esp_sec_%s.html", sufijo_mes)))

contrib_imp_esp_sec <- .grafica_contribuciones_sectores_com(
  dt = df_sec, flujo = "imp", region = "esp", parametros = paramets
)
.guardar_html(contrib_imp_esp_sec,
              file.path(paramets$path_outh, "espana_mes",
                        sprintf("contrib_imp_esp_sec_%s.html", sufijo_mes)))

vol_imp_esp_pais <- .grafica_volumen_paises_com(
  dt = df_country, flujo = "imp", region = "esp", parametros = paramets
)
.guardar_html(vol_imp_esp_pais,
              file.path(paramets$path_outh, "espana_mes",
                        sprintf("vol_imp_esp_pais_%s.html", sufijo_mes)))

contrib_imp_esp_pais <- .grafica_contribuciones_paises_com(
  dt = df_country, flujo = "imp", region = "esp", parametros = paramets
)
.guardar_html(contrib_imp_esp_pais,
              file.path(paramets$path_outh, "espana_mes",
                        sprintf("contrib_imp_esp_pais_%s.html", sufijo_mes)))

## Bump charts mes ----

### Madrid ----
bump_exp_mad_paises <- .grafica_bump_chart(
  dt         = df_evol_countryfull[cod != 0],
  flujo      = "exp",
  region     = "mad",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_exp_mad_paises,
              file.path(paramets$path_outh, "madrid_mes",
                        sprintf("bump_exp_mad_paises_%s.html", sufijo_mes)))

bump_exp_mad_sec <- .grafica_bump_chart(
  dt         = df_evol_secfull[niv >= 2],
  flujo      = "exp",
  region     = "mad",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_exp_mad_sec,
              file.path(paramets$path_outh, "madrid_mes",
                        sprintf("bump_exp_mad_sec_%s.html", sufijo_mes)))

bump_imp_mad_paises <- .grafica_bump_chart(
  dt         = df_evol_countryfull[cod != 0],
  flujo      = "imp",
  region     = "mad",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_imp_mad_paises,
              file.path(paramets$path_outh, "madrid_mes",
                        sprintf("bump_imp_mad_paises_%s.html", sufijo_mes)))

bump_imp_mad_sec <- .grafica_bump_chart(
  dt         = df_evol_secfull[niv >= 2],
  flujo      = "imp",
  region     = "mad",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_imp_mad_sec,
              file.path(paramets$path_outh, "madrid_mes",
                        sprintf("bump_imp_mad_sec_%s.html", sufijo_mes)))

### España ----
bump_exp_esp_paises <- .grafica_bump_chart(
  dt         = df_evol_countryfull[cod != 0],
  flujo      = "exp",
  region     = "esp",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_exp_esp_paises,
              file.path(paramets$path_outh, "espana_mes",
                        sprintf("bump_exp_esp_paises_%s.html", sufijo_mes)))

bump_exp_esp_sec <- .grafica_bump_chart(
  dt         = df_evol_secfull[niv >= 2],
  flujo      = "exp",
  region     = "esp",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_exp_esp_sec,
              file.path(paramets$path_outh, "espana_mes",
                        sprintf("bump_exp_esp_sec_%s.html", sufijo_mes)))

bump_imp_esp_paises <- .grafica_bump_chart(
  dt         = df_evol_countryfull[cod != 0],
  flujo      = "imp",
  region     = "esp",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_imp_esp_paises,
              file.path(paramets$path_outh, "espana_mes",
                        sprintf("bump_imp_esp_paises_%s.html", sufijo_mes)))

bump_imp_esp_sec <- .grafica_bump_chart(
  dt         = df_evol_secfull[niv >= 2],
  flujo      = "imp",
  region     = "esp",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_imp_esp_sec,
              file.path(paramets$path_outh, "espana_mes",
                        sprintf("bump_imp_esp_sec_%s.html", sufijo_mes)))

# Plots acumulado ----

## Treemaps acumulado ----

### Madrid ----
#### Exportaciones ----
treemap_exp_mad_sec_acu <- .grafica_treemap_plotly(
  dt         = df_sectores_acu,
  flujo      = "exp",
  territorio = "mad",
  tipo       = "sectores",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.00)
)
.guardar_html(treemap_exp_mad_sec_acu,
              file.path(paramets$path_outh, "madrid_ytm",
                        sprintf("treemap_exp_mad_sec_%s.html", sufijo_ytm)))

treemap_exp_mad_pais_acu <- .grafica_treemap_plotly(
  dt         = df_paises_acu,
  flujo      = "exp",
  territorio = "mad",
  tipo       = "paises",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_exp_mad_pais_acu,
              file.path(paramets$path_outh, "madrid_ytm",
                        sprintf("treemap_exp_mad_pais_%s.html", sufijo_ytm)))

#### Importaciones ----
treemap_imp_mad_sec_acu <- .grafica_treemap_plotly(
  dt         = df_sectores_acu,
  flujo      = "imp",
  territorio = "mad",
  tipo       = "sectores",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_imp_mad_sec_acu,
              file.path(paramets$path_outh, "madrid_ytm",
                        sprintf("treemap_imp_mad_sec_%s.html", sufijo_ytm)))

treemap_imp_mad_pais_acu <- .grafica_treemap_plotly(
  dt         = df_paises_acu,
  flujo      = "imp",
  territorio = "mad",
  tipo       = "paises",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_imp_mad_pais_acu,
              file.path(paramets$path_outh, "madrid_ytm",
                        sprintf("treemap_imp_mad_pais_%s.html", sufijo_ytm)))

### España ----
#### Exportaciones ----
treemap_exp_esp_sec_acu <- .grafica_treemap_plotly(
  dt         = df_sectores_acu,
  flujo      = "exp",
  territorio = "esp",
  tipo       = "sectores",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_exp_esp_sec_acu,
              file.path(paramets$path_outh, "espana_ytm",
                        sprintf("treemap_exp_esp_sec_%s.html", sufijo_ytm)))

treemap_exp_esp_pais_acu <- .grafica_treemap_plotly(
  dt         = df_paises_acu,
  flujo      = "exp",
  territorio = "esp",
  tipo       = "paises",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_exp_esp_pais_acu,
              file.path(paramets$path_outh, "espana_ytm",
                        sprintf("treemap_exp_esp_pais_%s.html", sufijo_ytm)))

#### Importaciones ----
treemap_imp_esp_sec_acu <- .grafica_treemap_plotly(
  dt         = df_sectores_acu,
  flujo      = "imp",
  territorio = "esp",
  tipo       = "sectores",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_imp_esp_sec_acu,
              file.path(paramets$path_outh, "espana_ytm",
                        sprintf("treemap_imp_esp_sec_%s.html", sufijo_ytm)))

treemap_imp_esp_pais_acu <- .grafica_treemap_plotly(
  dt         = df_paises_acu,
  flujo      = "imp",
  territorio = "esp",
  tipo       = "paises",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_imp_esp_pais_acu,
              file.path(paramets$path_outh, "espana_ytm",
                        sprintf("treemap_imp_esp_pais_%s.html", sufijo_ytm)))

## Volumen y Contribuciones acumulado ----

### Madrid ----
#### Exportaciones ----
vol_exp_mad_sec_acu <- .grafica_volumen_sectores_com(
  dt = df_sec_acu, flujo = "exp", region = "mad", parametros = paramets
)
.guardar_html(vol_exp_mad_sec_acu,
              file.path(paramets$path_outh, "madrid_ytm",
                        sprintf("vol_exp_mad_sec_%s.html", sufijo_ytm)))

contrib_exp_mad_sec_acu <- .grafica_contribuciones_sectores_com(
  dt = df_sec_acu, flujo = "exp", region = "mad", parametros = paramets
)
.guardar_html(contrib_exp_mad_sec_acu,
              file.path(paramets$path_outh, "madrid_ytm",
                        sprintf("contrib_exp_mad_sec_%s.html", sufijo_ytm)))

vol_exp_mad_pais_acu <- .grafica_volumen_paises_com(
  dt = df_country_acu, flujo = "exp", region = "mad", parametros = paramets
)
.guardar_html(vol_exp_mad_pais_acu,
              file.path(paramets$path_outh, "madrid_ytm",
                        sprintf("vol_exp_mad_pais_%s.html", sufijo_ytm)))

contrib_exp_mad_pais_acu <- .grafica_contribuciones_paises_com(
  dt = df_country_acu, flujo = "exp", region = "mad", parametros = paramets
)
.guardar_html(contrib_exp_mad_pais_acu,
              file.path(paramets$path_outh, "madrid_ytm",
                        sprintf("contrib_exp_mad_pais_%s.html", sufijo_ytm)))

#### Importaciones ----
vol_imp_mad_sec_acu <- .grafica_volumen_sectores_com(
  dt = df_sec_acu, flujo = "imp", region = "mad", parametros = paramets
)
.guardar_html(vol_imp_mad_sec_acu,
              file.path(paramets$path_outh, "madrid_ytm",
                        sprintf("vol_imp_mad_sec_%s.html", sufijo_ytm)))

contrib_imp_mad_sec_acu <- .grafica_contribuciones_sectores_com(
  dt = df_sec_acu, flujo = "imp", region = "mad", parametros = paramets
)
.guardar_html(contrib_imp_mad_sec_acu,
              file.path(paramets$path_outh, "madrid_ytm",
                        sprintf("contrib_imp_mad_sec_%s.html", sufijo_ytm)))

vol_imp_mad_pais_acu <- .grafica_volumen_paises_com(
  dt = df_country_acu, flujo = "imp", region = "mad", parametros = paramets
)
.guardar_html(vol_imp_mad_pais_acu,
              file.path(paramets$path_outh, "madrid_ytm",
                        sprintf("vol_imp_mad_pais_%s.html", sufijo_ytm)))

contrib_imp_mad_pais_acu <- .grafica_contribuciones_paises_com(
  dt = df_country_acu, flujo = "imp", region = "mad", parametros = paramets
)
.guardar_html(contrib_imp_mad_pais_acu,
              file.path(paramets$path_outh, "madrid_ytm",
                        sprintf("contrib_imp_mad_pais_%s.html", sufijo_ytm)))

### España ----
#### Exportaciones ----
vol_exp_esp_sec_acu <- .grafica_volumen_sectores_com(
  dt = df_sec_acu, flujo = "exp", region = "esp", parametros = paramets
)
.guardar_html(vol_exp_esp_sec_acu,
              file.path(paramets$path_outh, "espana_ytm",
                        sprintf("vol_exp_esp_sec_%s.html", sufijo_ytm)))

contrib_exp_esp_sec_acu <- .grafica_contribuciones_sectores_com(
  dt = df_sec_acu, flujo = "exp", region = "esp", parametros = paramets
)
.guardar_html(contrib_exp_esp_sec_acu,
              file.path(paramets$path_outh, "espana_ytm",
                        sprintf("contrib_exp_esp_sec_%s.html", sufijo_ytm)))

vol_exp_esp_pais_acu <- .grafica_volumen_paises_com(
  dt = df_country_acu, flujo = "exp", region = "esp", parametros = paramets
)
.guardar_html(vol_exp_esp_pais_acu,
              file.path(paramets$path_outh, "espana_ytm",
                        sprintf("vol_exp_esp_pais_%s.html", sufijo_ytm)))

contrib_exp_esp_pais_acu <- .grafica_contribuciones_paises_com(
  dt = df_country_acu, flujo = "exp", region = "esp", parametros = paramets
)
.guardar_html(contrib_exp_esp_pais_acu,
              file.path(paramets$path_outh, "espana_ytm",
                        sprintf("contrib_exp_esp_pais_%s.html", sufijo_ytm)))

#### Importaciones ----
vol_imp_esp_sec_acu <- .grafica_volumen_sectores_com(
  dt = df_sec_acu, flujo = "imp", region = "esp", parametros = paramets
)
.guardar_html(vol_imp_esp_sec_acu,
              file.path(paramets$path_outh, "espana_ytm",
                        sprintf("vol_imp_esp_sec_%s.html", sufijo_ytm)))

contrib_imp_esp_sec_acu <- .grafica_contribuciones_sectores_com(
  dt = df_sec_acu, flujo = "imp", region = "esp", parametros = paramets
)
.guardar_html(contrib_imp_esp_sec_acu,
              file.path(paramets$path_outh, "espana_ytm",
                        sprintf("contrib_imp_esp_sec_%s.html", sufijo_ytm)))

vol_imp_esp_pais_acu <- .grafica_volumen_paises_com(
  dt = df_country_acu, flujo = "imp", region = "esp", parametros = paramets
)
.guardar_html(vol_imp_esp_pais_acu,
              file.path(paramets$path_outh, "espana_ytm",
                        sprintf("vol_imp_esp_pais_%s.html", sufijo_ytm)))

contrib_imp_esp_pais_acu <- .grafica_contribuciones_paises_com(
  dt = df_country_acu, flujo = "imp", region = "esp", parametros = paramets
)
.guardar_html(contrib_imp_esp_pais_acu,
              file.path(paramets$path_outh, "espana_ytm",
                        sprintf("contrib_imp_esp_pais_%s.html", sufijo_ytm)))

## Bump charts acumulado ----

### Madrid ----
bump_exp_mad_paises_acu <- .grafica_bump_chart(
  dt         = df_evol_countryfull_acu[cod != 0],
  flujo      = "exp",
  region     = "mad",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_exp_mad_paises_acu,
              file.path(paramets$path_outh, "madrid_ytm",
                        sprintf("bump_exp_mad_paises_%s.html", sufijo_ytm)))

bump_exp_mad_sec_acu <- .grafica_bump_chart(
  dt         = df_evol_secfull_acu[niv >= 2],
  flujo      = "exp",
  region     = "mad",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_exp_mad_sec_acu,
              file.path(paramets$path_outh, "madrid_ytm",
                        sprintf("bump_exp_mad_sec_%s.html", sufijo_ytm)))

bump_imp_mad_paises_acu <- .grafica_bump_chart(
  dt         = df_evol_countryfull_acu[cod != 0],
  flujo      = "imp",
  region     = "mad",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_imp_mad_paises_acu,
              file.path(paramets$path_outh, "madrid_ytm",
                        sprintf("bump_imp_mad_paises_%s.html", sufijo_ytm)))

bump_imp_mad_sec_acu <- .grafica_bump_chart(
  dt         = df_evol_secfull_acu[niv >= 2],
  flujo      = "imp",
  region     = "mad",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_imp_mad_sec_acu,
              file.path(paramets$path_outh, "madrid_ytm",
                        sprintf("bump_imp_mad_sec_%s.html", sufijo_ytm)))

### España ----
bump_exp_esp_paises_acu <- .grafica_bump_chart(
  dt         = df_evol_countryfull_acu[cod != 0],
  flujo      = "exp",
  region     = "esp",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_exp_esp_paises_acu,
              file.path(paramets$path_outh, "espana_ytm",
                        sprintf("bump_exp_esp_paises_%s.html", sufijo_ytm)))

bump_exp_esp_sec_acu <- .grafica_bump_chart(
  dt         = df_evol_secfull_acu[niv >= 2],
  flujo      = "exp",
  region     = "esp",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_exp_esp_sec_acu,
              file.path(paramets$path_outh, "espana_ytm",
                        sprintf("bump_exp_esp_sec_%s.html", sufijo_ytm)))

bump_imp_esp_paises_acu <- .grafica_bump_chart(
  dt         = df_evol_countryfull_acu[cod != 0],
  flujo      = "imp",
  region     = "esp",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_imp_esp_paises_acu,
              file.path(paramets$path_outh, "espana_ytm",
                        sprintf("bump_imp_esp_paises_%s.html", sufijo_ytm)))

bump_imp_esp_sec_acu <- .grafica_bump_chart(
  dt         = df_evol_secfull_acu[niv >= 2],
  flujo      = "imp",
  region     = "esp",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_imp_esp_sec_acu,
              file.path(paramets$path_outh, "espana_ytm",
                        sprintf("bump_imp_esp_sec_%s.html", sufijo_ytm)))

# Plots año pasado ----

## Treemaps año pasado ----

### Madrid ----
#### Exportaciones ----
treemap_exp_mad_sec_anopas <- .grafica_treemap_plotly(
  dt         = df_sectores_anopas,
  flujo      = "exp",
  territorio = "mad",
  tipo       = "sectores",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.00)
)
.guardar_html(treemap_exp_mad_sec_anopas,
              file.path(paramets$path_outh, "madrid_anopasado",
                        sprintf("treemap_exp_mad_sec_%s.html", sufijo_anopas)))

treemap_exp_mad_pais_anopas <- .grafica_treemap_plotly(
  dt         = df_paises_anopas,
  flujo      = "exp",
  territorio = "mad",
  tipo       = "paises",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_exp_mad_pais_anopas,
              file.path(paramets$path_outh, "madrid_anopasado",
                        sprintf("treemap_exp_mad_pais_%s.html", sufijo_anopas)))

#### Importaciones ----
treemap_imp_mad_sec_anopas <- .grafica_treemap_plotly(
  dt         = df_sectores_anopas,
  flujo      = "imp",
  territorio = "mad",
  tipo       = "sectores",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_imp_mad_sec_anopas,
              file.path(paramets$path_outh, "madrid_anopasado",
                        sprintf("treemap_imp_mad_sec_%s.html", sufijo_anopas)))

treemap_imp_mad_pais_anopas <- .grafica_treemap_plotly(
  dt         = df_paises_anopas,
  flujo      = "imp",
  territorio = "mad",
  tipo       = "paises",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_imp_mad_pais_anopas,
              file.path(paramets$path_outh, "madrid_anopasado",
                        sprintf("treemap_imp_mad_pais_%s.html", sufijo_anopas)))

### España ----
#### Exportaciones ----
treemap_exp_esp_sec_anopas <- .grafica_treemap_plotly(
  dt         = df_sectores_anopas,
  flujo      = "exp",
  territorio = "esp",
  tipo       = "sectores",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_exp_esp_sec_anopas,
              file.path(paramets$path_outh, "espana_anopasado",
                        sprintf("treemap_exp_esp_sec_%s.html", sufijo_anopas)))

treemap_exp_esp_pais_anopas <- .grafica_treemap_plotly(
  dt         = df_paises_anopas,
  flujo      = "exp",
  territorio = "esp",
  tipo       = "paises",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_exp_esp_pais_anopas,
              file.path(paramets$path_outh, "espana_anopasado",
                        sprintf("treemap_exp_esp_pais_%s.html", sufijo_anopas)))

#### Importaciones ----
treemap_imp_esp_sec_anopas <- .grafica_treemap_plotly(
  dt         = df_sectores_anopas,
  flujo      = "imp",
  territorio = "esp",
  tipo       = "sectores",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_imp_esp_sec_anopas,
              file.path(paramets$path_outh, "espana_anopasado",
                        sprintf("treemap_imp_esp_sec_%s.html", sufijo_anopas)))

treemap_imp_esp_pais_anopas <- .grafica_treemap_plotly(
  dt         = df_paises_anopas,
  flujo      = "imp",
  territorio = "esp",
  tipo       = "paises",
  parametros = paramets,
  posiciones = list(plot_y = c(0.25, 1), cbar_y = 0.05)
)
.guardar_html(treemap_imp_esp_pais_anopas,
              file.path(paramets$path_outh, "espana_anopasado",
                        sprintf("treemap_imp_esp_pais_%s.html", sufijo_anopas)))

## Volumen y Contribuciones año pasado ----

### Madrid ----
#### Exportaciones ----
vol_exp_mad_sec_anopas <- .grafica_volumen_sectores_com(
  dt = df_sec_anopas, flujo = "exp", region = "mad", parametros = paramets
)
.guardar_html(vol_exp_mad_sec_anopas,
              file.path(paramets$path_outh, "madrid_anopasado",
                        sprintf("vol_exp_mad_sec_%s.html", sufijo_anopas)))

contrib_exp_mad_sec_anopas <- .grafica_contribuciones_sectores_com(
  dt = df_sec_anopas, flujo = "exp", region = "mad", parametros = paramets
)
.guardar_html(contrib_exp_mad_sec_anopas,
              file.path(paramets$path_outh, "madrid_anopasado",
                        sprintf("contrib_exp_mad_sec_%s.html", sufijo_anopas)))

vol_exp_mad_pais_anopas <- .grafica_volumen_paises_com(
  dt = df_country_anopas, flujo = "exp", region = "mad", parametros = paramets
)
.guardar_html(vol_exp_mad_pais_anopas,
              file.path(paramets$path_outh, "madrid_anopasado",
                        sprintf("vol_exp_mad_pais_%s.html", sufijo_anopas)))

contrib_exp_mad_pais_anopas <- .grafica_contribuciones_paises_com(
  dt = df_country_anopas, flujo = "exp", region = "mad", parametros = paramets
)
.guardar_html(contrib_exp_mad_pais_anopas,
              file.path(paramets$path_outh, "madrid_anopasado",
                        sprintf("contrib_exp_mad_pais_%s.html", sufijo_anopas)))

#### Importaciones ----
vol_imp_mad_sec_anopas <- .grafica_volumen_sectores_com(
  dt = df_sec_anopas, flujo = "imp", region = "mad", parametros = paramets
)
.guardar_html(vol_imp_mad_sec_anopas,
              file.path(paramets$path_outh, "madrid_anopasado",
                        sprintf("vol_imp_mad_sec_%s.html", sufijo_anopas)))

contrib_imp_mad_sec_anopas <- .grafica_contribuciones_sectores_com(
  dt = df_sec_anopas, flujo = "imp", region = "mad", parametros = paramets
)
.guardar_html(contrib_imp_mad_sec_anopas,
              file.path(paramets$path_outh, "madrid_anopasado",
                        sprintf("contrib_imp_mad_sec_%s.html", sufijo_anopas)))

vol_imp_mad_pais_anopas <- .grafica_volumen_paises_com(
  dt = df_country_anopas, flujo = "imp", region = "mad", parametros = paramets
)
.guardar_html(vol_imp_mad_pais_anopas,
              file.path(paramets$path_outh, "madrid_anopasado",
                        sprintf("vol_imp_mad_pais_%s.html", sufijo_anopas)))

contrib_imp_mad_pais_anopas <- .grafica_contribuciones_paises_com(
  dt = df_country_anopas, flujo = "imp", region = "mad", parametros = paramets
)
.guardar_html(contrib_imp_mad_pais_anopas,
              file.path(paramets$path_outh, "madrid_anopasado",
                        sprintf("contrib_imp_mad_pais_%s.html", sufijo_anopas)))

### España ----
#### Exportaciones ----
vol_exp_esp_sec_anopas <- .grafica_volumen_sectores_com(
  dt = df_sec_anopas, flujo = "exp", region = "esp", parametros = paramets
)
.guardar_html(vol_exp_esp_sec_anopas,
              file.path(paramets$path_outh, "espana_anopasado",
                        sprintf("vol_exp_esp_sec_%s.html", sufijo_anopas)))

contrib_exp_esp_sec_anopas <- .grafica_contribuciones_sectores_com(
  dt = df_sec_anopas, flujo = "exp", region = "esp", parametros = paramets
)
.guardar_html(contrib_exp_esp_sec_anopas,
              file.path(paramets$path_outh, "espana_anopasado",
                        sprintf("contrib_exp_esp_sec_%s.html", sufijo_anopas)))

vol_exp_esp_pais_anopas <- .grafica_volumen_paises_com(
  dt = df_country_anopas, flujo = "exp", region = "esp", parametros = paramets
)
.guardar_html(vol_exp_esp_pais_anopas,
              file.path(paramets$path_outh, "espana_anopasado",
                        sprintf("vol_exp_esp_pais_%s.html", sufijo_anopas)))

contrib_exp_esp_pais_anopas <- .grafica_contribuciones_paises_com(
  dt = df_country_anopas, flujo = "exp", region = "esp", parametros = paramets
)
.guardar_html(contrib_exp_esp_pais_anopas,
              file.path(paramets$path_outh, "espana_anopasado",
                        sprintf("contrib_exp_esp_pais_%s.html", sufijo_anopas)))

#### Importaciones ----
vol_imp_esp_sec_anopas <- .grafica_volumen_sectores_com(
  dt = df_sec_anopas, flujo = "imp", region = "esp", parametros = paramets
)
.guardar_html(vol_imp_esp_sec_anopas,
              file.path(paramets$path_outh, "espana_anopasado",
                        sprintf("vol_imp_esp_sec_%s.html", sufijo_anopas)))

contrib_imp_esp_sec_anopas <- .grafica_contribuciones_sectores_com(
  dt = df_sec_anopas, flujo = "imp", region = "esp", parametros = paramets
)
.guardar_html(contrib_imp_esp_sec_anopas,
              file.path(paramets$path_outh, "espana_anopasado",
                        sprintf("contrib_imp_esp_sec_%s.html", sufijo_anopas)))

vol_imp_esp_pais_anopas <- .grafica_volumen_paises_com(
  dt = df_country_anopas, flujo = "imp", region = "esp", parametros = paramets
)
.guardar_html(vol_imp_esp_pais_anopas,
              file.path(paramets$path_outh, "espana_anopasado",
                        sprintf("vol_imp_esp_pais_%s.html", sufijo_anopas)))

contrib_imp_esp_pais_anopas <- .grafica_contribuciones_paises_com(
  dt = df_country_anopas, flujo = "imp", region = "esp", parametros = paramets
)
.guardar_html(contrib_imp_esp_pais_anopas,
              file.path(paramets$path_outh, "espana_anopasado",
                        sprintf("contrib_imp_esp_pais_%s.html", sufijo_anopas)))

## Bump charts año pasado ----

### Madrid ----
bump_exp_mad_paises_anopas <- .grafica_bump_chart(
  dt         = df_evol_countryfull_anopas[cod != 0],
  flujo      = "exp",
  region     = "mad",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_exp_mad_paises_anopas,
              file.path(paramets$path_outh, "madrid_anopasado",
                        sprintf("bump_exp_mad_paises_%s.html", sufijo_anopas)))

bump_exp_mad_sec_anopas <- .grafica_bump_chart(
  dt         = df_evol_secfull_anopas[niv >= 2],
  flujo      = "exp",
  region     = "mad",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_exp_mad_sec_anopas,
              file.path(paramets$path_outh, "madrid_anopasado",
                        sprintf("bump_exp_mad_sec_%s.html", sufijo_anopas)))

bump_imp_mad_paises_anopas <- .grafica_bump_chart(
  dt         = df_evol_countryfull_anopas[cod != 0],
  flujo      = "imp",
  region     = "mad",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_imp_mad_paises_anopas,
              file.path(paramets$path_outh, "madrid_anopasado",
                        sprintf("bump_imp_mad_paises_%s.html", sufijo_anopas)))

bump_imp_mad_sec_anopas <- .grafica_bump_chart(
  dt         = df_evol_secfull_anopas[niv >= 2],
  flujo      = "imp",
  region     = "mad",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_imp_mad_sec_anopas,
              file.path(paramets$path_outh, "madrid_anopasado",
                        sprintf("bump_imp_mad_sec_%s.html", sufijo_anopas)))

### España ----
bump_exp_esp_paises_anopas <- .grafica_bump_chart(
  dt         = df_evol_countryfull_anopas[cod != 0],
  flujo      = "exp",
  region     = "esp",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_exp_esp_paises_anopas,
              file.path(paramets$path_outh, "espana_anopasado",
                        sprintf("bump_exp_esp_paises_%s.html", sufijo_anopas)))

bump_exp_esp_sec_anopas <- .grafica_bump_chart(
  dt         = df_evol_secfull_anopas[niv >= 2],
  flujo      = "exp",
  region     = "esp",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_exp_esp_sec_anopas,
              file.path(paramets$path_outh, "espana_anopasado",
                        sprintf("bump_exp_esp_sec_%s.html", sufijo_anopas)))

bump_imp_esp_paises_anopas <- .grafica_bump_chart(
  dt         = df_evol_countryfull_anopas[cod != 0],
  flujo      = "imp",
  region     = "esp",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_imp_esp_paises_anopas,
              file.path(paramets$path_outh, "espana_anopasado",
                        sprintf("bump_imp_esp_paises_%s.html", sufijo_anopas)))

bump_imp_esp_sec_anopas <- .grafica_bump_chart(
  dt         = df_evol_secfull_anopas[niv >= 2],
  flujo      = "imp",
  region     = "esp",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = paramets
)
.guardar_html(bump_imp_esp_sec_anopas,
              file.path(paramets$path_outh, "espana_anopasado",
                        sprintf("bump_imp_esp_sec_%s.html", sufijo_anopas)))

# Limpieza de memoria ----
.limpiar_memoria()