# Entorno ----
source("./scr/R/nota_sectores/procfun/funciones_phtmls.R")

# Plots mes ----
if (isTRUE(paramets$flagmadmes)) {
  ## Treemaps mes ----
  ## Madrid ----
  ### Exportaciones ----
  #### Sectores ----
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
  .html_a_png(file.path(paramets$path_outh, "madrid_mes",
                        sprintf("treemap_exp_mad_sec_%s.html", sufijo_mes)), parametros = paramets)
  
  #### Países ----
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
  .html_a_png(file.path(paramets$path_outh, "madrid_mes",
                        sprintf("treemap_exp_mad_pais_%s.html", sufijo_mes)), parametros = paramets)
  
  ### Importaciones ----
  #### Sectores ----
  treemap_imp_mad_sec  <- .grafica_treemap_plotly(
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
  .html_a_png(file.path(paramets$path_outh, "madrid_mes",
                        sprintf("treemap_imp_mad_sec_%s.html", sufijo_mes)), parametros = paramets)
  
  #### Países ----
  treemap_imp_mad_pais  <- .grafica_treemap_plotly(
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
  .html_a_png(file.path(paramets$path_outh, "madrid_mes",
                        sprintf("treemap_imp_mad_pais_%s.html", sufijo_mes)), parametros = paramets)
  
  ## Volumen y Contribuciones mes ----
  ## Madrid ----
  ### Exportaciones ----
  #### Vol Sectores ----
  vol_exp_mad_sec <- .grafica_volumen_sectores_com(
    dt = df_sec, flujo = "exp", region = "mad", parametros = paramets
  )
  .guardar_html(vol_exp_mad_sec,
                file.path(paramets$path_outh, "madrid_mes",
                          sprintf("vol_exp_mad_sec_%s.html", sufijo_mes)))
  
  #### Contribuciones sectores ----
  contrib_exp_mad_sec <- .grafica_contribuciones_sectores_combis(
    dt         = df_contrib_sec_exp_informe,
    tit        = "Contribuciones más destacadas a la TVA de las exportaciones madrileñas",
    parametros = paramets
  )
  .guardar_html(contrib_exp_mad_sec,
                file.path(paramets$path_outh, "madrid_mes",
                          sprintf("contrib_exp_mad_sec_%s.html", sufijo_mes)))
  
  .html_a_png(file.path(paramets$path_outh, "madrid_mes",
                        sprintf("contrib_exp_mad_sec_%s.html", sufijo_mes)), 
              parametros = modifyList(paramets, list(ws_width_cm = paramets$ws_width_cm_alt, ws_height_cm = paramets$ws_height_cm_alt)))
  
  #### Vol países ----
  vol_exp_mad_pais <- .grafica_volumen_paises_com(
    dt = df_country, flujo = "exp", region = "mad", parametros = paramets
  )
  .guardar_html(vol_exp_mad_pais,
                file.path(paramets$path_outh, "madrid_mes",
                          sprintf("vol_exp_mad_pais_%s.html", sufijo_mes)))
  
  #### Contribuciones países ----
  contrib_exp_mad_pais <- .grafica_contribuciones_paises_combis(
    dt         = df_contrib_paises_exp_informe,
    tit        = "Contribuciones más destacadas a la TVA de las exportaciones madrileñas",
    parametros = paramets
  )
  .guardar_html(contrib_exp_mad_pais,
                file.path(paramets$path_outh, "madrid_mes",
                          sprintf("contrib_exp_mad_pais_%s.html", sufijo_mes)))
  
  .html_a_png(file.path(paramets$path_outh, "madrid_mes",
                        sprintf("contrib_exp_mad_pais_%s.html", sufijo_mes)), 
              parametros = modifyList(paramets, list(ws_width_cm = paramets$ws_width_cm_alt, ws_height_cm = paramets$ws_height_cm_alt)))
 
   ### Importaciones ----
  #### Vol Sectores ----
  vol_imp_mad_sec <- .grafica_volumen_sectores_com(
    dt = df_sec, flujo = "imp", region = "mad", parametros = paramets
  )
  .guardar_html(vol_imp_mad_sec,
                file.path(paramets$path_outh, "madrid_mes",
                          sprintf("vol_imp_mad_sec_%s.html", sufijo_mes)))
  
  #### Contribuciones Sectores ----
  contrib_imp_mad_sec <- .grafica_contribuciones_sectores_combis(
    dt         = df_contrib_sec_imp_informe,
    tit        = "Contribuciones más destacadas a la TVA de las importaciones madrileñas",
    parametros = paramets
  )
  .guardar_html(contrib_imp_mad_sec,
                file.path(paramets$path_outh, "madrid_mes",
                          sprintf("contrib_imp_mad_sec_%s.html", sufijo_mes)))
  
  .html_a_png(file.path(paramets$path_outh, "madrid_mes",
                        sprintf("contrib_imp_mad_sec_%s.html", sufijo_mes)), 
              parametros = modifyList(paramets, list(ws_width_cm = paramets$ws_width_cm_alt, ws_height_cm = paramets$ws_height_cm_alt)))
  
  #### Vol países ----
  vol_imp_mad_pais <- .grafica_volumen_paises_com(
    dt = df_country, flujo = "imp", region = "mad", parametros = paramets
  )
  .guardar_html(vol_imp_mad_pais,
                file.path(paramets$path_outh, "madrid_mes",
                          sprintf("vol_imp_mad_pais_%s.html", sufijo_mes)))
  
  #### Contribuciones países ----
  contrib_imp_mad_pais <- .grafica_contribuciones_paises_combis(
    dt         = df_contrib_paises_imp_informe,
    tit        = "Contribuciones más destacadas a la TVA de las importaciones madrileñas",
    parametros = paramets
  )
  .guardar_html(contrib_imp_mad_pais,
                file.path(paramets$path_outh, "madrid_mes",
                          sprintf("contrib_imp_mad_pais_%s.html", sufijo_mes)))
  
  .html_a_png(file.path(paramets$path_outh, "madrid_mes",
                        sprintf("contrib_imp_mad_pais_%s.html", sufijo_mes)), 
              parametros = modifyList(paramets, list(ws_width_cm = paramets$ws_width_cm_alt, ws_height_cm = paramets$ws_height_cm_alt)))
  
  ## Bump charts mes ----
  ## Madrid ----
  ### Exportaciones ----
  #### Países ----
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
  
  #### Sectores ----
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
  
  ### Importaciones ----
  #### Países ----
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
  
  #### Sectores ----
  bump_imp_mad_sec  <- .grafica_bump_chart(
    dt         = df_evol_secfull[niv  >= 2],
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
} # end flagmadmes

if (isTRUE(paramets$flagespmes)) {
  ## Treemaps mes ----
  ## España ----
  ### Exportaciones ----
  #### Sectores ----
  treemap_exp_esp_sec  <- .grafica_treemap_plotly(
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
  
  #### Países ----
  treemap_exp_esp_pais  <- .grafica_treemap_plotly(
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
  
  ### Importaciones ----
  #### Sectores ----
  treemap_imp_esp_sec  <- .grafica_treemap_plotly(
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
  
  #### Países ----
  treemap_imp_esp_pais  <- .grafica_treemap_plotly(
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
  ## España ----
  ### Exportaciones ----
  #### Vol Sectores ----
  vol_exp_esp_sec <- .grafica_volumen_sectores_com(
    dt = df_sec, flujo = "exp", region = "esp", parametros = paramets
  )
  .guardar_html(vol_exp_esp_sec,
                file.path(paramets$path_outh, "espana_mes",
                          sprintf("vol_exp_esp_sec_%s.html", sufijo_mes)))
  
  #### Contribuciones sectores ----
  contrib_exp_esp_sec <- .grafica_contribuciones_sectores_combis(
    dt         = df_contrib_sec_exp_informe_esp,
    tit        = "Contribuciones más destacadas a la TVA de las exportaciones españolas",
    parametros = paramets
  )
  .guardar_html(contrib_exp_esp_sec,
                file.path(paramets$path_outh, "espana_mes",
                          sprintf("contrib_exp_esp_sec_%s.html", sufijo_mes)))
  
  #### Vol países ----
  vol_exp_esp_pais <- .grafica_volumen_paises_com(
    dt = df_country, flujo = "exp", region = "esp", parametros = paramets
  )
  .guardar_html(vol_exp_esp_pais,
                file.path(paramets$path_outh, "espana_mes",
                          sprintf("vol_exp_esp_pais_%s.html", sufijo_mes)))
  
  #### Contribuciones países ----
  contrib_exp_esp_pais <- .grafica_contribuciones_paises_combis(
    dt         = df_contrib_paises_exp_informe_esp,
    tit        = "Contribuciones más destacadas a la TVA de las exportaciones españolas",
    parametros = paramets
  )
  .guardar_html(contrib_exp_esp_pais,
                file.path(paramets$path_outh, "espana_mes",
                          sprintf("contrib_exp_esp_pais_%s.html", sufijo_mes)))
  
  ### Importaciones ----
  #### Vol Sectores ----
  vol_imp_esp_sec <- .grafica_volumen_sectores_com(
    dt = df_sec, flujo = "imp", region = "esp", parametros = paramets
  )
  .guardar_html(vol_imp_esp_sec,
                file.path(paramets$path_outh, "espana_mes",
                          sprintf("vol_imp_esp_sec_%s.html", sufijo_mes)))
  
  #### Contribuciones Sectores ----
  contrib_imp_esp_sec <- .grafica_contribuciones_sectores_combis(
    dt         = df_contrib_sec_imp_informe_esp,
    tit        = "Contribuciones más destacadas a la TVA de las importaciones españolas",
    parametros = paramets
  )
  .guardar_html(contrib_imp_esp_sec,
                file.path(paramets$path_outh, "espana_mes",
                          sprintf("contrib_imp_esp_sec_%s.html", sufijo_mes)))
  
  #### Vol países ----
  vol_imp_esp_pais <- .grafica_volumen_paises_com(
    dt = df_country, flujo = "imp", region = "esp", parametros = paramets
  )
  .guardar_html(vol_imp_esp_pais,
                file.path(paramets$path_outh, "espana_mes",
                          sprintf("vol_imp_esp_pais_%s.html", sufijo_mes)))
  
  #### Contribuciones países ----
  contrib_imp_esp_pais <- .grafica_contribuciones_paises_combis(
    dt         = df_contrib_paises_imp_informe_esp,
    tit        = "Contribuciones más destacadas a la TVA de las importaciones españolas",
    parametros = paramets
  )
  .guardar_html(contrib_imp_esp_pais,
                file.path(paramets$path_outh, "espana_mes",
                          sprintf("contrib_imp_esp_pais_%s.html", sufijo_mes)))
  
  ## Bump charts mes ----
  ## España ----
  ### Exportaciones ----
  #### Países ----
  bump_exp_esp_paises  <- .grafica_bump_chart(
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
  
  #### Sectores ----
  bump_exp_esp_sec  <- .grafica_bump_chart(
    dt         = df_evol_secfull[niv  >= 2],
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
  
  ### Importaciones ----
  #### Países ----
  bump_imp_esp_paises  <- .grafica_bump_chart(
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
  
  #### Sectores ----
  bump_imp_esp_sec  <- .grafica_bump_chart(
    dt         = df_evol_secfull[niv  >= 2],
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
} # end flagespmes

# Plots acumulado ----
if (isTRUE(paramets$flagmadytm)) {
  ## Treemaps acumulado ----
  ## Madrid ----
  ### Exportaciones ----
  #### Sectores ----
  treemap_exp_mad_sec_acu  <- .grafica_treemap_plotly(
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
  
  #### Países ----
  treemap_exp_mad_pais_acu  <- .grafica_treemap_plotly(
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
  
  ### Importaciones ----
  #### Sectores ----
  treemap_imp_mad_sec_acu  <- .grafica_treemap_plotly(
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
  
  #### Países ----
  treemap_imp_mad_pais_acu  <- .grafica_treemap_plotly(
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
  
  ## Volumen y Contribuciones acumulado ----
  ## Madrid ----
  ### Exportaciones ----
  #### Vol Sectores ----
  vol_exp_mad_sec_acu <- .grafica_volumen_sectores_com(
    dt = df_sec_acu, flujo = "exp", region = "mad", parametros = paramets
  )
  .guardar_html(vol_exp_mad_sec_acu,
                file.path(paramets$path_outh, "madrid_ytm",
                          sprintf("vol_exp_mad_sec_%s.html", sufijo_ytm)))
  
  #### Contribuciones sectores ----
  contrib_exp_mad_sec_acu <- .grafica_contribuciones_sectores_combis(
    dt         = df_contrib_sec_exp_informe_acu,
    tit        = "Contribuciones más destacadas a la TVA de las exportaciones madrileñas",
    parametros = paramets
  )
  .guardar_html(contrib_exp_mad_sec_acu,
                file.path(paramets$path_outh, "madrid_ytm",
                          sprintf("contrib_exp_mad_sec_%s.html", sufijo_ytm)))
  
  #### Vol países ----
  vol_exp_mad_pais_acu <- .grafica_volumen_paises_com(
    dt = df_country_acu, flujo = "exp", region = "mad", parametros = paramets
  )
  .guardar_html(vol_exp_mad_pais_acu,
                file.path(paramets$path_outh, "madrid_ytm",
                          sprintf("vol_exp_mad_pais_%s.html", sufijo_ytm)))
  
  #### Contribuciones países ----
  contrib_exp_mad_pais_acu <- .grafica_contribuciones_paises_combis(
    dt         = df_contrib_paises_exp_informe_acu,
    tit        = "Contribuciones más destacadas a la TVA de las exportaciones madrileñas",
    parametros = paramets
  )
  .guardar_html(contrib_exp_mad_pais_acu,
                file.path(paramets$path_outh, "madrid_ytm",
                          sprintf("contrib_exp_mad_pais_%s.html", sufijo_ytm)))
  
  ### Importaciones ----
  #### Vol Sectores ----
  vol_imp_mad_sec_acu <- .grafica_volumen_sectores_com(
    dt = df_sec_acu, flujo = "imp", region = "mad", parametros = paramets
  )
  .guardar_html(vol_imp_mad_sec_acu,
                file.path(paramets$path_outh, "madrid_ytm",
                          sprintf("vol_imp_mad_sec_%s.html", sufijo_ytm)))
  
  #### Contribuciones Sectores ----
  contrib_imp_mad_sec_acu <- .grafica_contribuciones_sectores_combis(
    dt         = df_contrib_sec_imp_informe_acu,
    tit        = "Contribuciones más destacadas a la TVA de las importaciones madrileñas",
    parametros = paramets
  )
  .guardar_html(contrib_imp_mad_sec_acu,
                file.path(paramets$path_outh, "madrid_ytm",
                          sprintf("contrib_imp_mad_sec_%s.html", sufijo_ytm)))
  
  #### Vol países ----
  vol_imp_mad_pais_acu <- .grafica_volumen_paises_com(
    dt = df_country_acu, flujo = "imp", region = "mad", parametros = paramets
  )
  .guardar_html(vol_imp_mad_pais_acu,
                file.path(paramets$path_outh, "madrid_ytm",
                          sprintf("vol_imp_mad_pais_%s.html", sufijo_ytm)))
  
  #### Contribuciones países ----
  contrib_imp_mad_pais_acu <- .grafica_contribuciones_paises_combis(
    dt         = df_contrib_paises_imp_informe_acu,
    tit        = "Contribuciones más destacadas a la TVA de las importaciones madrileñas",
    parametros = paramets
  )
  .guardar_html(contrib_imp_mad_pais_acu,
                file.path(paramets$path_outh, "madrid_ytm",
                          sprintf("contrib_imp_mad_pais_%s.html", sufijo_ytm)))
  
  ## Bump charts acumulado ----
  ## Madrid ----
  ### Exportaciones ----
  #### Países ----
  bump_exp_mad_paises_acu  <- .grafica_bump_chart(
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
  
  #### Sectores ----
  bump_exp_mad_sec_acu  <- .grafica_bump_chart(
    dt         = df_evol_secfull_acu[niv  >= 2],
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
  
  ### Importaciones ----
  #### Países ----
  bump_imp_mad_paises_acu  <- .grafica_bump_chart(
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
  
  #### Sectores ----
  bump_imp_mad_sec_acu  <- .grafica_bump_chart(
    dt         = df_evol_secfull_acu[niv  >= 2],
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
} # end flagmadytm

if (isTRUE(paramets$flagespytm)) {
  ## Treemaps acumulado ----
  ## España ----
  ### Exportaciones ----
  #### Sectores ----
  treemap_exp_esp_sec_acu  <- .grafica_treemap_plotly(
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
  
  #### Países ----
  treemap_exp_esp_pais_acu  <- .grafica_treemap_plotly(
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
  
  ### Importaciones ----
  #### Sectores ----
  treemap_imp_esp_sec_acu  <- .grafica_treemap_plotly(
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
  
  #### Países ----
  treemap_imp_esp_pais_acu  <- .grafica_treemap_plotly(
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
  ## España ----
  ### Exportaciones ----
  #### Vol Sectores ----
  vol_exp_esp_sec_acu <- .grafica_volumen_sectores_com(
    dt = df_sec_acu, flujo = "exp", region = "esp", parametros = paramets
  )
  .guardar_html(vol_exp_esp_sec_acu,
                file.path(paramets$path_outh, "espana_ytm",
                          sprintf("vol_exp_esp_sec_%s.html", sufijo_ytm)))
  
  #### Contribuciones sectores ----
  contrib_exp_esp_sec_acu <- .grafica_contribuciones_sectores_combis(
    dt         = df_contrib_sec_exp_informe_esp_acu,
    tit        = "Contribuciones más destacadas a la TVA de las exportaciones españolas",
    parametros = paramets
  )
  .guardar_html(contrib_exp_esp_sec_acu,
                file.path(paramets$path_outh, "espana_ytm",
                          sprintf("contrib_exp_esp_sec_%s.html", sufijo_ytm)))
  
  #### Vol países ----
  vol_exp_esp_pais_acu <- .grafica_volumen_paises_com(
    dt = df_country_acu, flujo = "exp", region = "esp", parametros = paramets
  )
  .guardar_html(vol_exp_esp_pais_acu,
                file.path(paramets$path_outh, "espana_ytm",
                          sprintf("vol_exp_esp_pais_%s.html", sufijo_ytm)))
  
  #### Contribuciones países ----
  contrib_exp_esp_pais_acu <- .grafica_contribuciones_paises_combis(
    dt         = df_contrib_paises_exp_informe_esp_acu,
    tit        = "Contribuciones más destacadas a la TVA de las exportaciones españolas",
    parametros = paramets
  )
  .guardar_html(contrib_exp_esp_pais_acu,
                file.path(paramets$path_outh, "espana_ytm",
                          sprintf("contrib_exp_esp_pais_%s.html", sufijo_ytm)))
  
  ### Importaciones ----
  #### Vol Sectores ----
  vol_imp_esp_sec_acu <- .grafica_volumen_sectores_com(
    dt = df_sec_acu, flujo = "imp", region = "esp", parametros = paramets
  )
  .guardar_html(vol_imp_esp_sec_acu,
                file.path(paramets$path_outh, "espana_ytm",
                          sprintf("vol_imp_esp_sec_%s.html", sufijo_ytm)))
  
  #### Contribuciones Sectores ----
  contrib_imp_esp_sec_acu <- .grafica_contribuciones_sectores_combis(
    dt         = df_contrib_sec_imp_informe_esp_acu,
    tit        = "Contribuciones más destacadas a la TVA de las importaciones españolas",
    parametros = paramets
  )
  .guardar_html(contrib_imp_esp_sec_acu,
                file.path(paramets$path_outh, "espana_ytm",
                          sprintf("contrib_imp_esp_sec_%s.html", sufijo_ytm)))
  
  #### Vol países ----
  vol_imp_esp_pais_acu <- .grafica_volumen_paises_com(
    dt = df_country_acu, flujo = "imp", region = "esp", parametros = paramets
  )
  .guardar_html(vol_imp_esp_pais_acu,
                file.path(paramets$path_outh, "espana_ytm",
                          sprintf("vol_imp_esp_pais_%s.html", sufijo_ytm)))
  
  #### Contribuciones países ----
  contrib_imp_esp_pais_acu <- .grafica_contribuciones_paises_combis(
    dt         = df_contrib_paises_imp_informe_esp_acu,
    tit        = "Contribuciones más destacadas a la TVA de las importaciones españolas",
    parametros = paramets
  )
  .guardar_html(contrib_imp_esp_pais_acu,
                file.path(paramets$path_outh, "espana_ytm",
                          sprintf("contrib_imp_esp_pais_%s.html", sufijo_ytm)))
  
  ## Bump charts acumulado ----
  ## España ----
  ### Exportaciones ----
  #### Países ----
  bump_exp_esp_paises_acu  <- .grafica_bump_chart(
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
  
  #### Sectores ----
  bump_exp_esp_sec_acu  <- .grafica_bump_chart(
    dt         = df_evol_secfull_acu[niv  >= 2],
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
  
  ### Importaciones ----
  #### Países ----
  bump_imp_esp_paises_acu  <- .grafica_bump_chart(
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
  
  #### Sectores ----
  bump_imp_esp_sec_acu  <- .grafica_bump_chart(
    dt         = df_evol_secfull_acu[niv  >= 2],
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
} # end flagespytm

# Plots año pasado ----
if (isTRUE(paramets$flagmadanop)) {
  ## Treemaps año pasado ----
  ## Madrid ----
  ### Exportaciones ----
  #### Sectores ----
  treemap_exp_mad_sec_anopas  <- .grafica_treemap_plotly(
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
  
  #### Países ----
  treemap_exp_mad_pais_anopas  <- .grafica_treemap_plotly(
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
  
  ### Importaciones ----
  #### Sectores ----
  treemap_imp_mad_sec_anopas  <- .grafica_treemap_plotly(
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
  
  #### Países ----
  treemap_imp_mad_pais_anopas  <- .grafica_treemap_plotly(
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
  
  ## Volumen y Contribuciones año pasado ----
  ## Madrid ----
  ### Exportaciones ----
  #### Vol Sectores ----
  vol_exp_mad_sec_anopas <- .grafica_volumen_sectores_com(
    dt = df_sec_anopas, flujo = "exp", region = "mad", parametros = paramets
  )
  .guardar_html(vol_exp_mad_sec_anopas,
                file.path(paramets$path_outh, "madrid_anopasado",
                          sprintf("vol_exp_mad_sec_%s.html", sufijo_anopas)))
  
  
  #### Vol países ----
  vol_exp_mad_pais_anopas <- .grafica_volumen_paises_com(
    dt = df_country_anopas, flujo = "exp", region = "mad", parametros = paramets
  )
  .guardar_html(vol_exp_mad_pais_anopas,
                file.path(paramets$path_outh, "madrid_anopasado",
                          sprintf("vol_exp_mad_pais_%s.html", sufijo_anopas)))
  
  #### Contribuciones países ----
  contrib_exp_mad_pais_anopas <- .grafica_contribuciones_paises_combis(
    dt         = df_contrib_paises_exp_informe_anopas,
    tit        = "Contribuciones más destacadas a la TVA de las exportaciones madrileñas",
    parametros = paramets
  )
  .guardar_html(contrib_exp_mad_pais_anopas,
                file.path(paramets$path_outh, "madrid_anopasado",
                          sprintf("contrib_exp_mad_pais_%s.html", sufijo_anopas)))
  
  ### Importaciones ----
  #### Vol Sectores ----
  vol_imp_mad_sec_anopas <- .grafica_volumen_sectores_com(
    dt = df_sec_anopas, flujo = "imp", region = "mad", parametros = paramets
  )
  .guardar_html(vol_imp_mad_sec_anopas,
                file.path(paramets$path_outh, "madrid_anopasado",
                          sprintf("vol_imp_mad_sec_%s.html", sufijo_anopas)))
  
  #### Contribuciones Sectores ----
  contrib_imp_mad_sec_anopas <- .grafica_contribuciones_sectores_combis(
    dt         = df_contrib_sec_imp_informe_anopas,
    tit        = "Contribuciones más destacadas a la TVA de las importaciones madrileñas",
    parametros = paramets
  )
  .guardar_html(contrib_imp_mad_sec_anopas,
                file.path(paramets$path_outh, "madrid_anopasado",
                          sprintf("contrib_imp_mad_sec_%s.html", sufijo_anopas)))
  
  #### Vol países ----
  vol_imp_mad_pais_anopas <- .grafica_volumen_paises_com(
    dt = df_country_anopas, flujo = "imp", region = "mad", parametros = paramets
  )
  .guardar_html(vol_imp_mad_pais_anopas,
                file.path(paramets$path_outh, "madrid_anopasado",
                          sprintf("vol_imp_mad_pais_%s.html", sufijo_anopas)))
  
  contrib_imp_mad_pais_anopas <- .grafica_contribuciones_paises_combis(
    dt         = df_contrib_paises_imp_informe_anopas,
    tit        = "Contribuciones más destacadas a la TVA de las exportaciones madrileñas",
    parametros = paramets
  )
  
  .guardar_html(contrib_imp_mad_pais_anopas,
                file.path(paramets$path_outh, "madrid_anopasado",
                          sprintf("contrib_imp_mad_pais_%s.html", sufijo_anopas)))
  
  ## Bump charts año pasado ----
  ## Madrid ----
  ### Exportaciones ----
  #### Países ----
  bump_exp_mad_paises_anopas  <- .grafica_bump_chart(
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
  
  #### Sectores ----
  bump_exp_mad_sec_anopas  <- .grafica_bump_chart(
    dt         = df_evol_secfull_anopas[niv  >= 2],
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
  
  ### Importaciones ----
  #### Países ----
  bump_imp_mad_paises_anopas  <- .grafica_bump_chart(
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
  
  #### Sectores ----
  bump_imp_mad_sec_anopas  <- .grafica_bump_chart(
    dt         = df_evol_secfull_anopas[niv  >= 2],
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
} # end flagmadanop

if (isTRUE(paramets$flagespanop)) {
  ## Treemaps año pasado ----
  ## España ----
  ### Exportaciones ----
  #### Sectores ----
  treemap_exp_esp_sec_anopas  <- .grafica_treemap_plotly(
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
  
  #### Países ----
  treemap_exp_esp_pais_anopas  <- .grafica_treemap_plotly(
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
  
  ### Importaciones ----
  #### Sectores ----
  treemap_imp_esp_sec_anopas  <- .grafica_treemap_plotly(
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
  
  #### Países ----
  treemap_imp_esp_pais_anopas  <- .grafica_treemap_plotly(
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
  ## España ----
  ### Exportaciones ----
  #### Vol Sectores ----
  vol_exp_esp_sec_anopas <- .grafica_volumen_sectores_com(
    dt = df_sec_anopas, flujo = "exp", region = "esp", parametros = paramets
  )
  .guardar_html(vol_exp_esp_sec_anopas,
                file.path(paramets$path_outh, "espana_anopasado",
                          sprintf("vol_exp_esp_sec_%s.html", sufijo_anopas)))
  
  #### Contribuciones sectores ----
  contrib_exp_esp_sec_anopas <- .grafica_contribuciones_sectores_combis(
    dt         = df_contrib_sec_exp_informe_esp_anopas,
    tit        = "Contribuciones más destacadas a la TVA de las exportaciones españolas",
    parametros = paramets
  )
  .guardar_html(contrib_exp_esp_sec_anopas,
                file.path(paramets$path_outh, "espana_anopasado",
                          sprintf("contrib_exp_esp_sec_%s.html", sufijo_anopas)))
  
  #### Vol países ----
  vol_exp_esp_pais_anopas <- .grafica_volumen_paises_com(
    dt = df_country_anopas, flujo = "exp", region = "esp", parametros = paramets
  )
  .guardar_html(vol_exp_esp_pais_anopas,
                file.path(paramets$path_outh, "espana_anopasado",
                          sprintf("vol_exp_esp_pais_%s.html", sufijo_anopas)))
  
  #### Contribuciones países ----
  contrib_exp_esp_pais_anopas <- .grafica_contribuciones_paises_combis(
    dt         = df_contrib_paises_exp_informe_esp_anopas,
    tit        = "Contribuciones más destacadas a la TVA de las exportaciones españolas",
    parametros = paramets
  )
  .guardar_html(contrib_exp_esp_pais_anopas,
                file.path(paramets$path_outh, "espana_anopasado",
                          sprintf("contrib_exp_esp_pais_%s.html", sufijo_anopas)))
  
  ### Importaciones ----
  #### Vol Sectores ----
  vol_imp_esp_sec_anopas <- .grafica_volumen_sectores_com(
    dt = df_sec_anopas, flujo = "imp", region = "esp", parametros = paramets
  )
  .guardar_html(vol_imp_esp_sec_anopas,
                file.path(paramets$path_outh, "espana_anopasado",
                          sprintf("vol_imp_esp_sec_%s.html", sufijo_anopas)))
  
  #### Contribuciones Sectores ----
  contrib_imp_esp_sec_anopas <- .grafica_contribuciones_sectores_combis(
    dt         = df_contrib_sec_imp_informe_esp_anopas,
    tit        = "Contribuciones más destacadas a la TVA de las importaciones españolas",
    parametros = paramets
  )
  .guardar_html(contrib_imp_esp_sec_anopas,
                file.path(paramets$path_outh, "espana_anopasado",
                          sprintf("contrib_imp_esp_sec_%s.html", sufijo_anopas)))
  
  #### Vol países ----
  vol_imp_esp_pais_anopas <- .grafica_volumen_paises_com(
    dt = df_country_anopas, flujo = "imp", region = "esp", parametros = paramets
  )
  .guardar_html(vol_imp_esp_pais_anopas,
                file.path(paramets$path_outh, "espana_anopasado",
                          sprintf("vol_imp_esp_pais_%s.html", sufijo_anopas)))
  
  #### Contribuciones países ----
  contrib_imp_esp_pais_anopas <- .grafica_contribuciones_paises_combis(
    dt         = df_contrib_paises_imp_informe_esp_anopas,
    tit        = "Contribuciones más destacadas a la TVA de las importaciones españolas",
    parametros = paramets
  )
  .guardar_html(contrib_imp_esp_pais_anopas,
                file.path(paramets$path_outh, "espana_anopasado",
                          sprintf("contrib_imp_esp_pais_%s.html", sufijo_anopas)))
  
  ## Bump charts año pasado ----
  ## España ----
  ### Exportaciones ----
  #### Países ----
  bump_exp_esp_paises_anopas  <- .grafica_bump_chart(
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
  
  #### Sectores ----
  bump_exp_esp_sec_anopas  <- .grafica_bump_chart(
    dt         = df_evol_secfull_anopas[niv  >= 2],
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
  
  ### Importaciones ----
  #### Países ----
  bump_imp_esp_paises_anopas  <- .grafica_bump_chart(
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
  
  #### Sectores ----
  bump_imp_esp_sec_anopas  <- .grafica_bump_chart(
    dt         = df_evol_secfull_anopas[niv  >= 2],
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
} # end flagespanop

# Limpieza de memoria ----
.limpiar_memoria()