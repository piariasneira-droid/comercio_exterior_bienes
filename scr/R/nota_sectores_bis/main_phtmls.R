# main_phtmls_bis.R
# ============================================================
# Generación de plots HTML + PNG
# ============================================================
# CAMBIOS respecto a main_phtmls.R:
#   1. Sustituye ~1200 líneas de copy-paste por una tabla de
#      configuración + un bucle genérico
#   2. Los sufijos (sufijo_mes, sufijo_ytm, sufijo_anopas)
#      vienen de parametros_bis.r → funcionan tanto con mes
#      escalar como con rango trimestral
#   3. La lógica de PNG se aplica solo a los plots que la
#      necesitan (contrib), declarado en la config
# ============================================================

# Entorno ----
source("./scr/R/nota_sectores_bis/procfun/funciones_phtmls.R")

# ============================================================
# Helper: construye la ruta de salida de forma consistente
# ============================================================
.ruta_html <- function(subdir, prefix, sufijo, ext = "html") {
  file.path(paramets$path_outh, subdir, sprintf("%s_%s.%s", prefix, sufijo, ext))
}

# ============================================================
# Helper: ejecuta un plot, lo guarda en HTML y opcionalmente
#         genera el PNG con dimensiones ajustadas
# ============================================================
.generar_y_guardar <- function(fn_plot, fn_args, subdir, prefix, sufijo,
                                a_png = FALSE, params_png = NULL) {
  ruta <- .ruta_html(subdir, prefix, sufijo)
  obj  <- do.call(fn_plot, fn_args)
  .guardar_html(obj, ruta)

  if (isTRUE(a_png)) {
    p_png <- if (!is.null(params_png)) params_png else paramets
    .html_a_png(ruta, parametros = p_png)
  }
  invisible(obj)
}

# Parámetros PNG reducidos (para contrib, que son más estrechos)
.params_contrib_png <- modifyList(
  paramets,
  list(ws_width_cm = paramets$ws_width_cm_alt, ws_height_cm = paramets$ws_height_cm_alt)
)

# ============================================================
# Tabla de configuración de plots
# ============================================================
# Cada fila describe un plot. Campos:
#   flag      : nombre del flag en paramets que activa el bloque
#   fn        : función generadora del plot
#   tipo_datos: de qué data.frame tomar los datos ("sectores",
#               "paises", "sec", "country", "contrib_sec_exp",
#               "contrib_sec_imp", "contrib_pais_exp",
#               "contrib_pais_imp", "evol_sec", "evol_pais")
#   flujo     : "exp" / "imp"
#   territorio: "mad" / "esp"   (solo treemaps)
#   region    : "mad" / "esp"   (vol, contrib, bump)
#   tipo      : "sectores" / "paises" (treemaps y bumps)
#   subdir    : subcarpeta dentro de path_outh
#   prefix    : prefijo del nombre de archivo
#   sufijo_var: qué variable de sufijo usar ("mes","ytm","anopas")
#   a_png     : TRUE si además hay que generar PNG
#   tit       : título del plot (solo para contrib)
#   posiciones: parámetros posición treemap
#   nmax      : nº máximo barras bump
# ============================================================

.cfg_plots <- list(

  # ===========================================================
  # BLOQUE: flagmadmes
  # ===========================================================

  ## Treemaps mes — Madrid ----
  list(flag="flagmadmes", fn=".grafica_treemap_plotly",
       tipo_datos="sectores", flujo="exp", territorio="mad", tipo="sectores",
       subdir="madrid_mes", prefix="treemap_exp_mad_sec", sufijo_var="mes", a_png=TRUE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.00)),
  list(flag="flagmadmes", fn=".grafica_treemap_plotly",
       tipo_datos="paises",   flujo="exp", territorio="mad", tipo="paises",
       subdir="madrid_mes", prefix="treemap_exp_mad_pais", sufijo_var="mes", a_png=TRUE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagmadmes", fn=".grafica_treemap_plotly",
       tipo_datos="sectores", flujo="imp", territorio="mad", tipo="sectores",
       subdir="madrid_mes", prefix="treemap_imp_mad_sec", sufijo_var="mes", a_png=TRUE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagmadmes", fn=".grafica_treemap_plotly",
       tipo_datos="paises",   flujo="imp", territorio="mad", tipo="paises",
       subdir="madrid_mes", prefix="treemap_imp_mad_pais", sufijo_var="mes", a_png=TRUE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),

  ## Volumen mes — Madrid ----
  list(flag="flagmadmes", fn=".grafica_volumen_sectores_com",
       tipo_datos="sec",    flujo="exp", region="mad",
       subdir="madrid_mes", prefix="vol_exp_mad_sec", sufijo_var="mes", a_png=FALSE),
  list(flag="flagmadmes", fn=".grafica_volumen_sectores_com",
       tipo_datos="sec",    flujo="imp", region="mad",
       subdir="madrid_mes", prefix="vol_imp_mad_sec", sufijo_var="mes", a_png=FALSE),
  list(flag="flagmadmes", fn=".grafica_volumen_paises_com",
       tipo_datos="country", flujo="exp", region="mad",
       subdir="madrid_mes", prefix="vol_exp_mad_pais", sufijo_var="mes", a_png=FALSE),
  list(flag="flagmadmes", fn=".grafica_volumen_paises_com",
       tipo_datos="country", flujo="imp", region="mad",
       subdir="madrid_mes", prefix="vol_imp_mad_pais", sufijo_var="mes", a_png=FALSE),

  ## Contribuciones mes — Madrid ----
  list(flag="flagmadmes", fn=".grafica_contribuciones_sectores_combis",
       tipo_datos="contrib_sec_exp", region="mad",
       tit="Contribuciones más destacadas a la TVA de las exportaciones madrileñas",
       subdir="madrid_mes", prefix="contrib_exp_mad_sec", sufijo_var="mes", a_png=TRUE),
  list(flag="flagmadmes", fn=".grafica_contribuciones_sectores_combis",
       tipo_datos="contrib_sec_imp", region="mad",
       tit="Contribuciones más destacadas a la TVA de las importaciones madrileñas",
       subdir="madrid_mes", prefix="contrib_imp_mad_sec", sufijo_var="mes", a_png=TRUE),
  list(flag="flagmadmes", fn=".grafica_contribuciones_paises_combis",
       tipo_datos="contrib_pais_exp", region="mad",
       tit="Contribuciones más destacadas a la TVA de las exportaciones madrileñas",
       subdir="madrid_mes", prefix="contrib_exp_mad_pais", sufijo_var="mes", a_png=TRUE),
  list(flag="flagmadmes", fn=".grafica_contribuciones_paises_combis",
       tipo_datos="contrib_pais_imp", region="mad",
       tit="Contribuciones más destacadas a la TVA de las importaciones madrileñas",
       subdir="madrid_mes", prefix="contrib_imp_mad_pais", sufijo_var="mes", a_png=TRUE),

  ## Bump charts mes — Madrid ----
  list(flag="flagmadmes", fn=".grafica_bump_chart",
       tipo_datos="evol_pais", flujo="exp", region="mad", tipo="paises", nmax=15L,
       subdir="madrid_mes", prefix="bump_exp_mad_paises", sufijo_var="mes", a_png=FALSE),
  list(flag="flagmadmes", fn=".grafica_bump_chart",
       tipo_datos="evol_sec",  flujo="exp", region="mad", tipo="sectores", nmax=15L,
       subdir="madrid_mes", prefix="bump_exp_mad_sec", sufijo_var="mes", a_png=FALSE),
  list(flag="flagmadmes", fn=".grafica_bump_chart",
       tipo_datos="evol_pais", flujo="imp", region="mad", tipo="paises", nmax=15L,
       subdir="madrid_mes", prefix="bump_imp_mad_paises", sufijo_var="mes", a_png=FALSE),
  list(flag="flagmadmes", fn=".grafica_bump_chart",
       tipo_datos="evol_sec",  flujo="imp", region="mad", tipo="sectores", nmax=15L,
       subdir="madrid_mes", prefix="bump_imp_mad_sec", sufijo_var="mes", a_png=FALSE),

  # ===========================================================
  # BLOQUE: flagespmes
  # ===========================================================

  ## Treemaps mes — España ----
  list(flag="flagespmes", fn=".grafica_treemap_plotly",
       tipo_datos="sectores", flujo="exp", territorio="esp", tipo="sectores",
       subdir="espana_mes", prefix="treemap_exp_esp_sec", sufijo_var="mes", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagespmes", fn=".grafica_treemap_plotly",
       tipo_datos="paises",   flujo="exp", territorio="esp", tipo="paises",
       subdir="espana_mes", prefix="treemap_exp_esp_pais", sufijo_var="mes", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagespmes", fn=".grafica_treemap_plotly",
       tipo_datos="sectores", flujo="imp", territorio="esp", tipo="sectores",
       subdir="espana_mes", prefix="treemap_imp_esp_sec", sufijo_var="mes", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagespmes", fn=".grafica_treemap_plotly",
       tipo_datos="paises",   flujo="imp", territorio="esp", tipo="paises",
       subdir="espana_mes", prefix="treemap_imp_esp_pais", sufijo_var="mes", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),

  ## Volumen mes — España ----
  list(flag="flagespmes", fn=".grafica_volumen_sectores_com",
       tipo_datos="sec",     flujo="exp", region="esp",
       subdir="espana_mes", prefix="vol_exp_esp_sec", sufijo_var="mes", a_png=FALSE),
  list(flag="flagespmes", fn=".grafica_volumen_sectores_com",
       tipo_datos="sec",     flujo="imp", region="esp",
       subdir="espana_mes", prefix="vol_imp_esp_sec", sufijo_var="mes", a_png=FALSE),
  list(flag="flagespmes", fn=".grafica_volumen_paises_com",
       tipo_datos="country", flujo="exp", region="esp",
       subdir="espana_mes", prefix="vol_exp_esp_pais", sufijo_var="mes", a_png=FALSE),
  list(flag="flagespmes", fn=".grafica_volumen_paises_com",
       tipo_datos="country", flujo="imp", region="esp",
       subdir="espana_mes", prefix="vol_imp_esp_pais", sufijo_var="mes", a_png=FALSE),

  ## Contribuciones mes — España ----
  list(flag="flagespmes", fn=".grafica_contribuciones_sectores_combis",
       tipo_datos="contrib_sec_exp", region="esp",
       tit="Contribuciones más destacadas a la TVA de las exportaciones españolas",
       subdir="espana_mes", prefix="contrib_exp_esp_sec", sufijo_var="mes", a_png=FALSE),
  list(flag="flagespmes", fn=".grafica_contribuciones_sectores_combis",
       tipo_datos="contrib_sec_imp", region="esp",
       tit="Contribuciones más destacadas a la TVA de las importaciones españolas",
       subdir="espana_mes", prefix="contrib_imp_esp_sec", sufijo_var="mes", a_png=FALSE),
  list(flag="flagespmes", fn=".grafica_contribuciones_paises_combis",
       tipo_datos="contrib_pais_exp", region="esp",
       tit="Contribuciones más destacadas a la TVA de las exportaciones españolas",
       subdir="espana_mes", prefix="contrib_exp_esp_pais", sufijo_var="mes", a_png=FALSE),
  list(flag="flagespmes", fn=".grafica_contribuciones_paises_combis",
       tipo_datos="contrib_pais_imp", region="esp",
       tit="Contribuciones más destacadas a la TVA de las importaciones españolas",
       subdir="espana_mes", prefix="contrib_imp_esp_pais", sufijo_var="mes", a_png=FALSE),

  ## Bump charts mes — España ----
  list(flag="flagespmes", fn=".grafica_bump_chart",
       tipo_datos="evol_pais", flujo="exp", region="esp", tipo="paises", nmax=15L,
       subdir="espana_mes", prefix="bump_exp_esp_paises", sufijo_var="mes", a_png=FALSE),
  list(flag="flagespmes", fn=".grafica_bump_chart",
       tipo_datos="evol_sec",  flujo="exp", region="esp", tipo="sectores", nmax=15L,
       subdir="espana_mes", prefix="bump_exp_esp_sec", sufijo_var="mes", a_png=FALSE),
  list(flag="flagespmes", fn=".grafica_bump_chart",
       tipo_datos="evol_pais", flujo="imp", region="esp", tipo="paises", nmax=15L,
       subdir="espana_mes", prefix="bump_imp_esp_paises", sufijo_var="mes", a_png=FALSE),
  list(flag="flagespmes", fn=".grafica_bump_chart",
       tipo_datos="evol_sec",  flujo="imp", region="esp", tipo="sectores", nmax=15L,
       subdir="espana_mes", prefix="bump_imp_esp_sec", sufijo_var="mes", a_png=FALSE),

  # ===========================================================
  # BLOQUE: flagmadytm (acumulado)
  # ===========================================================

  ## Treemaps acumulado — Madrid ----
  list(flag="flagmadytm", fn=".grafica_treemap_plotly",
       tipo_datos="sectores_acu", flujo="exp", territorio="mad", tipo="sectores",
       subdir="madrid_ytm", prefix="treemap_exp_mad_sec", sufijo_var="ytm", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.00)),
  list(flag="flagmadytm", fn=".grafica_treemap_plotly",
       tipo_datos="paises_acu",   flujo="exp", territorio="mad", tipo="paises",
       subdir="madrid_ytm", prefix="treemap_exp_mad_pais", sufijo_var="ytm", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagmadytm", fn=".grafica_treemap_plotly",
       tipo_datos="sectores_acu", flujo="imp", territorio="mad", tipo="sectores",
       subdir="madrid_ytm", prefix="treemap_imp_mad_sec", sufijo_var="ytm", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagmadytm", fn=".grafica_treemap_plotly",
       tipo_datos="paises_acu",   flujo="imp", territorio="mad", tipo="paises",
       subdir="madrid_ytm", prefix="treemap_imp_mad_pais", sufijo_var="ytm", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),

  ## Volumen acumulado — Madrid ----
  list(flag="flagmadytm", fn=".grafica_volumen_sectores_com",
       tipo_datos="sec_acu",     flujo="exp", region="mad",
       subdir="madrid_ytm", prefix="vol_exp_mad_sec", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagmadytm", fn=".grafica_volumen_sectores_com",
       tipo_datos="sec_acu",     flujo="imp", region="mad",
       subdir="madrid_ytm", prefix="vol_imp_mad_sec", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagmadytm", fn=".grafica_volumen_paises_com",
       tipo_datos="country_acu", flujo="exp", region="mad",
       subdir="madrid_ytm", prefix="vol_exp_mad_pais", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagmadytm", fn=".grafica_volumen_paises_com",
       tipo_datos="country_acu", flujo="imp", region="mad",
       subdir="madrid_ytm", prefix="vol_imp_mad_pais", sufijo_var="ytm", a_png=FALSE),

  ## Contribuciones acumulado — Madrid ----
  list(flag="flagmadytm", fn=".grafica_contribuciones_sectores_combis",
       tipo_datos="contrib_sec_exp_acu", region="mad",
       tit="Contribuciones más destacadas a la TVA de las exportaciones madrileñas",
       subdir="madrid_ytm", prefix="contrib_exp_mad_sec", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagmadytm", fn=".grafica_contribuciones_sectores_combis",
       tipo_datos="contrib_sec_imp_acu", region="mad",
       tit="Contribuciones más destacadas a la TVA de las importaciones madrileñas",
       subdir="madrid_ytm", prefix="contrib_imp_mad_sec", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagmadytm", fn=".grafica_contribuciones_paises_combis",
       tipo_datos="contrib_pais_exp_acu", region="mad",
       tit="Contribuciones más destacadas a la TVA de las exportaciones madrileñas",
       subdir="madrid_ytm", prefix="contrib_exp_mad_pais", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagmadytm", fn=".grafica_contribuciones_paises_combis",
       tipo_datos="contrib_pais_imp_acu", region="mad",
       tit="Contribuciones más destacadas a la TVA de las importaciones madrileñas",
       subdir="madrid_ytm", prefix="contrib_imp_mad_pais", sufijo_var="ytm", a_png=FALSE),

  ## Bump acumulado — Madrid ----
  list(flag="flagmadytm", fn=".grafica_bump_chart",
       tipo_datos="evol_pais_acu", flujo="exp", region="mad", tipo="paises", nmax=15L,
       subdir="madrid_ytm", prefix="bump_exp_mad_paises", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagmadytm", fn=".grafica_bump_chart",
       tipo_datos="evol_sec_acu",  flujo="exp", region="mad", tipo="sectores", nmax=15L,
       subdir="madrid_ytm", prefix="bump_exp_mad_sec", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagmadytm", fn=".grafica_bump_chart",
       tipo_datos="evol_pais_acu", flujo="imp", region="mad", tipo="paises", nmax=15L,
       subdir="madrid_ytm", prefix="bump_imp_mad_paises", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagmadytm", fn=".grafica_bump_chart",
       tipo_datos="evol_sec_acu",  flujo="imp", region="mad", tipo="sectores", nmax=15L,
       subdir="madrid_ytm", prefix="bump_imp_mad_sec", sufijo_var="ytm", a_png=FALSE),

  # ===========================================================
  # BLOQUE: flagespytm (acumulado España)
  # ===========================================================

  ## Treemaps acumulado — España ----
  list(flag="flagespytm", fn=".grafica_treemap_plotly",
       tipo_datos="sectores_acu", flujo="exp", territorio="esp", tipo="sectores",
       subdir="espana_ytm", prefix="treemap_exp_esp_sec", sufijo_var="ytm", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagespytm", fn=".grafica_treemap_plotly",
       tipo_datos="paises_acu",   flujo="exp", territorio="esp", tipo="paises",
       subdir="espana_ytm", prefix="treemap_exp_esp_pais", sufijo_var="ytm", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagespytm", fn=".grafica_treemap_plotly",
       tipo_datos="sectores_acu", flujo="imp", territorio="esp", tipo="sectores",
       subdir="espana_ytm", prefix="treemap_imp_esp_sec", sufijo_var="ytm", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagespytm", fn=".grafica_treemap_plotly",
       tipo_datos="paises_acu",   flujo="imp", territorio="esp", tipo="paises",
       subdir="espana_ytm", prefix="treemap_imp_esp_pais", sufijo_var="ytm", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),

  ## Volumen / Contrib / Bump acumulado — España ----
  list(flag="flagespytm", fn=".grafica_volumen_sectores_com",
       tipo_datos="sec_acu",     flujo="exp", region="esp",
       subdir="espana_ytm", prefix="vol_exp_esp_sec", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagespytm", fn=".grafica_volumen_sectores_com",
       tipo_datos="sec_acu",     flujo="imp", region="esp",
       subdir="espana_ytm", prefix="vol_imp_esp_sec", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagespytm", fn=".grafica_volumen_paises_com",
       tipo_datos="country_acu", flujo="exp", region="esp",
       subdir="espana_ytm", prefix="vol_exp_esp_pais", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagespytm", fn=".grafica_volumen_paises_com",
       tipo_datos="country_acu", flujo="imp", region="esp",
       subdir="espana_ytm", prefix="vol_imp_esp_pais", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagespytm", fn=".grafica_contribuciones_sectores_combis",
       tipo_datos="contrib_sec_exp_acu", region="esp",
       tit="Contribuciones más destacadas a la TVA de las exportaciones españolas",
       subdir="espana_ytm", prefix="contrib_exp_esp_sec", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagespytm", fn=".grafica_contribuciones_sectores_combis",
       tipo_datos="contrib_sec_imp_acu", region="esp",
       tit="Contribuciones más destacadas a la TVA de las importaciones españolas",
       subdir="espana_ytm", prefix="contrib_imp_esp_sec", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagespytm", fn=".grafica_contribuciones_paises_combis",
       tipo_datos="contrib_pais_exp_acu", region="esp",
       tit="Contribuciones más destacadas a la TVA de las exportaciones españolas",
       subdir="espana_ytm", prefix="contrib_exp_esp_pais", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagespytm", fn=".grafica_contribuciones_paises_combis",
       tipo_datos="contrib_pais_imp_acu", region="esp",
       tit="Contribuciones más destacadas a la TVA de las importaciones españolas",
       subdir="espana_ytm", prefix="contrib_imp_esp_pais", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagespytm", fn=".grafica_bump_chart",
       tipo_datos="evol_pais_acu", flujo="exp", region="esp", tipo="paises", nmax=15L,
       subdir="espana_ytm", prefix="bump_exp_esp_paises", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagespytm", fn=".grafica_bump_chart",
       tipo_datos="evol_sec_acu",  flujo="exp", region="esp", tipo="sectores", nmax=15L,
       subdir="espana_ytm", prefix="bump_exp_esp_sec", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagespytm", fn=".grafica_bump_chart",
       tipo_datos="evol_pais_acu", flujo="imp", region="esp", tipo="paises", nmax=15L,
       subdir="espana_ytm", prefix="bump_imp_esp_paises", sufijo_var="ytm", a_png=FALSE),
  list(flag="flagespytm", fn=".grafica_bump_chart",
       tipo_datos="evol_sec_acu",  flujo="imp", region="esp", tipo="sectores", nmax=15L,
       subdir="espana_ytm", prefix="bump_imp_esp_sec", sufijo_var="ytm", a_png=FALSE),

  # ===========================================================
  # BLOQUE: flagmadanop (año pasado Madrid)
  # ===========================================================

  list(flag="flagmadanop", fn=".grafica_treemap_plotly",
       tipo_datos="sectores_anopas", flujo="exp", territorio="mad", tipo="sectores",
       subdir="madrid_anopasado", prefix="treemap_exp_mad_sec", sufijo_var="anopas", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.00)),
  list(flag="flagmadanop", fn=".grafica_treemap_plotly",
       tipo_datos="paises_anopas",   flujo="exp", territorio="mad", tipo="paises",
       subdir="madrid_anopasado", prefix="treemap_exp_mad_pais", sufijo_var="anopas", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagmadanop", fn=".grafica_treemap_plotly",
       tipo_datos="sectores_anopas", flujo="imp", territorio="mad", tipo="sectores",
       subdir="madrid_anopasado", prefix="treemap_imp_mad_sec", sufijo_var="anopas", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagmadanop", fn=".grafica_treemap_plotly",
       tipo_datos="paises_anopas",   flujo="imp", territorio="mad", tipo="paises",
       subdir="madrid_anopasado", prefix="treemap_imp_mad_pais", sufijo_var="anopas", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagmadanop", fn=".grafica_volumen_sectores_com",
       tipo_datos="sec_anopas",     flujo="exp", region="mad",
       subdir="madrid_anopasado", prefix="vol_exp_mad_sec", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagmadanop", fn=".grafica_volumen_sectores_com",
       tipo_datos="sec_anopas",     flujo="imp", region="mad",
       subdir="madrid_anopasado", prefix="vol_imp_mad_sec", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagmadanop", fn=".grafica_volumen_paises_com",
       tipo_datos="country_anopas", flujo="exp", region="mad",
       subdir="madrid_anopasado", prefix="vol_exp_mad_pais", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagmadanop", fn=".grafica_volumen_paises_com",
       tipo_datos="country_anopas", flujo="imp", region="mad",
       subdir="madrid_anopasado", prefix="vol_imp_mad_pais", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagmadanop", fn=".grafica_contribuciones_sectores_combis",
       tipo_datos="contrib_sec_imp_anopas", region="mad",
       tit="Contribuciones más destacadas a la TVA de las importaciones madrileñas",
       subdir="madrid_anopasado", prefix="contrib_imp_mad_sec", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagmadanop", fn=".grafica_contribuciones_paises_combis",
       tipo_datos="contrib_pais_exp_anopas", region="mad",
       tit="Contribuciones más destacadas a la TVA de las exportaciones madrileñas",
       subdir="madrid_anopasado", prefix="contrib_exp_mad_pais", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagmadanop", fn=".grafica_contribuciones_paises_combis",
       tipo_datos="contrib_pais_imp_anopas", region="mad",
       tit="Contribuciones más destacadas a la TVA de las importaciones madrileñas",
       subdir="madrid_anopasado", prefix="contrib_imp_mad_pais", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagmadanop", fn=".grafica_bump_chart",
       tipo_datos="evol_pais_anopas", flujo="exp", region="mad", tipo="paises", nmax=15L,
       subdir="madrid_anopasado", prefix="bump_exp_mad_paises", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagmadanop", fn=".grafica_bump_chart",
       tipo_datos="evol_sec_anopas",  flujo="exp", region="mad", tipo="sectores", nmax=15L,
       subdir="madrid_anopasado", prefix="bump_exp_mad_sec", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagmadanop", fn=".grafica_bump_chart",
       tipo_datos="evol_pais_anopas", flujo="imp", region="mad", tipo="paises", nmax=15L,
       subdir="madrid_anopasado", prefix="bump_imp_mad_paises", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagmadanop", fn=".grafica_bump_chart",
       tipo_datos="evol_sec_anopas",  flujo="imp", region="mad", tipo="sectores", nmax=15L,
       subdir="madrid_anopasado", prefix="bump_imp_mad_sec", sufijo_var="anopas", a_png=FALSE),

  # ===========================================================
  # BLOQUE: flagespanop (año pasado España)
  # ===========================================================

  list(flag="flagespanop", fn=".grafica_treemap_plotly",
       tipo_datos="sectores_anopas", flujo="exp", territorio="esp", tipo="sectores",
       subdir="espana_anopasado", prefix="treemap_exp_esp_sec", sufijo_var="anopas", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagespanop", fn=".grafica_treemap_plotly",
       tipo_datos="paises_anopas",   flujo="exp", territorio="esp", tipo="paises",
       subdir="espana_anopasado", prefix="treemap_exp_esp_pais", sufijo_var="anopas", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagespanop", fn=".grafica_treemap_plotly",
       tipo_datos="sectores_anopas", flujo="imp", territorio="esp", tipo="sectores",
       subdir="espana_anopasado", prefix="treemap_imp_esp_sec", sufijo_var="anopas", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagespanop", fn=".grafica_treemap_plotly",
       tipo_datos="paises_anopas",   flujo="imp", territorio="esp", tipo="paises",
       subdir="espana_anopasado", prefix="treemap_imp_esp_pais", sufijo_var="anopas", a_png=FALSE,
       posiciones=list(plot_y=c(0.25,1), cbar_y=0.05)),
  list(flag="flagespanop", fn=".grafica_volumen_sectores_com",
       tipo_datos="sec_anopas",     flujo="exp", region="esp",
       subdir="espana_anopasado", prefix="vol_exp_esp_sec", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagespanop", fn=".grafica_volumen_sectores_com",
       tipo_datos="sec_anopas",     flujo="imp", region="esp",
       subdir="espana_anopasado", prefix="vol_imp_esp_sec", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagespanop", fn=".grafica_volumen_paises_com",
       tipo_datos="country_anopas", flujo="exp", region="esp",
       subdir="espana_anopasado", prefix="vol_exp_esp_pais", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagespanop", fn=".grafica_volumen_paises_com",
       tipo_datos="country_anopas", flujo="imp", region="esp",
       subdir="espana_anopasado", prefix="vol_imp_esp_pais", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagespanop", fn=".grafica_contribuciones_sectores_combis",
       tipo_datos="contrib_sec_exp_anopas", region="esp",
       tit="Contribuciones más destacadas a la TVA de las exportaciones españolas",
       subdir="espana_anopasado", prefix="contrib_exp_esp_sec", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagespanop", fn=".grafica_contribuciones_sectores_combis",
       tipo_datos="contrib_sec_imp_anopas", region="esp",
       tit="Contribuciones más destacadas a la TVA de las importaciones españolas",
       subdir="espana_anopasado", prefix="contrib_imp_esp_sec", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagespanop", fn=".grafica_contribuciones_paises_combis",
       tipo_datos="contrib_pais_exp_anopas", region="esp",
       tit="Contribuciones más destacadas a la TVA de las exportaciones españolas",
       subdir="espana_anopasado", prefix="contrib_exp_esp_pais", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagespanop", fn=".grafica_contribuciones_paises_combis",
       tipo_datos="contrib_pais_imp_anopas", region="esp",
       tit="Contribuciones más destacadas a la TVA de las importaciones españolas",
       subdir="espana_anopasado", prefix="contrib_imp_esp_pais", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagespanop", fn=".grafica_bump_chart",
       tipo_datos="evol_pais_anopas", flujo="exp", region="esp", tipo="paises", nmax=15L,
       subdir="espana_anopasado", prefix="bump_exp_esp_paises", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagespanop", fn=".grafica_bump_chart",
       tipo_datos="evol_sec_anopas",  flujo="exp", region="esp", tipo="sectores", nmax=15L,
       subdir="espana_anopasado", prefix="bump_exp_esp_sec", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagespanop", fn=".grafica_bump_chart",
       tipo_datos="evol_pais_anopas", flujo="imp", region="esp", tipo="paises", nmax=15L,
       subdir="espana_anopasado", prefix="bump_imp_esp_paises", sufijo_var="anopas", a_png=FALSE),
  list(flag="flagespanop", fn=".grafica_bump_chart",
       tipo_datos="evol_sec_anopas",  flujo="imp", region="esp", tipo="sectores", nmax=15L,
       subdir="espana_anopasado", prefix="bump_imp_esp_sec", sufijo_var="anopas", a_png=FALSE)
)

# ============================================================
# Mapa: tipo_datos → data.frame real en el entorno global
# ============================================================
.resolver_datos <- function(tipo_datos, flujo = NULL, tipo = NULL) {
  df <- switch(tipo_datos,
    "sectores"             = df_sectores,
    "paises"               = df_paises,
    "sec"                  = df_sec,
    "country"              = df_country,
    "evol_sec"             = df_evol_secfull,
    "evol_pais"            = df_evol_countryfull,
    "contrib_sec_exp"      = df_contrib_sec_exp_informe,
    "contrib_sec_imp"      = df_contrib_sec_imp_informe,
    "contrib_pais_exp"     = df_contrib_paises_exp_informe,
    "contrib_pais_imp"     = df_contrib_paises_imp_informe,
    "contrib_sec_exp_esp"  = df_contrib_sec_exp_informe_esp,
    "contrib_sec_imp_esp"  = df_contrib_sec_imp_informe_esp,
    "contrib_pais_exp_esp" = df_contrib_paises_exp_informe_esp,
    "contrib_pais_imp_esp" = df_contrib_paises_imp_informe_esp,
    # acumulado
    "sectores_acu"             = df_sectores_acu,
    "paises_acu"               = df_paises_acu,
    "sec_acu"                  = df_sec_acu,
    "country_acu"              = df_country_acu,
    "evol_sec_acu"             = df_evol_secfull_acu,
    "evol_pais_acu"            = df_evol_countryfull_acu,
    "contrib_sec_exp_acu"      = df_contrib_sec_exp_informe_acu,
    "contrib_sec_imp_acu"      = df_contrib_sec_imp_informe_acu,
    "contrib_pais_exp_acu"     = df_contrib_paises_exp_informe_acu,
    "contrib_pais_imp_acu"     = df_contrib_paises_imp_informe_acu,
    "contrib_sec_exp_esp_acu"  = df_contrib_sec_exp_informe_esp_acu,
    "contrib_sec_imp_esp_acu"  = df_contrib_sec_imp_informe_esp_acu,
    "contrib_pais_exp_esp_acu" = df_contrib_paises_exp_informe_esp_acu,
    "contrib_pais_imp_esp_acu" = df_contrib_paises_imp_informe_esp_acu,
    # año pasado
    "sectores_anopas"             = df_sectores_anopas,
    "paises_anopas"               = df_paises_anopas,
    "sec_anopas"                  = df_sec_anopas,
    "country_anopas"              = df_country_anopas,
    "evol_sec_anopas"             = df_evol_secfull_anopas,
    "evol_pais_anopas"            = df_evol_countryfull_anopas,
    "contrib_sec_exp_anopas"      = df_contrib_sec_exp_informe_anopas,
    "contrib_sec_imp_anopas"      = df_contrib_sec_imp_informe_anopas,
    "contrib_pais_exp_anopas"     = df_contrib_paises_exp_informe_anopas,
    "contrib_pais_imp_anopas"     = df_contrib_paises_imp_informe_anopas,
    "contrib_sec_exp_esp_anopas"  = df_contrib_sec_exp_informe_esp_anopas,
    "contrib_sec_imp_esp_anopas"  = df_contrib_sec_imp_informe_esp_anopas,
    "contrib_pais_exp_esp_anopas" = df_contrib_paises_exp_informe_esp_anopas,
    "contrib_pais_imp_esp_anopas" = df_contrib_paises_imp_informe_esp_anopas,
    stop("tipo_datos no reconocido: ", tipo_datos)
  )
  # Filtros estándar que el código original aplicaba inline
  if (!is.null(flujo) && !is.null(tipo)) {
    if (tipo == "paises"   && "cod" %in% names(df)) df <- df[cod  != 0]
    if (tipo == "sectores" && "niv" %in% names(df)) df <- df[niv  >= 2]
  }
  df
}

# ============================================================
# Bucle de ejecución
# ============================================================
for (.cfg in .cfg_plots) {

  # Saltar si el flag está desactivado
  if (!isTRUE(paramets[[.cfg$flag]])) next

  # Sufijo de archivo para este bloque
  .sufijo <- switch(.cfg$sufijo_var,
    "mes"    = sufijo_mes,
    "ytm"    = sufijo_ytm,
    "anopas" = sufijo_anopas
  )

  # Construir argumentos según la función
  .args <- list(parametros = paramets)

  if (.cfg$fn == ".grafica_treemap_plotly") {
    .args$dt         <- .resolver_datos(.cfg$tipo_datos)
    .args$flujo      <- .cfg$flujo
    .args$territorio <- .cfg$territorio
    .args$tipo       <- .cfg$tipo
    .args$posiciones <- .cfg$posiciones

  } else if (.cfg$fn %in% c(".grafica_volumen_sectores_com",
                             ".grafica_volumen_paises_com")) {
    .args$dt     <- .resolver_datos(.cfg$tipo_datos)
    .args$flujo  <- .cfg$flujo
    .args$region <- .cfg$region

  } else if (.cfg$fn %in% c(".grafica_contribuciones_sectores_combis",
                             ".grafica_contribuciones_paises_combis")) {
    .args$dt  <- .resolver_datos(.cfg$tipo_datos)
    .args$tit <- .cfg$tit

  } else if (.cfg$fn == ".grafica_bump_chart") {
    .args$dt     <- .resolver_datos(.cfg$tipo_datos, .cfg$flujo, .cfg$tipo)
    .args$flujo  <- .cfg$flujo
    .args$region <- .cfg$region
    .args$tipo   <- .cfg$tipo
    .args$nmax   <- .cfg$nmax
    .args$titulo <- NULL
  }

  # PNG params (solo contrib usa dimensiones reducidas)
  .params_png_cfg <- if (grepl("^contrib", .cfg$tipo_datos)) .params_contrib_png else paramets

  .generar_y_guardar(
    fn_plot    = get(.cfg$fn),
    fn_args    = .args,
    subdir     = .cfg$subdir,
    prefix     = .cfg$prefix,
    sufijo     = .sufijo,
    a_png      = .cfg$a_png,
    params_png = .params_png_cfg
  )

  message("  [OK] ", .cfg$prefix, "_", .sufijo)
}

# Limpieza ----
.limpiar_memoria()
message("[phtmls] Completado.")
