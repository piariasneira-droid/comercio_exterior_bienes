# configaux.r
# ============================================================
# Variables de contexto derivadas de paramets
# ============================================================
# Genera:
#   - Rutas de salida (path_out, path_outx, path_outp, etc.)
#     y crea los directorios si no existen
#   - paramets$fecha, paramets$fecha_ini
#   - mes_label   → etiqueta legible del periodo
#   - fecha_hoy   → fecha de generación
#   - sufijo_mes, sufijo_ytm, sufijo_anopas
#
# IMPORTANTE: espera que paramets ya esté cargado con los
# cuatro parámetros clave (anho, mes, ano_ini, anho_idx)
# actualizados antes de hacer source() de este fichero.
# ============================================================

source("./scr/R/nota_sectores_bis/procfun/funciones_text.r")

# ----------------------------------------------------------
# Periodo: helpers internos válidos para mes escalar o vector
# ----------------------------------------------------------
.m_min <- min(paramets$mes)
.m_max <- max(paramets$mes)
.n_mes <- length(paramets$mes)

# ¿Es un trimestre natural? (Q1=1:3, Q2=4:6, Q3=7:9, Q4=10:12)
.es_trimestre <- .n_mes == 3L &&
                 all(diff(paramets$mes) == 1L) &&
                 (.m_min %% 3L) == 1L

# ----------------------------------------------------------
# Rutas de salida
# ----------------------------------------------------------
nompath <- if (.m_min == .m_max) {
  sprintf("nota_sec_%d_%02d",      paramets$anho, .m_min)
} else if (.es_trimestre) {
  sprintf("nota_sec_%d_Q%d",       paramets$anho, ceiling(.m_max / 3L))
} else {
  sprintf("nota_sec_%d_%02d-%02d", paramets$anho, .m_min, .m_max)
}

paramets$path_out  <- file.path("./data/output", nompath)
paramets$path_outx <- file.path(paramets$path_out, "exceles")
paramets$path_outp <- file.path(paramets$path_out, "plots")
paramets$path_outt <- file.path(paramets$path_out, "tablas")
paramets$path_outh <- file.path(paramets$path_out, "htmls")

# Crear directorios principales
for (.p in c(paramets$path_out,  paramets$path_outx, paramets$path_outp,
             paramets$path_outt, paramets$path_outh)) {
  if (!dir.exists(.p)) dir.create(.p, recursive = TRUE)
}

# Subcarpetas de HTMLs
invisible(lapply(
  c("madrid_mes", "madrid_ytm", "madrid_anopasado",
    "espana_mes", "espana_ytm", "espana_anopasado"),
  function(d) {
    .p <- file.path(paramets$path_outh, d)
    if (!dir.exists(.p)) dir.create(.p, recursive = TRUE, showWarnings = FALSE)
  }
))

# ----------------------------------------------------------
# Fechas en paramets
# ----------------------------------------------------------
# Para fecha se usa el último mes del rango como referencia
paramets$fecha     <- as.Date(paste(paramets$anho, .m_max, "01", sep = "-"))
paramets$fecha_ini <- as.Date(paste(paramets$ano_ini, "01", "01", sep = "-"))

# ----------------------------------------------------------
# Labels
# ----------------------------------------------------------
mes_label <- stringr::str_to_sentence(.build_period_labels(paramets))[1]
fecha_hoy <- format(Sys.Date(), "%d de %B de %Y")

# ----------------------------------------------------------
# Sufijos de archivo
# ----------------------------------------------------------
# sufijo_mes: identifica el periodo analizado
sufijo_mes <- if (.m_min == .m_max) {
  sprintf("%d_%02d",       paramets$anho, .m_max)
} else if (.es_trimestre) {
  sprintf("%d_Q%d",        paramets$anho, ceiling(.m_max / 3L))
} else {
  sprintf("%d_%02d-%02d",  paramets$anho, .m_min, .m_max)
}

# sufijo_ytm: acumulado enero → mes_max
sufijo_ytm    <- sprintf("%d_ytm%02d", paramets$anho, .m_max)

# sufijo_anopas: año anterior completo
sufijo_anopas <- sprintf("%d_anual",   paramets$anho - 1L)

message(
  "[configaux] Periodo: ", mes_label,
  " | sufijo: ", sufijo_mes,
  " | out: ",    paramets$path_out
)
