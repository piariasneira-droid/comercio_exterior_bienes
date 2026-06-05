# Environment ----
source("./scr/R/nota_sectores/procfun/funciones_text.r")

# Rutas salida ----
m_start <- min(paramets$mes)
m_end   <- max(paramets$mes)

nompath <- if (m_start == m_end) {
  sprintf("nota_sec_%d_%02d",      paramets$anho, m_start)
} else {
  sprintf("nota_sec_%d_%02d-%02d", paramets$anho, m_start, m_end)
}

paramets$path_out  <- file.path("./data/output", nompath)
paramets$path_outx <- file.path(paramets$path_out, "exceles")
paramets$path_outp <- file.path(paramets$path_out, "plots")
paramets$path_outt <- file.path(paramets$path_out, "tablas")
paramets$path_outh <- file.path(paramets$path_out, "htmls")


all_paths <- c(paramets$path_out, paramets$path_outx, paramets$path_outp, paramets$path_outt, paramets$path_outh)
for (path in all_paths) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}

# Labels ----
mes_label <- stringr::str_to_sentence(.build_period_labels(paramets))[1]
fecha_hoy <- format(Sys.Date(), "%d de %B de %Y")

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
