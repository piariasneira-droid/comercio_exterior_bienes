# ── MAPEO BANDERAS (ISO → emoji) ─────────────────────────────────────────────
# Función auxiliar: convierte código ISO2 en emoji bandera
iso2_flag <- function(iso2) {
  chars <- strsplit(toupper(iso2), "")[[1]]
  paste0(intToUtf8(0x1F1E6 - 65L + utf8ToInt(chars[1])),
         intToUtf8(0x1F1E6 - 65L + utf8ToInt(chars[2])))
}

# Tabla de mapeo nombre_pais → ISO2
mapa_banderas <- c(
  "Alemania"           = "DE", "Francia"         = "FR",
  "Italia"             = "IT", "Portugal"        = "PT",
  "Reino Unido"        = "GB", "Estados Unidos"  = "US",
  "China"              = "CN", "Japón"           = "JP",
  "México"             = "MX", "Brasil"          = "BR",
  "Países Bajos"       = "NL", "Bélgica"         = "BE",
  "Polonia"            = "PL", "Marruecos"       = "MA",
  "Turquía"            = "TR", "Arabia Saudí"    = "SA",
  "India"              = "IN", "Corea del Sur"   = "KR",
  "Suecia"             = "SE", "Suiza"           = "CH",
  "Austria"            = "AT", "Argelia"         = "DZ",
  "Argentina"          = "AR", "Chile"           = "CL",
  "Colombia"           = "CO", "Australia"       = "AU",
  "Canadá"             = "CA", "Rusia"           = "RU",
  "Emiratos Árabes Unidos" = "AE", "Singapur"    = "SG",
  "Resto del mundo"    = "🌐"
)

get_flag <- function(nombre_pais) {
  iso <- mapa_banderas[nombre_pais]
  if (is.na(iso) || nchar(iso) != 2) return("🌐")
  tryCatch(iso2_flag(iso), error = function(e) "🌐")
}


# ── FUNCIÓN PRINCIPAL ─────────────────────────────────────────────────────────
# NOTA: dt debe llegar ya filtrado (sin "Total", sin niveles no deseados, etc.)
# La función NO aplica ningún filtro interno.

grafica_bump_chart <- function(
    dt,
    flujo      = "exp",
    region     = "mad",
    tipo       = "paises",     # "paises" | "sectores" — solo afecta etiquetas
    nmax       = 15L,
    titulo     = NULL,
    parametros = list(
      varfactor    = 1e6,
      varud        = "M€",
      dec_num      = 1L,
      fuente_texto = "Calibri",
      colorbf      = "#FFFAE5",
      colpal1      = "#526DB0",
      colpal3      = "#F5C201"
    )
) {
  anos        <- 2019:2025
  varfactor   <- parametros$varfactor
  varud       <- parametros$varud
  dec_num     <- parametros$dec_num
  font        <- parametros$fuente_texto
  colorbf     <- parametros$colorbf
  col_linea   <- if (flujo == "exp") parametros$colpal1 else parametros$colpal3
  
  nombre_flujo   <- if (flujo  == "exp") "Exportaciones" else "Importaciones"
  nombre_region  <- if (region == "mad") "Madrid"        else "España"
  nombre_tipo    <- if (tipo   == "paises") "por país/región" else "por sector"
  
  cols_val  <- paste0(flujo, "_", region, "_", anos)
  col_label <- if (tipo == "paises") "pais" else "nombre"
  
  # ── Sin filtro: se usa dt tal como viene ────────────────────────────────────
  df <- data.table::copy(dt)
  
  missing <- setdiff(c(col_label, cols_val), names(df))
  if (length(missing) > 0)
    stop("Columnas no encontradas en dt: ", paste(missing, collapse = ", "))
  
  df[, valor_medio := rowMeans(.SD, na.rm = TRUE), .SDcols = cols_val]
  df <- df[order(-valor_medio)][seq_len(min(nmax, .N))]
  etiquetas <- df[[col_label]]
  
  df_long <- data.table::melt(
    df,
    id.vars       = col_label,
    measure.vars  = cols_val,
    variable.name = "ano_var",
    value.name    = "valor"
  )
  df_long[, ano := as.integer(gsub(paste0(flujo, "_", region, "_"), "", ano_var))]
  data.table::setnames(df_long, col_label, "label")
  df_long[, rank := rank(-valor, ties.method = "first"), by = ano]
  df_long[, valor_fmt := formatC(
    round(valor / varfactor, dec_num),
    format = "f", digits = dec_num, big.mark = ".", decimal.mark = ","
  )]
  
  n <- length(etiquetas)
  pal_base <- c(
    "#526DB0", "#F5C201", "#E47F56", "#2E7D5E", "#C0392B",
    "#8E44AD", "#1A5276", "#117A65", "#784212", "#616A6B",
    "#2874A6", "#A93226", "#1E8449", "#D4AC0D", "#6C3483"
  )
  paleta <- setNames(pal_base[seq_len(n)], etiquetas)
  
  fig <- plotly::plot_ly()
  
  for (ent in etiquetas) {
    d       <- df_long[label == ent][order(ano)]
    col_ent <- paleta[[ent]]
    
    # línea
    fig <- plotly::add_trace(
      fig,
      data        = d, x = ~ano, y = ~rank,
      type        = "scatter", mode = "lines",
      name        = ent,
      line        = list(color = col_ent, width = 2),
      hoverinfo   = "none",
      showlegend  = FALSE
    )
    
    # puntos
    fig <- plotly::add_trace(
      fig,
      data        = d, x = ~ano, y = ~rank,
      type        = "scatter", mode = "markers",
      name        = ent,
      marker      = list(color = col_ent, size = 9,
                         line = list(color = "white", width = 1.5)),
      text        = ~paste0(
        "<b>", label, "</b><br>",
        "Año: ", ano, "<br>",
        "Ranking: #", rank, "<br>",
        "Valor: ", valor_fmt, " ", varud
      ),
      hovertemplate = "%{text}<extra></extra>",
      showlegend  = FALSE
    )
    
    d_first <- d[ano == min(anos)]
    d_last  <- d[ano == max(anos)]
    
    # ── Etiqueta izquierda: nombre ──────────────────────────────────────────
    fig <- plotly::add_annotations(
      fig,
      x         = d_first$ano,
      y         = d_first$rank,
      text      = paste0("<b>", d_first$label, "</b>"),
      xanchor   = "right",
      yanchor   = "middle",
      showarrow = FALSE,
      font      = list(size = 9, color = col_ent, family = font),
      xshift    = -6
    )
    
    # ── Etiqueta derecha: bandera (países) o ranking (sectores) ────────────
    if (tipo == "paises") {
      texto_derecha <- paste0(get_flag(ent), " <b>", d_last$rank, "</b>")
      size_derecha  <- 12
    } else {
      texto_derecha <- paste0("<b>", d_last$rank, "</b>")
      size_derecha  <- 9
    }
    
    fig <- plotly::add_annotations(
      fig,
      x         = d_last$ano,
      y         = d_last$rank,
      text      = texto_derecha,
      xanchor   = "left",
      yanchor   = "middle",
      showarrow = FALSE,
      font      = list(size = size_derecha, color = col_ent, family = font),
      xshift    = 8
    )
  }
  
  # ── Layout ─────────────────────────────────────────────────────────────────
  if (is.null(titulo))
    titulo <- paste0(nombre_flujo, " de ", nombre_region, " ", nombre_tipo,
                     " — Evolución del ranking (", min(anos), "-", max(anos), ")")
  
  fig <- plotly::layout(
    fig,
    title = list(
      text = paste0("<b>", titulo, "</b>"),
      x    = 0.5,
      font = list(size = 12, color = "black", family = font)
    ),
    xaxis = list(
      title      = "",
      tickvals   = anos,
      ticktext   = as.character(anos),
      tickfont   = list(family = font, size = 10),
      showgrid   = FALSE,
      zeroline   = FALSE,
      showline   = TRUE,
      linecolor  = "black",
      linewidth  = 1
    ),
    yaxis = list(
      title          = "",
      autorange      = "reversed",
      showticklabels = FALSE,
      showgrid       = TRUE,
      gridcolor      = "rgba(0,0,0,0.07)",
      zeroline       = FALSE,
      showline       = FALSE
    ),
    showlegend    = FALSE,
    margin        = list(l = 200, r = 80, t = 60, b = 40),
    paper_bgcolor = colorbf,
    plot_bgcolor  = "rgba(0,0,0,0)"
  )
  
  return(fig)
}


# ── EJEMPLOS DE USO ───────────────────────────────────────────────────────────
# Filtra ANTES de llamar a la función:
#
#   paises  → excluir cod == 0 (Total)
#   sectores → niv >= 2

# Exportaciones Madrid por países
bump_exp_mad_paises <- grafica_bump_chart(
  dt         = df_evol_countryfull[cod != 0],
  flujo      = "exp",
  region     = "mad",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = params
)

# Importaciones Madrid por países
bump_imp_mad_paises <- grafica_bump_chart(
  dt         = df_evol_countryfull[cod != 0],
  flujo      = "imp",
  region     = "mad",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = params
)

# Exportaciones Madrid por sectores
bump_exp_mad_sec <- grafica_bump_chart(
  dt         = df_evol_secfull[niv >= 2],
  flujo      = "exp",
  region     = "mad",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = params
)

# Importaciones Madrid por sectores
bump_imp_mad_sec <- grafica_bump_chart(
  dt         = df_evol_secfull[niv >= 2],
  flujo      = "imp",
  region     = "mad",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = params
)

# ── ESPAÑA ────────────────────────────────────────────────────────────────────

# Exportaciones España por países
bump_exp_esp_paises <- grafica_bump_chart(
  dt         = df_evol_countryfull[cod != 0],
  flujo      = "exp",
  region     = "esp",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = params
)

# Importaciones España por países
bump_imp_esp_paises <- grafica_bump_chart(
  dt         = df_evol_countryfull[cod != 0],
  flujo      = "imp",
  region     = "esp",
  tipo       = "paises",
  nmax       = 15L,
  titulo     = NULL,
  parametros = params
)

# Exportaciones España por sectores
bump_exp_esp_sec <- grafica_bump_chart(
  dt         = df_evol_secfull[niv >= 2],
  flujo      = "exp",
  region     = "esp",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = params
)

# Importaciones España por sectores
bump_imp_esp_sec <- grafica_bump_chart(
  dt         = df_evol_secfull[niv >= 2],
  flujo      = "imp",
  region     = "esp",
  tipo       = "sectores",
  nmax       = 15L,
  titulo     = NULL,
  parametros = params
)