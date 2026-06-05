# Auxiliar ----

## Operador %||% (si no está ya definido en el entorno) ----
`%||%` <- function(a, b) if (!is.null(a)) a else b

## Guardado HTML con librería compartida por subcarpeta ----
.guardar_html <- function(fig, ruta, libdir = "lib") {
  htmlwidgets::saveWidget(
    widget        = plotly::as_widget(fig),
    file          = normalizePath(ruta, mustWork = FALSE),
    selfcontained = FALSE,
    libdir        = libdir
  )
  invisible(ruta)
}

## Conversión HTML → PNG mediante screenshot (webshot2) ----
.html_a_png <- function(ruta_html,
                        vwidth     = NULL,
                        vheight    = NULL,
                        delay      = NULL,
                        zoom       = NULL,
                        dpi        = 300,      
                        parametros = list(
                          ws_width_cm  = 18,   
                          ws_height_cm = 8,    
                          ws_delay     = 1,
                          ws_zoom      = NULL, 
                          path_outp    = "./data/output/nota_sec_2026_03/plots"
                        )) {
  
  # 1. Base screen DPI vs Target DPI
  base_dpi  <- 96
  width_cm  <- parametros$ws_width_cm  %||% 18
  height_cm <- parametros$ws_height_cm %||% 8
  
  # 2. Calculate LOGICAL viewport (prevents layout distortion)
  vwidth  <- vwidth  %||% round((width_cm / 2.54) * base_dpi)
  vheight <- vheight %||% round((height_cm / 2.54) * base_dpi)
  
  # 3. Calculate ZOOM factor for crisp 300 DPI print quality
  zoom    <- zoom %||% parametros$ws_zoom %||% (dpi / base_dpi)
  delay   <- delay %||% parametros$ws_delay %||% 2
  
  # 4. Clean Path Assembly
  nombre_html <- basename(ruta_html)
  nombre_png  <- sub("\\.html$", ".png", nombre_html, ignore.case = TRUE)
  ruta_png    <- file.path(parametros$path_outp, nombre_png)
  
  # 5. Inject CSS to hide the Plotly modebar
  html_lines <- readLines(ruta_html, warn = FALSE)
  css_hide_modebar <- "<style>.modebar { display: none !important; }</style>"
  
  # FIX: Create the temp file in the SAME directory to preserve relative paths (.js, .css)
  directorio_origen <- dirname(ruta_html)
  ruta_html_temp    <- file.path(directorio_origen, paste0("temp_hide_bar_", nombre_html))
  
  writeLines(c(html_lines, css_hide_modebar), ruta_html_temp)
  
  # 6. Execute Webshot on the patched file
  webshot2::webshot(
    url     = normalizePath(ruta_html_temp, mustWork = FALSE),
    file    = ruta_png,
    vwidth  = vwidth,
    vheight = vheight,
    delay   = delay,
    zoom    = zoom
  )
  
  # 7. Clean up the temporary file immediately
  unlink(ruta_html_temp)
  
  invisible(ruta_png)
}

## Formato porcentaje desde ratio (×100) — complementa funciones_text.r ----
# (.fmt_num_inf, .fmt_num, .fmt_pct, .fmt_pp ya están en funciones_text.r)
.fmt_pct_val <- function(x, dec) {
  formatC(round(x * 100, dec), format = "f", digits = dec,
          big.mark = ".", decimal.mark = ",")
}


# Treemaps ----

## Ggplot ----
.grafica_treemap_ggplot <- function(dt,
                                    flujo      = "exp",
                                    territorio = "mad",
                                    tipo       = "sectores",
                                    titulo     = NULL,
                                    parametros = list(
                                      varfactor     = 1e6,
                                      varud         = "M€",
                                      dec_num       = 1L,
                                      dec_per       = 1L,
                                      max_nivel_sec = 2L,
                                      max_nivel_pai = 2L,
                                      plot_width    = 9.5,
                                      plot_height   = 13,
                                      plot_units    = "cm",
                                      tema          = NULL,
                                      fuente_texto  = "Whitney Light",
                                      colorbf       = "#FFFFFF"
                                    )) {
  
  varfactor <- parametros$varfactor
  varud     <- parametros$varud
  dec_num   <- parametros$dec_num
  dec_per   <- parametros$dec_per
  max_nivel <- if (tipo == "sectores") parametros$max_nivel_sec else parametros$max_nivel_pai
  
  if (tipo == "sectores") {
    filtro_orden  <- 66
    orden_raiz    <- 65
    col_label     <- "nombre"
    texto_entidad <- "Producto"
  } else {
    filtro_orden  <- 72
    orden_raiz    <- 71
    col_label     <- "pais"
    texto_entidad <- "País/Región"
  }
  
  var_val <- paste0(flujo, "_", territorio)
  var_pct <- paste0(flujo, "_", territorio, "_pct")
  var_tva <- paste0(flujo, "_", territorio, "_tva")
  var_con <- paste0(flujo, "_", territorio, "_contrib")
  
  needed_cols <- c("orden", "niv", col_label, var_val, var_pct, var_tva, var_con)
  missing     <- setdiff(needed_cols, names(dt))
  if (length(missing) > 0) {
    stop("Columnas no encontradas en dt: ", paste(missing, collapse = ", "))
  }
  
  df <- data.table::copy(dt)
  df <- df[orden != filtro_orden]
  df <- df[orden != orden_raiz]
  df <- df[niv >= 1 & niv <= max_nivel]
  
  df <- df[, c("orden", "niv", col_label, var_val, var_pct, var_tva, var_con),
           with = FALSE]
  data.table::setnames(
    df,
    old = c(col_label, var_val, var_pct, var_tva, var_con),
    new = c("label",   "valor", "pct",   "tva",   "contrib")
  )
  
  df <- df[!is.na(valor) & valor > 0]
  
  if (nrow(df) == 0) stop("No hay datos para representar con los parámetros indicados.")
  
  niveles_presentes <<- sort(unique(df$niv))
  
  if (length(niveles_presentes) == 1) {
    df[, subgroup := NA_character_]
  } else {
    df[, subgroup := NA_character_]
    
    for (i in seq_len(nrow(df))) {
      niv_i <- df$niv[i]
      if (niv_i == min(niveles_presentes)) {
        df$subgroup[i] <- df$label[i]
      } else {
        candidatos <- df[1:(i - 1)][niv < niv_i]
        if (nrow(candidatos) > 0) {
          df$subgroup[i] <- candidatos[.N, label]
        } else {
          df$subgroup[i] <- "Otros"
        }
      }
    }
    
    nivel_tile <- max(niveles_presentes)
    df <- df[niv == nivel_tile]
  }
  
  c_max    <- max(df$contrib, na.rm = TRUE)
  c_min    <- min(df$contrib, na.rm = TRUE)
  midpoint <- 0
  
  df[, label_plot := paste0(
    label, "\n",
    .fmt_num(valor, varfactor, dec_num), " ", varud, " (", .fmt_pct_val(contrib, dec_per), " p.p.)"
  )]
  
  flujo_text      <- ifelse(flujo == "exp", "Exportaciones", "Importaciones")
  territorio_text <- ifelse(territorio == "mad", "Madrid", "España")
  tipo_text       <- ifelse(tipo == "sectores", "por Sector", "por País/Región")
  
  if (is.null(titulo)) {
    titulo <- paste0(flujo_text, " ", territorio_text, " ", tipo_text)
  }
  
  df_plot <<- df
  
  p <- ggplot(df,
              aes(area     = valor,
                  fill     = contrib,
                  label    = label_plot,
                  subgroup = subgroup)) +
    
    treemapify::geom_treemap(colour = "white", size = 0.8) +
    treemapify::geom_treemap_subgroup_border(colour = "grey30", size = 2) +
    treemapify::geom_treemap_subgroup_text(
      place    = "topleft",
      colour   = "black",
      fontface = "bold",
      size     = 8,
      alpha    = 0.6,
      grow     = FALSE,
      family   = parametros$fuente_texto
    ) +
    treemapify::geom_treemap_text(
      colour  = "blue",
      place   = "bottomright",
      size    = 7,
      grow    = FALSE,
      reflow  = TRUE,
      family  = parametros$fuente_texto
    ) +
    scale_fill_gradient2(
      low      = "red",
      mid      = "lightgrey",
      high     = "green",
      midpoint = midpoint,
      limits   = c(c_min, c_max),
      name     = "Contribución a la TVA (p.p.)",
      labels   = function(x) paste0(
        formatC(round(x * 100, dec_per), format = "f", digits = dec_per, decimal.mark = ","),
        " p.p."
      )
      # ,
      # guide    = guide_colorbar(
      #   title.position = "top",
      #   title.hjust    = 0.5,
      #   barwidth       = unit(1, "npc"),
      #   barheight      = unit(0.3, "cm"),
      #   ticks          = TRUE
      # )
    ) +
    labs(
      title = titulo
    ) +
    parametros$tema +
    theme(
      panel.background  = element_rect(fill = parametros$colorbf, color = NA),
      plot.background   = element_rect(fill = parametros$colorbf, color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      text              = element_text(family = parametros$fuente_texto),
      plot.title        = element_text(family = parametros$fuente_texto),
      plot.caption      = element_text(family = parametros$fuente_texto),
      legend.text       = element_text(family = parametros$fuente_texto),
      legend.title      = element_text(family = parametros$fuente_texto),
      axis.text         = element_text(family = parametros$fuente_texto),
      axis.title        = element_text(family = parametros$fuente_texto)
    )
  
  return(p)
}

## Plotly ----
.grafica_treemap_plotly <- function(dt,
                                    flujo      = "exp",
                                    territorio = "mad",
                                    tipo       = "sectores",
                                    titulo     = NULL,
                                    parametros = list(
                                      varfactor     = 1e6,
                                      varud         = "M€",
                                      dec_num       = 1L,
                                      dec_per       = 1L,
                                      max_nivel_sec = 2L,
                                      max_nivel_pai = 2L,
                                      font_title    = 11,
                                      fuente_texto  = "Whitney Light",
                                      colorbf       = "#FFFFFF"
                                    ),
                                    posiciones = NULL) {
  
  varfactor    <- parametros$varfactor
  varud        <- parametros$varud
  dec_num      <- parametros$dec_num
  dec_per      <- parametros$dec_per
  max_nivel    <- if (tipo == "sectores") parametros$max_nivel_sec else parametros$max_nivel_pai
  font_title   <- if (!is.null(parametros$font_title))  parametros$font_title  else 11
  
  palette_treemap <- if (flujo == "exp") parametros$palette_treemap_exp else parametros$palette_treemap_imp
  c_min <- palette_treemap[1]
  c_mid <- palette_treemap[2]
  c_max <- palette_treemap[3]
  
  if (tipo == "sectores") {
    filtro_orden  <- 66
    parent_raiz   <- "65"
    orden_raiz    <- 65
    col_codigo    <- "cod_sec"
    col_label     <- "nombre"
    col_label_fin <- "sec"
  } else {
    filtro_orden  <- 72
    parent_raiz   <- "71"
    orden_raiz    <- 71
    col_codigo    <- "cod_pais"
    col_label     <- "pais"
    col_label_fin <- "pais"
  }
  
  var_val <- paste0(flujo, "_", territorio)
  var_pct <- paste0(flujo, "_", territorio, "_pct")
  var_tva <- paste0(flujo, "_", territorio, "_tva")
  var_con <- paste0(flujo, "_", territorio, "_contrib")
  
  df <- data.table::copy(dt)
  df <- df[orden != filtro_orden]
  df <- df[niv <= max_nivel | orden == orden_raiz]
  df[, (col_codigo) := as.character(orden)]
  
  df[orden == orden_raiz,
     (col_label) := ifelse(territorio %in% c("mad", "esp"), "C. de Madrid", "España")]
  
  df[, parent := ""]
  for (i in seq_len(nrow(df))) {
    niv_actual <- df[i, niv]
    if (niv_actual <= 1) {
      df[i, parent := if (niv_actual == 0) "" else parent_raiz]
    } else {
      idx_parent <- max(df[1:(i - 1)][niv < niv_actual, orden], na.rm = TRUE)
      df[i, parent := if (is.finite(idx_parent)) as.character(idx_parent) else parent_raiz]
    }
  }
  
  if (orden_raiz %in% df$orden) df[orden == orden_raiz, parent := ""]
  if (col_label != col_label_fin) data.table::setnames(df, col_label, col_label_fin)
  
  max_val <- max(df[[var_val]], na.rm = TRUE)
  scale_factor <- max_val / .Machine$integer.max
  df[, Volumen := as.integer(get(var_val) / scale_factor)]
  df[, tvac := get(var_tva) * 100]
  df[, contrib_pp := get(var_con) * 100]
  
  df[, text_label := paste0(
    get(col_label_fin), "<br><br>",
    .fmt_num(get(var_val), varfactor, dec_num), " ", varud,
    " (", .fmt_pct_val(get(var_pct), dec_per), "%)<br><br>",
    "[", .fmt_pp(contrib_pp, dec_per), " p.p.]"
  )]
  df[orden == orden_raiz, text_label := get(col_label_fin)]
  
  df[, hover_label := paste0(
    "<b>", get(col_label_fin), "</b><br><br>",
    ifelse(flujo == "exp", "Exportaciones", "Importaciones"), "<br>",
    "Volumen: ", .fmt_num(get(var_val), varfactor, dec_num), " ", varud, "<br>",
    "Peso: ", .fmt_pct_val(get(var_pct), dec_per), "%<br>",
    "TVA: ", .fmt_pp(tvac, dec_per), "%<br>",
    "Contribución: ", .fmt_pp(contrib_pp, dec_per), " p.p."
  )]
  
  df[orden == orden_raiz, hover_label := get(col_label_fin)]
  
  
  contrib_max <- max(df$contrib_pp, na.rm = TRUE)
  contrib_min <- min(df$contrib_pp, na.rm = TRUE)
  zero_position <- if (contrib_max == contrib_min) 0.5 else (-contrib_min) / (contrib_max - contrib_min)
  
  flujo_text      <- ifelse(flujo == "exp", "Exportaciones", "Importaciones")
  territorio_text <- ifelse(territorio == "mad", "de la C. de Madrid", " de España")
  tipo_text       <- ifelse(tipo == "sectores", "por sector económico", "por país/región")
  if (is.null(titulo)) titulo <- paste0(flujo_text, " ", territorio_text, " ", tipo_text)
  
  fig <- plotly::plot_ly(
    data         = df,
    type         = "treemap",
    ids          = as.formula(paste0("~", col_codigo)),
    labels       = as.formula(paste0("~", col_label_fin)),
    parents      = ~parent,
    values       = ~Volumen,
    branchvalues = "total",
    text      = ~text_label,
    textinfo  = "text",
    hovertext = ~hover_label,
    hovertemplate = "%{hovertext}<extra></extra>",
    hoverinfo    = "none",
    marker       = list(
      colors     = df$contrib_pp,
      colorscale = list(
        list(0,             c_min),
        list(zero_position, c_mid),
        list(1,             c_max)
      ),
      cmin = contrib_min,
      cmax = contrib_max,
      colorbar = list(
        title       = list(
          text = "Contribución",
          side = "right",
          font = list(color = "black", family = parametros$fuente_texto)
        ),
        orientation = "v",
        x           = 1.01,
        xanchor     = "left",
        y           = 0.5,
        yanchor     = "middle",
        len         = 1,
        lenmode     = "fraction",
        thickness   = 15,
        tickformat  = paste0(".", dec_per, "f"),
        tickfont    = list(family = parametros$fuente_texto)
      )
    )
  )
  
  fig <- plotly::layout(
    fig,
    title = list(
      text = paste0("<b>", titulo, "</b>"),
      x = 0.5,
      font = list(size = font_title, color = "black", family = parametros$fuente_texto)
    ),
    margin = list(t = 60, b = 20, l = 10, r = 80),
    paper_bgcolor = parametros$colorbf,
    plot_bgcolor  = parametros$colorbf
  )
  
  return(fig)
}


# Volumen ----

## .grafica_volumen_sectores_com ----
.grafica_volumen_sectores_com <- function(dt,
                                          flujo      = "exp",
                                          region     = "mad",
                                          nmax       = 8L,
                                          titulo     = NULL,
                                          parametros = list(
                                            anho         = 2024L,
                                            varud         = "M€",
                                            dec_num       = 1L,
                                            dec_per       = 1L,
                                            max_bars_vol = 8L,
                                            font_title   = 11,
                                            font_axis     = 9,
                                            fuente_texto  = "Whitney Light",
                                            colorbf       = "#FFFFFF"
                                          )) {
  
  dec_num      <- parametros$dec_num
  dec_per      <- parametros$dec_per
  anho          <- parametros$anho
  nmax          <- if (!is.null(nmax)) nmax else parametros$max_bars_vol
  font_title    <- if (!is.null(parametros$font_title))  parametros$font_title  else 11
  font_axis     <- if (!is.null(parametros$font_axis))   parametros$font_axis   else 9
  
  region_label   <- ifelse(region == "mad", "Madrid", "España")
  nombre_flujo   <- ifelse(flujo  == "exp", "Exportaciones", "Importaciones")
  adj_territorio <- ifelse(region == "mad", "de la C. de Madrid", "de España")
  
  col_actual <- flujo
  col_prev   <- paste0(flujo, "_prev")
  col_tva    <- paste0("tva_", flujo)
  col_dif    <- paste0(flujo, "_dif")
  
  paleta <- if (flujo == "exp") c(col1 = parametros$colpal1, col2 = parametros$colpal2) else
    c(col1 = parametros$colpal3, col2 = parametros$colpal4)
  
  fila_total <- dt[region == region_label & niv == 0L]
  if (nrow(fila_total) == 0L) stop(paste0("Fila total (niv 0) no encontrada."))
  
  total_prev <- fila_total[[col_prev]]
  
  df <- data.table::copy(dt)
  df <- df[region == region_label & niv >= 2L]
  df[, contrib_pp := get(col_dif) / total_prev * 100]
  df[, tvac := get(col_tva) * 100]
  
  df_filtrado <- df[order(-get(col_actual))][1:min(nmax, .N)]
  df_filtrado <- df_filtrado[order(-get(col_actual))]
  
  df_filtrado[, val_actual_sc := get(col_actual) / 1e9]
  df_filtrado[, val_previo_sc := get(col_prev)   / 1e9]
  df_filtrado[, etiqueta        := substr(nombre, 1L, 25L)]
  df_filtrado[, etiqueta_factor := factor(etiqueta, levels = unique(etiqueta))]
  
  df_filtrado[, hover_label := paste0(
    "<b>", nombre, "</b><br><br>",
    nombre_flujo, "<br>",
    "Volumen: ", .fmt_num(get(col_actual), 1e6, dec_num), " M€<br>",
    "Volumen año anterior: ", .fmt_num(get(col_prev), 1e6, dec_num), " M€<br>",
    "Variación absoluta: ", .fmt_num(get(col_dif), 1e6, dec_num), " M€<br>",
    "TVA: ", .fmt_pp(tvac, dec_per), "%<br>",
    "Contribución: ", .fmt_pp(contrib_pp, dec_per), " p.p."
  )]
  
  if (is.null(titulo))
    titulo <- paste0("Subsectores por volumen total ",
                     ifelse(flujo == "exp", "exportado", "importado"),
                     " ", adj_territorio)
  
  fig <- plotly::plot_ly() |>
    plotly::add_bars(
      data          = df_filtrado,
      x             = ~etiqueta_factor,
      y             = ~val_actual_sc,
      name          = as.character(anho),
      marker        = list(color = paleta["col1"]),
      hovertext     = ~hover_label,
      hovertemplate = "%{hovertext}<extra></extra>"
    ) |>
    plotly::add_bars(
      data          = df_filtrado,
      x             = ~etiqueta_factor,
      y             = ~val_previo_sc,
      name          = as.character(anho - 1L),
      marker        = list(color = paleta["col2"]),
      hovertext     = ~hover_label,
      hovertemplate = "%{hovertext}<extra></extra>"
    ) |>
    plotly::layout(
      title  = list(text = paste0("<b>", titulo, "</b>"), x = 0.5, font = list(size = font_title, color = "black", family = parametros$fuente_texto)),
      barmode = "group",
      xaxis  = list(title = list(text = "Subsector", font = list(family = parametros$fuente_texto)), tickfont = list(size = font_axis, family = parametros$fuente_texto), tickangle = 45, automargin = TRUE,
                    showgrid = FALSE, zeroline = FALSE, showline = TRUE, linecolor = "black", linewidth = 1),
      yaxis  = list(title = list(text = paste0(nombre_flujo, " (mM€)"), standoff = 20, font = list(family = parametros$fuente_texto)), tickfont = list(family = parametros$fuente_texto), automargin = TRUE,
                    showgrid = FALSE, zeroline = FALSE, showline = TRUE, linecolor = "black", linewidth = 1),
      
      showlegend = TRUE,
      legend = list(orientation = "h", xanchor = "right", x = 1, yanchor = "top", y = 1, font = list(family = parametros$fuente_texto)),
      paper_bgcolor = parametros$colorbf,
      plot_bgcolor  = "rgba(0,0,0,0)"
    )
  
  return(fig)
}

## .grafica_volumen_paises_com ----
.grafica_volumen_paises_com <- function(dt,
                                        flujo      = "exp",
                                        region     = "mad",
                                        nmax       = NULL,
                                        titulo     = NULL,
                                        parametros = list(
                                          anho         = 2024L,
                                          varud         = "M€",
                                          dec_num       = 1L,
                                          dec_per       = 1L,
                                          max_bars_vol = 8L,
                                          font_title   = 11,
                                          font_axis     = 9,
                                          fuente_texto  = "Whitney Light",
                                          colorbf       = "#FFFFFF"
                                        )) {
  
  dec_num      <- parametros$dec_num
  dec_per      <- parametros$dec_per
  anho          <- parametros$anho
  nmax          <- if (!is.null(nmax)) nmax else parametros$max_bars_vol
  font_title    <- if (!is.null(parametros$font_title))  parametros$font_title  else 11
  font_axis     <- if (!is.null(parametros$font_axis))   parametros$font_axis   else 9
  
  region_label   <- ifelse(region == "mad", "Madrid", "España")
  nombre_flujo   <- ifelse(flujo  == "exp", "Exportaciones", "Importaciones")
  adj_territorio <- ifelse(region == "mad", "de la C. de Madrid", "de España")
  
  col_actual <- flujo
  col_prev   <- paste0(flujo, "_prev")
  col_tva    <- paste0("tva_", flujo)
  col_dif    <- paste0(flujo, "_dif")
  
  paleta <- if (flujo == "exp") c(col1 = parametros$colpal1, col2 = parametros$colpal2) else
    c(col1 = parametros$colpal3, col2 = parametros$colpal4)
  
  fila_total <- dt[region == region_label & cod == 0L]
  if (nrow(fila_total) == 0L) stop(paste0("Fila total (cod 0) no encontrada."))
  
  total_prev <- fila_total[[col_prev]]
  
  df <- data.table::copy(dt)
  df <- df[region == region_label & cod >= 1L]
  df[, contrib_pp := get(col_dif) / total_prev * 100]
  df[, tvac := get(paste0("tva_", flujo)) * 100]
  
  df_filtrado <- df[order(-get(col_actual))][1:min(nmax, .N)]
  df_filtrado <- df_filtrado[order(-get(col_actual))]
  
  df_filtrado[, val_actual_sc := get(col_actual) / 1e9]
  df_filtrado[, val_previo_sc := get(col_prev)   / 1e9]
  df_filtrado[, pais_factor    := factor(pais, levels = unique(pais))]
  
  df_filtrado[, hover_label := paste0(
    "<b>", pais, "</b><br>",
    reg, "<br><br>",
    nombre_flujo, "<br>",
    "Volumen: ", .fmt_num(get(flujo), 1e6, 1), " M€<br>",
    "Volumen año anterior: ", .fmt_num(get(col_prev), 1e6, 1), " M€<br>",
    "Variación absoluta: ", .fmt_num(get(col_dif), 1e6, 1), " M€<br>",
    "TVA: ", .fmt_pp(tvac, dec_per), "%<br>",
    "Contribución: ", .fmt_pp(contrib_pp, dec_per), " p.p."
  )]
  
  if (is.null(titulo))
    titulo <- paste0("Países por volumen total ",
                     ifelse(flujo == "exp", "exportado", "importado"),
                     " ", adj_territorio)
  
  fig <- plotly::plot_ly() |>
    plotly::add_bars(
      data   = df_filtrado,
      x      = ~pais_factor,
      y      = ~val_actual_sc,
      name   = as.character(anho),
      marker = list(color = paleta["col1"]),
      hovertext     = ~hover_label,
      hovertemplate = "%{hovertext}<extra></extra>"
    ) |>
    plotly::add_bars(
      data   = df_filtrado,
      x      = ~pais_factor,
      y      = ~val_previo_sc,
      name   = as.character(anho - 1L),
      marker = list(color = paleta["col2"]),
      hovertext     = ~hover_label,
      hovertemplate = "%{hovertext}<extra></extra>"
    ) |>
    plotly::layout(
      title  = list(text = paste0("<b>", titulo, "</b>"), x = 0.5, font = list(size = font_title, color = "black", family = parametros$fuente_texto)),
      barmode = "group",
      xaxis  = list(title = list(text = "País", font = list(family = parametros$fuente_texto)), tickfont = list(size = font_axis, family = parametros$fuente_texto), tickangle = 45, automargin = TRUE,
                    showgrid = FALSE, zeroline = FALSE, showline = TRUE, linecolor = "black", linewidth = 1),
      yaxis  = list(title = list(text = paste0(nombre_flujo, " (mM€)"), standoff = 20, font = list(family = parametros$fuente_texto)), tickfont = list(family = parametros$fuente_texto), automargin = TRUE,
                    showgrid = FALSE, zeroline = FALSE, showline = TRUE, linecolor = "black", linewidth = 1),
      
      showlegend = TRUE,
      legend = list(orientation = "h", xanchor = "right", x = 1, yanchor = "top", y = 1, font = list(family = parametros$fuente_texto)),
      paper_bgcolor = parametros$colorbf,
      plot_bgcolor  = "rgba(0,0,0,0)"
    )
  
  return(fig)
}

## .grafica_contribuciones_sectores_com ----
.grafica_contribuciones_sectores_com <- function(dt,
                                                 flujo      = "exp",
                                                 region     = "mad",
                                                 nmax       = 4L,
                                                 titulo     = NULL,
                                                 parametros = list(
                                                   dec_per      = 1L,
                                                   max_bars_con = 4L,
                                                   font_title   = 11,
                                                   font_axis     = 9,
                                                   fuente_texto  = "Whitney Light",
                                                   colorbf       = "#FFFFFF"
                                                 )) {
  
  dec_per      <- parametros$dec_per
  nmax          <- if (!is.null(nmax)) nmax else parametros$max_bars_con
  font_title    <- if (!is.null(parametros$font_title))  parametros$font_title  else 11
  font_axis     <- if (!is.null(parametros$font_axis))   parametros$font_axis   else 9
  
  region_label   <- ifelse(region == "mad", "Madrid", "España")
  nombre_flujo   <- ifelse(flujo  == "exp", "Exportaciones", "Importaciones")
  adj_territorio <- ifelse(region == "mad", "madrileñas", "españolas")
  
  col_prev <- paste0(flujo, "_prev")
  col_dif  <- paste0(flujo, "_dif")
  
  paleta <- if (flujo == "exp") c(positivo = parametros$colpal1, negativo = parametros$colpal2) else
    c(positivo = parametros$colpal3, negativo = parametros$colpal4)
  
  fila_total <- dt[region == region_label & niv == 0L]
  if (nrow(fila_total) == 0L) stop(paste0("Fila total (niv 0) no encontrada."))
  
  total_prev <- fila_total[[col_prev]]
  
  df <- data.table::copy(dt)
  df <- df[region == region_label & niv >= 2L]
  df[, contrib_pp := get(col_dif) / total_prev * 100]
  df[, tvac := get(paste0("tva_", flujo)) * 100]
  
  df_pos <- df[contrib_pp >  0][order(-contrib_pp)][1:min(nmax, .N)]
  df_neg <- df[contrib_pp <= 0][order( contrib_pp)][1:min(nmax, .N)]
  
  df_plot <- data.table::rbindlist(list(df_pos, df_neg))
  df_plot <- df_plot[order(contrib_pp)]
  df_plot[, nombre_factor := factor(nombre, levels = unique(nombre))]
  df_plot[, color    := ifelse(contrib_pp > 0, paleta["positivo"], paleta["negativo"])]
  df_plot[, text_con := .fmt_num_inf(contrib_pp, dec_per)]
  
  df_plot[, hover_label := paste0(
    "<b>", nombre, "</b><br><br>",
    nombre_flujo, "<br>",
    "Volumen: ", .fmt_num(get(ifelse(flujo == "exp", "exp", "imp")), 1e6, 1), " M€<br>",
    "Volumen año anterior: ", .fmt_num(get(col_prev), 1e6, 1), " M€<br>",
    "Variación absoluta: ", .fmt_num(get(col_dif), 1e6, 1), " M€<br>",
    "TVA: ", .fmt_pp(tvac, dec_per), "%<br>",
    "Contribución: ", .fmt_pp(contrib_pp, dec_per), " p.p."
  )]
  
  if (is.null(titulo))
    titulo <- paste0("Contribuciones por sector a la TVA de las ",
                     tolower(nombre_flujo), " ", adj_territorio)
  
  fig <- plotly::plot_ly() |>
    plotly::add_bars(
      data         = df_plot,
      y            = ~nombre_factor,
      x            = ~contrib_pp,
      orientation  = "h",
      marker       = list(color = ~color),
      hovertext     = ~hover_label,
      hovertemplate = "%{hovertext}<extra></extra>",
      showlegend   = FALSE
    ) |>
    plotly::layout(
      title  = list(text = paste0("<b>", titulo, "</b>"), x = 0.5, font = list(size = font_title, color = "black", family = parametros$fuente_texto)),
      xaxis  = list(title = list(text = paste0("Contribución a ", tolower(nombre_flujo), " (p.p.)"), font = list(family = parametros$fuente_texto)), tickfont = list(family = parametros$fuente_texto), automargin = TRUE,
                    showgrid = FALSE, zeroline = FALSE, showline = TRUE, linecolor = "black", linewidth = 1),
      yaxis  = list(title = "", tickfont = list(size = font_axis, family = parametros$fuente_texto), automargin = TRUE,
                    showgrid = FALSE, zeroline = FALSE, showline = TRUE, linecolor = "black", linewidth = 1),
      annotations = lapply(seq_len(nrow(df_plot)), function(i) {
        positivo <- df_plot$contrib_pp[i] > 0
        list(
          x         = 0,
          y         = as.character(df_plot$nombre_factor[i]),
          text      = df_plot$text_con[i],
          xanchor   = ifelse(positivo, "right", "left"),
          yanchor   = "middle",
          showarrow = FALSE,
          xref      = "x",
          yref      = "y",
          xshift    = ifelse(positivo, -4, 4),
          font      = list(color = "black", size = 10, family = parametros$fuente_texto)
        )
      }),
      paper_bgcolor = parametros$colorbf,
      plot_bgcolor  = "rgba(0,0,0,0)"
    )
  
  return(fig)
}

.grafica_contribuciones_sectores_combis <- function(
    dt,
    tit = "Contrib",
    parametros = list(
      anho         = 2024L,
      varud        = "M€",
      dec_num      = 1L,
      dec_per      = 1L,
      font_title   = 11,
      font_axis    = 9,
      fuente_texto = "Whitney Light",
      colorbf      = "#FFFFFF"
    )
) {
  
  dec_num    <- parametros$dec_num
  dec_per    <- parametros$dec_per
  font_title <- if (!is.null(parametros$font_title)) parametros$font_title else 11
  font_axis  <- if (!is.null(parametros$font_axis)) parametros$font_axis else 9
  
  df <- data.table::copy(dt)
  df <- df[order(rep)]
  
  paises_recortados <- sapply(
    strsplit(df$paises, ",\\s*"),
    function(lista_paises) {
      paste(substr(trimws(lista_paises), 1L, 14L), collapse = ", ")
    }
  )
  
  df[, etiqueta_completa := paste0(
    nombre,
    "<br>(",
    paises_recortados,
    ")"
  )]
  
  df[, etiqueta_factor := factor(
    etiqueta_completa,
    levels = unique(etiqueta_completa)
  )]
  
  df[, color_sector := ifelse(
    rep >= 0,
    parametros$colpal1,
    parametros$colpal2
  )]
  
  df[, color_paises := ifelse(
    rep_paises >= 0,
    "#B0B0B0",
    "#E0E0E0"
  )]
  
  # Text displayed between bars
  df[, text_con := paste0(
    .fmt_num_inf(rep, dec_per),
    "<br><span style='font-size:8px;color:gray'>(",
    .fmt_num_inf(rep_paises, dec_per),
    ")</span>"
  )]
  
  # Sector bars
  df_sector <- data.table::copy(df)
  df_sector[, bar_type := "sector"]
  df_sector[, y_pos := as.numeric(etiqueta_factor) + 0.14]
  
  # Countries bars
  df_paises <- data.table::copy(df)
  df_paises[, bar_type := "paises"]
  df_paises[, y_pos := as.numeric(etiqueta_factor) - 0.14]
  
  df_combined <- data.table::rbindlist(
    list(df_sector, df_paises)
  )
  
  df_combined[, hover_label := paste0(
    "<b>", nombre, "</b><br><br>",
    "Contribución sector: ", .fmt_pp(rep, dec_per), " p.p.<br>",
    "TVA: ", .fmt_pp(tva, dec_per), "%<br>",
    "Países relevantes: ", paises, "<br>",
    "Contribución mercados a sector: ",
    .fmt_pp(rep_paises, dec_per), " p.p."
  )]
  
  fig <- plotly::plot_ly()
  
  # Sector bars
  df_sector_plot <- df_combined[bar_type == "sector"]
  
  fig <- fig |>
    plotly::add_bars(
      data          = df_sector_plot,
      y             = ~y_pos,
      x             = ~rep,
      name          = "Contribución sector",
      marker        = list(color = ~color_sector),
      hovertext     = ~hover_label,
      hovertemplate = "%{hovertext}<extra></extra>",
      orientation   = "h",
      width         = 0.66
    )
  
  # Countries bars
  df_paises_plot <- df_combined[bar_type == "paises"]
  
  fig <- fig |>
    plotly::add_bars(
      data          = df_paises_plot,
      y             = ~y_pos,
      x             = ~rep_paises,
      name          = "Contribución países",
      marker        = list(color = ~color_paises),
      hoverinfo     = "skip",
      orientation   = "h",
      width         = 0.10
    ) |>
    plotly::layout(
      
      title = list(
        text = paste0("<b>", tit, "</b>"),
        x = 0.5,
        font = list(
          size = font_title,
          color = "black",
          family = parametros$fuente_texto
        )
      ),
      
      barmode = "overlay",
      
      xaxis = list(
        title = "",
        automargin = TRUE,
        showgrid = FALSE,
        zeroline = TRUE,
        zerolinecolor = "black",
        zerolinewidth = 1,
        showline = TRUE,
        linecolor = "black",
        linewidth = 1
      ),
      
      yaxis = list(
        title = "",
        tickfont = list(
          size = font_axis,
          family = parametros$fuente_texto
        ),
        tickmode = "array",
        tickvals = seq_len(nrow(df)),
        ticktext = df$etiqueta_completa,
        automargin = TRUE,
        showgrid = FALSE,
        zeroline = FALSE,
        showline = TRUE,
        linecolor = "black",
        linewidth = 1
      ),
      
      showlegend = FALSE,
      
      paper_bgcolor = parametros$colorbf,
      plot_bgcolor  = "rgba(0,0,0,0)",
      
      annotations = lapply(seq_len(nrow(df)), function(i) {
        
        positivo <- df$rep[i] >= 0
        
        list(
          x         = 0,
          y         = i,
          text      = df$text_con[i],
          
          xanchor   = ifelse(
            positivo,
            "right",
            "left"
          ),
          
          yanchor   = "middle",
          
          showarrow = FALSE,
          
          xref      = "x",
          yref      = "y",
          
          xshift    = ifelse(
            positivo,
            -4,
            4
          ),
          
          align = "center",
          
          font = list(
            color  = "black",
            size   = 9,
            family = parametros$fuente_texto
          )
        )
      })
    )
  
  return(fig)
}


## .grafica_contribuciones_paises_com ----
.grafica_contribuciones_paises_com <- function(dt,
                                               flujo      = "exp",
                                               region     = "mad",
                                               nmax       = NULL,
                                               titulo     = NULL,
                                               parametros = list(
                                                 dec_per      = 1L,
                                                 max_bars_con = 4L,
                                                 font_title   = 11,
                                                 font_axis     = 9,
                                                 fuente_texto  = "Whitney Light",
                                                 colorbf       = "#FFFFFF"
                                               )) {
  
  dec_per      <- parametros$dec_per
  nmax          <- if (!is.null(nmax)) nmax else parametros$max_bars_con
  font_title    <- if (!is.null(parametros$font_title))  parametros$font_title  else 11
  font_axis     <- if (!is.null(parametros$font_axis))   parametros$font_axis   else 9
  
  region_label   <- ifelse(region == "mad", "Madrid", "España")
  nombre_flujo   <- ifelse(flujo  == "exp", "Exportaciones", "Importaciones")
  adj_territorio <- ifelse(region == "mad", "madrileñas", "españolas")
  
  col_actual <- flujo
  col_prev   <- paste0(flujo, "_prev")
  col_dif    <- paste0(flujo, "_dif")
  col_tva    <- paste0("tva_", flujo)
  
  paleta <- if (flujo == "exp") c(positivo = parametros$colpal1, negativo = parametros$colpal2) else
    c(positivo = parametros$colpal3, negativo = parametros$colpal4)
  
  fila_total <- dt[region == region_label & cod == 0L]
  if (nrow(fila_total) == 0L) stop(paste0("Fila total (cod 0) no encontrada."))
  
  total_prev <- fila_total[[col_prev]]
  
  df <- data.table::copy(dt)
  df <- df[region == region_label & cod >= 1L]
  df[, contrib_pp := get(col_dif) / total_prev * 100]
  
  df_pos <- df[contrib_pp >  0][order(-contrib_pp)][1:min(nmax, .N)]
  df_neg <- df[contrib_pp <= 0][order( contrib_pp)][1:min(nmax, .N)]
  
  df_plot <- data.table::rbindlist(list(df_pos, df_neg))
  df_plot <- df_plot[order(contrib_pp)]
  df_plot[, pais_factor := factor(pais, levels = unique(pais))]
  df_plot[, color    := ifelse(contrib_pp > 0, paleta["positivo"], paleta["negativo"])]
  df_plot[, text_con := .fmt_num_inf(contrib_pp, dec_per)]
  df_plot[, tvac := get(paste0("tva_", flujo)) * 100]
  
  df_plot[, hover_label := paste0(
    "<b>", pais, "</b><br>",
    reg, "<br><br>",
    nombre_flujo, "<br>",
    "Volumen: ", .fmt_num(get(col_actual), 1e6, 1), " M€<br>",
    "Volumen año anterior: ", .fmt_num(get(col_prev), 1e6, 1), " M€<br>",
    "Variación absoluta: ", .fmt_num(get(col_dif), 1e6, 1), " M€<br>",
    "TVA: ", .fmt_pp(tvac, dec_per), "%<br>",
    "Contribución: ", .fmt_pp(contrib_pp, dec_per), " p.p."
  )]
  
  if (is.null(titulo))
    titulo <- paste0("Mayores contribuciones por país a la TVA de las ",
                     tolower(nombre_flujo), " ", adj_territorio)
  
  fig <- plotly::plot_ly() |>
    plotly::add_bars(
      data         = df_plot,
      y            = ~pais_factor,
      x            = ~contrib_pp,
      orientation  = "h",
      marker       = list(color = ~color),
      hovertext     = ~hover_label,
      hovertemplate = "%{hovertext}<extra></extra>",
      showlegend   = FALSE
    ) |>
    plotly::layout(
      title  = list(text = paste0("<b>", titulo, "</b>"), x = 0.5, font = list(size = font_title, color = "black", family = parametros$fuente_texto)),
      xaxis  = list(title = list(text = paste0("Contribución a ", tolower(nombre_flujo), " (p.p.)"), font = list(family = parametros$fuente_texto)), tickfont = list(family = parametros$fuente_texto), automargin = TRUE,
                    showgrid = FALSE, zeroline = FALSE, showline = TRUE, linecolor = "black", linewidth = 1),
      yaxis  = list(title = "", tickfont = list(size = font_axis, family = parametros$fuente_texto), automargin = TRUE,
                    showgrid = FALSE, zeroline = FALSE, showline = TRUE, linecolor = "black", linewidth = 1),
      annotations = lapply(seq_len(nrow(df_plot)), function(i) {
        positivo <- df_plot$contrib_pp[i] > 0
        list(
          x         = 0,
          y         = as.character(df_plot$pais_factor[i]),
          text      = df_plot$text_con[i],
          xanchor   = ifelse(positivo, "right", "left"),
          yanchor   = "middle",
          showarrow = FALSE,
          xref      = "x",
          yref      = "y",
          xshift    = ifelse(positivo, -4, 4),
          font      = list(color = "black", size = 10, family = parametros$fuente_texto)
        )
      }),
      paper_bgcolor = parametros$colorbf,
      plot_bgcolor  = "rgba(0,0,0,0)"
    )
  
  return(fig)
}

.grafica_contribuciones_paises_combis <- function(
    dt,
    tit = "Contrib",
    parametros = list(
      anho         = 2024L,
      varud        = "M€",
      dec_num      = 1L,
      dec_per      = 1L,
      font_title   = 11,
      font_axis    = 9,
      fuente_texto = "Whitney Light",
      colorbf      = "#FFFFFF"
    )
) {
  
  dec_num    <- parametros$dec_num
  dec_per    <- parametros$dec_per
  font_title <- if (!is.null(parametros$font_title)) parametros$font_title else 11
  font_axis  <- if (!is.null(parametros$font_axis)) parametros$font_axis else 9
  
  df <- data.table::copy(dt)
  df <- df[order(rep)]
  
  sectores_recortados <- sapply(
    strsplit(df$sectores, ",\\s*"),
    function(lista_paises) {
      paste(substr(trimws(lista_paises), 1L, 14L), collapse = ", ")
    }
  )
  
  df[, etiqueta_completa := paste0(
    pais,
    "<br>(",
    sectores_recortados,
    ")"
  )]
  
  df[, etiqueta_factor := factor(
    etiqueta_completa,
    levels = unique(etiqueta_completa)
  )]
  
  df[, color_pais := ifelse(
    rep >= 0,
    parametros$colpal1,
    parametros$colpal2
  )]
  
  df[, color_sectores := ifelse(
    rep_sectores >= 0,
    "#B0B0B0",
    "#E0E0E0"
  )]
  
  # Text displayed between bars
  df[, text_con := paste0(
    .fmt_num_inf(rep, dec_per),
    "<br><span style='font-size:8px;color:gray'>(",
    .fmt_num_inf(rep_sectores, dec_per),
    ")</span>"
  )]
  
  # Sector bars
  df_pais <- data.table::copy(df)
  df_pais[, bar_type := "pais"]
  df_pais[, y_pos := as.numeric(etiqueta_factor) + 0.14]
  
  # Countries bars
  df_sectores <- data.table::copy(df)
  df_sectores[, bar_type := "sectores"]
  df_sectores[, y_pos := as.numeric(etiqueta_factor) - 0.14]
  
  df_combined <- data.table::rbindlist(
    list(df_pais, df_sectores)
  )
  
  df_combined[, hover_label := paste0(
    "<b>", pais, "</b><br><br>",
    "Contribución país: ", .fmt_pp(rep, dec_per), " p.p.<br>",
    "TVA: ", .fmt_pp(tva, dec_per), "%<br>",
    "Subsectores relevantes: ", sectores, "<br>",
    "Contribución subsectores a mercado: ",
    .fmt_pp(rep_sectores, dec_per), " p.p."
  )]
  
  fig <- plotly::plot_ly()
  
  # Sector bars
  df_pais_plot <- df_combined[bar_type == "pais"]
  
  fig <- fig |>
    plotly::add_bars(
      data          = df_pais_plot,
      y             = ~y_pos,
      x             = ~rep,
      name          = "Contribución pais",
      marker        = list(color = ~color_pais),
      hovertext     = ~hover_label,
      hovertemplate = "%{hovertext}<extra></extra>",
      orientation   = "h",
      width         = 0.66
    )
  
  # Countries bars
  df_sectores_plot <- df_combined[bar_type == "sectores"]
  
  fig <- fig |>
    plotly::add_bars(
      data          = df_sectores_plot,
      y             = ~y_pos,
      x             = ~rep_sectores,
      name          = "Contribución sectores",
      marker        = list(color = ~color_sectores),
      hoverinfo     = "skip",
      orientation   = "h",
      width         = 0.10
    ) |>
    plotly::layout(
      
      title = list(
        text = paste0("<b>", tit, "</b>"),
        x = 0.5,
        font = list(
          size = font_title,
          color = "black",
          family = parametros$fuente_texto
        )
      ),
      
      barmode = "overlay",
      
      xaxis = list(
        title = "",
        automargin = TRUE,
        showgrid = FALSE,
        zeroline = TRUE,
        zerolinecolor = "black",
        zerolinewidth = 1,
        showline = TRUE,
        linecolor = "black",
        linewidth = 1
      ),
      
      yaxis = list(
        title = "",
        tickfont = list(
          size = font_axis,
          family = parametros$fuente_texto
        ),
        tickmode = "array",
        tickvals = seq_len(nrow(df)),
        ticktext = df$etiqueta_completa,
        automargin = TRUE,
        showgrid = FALSE,
        zeroline = FALSE,
        showline = TRUE,
        linecolor = "black",
        linewidth = 1
      ),
      
      showlegend = FALSE,
      
      paper_bgcolor = parametros$colorbf,
      plot_bgcolor  = "rgba(0,0,0,0)",
      
      annotations = lapply(seq_len(nrow(df)), function(i) {
        positivo <- df$rep[i] >= 0
        
        list(
          x         = 0,
          y         = i,
          text      = df$text_con[i],
          
          xanchor   = ifelse(
            positivo,
            "right",
            "left"
          ),
          
          yanchor   = "middle",
          
          showarrow = FALSE,
          
          xref      = "x",
          yref      = "y",
          
          xshift    = ifelse(
            positivo,
            -4,
            4
          ),
          
          align = "center",
          
          font = list(
            color  = "black",
            size   = 9,
            family = parametros$fuente_texto
          )
        )
      })
    )
  
  return(fig)
}


# Bump charts ----

## Auxiliar: emoji bandera desde ISO2 ----
.iso2_flag <- function(iso2) {
  chars <- strsplit(toupper(iso2), "")[[1]]
  paste0(intToUtf8(0x1F1E6 - 65L + utf8ToInt(chars[1])),
         intToUtf8(0x1F1E6 - 65L + utf8ToInt(chars[2])))
}

## Tabla de mapeo nombre_pais → ISO2 ----
.mapa_banderas <- c(
  "Alemania"               = "DE", "Francia"         = "FR",
  "Italia"                 = "IT", "Portugal"        = "PT",
  "Reino Unido"            = "GB", "Estados Unidos"  = "US",
  "China"                  = "CN", "Japón"           = "JP",
  "México"                 = "MX", "Brasil"          = "BR",
  "Países Bajos"           = "NL", "Bélgica"         = "BE",
  "Polonia"                = "PL", "Marruecos"       = "MA",
  "Turquía"                = "TR", "Arabia Saudí"    = "SA",
  "India"                  = "IN", "Corea del Sur"   = "KR",
  "Suecia"                 = "SE", "Suiza"           = "CH",
  "Austria"                = "AT", "Argelia"         = "DZ",
  "Argentina"              = "AR", "Chile"           = "CL",
  "Colombia"               = "CO", "Australia"       = "AU",
  "Canadá"                 = "CA", "Rusia"           = "RU",
  "Emiratos Árabes Unidos" = "AE", "Singapur"        = "SG",
  "Resto del mundo"        = "🌐"
)

.get_flag <- function(nombre_pais) {
  iso <- .mapa_banderas[nombre_pais]
  if (is.na(iso) || nchar(iso) != 2) return("🌐")
  tryCatch(.iso2_flag(iso), error = function(e) "🌐")
}

## .grafica_bump_chart ----
# NOTA: dt debe llegar ya filtrado (sin "Total", sin niveles no deseados).
# La función NO aplica ningún filtro interno.
.grafica_bump_chart <- function(
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
      colorbf      = "#FFFFFF",
      colpal1      = "#2d5532",
      colpal3      = "#a6a6a6"
    )
) {
  varfactor   <- parametros$varfactor
  varud       <- parametros$varud
  dec_num     <- parametros$dec_num
  font        <- parametros$fuente_texto
  colorbf     <- parametros$colorbf
  
  nombre_flujo   <- if (flujo  == "exp") "Exportaciones" else "Importaciones"
  nombre_region  <- if (region == "mad") "Madrid"        else "España"
  nombre_tipo    <- if (tipo   == "paises") "por país/región" else "por sector"
  
  col_label <- if (tipo == "paises") "pais" else "nombre"
  
  # Inferir años disponibles directamente de las columnas del dataset
  prefijo    <- paste0("^", flujo, "_", region, "_([0-9]{4})$")
  cols_match <- grep(prefijo, names(dt), value = TRUE)
  anos       <- sort(as.integer(regmatches(cols_match, regexpr("[0-9]{4}$", cols_match))))
  
  if (length(anos) == 0)
    stop("No se encontraron columnas con el patrón '", flujo, "_", region,
         "_YYYY' en dt. Comprueba los parámetros flujo/region.")
  
  cols_val  <- paste0(flujo, "_", region, "_", anos)
  
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
  
  paleta <- setNames(
    grDevices::hcl.colors(
      n,
      palette = "Dynamic"
    ),
    etiquetas
  )
  
  fig <- plotly::plot_ly()
  
  for (ent in etiquetas) {
    d       <- df_long[label == ent][order(ano)]
    col_ent <- paleta[[ent]]
    
    fig <- plotly::add_trace(
      fig,
      data = d, x = ~ano, y = ~rank,
      type = "scatter", mode = "lines",
      name = ent,
      line = list(color = col_ent, width = 2),
      hoverinfo  = "none",
      showlegend = FALSE
    )
    
    fig <- plotly::add_trace(
      fig,
      data   = d, x = ~ano, y = ~rank,
      type   = "scatter", mode = "markers",
      name   = ent,
      marker = list(color = col_ent, size = 9,
                    line = list(color = "white", width = 1.5)),
      text   = ~paste0(
        "<b>", label, "</b><br>",
        "Año: ", ano, "<br>",
        "Ranking: #", rank, "<br>",
        "Valor: ", valor_fmt, " ", varud
      ),
      hovertemplate = "%{text}<extra></extra>",
      showlegend    = FALSE
    )
    
    d_first <- d[ano == min(anos)]
    d_last  <- d[ano == max(anos)]
    
    fig <- plotly::add_annotations(
      fig,
      x = d_first$ano, y = d_first$rank,
      text      = paste0("<b>", d_first$label, "</b>"),
      xanchor   = "right", yanchor = "middle",
      showarrow = FALSE,
      font      = list(size = 9, color = col_ent, family = font),
      xshift    = -6
    )
    
    if (tipo == "paises") {
      texto_derecha <- paste0(.get_flag(ent), " <b>", d_last$rank, "</b>")
      size_derecha  <- 12
    } else {
      texto_derecha <- paste0("<b>", d_last$rank, "</b>")
      size_derecha  <- 9
    }
    
    fig <- plotly::add_annotations(
      fig,
      x = d_last$ano, y = d_last$rank,
      text      = texto_derecha,
      xanchor   = "left", yanchor = "middle",
      showarrow = FALSE,
      font      = list(size = size_derecha, color = col_ent, family = font),
      xshift    = 8
    )
  }
  
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
      title     = "",
      tickvals  = anos, ticktext = as.character(anos),
      tickfont  = list(family = font, size = 10),
      showgrid  = FALSE, zeroline = FALSE,
      showline  = TRUE, linecolor = "black", linewidth = 1
    ),
    yaxis = list(
      title          = "",
      autorange      = "reversed",
      showticklabels = FALSE,
      showgrid       = TRUE, gridcolor = "rgba(0,0,0,0.07)",
      zeroline       = FALSE, showline  = FALSE
    ),
    showlegend    = FALSE,
    margin        = list(l = 200, r = 80, t = 60, b = 40),
    paper_bgcolor = colorbf,
    plot_bgcolor  = "rgba(0,0,0,0)"
  )
  
  return(fig)
}