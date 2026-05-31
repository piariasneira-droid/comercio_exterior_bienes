# Auxiliar ----
.fmt_num_inf <- function(x, dec = 1L) {
  formatC(round(x, dec), format = "f", digits = dec,
          big.mark = ".", decimal.mark = ",")
}

.fmt_num <- function(x, varfactor, dec) {
  formatC(round(x / varfactor, dec), format = "f", digits = dec,
          big.mark = ".", decimal.mark = ",")
}

.fmt_per <- function(x, dec) {
  formatC(round(x * 100, dec), format = "f", digits = dec,
          big.mark = ".", decimal.mark = ",")
}

.fmt_pp <- function(x, dec) {
  formatC(round(x, dec), format = "f", digits = dec,
          big.mark = ".", decimal.mark = ",")
}


# Treemaps ----

## Ggplot ----
grafica_treemap_ggplot <- function(dt,
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
                                     colorbf       = "#FFF4CA80"
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
  
  niveles_presentes <- sort(unique(df$niv))
  
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
    .fmt_num(valor, varfactor, dec_num), " ", varud, " (", .fmt_per(contrib, dec_per), " p.p.)"
  )]
  
  flujo_text      <- ifelse(flujo == "exp", "Exportaciones", "Importaciones")
  territorio_text <- ifelse(territorio == "mad", "Madrid", "España")
  tipo_text       <- ifelse(tipo == "sectores", "por Sector", "por País/Región")
  
  if (is.null(titulo)) {
    titulo <- paste0(flujo_text, " ", territorio_text, " ", tipo_text)
  }
  
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
      ),
      guide    = guide_colorbar(
        title.position = "top",
        title.hjust    = 0.5,
        barwidth       = unit(1, "npc"),
        barheight      = unit(0.3, "cm"),
        ticks          = TRUE
      )
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
grafica_treemap_plotly <- function(dt,
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
                                     colorbf       = "rgba(255,244,202,0.5)"
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
  df[, contrib_pp := get(var_con) * 100]
  
  df[, text_label := paste0(
    get(col_label_fin), "<br><br>",
    .fmt_num(get(var_val), varfactor, dec_num), " ", varud,
    " (", .fmt_per(get(var_pct), dec_per), "%)<br><br>",
    "[", .fmt_pp(contrib_pp, dec_per), " p.p.]"
  )]
  df[orden == orden_raiz, text_label := get(col_label_fin)]
  
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
    text         = ~text_label,
    textinfo     = "text",
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

## grafica_volumen_sectores_com ----
grafica_volumen_sectores_com <- function(dt,
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
                                           colorbf       = "rgba(255,244,202,0.5)"
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
  
  df_filtrado <- df[order(-get(col_actual))][1:min(nmax, .N)]
  df_filtrado <- df_filtrado[order(-get(col_actual))]
  
  df_filtrado[, val_actual_sc := get(col_actual) / 1e9]
  df_filtrado[, val_previo_sc := get(col_prev)   / 1e9]
  df_filtrado[, etiqueta        := substr(nombre, 1L, 25L)]
  df_filtrado[, etiqueta_factor := factor(etiqueta, levels = unique(etiqueta))]
  
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
      customdata    = ~paste0("Subsector: ", nombre, "<br>", nombre_flujo, " ", anho, ": ", .fmt_num_inf(val_actual_sc, dec_num), "<br>TVA: ", .fmt_num_inf(get(col_tva)*100, dec_per), "%"),
      hovertemplate = "%{customdata}<extra></extra>"
    ) |>
    plotly::add_bars(
      data          = df_filtrado,
      x             = ~etiqueta_factor,
      y             = ~val_previo_sc,
      name          = as.character(anho - 1L),
      marker        = list(color = paleta["col2"]),
      customdata    = ~paste0(anho-1, ": ", .fmt_num_inf(val_previo_sc, dec_num)),
      hovertemplate = "%{customdata}<extra></extra>"
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

## grafica_volumen_paises_com ----
grafica_volumen_paises_com <- function(dt,
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
                                         colorbf       = "rgba(255,244,202,0.5)"
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
  
  df_filtrado <- df[order(-get(col_actual))][1:min(nmax, .N)]
  df_filtrado <- df_filtrado[order(-get(col_actual))]
  
  df_filtrado[, val_actual_sc := get(col_actual) / 1e9]
  df_filtrado[, val_previo_sc := get(col_prev)   / 1e9]
  df_filtrado[, pais_factor    := factor(pais, levels = unique(pais))]
  
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
      marker = list(color = paleta["col1"])
    ) |>
    plotly::add_bars(
      data   = df_filtrado,
      x      = ~pais_factor,
      y      = ~val_previo_sc,
      name   = as.character(anho - 1L),
      marker = list(color = paleta["col2"])
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

## grafica_contribuciones_sectores_com ----
grafica_contribuciones_sectores_com <- function(dt,
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
                                                  colorbf       = "rgba(255,244,202,0.5)"
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
  
  df_pos <- df[contrib_pp >  0][order(-contrib_pp)][1:min(nmax, .N)]
  df_neg <- df[contrib_pp <= 0][order( contrib_pp)][1:min(nmax, .N)]
  
  df_plot <- data.table::rbindlist(list(df_pos, df_neg))
  df_plot <- df_plot[order(contrib_pp)]
  df_plot[, nombre_factor := factor(nombre, levels = unique(nombre))]
  df_plot[, color    := ifelse(contrib_pp > 0, paleta["positivo"], paleta["negativo"])]
  df_plot[, text_con := .fmt_num_inf(contrib_pp, dec_per)]
  
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
      hoverinfo    = "none",
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

## grafica_contribuciones_paises_com ----
grafica_contribuciones_paises_com <- function(dt,
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
                                                colorbf       = "rgba(255,244,202,0.5)"
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
      hoverinfo    = "none",
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

## grafica_bump_chart ----
# NOTA: dt debe llegar ya filtrado (sin "Total", sin niveles no deseados).
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
  
  nombre_flujo   <- if (flujo  == "exp") "Exportaciones" else "Importaciones"
  nombre_region  <- if (region == "mad") "Madrid"        else "España"
  nombre_tipo    <- if (tipo   == "paises") "por país/región" else "por sector"
  
  cols_val  <- paste0(flujo, "_", region, "_", anos)
  col_label <- if (tipo == "paises") "pais" else "nombre"
  
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