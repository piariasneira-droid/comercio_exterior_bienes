# funciones_tabla_anexos.R
#
# Funciones para generar tablas GT con minigráficas de tendencia (sparklines SVG)
# para exportaciones e importaciones, tanto por sectores como por países.
#
# Funciones exportadas (públicas):
#   .exportar_sec_spark_imagen()   — tabla de sectores con sparklines
#   .exportar_pais_spark_imagen()  — tabla de países con sparklines
#
# Helpers privados (no llamar directamente):
#   .prep_tabla_datos()            — filtra, escala y añade .niv / .orden
#   .extraer_sparklines()          — construye listas de spark desde tabla_evol
#   .render_contrib_bars()         — transforma columnas de contribución en HTML
#   .render_spark_col()            — transforma columna spark en SVG inline
#   .build_gt_base()               — construye el objeto gt con estilos comunes
#   .sufijo_mes()                  — genera sufijo de nombre de archivo
#   %||%                           — operador null-coalesce

# Operador null-coalesce ----
`%||%` <- function(a, b) if (!is.null(a)) a else b


# ── Helpers privados ──────────────────────────────────────────────────────────

# .prep_tabla_datos ----
# Filtra filas, selecciona columnas relevantes y escala valores numéricos.
# Añade columnas auxiliares .niv y .orden al data.frame resultante.
#
# Parámetros:
#   tabla         data.frame de sectores o países
#   col_nombre    nombre de la columna stub ("nombre" o "pais")
#   cols_datos    vector de nombres de columnas de datos a conservar
#   cols_millones columnas a dividir entre 1e6
#   cols_pct      columnas a multiplicar por 100
#   cols_contrib  columnas de contribución a multiplicar por 100
#   omitir_orden  vector de valores de 'orden' a excluir (NULL = ninguno)
.prep_tabla_datos <- function(tabla,
                              col_nombre,
                              cols_datos,
                              cols_millones,
                              cols_pct,
                              cols_contrib,
                              omitir_orden = NULL) {
  
  df <- as.data.frame(tabla)
  
  if (!is.null(omitir_orden) && length(omitir_orden) > 0)
    df <- df[!df$orden %in% omitir_orden, ]
  
  # Reordenar por orden ascendente (garantiza consistencia visual)
  df <- df[order(df$orden, decreasing = FALSE), ]
  rownames(df) <- NULL
  
  cols_sel <- c(col_nombre, cols_datos)
  cols_sel <- cols_sel[cols_sel %in% names(df)]
  
  niv_vec   <- df$niv
  orden_vec <- df$orden
  df        <- df[, cols_sel, drop = FALSE]
  
  for (col in names(df)) {
    if      (col %in% cols_millones)                 df[[col]] <- df[[col]] / 1e6
    else if (col %in% c(cols_pct, cols_contrib))     df[[col]] <- df[[col]] * 100
  }
  
  df$.niv   <- niv_vec
  df$.orden <- orden_vec
  
  df
}


# .extraer_sparklines ----
# Extrae las series de índice (base 100) desde tabla_evol y construye listas
# de spark para exp e imp, según territorio y rango de años.
#
# Parámetros:
#   tabla_evol  data.frame con columnas de índice del tipo exp_mad_idx_YYYY
#   territorio  "mad" | "esp"
#   anos        vector de años enteros (p.ej. 2019:2025)
#
# Devuelve un data.frame con columnas: orden, .spark_exp, .spark_imp
.extraer_sparklines <- function(tabla_evol, territorio, anos) {
  
  df_evol <- as.data.frame(data.table::copy(tabla_evol))
  ter      <- territorio
  
  cols_idx_exp <- paste0("exp_", ter, "_idx_", anos)
  cols_idx_imp <- paste0("imp_", ter, "_idx_", anos)
  
  cols_exp_ok <- cols_idx_exp[cols_idx_exp %in% names(df_evol)]
  cols_imp_ok <- cols_idx_imp[cols_idx_imp %in% names(df_evol)]
  
  need <- c("orden", cols_exp_ok, cols_imp_ok)
  need <- need[need %in% names(df_evol)]
  df_evol <- df_evol[, need, drop = FALSE]
  
  df_evol$.spark_exp <- if (length(cols_exp_ok) > 0)
    apply(df_evol[, cols_exp_ok, drop = FALSE], 1, function(r) list(as.numeric(r)))
  else
    vector("list", nrow(df_evol))
  
  df_evol$.spark_imp <- if (length(cols_imp_ok) > 0)
    apply(df_evol[, cols_imp_ok, drop = FALSE], 1, function(r) list(as.numeric(r)))
  else
    vector("list", nrow(df_evol))
  
  df_evol[, c("orden", ".spark_exp", ".spark_imp"), drop = FALSE]
}


# .render_contrib_bars ----
# Transforma columnas de contribución en celdas HTML con micro-barras
# horizontales proporcionales al valor (positivo/negativo).
#
# Parámetros:
#   gt_tbl         objeto gt
#   df             data.frame subyacente (con las columnas a transformar)
#   col_contrib_bar vector de nombres de columnas a procesar (NULL = ninguna)
#   col_pal        lista de colores (bar_pos, bar_neg)
#   tam_fuente     tamaño de fuente en px
#   dec_pct        decimales para el texto del valor
#
# Devuelve el objeto gt modificado
.render_contrib_bars <- function(gt_tbl, df, col_contrib_bar,
                                 col_pal, tam_fuente, dec_pct) {
  
  if (is.null(col_contrib_bar)) return(gt_tbl)
  
  for (col_target in col_contrib_bar) {
    if (!col_target %in% names(df)) next
    
    vals    <- df[[col_target]]
    max_abs <- max(abs(vals), na.rm = TRUE)
    if (max_abs == 0) max_abs <- 1
    
    bar_html <- sapply(vals, function(v) {
      if (is.na(v)) return("")
      pct     <- (abs(v) / max_abs) * 45
      col_bar <- if (v >= 0) col_pal$bar_pos else col_pal$bar_neg
      label   <- formatC(v, format = "f", digits = dec_pct,
                         decimal.mark = ",", big.mark = ".")
      paste0(
        '<div style="display:flex; align-items:center; justify-content:center;',
        ' width:100%; height:100%;">',
        if (v >= 0) {
          paste0(
            '<div style="width:50%;text-align:right;padding-right:3px;',
            'font-size:', tam_fuente, 'px;">', label, '</div>',
            '<div style="width:50%;display:flex;justify-content:flex-start;">',
            '<div style="width:', round(pct, 1), '%;height:10px;background:',
            col_bar, ';"></div></div>'
          )
        } else {
          paste0(
            '<div style="width:50%;display:flex;justify-content:flex-end;">',
            '<div style="width:', round(pct, 1), '%;height:10px;background:',
            col_bar, ';"></div></div>',
            '<div style="width:50%;text-align:left;padding-left:3px;',
            'font-size:', tam_fuente, 'px;">', label, '</div>'
          )
        },
        '</div>'
      )
    })
    
    gt_tbl <- gt_tbl |>
      text_transform(
        locations = cells_body(columns = any_of(col_target)),
        fn = local({
          html_eval <- bar_html
          function(x) html_eval
        })
      )
  }
  
  gt_tbl
}


# .render_spark_col ----
# Transforma una columna de listas de valores en sparklines SVG inline.
#
# Parámetros:
#   gt_tbl         objeto gt (modificado por referencia con <<-)
#   df             data.frame subyacente
#   spark_col_name nombre de la columna (.spark_exp | .spark_imp)
#   col_pal        lista de colores (spark_pos, spark_neg, spark_ref, bar_pos, bar_neg)
#   w_total        ancho del SVG en px
#   h_total        alto del SVG en px
#
# Devuelve el objeto gt modificado
.render_spark_col <- function(gt_tbl, df, spark_col_name,
                              col_pal, w_total = 50L, h_total = 20L) {
  
  if (!spark_col_name %in% names(df)) return(gt_tbl)
  
  pad     <- 3
  w_inner <- w_total - 2 * pad
  h_inner <- h_total - 2 * pad
  
  gt_tbl |>
    text_transform(
      locations = cells_body(columns = any_of(spark_col_name)),
      fn = function(x) {
        lapply(df[[spark_col_name]], function(vals) {
          vals <- unlist(vals)
          if (is.null(vals) || all(is.na(vals))) return("")
          n <- length(vals)
          if (n < 2) return("")
          
          mn  <- min(vals, na.rm = TRUE)
          mx  <- max(vals, na.rm = TRUE)
          rng <- if ((mx - mn) == 0) 1 else mx - mn
          
          xs <- round(pad + (seq_along(vals) - 1) / (n - 1) * w_inner, 2)
          ys <- round(pad + (1 - (vals - mn) / rng) * h_inner, 2)
          
          line_col   <- if (vals[n] >= 100) col_pal$spark_pos %||% col_pal$bar_pos
          else                col_pal$spark_neg %||% col_pal$bar_neg
          points_str <- paste(paste0(xs, ",", ys), collapse = " ")
          last_dot   <- paste0('<circle cx="', xs[n], '" cy="', ys[n],
                               '" r="2" fill="', line_col, '"/>')
          
          y100 <- round(pad + (1 - (100 - mn) / rng) * h_inner, 2)
          ref_line <- if (y100 >= pad && y100 <= (h_total - pad)) {
            paste0('<line x1="', pad, '" y1="', y100,
                   '" x2="', w_total - pad, '" y2="', y100,
                   '" stroke="', col_pal$spark_ref %||% "#AAAAAA",
                   '" stroke-width="0.5" stroke-dasharray="2,2"/>')
          } else ""
          
          paste0(
            '<svg width="', w_total, '" height="', h_total,
            '" xmlns="http://www.w3.org/2000/svg">',
            ref_line,
            '<polyline points="', points_str,
            '" fill="none" stroke="', line_col,
            '" stroke-width="1.8" stroke-linejoin="round"',
            ' stroke-linecap="round"/>',
            last_dot, '</svg>'
          )
        })
      }
    )
}


# .build_gt_base ----
# Construye el objeto gt con todos los estilos compartidos entre sectores y países.
# La lógica específica de cada tipo (niveles 3/4, indentación) se aplica después.
#
# Parámetros:
#   df             data.frame final listo para gt (con .niv y .orden)
#   col_nombre     columna stub
#   cols_exp_ok    columnas de exportación que existen en df
#   cols_imp_ok    columnas de importación que existen en df
#   cols_extra_ok  columnas extra que existen en df
#   cols_millones  columnas en millones (para fmt_number)
#   cols_pct       columnas porcentuales (para fmt_number)
#   cols_contrib   columnas de contribución (para fmt_number)
#   labels_para_gt lista nombrada de etiquetas de columna
#   label_exp      etiqueta del spanner de exportaciones
#   label_imp      etiqueta del spanner de importaciones
#   titulo         título de la tabla
#   subtitulo      subtítulo (puede ser NULL)
#   caption        nota al pie
#   col_pal        lista de colores
#   tam_fuente     tamaño de fuente base en px
#   fuente         familia tipográfica
#   dec_num        decimales para valores en millones
#   dec_pct        decimales para porcentajes
.build_gt_base <- function(df,
                           col_nombre,
                           cols_exp_ok, cols_imp_ok, cols_extra_ok,
                           cols_millones, cols_pct, cols_contrib,
                           labels_para_gt,
                           label_exp, label_imp,
                           titulo, subtitulo, caption,
                           col_pal, tam_fuente, fuente,
                           dec_num, dec_pct) {
  
  stub_label <- if (col_nombre == "nombre") "Sector econ\u00f3mico"
  else                        "Pa\u00eds / Zona geogr\u00e1fica"
  
  df |>
    gt(rowname_col = col_nombre) |>
    cols_hide(columns = any_of(c(".niv", ".orden"))) |>
    tab_stubhead(label = stub_label) |>
    tab_header(title = titulo, subtitle = subtitulo) |>
    tab_source_note(source_note = caption) |>
    
    tab_spanner(label = label_exp,
                columns = any_of(c(cols_exp_ok, ".spark_exp"))) |>
    tab_spanner(label = label_imp,
                columns = any_of(c(cols_imp_ok, ".spark_imp"))) |>
    
    cols_label(.list = labels_para_gt) |>
    
    # Formato numérico: valores en millones
    fmt_number(
      columns  = any_of(c(
        cols_exp_ok[cols_exp_ok %in% cols_millones],
        cols_imp_ok[cols_imp_ok %in% cols_millones],
        cols_extra_ok[cols_extra_ok %in% cols_millones]
      )),
      decimals = dec_num, sep_mark = ".", dec_mark = ","
    ) |>
    # Formato numérico: porcentajes y contribuciones
    fmt_number(
      columns  = any_of(c(
        cols_exp_ok[cols_exp_ok %in% c(cols_pct, cols_contrib)],
        cols_imp_ok[cols_imp_ok %in% c(cols_pct, cols_contrib)],
        cols_extra_ok[cols_extra_ok %in% c(cols_pct, cols_contrib)]
      )),
      decimals = dec_pct, sep_mark = ".", dec_mark = ","
    ) |>
    
    cols_align(align = "center", columns = everything()) |>
    cols_align(align = "left",   columns = stub()) |>
    
    # Opciones globales de tabla
    tab_options(
      table.font.names                 = fuente,
      table.font.size                  = px(tam_fuente),
      heading.background.color         = col_pal$heading_bg,
      column_labels.background.color   = col_pal$labels_bg,
      table.border.top.style           = "none",
      table.border.bottom.style        = "none",
      table.border.left.style          = "none",
      table.border.right.style         = "none",
      table.margin.left                = px(0),
      table.margin.right               = px(0),
      heading.padding                  = px(2),
      heading.padding.horizontal       = px(4),
      column_labels.padding            = px(2),
      column_labels.padding.horizontal = px(4),
      source_notes.padding             = px(2),
      source_notes.padding.horizontal  = px(4),
      data_row.padding                 = px(1),
      data_row.padding.horizontal      = px(4),
      stub.indent_length               = px(6)
    ) |>
    
    # Estilos de cabecera y fuente de datos
    tab_style(
      style     = cell_text(color  = col_pal$heading_fg, weight = "bold",
                            size   = px(tam_fuente + 5)),
      locations = cells_title(groups = "title")
    ) |>
    tab_style(
      style = list(
        cell_fill(color = col_pal$niv3_bg),
        cell_text(color = col_pal$niv3_fg, size = px(tam_fuente + 2),
                  style = "italic", align = "left")
      ),
      locations = cells_title(groups = "subtitle")
    ) |>
    tab_style(
      style = list(
        cell_fill(color = col_pal$niv3_bg),
        cell_text(color = col_pal$niv3_fg, size = px(tam_fuente), align = "left")
      ),
      locations = cells_source_notes()
    ) |>
    tab_style(
      style = list(
        cell_fill(color = col_pal$labels_bg),
        cell_text(color = col_pal$labels_fg, weight = "bold",
                  v_align = "middle", align = "center")
      ),
      locations = list(cells_column_labels(), cells_column_spanners(),
                       cells_stubhead())
    ) |>
    
    # Bordes de separación entre bloques exp / imp / extra
    tab_style(
      style = cell_borders(sides = "right", color = col_pal$border, weight = px(1)),
      locations = list(
        cells_stub(), cells_stubhead(),
        cells_body(columns         = any_of(".spark_exp")),
        cells_column_labels(columns = any_of(".spark_exp")),
        cells_body(columns         = any_of(".spark_imp")),
        cells_column_labels(columns = any_of(".spark_imp"))
      )
    ) |>
    
    # Estilos por nivel (niv 0, 1/9, 2)
    tab_style(
      style     = list(cell_fill(color = col_pal$niv0_bg),
                       cell_text(weight = "bold", color = col_pal$niv0_fg,
                                 v_align = "middle")),
      locations = list(cells_body(rows = .niv == 0),
                       cells_stub(rows = .niv == 0))
    ) |>
    tab_style(
      style     = list(cell_fill(color = col_pal$niv1_bg),
                       cell_text(weight = "bold", color = col_pal$niv1_fg,
                                 v_align = "middle")),
      locations = list(cells_body(rows = .niv %in% c(1, 9)),
                       cells_stub(rows = .niv %in% c(1, 9)))
    ) |>
    tab_style(
      style     = list(cell_fill(color = col_pal$niv2_bg),
                       cell_text(color = col_pal$niv2_fg, v_align = "middle")),
      locations = list(cells_body(rows = .niv == 2),
                       cells_stub(rows = .niv == 2))
    ) |>
    
    # Padding lateral del stub
    tab_style(
      style     = css("padding-left" = "4px", "padding-right" = "4px"),
      locations = list(cells_stub(), cells_stubhead())
    )
}


# .sufijo_mes ----
# Construye el sufijo de nombre de archivo a partir de paramets.
# Usa paramets$mes (escalar o vector) y paramets$anho.
#
# Ejemplos:
#   mes = 3  → "2026_03"
#   mes = 1:3 → "2026_01-03"
.sufijo_mes <- function(para) {
  m_start <- min(para$mes)
  m_end   <- max(para$mes)
  if (m_start == m_end) {
    sprintf("%d_%02d",      para$anho, m_start)
  } else {
    sprintf("%d_%02d-%02d", para$anho, m_start, m_end)
  }
}


# ── Funciones principales ─────────────────────────────────────────────────────

# .exportar_sec_spark_imagen ----
#
# Genera una imagen PNG con la tabla de sectores combinada con sparklines SVG
# de tendencia (índice base 100) de exportaciones e importaciones.
#
# Estructura de columnas resultante:
#   stub | [cols_exp] + .spark_exp | [cols_imp] + .spark_imp | [cols_extra]
#
# Parámetros principales:
#   tabla_sec       data.frame de sectores (df_sectores o df_sectores_acu)
#   tabla_evol      data.frame de evolución anual (df_evol_sec o df_evol_sec_acu)
#   territorio      "mad" | "esp"  — determina los prefijos en tabla_evol
#   anos_spark      vector de años para el sparkline (ej. 2019:2025)
#   cols_exp        columnas de exportación a mostrar
#   cols_imp        columnas de importación a mostrar
#   cols_extra      columnas extra (saldo, tasa de cobertura, etc.)
#   omitir_orden    vector de valores de orden a excluir (NULL = ninguno)
#   col_contrib_bar columnas que se transforman en micro-barras HTML
#   cols_millones   columnas a escalar a millones de euros
#   cols_pct        columnas porcentuales
#   cols_contrib    columnas de contribución (pp)
#   header_cols     named char vector para sobreescribir etiquetas de columna
#   titulo          título de la tabla
#   subtitulo       subtítulo (puede ser NULL; usar .per_label() de funciones_text.r)
#   caption         nota al pie (ej. paramets$caption)
#   ruta_salida     ruta del fichero PNG de salida
#   ancho_cm        ancho de la tabla en cm (controla vwidth del screenshot)
#   ancho_px        ancho final en píxeles (si no NULL, reescala con magick)
#   alto_px         alto final en píxeles (si no NULL, reescala con magick)
#   tam_fuente      tamaño base de fuente en px
#   fuente          familia tipográfica (ej. paramets$fuente_texto)
#   dec_num         decimales para valores en millones
#   dec_pct         decimales para porcentajes y contribuciones
#   dpi             resolución de salida
#   col_pal         lista de colores (ej. paramets$gt_col_pal)
.exportar_sec_spark_imagen <- function(
    tabla_sec,
    tabla_evol,
    territorio       = "mad",
    anos_spark       = 2019L:2025L,
    cols_exp         = c("exp_mad", "exp_mad_pct", "exp_mad_tva",
                         "exp_mad_contrib", "exp_mad_vs_esp"),
    cols_imp         = c("imp_mad", "imp_mad_pct", "imp_mad_tva",
                         "imp_mad_contrib", "imp_mad_vs_esp"),
    cols_extra       = c("saldo_mad", "tasa_cob_mad"),
    omitir_orden     = NULL,
    label_exp        = "Exportaciones",
    label_imp        = "Importaciones",
    label_extra      = "Saldo",
    col_contrib_bar  = NULL,
    cols_millones    = c("exp_mad", "imp_mad", "saldo_mad",
                         "exp_esp", "imp_esp", "saldo_esp"),
    cols_pct         = c("exp_mad_pct", "imp_mad_pct", "exp_mad_tva",
                         "imp_mad_tva", "exp_mad_vs_esp", "imp_mad_vs_esp",
                         "tasa_cob_mad", "exp_esp_pct", "imp_esp_pct",
                         "exp_esp_tva", "imp_esp_tva", "tasa_cob_esp"),
    cols_contrib     = c("exp_mad_contrib", "imp_mad_contrib",
                         "exp_esp_contrib", "imp_esp_contrib"),
    header_cols      = NULL,
    titulo           = "Comercio exterior por sectores",
    subtitulo        = NULL,
    caption          = "Elaboraci\u00f3n propia a partir de microdatos de DataComex.",
    ruta_salida      = "./tabla_sec_spark.png",
    ancho_cm         = 18L,
    ancho_px         = NULL,
    alto_px          = NULL,
    tam_fuente       = 7L,
    fuente           = "Arial",
    dec_num          = 1L,
    dec_pct          = 1L,
    dpi              = 300L,
    col_pal          = list(
      heading_bg = "#526DB0", heading_fg = "#F5C201",
      labels_bg  = "#F5C201", labels_fg  = "black",
      border     = "#AAAAAA",
      niv0_bg    = "#F5C201", niv0_fg    = "black",
      niv1_bg    = "#B9C4DF", niv1_fg    = "black",
      niv2_bg    = "#F2F2F2", niv2_fg    = "black",
      niv3_bg    = "#FFFAE5", niv3_fg    = "black",
      niv4_bg    = "#FFFAE5", niv4_fg    = "#333333",
      bar_pos    = "#2E7D5E", bar_neg    = "#C0392B",
      spark_pos  = "#2E7D5E", spark_neg  = "#C0392B",
      spark_ref  = "#AAAAAA"
    )
) {
  
  ter  <- territorio
  anos <- sort(anos_spark)
  ab   <- as.character(min(anos))
  af   <- as.character(max(anos))
  
  # ── 1. Preparar tabla de datos ────────────────────────────────────────────────
  cols_datos <- unique(c(cols_exp, cols_imp, cols_extra))
  
  df <- .prep_tabla_datos(
    tabla        = tabla_sec,
    col_nombre   = "nombre",
    cols_datos   = cols_datos,
    cols_millones = cols_millones,
    cols_pct     = cols_pct,
    cols_contrib = cols_contrib,
    omitir_orden = omitir_orden
  )
  orden_vec <- df$.orden
  
  # ── 2. Extraer sparklines ─────────────────────────────────────────────────────
  df_spark <- .extraer_sparklines(tabla_evol, ter, anos)
  
  # ── 3. Join y reordenar columnas ──────────────────────────────────────────────
  df <- merge(df, df_spark, by.x = ".orden", by.y = "orden", all.x = TRUE)
  df <- df[match(orden_vec, df$.orden), ]
  rownames(df) <- seq_len(nrow(df))
  
  cols_exp_ok   <- cols_exp[cols_exp %in% names(df)]
  cols_imp_ok   <- cols_imp[cols_imp %in% names(df)]
  cols_extra_ok <- cols_extra[cols_extra %in% names(df)]
  
  col_order <- c("nombre",
                 cols_exp_ok, ".spark_exp",
                 cols_imp_ok, ".spark_imp",
                 cols_extra_ok,
                 ".niv", ".orden")
  df <- df[, col_order[col_order %in% names(df)], drop = FALSE]
  
  # ── 4. Etiquetas de columna ───────────────────────────────────────────────────
  etiqueta_spark <- paste0(ab, " \u2192 ", af)
  
  labels_map <- c(
    nombre         = "Sector econ\u00f3mico",
    exp_mad        = "Mill. \u20ac", exp_mad_pct     = "% s/total",
    exp_mad_tva    = "TVA (%)",      exp_mad_contrib = "Con. (pp)",
    exp_mad_vs_esp = "% s/Esp.",
    imp_mad        = "Mill. \u20ac", imp_mad_pct     = "% s/total",
    imp_mad_tva    = "TVA (%)",      imp_mad_contrib = "Con. (pp)",
    imp_mad_vs_esp = "% s/Esp.",
    exp_esp        = "Mill. \u20ac", exp_esp_pct     = "% s/total",
    exp_esp_tva    = "TVA (%)",      exp_esp_contrib = "Con. (pp)",
    imp_esp        = "Mill. \u20ac", imp_esp_pct     = "% s/total",
    imp_esp_tva    = "TVA (%)",      imp_esp_contrib = "Con. (pp)",
    saldo_mad      = "Saldo (M\u20ac)", tasa_cob_mad = "T. cob. (%)",
    saldo_esp      = "Saldo (M\u20ac)", tasa_cob_esp = "T. cob. (%)",
    .spark_exp     = etiqueta_spark,
    .spark_imp     = etiqueta_spark
  )
  if (!is.null(header_cols)) labels_map[names(header_cols)] <- header_cols
  
  cols_para_label <- names(df)[!names(df) %in% c(".niv", ".orden")]
  labels_para_gt  <- as.list(labels_map[cols_para_label])
  labels_para_gt  <- labels_para_gt[!sapply(labels_para_gt, is.null)]
  
  # ── 5. Construir gt base ──────────────────────────────────────────────────────
  gt_tbl <- .build_gt_base(
    df            = df,
    col_nombre    = "nombre",
    cols_exp_ok   = cols_exp_ok,
    cols_imp_ok   = cols_imp_ok,
    cols_extra_ok = cols_extra_ok,
    cols_millones = cols_millones,
    cols_pct      = cols_pct,
    cols_contrib  = cols_contrib,
    labels_para_gt = labels_para_gt,
    label_exp     = label_exp,
    label_imp     = label_imp,
    titulo        = titulo,
    subtitulo     = subtitulo,
    caption       = caption,
    col_pal       = col_pal,
    tam_fuente    = tam_fuente,
    fuente        = fuente,
    dec_num       = dec_num,
    dec_pct       = dec_pct
  )
  
  # ── 6. Estilos de nivel específicos para sectores (niv 3) ────────────────────
  gt_tbl <- gt_tbl |>
    tab_style(
      style     = list(cell_fill(color = col_pal$niv3_bg),
                       cell_text(color = col_pal$niv3_fg, v_align = "middle")),
      locations = list(cells_body(rows = .niv == 3),
                       cells_stub(rows = .niv == 3))
    ) |>
    # Indentación del stub por nivel
    text_transform(
      locations = cells_stub(),
      fn = function(x) {
        niveles <- df$.niv
        sapply(seq_along(x), function(i) {
          indent <- switch(as.character(niveles[i]),
                           "2" = "&nbsp;&nbsp;&nbsp;",
                           "3" = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                           "4" = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                           "")
          paste0(indent, x[i])
        })
      }
    )
  
  # ── 7. Barras de contribución ─────────────────────────────────────────────────
  gt_tbl <- .render_contrib_bars(gt_tbl, df, col_contrib_bar,
                                 col_pal, tam_fuente, dec_pct)
  
  # ── 8. Sparklines SVG ─────────────────────────────────────────────────────────
  gt_tbl <- .render_spark_col(gt_tbl, df, ".spark_exp", col_pal)
  gt_tbl <- .render_spark_col(gt_tbl, df, ".spark_imp", col_pal)
  
  # ── 9. Guardar imagen ─────────────────────────────────────────────────────────
  gtsave(gt_tbl, filename = ruta_salida,
         vwidth = round(ancho_cm / 2.54 * dpi), zoom = dpi / 96)
  
  if (!is.null(ancho_px) && !is.null(alto_px)) {
    magick::image_read(ruta_salida) |>
      magick::image_resize(paste0(ancho_px, "x", alto_px, "!")) |>
      magick::image_write(ruta_salida, density = dpi)
  }
  
  invisible(gt_tbl)
}


# .exportar_pais_spark_imagen ----
#
# Versión para países de .exportar_sec_spark_imagen.
# Diferencias respecto a la función de sectores:
#   · col_nombre  → "pais"
#   · stub label  → "País / Zona geográfica"
#   · Nivel 3     → cursiva
#   · Nivel 4     → cursiva + color niv4_fg (países individuales)
#   · Defaults de cols_exp/imp sin _vs_esp (países no tienen esa columna)
#   · cols_extra incluye saldo y tasa de cobertura por defecto
#
# El resto de parámetros son idénticos a .exportar_sec_spark_imagen.
.exportar_pais_spark_imagen <- function(
    tabla_sec,
    tabla_evol,
    territorio       = "mad",
    anos_spark       = 2019L:2025L,
    cols_exp         = c("exp_mad", "exp_mad_pct", "exp_mad_tva",
                         "exp_mad_contrib"),
    cols_imp         = c("imp_mad", "imp_mad_pct", "imp_mad_tva",
                         "imp_mad_contrib"),
    cols_extra       = c("saldo_mad", "tasa_cob_mad"),
    omitir_orden     = NULL,
    label_exp        = "Exportaciones",
    label_imp        = "Importaciones",
    label_extra      = "Saldo",
    col_contrib_bar  = NULL,
    cols_millones    = c("exp_mad", "imp_mad", "saldo_mad",
                         "exp_esp", "imp_esp", "saldo_esp"),
    cols_pct         = c("exp_mad_pct", "imp_mad_pct", "exp_mad_tva",
                         "imp_mad_tva", "exp_mad_vs_esp", "imp_mad_vs_esp",
                         "tasa_cob_mad", "exp_esp_pct", "imp_esp_pct",
                         "exp_esp_tva", "imp_esp_tva", "tasa_cob_esp"),
    cols_contrib     = c("exp_mad_contrib", "imp_mad_contrib",
                         "exp_esp_contrib", "imp_esp_contrib"),
    header_cols      = NULL,
    titulo           = "Comercio exterior por pa\u00edses",
    subtitulo        = NULL,
    caption          = "Elaboraci\u00f3n propia a partir de microdatos de DataComex.",
    ruta_salida      = "./tabla_pais_spark.png",
    ancho_cm         = 18L,
    ancho_px         = NULL,
    alto_px          = NULL,
    tam_fuente       = 7L,
    fuente           = "Arial",
    dec_num          = 1L,
    dec_pct          = 1L,
    dpi              = 300L,
    col_pal          = list(
      heading_bg = "#526DB0", heading_fg = "#F5C201",
      labels_bg  = "#F5C201", labels_fg  = "black",
      border     = "#AAAAAA",
      niv0_bg    = "#F5C201", niv0_fg    = "black",
      niv1_bg    = "#B9C4DF", niv1_fg    = "black",
      niv2_bg    = "#F2F2F2", niv2_fg    = "black",
      niv3_bg    = "#FFFFFF", niv3_fg    = "black",
      niv4_bg    = "#FFFFFF", niv4_fg    = "#333333",
      bar_pos    = "#2E7D5E", bar_neg    = "#C0392B",
      spark_pos  = "#2E7D5E", spark_neg  = "#C0392B",
      spark_ref  = "#AAAAAA"
    )
) {
  
  ter  <- territorio
  anos <- sort(anos_spark)
  ab   <- as.character(min(anos))
  af   <- as.character(max(anos))
  
  # ── 1. Preparar tabla de datos ────────────────────────────────────────────────
  cols_datos <- unique(c(cols_exp, cols_imp, cols_extra))
  
  df <- .prep_tabla_datos(
    tabla         = tabla_sec,
    col_nombre    = "pais",
    cols_datos    = cols_datos,
    cols_millones = cols_millones,
    cols_pct      = cols_pct,
    cols_contrib  = cols_contrib,
    omitir_orden  = omitir_orden
  )
  orden_vec <- df$.orden
  
  # ── 2. Extraer sparklines ─────────────────────────────────────────────────────
  df_spark <- .extraer_sparklines(tabla_evol, ter, anos)
  
  # ── 3. Join y reordenar columnas ──────────────────────────────────────────────
  df <- merge(df, df_spark, by.x = ".orden", by.y = "orden", all.x = TRUE)
  df <- df[order(df$.orden, decreasing = FALSE), ]
  rownames(df) <- seq_len(nrow(df))
  
  cols_exp_ok   <- cols_exp[cols_exp %in% names(df)]
  cols_imp_ok   <- cols_imp[cols_imp %in% names(df)]
  cols_extra_ok <- cols_extra[cols_extra %in% names(df)]
  
  col_order <- c("pais",
                 cols_exp_ok, ".spark_exp",
                 cols_imp_ok, ".spark_imp",
                 cols_extra_ok,
                 ".niv", ".orden")
  df <- df[, col_order[col_order %in% names(df)], drop = FALSE]
  
  # ── 4. Etiquetas de columna ───────────────────────────────────────────────────
  etiqueta_spark <- paste0(ab, " \u2192 ", af)
  
  labels_map <- c(
    pais           = "Pa\u00eds / Zona geogr\u00e1fica",
    exp_mad        = "Mill. \u20ac", exp_mad_pct     = "% s/total",
    exp_mad_tva    = "TVA (%)",      exp_mad_contrib = "Con. (pp)",
    exp_mad_vs_esp = "% s/Esp.",
    imp_mad        = "Mill. \u20ac", imp_mad_pct     = "% s/total",
    imp_mad_tva    = "TVA (%)",      imp_mad_contrib = "Con. (pp)",
    imp_mad_vs_esp = "% s/Esp.",
    exp_esp        = "Mill. \u20ac", exp_esp_pct     = "% s/total",
    exp_esp_tva    = "TVA (%)",      exp_esp_contrib = "Con. (pp)",
    imp_esp        = "Mill. \u20ac", imp_esp_pct     = "% s/total",
    imp_esp_tva    = "TVA (%)",      imp_esp_contrib = "Con. (pp)",
    saldo_mad      = "Saldo (M\u20ac)", tasa_cob_mad = "T. cob. (%)",
    saldo_esp      = "Saldo (M\u20ac)", tasa_cob_esp = "T. cob. (%)",
    .spark_exp     = etiqueta_spark,
    .spark_imp     = etiqueta_spark
  )
  if (!is.null(header_cols)) labels_map[names(header_cols)] <- header_cols
  
  cols_para_label <- names(df)[!names(df) %in% c(".niv", ".orden")]
  labels_para_gt  <- as.list(labels_map[cols_para_label])
  labels_para_gt  <- labels_para_gt[!sapply(labels_para_gt, is.null)]
  
  # ── 5. Construir gt base ──────────────────────────────────────────────────────
  gt_tbl <- .build_gt_base(
    df            = df,
    col_nombre    = "pais",
    cols_exp_ok   = cols_exp_ok,
    cols_imp_ok   = cols_imp_ok,
    cols_extra_ok = cols_extra_ok,
    cols_millones = cols_millones,
    cols_pct      = cols_pct,
    cols_contrib  = cols_contrib,
    labels_para_gt = labels_para_gt,
    label_exp     = label_exp,
    label_imp     = label_imp,
    titulo        = titulo,
    subtitulo     = subtitulo,
    caption       = caption,
    col_pal       = col_pal,
    tam_fuente    = tam_fuente,
    fuente        = fuente,
    dec_num       = dec_num,
    dec_pct       = dec_pct
  )
  
  # ── 6. Estilos de nivel específicos para países (niv 3 cursiva, niv 4) ────────
  gt_tbl <- gt_tbl |>
    tab_style(
      style     = list(cell_fill(color = col_pal$niv3_bg),
                       cell_text(color = col_pal$niv3_fg, style = "italic",
                                 v_align = "middle")),
      locations = list(cells_body(rows = .niv == 3),
                       cells_stub(rows = .niv == 3))
    ) |>
    tab_style(
      style     = list(cell_fill(color = col_pal$niv4_bg),
                       cell_text(color = col_pal$niv4_fg, style = "italic",
                                 size  = px(tam_fuente), v_align = "middle")),
      locations = list(cells_body(rows = .niv == 4),
                       cells_stub(rows = .niv == 4))
    ) |>
    # Indentación del stub por nivel
    text_transform(
      locations = cells_stub(),
      fn = function(x) {
        niveles <- df$.niv
        sapply(seq_along(x), function(i) {
          indent <- switch(as.character(niveles[i]),
                           "2" = "&nbsp;&nbsp;&nbsp;",
                           "3" = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                           "4" = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                           "")
          paste0(indent, x[i])
        })
      }
    )
  
  # ── 7. Barras de contribución ─────────────────────────────────────────────────
  gt_tbl <- .render_contrib_bars(gt_tbl, df, col_contrib_bar,
                                 col_pal, tam_fuente, dec_pct)
  
  # ── 8. Sparklines SVG (más pequeños para países: más filas) ──────────────────
  gt_tbl <- .render_spark_col(gt_tbl, df, ".spark_exp", col_pal,
                              w_total = 40L, h_total = 16L)
  gt_tbl <- .render_spark_col(gt_tbl, df, ".spark_imp", col_pal,
                              w_total = 40L, h_total = 16L)
  
  # ── 9. Guardar imagen ─────────────────────────────────────────────────────────
  gtsave(gt_tbl, filename = ruta_salida,
         vwidth = round(ancho_cm / 2.54 * dpi), zoom = dpi / 96)
  
  if (!is.null(ancho_px) && !is.null(alto_px)) {
    magick::image_read(ruta_salida) |>
      magick::image_resize(paste0(ancho_px, "x", alto_px, "!")) |>
      magick::image_write(ruta_salida, density = dpi)
  }
  
  invisible(gt_tbl)
}