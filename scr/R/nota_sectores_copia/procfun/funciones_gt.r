# funciones_gtb.R
# Funciones de exportación gt y Excel para tablas de comercio exterior
#   · exportar_sectores_imagen  — tabla sectores → PNG
#   · exportar_sectores_excel   — tabla sectores → XLSX
#   · exportar_paises_imagen    — tabla países   → PNG  (niveles hasta 4, cursiva en niv 3-4)
#   · exportar_paises_excel     — tabla países   → XLSX (niveles hasta 4, cursiva en niv 3-4)


# ── 1. SECTORES: EXPORTACIÓN A IMAGEN (GT) ────────────────────────────────────
exportar_sectores_imagen <- function(
    tabla,
    cols_exp         = c("exp_mad", "exp_mad_pct", "exp_mad_tva", "exp_mad_contrib"),
    cols_imp         = c("imp_mad", "imp_mad_pct", "imp_mad_tva", "imp_mad_contrib"),
    cols_extra       = c("saldo_mad", "tasa_cob_mad"),
    omitir_orden     = NULL,
    label_exp        = "Exportaciones",
    label_imp        = "Importaciones",
    label_extra      = "Saldo",
    col_contrib_bar  = NULL, 
    cols_millones  = c("exp_mad", "imp_mad", "saldo_mad", "saldo_mad_prev",
                       "exp_esp", "imp_esp", "saldo_esp", "saldo_esp_prev"),
    cols_pct       = c("exp_mad_pct", "imp_mad_pct", "exp_mad_tva", "imp_mad_tva",
                       "exp_mad_vs_esp", "imp_mad_vs_esp", "tasa_cob_mad",
                       "exp_esp_pct", "imp_esp_pct", "exp_esp_tva", "imp_esp_tva",
                       "tasa_cob_esp"),
    cols_contrib   = c("exp_mad_contrib", "imp_mad_contrib",
                       "exp_esp_contrib", "imp_esp_contrib"),
    header_cols    = NULL,
    titulo         = "Comercio exterior por sectores",
    subtitulo      = NULL,
    caption        = "Elaboración propia a partir de microdatos",
    ruta_salida    = "./tabla_sectores.png",
    ancho_cm       = 25,
    alto_cm        = NULL,
    tam_fuente     = 7,
    fuente         = "Arial",
    dec_num        = 1L,
    dec_pct        = 1L,
    dpi            = 300,
    col_pal        = list(
      heading_bg = "#526DB0", heading_fg = "#F5C201",
      labels_bg  = "#F5C201", labels_fg  = "black",
      border     = "#AAAAAA",
      niv0_bg    = "#F5C201", niv0_fg    = "black",
      niv1_bg    = "#B9C4DF", niv1_fg    = "black",
      niv2_bg    = "#F2F2F2", niv2_fg    = "black",
      bar_pos    = "#2E7D5E", bar_neg    = "#C0392B"
    )
) {
  
  df <- as.data.frame(tabla)
  if (!is.null(omitir_orden) && length(omitir_orden) > 0) df <- df[!df$orden %in% omitir_orden, ]
  
  col_nombre <- "nombre"
  cols_datos <- c(cols_exp, cols_imp, cols_extra)
  cols_datos <- cols_datos[lengths(list(cols_datos)) > 0]
  cols_sel   <- c(col_nombre, cols_datos)
  cols_sel   <- cols_sel[cols_sel %in% names(df)]
  
  niv_vector <- df$niv
  df         <- df[, cols_sel, drop = FALSE]
  
  for (col in names(df)) {
    if (col %in% cols_millones) df[[col]] <- df[[col]] / 1e6
    else if (col %in% c(cols_pct, cols_contrib)) df[[col]] <- df[[col]] * 100
  }
  df$.niv <- niv_vector
  
  labels_map <- c(
    nombre = "Sector económico",
    exp_mad = "Mill. €", exp_mad_pct = "% s/total", exp_mad_tva = "TVA (%)", exp_mad_contrib = "Con. (pp)",
    imp_mad = "Mill. €", imp_mad_pct = "% s/total", imp_mad_tva = "TVA (%)", imp_mad_contrib = "Con. (pp)",
    saldo_mad = "Saldo (M€)", tasa_cob_mad = "T. cob. (%)"
  )
  if (!is.null(header_cols)) labels_map[names(header_cols)] <- header_cols
  
  cols_exp_ok <- cols_exp[cols_exp %in% names(df)]
  cols_imp_ok <- cols_imp[cols_imp %in% names(df)]
  
  gt_tbl <- df |>
    gt(rowname_col = col_nombre) |>
    cols_hide(columns = ".niv") |>
    tab_stubhead(label = "Sector económico") |> 
    tab_header(title = titulo, subtitle = if (!is.null(subtitulo)) subtitulo else NULL) |>
    tab_source_note(source_note = caption) |>
    tab_spanner(label = label_exp, columns = any_of(cols_exp_ok)) |>
    tab_spanner(label = label_imp, columns = any_of(cols_imp_ok)) |>
    cols_label(.list = as.list(labels_map[names(df)[names(df) != ".niv"]])) |>
    fmt_number(columns = any_of(cols_millones), decimals = dec_num, sep_mark = ".", dec_mark = ",") |>
    fmt_number(columns = any_of(c(cols_pct, cols_contrib)), decimals = dec_pct, sep_mark = ".", dec_mark = ",") |>
    cols_align(align = "center", columns = everything()) |>
    cols_align(align = "left", columns = stub()) |> 
    tab_options(
      table.font.names = fuente,
      table.font.size = px(tam_fuente + 1),
      heading.background.color = col_pal$heading_bg,
      column_labels.background.color = col_pal$labels_bg,
      table.border.top.style = "none",
      data_row.padding = px(4)
    ) |>
    tab_style(
      style = cell_text(color = col_pal$heading_fg, weight = "bold", size = px(tam_fuente + 5)),
      locations = cells_title(groups = "title")
    ) |>
    tab_style(
      style = list(
        cell_fill(color = col_pal$niv3_bg),
        cell_text(color = col_pal$niv3_fg, size = px(tam_fuente + 2), style = "italic", align = "left")
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
        cell_text(color = col_pal$labels_fg, weight = "bold", v_align = "middle", align = "center")
      ),
      locations = list(cells_column_labels(), cells_column_spanners(), cells_stubhead())
    ) |>
    
    # 1. Bordes laterales de separación de grupos
    tab_style(
      style = cell_borders(sides = "right", color = col_pal$border, weight = px(1)),
      locations = list(
        cells_stub(), 
        cells_stubhead(),
        cells_body(columns = any_of(tail(cols_exp_ok, 1))),
        cells_column_labels(columns = any_of(tail(cols_exp_ok, 1))),
        cells_body(columns = any_of(tail(cols_imp_ok, 1))),
        cells_column_labels(columns = any_of(tail(cols_imp_ok, 1)))
      )
    ) |>
    # 2. Estilo Nivel 0 (Total General)
    tab_style(
      style = list(cell_fill(color = col_pal$niv0_bg), cell_text(weight = "bold", color = col_pal$niv0_fg, v_align = "middle")),
      locations = list(cells_body(rows = .niv == 0), cells_stub(rows = .niv == 0))
    ) |>
    # 3. Estilo Nivel 1 y 9 (Grandes Agregados)
    tab_style(
      style = list(cell_fill(color = col_pal$niv1_bg), cell_text(weight = "bold", color = col_pal$niv1_fg, v_align = "middle")),
      locations = list(cells_body(rows = .niv %in% c(1, 9)), cells_stub(rows = .niv %in% c(1, 9)))
    ) |>
    # 4. Estilo Nivel 2 (Sectores / Zonas)
    tab_style(
      style = list(cell_fill(color = col_pal$niv2_bg), cell_text(color = col_pal$niv2_fg, v_align = "middle")),
      locations = list(cells_body(rows = .niv == 2), cells_stub(rows = .niv == 2))
    ) |>
    # 5. Estilo Nivel 3 (Subsectores / Subzonas) - EL QUE FALTABA
    tab_style(
      style = list(cell_fill(color = col_pal$niv3_bg), cell_text(color = col_pal$niv3_fg, v_align = "middle")),
      locations = list(cells_body(rows = .niv == 3), cells_stub(rows = .niv == 3))
    ) |>
    # 6. Sangrías (Indentación) en el Stub
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
  
  # FIX: Refined loop to ensure bars are applied to all specified columns identically
  if (!is.null(col_contrib_bar)) {
    for (col_target in col_contrib_bar) {
      if (col_target %in% names(df)) {
        vals <- df[[col_target]]
        max_abs <- max(abs(vals), na.rm = TRUE)
        if (max_abs == 0) max_abs <- 1
        
        bar_html <- sapply(vals, function(v) {
          if (is.na(v)) return("")
          pct <- (abs(v) / max_abs) * 45
          col_bar <- if (v >= 0) col_pal$bar_pos else col_pal$bar_neg
          label <- formatC(v, format = "f", digits = dec_pct, decimal.mark = ",", big.mark = ".")
          
          paste0('<div style="display:flex; align-items:center; justify-content:center; width:100%; height:100%;">',
                 if(v >= 0) {
                   paste0('<div style="width:50%;text-align:right;padding-right:3px;font-size:',tam_fuente,'px;">', label, '</div>',
                          '<div style="width:50%;display:flex;justify-content:flex-start;"><div style="width:', round(pct,1), '%;height:10px;background:', col_bar, ';"></div></div>')
                 } else {
                   paste0('<div style="width:50%;display:flex;justify-content:flex-end;"><div style="width:', round(pct,1), 
                          '%;height:10px;background:', col_bar, ';"></div></div><div style="width:50%;text-align:left;padding-left:3px;font-size:',tam_fuente,'px;">', label, '</div>')
                 }, '</div>')
        })
        
        # EL FIX: Usamos local() para forzar a que la función capture el valor actual, no la referencia
        gt_tbl <- gt_tbl |> text_transform(
          locations = cells_body(columns = any_of(col_target)), 
          fn = local({
            html_evaluado <- bar_html
            function(x) html_evaluado
          })
        )
      }
    }
  }
  
  gtsave(gt_tbl, filename = ruta_salida, vwidth = round(ancho_cm / 2.54 * dpi), zoom = dpi / 96)
  return(gt_tbl)
}


# ── 2. SECTORES: EXPORTACIÓN A EXCEL (OPENXLSX) ───────────────────────────────
exportar_sectores_excel <- function(
    tabla,
    cols_exp        = c("exp_mad", "exp_mad_pct", "exp_mad_tva", "exp_mad_contrib"),
    cols_imp        = c("imp_mad", "imp_mad_pct", "imp_mad_tva", "imp_mad_contrib"),
    cols_extra      = c("saldo_mad", "tasa_cob_mad"),
    cols_contrib    = c("exp_mad_contrib", "imp_mad_contrib", "exp_esp_contrib", "imp_esp_contrib"),
    omitir_orden    = NULL,
    label_exp       = "Exportaciones",
    label_imp       = "Importaciones",
    label_extra     = "Saldo",
    cols_millones   = c("exp_mad", "imp_mad", "saldo_mad", "saldo_mad_prev", "exp_esp", "imp_esp", "saldo_esp", "saldo_esp_prev"),
    cols_pct        = c("exp_mad_pct", "imp_mad_pct", "exp_mad_tva", "imp_mad_tva", "exp_mad_vs_esp", "imp_mad_vs_esp", "tasa_cob_mad", "exp_esp_pct", "imp_esp_pct", "exp_esp_tva", "imp_esp_tva", "tasa_cob_esp"),
    header_cols     = NULL,
    titulo          = "Comercio exterior por sectores",
    caption         = "Elaboración propia a partir de microdatos de DataComex.",
    ruta_salida     = "./tabla_sectores.xlsx",
    tam_fuente      = 8,
    dec_num         = 1L,
    dec_pct         = 1L,
    col_pal         = list(
      heading_bg = "#526DB0", heading_fg = "#F5C201",
      labels_bg  = "#F5C201", labels_fg  = "black",
      niv1_bg    = "#B9C4DF", niv2_bg    = "#F2F2F2"
    )
) {
  df <- as.data.frame(tabla)
  if (!is.null(omitir_orden) && length(omitir_orden) > 0) df <- df[!df$orden %in% omitir_orden, ]
  
  col_nombre <- "nombre"; cols_datos <- c(cols_exp, cols_imp, cols_extra)
  cols_sel <- c(col_nombre, cols_datos[cols_datos %in% names(df)])
  niv_vector <- df$niv; df <- df[, cols_sel, drop = FALSE]
  
  for (col in names(df)) {
    if (col %in% cols_millones) df[[col]] <- df[[col]] / 1e6
    else if (col %in% c(cols_pct, cols_contrib)) df[[col]] <- df[[col]] * 100
  }
  
  wb <- createWorkbook()
  addWorksheet(wb, "Sectores", gridLines = FALSE)
  
  st_titulo  <- createStyle(fontSize = tam_fuente + 1, fontName = "Arial", fontColour = col_pal$heading_fg, textDecoration = "bold", fgFill = col_pal$heading_bg, halign = "center")
  st_spanner <- createStyle(fontSize = tam_fuente,     fontName = "Arial", fontColour = col_pal$labels_fg,  textDecoration = "bold", fgFill = col_pal$labels_bg,  halign = "center", border = "TopBottomLeftRight")
  st_niv1    <- createStyle(fontSize = tam_fuente,     fontName = "Arial", fontColour = "black", textDecoration = "bold", fgFill = col_pal$niv1_bg, halign = "left")
  st_niv2    <- createStyle(fontSize = tam_fuente,     fontName = "Arial", fontColour = "black", fgFill = col_pal$niv2_bg, halign = "left", indent = 1)
  st_niv3    <- createStyle(fontSize = tam_fuente - 1, fontName = "Arial", fontColour = "black", halign = "left", indent = 2)
  
  writeData(wb, "Sectores", titulo, startRow = 1, startCol = 1)
  mergeCells(wb, "Sectores", rows = 1, cols = 1:ncol(df))
  addStyle(wb, "Sectores", st_titulo, rows = 1, cols = 1:ncol(df))
  
  writeData(wb, "Sectores", df, startRow = 4)
  
  for(i in 1:nrow(df)) {
    style <- if(niv_vector[i] == 1) st_niv1 else if(niv_vector[i] == 2) st_niv2 else st_niv3
    addStyle(wb, "Sectores", style, rows = i + 3, cols = 1:ncol(df), stack = TRUE)
  }
  
  saveWorkbook(wb, ruta_salida, overwrite = TRUE)
  return(invisible(wb))
}


# ── 3. PAÍSES: EXPORTACIÓN A IMAGEN (GT) ──────────────────────────────────────
# Versión para tabla_paises — replica exportar_sectores_imagen con soporte hasta nivel 4
# Diferencias respecto a la versión sectores:
#   · stub label → "País / Zona geográfica"
#   · col_nombre → "pais"
#   · Nivel 3 → cursiva (italic), indentación extra
#   · Nivel 4 → cursiva (italic), indentación máxima
exportar_paises_imagen <- function(
    tabla,
    cols_exp         = c("exp_mad", "exp_mad_pct", "exp_mad_tva", "exp_mad_contrib"),
    cols_imp         = c("imp_mad", "imp_mad_pct", "imp_mad_tva", "imp_mad_contrib"),
    cols_extra       = c("saldo_mad", "tasa_cob_mad"),
    omitir_orden     = NULL,
    label_exp        = "Exportaciones",
    label_imp        = "Importaciones",
    label_extra      = "Saldo",
    col_contrib_bar  = NULL,
    cols_millones    = c("exp_mad", "imp_mad", "saldo_mad", "saldo_mad_prev",
                         "exp_esp", "imp_esp", "saldo_esp", "saldo_esp_prev"),
    cols_pct         = c("exp_mad_pct", "imp_mad_pct", "exp_mad_tva", "imp_mad_tva",
                         "exp_mad_vs_esp", "imp_mad_vs_esp", "tasa_cob_mad",
                         "exp_esp_pct", "imp_esp_pct", "exp_esp_tva", "imp_esp_tva",
                         "tasa_cob_esp"),
    cols_contrib     = c("exp_mad_contrib", "imp_mad_contrib",
                         "exp_esp_contrib", "imp_esp_contrib"),
    header_cols      = NULL,
    titulo           = "Comercio exterior por países",
    subtitulo        = NULL,
    caption          = "Elaboración propia a partir de microdatos",
    ruta_salida      = "./tabla_paises.png",
    ancho_cm         = 25,
    alto_cm          = NULL,
    tam_fuente       = 7,
    fuente           = "Arial",
    dec_num          = 1L,
    dec_pct          = 1L,
    dpi              = 300,
    col_pal          = list(
      heading_bg = "#526DB0", heading_fg = "#F5C201",
      labels_bg  = "#F5C201", labels_fg  = "black",
      border     = "#AAAAAA",
      niv0_bg    = "#F5C201", niv0_fg    = "black",
      niv1_bg    = "#B9C4DF", niv1_fg    = "black",
      niv2_bg    = "#F2F2F2", niv2_fg    = "black",
      niv3_bg    = "#FFFFFF", niv3_fg    = "black",
      niv4_bg    = "#FFFFFF", niv4_fg    = "#333333",
      bar_pos    = "#2E7D5E", bar_neg    = "#C0392B"
    )
) {
  
  df <- as.data.frame(tabla)
  if (!is.null(omitir_orden) && length(omitir_orden) > 0) df <- df[!df$orden %in% omitir_orden, ]
  
  col_nombre <- "pais"
  cols_datos <- c(cols_exp, cols_imp, cols_extra)
  cols_datos <- cols_datos[lengths(list(cols_datos)) > 0]
  cols_sel   <- c(col_nombre, cols_datos)
  cols_sel   <- cols_sel[cols_sel %in% names(df)]
  
  niv_vector <- df$niv
  df         <- df[, cols_sel, drop = FALSE]
  
  for (col in names(df)) {
    if (col %in% cols_millones)              df[[col]] <- df[[col]] / 1e6
    else if (col %in% c(cols_pct, cols_contrib)) df[[col]] <- df[[col]] * 100
  }
  df$.niv <- niv_vector
  
  labels_map <- c(
    pais         = "País / Zona geográfica",
    exp_mad      = "Mill. €",  exp_mad_pct     = "% s/total",
    exp_mad_tva  = "TVA (%)",  exp_mad_contrib = "Con. (pp)",
    imp_mad      = "Mill. €",  imp_mad_pct     = "% s/total",
    imp_mad_tva  = "TVA (%)",  imp_mad_contrib = "Con. (pp)",
    saldo_mad    = "Saldo (M€)", tasa_cob_mad  = "T. cob. (%)"
  )
  if (!is.null(header_cols)) labels_map[names(header_cols)] <- header_cols
  
  cols_exp_ok <- cols_exp[cols_exp %in% names(df)]
  cols_imp_ok <- cols_imp[cols_imp %in% names(df)]
  
  gt_tbl <- df |>
    gt(rowname_col = col_nombre) |>
    cols_hide(columns = ".niv") |>
    tab_stubhead(label = "País / Zona geográfica") |>
    tab_header(title = titulo, subtitle = if (!is.null(subtitulo)) subtitulo else NULL) |>
    tab_source_note(source_note = caption) |>
    tab_spanner(label = label_exp, columns = any_of(cols_exp_ok)) |>
    tab_spanner(label = label_imp, columns = any_of(cols_imp_ok)) |>
    cols_label(.list = as.list(labels_map[names(df)[names(df) != ".niv"]])) |>
    fmt_number(columns = any_of(cols_millones),              decimals = dec_num, sep_mark = ".", dec_mark = ",") |>
    fmt_number(columns = any_of(c(cols_pct, cols_contrib)),  decimals = dec_pct, sep_mark = ".", dec_mark = ",") |>
    cols_align(align = "center", columns = everything()) |>
    cols_align(align = "left",   columns = stub()) |>
    tab_options(
      table.font.names                  = fuente,
      table.font.size                   = px(tam_fuente + 1),
      heading.background.color          = col_pal$heading_bg,
      column_labels.background.color    = col_pal$labels_bg,
      table.border.top.style            = "none",
      data_row.padding                  = px(4)
    ) |>
    tab_style(
      style     = cell_text(color = col_pal$heading_fg, weight = "bold", size = px(tam_fuente + 5)),
      locations = cells_title(groups = "title")
    ) |>
    tab_style(
      style = list(
        cell_fill(color = col_pal$niv3_bg),
        cell_text(color = col_pal$niv3_fg, size = px(tam_fuente + 2), style = "italic", align = "left")
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
        cell_text(color = col_pal$labels_fg, weight = "bold", v_align = "middle", align = "center")
      ),
      locations = list(cells_column_labels(), cells_column_spanners(), cells_stubhead())
    ) |>
    
    # Líneas verticales de separación de bloques
    tab_style(
      style = cell_borders(sides = "right", color = col_pal$border, weight = px(1)),
      locations = list(
        cells_stub(),
        cells_stubhead(),
        cells_body(columns = any_of(tail(cols_exp_ok, 1))),
        cells_column_labels(columns = any_of(tail(cols_exp_ok, 1))),
        cells_body(columns = any_of(tail(cols_imp_ok, 1))),
        cells_column_labels(columns = any_of(tail(cols_imp_ok, 1)))
      )
    ) |>
    
    # Nivel 0 — Total general (fondo amarillo, negrita)
    tab_style(
      style     = list(cell_fill(color = col_pal$niv0_bg), cell_text(weight = "bold", color = col_pal$niv0_fg, v_align = "middle")),
      locations = list(cells_body(rows = .niv == 0), cells_stub(rows = .niv == 0))
    ) |>
    
    # Nivel 1 y 9 — Grandes agregados (azul claro, negrita)
    tab_style(
      style     = list(cell_fill(color = col_pal$niv1_bg), cell_text(weight = "bold", color = col_pal$niv1_fg, v_align = "middle")),
      locations = list(cells_body(rows = .niv %in% c(1, 9)), cells_stub(rows = .niv %in% c(1, 9)))
    ) |>
    
    # Nivel 2 — Zonas / regiones (gris muy claro)
    tab_style(
      style     = list(cell_fill(color = col_pal$niv2_bg), cell_text(color = col_pal$niv2_fg, v_align = "middle")),
      locations = list(cells_body(rows = .niv == 2), cells_stub(rows = .niv == 2))
    ) |>
    
    # Nivel 3 — Subzonas (fondo blanco, cursiva)
    tab_style(
      style     = list(cell_fill(color = col_pal$niv3_bg), cell_text(color = col_pal$niv3_fg, style = "italic", v_align = "middle")),
      locations = list(cells_body(rows = .niv == 3), cells_stub(rows = .niv == 3))
    ) |>
    
    # Nivel 4 — Países individuales (fondo blanco, cursiva, menor tamaño)
    tab_style(
      style     = list(
        cell_fill(color = col_pal$niv4_bg),
        cell_text(color = col_pal$niv4_fg, style = "italic", size = px(tam_fuente), v_align = "middle")
      ),
      locations = list(cells_body(rows = .niv == 4), cells_stub(rows = .niv == 4))
    ) |>
    
    # Indentación en stub según nivel
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
  
  # Barras de contribución
  if (!is.null(col_contrib_bar)) {
    for (col_target in col_contrib_bar) {
      if (col_target %in% names(df)) {
        vals <- df[[col_target]]
        max_abs <- max(abs(vals), na.rm = TRUE)
        if (max_abs == 0) max_abs <- 1
        
        bar_html <- sapply(vals, function(v) {
          if (is.na(v)) return("")
          pct <- (abs(v) / max_abs) * 45
          col_bar <- if (v >= 0) col_pal$bar_pos else col_pal$bar_neg
          label <- formatC(v, format = "f", digits = dec_pct, decimal.mark = ",", big.mark = ".")
          
          paste0('<div style="display:flex; align-items:center; justify-content:center; width:100%; height:100%;">',
                 if(v >= 0) {
                   paste0('<div style="width:50%;text-align:right;padding-right:3px;font-size:',tam_fuente,'px;">', label, '</div>',
                          '<div style="width:50%;display:flex;justify-content:flex-start;"><div style="width:', round(pct,1), '%;height:10px;background:', col_bar, ';"></div></div>')
                 } else {
                   paste0('<div style="width:50%;display:flex;justify-content:flex-end;"><div style="width:', round(pct,1), 
                          '%;height:10px;background:', col_bar, ';"></div></div><div style="width:50%;text-align:left;padding-left:3px;font-size:',tam_fuente,'px;">', label, '</div>')
                 }, '</div>')
        })
        
        # EL FIX: Usamos local() para forzar a que la función capture el valor actual, no la referencia
        gt_tbl <- gt_tbl |> text_transform(
          locations = cells_body(columns = any_of(col_target)), 
          fn = local({
            html_evaluado <- bar_html
            function(x) html_evaluado
          })
        )
      }
    }
  }
  
  gtsave(gt_tbl, filename = ruta_salida, vwidth = round(ancho_cm / 2.54 * dpi), zoom = dpi / 96)
  return(gt_tbl)
}


# ── 4. PAÍSES: EXPORTACIÓN A EXCEL (OPENXLSX) ─────────────────────────────────
# Diferencias respecto a la versión sectores:
#   · col_nombre → "pais"
#   · Estilos niv3 (italic, indent 2) y niv4 (italic, indent 3)
exportar_paises_excel <- function(
    tabla,
    cols_exp        = c("exp_mad", "exp_mad_pct", "exp_mad_tva", "exp_mad_contrib"),
    cols_imp        = c("imp_mad", "imp_mad_pct", "imp_mad_tva", "imp_mad_contrib"),
    cols_extra      = c("saldo_mad", "tasa_cob_mad"),
    cols_contrib    = c("exp_mad_contrib", "imp_mad_contrib", "exp_esp_contrib", "imp_esp_contrib"),
    omitir_orden    = NULL,
    label_exp       = "Exportaciones",
    label_imp       = "Importaciones",
    label_extra     = "Saldo",
    cols_millones   = c("exp_mad", "imp_mad", "saldo_mad", "saldo_mad_prev",
                        "exp_esp", "imp_esp", "saldo_esp", "saldo_esp_prev"),
    cols_pct        = c("exp_mad_pct", "imp_mad_pct", "exp_mad_tva", "imp_mad_tva",
                        "exp_mad_vs_esp", "imp_mad_vs_esp", "tasa_cob_mad",
                        "exp_esp_pct", "imp_esp_pct", "exp_esp_tva", "imp_esp_tva",
                        "tasa_cob_esp"),
    header_cols     = NULL,
    titulo          = "Comercio exterior por países",
    caption         = "Elaboración propia a partir de microdatos de DataComex.",
    ruta_salida     = "./tabla_paises.xlsx",
    tam_fuente      = 8,
    dec_num         = 1L,
    dec_pct         = 1L,
    col_pal         = list(
      heading_bg = "#526DB0", heading_fg = "#F5C201",
      labels_bg  = "#F5C201", labels_fg  = "black",
      niv1_bg    = "#B9C4DF", niv2_bg    = "#F2F2F2",
      niv4_fg    = "#333333"
    )
) {
  df <- as.data.frame(tabla)
  if (!is.null(omitir_orden) && length(omitir_orden) > 0) df <- df[!df$orden %in% omitir_orden, ]
  
  col_nombre <- "pais"
  cols_datos <- c(cols_exp, cols_imp, cols_extra)
  cols_sel   <- c(col_nombre, cols_datos[cols_datos %in% names(df)])
  niv_vector <- df$niv
  df         <- df[, cols_sel, drop = FALSE]
  
  for (col in names(df)) {
    if (col %in% cols_millones)                  df[[col]] <- df[[col]] / 1e6
    else if (col %in% c(cols_pct, cols_contrib)) df[[col]] <- df[[col]] * 100
  }
  
  wb <- createWorkbook()
  addWorksheet(wb, "Paises", gridLines = FALSE)
  
  st_titulo  <- createStyle(fontSize = tam_fuente + 1, fontName = "Arial", fontColour = col_pal$heading_fg,
                            textDecoration = "bold",   fgFill = col_pal$heading_bg, halign = "center")
  st_spanner <- createStyle(fontSize = tam_fuente,     fontName = "Arial", fontColour = col_pal$labels_fg,
                            textDecoration = "bold",   fgFill = col_pal$labels_bg,  halign = "center",
                            border = "TopBottomLeftRight")
  st_niv1    <- createStyle(fontSize = tam_fuente,     fontName = "Arial", fontColour = "black",
                            textDecoration = "bold",   fgFill = col_pal$niv1_bg, halign = "left")
  st_niv2    <- createStyle(fontSize = tam_fuente,     fontName = "Arial", fontColour = "black",
                            fgFill = col_pal$niv2_bg,  halign = "left",    indent = 1)
  st_niv3    <- createStyle(fontSize = tam_fuente - 1, fontName = "Arial", fontColour = "black",
                            textDecoration = "italic", halign = "left",    indent = 2)
  st_niv4    <- createStyle(fontSize = tam_fuente - 1, fontName = "Arial", fontColour = col_pal$niv4_fg,
                            textDecoration = "italic", halign = "left",    indent = 3)
  
  writeData(wb, "Paises", titulo, startRow = 1, startCol = 1)
  mergeCells(wb, "Paises", rows = 1, cols = 1:ncol(df))
  addStyle(wb,  "Paises", st_titulo, rows = 1, cols = 1:ncol(df))
  
  writeData(wb, "Paises", df, startRow = 4)
  
  for (i in seq_len(nrow(df))) {
    style <- switch(as.character(niv_vector[i]),
                    "1" = st_niv1,
                    "2" = st_niv2,
                    "3" = st_niv3,
                    "4" = st_niv4,
                    st_niv1)   # niv 0 / 9 → mismo estilo que niv1
    addStyle(wb, "Paises", style, rows = i + 3, cols = 1:ncol(df), stack = TRUE)
  }
  
  saveWorkbook(wb, ruta_salida, overwrite = TRUE)
  return(invisible(wb))
}

# funciones_gtb_evol.R
# Exportación gt de tablas de evolución anual
#
# Orden de grupos de columnas:
#   1. Volumen      : _ano_base  y  _ano_final   (Mill. €)
#   2. Variación    : TVA (ano_base→ano_final) + contrib barra centrada
#   3. Índice       : _idx_Y  numérico, todos los años
#   4. Tendencia    : sparkline de línea sobre _idx_Y
#   5. % s/total    : _pct_Y  todos los años

# funciones_gtb_evol.R
# Exportación gt de tablas de evolución anual
#
# Orden de grupos de columnas:
#   1. Volumen      : todos los años  (Mill. €)
#   2. Variación    : TVA (ano_base→ano_final) + contrib barra centrada
#   3. Índice       : _idx_Y  numérico, todos los años
#   4. Tendencia    : sparkline de línea sobre _idx_Y

exportar_evol_imagen <- function(
    tabla,
    flujo        = "exp",
    territorio   = "mad",
    tipo         = "sectores",
    ano_base     = 2019L,
    ano_final    = 2025L,
    anos_mostrar = 2019L:2025L,
    cols_millones_factor = 1e6,
    dec_num      = 1L,
    dec_pct      = 1L,
    titulo       = NULL,
    subtitulo    = NULL,
    caption      = "Elaboración propia a partir de microdatos obtenidos de DataComex.",
    ruta_salida  = "./tabla_evol.png",
    ancho_cm     = 32,
    tam_fuente   = 7L,
    fuente       = "Arial",
    dpi          = 300L,
    col_pal      = list(
      heading_bg = "#526DB0", heading_fg = "#F5C201",
      labels_bg  = "#F5C201", labels_fg  = "black",
      border     = "#AAAAAA",
      niv0_bg    = "#F5C201", niv0_fg    = "black",
      niv1_bg    = "#B9C4DF", niv1_fg    = "black",
      niv2_bg    = "#F2F2F2", niv2_fg    = "black",
      niv3_bg    = "#FFFFFF", niv3_fg    = "black",
      niv4_bg    = "#FFFFFF", niv4_fg    = "#333333",
      bar_pos    = "#2E7D5E", bar_neg    = "#C0392B"
    )
) {
  
  df   <- as.data.frame(data.table::copy(tabla))
  fl   <- flujo
  ter  <- territorio
  pref <- paste0(fl, "_", ter)
  anos <- sort(anos_mostrar)
  ab   <- as.character(ano_base)
  af   <- as.character(ano_final)
  
  # ── Columnas id ──────────────────────────────────────────────────────────────
  id_col  <- if (tipo == "sectores") "nombre" else "pais"
  id_cols <- if (tipo == "sectores") c("orden", "niv", "nombre") else c("orden", "niv", "pais")
  
  # ── Nombres de columnas ──────────────────────────────────────────────────────
  cols_vol  <- paste0(pref, "_", anos)          # volumen todos los años
  cols_idx  <- paste0(pref, "_idx_", anos)
  col_contrib <- paste0(pref, "_contrib")
  
  # TVA ano_base → ano_final calculada desde los raw
  col_tva      <- paste0(pref, "_tva_evol")
  col_raw_base <- paste0(pref, "_", ab)
  col_raw_final <- paste0(pref, "_", af)
  if (col_raw_base %in% names(df) && col_raw_final %in% names(df)) {
    df[[col_tva]] <- ifelse(
      df[[col_raw_base]] != 0,
      (df[[col_raw_final]] - df[[col_raw_base]]) / abs(df[[col_raw_base]]) * 100,
      NA_real_
    )
  }
  
  # ── Seleccionar columnas necesarias ──────────────────────────────────────────
  all_need <- c(id_cols, cols_vol, col_tva, col_contrib, cols_idx)
  all_need <- all_need[all_need %in% names(df)]
  df       <- df[, all_need, drop = FALSE]
  
  niv_vec <- df$niv
  
  # ── Escalar volumen a Mill. € ────────────────────────────────────────────────
  for (cv in cols_vol) {
    if (cv %in% names(df)) df[[cv]] <- df[[cv]] / cols_millones_factor
  }
  # col_contrib → × 100 (está en proporción)
  if (col_contrib %in% names(df)) df[[col_contrib]] <- df[[col_contrib]] * 100
  # col_tva ya en %, cols_idx ya en base 100 — sin escalar
  
  # ── Sparkline de línea desde idx ─────────────────────────────────────────────
  idx_present <- cols_idx[cols_idx %in% names(df)]
  if (length(idx_present) > 0) {
    df$.spark <- apply(df[, idx_present, drop = FALSE], 1,
                       function(r) list(as.numeric(r)))
  }
  
  df$.niv <- niv_vec
  
  # ── Orden de columnas en el df → define orden en gt ──────────────────────────
  cols_vol_present <- cols_vol[cols_vol %in% names(df)]
  col_order <- c(id_cols,
                 cols_vol_present,
                 col_tva, col_contrib,
                 idx_present,
                 ".spark",
                 ".niv")
  col_order <- col_order[col_order %in% names(df)]
  df        <- df[, col_order, drop = FALSE]
  
  # ── Construir gt ─────────────────────────────────────────────────────────────
  gt_tbl <- df |>
    gt(rowname_col = id_col) |>
    cols_hide(columns = any_of(c("orden", "niv", ".niv"))) |>
    tab_stubhead(label = if (tipo == "sectores") "Sector económico" else "País / Zona") |>
    tab_header(
      title = titulo %||% paste0(
        if (fl == "exp") "Exportaciones" else "Importaciones",
        " de ", if (ter == "mad") "Madrid" else "España",
        " \u2014 Evolución ", ano_base, "\u2013", ano_final
      ),
      subtitle = subtitulo %||% paste0(
        "Volumen (Mill.\u20ac), variación ", ano_base, "\u2013", ano_final,
        ", \u00edndice (", ano_base, "=100) y tendencia por ",
        if (tipo == "sectores") "sector econ\u00f3mico" else "pa\u00eds / zona geogr\u00e1fica"
      )
    ) |>
    tab_source_note(source_note = caption) |>
    
    # ── Spanners ─────────────────────────────────────────────────────────────
    tab_spanner(
      label   = paste0("Volumen (Mill.\u20ac)"),
      columns = any_of(cols_vol_present)
    ) |>
    tab_spanner(
      label   = paste0("Variaci\u00f3n (", ab, "\u2013", af, ")"),
      columns = any_of(c(col_tva, col_contrib))
    ) |>
    tab_spanner(
      label   = paste0("\u00cdndice (", ab, "=100)"),
      columns = any_of(idx_present)
    ) |>
    tab_spanner(
      label   = "Tendencia",
      columns = any_of(".spark")
    )
  
  # ── Etiquetas individuales ───────────────────────────────────────────────────
  labels_list <- c(
    setNames(as.list(as.character(anos)), cols_vol_present),
    setNames(list("TVA (%)"),             col_tva),
    setNames(list("Con. (p.p.)"),         col_contrib),
    setNames(as.list(as.character(anos)), idx_present),
    list(.spark = paste0(ab, " \u2192 ", af))
  )
  labels_list <- labels_list[names(labels_list) %in% names(df)]
  gt_tbl <- gt_tbl |> cols_label(.list = labels_list)
  
  # ── Formatos numéricos ───────────────────────────────────────────────────────
  gt_tbl <- gt_tbl |>
    fmt_number(
      columns  = any_of(cols_vol_present),
      decimals = dec_num, sep_mark = ".", dec_mark = ","
    ) |>
    fmt_number(
      columns  = any_of(col_tva),
      decimals = dec_pct, sep_mark = ".", dec_mark = ","
    ) |>
    fmt_number(
      columns  = any_of(idx_present),
      decimals = 1L, sep_mark = ".", dec_mark = ","
    ) |>
    cols_align(align = "center", columns = everything()) |>
    cols_align(align = "left",   columns = stub())
  
  # ── Sparkline de LÍNEA ───────────────────────────────────────────────────────
  if (".spark" %in% names(df)) {
    gt_tbl <- gt_tbl |>
      text_transform(
        locations = cells_body(columns = ".spark"),
        fn = function(x) {
          lapply(df$.spark, function(vals) {
            vals <- unlist(vals)
            if (all(is.na(vals))) return("")
            
            n       <- length(vals)
            w_total <- 80
            h_total <- 30
            pad     <- 3
            w_inner <- w_total - 2 * pad
            h_inner <- h_total - 2 * pad
            
            mn  <- min(vals, na.rm = TRUE)
            mx  <- max(vals, na.rm = TRUE)
            rng <- if ((mx - mn) == 0) 1 else mx - mn
            
            xs <- round(pad + (seq_along(vals) - 1) / (n - 1) * w_inner, 2)
            ys <- round(pad + (1 - (vals - mn) / rng) * h_inner, 2)
            
            line_col   <- if (vals[n] >= 100) col_pal$spark_pos %||% col_pal$bar_pos else col_pal$spark_neg %||% col_pal$bar_neg
            points_str <- paste(paste0(xs, ",", ys), collapse = " ")
            last_dot   <- paste0('<circle cx="', xs[n], '" cy="', ys[n],
                                 '" r="2.5" fill="', line_col, '"/>')
            
            y100 <- round(pad + (1 - (100 - mn) / rng) * h_inner, 2)
            ref_line <- if (y100 >= pad && y100 <= h_total - pad) {
              paste0('<line x1="', pad, '" y1="', y100,
                     '" x2="', w_total - pad, '" y2="', y100,
                     '" stroke="', col_pal$spark_ref %||% col_pal$border, '" stroke-width="0.5" stroke-dasharray="2,2"/>')
            } else ""
            
            paste0('<svg width="', w_total, '" height="', h_total,
                   '" xmlns="http://www.w3.org/2000/svg">',
                   ref_line,
                   '<polyline points="', points_str,
                   '" fill="none" stroke="', line_col,
                   '" stroke-width="1.8" stroke-linejoin="round" stroke-linecap="round"/>',
                   last_dot, '</svg>')
          })
        }
      )
  }
  
  # ── Barra centrada para contrib ──────────────────────────────────────────────
  if (col_contrib %in% names(df)) {
    vals_contrib <- df[[col_contrib]]
    max_abs      <- max(abs(vals_contrib), na.rm = TRUE)
    if (max_abs == 0) max_abs <- 1
    
    bar_html <- sapply(vals_contrib, function(v) {
      if (is.na(v)) return("")
      pct     <- (abs(v) / max_abs) * 45
      col_bar <- if (v >= 0) col_pal$bar_pos else col_pal$bar_neg
      label   <- formatC(v, format = "f", digits = dec_pct,
                         decimal.mark = ",", big.mark = ".")
      if (v >= 0) {
        paste0('<div style="display:flex;align-items:center;justify-content:center;',
               'width:100%;height:100%;">',
               '<div style="width:50%;text-align:right;padding-right:3px;font-size:',
               tam_fuente, 'px;">', label, '</div>',
               '<div style="width:50%;display:flex;justify-content:flex-start;">',
               '<div style="width:', round(pct,1), '%;height:10px;background:',
               col_bar, ';"></div></div></div>')
      } else {
        paste0('<div style="display:flex;align-items:center;justify-content:center;',
               'width:100%;height:100%;">',
               '<div style="width:50%;display:flex;justify-content:flex-end;">',
               '<div style="width:', round(pct,1), '%;height:10px;background:',
               col_bar, ';"></div></div>',
               '<div style="width:50%;text-align:left;padding-left:3px;font-size:',
               tam_fuente, 'px;">', label, '</div></div>')
      }
    })
    
    gt_tbl <- gt_tbl |>
      text_transform(
        locations = cells_body(columns = any_of(col_contrib)),
        fn        = function(x) bar_html
      )
  }
  
  # ── Opciones y estilos ───────────────────────────────────────────────────────
  gt_tbl <- gt_tbl |>
    tab_options(
      table.font.names               = fuente,
      table.font.size                = px(tam_fuente + 1),
      heading.background.color       = col_pal$heading_bg,
      column_labels.background.color = col_pal$labels_bg,
      table.border.top.style         = "none",
      data_row.padding               = px(4)
    ) |>
    tab_style(
      style     = cell_text(color = col_pal$heading_fg, weight = "bold", size = px(tam_fuente + 5)),
      locations = cells_title(groups = "title")
    ) |>
    tab_style(
      style = list(
        cell_fill(color = col_pal$niv3_bg),
        cell_text(color = col_pal$niv3_fg, size = px(tam_fuente + 2), style = "italic", align = "left")
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
        cell_text(color = col_pal$labels_fg, weight = "bold", v_align = "middle", align = "center")
      ),
      locations = list(cells_column_labels(), cells_column_spanners(), cells_stubhead())
    ) |>
    # Bordes laterales de separación de bloques
    # stub | Volumen | Variación | Índice | Tendencia
    tab_style(
      style = cell_borders(sides = "right", color = col_pal$border, weight = px(1)),
      locations = list(
        cells_stub(),
        cells_stubhead(),
        cells_body(columns = any_of(tail(cols_vol_present, 1))),
        cells_column_labels(columns = any_of(tail(cols_vol_present, 1))),
        cells_body(columns = any_of(col_contrib)),
        cells_column_labels(columns = any_of(col_contrib)),
        cells_body(columns = any_of(tail(idx_present, 1))),
        cells_column_labels(columns = any_of(tail(idx_present, 1)))
      )
    ) |>
    # Nivel 0 — Total general
    tab_style(
      style     = list(cell_fill(color = col_pal$niv0_bg), cell_text(weight = "bold", color = col_pal$niv0_fg, v_align = "middle")),
      locations = list(cells_body(rows = .niv == 0), cells_stub(rows = .niv == 0))
    ) |>
    # Nivel 1 y 9 — Grandes agregados
    tab_style(
      style     = list(cell_fill(color = col_pal$niv1_bg), cell_text(weight = "bold", color = col_pal$niv1_fg, v_align = "middle")),
      locations = list(cells_body(rows = .niv %in% c(1, 9)), cells_stub(rows = .niv %in% c(1, 9)))
    ) |>
    # Nivel 2 — Sectores / Zonas
    tab_style(
      style     = list(cell_fill(color = col_pal$niv2_bg), cell_text(color = col_pal$niv2_fg, v_align = "middle")),
      locations = list(cells_body(rows = .niv == 2), cells_stub(rows = .niv == 2))
    )
  
  if (any(niv_vec %in% c(3, 4))) {
    gt_tbl <- gt_tbl |>
      # Nivel 3 — Subsectores / Subzonas (cursiva)
      tab_style(
        style     = list(cell_fill(color = col_pal$niv3_bg), cell_text(color = col_pal$niv3_fg, style = "italic", v_align = "middle")),
        locations = list(cells_body(rows = .niv == 3), cells_stub(rows = .niv == 3))
      ) |>
      # Nivel 4 — Detalle (cursiva, menor tamaño)
      tab_style(
        style     = list(
          cell_fill(color = col_pal$niv4_bg),
          cell_text(color = col_pal$niv4_fg, style = "italic", size = px(tam_fuente), v_align = "middle")
        ),
        locations = list(cells_body(rows = .niv == 4), cells_stub(rows = .niv == 4))
      )
  }
  
  gt_tbl <- gt_tbl |>
    text_transform(
      locations = cells_stub(),
      fn = function(x) {
        sapply(seq_along(x), function(i) {
          indent <- switch(as.character(niv_vec[i]),
                           "2" = "&nbsp;&nbsp;&nbsp;",
                           "3" = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                           "4" = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                           "")
          paste0(indent, x[i])
        })
      }
    )
  
  gtsave(gt_tbl, filename = ruta_salida,
         vwidth = round(ancho_cm / 2.54 * dpi), zoom = dpi / 96)
  invisible(gt_tbl)
}


# ── exportar_evol_pct_imagen ──────────────────────────────────────────────────
# Tabla de evolución basada en PORCENTAJES:
#   Bloque izq : _pct_  de Madrid (% s/total Madrid)  + sparkline de tendencia
#   Bloque der : _vs_esp_ de Madrid (% s/total España) + sparkline de tendencia
# Solo Madrid (territorio = "mad" implícito en los nombres de columna).
# Funciona para tipo = "sectores" (col_nombre = "nombre") y
#                     "paises"   (col_nombre = "pais").
exportar_evol_pct_imagen <- function(
    tabla,
    flujo        = "exp",
    tipo         = "sectores",       # "sectores" | "paises"
    anos_mostrar = 2019L:2025L,
    dec_pct      = 1L,
    titulo       = NULL,
    subtitulo    = NULL,
    caption      = "Elaboración propia a partir de microdatos obtenidos de DataComex.",
    ruta_salida  = "./tabla_evol_pct.png",
    ancho_cm     = 32,
    tam_fuente   = 7L,
    fuente       = "Arial",
    dpi          = 300L,
    col_pal      = list(
      heading_bg = "#526DB0", heading_fg = "#F5C201",
      labels_bg  = "#F5C201", labels_fg  = "black",
      border     = "#AAAAAA",
      niv0_bg    = "#F5C201", niv0_fg    = "black",
      niv1_bg    = "#B9C4DF", niv1_fg    = "black",
      niv2_bg    = "#F2F2F2", niv2_fg    = "black",
      niv3_bg    = "#FFFFFF", niv3_fg    = "black",
      niv4_bg    = "#FFFFFF", niv4_fg    = "#333333",
      bar_pos    = "#2E7D5E", bar_neg    = "#C0392B"
    )
) {
  
  df   <- as.data.frame(data.table::copy(tabla))
  fl   <- flujo
  anos <- sort(anos_mostrar)
  ab   <- as.character(min(anos))
  af   <- as.character(max(anos))
  
  # ── Columnas id ──────────────────────────────────────────────────────────────
  id_col  <- if (tipo == "sectores") "nombre" else "pais"
  id_cols <- if (tipo == "sectores") c("orden", "niv", "nombre") else c("orden", "niv", "pais")
  
  # ── Nombres de columnas fuente ────────────────────────────────────────────────
  # Porcentaje s/total Madrid:  exp_mad_pct_2019 … exp_mad_pct_2025
  cols_pct    <- paste0(fl, "_mad_pct_", anos)
  # % Madrid sobre España:      exp_mad_vs_esp_2019 … exp_mad_vs_esp_2025
  cols_vs_esp <- paste0(fl, "_mad_vs_esp_", anos)
  
  # ── Seleccionar y escalar ─────────────────────────────────────────────────────
  all_need <- c(id_cols, cols_pct, cols_vs_esp)
  all_need <- all_need[all_need %in% names(df)]
  df       <- df[, all_need, drop = FALSE]
  
  niv_vec <- df$niv
  
  # Ambas columnas están en proporción → × 100
  cols_pct_present    <- cols_pct[cols_pct %in% names(df)]
  cols_vs_esp_present <- cols_vs_esp[cols_vs_esp %in% names(df)]
  
  for (col in c(cols_pct_present, cols_vs_esp_present)) df[[col]] <- df[[col]] * 100
  
  # ── Sparklines ───────────────────────────────────────────────────────────────
  # .spark_pct    → evolución del % s/total Madrid
  # .spark_vs_esp → evolución del % s/total España
  if (length(cols_pct_present) > 0) {
    df$.spark_pct <- apply(df[, cols_pct_present, drop = FALSE], 1,
                           function(r) list(as.numeric(r)))
  }
  if (length(cols_vs_esp_present) > 0) {
    df$.spark_vs_esp <- apply(df[, cols_vs_esp_present, drop = FALSE], 1,
                              function(r) list(as.numeric(r)))
  }
  
  df$.niv <- niv_vec
  
  # ── Orden de columnas ─────────────────────────────────────────────────────────
  col_order <- c(id_cols,
                 cols_pct_present,    ".spark_pct",
                 cols_vs_esp_present, ".spark_vs_esp",
                 ".niv")
  col_order <- col_order[col_order %in% names(df)]
  df        <- df[, col_order, drop = FALSE]
  
  # ── Helper: renderizar sparkline SVG ─────────────────────────────────────────
  make_spark_svg <- function(vals_list, col_pal, tam_fuente) {
    lapply(vals_list, function(vals) {
      vals <- unlist(vals)
      if (all(is.na(vals))) return("")
      n       <- length(vals)
      w_total <- 80; h_total <- 30; pad <- 3
      w_inner <- w_total - 2 * pad; h_inner <- h_total - 2 * pad
      mn  <- min(vals, na.rm = TRUE)
      mx  <- max(vals, na.rm = TRUE)
      rng <- if ((mx - mn) == 0) 1 else mx - mn
      xs  <- round(pad + (seq_along(vals) - 1) / (n - 1) * w_inner, 2)
      ys  <- round(pad + (1 - (vals - mn) / rng) * h_inner, 2)
      # Color: verde si último valor ≥ primer valor, rojo si no
      line_col   <- if (vals[n] >= vals[1]) col_pal$spark_pos %||% col_pal$bar_pos else col_pal$spark_neg %||% col_pal$bar_neg
      points_str <- paste(paste0(xs, ",", ys), collapse = " ")
      last_dot   <- paste0('<circle cx="', xs[n], '" cy="', ys[n],
                           '" r="2.5" fill="', line_col, '"/>')
      paste0('<svg width="', w_total, '" height="', h_total,
             '" xmlns="http://www.w3.org/2000/svg">',
             '<polyline points="', points_str,
             '" fill="none" stroke="', line_col,
             '" stroke-width="1.8" stroke-linejoin="round" stroke-linecap="round"/>',
             last_dot, '</svg>')
    })
  }
  
  # ── Construir gt ─────────────────────────────────────────────────────────────
  gt_tbl <- df |>
    gt(rowname_col = id_col) |>
    cols_hide(columns = any_of(c("orden", "niv", ".niv"))) |>
    tab_stubhead(label = if (tipo == "sectores") "Sector económico" else "País / Zona") |>
    tab_header(
      title = titulo %||% paste0(
        if (fl == "exp") "Exportaciones" else "Importaciones",
        " de Madrid \u2014 Estructura porcentual ", ab, "\u2013", af
      ),
      subtitle = subtitulo %||% paste0(
        "Cuota sobre total Madrid y cuota de Madrid sobre Espa\u00f1a ",
        ab, "\u2013", af, " por ",
        if (tipo == "sectores") "sector econ\u00f3mico" else "pa\u00eds / zona geogr\u00e1fica"
      )
    ) |>
    tab_source_note(source_note = caption) |>
    
    # ── Spanners ─────────────────────────────────────────────────────────────
    tab_spanner(
      label   = paste0("% s/total Madrid (", ab, "\u2013", af, ")"),
      columns = any_of(c(cols_pct_present, ".spark_pct"))
    ) |>
    tab_spanner(
      label   = paste0("% s/total España (", ab, "\u2013", af, ")"),
      columns = any_of(c(cols_vs_esp_present, ".spark_vs_esp"))
    ) |>
    
    # ── Etiquetas individuales ────────────────────────────────────────────────
    cols_label(
      .list = c(
        setNames(as.list(as.character(anos)), cols_pct_present),
        list(.spark_pct    = paste0("Tend.")),
        setNames(as.list(as.character(anos)), cols_vs_esp_present),
        list(.spark_vs_esp = paste0("Tend."))
      ) |> (\(l) l[names(l) %in% names(df)])()
    ) |>
    
    # ── Formatos ─────────────────────────────────────────────────────────────
    fmt_number(
      columns  = any_of(c(cols_pct_present, cols_vs_esp_present)),
      decimals = dec_pct, sep_mark = ".", dec_mark = ","
    ) |>
    cols_align(align = "center", columns = everything()) |>
    cols_align(align = "left",   columns = stub())
  
  # ── Sparklines ───────────────────────────────────────────────────────────────
  if (".spark_pct" %in% names(df)) {
    spark_pct_html <- make_spark_svg(df$.spark_pct, col_pal, tam_fuente)
    gt_tbl <- gt_tbl |>
      text_transform(
        locations = cells_body(columns = ".spark_pct"),
        fn = function(x) spark_pct_html
      )
  }
  if (".spark_vs_esp" %in% names(df)) {
    spark_vs_html <- make_spark_svg(df$.spark_vs_esp, col_pal, tam_fuente)
    gt_tbl <- gt_tbl |>
      text_transform(
        locations = cells_body(columns = ".spark_vs_esp"),
        fn = function(x) spark_vs_html
      )
  }
  
  # ── Opciones y estilos ───────────────────────────────────────────────────────
  gt_tbl <- gt_tbl |>
    tab_options(
      table.font.names               = fuente,
      table.font.size                = px(tam_fuente + 1),
      heading.background.color       = col_pal$heading_bg,
      column_labels.background.color = col_pal$labels_bg,
      table.border.top.style         = "none",
      data_row.padding               = px(4)
    ) |>
    tab_style(
      style     = cell_text(color = col_pal$heading_fg, weight = "bold", size = px(tam_fuente + 5)),
      locations = cells_title(groups = "title")
    ) |>
    tab_style(
      style = list(
        cell_fill(color = col_pal$niv3_bg),
        cell_text(color = col_pal$niv3_fg, size = px(tam_fuente + 2), style = "italic", align = "left")
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
        cell_text(color = col_pal$labels_fg, weight = "bold", v_align = "middle", align = "center")
      ),
      locations = list(cells_column_labels(), cells_column_spanners(), cells_stubhead())
    ) |>
    # Bordes de separación: stub | bloque pct+spark | bloque vs_esp+spark
    tab_style(
      style = cell_borders(sides = "right", color = col_pal$border, weight = px(1)),
      locations = list(
        cells_stub(),
        cells_stubhead(),
        cells_body(columns = any_of(".spark_pct")),
        cells_column_labels(columns = any_of(".spark_pct"))
      )
    ) |>
    # Nivel 0
    tab_style(
      style     = list(cell_fill(color = col_pal$niv0_bg), cell_text(weight = "bold", color = col_pal$niv0_fg, v_align = "middle")),
      locations = list(cells_body(rows = .niv == 0), cells_stub(rows = .niv == 0))
    ) |>
    # Nivel 1 y 9
    tab_style(
      style     = list(cell_fill(color = col_pal$niv1_bg), cell_text(weight = "bold", color = col_pal$niv1_fg, v_align = "middle")),
      locations = list(cells_body(rows = .niv %in% c(1, 9)), cells_stub(rows = .niv %in% c(1, 9)))
    ) |>
    # Nivel 2
    tab_style(
      style     = list(cell_fill(color = col_pal$niv2_bg), cell_text(color = col_pal$niv2_fg, v_align = "middle")),
      locations = list(cells_body(rows = .niv == 2), cells_stub(rows = .niv == 2))
    )
  
  if (any(niv_vec %in% c(3, 4))) {
    gt_tbl <- gt_tbl |>
      tab_style(
        style     = list(cell_fill(color = col_pal$niv3_bg), cell_text(color = col_pal$niv3_fg, style = "italic", v_align = "middle")),
        locations = list(cells_body(rows = .niv == 3), cells_stub(rows = .niv == 3))
      ) |>
      tab_style(
        style     = list(
          cell_fill(color = col_pal$niv4_bg),
          cell_text(color = col_pal$niv4_fg, style = "italic", size = px(tam_fuente), v_align = "middle")
        ),
        locations = list(cells_body(rows = .niv == 4), cells_stub(rows = .niv == 4))
      )
  }
  
  gt_tbl <- gt_tbl |>
    text_transform(
      locations = cells_stub(),
      fn = function(x) {
        sapply(seq_along(x), function(i) {
          indent <- switch(as.character(niv_vec[i]),
                           "2" = "&nbsp;&nbsp;&nbsp;",
                           "3" = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                           "4" = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                           "")
          paste0(indent, x[i])
        })
      }
    )
  
  gtsave(gt_tbl, filename = ruta_salida,
         vwidth = round(ancho_cm / 2.54 * dpi), zoom = dpi / 96)
  invisible(gt_tbl)
}

# ── 5. EVOLUCIÓN: EXPORTACIÓN A EXCEL (OPENXLSX) ──────────────────────────────
exportar_evol_excel <- function(
    tabla,
    flujo                = "exp",
    territorio           = "mad",
    tipo                 = "sectores",
    ano_base             = 2019L,
    ano_final            = 2025L,
    anos_mostrar         = 2019L:2025L,
    cols_millones_factor = 1e6,
    dec_num              = 1L,
    dec_pct              = 1L,
    titulo               = NULL,
    caption              = "Elaboración propia a partir de microdatos obtenidos de DataComex.",
    ruta_salida          = "./tabla_evol.xlsx",
    tam_fuente           = 8L,
    col_pal              = list(
      heading_bg = "#526DB0", heading_fg = "#F5C201",
      labels_bg  = "#F5C201", labels_fg  = "black",
      niv0_bg    = "#F5C201", niv1_bg    = "#B9C4DF",
      niv2_bg    = "#F2F2F2", niv3_bg    = "#FFFFFF",
      niv4_fg    = "#333333"
    )
) {
  df   <- as.data.frame(data.table::copy(tabla))
  fl   <- flujo
  ter  <- territorio
  pref <- paste0(fl, "_", ter)
  anos <- sort(anos_mostrar)
  ab   <- as.character(ano_base)
  af   <- as.character(ano_final)
  
  id_col  <- if (tipo == "sectores") "nombre" else "pais"
  id_cols <- if (tipo == "sectores") c("orden", "niv", "nombre") else c("orden", "niv", "pais")
  
  cols_vol    <- paste0(pref, "_", anos)
  cols_idx    <- paste0(pref, "_idx_", anos)
  col_contrib <- paste0(pref, "_contrib")
  col_tva     <- paste0(pref, "_tva_evol")
  
  col_raw_base  <- paste0(pref, "_", ab)
  col_raw_final <- paste0(pref, "_", af)
  if (col_raw_base %in% names(df) && col_raw_final %in% names(df)) {
    df[[col_tva]] <- ifelse(
      df[[col_raw_base]] != 0,
      (df[[col_raw_final]] - df[[col_raw_base]]) / abs(df[[col_raw_base]]) * 100,
      NA_real_
    )
  }
  
  all_need <- c(id_cols, cols_vol, col_tva, col_contrib, cols_idx)
  all_need <- all_need[all_need %in% names(df)]
  df       <- df[, all_need, drop = FALSE]
  
  niv_vector <- df$niv
  
  for (cv in cols_vol[cols_vol %in% names(df)])   df[[cv]] <- df[[cv]] / cols_millones_factor
  if (col_contrib %in% names(df))                 df[[col_contrib]] <- df[[col_contrib]] * 100
  
  # Cabeceras legibles
  cols_vol_present <- cols_vol[cols_vol %in% names(df)]
  cols_idx_present <- cols_idx[cols_idx %in% names(df)]
  header <- c(
    setNames(as.character(anos[seq_along(cols_vol_present)]), cols_vol_present),
    setNames("TVA (%)",      col_tva),
    setNames("Con. (p.p.)",  col_contrib),
    setNames(paste0("Idx ", anos[seq_along(cols_idx_present)]), cols_idx_present)
  )
  header <- header[names(header) %in% names(df)]
  names(df)[names(df) %in% names(header)] <- header[names(df)[names(df) %in% names(header)]]
  
  titulo_auto <- titulo %||% paste0(
    if (fl == "exp") "Exportaciones" else "Importaciones",
    " de ", if (ter == "mad") "Madrid" else "Espa\u00f1a",
    " \u2014 Evoluci\u00f3n ", ab, "\u2013", af
  )
  
  wb <- createWorkbook()
  sheet <- "Evolucion"
  addWorksheet(wb, sheet, gridLines = FALSE)
  
  st_titulo <- createStyle(fontSize = tam_fuente + 1, fontName = "Arial",
                           fontColour = col_pal$heading_fg, textDecoration = "bold",
                           fgFill = col_pal$heading_bg, halign = "center")
  st_niv0   <- createStyle(fontSize = tam_fuente, fontName = "Arial",
                           fontColour = "black", textDecoration = "bold",
                           fgFill = col_pal$niv0_bg, halign = "left")
  st_niv1   <- createStyle(fontSize = tam_fuente, fontName = "Arial",
                           fontColour = "black", textDecoration = "bold",
                           fgFill = col_pal$niv1_bg, halign = "left")
  st_niv2   <- createStyle(fontSize = tam_fuente, fontName = "Arial",
                           fontColour = "black", fgFill = col_pal$niv2_bg,
                           halign = "left", indent = 1)
  st_niv3   <- createStyle(fontSize = tam_fuente - 1, fontName = "Arial",
                           fontColour = "black", textDecoration = "italic",
                           halign = "left", indent = 2)
  st_niv4   <- createStyle(fontSize = tam_fuente - 1, fontName = "Arial",
                           fontColour = col_pal$niv4_fg %||% "#333333",
                           textDecoration = "italic", halign = "left", indent = 3)
  
  writeData(wb, sheet, titulo_auto, startRow = 1, startCol = 1)
  mergeCells(wb, sheet, rows = 1, cols = 1:ncol(df))
  addStyle(wb, sheet, st_titulo, rows = 1, cols = 1:ncol(df))
  
  # Fila de caption
  writeData(wb, sheet, caption, startRow = 2, startCol = 1)
  
  writeData(wb, sheet, df, startRow = 4)
  
  for (i in seq_len(nrow(df))) {
    style <- switch(as.character(niv_vector[i]),
                    "0" = st_niv0,
                    "1" = st_niv1, "9" = st_niv1,
                    "2" = st_niv2,
                    "3" = st_niv3,
                    "4" = st_niv4,
                    st_niv1)
    addStyle(wb, sheet, style, rows = i + 3, cols = 1:ncol(df), stack = TRUE)
  }
  
  fmt_num <- paste0("#,##0.", paste(rep("0", dec_num), collapse = ""))
  fmt_pct <- paste0("#,##0.", paste(rep("0", dec_pct), collapse = ""))
  
  # Columnas numéricas (volumen e índice) → fmt_num
  vol_idx_cols <- which(names(df) %in% c(
    as.character(anos[seq_along(cols_vol_present)]),
    paste0("Idx ", anos[seq_along(cols_idx_present)])
  ))
  if (length(vol_idx_cols) > 0)
    addStyle(wb, sheet, createStyle(numFmt = fmt_num),
             rows = 5:(nrow(df) + 4), cols = vol_idx_cols, gridExpand = TRUE, stack = TRUE)
  
  # Columnas porcentaje (TVA, contrib) → fmt_pct
  pct_cols_pos <- which(names(df) %in% c("TVA (%)", "Con. (p.p.)"))
  if (length(pct_cols_pos) > 0)
    addStyle(wb, sheet, createStyle(numFmt = fmt_pct),
             rows = 5:(nrow(df) + 4), cols = pct_cols_pos, gridExpand = TRUE, stack = TRUE)
  
  setColWidths(wb, sheet, cols = 1:ncol(df), widths = "auto")
  saveWorkbook(wb, ruta_salida, overwrite = TRUE)
  invisible(wb)
}


# ── 6. EVOLUCIÓN PCT: EXPORTACIÓN A EXCEL (OPENXLSX) ──────────────────────────
exportar_evol_pct_excel <- function(
    tabla,
    flujo        = "exp",
    tipo         = "sectores",
    anos_mostrar = 2019L:2025L,
    dec_pct      = 1L,
    titulo       = NULL,
    caption      = "Elaboración propia a partir de microdatos obtenidos de DataComex.",
    ruta_salida  = "./tabla_evol_pct.xlsx",
    tam_fuente   = 8L,
    col_pal      = list(
      heading_bg = "#526DB0", heading_fg = "#F5C201",
      labels_bg  = "#F5C201", labels_fg  = "black",
      niv0_bg    = "#F5C201", niv1_bg    = "#B9C4DF",
      niv2_bg    = "#F2F2F2", niv3_bg    = "#FFFFFF",
      niv4_fg    = "#333333"
    )
) {
  df   <- as.data.frame(data.table::copy(tabla))
  fl   <- flujo
  anos <- sort(anos_mostrar)
  ab   <- as.character(min(anos))
  af   <- as.character(max(anos))
  
  id_col  <- if (tipo == "sectores") "nombre" else "pais"
  id_cols <- if (tipo == "sectores") c("orden", "niv", "nombre") else c("orden", "niv", "pais")
  
  cols_pct    <- paste0(fl, "_mad_pct_",    anos)
  cols_vs_esp <- paste0(fl, "_mad_vs_esp_", anos)
  
  all_need <- c(id_cols, cols_pct, cols_vs_esp)
  all_need <- all_need[all_need %in% names(df)]
  df       <- df[, all_need, drop = FALSE]
  
  niv_vector <- df$niv
  
  cols_pct_present    <- cols_pct[cols_pct %in% names(df)]
  cols_vs_esp_present <- cols_vs_esp[cols_vs_esp %in% names(df)]
  for (col in c(cols_pct_present, cols_vs_esp_present)) df[[col]] <- df[[col]] * 100
  
  # Renombrar columnas con año legible + bloque
  names(df)[names(df) %in% cols_pct_present]    <- paste0("pct_",    anos[seq_along(cols_pct_present)])
  names(df)[names(df) %in% cols_vs_esp_present] <- paste0("vsEsp_",  anos[seq_along(cols_vs_esp_present)])
  
  titulo_auto <- titulo %||% paste0(
    if (fl == "exp") "Exportaciones" else "Importaciones",
    " de Madrid \u2014 Estructura porcentual ", ab, "\u2013", af
  )
  
  wb <- createWorkbook()
  sheet <- "EvolPct"
  addWorksheet(wb, sheet, gridLines = FALSE)
  
  st_titulo <- createStyle(fontSize = tam_fuente + 1, fontName = "Arial",
                           fontColour = col_pal$heading_fg, textDecoration = "bold",
                           fgFill = col_pal$heading_bg, halign = "center")
  st_niv0   <- createStyle(fontSize = tam_fuente, fontName = "Arial",
                           fontColour = "black", textDecoration = "bold",
                           fgFill = col_pal$niv0_bg, halign = "left")
  st_niv1   <- createStyle(fontSize = tam_fuente, fontName = "Arial",
                           fontColour = "black", textDecoration = "bold",
                           fgFill = col_pal$niv1_bg, halign = "left")
  st_niv2   <- createStyle(fontSize = tam_fuente, fontName = "Arial",
                           fontColour = "black", fgFill = col_pal$niv2_bg,
                           halign = "left", indent = 1)
  st_niv3   <- createStyle(fontSize = tam_fuente - 1, fontName = "Arial",
                           fontColour = "black", textDecoration = "italic",
                           halign = "left", indent = 2)
  st_niv4   <- createStyle(fontSize = tam_fuente - 1, fontName = "Arial",
                           fontColour = col_pal$niv4_fg %||% "#333333",
                           textDecoration = "italic", halign = "left", indent = 3)
  
  writeData(wb, sheet, titulo_auto, startRow = 1, startCol = 1)
  mergeCells(wb, sheet, rows = 1, cols = 1:ncol(df))
  addStyle(wb, sheet, st_titulo, rows = 1, cols = 1:ncol(df))
  
  writeData(wb, sheet, caption, startRow = 2, startCol = 1)
  writeData(wb, sheet, df, startRow = 4)
  
  for (i in seq_len(nrow(df))) {
    style <- switch(as.character(niv_vector[i]),
                    "0" = st_niv0,
                    "1" = st_niv1, "9" = st_niv1,
                    "2" = st_niv2,
                    "3" = st_niv3,
                    "4" = st_niv4,
                    st_niv1)
    addStyle(wb, sheet, style, rows = i + 3, cols = 1:ncol(df), stack = TRUE)
  }
  
  fmt_pct <- paste0("#,##0.", paste(rep("0", dec_pct), collapse = ""))
  pct_cols_pos <- which(grepl("^(pct_|vsEsp_)", names(df)))
  if (length(pct_cols_pos) > 0)
    addStyle(wb, sheet, createStyle(numFmt = fmt_pct),
             rows = 5:(nrow(df) + 4), cols = pct_cols_pos, gridExpand = TRUE, stack = TRUE)
  
  setColWidths(wb, sheet, cols = 1:ncol(df), widths = "auto")
  saveWorkbook(wb, ruta_salida, overwrite = TRUE)
  invisible(wb)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b