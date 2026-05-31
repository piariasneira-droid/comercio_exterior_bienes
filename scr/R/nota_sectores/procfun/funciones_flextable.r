# Prepare DATAFRAMES ----
.prepare_flextable_ccaas <- function(df) {
  df <- df[, `:=`(
    c1 = Etiqueta,
    # EXPORTS (monthly)
    c2 = if_else(
      Etiqueta == "ESP",
      format(round(exp_euros / 1e6, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
      paste0(
        format(round(exp_euros / 1e6, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
        " (",
        format(round(exp_euros_peso, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
        ")"
      )
    ),
    c3 = if_else(Etiqueta == "ESP", NA_integer_, exp_euros_rank),
    c4 = if_else(
      Etiqueta == "ESP",
      paste0(format(round(exp_euros_tva, 1), nsmall = 1, decimal.mark = ",", big.mark = "."), "%"),
      paste0(
        format(round(exp_euros_tva, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
        "% (",
        format(round(exp_euros_rep, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
        ")"
      )
    ),
    # IMPORTS (monthly)
    c5 = if_else(
      Etiqueta == "ESP",
      format(round(imp_euros / 1e6, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
      paste0(
        format(round(imp_euros / 1e6, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
        " (",
        format(round(imp_euros_peso, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
        ")"
      )
    ),
    c6 = if_else(Etiqueta == "ESP", NA_integer_, imp_euros_rank),
    c7 = if_else(
      Etiqueta == "ESP",
      paste0(format(round(imp_euros_tva, 1), nsmall = 1, decimal.mark = ",", big.mark = "."), "%"),
      paste0(
        format(round(imp_euros_tva, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
        "% (",
        format(round(imp_euros_rep, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
        ")"
      )
    ),
    # Coverage ratio (monthly)
    c8 = format(
      round((exp_euros / imp_euros) * 100, 1),
      nsmall = 1, decimal.mark = ",", big.mark = "."
    ),
    # EXPORTS (YTM)
    c9 = if_else(
      Etiqueta == "ESP",
      format(round(exp_euros_acu / 1e6, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
      paste0(
        format(round(exp_euros_acu / 1e6, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
        " (",
        format(round(exp_euros_acu_peso, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
        ")"
      )
    ),
    c10 = if_else(Etiqueta == "ESP", NA_integer_, exp_euros_acu_rank),
    c11 = if_else(
      Etiqueta == "ESP",
      paste0(format(round(exp_euros_acu_tva, 1), nsmall = 1, decimal.mark = ",", big.mark = "."), "%"),
      paste0(
        format(round(exp_euros_acu_tva, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
        "% (",
        format(round(exp_euros_acu_rep, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
        ")"
      )
    ),
    # IMPORTS (YTM)
    c12 = if_else(
      Etiqueta == "ESP",
      format(round(imp_euros_acu / 1e6, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
      paste0(
        format(round(imp_euros_acu / 1e6, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
        " (",
        format(round(imp_euros_acu_peso, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
        ")"
      )
    ),
    c13 = if_else(Etiqueta == "ESP", NA_integer_, imp_euros_acu_rank),
    c14 = if_else(
      Etiqueta == "ESP",
      paste0(format(round(imp_euros_acu_tva, 1), nsmall = 1, decimal.mark = ",", big.mark = "."), "%"),
      paste0(
        format(round(imp_euros_acu_tva, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
        "% (",
        format(round(imp_euros_acu_rep, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),
        ")"
      )
    ),
    # Coverage ratio (YTM)
    c15 = format(
      round((exp_euros_acu / imp_euros_acu) * 100, 1),
      nsmall = 1, decimal.mark = ",", big.mark = "."
    )
  )]
  
  df
}


# RENDER TABLES ----
# ── Main flextable builder ────────────────────────────────────────────────────
#
# Args:
#   df   : data.table / data.frame with columns c1..c15
#   para : paramets list; uses $colpal1, $meses, $anho, $fuente_texto
#
.make_flextable_ccaa <- function(df, para = list(colpal1 = "#2d5532")) {
  
  # ── Colours ────────────────────────────────────────────────────────────────
  col_title  <- para$colpal1   
  col_white  <- "#FFFFFF"      
  col_text   <- para$colpal1   
  col_hi_row <- para$colpal2     
  col_stripe <- "#F5F5F5"      
  font_name  <- if (!is.null(para$fuente_texto)) para$fuente_texto else "Calibri"
  
  # ── Period labels (via helper) ─────────────────────────────────────────────
  periods    <- .build_period_labels(para)
  mes_label  <- periods$mes_label   
  acu_label  <- periods$acu_label   #
  
  # ── 1. Base flextable ──────────────────────────────────────────────────────
  ft <- flextable(as.data.frame(df))
  
  # ── 2. Column labels (bottom header row, i = 4 after spanners added) ───────
  ft <- set_header_labels(ft,
                          c1  = "",
                          c2  = "Vol. (%/total)", c3  = "RK", c4  = "TVA (con)",
                          c5  = "Vol. (%/total)", c6  = "RK", c7  = "TVA (con)",
                          c8  = "TC",
                          c9  = "Vol. (%/total)", c10 = "RK", c11 = "TVA (con)",
                          c12 = "Vol. (%/total)", c13 = "RK", c14 = "TVA (con)",
                          c15 = "TC"
  )
  
  # ── 3. Spanner rows ────────────────────────────────────────────────────────
  # Each add_header_row(top=TRUE) becomes the new row 1; prior rows shift down.
  # Final order after all three calls:
  #   i=1  title
  #   i=2  period (mes_label / acu_label)
  #   i=3  Exportaciones / Importaciones
  #   i=4  Vol. / RK / TVA / TC  (set_header_labels row)
  
  # i=3 after insertion — Exportaciones / Importaciones
  # colwidths: 1+3+3+1+3+3+1 = 15
  ft <- add_header_row(ft, top = TRUE,
                       values    = c("", "Exportaciones", "Importaciones", "TC",
                                     "Exportaciones", "Importaciones", "TC"),
                       colwidths = c(1,   3,               3,               1,
                                     3,               3,               1)
  )
  
  # i=2 after insertion — period labels
  # colwidths: 1+7+7 = 15
  ft <- add_header_row(ft, top = TRUE,
                       values    = c("", mes_label, acu_label),
                       colwidths = c(1,  7,         7)
  )
  
  # i=1 — title (uppercase, matches the month label style)
  ft <- add_header_row(ft, top = TRUE,
                       values    = "COMERCIO EXTERIOR POR COMUNIDADES AUT\u00d3NOMAS",
                       colwidths = 15
  )
  
  # ── 4. Header styling — row by row ────────────────────────────────────────
  
  # Row 1: title — colpal1 background, white bold text, size 10
  ft <- bg(ft,       part = "header", i = 1, bg = col_title)
  ft <- color(ft,    part = "header", i = 1, color = col_white)
  ft <- bold(ft,     part = "header", i = 1)
  ft <- fontsize(ft, part = "header", i = 1, size = 10)
  ft <- align(ft,    part = "header", i = 1, align = "center")
  
  # Rows 2-4: white background, colpal1 bold text
  ft <- bg(ft,    part = "header", i = 2:4, bg = col_white)
  ft <- color(ft, part = "header", i = 2:4, color = col_text)
  ft <- bold(ft,  part = "header", i = 2:4)
  ft <- align(ft, part = "header", i = 2:4, align = "center")
  
  # Row 2 (period): size 10
  ft <- fontsize(ft, part = "header", i = 2, size = 10)
  
  # Rows 3-4 (Exp/Imp + column labels): size 8
  ft <- fontsize(ft, part = "header", i = 3:4, size = 8)
  
  # ── 5. Body alignment ──────────────────────────────────────────────────────
  ft <- align(ft, part = "body", j = 1,                           align = "left")
  ft <- align(ft, part = "body", j = c(2,4,5,7,8,9,11,12,14,15), align = "right")
  ft <- align(ft, part = "body", j = c(3,6,10,13),                align = "center")
  
  # ── 6. Highlight rows: ESP (row 1) and CM (dynamic) ───────────────────────
  hi_rows <- c(1L, which(df$c1 == "CM"))
  ft <- bold(ft, part = "body", i = hi_rows)
  ft <- bg(ft,   part = "body", i = hi_rows, bg = col_hi_row)
  
  # ── 7. Alternating stripe on remaining even rows ───────────────────────────
  even_rows <- setdiff(seq(2L, nrow(df), by = 2L), hi_rows)
  if (length(even_rows))
    ft <- bg(ft, part = "body", i = even_rows, bg = col_stripe)
  
  # ── 8. Font (all parts) and sizes ─────────────────────────────────────────
  ft <- font(ft,     part = "all",    fontname = font_name)
  ft <- fontsize(ft, part = "body",   size = 7)
  ft <- fontsize(ft, part = "footer", size = 7)
  
  # ── 9. Row height ──────────────────────────────────────────────────────────
  ft <- height_all(ft, height = 0.42, part = "body",   unit = "cm")
  ft <- height_all(ft, height = 0.50, part = "header", unit = "cm")
  ft <- hrule(ft, rule = "exact", part = "body")
  ft <- hrule(ft, rule = "exact", part = "header")
  
  # ── 10. Cell padding ───────────────────────────────────────────────────────
  ft <- padding(ft, part = "all",
                padding.top    = 1, padding.bottom = 1,
                padding.left   = 2, padding.right  = 2)
  
  # ── 11. Borders ────────────────────────────────────────────────────────────
  thin_border  <- fp_border(color = "#AAAAAA", width = 0.5)
  outer_border <- fp_border(color = col_title, width = 1.5)
  sep_border   <- fp_border(color = col_title, width = 0.5)  # header row seps
  
  ft <- border_inner_h(ft, border = thin_border,  part = "body")
  ft <- border_inner_v(ft, border = thin_border,  part = "body")
  ft <- border_outer(ft,   border = outer_border)
  
  # Thin colpal1 line between header rows 1-2, 2-3, 3-4
  ft <- hline(ft, part = "header", i = c(1, 2, 3), border = sep_border)
  
  # ── 12. Anchos fijos (18 cm total, layout fixed) ──────────────────────────
  anchos_cm <- c(
    0.7,                      # c1  CCAA (siglas)
    1.8, 0.5, 1.6,            # c2-c4  Mensual Exp: Vol, RK, TVA
    1.8, 0.5, 1.6, 0.9,      # c5-c8  Mensual Imp: Vol, RK, TVA, TC
    1.8, 0.5, 1.6,            # c9-c11 Acumulado Exp: Vol, RK, TVA
    1.8, 0.5, 1.6, 0.9       # c12-c15 Acumulado Imp: Vol, RK, TVA, TC
  )
  # Verificación: sum(anchos_cm) == 18
  ft <- width(ft, width = anchos_cm / 2.54)   # cm → pulgadas (unidad de flextable)
  ft <- set_table_properties(ft, layout = "fixed")
  
  # ── 13. Footer note ────────────────────────────────────────────────────────
  ft <- add_footer_lines(ft, values = paste0(
    "Vol.: Volumen en millones de euros. RK: R\u00e1nking; ",
    "TVA: Tasa de variaci\u00f3n anual (%); TC: Tasa de cobertura (%). ",
    "Con.: Contribuci\u00f3n TVA Espa\u00f1a en p.p. Fuente: AEAT"
  ))
  ft <- font(ft,     part = "footer", fontname = font_name)
  ft <- color(ft,    part = "footer", color = "#555555")
  ft <- italic(ft,   part = "footer")
  ft <- fontsize(ft, part = "footer", size = 7)
  ft <- align(ft,    part = "footer", align = "left")
  
  ft
}