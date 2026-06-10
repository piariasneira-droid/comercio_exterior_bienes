# main_texts_bis.R
# ============================================================
# Generación de listas de texto para el informe
# ============================================================
# CAMBIOS respecto a main_texts.R:
#   1. Los filtros de ranking usan max(paramets$mes) en lugar
#      de paramets$mes directamente, lo que funciona tanto con
#      mes escalar como con vector (4L:6L para Q2)
# ============================================================

# Entorno ----
source("./scr/R/nota_sectores_bis/procfun/funciones_text.r")

# mes de referencia para los rankings (último mes del periodo)
.mes_ref <- max(paramets$mes)

if (isTRUE(paramets$flag_ccaa)) {

  # Listas generales (dependen de df_ccaas, solo disponible si flag_ccaa) ----
  lista_esp <- as.list(df_ccaas[Etiqueta == "ESP"])
  lista_mad <- as.list(df_ccaas[Etiqueta == "CM"])

  ## Rankings Madrid ----
  lista_mad$exp_rankmes    <- df_mad_rank[flujo == "EXPORT" & Año == paramets$anho & Mes == .mes_ref]$rank_mes
  lista_mad$exp_rankmesacu <- df_mad_rank[flujo == "EXPORT" & Año == paramets$anho & Mes == .mes_ref]$rank_ytd
  lista_mad$imp_rankmes    <- df_mad_rank[flujo == "IMPORT" & Año == paramets$anho & Mes == .mes_ref]$rank_mes
  lista_mad$imp_rankmesacu <- df_mad_rank[flujo == "IMPORT" & Año == paramets$anho & Mes == .mes_ref]$rank_ytd

  lista_mad$exp_rank_hist_txt <- .fmt_rank_hist(
    rank  = lista_mad$exp_rankmes,
    anhos = df_mad_rank[flujo == "EXPORT" & rank_mes < lista_mad$exp_rankmes][order(rank_mes)]$Año
  )
  lista_mad$exp_rank_hist_ytd_txt <- .fmt_rank_hist(
    rank  = lista_mad$exp_rankmesacu,
    anhos = df_mad_rank[flujo == "EXPORT" & rank_ytd < lista_mad$exp_rankmesacu][order(rank_ytd)]$Año
  )
  lista_mad$imp_rank_hist_txt <- .fmt_rank_hist(
    rank  = lista_mad$imp_rankmes,
    anhos = df_mad_rank[flujo == "IMPORT" & rank_mes < lista_mad$imp_rankmes][order(rank_mes)]$Año
  )
  lista_mad$imp_rank_hist_ytd_txt <- .fmt_rank_hist(
    rank  = lista_mad$imp_rankmesacu,
    anhos = df_mad_rank[flujo == "IMPORT" & rank_ytd < lista_mad$imp_rankmesacu][order(rank_ytd)]$Año
  )

  ## Rankings España ----
  lista_esp$exp_rankmes    <- df_esp_rank[flujo == "EXPORT" & Año == paramets$anho & Mes == .mes_ref]$rank_mes
  lista_esp$exp_rankmesacu <- df_esp_rank[flujo == "EXPORT" & Año == paramets$anho & Mes == .mes_ref]$rank_ytd
  lista_esp$imp_rankmes    <- df_esp_rank[flujo == "IMPORT" & Año == paramets$anho & Mes == .mes_ref]$rank_mes
  lista_esp$imp_rankmesacu <- df_esp_rank[flujo == "IMPORT" & Año == paramets$anho & Mes == .mes_ref]$rank_ytd

  ## Texto ccaas ----
  lista_texto_ccaas <- local({

    df <- df_ccaas[Coddax >= 1 & Coddax <= 17]

    n_exp_inc <- sum(df$exp_euros_dif > 0)
    n_imp_inc <- sum(df$imp_euros_dif > 0)

    list(
      n_exp_inc   = n_exp_inc,
      n_imp_inc   = n_imp_inc,
      lbl_exp_inc = paste0("**", n_exp_inc, "** ", ifelse(n_exp_inc == 1,
                                                          "comunidad autónoma aumentó sus",
                                                          "comunidades autónomas aumentaron sus")),
      lbl_imp_inc = paste0("**", n_imp_inc, "** ", ifelse(n_imp_inc == 1,
                                                          "registró un incremento",
                                                          "registraron incrementos")),
      exp_pos     = .fmt_top3(df[order(-exp_euros_rep)][1:3], "Región", "exp_euros_rep"),
      exp_neg     = .fmt_top3(df[order(exp_euros_rep)][1:3],  "Región", "exp_euros_rep"),
      imp_pos     = .fmt_top3(df[order(-imp_euros_rep)][1:3], "Región", "imp_euros_rep"),
      imp_neg     = .fmt_top3(df[order(imp_euros_rep)][1:3],  "Región", "imp_euros_rep")
    )
  })

} else {
  message("[texts] flag_ccaa = FALSE: rankings y texto de CC.AA. omitidos.")
}

## Texto sectores y subsectores ----
lista_texto_sectores <- local({

  df  <- df_sectores[niv == 1L]
  dfa <- df_sectores_acu[niv == 1L]

  list(
    # Mes — Madrid
    exp_pos     = .top3_pos(df,  "exp_mad_contrib"),
    exp_neg     = .top3_neg(df,  "exp_mad_contrib"),
    imp_pos     = .top3_pos(df,  "imp_mad_contrib"),
    imp_neg     = .top3_neg(df,  "imp_mad_contrib"),

    # Mes — España
    exp_pos_esp = .top3_pos(df,  "exp_esp_contrib"),
    exp_neg_esp = .top3_neg(df,  "exp_esp_contrib"),
    imp_pos_esp = .top3_pos(df,  "imp_esp_contrib"),
    imp_neg_esp = .top3_neg(df,  "imp_esp_contrib"),

    # Acumulado — Madrid
    exp_pos_acu = .top3_pos(dfa, "exp_mad_contrib"),
    exp_neg_acu = .top3_neg(dfa, "exp_mad_contrib"),
    imp_pos_acu = .top3_pos(dfa, "imp_mad_contrib"),
    imp_neg_acu = .top3_neg(dfa, "imp_mad_contrib"),

    # Subsectores mes (rep ya en p.p.)
    subsec_exp_pos = .fmt_top3_sectores(
      df_contrib_sec_exp_informe[rep > 0][order(-rep)][1:min(3L, .N),], "nombre", "rep"),
    subsec_exp_neg = .fmt_top3_sectores(
      df_contrib_sec_exp_informe[rep < 0][order(rep)][1:min(3L, .N),],  "nombre", "rep"),
    subsec_imp_pos = .fmt_top3_sectores(
      df_contrib_sec_imp_informe[rep > 0][order(-rep)][1:min(3L, .N),], "nombre", "rep"),
    subsec_imp_neg = .fmt_top3_sectores(
      df_contrib_sec_imp_informe[rep < 0][order(rep)][1:min(3L, .N),],  "nombre", "rep")
  )
})

## Texto países ----
lista_texto_paises <- local({
  list(
    # Mes
    exp_pos     = .top3_pos_pais(df_contrib_paises_exp_informe),
    exp_neg     = .top3_neg_pais(df_contrib_paises_exp_informe),
    imp_pos     = .top3_pos_pais(df_contrib_paises_imp_informe),
    imp_neg     = .top3_neg_pais(df_contrib_paises_imp_informe),

    # Acumulado
    exp_pos_acu = .top3_pos_pais(df_contrib_paises_exp_informe_acu),
    exp_neg_acu = .top3_neg_pais(df_contrib_paises_exp_informe_acu),
    imp_pos_acu = .top3_pos_pais(df_contrib_paises_imp_informe_acu),
    imp_neg_acu = .top3_neg_pais(df_contrib_paises_imp_informe_acu)
  )
})