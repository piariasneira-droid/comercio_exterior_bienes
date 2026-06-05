df_plot_barras_contribucion_datacomex <- function(
    df               = df_evol_countryfull,
    para             = paramets,
    flujo            = "exp",
    region           = "esp",
    n_sectores       = 4L,
    meta             = meta_sec,
    dsmad            = ds_mad,
    dsesp            = ds_esp) {
  
  # ── Helpers ----------------------------------------------------------------
  
  # Para cada país: reutiliza la pipeline oficial de sectores
  # cambiando solo cod_pais en los parámetros
  .get_sector_breakdown_pais <- function(cod_pais_i) {
    para_pais <- modifyList(para, list(cod_pais = cod_pais_i))
    
    tabla <- .tabla_sectores_datacomex(
      ds_mad     = dsmad,
      ds_esp     = dsesp,
      df_sec     = meta,
      parametros = para_pais
    )
    totales_pais <- .extraer_totales_de_tabla(tabla)
    
    df_sec <- .procesar_salida_sectores(
      tabla        = tabla,
      listatotales = totales_pais
    )
    
    # Nos quedamos solo con Madrid, nivel que corresponde y flujo correcto
    col_contrib <- if (flujo == "exp") "exp_mad_contrib" else "imp_mad_contrib"
    
    df_sec[niv >= 1 & niv <=5, .(
      pais    = cod_pais_i,
      nombre,
      contrib = get(col_contrib)  
    )]
  }
  
  .build_chart_df <- function(paises_dt, df_sector, signo = c("pos", "neg")) {
    signo <- match.arg(signo)
    
    sector_top <- df_sector |>
      dplyr::slice_max(
        order_by  = if (signo == "pos") contrib else -contrib,
        n         = n_sectores,
        by        = pais,
        with_ties = FALSE
      )
    
    sector_agg <- sector_top |>
      dplyr::summarise(
        rep_sectores   = sum(contrib, na.rm = TRUE),
        label_sectores = paste(nombre, collapse = ", "),
        .by = pais
      )
    
    paises_dt |>
      dplyr::left_join(sector_agg, by = c("cod" = "pais")) |>
      dplyr::mutate(signo = signo)
  }
  
  # ── Variables auxiliares ---------------------------------------------------
  N         <- para$max_bars_con
  col_ano   <- paste0(flujo, "_", region, "_", para$anho)
  col_anop  <- paste0(flujo, "_", region, "_", para$anho - 1)
  fil_flujo <- ifelse(flujo == "exp", 1L, 0L)
  
  # ── Dataframes base --------------------------------------------------------
  df <- df[, .(cod, pais, paisconcod, reg,
               valano  = get(col_ano),
               valanop = get(col_anop))]
  
  totales   <- as.list(df[cod == 0L])
  df_paises <- df[cod != 0L][, dif := valano - valanop]
  
  paises_pos <- df_paises[order(-dif)][1:N][
    , `:=`(
      tva    = 100 * dif / valanop,
      rep    = 100 * dif / totales$valanop,
      peso   = 100 * valano / totales$valano,
      sector = "0"
    )
  ]
  
  paises_neg <- df_paises[order(dif)][1:N][
    , `:=`(
      tva    = 100 * dif / valanop,
      rep    = 100 * dif / totales$valanop,
      peso   = 100 * valano / totales$valano,
      sector = "0"
    )
  ]
  
  # ── Sectores por país: una llamada por país --------------------------------
  df_pos_sector <<- data.table::rbindlist(
    lapply(unique(paises_pos$cod), .get_sector_breakdown_pais)
  )
  df_neg_sector <<- data.table::rbindlist(
    lapply(unique(paises_neg$cod), .get_sector_breakdown_pais)
  )
  
  # ── Chart dfs -------------------------------------------------------------
  chart_pos <- .build_chart_df(paises_pos, df_pos_sector, signo = "pos")
  chart_neg <- .build_chart_df(paises_neg, df_neg_sector, signo = "neg")
  
  dplyr::bind_rows(chart_pos, chart_neg) |>
    dplyr::arrange(dplyr::desc(rep))
}


df_chart <- df_plot_barras_contribucion_datacomex(
  df               = df_evol_countryfull,
  para             = paramets,
  flujo            = "exp",
  region           = "esp",
  n_sectores       = 3L,
  meta             = meta_sec,
  dsmad            = dsmad,
  dsesp            = dsesp
)

