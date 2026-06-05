df_plot_barras_contribucion_datacomex <- function(
    df               = df_evol_countryfull,
    para             = paramets,
    totalesf         = totalesanho,  
    flujo            = "exp",
    region           = "esp",
    n_sectores       = 4L,
    meta             = meta_sec,
    dss_mad          = ds_mad,
    dss_esp          = ds_esp) {
  
  # ── Variables auxiliares ───────────────────────────────────────────────────
  N           <- para$max_bars_con
  col_ano     <- paste0(flujo, "_", region, "_", para$anho)
  col_anop    <- paste0(flujo, "_", region, "_", para$anho - 1)
  fil_flujo   <- ifelse(flujo == "exp", 1L, 0L)
  reg_buscada <- ifelse(region == "esp", "España", "Madrid")
  
  col_val_procesado  <- paste0(flujo, "_", region)
  col_total_bueno    <- paste0(flujo, "_prev_", region)
  
  # Extraemos el total previo global correcto desde totalesf
  val_total_prev_bueno <- totalesf[[col_total_bueno]]
  
  # ── Dataframes base ────────────────────────────────────────────────────────
  df <- df[, .(cod, pais, paisconcod, reg,
               valano  = get(col_ano),
               valanop = get(col_anop))]
  
  totales   <- as.list(df[cod == 0L])
  df_paises <- df[cod != 0L][, dif := valano - valanop]
  
  paises_pos <- df_paises[order(-dif)][1:N][
    , `:=`(
      tva    = 100 * dif / valanop,
      rep    = 100 * dif / totales$valanop,
      peso   = 100 * valano / totales$valano
    )
  ]
  
  paises_neg <- df_paises[order(dif)][1:N][
    , `:=`(
      tva    = 100 * dif / valanop,
      rep    = 100 * dif / totales$valanop,
      peso   = 100 * valano / totales$valano
    )
  ]
  
  lista_pos <- as.list(paises_pos$cod)
  lista_neg <- as.list(paises_neg$cod)
  
  para_pais <- para
  
  # ── Procesar Países Positivos ──────────────────────────────────────────────
  res_pos_list <- list()
  for (i in lista_pos) {
    para_pais$cod_pais <- i
    
    tabla_sectores_aux_paispos <- .tabla_sectores_datacomex(
      ds_mad     = dss_mad,
      ds_esp     = dss_esp,
      df_sec     = meta,
      parametros = para_pais
    )
    
    # CORRECCIÓN/MEJORA: Pasamos de forma consistente el 'totalesf' externo
    df_secpaispos <- .procesar_salida_sectores(
      tabla        = tabla_sectores_aux_paispos,
      listatotales = totalesf
    )
    
    col_contrib_mecanica <- paste0(flujo, "_", region, "_contrib")
    
    fil_sectores_plot <- c(1, 11, 15, 18, 24, 33, 34, 37, 40, 45, 50, 53, 58, 59, 65, 66)
    
    df_filtrado <- df_secpaispos[
      !orden %in% para$fil_sectores_plot
    ][order(-get(col_contrib_mecanica))][1:n_sectores]
    
    # Mapeo y suma de diferencias asegurando correspondencia por código de sector
    reg_tabla_aux <- ifelse(region == "esp", "España", "Madrid")
    col_dif_aux   <- paste0(flujo, "_dif")
    
    total_dif_calc <- sum(
      tabla_sectores_aux_paispos[region == reg_tabla_aux & orden %in% df_filtrado$orden, get(col_dif_aux)], 
      na.rm = TRUE
    )
    
    res_pos_list[[as.character(i)]] <- df_filtrado[, .(
      cod           = i,
      sectores      = paste(nombre, collapse = ", "),
      total_dif     = total_dif_calc,
      rep_sectores  = 100 * total_dif_calc / val_total_prev_bueno
    )]
  }
  df_sectores_pos <- data.table::rbindlist(res_pos_list)
  
  # ── Procesar Países Negativos ──────────────────────────────────────────────
  res_neg_list <- list()
  for (i in lista_neg) {
    para_pais$cod_pais <- i
    
    tabla_sectores_aux_paisneg <- .tabla_sectores_datacomex(
      ds_mad     = dss_mad,
      ds_esp     = dss_esp,
      df_sec     = meta,
      parametros = para_pais
    )
    
    # CORRECCIÓN: Cambiado 'totalef' por el parámetro correcto 'totalesf'
    df_secpaisneg <- .procesar_salida_sectores(
      tabla        = tabla_sectores_aux_paisneg,
      listatotales = totalesf
    )
    
    col_contrib_mecanica <- paste0(flujo, "_", region, "_contrib")
    
    df_filtrado_neg <- df_secpaisneg[
      orden < 65 & niv >= 2
    ][order(get(col_contrib_mecanica))][1:n_sectores]
    
    reg_tabla_aux <- ifelse(region == "esp", "España", "Madrid")
    col_dif_aux   <- paste0(flujo, "_dif")
    
    total_dif_calc_neg <- sum(
      tabla_sectores_aux_paisneg[region == reg_tabla_aux & orden %in% df_filtrado_neg$orden, get(col_dif_aux)], 
      na.rm = TRUE
    )
    
    res_neg_list[[as.character(i)]] <- df_filtrado_neg[, .(
      cod           = i,
      sectores      = paste(nombre, collapse = ", "),
      total_dif     = total_dif_calc_neg,
      rep_sectores  = 100 * total_dif_calc_neg / val_total_prev_bueno
    )]
  }
  df_sectores_neg <- data.table::rbindlist(res_neg_list)
  
  # ── Uniones Finales ────────────────────────────────────────────────────────
  paises_pos_final <- df_sectores_pos[paises_pos, on = "cod"]
  paises_neg_final <- df_sectores_neg[paises_neg, on = "cod"]
  
  chart_final <- data.table::rbindlist(list(paises_pos_final, paises_neg_final))
  chart_final <- chart_final[order(-rep)]
  
  return(chart_final)
}

df_chart <- df_plot_barras_contribucion_datacomex(
  df         = df_evol_countryfull,
  para       = paramets,
  totalesf   = totalesanho,    
  flujo      = "exp",
  region     = "esp",
  n_sectores = 3L,
  meta       = meta_sec,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)