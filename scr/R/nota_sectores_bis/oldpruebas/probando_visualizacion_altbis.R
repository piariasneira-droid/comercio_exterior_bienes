df_plot_barras_contribucion_datacomex <- function(
    df               = df_evol_countryfull,
    para             = paramets,
    flujo            = "exp",
    region           = "esp",
    n_sectores       = 4L,
    niv_fil          = 2L,
    meta             = meta_sec,
    dss_mad          = ds_mad,
    dss_esp          = ds_esp) {
  
  # ── Variables auxiliares 
  N           <- para$max_bars_con
  col_ano     <- paste0(flujo, "_", region, "_", para$anho)
  col_anop    <- paste0(flujo, "_", region, "_", para$anho - 1)
  fil_flujo   <- ifelse(flujo == "exp", 1L, 0L)
  reg_buscada <- ifelse(region == "esp", "España", "Madrid")
  col_dif     <- paste0(flujo, "_dif")
  
  # ── Dataframes base 
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
  
  # Aseguramos que las listas extraigan los códigos crudos (deslistados si hace falta)
  lista_pos <- as.list(paises_pos$cod)
  lista_neg <- as.list(paises_neg$cod)
  
  para_pais <- para
  
  # Procesar Países Positivos 
  res_pos_list <- list()
  for (i in lista_pos) {
    para_pais$cod_pais <- i
    
    df_secpaispos <- .tabla_sectores_f(
      ds_mad     = dss_mad,
      ds_esp     = dss_esp,
      df_sec     = meta, 
      parametros = para_pais
    )
    
    # Orden descendente (de mayor a menor diferencia)
    df_filtrado <- df_secpaispos[
      region == reg_buscada & orden < 65 & niv %in% niv_fil
    ][order(-get(col_dif))][1:n_sectores]
    
    res_pos_list[[as.character(i)]] <- df_filtrado[, .(
      cod          = i,
      sectores     = paste(nombre, collapse = ", "),
      total_dif    = sum(get(col_dif), na.rm = TRUE)
    )]
  }
  # Combinamos todos los países positivos en un único data.table
  df_sectores_pos <- data.table::rbindlist(res_pos_list)
  
  # Procesar Países Negativos -
  res_neg_list <- list()
  for (i in lista_neg) {
    para_pais$cod_pais <- i
    
    df_secpaisneg <- .tabla_sectores_f(
      ds_mad     = dss_mad,
      ds_esp     = dss_esp,
      df_sec     = meta, 
      parametros = para_pais
    )
    
    # Orden ascendente (de más negativo a menos negativo para capturar las caídas)
    df_filtrado_neg <- df_secpaisneg[
      region == reg_buscada & orden < 65 & niv>= 2
    ][order(get(col_dif))][1:n_sectores]
    
    res_neg_list[[as.character(i)]] <- df_filtrado_neg[, .(
      cod          = i,
      sectores     = paste(nombre, collapse = ", "),
      total_dif    = sum(get(col_dif), na.rm = TRUE)
    )]
  }
  
  # Combinamos todos los países negativos en un único data.table
  df_sectores_neg <- data.table::rbindlist(res_neg_list)
  
  # ── Uniones Finales (Equivalente al left_join por 'cod') 
  paises_pos_final <- df_sectores_pos[paises_pos, on = "cod"]
  paises_neg_final <- df_sectores_neg[paises_neg, on = "cod"]
  
  # Juntamos ambos bloques en el dataframe maestro listo para el gráfico
  chart_final <- data.table::rbindlist(list(paises_pos_final, paises_neg_final))
  chart_final[, rep_sectores := 100 * total_dif / totales$valanop]
  chart_final <- chart_final[order(-rep)]
  
  
  return(chart_final)
}

# Ejecución del script
df_chart <- df_plot_barras_contribucion_datacomex(
  df         = df_evol_countryfull,
  para       = paramets,
  flujo      = "exp",
  region     = "esp",
  n_sectores = 3L,
  niv_fil    = 2L,
  meta       = meta_sec,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)