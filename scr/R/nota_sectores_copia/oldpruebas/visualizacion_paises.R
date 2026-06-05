.df_plot_barras_contribucion_paises_datacomex <- function(
    df               = df_sectores[!orden %in% paramets$fil_sectores_plot],
    para             = paramets,
    totalesf         = totalesanho,  
    flujo            = "exp",
    region           = "esp",
    metas            = meta_sec,
    metap            = meta_pais,
    dss_mad          = ds_mad,
    dss_esp          = ds_esp) {
  
  # Helper 
  clean_listasec <- function(x) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
    x <- lapply(x, function(z) {
      if (is.character(z) && grepl("^c\\(", z)) return(eval(parse(text = z)))
      return(z)
    })
    unique(as.character(unlist(x)))
  }
  
  # Vars aux
  N            <- para$max_bars_con
  col_dif      <- paste0(flujo, "_dif")
  col_tva      <- paste0(flujo, "_", region, "_tva")
  col_rep      <- paste0(flujo, "_", region, "_contrib")
  
  val_total_prev_bueno <- totalesf[[paste0(flujo, "_prev_", region)]]
  
  # Sectores pos/neg desde df_sectores
  df_base <- df[, .(
    orden, niv, nombre,
    tva = 100 * get(col_tva),
    rep = 100 * get(col_rep)
  )]
  
  sectores_pos <- df_base[order(-rep)][1:N]
  sectores_neg <- df_base[order(rep)][1:N]
  
  lista_pos <- sectores_pos$nombre
  lista_neg <- sectores_neg$nombre
  
  # Diccionario nombre → vector de cod_sec
  map_nombre_sec <- metas[
    !is.na(nombre),
    .(listasec = list(clean_listasec(listasec))),
    by = nombre
  ]
  dict_sec <- setNames(map_nombre_sec$listasec, map_nombre_sec$nombre)
  
  # Helper: llama a .tabla_paises_f para un nombre de sector y devuelve
  # los n_subsec_plotpais países más destacados (pos o neg) con sus métricas
  procesar_sector <- function(nombre_sec, orden_sec, dir_pos) {
    
    parasec <- para
    parasec$cod_sector <- as.character(unlist(dict_sec[[nombre_sec]]))
    
    df_country <- .tabla_paises_f(
      ds_mad     = dss_mad,
      ds_esp     = dss_esp,
      df_paises  = metap,
      parametros = parasec
    )
    
    reg_label <- ifelse(region == "esp", "España", "Madrid")
    
    df_paises_fil <- df_country[region == reg_label & cod > 0 & cod < 1000]
    
    df_top <- if (dir_pos) {
      df_paises_fil[order(-get(col_dif))][1:para$n_paises_plotsectores]
    } else {
      df_paises_fil[order(get(col_dif))][1:para$n_paises_plotsectores]
    }
    
    total_dif <- sum(df_top[[col_dif]], na.rm = TRUE)
    
    data.table::data.table(
      orden      = orden_sec,
      nombre     = nombre_sec,
      paises     = paste(df_top$pais, collapse = ", "),
      total_dif  = total_dif,
      rep_paises = 100 * total_dif / val_total_prev_bueno
    )
  }
  
  # ── Bucle positivos ───────────────────────────────────────────────────────
  res_pos_list <- vector("list", length(lista_pos))
  for (k in seq_along(lista_pos)) {
    res_pos_list[[k]] <- procesar_sector(lista_pos[k], sectores_pos$orden[k], dir_pos = TRUE)
  }
  
  # ── Bucle negativos ───────────────────────────────────────────────────────
  res_neg_list <- vector("list", length(lista_neg))
  for (k in seq_along(lista_neg)) {
    res_neg_list[[k]] <- procesar_sector(lista_neg[k], sectores_neg$orden[k], dir_pos = FALSE)
  }
  
  df_res_pos <- data.table::rbindlist(res_pos_list)
  df_res_neg <- data.table::rbindlist(res_neg_list)
  
  # ── Join con métricas del sector y unión final ────────────────────────────
  sec_pos_final <- df_res_pos[sectores_pos, on = "orden"]
  sec_neg_final <- df_res_neg[sectores_neg, on = "orden"]
  
  chart_final <- data.table::rbindlist(list(sec_pos_final, sec_neg_final))
  chart_final[order(-rep)]
}

df_contrib_paises_exp_informe <- .df_plot_barras_contribucion_paises_datacomex(
  df         = df_sectores[!orden %in% paramets$fil_sectores_plot],
  para       = paramets,
  totalesf   = totalesanho,    
  flujo      = "exp",
  region     = "esp",
  metas      = meta_sec,
  metap      = meta_pais,
  dss_mad    = dsmad,
  dss_esp    = dsesp
)