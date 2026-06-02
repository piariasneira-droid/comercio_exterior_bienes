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
  
  clean_listasec <- function(x) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
    
    x <- lapply(x, function(z) {
      if (is.character(z) && grepl("^c\\(", z)) {
        return(eval(parse(text = z)))
      }
      return(z)
    })
    
    unique(as.character(unlist(x)))
  }
  
  N <- para$max_bars_con
  
  col_ano <- paste0(flujo, "_", region)
  col_tva <- paste0(flujo, "_", region, "_tva")
  col_rep <- paste0(flujo, "_", region, "_contrib")
  
  val_total_prev_bueno <- totalesf[[paste0(flujo, "_prev_", region)]]
  
  df <- df[, .(
    orden, niv, nombre,
    valano = get(col_ano),
    tva    = 100 * get(col_tva),
    rep    = 100 * get(col_rep)
  )]
  
  sectores_pos <<- df[order(-rep)][1:N]
  sectores_neg <- df[order(rep)][1:N]
  
  lista_pos <- unique(sectores_pos$nombre)
  lista_neg <- unique(sectores_neg$nombre)
  
  # ✔️ MAPA LIMPIO
  map_nombre_sec <- metas[
    !is.na(nombre),
    .(listasec = list(clean_listasec(listasec))),
    by = nombre
  ]
  
  # ✔️ DICCIONARIO (ESTO TE FALTABA)
  dict_sec <<- setNames(map_nombre_sec$listasec, map_nombre_sec$nombre)
  
  res_pos_list <- list()
  
  lista_pos <- "Carbón y electricidad"
  parasec <- para
  sectores_validos <<- dict_sec[[lista_pos]]
  parasec$cod_sector <- sectores_validos
  
  df_country_sec <- .tabla_paises_f(
    ds_mad     = dss_mad,
    ds_esp     = dss_esp,
    df_paises  = metap,
    parametros = parasec
  )
  
  # for (i in lista_pos) {
  # 
  #   sectores_validos <- dict_sec[[i]]
  #   sectores_validos <- as.character(unlist(sectores_validos))
  #   
  #   parasec$cod_sector <- sectores_validos 
  # 
  #   df_country <- .tabla_paises_f(
  #     ds_mad     = dss_mad,
  #     ds_esp     = dss_esp,
  #     df_paises  = metap,
  #     parametros = parasec
  #   )
  # 
  #   res_pos_list[[as.character(i)]] <- df_country
  # }
  
  df_country_sec
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