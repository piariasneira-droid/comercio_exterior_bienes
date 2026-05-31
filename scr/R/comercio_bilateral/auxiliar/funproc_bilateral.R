##### Tema plotly y paleta -----
colde1 <- "#2d5532"
colde2 <- "#6f6f4e"
colde3 <- "#b4d7b4"
colde4 <- "#ddd9c3"
colde5 <- "#a6a6a6"
colde6 <- "#d9d9d9"
colde7 <- "#a1a17a"
colde8 <- "#2d3535"
colde9 <- "#b4c7d7"

custom_theme_plotly <- function() {
  list(
    plot_bgcolor = 'rgba(255, 255, 255, 1)',
    paper_bgcolor = 'rgba(255, 255, 255, 1)',
    font = list(size = 14, family = "Calibri", color = "black"),
    xaxis = list(
      title = list(font = list(size = 14, family = "Calibri", color = "black")),
      tickfont = list(size = 14, family = "Calibri", color = "black"),
      ticks = "outside",
      tickcolor = "black",
      linecolor = "black",
      gridcolor = "lightgrey",
      gridwidth = 1,
      griddash = "dot",
      zeroline = FALSE
    ),
    yaxis = list(
      title = list(font = list(size = 14, family = "Calibri", color = "black")),
      tickfont = list(size = 14, family = "Calibri", color = "black"),
      ticks = "outside",
      tickcolor = "black",
      linecolor = "black",
      gridcolor = "lightgrey",
      gridwidth = 1,
      griddash = "dot",
      zeroline = FALSE
    ),
    legend = list(
      bgcolor = 'rgba(0, 0, 0, 0)',
      font = list(size = 12, family = "Calibri"),
      orientation = "h",
      x = 1,
      xanchor = "right",
      y = 1.02,
      yanchor = "bottom"
    ),
    margin = list(t = 40, b = 40, l = 40, r = 40)
  )
}

#### Carga TARIC y países----
cargar_taric <- function(path) {
  # Leer el fichero con vroom
  df_taric <- vroom::vroom(
    file       = path,
    delim      = "\t",
    col_names  = TRUE,
    locale     = vroom::locale(encoding = "UTF-16LE"),
    col_types  = vroom::cols(.default = vroom::col_character())
  )
  
  # Convertir a data.table
  dt_taric <- data.table::as.data.table(df_taric)
  
  # Transformaciones
  dt_taric[, nivel_taric := readr::parse_integer(nivel_taric, na = c("", "NA"))]
  dt_taric[, codint_taric := as.numeric(cod_taric)]
  dt_taric[codint_taric == as.numeric(0), codint_taric := NA]
  
  # Filtrar filas: eliminar cod_taric que empiezan con "00" y codint_taric NA
  dt_taric <- dt_taric[substr(cod_taric, 1, 2) != "00" & !is.na(codint_taric)]
  
  # Fila TOTAL
  fila_total <- data.table::data.table(
    cod_taric    = "0",
    nivel_taric  = 0L,
    taric        = "TOTAL",
    codint_taric = as.numeric(0)
  )
  
  # Combinar todo
  resultado <- data.table::rbindlist(list(fila_total, dt_taric), use.names = TRUE, fill = TRUE)
  resultado <- anade_padres_dt(resultado)
  
  return(resultado)
}

niveles_map <- c(
  "0" = "Total",
  "1" = "Capítulo",
  "2" = "Partida",
  "3" = "Subpartida",
  "4" = "Nomenclatura combinada",
  "5" = "Arancel"
)

cargar_pais <- function(path) {
  # Leer el archivo Excel
  df_pais <- openxlsx::read.xlsx(path)
  
  # Convertir a data.table
  setDT(df_pais)
  df_pais[, cod_pais := as.integer(cod_pais)]
  
  # Añadir la fila "Total"
  df_pais <- rbind(
    df_pais,
    data.table(
      pais = "000 - Total",
      region = "Total",
      continente = "Total",
      cod_pais = 0L,
      nombre = "Total"
    ),
    fill = TRUE
  )
  
  return(df_pais)
}

anade_padres_dt <- function(dt) {
  dt[, longitud := nchar(cod_taric)]
  
  # Asignar códigos padres solo si cod_taric ≠ "0"
  dt[, Capítulo := fifelse(
    cod_taric == "0", NA_character_,
    fifelse(longitud > 2, substr(cod_taric, 1, 2), NA_character_)
  )]
  
  dt[, Partida := fifelse(
    cod_taric == "0", NA_character_,
    fifelse(longitud > 4, substr(cod_taric, 1, 4), NA_character_)
  )]
  
  dt[, Subpartida := fifelse(
    cod_taric == "0", NA_character_,
    fifelse(longitud > 6, substr(cod_taric, 1, 6), NA_character_)
  )]
  
  dt[, NC := fifelse(
    cod_taric == "0", NA_character_,
    fifelse(longitud > 8, substr(cod_taric, 1, 8), NA_character_)
  )]
  
  dt[, longitud := NULL]
  
  # Crear tabla lookup con código -> descripción
  lookup <- unique(dt[, .(cod_taric, etiqueta = paste0(cod_taric, " - ", taric))])
  setkey(lookup, cod_taric)
  
  # Asignar descripciones concatenadas
  dt[, Tar := lookup[cod_taric, etiqueta, on = "cod_taric"]]
  dt[, Cap := lookup[Capítulo, etiqueta, on = "cod_taric"]]
  dt[, Par := lookup[Partida, etiqueta, on = "cod_taric"]]
  dt[, Sub := lookup[Subpartida, etiqueta, on = "cod_taric"]]
  dt[, N := lookup[NC, etiqueta, on = "cod_taric"]]
  
  return(dt)
}

cruce_taric_pais <- function(dfbase, dftar, dfpais) {
  # Join con df_taric
  df <- merge(dfbase, dftar, by.x = "cod_taric", by.y = "codint_taric", all.x = TRUE)
  setnames(df, "cod_taric.y", "cod_taric_char")
  
  # Join con df_paises
  df <- merge(df, dfpais, by.x = "pais", by.y = "cod_pais", all.x = TRUE)
  setnames(df, c("pais", "pais.y"), c("cod_pais", "pais"))
  
  return(df)
}

#### Procesamiento dataset general ----
comercio_bilateral_anual <- function(ds, lista_meses, regiones, orden, nombre_territorio) {
  
  df_resultado <- purrr::map_dfr(names(regiones), function(nombre_region) {
    
    ds %>%
      filter(mes %in% lista_meses,
             nivel_taric == 0L,
             pais %in% regiones[[nombre_region]]) %>%
      group_by(flujo, año) %>%
      summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") %>%
      collect() %>%                       
      mutate(
        euros     = euros / 1e6,
        region    = nombre_region,
        territorio = nombre_territorio
      )
  })
  
  df_resultado %>%
    pivot_wider(
      names_from  = año,
      values_from = euros
    ) %>%
    arrange(
      desc(flujo),
      factor(region, levels = orden)
    )
}

exportar_dataframes_anuales <- function(dfmad, dfesp, savepath = "./data/output/comercio_regiones.xlsx") {
  
  dir_path <- dirname(savepath)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("Directorio creado:", dir_path, "\n")
  }
  
  # Combinar Madrid y España y separar por flujo
  df_combined <- bind_rows(dfmad, dfesp) %>%
    select(-flujo)  # quitamos flujo, ya lo usamos para separar
  
  df_exp <- bind_rows(dfmad, dfesp) %>%
    filter(flujo == 1) %>%
    select(-flujo) %>%
    rename(región = territorio) %>%
    arrange(factor(región, levels = c("Madrid", "España"))) %>%
    select(región, region, everything())
  
  df_imp <- bind_rows(dfmad, dfesp) %>%
    filter(flujo == 0) %>%
    select(-flujo) %>%
    rename(región = territorio) %>%
    arrange(factor(región, levels = c("Madrid", "España"))) %>%
    select(región, region, everything())
  
  write_xlsx(
    list(
      "Exportaciones" = df_exp,
      "Importaciones" = df_imp
    ),
    savepath
  )
  
  cat("Archivos exportados correctamente en:", savepath, "\n")
}


#### Procesamiento productos ----
##### Helpers: Materializar query Arrow a data.table ----
collect_as_dt <- function(ds, filtro_flujo, filtro_ano = NULL, filtro_mes = NULL,
                          filtro_pais = NULL, filtro_taric = NULL,
                          col_var = "euros",
                          group_cols = c("año", "pais", "cod_taric")) {
  
  col_sym <- rlang::sym(col_var)
  
  # Normalizar tipos — TARIC puede ser integer64 con valores > 2^31, no usar as.integer()
  if (!is.null(filtro_taric)) filtro_taric <- bit64::as.integer64(filtro_taric)
  if (!is.null(filtro_ano))   filtro_ano   <- as.integer(filtro_ano)
  if (!is.null(filtro_mes))   filtro_mes   <- as.integer(filtro_mes)
  if (!is.null(filtro_pais))  filtro_pais  <- as.integer(filtro_pais)
  filtro_flujo <- as.integer(filtro_flujo)
  
  query <- ds %>% filter(flujo == filtro_flujo)
  
  if (!is.null(filtro_ano))   query <- query %>% filter(año %in% filtro_ano)
  if (!is.null(filtro_mes))   query <- query %>% filter(mes %in% filtro_mes)
  if (!is.null(filtro_pais))  query <- query %>% filter(pais %in% filtro_pais)
  if (!is.null(filtro_taric)) query <- query %>% filter(cod_taric %in% filtro_taric)
  
  needed_cols <- unique(c(group_cols, "flujo", "año", "mes", "pais", "cod_taric", col_var))
  existing_cols <- names(ds$schema)
  needed_cols <- needed_cols[needed_cols %in% existing_cols]
  query <- query %>% select(all_of(needed_cols))
  
  result <- query %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(Volumen = sum(!!col_sym, na.rm = TRUE), .groups = "drop") %>%
    collect()
  
  setDT(result)
  
  # Convertir cod_taric a integer64 consistente para comparaciones posteriores
  if ("cod_taric" %in% names(result)) {
    result[, cod_taric := bit64::as.integer64(cod_taric)]
  }
  
  return(result)
}
# TOP-LEVEL HELPER
calcular_resto <- function(df, by_cols = NULL) {
  if (is.null(by_cols)) {
    by_cols <- intersect(c("año", "mes", "cod_taric"), names(df))
  }
  totales     <- df[pais == 0L, .(Total = Volumen), by = by_cols]
  suma_paises <- df[pais != 0L, .(Suma = sum(Volumen)), by = by_cols]
  resto <- merge(totales, suma_paises, by = by_cols, all = TRUE)
  resto[is.na(Suma), Suma := 0]
  resto[, `:=`(pais = 1000L, Volumen = Total - Suma)]
  resto[, c("Total", "Suma") := NULL]
  rbind(df, resto)
}

# Helper para construir el directorio raíz dinámico por país
build_outdir_pais <- function(pais_code, meses, anios, df_paises,
                              base = "./data/output/comercio_bilateral") {
  
  abreviaturas_mes <- c("ene", "feb", "mar", "abr", "may", "jun",
                        "jul", "ago", "sep", "oct", "nov", "dic")
  
  nombre_pais <- df_paises[cod_pais == pais_code, nombre]
  nombre_pais <- if (length(nombre_pais) == 0) {
    as.character(pais_code)
  } else {
    tolower(gsub(" ", "_", nombre_pais[1]))
  }
  
  rango_mes <- if (length(meses) == 1) {
    abreviaturas_mes[meses]
  } else {
    paste0(abreviaturas_mes[min(meses)], "-", abreviaturas_mes[max(meses)])
  }
  
  rango_anio <- if (length(anios) == 1) {
    as.character(anios)
  } else {
    paste0(min(anios), "-", max(anios))
  }
  
  file.path(base, paste0(nombre_pais, "_", rango_mes, "_", rango_anio))
}

# Helper para crear todos los subdirectorios bajo un outdir dado
crear_dirs_pais <- function(outdir) {
  subdirs <- file.path(outdir, c(
    "dispersion/exceles",
    "dispersion/htmls",
    "anuales/exceles",
    "anuales/htmls",
    "mensuales/exceles",
    "mensuales/htmls"
  ))
  invisible(lapply(subdirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

build_name_anual <- function(pais_code, meses, anios, df_paises,
                             outdir = "./data/output/comercio_bilateral") {
  
  abreviaturas_mes <- c("ene", "feb", "mar", "abr", "may", "jun",
                        "jul", "ago", "sep", "oct", "nov", "dic")
  
  # cod_pais es int, pais_code es int -> comparación directa sin coerción
  nombre_pais <- df_paises[cod_pais == pais_code, nombre]
  
  nombre_pais <- if (length(nombre_pais) == 0) {
    as.character(pais_code)
  } else {
    tolower(gsub(" ", "_", nombre_pais[1]))
  }
  
  rango_mes <- if (length(meses) == 1) {
    abreviaturas_mes[meses]
  } else {
    paste0(abreviaturas_mes[min(meses)], "-", abreviaturas_mes[max(meses)])
  }
  
  rango_anio <- if (length(anios) == 1) {
    as.character(anios)
  } else {
    paste0(min(anios), "-", max(anios))
  }
  
  file.path(outdir, paste0(nombre_pais, "_", rango_mes, "_", rango_anio, ".xlsx"))
}

# Guarda exportaciones e importaciones en un único Excel con dos hojas
exportar_top_tarics <- function(df_exp, df_imp, pais_code, flujo_label = NULL,
                                outdir = "./data/output/comercio_bilateral") {
  
  nombre_archivo <- file.path(
    outdir,
    paste0("top_tarics_pais_", pais_code,
           if (!is.null(flujo_label)) paste0("_", flujo_label) else "",
           ".xlsx")
  )
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Exportaciones")
  openxlsx::addWorksheet(wb, "Importaciones")
  openxlsx::writeData(wb, "Exportaciones", df_exp)
  openxlsx::writeData(wb, "Importaciones", df_imp)
  openxlsx::saveWorkbook(wb, nombre_archivo, overwrite = TRUE)
  
  invisible(nombre_archivo)
}

##### Tops ----
top_tarics_exposicion <- function(df_mad,
                                  df_esp,
                                  filtro_nivel = 5L,
                                  filtro_ano = 2024L,
                                  filtro_mes = 1L:12L,
                                  n_max = 50L,
                                  filtro_flujo = 1L,
                                  filtro_pais = 400L,
                                  col_var = "euros",
                                  df_taric = df_tarics,
                                  df_pais = df_paises,
                                  incluir_ranking_total = TRUE) {
  
  # --- Lazy collect: MAD ---
  df_mad_fil <- collect_as_dt(
    ds          = df_mad,
    filtro_flujo = filtro_flujo,
    filtro_ano   = filtro_ano,
    filtro_mes   = filtro_mes,
    filtro_pais  = c(filtro_pais, 0L),
    col_var      = col_var,
    group_cols   = c("año", "pais", "cod_taric")
  )
  
  total_mad      <- df_mad_fil[pais == 0L & cod_taric == 0L, Volumen]
  total_mad_pais <- df_mad_fil[pais == filtro_pais & cod_taric == 0L, Volumen]
  
  df_mad_fil <- cruce_taric_pais(dfbase = df_mad_fil, dftar = df_taric, dfpais = df_pais)
  
  # --- Lazy collect: ESP ---
  df_esp_fil <- collect_as_dt(
    ds          = df_esp,
    filtro_flujo = filtro_flujo,
    filtro_ano   = filtro_ano,
    filtro_mes   = filtro_mes,
    filtro_pais  = c(filtro_pais, 0L),
    col_var      = col_var,
    group_cols   = c("año", "pais", "cod_taric")
  )
  
  total_esp      <- df_esp_fil[pais == 0L & cod_taric == 0L, Volumen]
  total_esp_pais <- df_esp_fil[pais == filtro_pais & cod_taric == 0L, Volumen]
  
  df_esp_fil <- cruce_taric_pais(dfbase = df_esp_fil, dftar = df_taric, dfpais = df_pais)
  
  # --- Top TARIC codes ---
  top_mad_codes <- df_mad_fil[
    nivel_taric %in% filtro_nivel & cod_pais == filtro_pais
  ][order(-Volumen)][1:n_max, cod_taric]
  
  df_mad_fil <- df_mad_fil[cod_taric %in% top_mad_codes]
  df_esp_fil <- df_esp_fil[cod_taric %in% top_mad_codes]
  
  # --- Columnas por nivel ---
  effective_filtro_nivel <- as.integer(max(filtro_nivel))
  desc_cols <- switch(as.character(effective_filtro_nivel),
                      "1" = "Tar",
                      "2" = c("Tar", "Cap"),
                      "3" = c("Tar", "Cap", "Par"),
                      "4" = c("Tar", "Cap", "Par", "Sub"),
                      "5" = c("Tar", "Cap", "Par", "Sub", "N"),
                      stop(sprintf("Máx filtro niveles (%d) debe estar entre 1 y 5.", effective_filtro_nivel))
  )
  
  merge_by_cols <- c("cod_taric", desc_cols)
  dcast_formula_lhs <- paste(c("cod_taric", desc_cols), collapse = " + ")
  dcast_formula <- as.formula(paste(dcast_formula_lhs, "~ cod_pais"))
  
  # --- Dcast MAD ---
  df_mad_fil <- data.table::dcast(df_mad_fil, dcast_formula, value.var = "Volumen")
  setnames(df_mad_fil,
           old = c("0", as.character(filtro_pais)),
           new = c("Volumen CM", "Volumen CM a país"))
  
  # --- Dcast ESP ---
  df_esp_fil <- data.table::dcast(df_esp_fil, dcast_formula, value.var = "Volumen")
  setnames(df_esp_fil,
           old = c("0", as.character(filtro_pais)),
           new = c("Volumen ESP", "Volumen ESP a país"))
  
  # --- Merge ---
  df <- merge(df_mad_fil, df_esp_fil, by = merge_by_cols, all.x = TRUE)
  
  # --- Totales (only if ranking total is included) ---
  if (incluir_ranking_total) {
    totales <- data.table(
      cod_taric          = 0L,
      Tar                = "0 - TOTAL",
      `Volumen CM`       = total_mad,
      `Volumen CM a país` = total_mad_pais,
      `Volumen ESP`       = total_esp,
      `Volumen ESP a país` = total_esp_pais
    )
    df <- rbindlist(list(df, totales), fill = TRUE)
  }
  
  # --- Ranking por TARIC (siempre se calcula) ---
  df_mad_rank <- collect_as_dt(
    ds          = df_mad,
    filtro_flujo = filtro_flujo,
    filtro_ano   = filtro_ano,
    filtro_mes   = filtro_mes,
    filtro_taric = top_mad_codes,
    col_var      = col_var,
    group_cols   = c("año", "pais", "cod_taric")
  )
  df_mad_rank <- df_mad_rank[pais != 0L]
  df_mad_rank[, `Ranking volumen país` := frank(-Volumen, ties.method = "average"), by = .(año, cod_taric)]
  df_mad_rank <- df_mad_rank[pais == filtro_pais, .(cod_taric, `Ranking volumen país`)]
  
  # --- Ranking total (cod_taric == 0): solo si incluir_ranking_total == TRUE ---
  if (incluir_ranking_total) {
    df_mad_rank0 <- collect_as_dt(
      ds          = df_mad,
      filtro_flujo = filtro_flujo,
      filtro_ano   = filtro_ano,
      filtro_mes   = filtro_mes,
      filtro_taric = 0L,
      col_var      = col_var,
      group_cols   = c("año", "pais", "cod_taric")
    )
    df_mad_rank0 <- df_mad_rank0[pais != 0L]
    df_mad_rank0[, `Ranking volumen país` := frank(-Volumen, ties.method = "average"), by = .(año, cod_taric)]
    df_mad_rank0 <- df_mad_rank0[pais == filtro_pais, .(cod_taric, `Ranking volumen país`)]
    
    df_mad_rank <- rbindlist(list(df_mad_rank, df_mad_rank0), fill = TRUE)
  }
  
  # --- Final merge + metrics ---
  df <- merge(df, df_mad_rank, by = "cod_taric", all.x = TRUE)
  setorder(df, -`Volumen CM a país`)
  
  df[, `:=`(
    `Grado dependencia`  = `Volumen CM a país` / `Volumen CM`,
    `Asimetría regional` = `Volumen CM a país` / `Volumen ESP a país`,
    `Peso país`          = `Volumen CM a país` / total_mad_pais
  )]
  
  return(df)
}

top_exposicion_asimetria <- function(
    df_mad,
    df_esp,
    filtro_nivel = 5L,
    filtro_ano = 2024L,
    filtro_mes = 1L:12L,
    filtro_flujo = 1L,
    filtro_pais = 400L,
    col_var = "euros",
    df_taric = df_tarics,
    df_pais = df_paises,
    filtro_porcentaje = 0.01,
    ordenar_por = "Grado dependencia") {
  
  # --- Lazy collect: MAD ---
  df_mad_fil <- collect_as_dt(
    ds          = df_mad,
    filtro_flujo = filtro_flujo,
    filtro_ano   = filtro_ano,
    filtro_mes   = filtro_mes,
    filtro_pais  = c(filtro_pais, 0L),
    col_var      = col_var
  )
  
  total_mad      <- df_mad_fil[pais == 0L & cod_taric == 0L, Volumen]
  total_mad_pais <- df_mad_fil[pais == filtro_pais & cod_taric == 0L, Volumen]
  
  df_mad_fil <- cruce_taric_pais(dfbase = df_mad_fil, dftar = df_taric, dfpais = df_pais)
  
  # --- Lazy collect: ESP ---
  df_esp_fil <- collect_as_dt(
    ds          = df_esp,
    filtro_flujo = filtro_flujo,
    filtro_ano   = filtro_ano,
    filtro_mes   = filtro_mes,
    filtro_pais  = c(filtro_pais, 0L),
    col_var      = col_var
  )
  
  total_esp      <- df_esp_fil[pais == 0L & cod_taric == 0L, Volumen]
  total_esp_pais <- df_esp_fil[pais == filtro_pais & cod_taric == 0L, Volumen]
  
  df_esp_fil <- cruce_taric_pais(dfbase = df_esp_fil, dftar = df_taric, dfpais = df_pais)
  
  # --- Top codes by percentage threshold ---
  top_mad_codes <- df_mad_fil[
    nivel_taric %in% filtro_nivel &
      cod_pais == filtro_pais &
      Volumen >= total_mad_pais * filtro_porcentaje,
    cod_taric
  ]
  
  df_mad_fil <- df_mad_fil[cod_taric %in% top_mad_codes]
  df_esp_fil <- df_esp_fil[cod_taric %in% top_mad_codes]
  
  # --- Columnas por nivel ---
  effective_filtro_nivel <- as.integer(max(filtro_nivel))
  desc_cols <- switch(as.character(effective_filtro_nivel),
                      "1" = "Tar",
                      "2" = c("Tar", "Cap"),
                      "3" = c("Tar", "Cap", "Par"),
                      "4" = c("Tar", "Cap", "Par", "Sub"),
                      "5" = c("Tar", "Cap", "Par", "Sub", "N"),
                      stop(sprintf("Máx filtro niveles (%d) debe estar entre 1 y 5.", effective_filtro_nivel))
  )
  
  merge_by_cols <- c("cod_taric", desc_cols)
  dcast_formula_lhs <- paste(c("cod_taric", desc_cols), collapse = " + ")
  dcast_formula <- as.formula(paste(dcast_formula_lhs, "~ cod_pais"))
  
  # --- Dcast ---
  df_mad_fil <- data.table::dcast(df_mad_fil, dcast_formula, value.var = "Volumen")
  setnames(df_mad_fil, c("0", as.character(filtro_pais)), c("Volumen CM", "Volumen CM a país"))
  
  df_esp_fil <- data.table::dcast(df_esp_fil, dcast_formula, value.var = "Volumen")
  setnames(df_esp_fil, c("0", as.character(filtro_pais)), c("Volumen ESP", "Volumen ESP a país"))
  
  # --- Merge + metrics ---
  df <- merge(df_mad_fil, df_esp_fil, by = merge_by_cols, all.x = TRUE)
  
  df[, `:=`(
    `Grado dependencia`  = `Volumen CM a país` / `Volumen CM`,
    `Asimetría regional`  = `Volumen CM a país` / `Volumen ESP a país`,
    `Peso país`           = `Volumen CM a país` / total_mad_pais
  )]
  
  setorderv(df, ordenar_por, order = -1L)
  
  return(df)
}

##### Dfs ----

df_periodo_exposicion_asimetria <- function(
    df_mad,
    df_esp,
    filtro_flujo   = 1L,
    filtro_taric   = 0L,
    col_var        = "euros",
    top_paises     = 5L,
    pais_analisis  = 400L,
    filtro_ano_top = 2024L,
    filtro_mes     = 1L:12L,
    df_taric       = df_tarics,
    df_pais        = df_paises) {
  
  # ── CRITICAL: keep integer64, NOT as.integer() ──
  filtro_taric_i64 <- bit64::as.integer64(filtro_taric)
  taric_cero       <- bit64::as.integer64(0L)
  tarics_query     <- unique(c(filtro_taric_i64, taric_cero))
  
  cat("      [periodo] TARIC:", as.character(filtro_taric_i64), 
      "| Query TARICs:", paste(as.character(tarics_query), collapse=","), "\n")
  
  # --- Copia local de países (no mutar original) ---
  df_pais_local <- copy(df_pais)
  if (!1000L %in% df_pais_local$cod_pais) {
    df_pais_local <- rbind(
      df_pais_local,
      data.table(pais = "1000 Resto paises", region = "Resto",
                 continente = "Resto", cod_pais = 1000L, nombre = "Resto"),
      fill = TRUE
    )
  }
  
  # --- Lazy collect: MAD (only this TARIC + total) ---
  df_mad_fil <- collect_as_dt(
    ds           = df_mad,
    filtro_flujo = filtro_flujo,
    filtro_mes   = filtro_mes,
    filtro_taric = tarics_query,
    col_var      = col_var,
    group_cols   = c("año", "pais", "cod_taric")
  )
  
  # --- Lazy collect: ESP (only this TARIC + total) ---
  df_esp_fil <- collect_as_dt(
    ds           = df_esp,
    filtro_flujo = filtro_flujo,
    filtro_mes   = filtro_mes,
    filtro_taric = tarics_query,
    col_var      = col_var,
    group_cols   = c("año", "pais", "cod_taric")
  )
  
  # --- Diagnóstico ---
  cat("      [periodo] Rows MAD:", nrow(df_mad_fil), "| Rows ESP:", nrow(df_esp_fil), "\n")
  if (nrow(df_mad_fil) == 0) {
    warning(sprintf("df_periodo: No data for taric=%s, flujo=%d",
                    as.character(filtro_taric_i64), filtro_flujo))
    return(data.table())
  }
  
  # --- Top países: FILTER BY THIS TARIC in Arrow ---
  df_top <- collect_as_dt(
    ds           = df_mad,
    filtro_flujo = filtro_flujo,
    filtro_ano   = filtro_ano_top,
    filtro_mes   = filtro_mes,
    filtro_taric = filtro_taric_i64,
    col_var      = col_var,
    group_cols   = c("año", "pais", "cod_taric")
  )
  df_top <- df_top[pais != 0L]
  
  n_available <- min(top_paises, nrow(df_top))
  if (n_available > 0) {
    top_mad_paises <- df_top[order(-Volumen)][1:n_available, pais]
  } else {
    top_mad_paises <- integer(0)
  }
  top_mad_paises <- unique(c(na.omit(top_mad_paises), as.integer(pais_analisis), 0L))
  
  cat("      [periodo] Top países:", paste(top_mad_paises, collapse=","), "\n")
  
  # --- Filtrar por países seleccionados ---
  df_mad_fil <- df_mad_fil[pais %in% top_mad_paises]
  df_esp_fil <- df_esp_fil[pais %in% top_mad_paises]
  
  # --- Calcular resto ---
  df_mad_fil  <- calcular_resto(df_mad_fil, by_cols = c("año", "cod_taric"))
  totales_mad <- df_mad_fil[pais == 0L, .(`Volumen CM` = Volumen), by = .(año, cod_taric)]
  df_mad_fil  <- df_mad_fil[pais != 0L]
  
  df_esp_fil  <- calcular_resto(df_esp_fil, by_cols = c("año", "cod_taric"))
  totales_esp <- df_esp_fil[pais == 0L, .(`Volumen ESP` = Volumen), by = .(año, cod_taric)]
  df_esp_fil  <- df_esp_fil[pais != 0L]
  
  # --- Merge MAD + ESP ---
  df_combined <- merge(
    df_mad_fil[, .(año, pais, cod_taric, Volumen_mad = Volumen)],
    df_esp_fil[, .(año, pais, cod_taric, Volumen_esp = Volumen)],
    by  = c("año", "pais", "cod_taric"),
    all = TRUE
  )
  setnafill(df_combined, fill = 0, cols = c("Volumen_mad", "Volumen_esp"))
  
  df_combined <- merge(df_combined, totales_mad, by = c("año", "cod_taric"), all.x = TRUE)
  df_combined <- merge(df_combined, totales_esp, by = c("año", "cod_taric"), all.x = TRUE)
  
  setnames(df_combined,
           c("Volumen_mad", "Volumen_esp"),
           c("Volumen CM a país", "Volumen ESP a país"))
  
  # --- Pivot: subset + merge (NOT dcast) ---
  if (filtro_taric_i64 == taric_cero) {
    # TARIC 0: value and total are the same
    df_wide <- df_combined[cod_taric == taric_cero]
    df_wide[, `:=`(
      `Volumen CM total`         = `Volumen CM`,
      `Volumen CM a país total`  = `Volumen CM a país`,
      `Volumen ESP total`        = `Volumen ESP`,
      `Volumen ESP a país total` = `Volumen ESP a país`
    )]
    df_wide[, cod_taric := NULL]
  } else {
    df_valor <- df_combined[cod_taric == filtro_taric_i64,
                            .(año, pais,
                              `Volumen CM`, `Volumen CM a país`,
                              `Volumen ESP`, `Volumen ESP a país`)]
    
    df_total <- df_combined[cod_taric == taric_cero,
                            .(año, pais,
                              `Volumen CM total`         = `Volumen CM`,
                              `Volumen CM a país total`  = `Volumen CM a país`,
                              `Volumen ESP total`        = `Volumen ESP`,
                              `Volumen ESP a país total` = `Volumen ESP a país`)]
    
    df_wide <- merge(df_valor, df_total, by = c("año", "pais"), all.x = TRUE)
  }
  
  cat("      [periodo] Rows after pivot:", nrow(df_wide), "\n")
  
  # --- Column order ---
  col_order <- c("año", "pais",
                 "Volumen CM", "Volumen CM a país",
                 "Volumen ESP", "Volumen ESP a país",
                 "Volumen CM total", "Volumen CM a país total",
                 "Volumen ESP total", "Volumen ESP a país total")
  col_order <- col_order[col_order %in% names(df_wide)]
  setcolorder(df_wide, col_order)
  
  # --- Join país info (clean, no pais.y ambiguity) ---
  df_wide <- merge(df_wide, df_pais_local[, .(cod_pais, nombre)],
                   by.x = "pais", by.y = "cod_pais", all.x = TRUE)
  setnames(df_wide, c("pais", "nombre"), c("cod_pais", "nombre_pais"))
  
  setcolorder(df_wide, c("año", "cod_pais", "nombre_pais",
                         setdiff(names(df_wide), c("año", "cod_pais", "nombre_pais"))))
  
  # --- Métricas ---
  df_wide[, `:=`(
    `Grado dependencia`          = fifelse(`Volumen CM` != 0, `Volumen CM a país` / `Volumen CM`, NA_real_),
    `Asimetría regional`         = fifelse(`Volumen ESP a país` != 0, `Volumen CM a país` / `Volumen ESP a país`, NA_real_),
    `Grado dependencia nacional` = fifelse(`Volumen ESP` != 0, `Volumen ESP a país` / `Volumen ESP`, NA_real_),
    `Peso país`                  = fifelse(`Volumen CM total` != 0, `Volumen CM a país` / `Volumen CM total`, NA_real_),
    `Peso país-arancel CM`       = fifelse(`Volumen CM total` != 0, `Volumen CM a país` / `Volumen CM total`, NA_real_),
    tamano = fifelse(cod_pais == pais_analisis, 5L, fifelse(cod_pais == 1000L, 3L, 1L))
  )]
  
  return(df_wide)
}

df_evolucion_exposicion_asimetria <- function(
    df_mad,
    df_esp,
    filtro_flujo   = 1L,
    filtro_taric   = 0L,
    col_var        = "euros",
    top_paises     = 5L,
    pais_analisis  = 400L,
    filtro_ano_top = 2024L,
    filtro_mes_top = 1L:12L,
    df_taric       = df_tarics,
    df_pais        = df_paises) {
  
  # ── CRITICAL: keep integer64 ──
  filtro_taric_i64 <- bit64::as.integer64(filtro_taric)
  taric_cero       <- bit64::as.integer64(0L)
  tarics_query     <- unique(c(filtro_taric_i64, taric_cero))
  
  cat("      [evolucion] TARIC:", as.character(filtro_taric_i64),
      "| Query TARICs:", paste(as.character(tarics_query), collapse=","), "\n")
  
  # --- Copia local de países ---
  df_pais_local <- copy(df_pais)
  if (!1000L %in% df_pais_local$cod_pais) {
    df_pais_local <- rbind(
      df_pais_local,
      data.table(pais = "1000 Resto paises", region = "Resto",
                 continente = "Resto", cod_pais = 1000L, nombre = "Resto"),
      fill = TRUE
    )
  }
  
  # --- Lazy collect: MAD (con mes, only this TARIC + total) ---
  df_mad_fil <- collect_as_dt(
    ds           = df_mad,
    filtro_flujo = filtro_flujo,
    filtro_taric = tarics_query,
    col_var      = col_var,
    group_cols   = c("año", "mes", "pais", "cod_taric")
  )
  
  # --- Lazy collect: ESP (con mes, only this TARIC + total) ---
  df_esp_fil <- collect_as_dt(
    ds           = df_esp,
    filtro_flujo = filtro_flujo,
    filtro_taric = tarics_query,
    col_var      = col_var,
    group_cols   = c("año", "mes", "pais", "cod_taric")
  )
  
  # --- Diagnóstico ---
  cat("      [evolucion] Rows MAD:", nrow(df_mad_fil), "| Rows ESP:", nrow(df_esp_fil), "\n")
  if (nrow(df_mad_fil) == 0) {
    warning(sprintf("df_evolucion: No data for taric=%s, flujo=%d",
                    as.character(filtro_taric_i64), filtro_flujo))
    return(data.table())
  }
  
  # --- Top países: FILTER BY THIS TARIC in Arrow ---
  df_top <- collect_as_dt(
    ds           = df_mad,
    filtro_flujo = filtro_flujo,
    filtro_ano   = filtro_ano_top,
    filtro_mes   = filtro_mes_top,
    filtro_taric = filtro_taric_i64,
    col_var      = col_var,
    group_cols   = c("año", "pais", "cod_taric")
  )
  df_top <- df_top[pais != 0L]
  
  n_available <- min(top_paises, nrow(df_top))
  if (n_available > 0) {
    top_mad_paises <- df_top[order(-Volumen)][1:n_available, pais]
  } else {
    top_mad_paises <- integer(0)
  }
  top_mad_paises <- unique(c(na.omit(top_mad_paises), as.integer(pais_analisis), 0L))
  
  cat("      [evolucion] Top países:", paste(top_mad_paises, collapse=","), "\n")
  
  # --- Filtrar ---
  df_mad_fil <- df_mad_fil[pais %in% top_mad_paises]
  df_esp_fil <- df_esp_fil[pais %in% top_mad_paises]
  
  # --- Calcular resto ---
  df_mad_fil  <- calcular_resto(df_mad_fil, by_cols = c("año", "mes", "cod_taric"))
  totales_mad <- df_mad_fil[pais == 0L, .(`Volumen CM` = Volumen), by = .(año, mes, cod_taric)]
  df_mad_fil  <- df_mad_fil[pais != 0L]
  
  df_esp_fil  <- calcular_resto(df_esp_fil, by_cols = c("año", "mes", "cod_taric"))
  totales_esp <- df_esp_fil[pais == 0L, .(`Volumen ESP` = Volumen), by = .(año, mes, cod_taric)]
  df_esp_fil  <- df_esp_fil[pais != 0L]
  
  # --- Merge MAD + ESP ---
  df_combined <- merge(
    df_mad_fil[, .(año, mes, pais, cod_taric, Volumen_mad = Volumen)],
    df_esp_fil[, .(año, mes, pais, cod_taric, Volumen_esp = Volumen)],
    by  = c("año", "mes", "pais", "cod_taric"),
    all = TRUE
  )
  setnafill(df_combined, fill = 0, cols = c("Volumen_mad", "Volumen_esp"))
  
  df_combined <- merge(df_combined, totales_mad, by = c("año", "mes", "cod_taric"), all.x = TRUE)
  df_combined <- merge(df_combined, totales_esp, by = c("año", "mes", "cod_taric"), all.x = TRUE)
  
  setnames(df_combined,
           c("Volumen_mad", "Volumen_esp"),
           c("Volumen CM a país", "Volumen ESP a país"))
  
  # --- Pivot: subset + merge (NOT dcast) ---
  if (filtro_taric_i64 == taric_cero) {
    df_wide <- df_combined[cod_taric == taric_cero]
    df_wide[, `:=`(
      `Volumen CM total`         = `Volumen CM`,
      `Volumen CM a país total`  = `Volumen CM a país`,
      `Volumen ESP total`        = `Volumen ESP`,
      `Volumen ESP a país total` = `Volumen ESP a país`
    )]
    df_wide[, cod_taric := NULL]
  } else {
    df_valor <- df_combined[cod_taric == filtro_taric_i64,
                            .(año, mes, pais,
                              `Volumen CM`, `Volumen CM a país`,
                              `Volumen ESP`, `Volumen ESP a país`)]
    
    df_total <- df_combined[cod_taric == taric_cero,
                            .(año, mes, pais,
                              `Volumen CM total`         = `Volumen CM`,
                              `Volumen CM a país total`  = `Volumen CM a país`,
                              `Volumen ESP total`        = `Volumen ESP`,
                              `Volumen ESP a país total` = `Volumen ESP a país`)]
    
    df_wide <- merge(df_valor, df_total, by = c("año", "mes", "pais"), all.x = TRUE)
  }
  
  cat("      [evolucion] Rows after pivot:", nrow(df_wide), "\n")
  
  # --- Column order ---
  col_order <- c("año", "mes", "pais",
                 "Volumen CM", "Volumen CM a país",
                 "Volumen ESP", "Volumen ESP a país",
                 "Volumen CM total", "Volumen CM a país total",
                 "Volumen ESP total", "Volumen ESP a país total")
  col_order <- col_order[col_order %in% names(df_wide)]
  setcolorder(df_wide, col_order)
  
  # --- Join país info ---
  df_wide <- merge(df_wide, df_pais_local[, .(cod_pais, nombre)],
                   by.x = "pais", by.y = "cod_pais", all.x = TRUE)
  setnames(df_wide, c("pais", "nombre"), c("cod_pais", "nombre_pais"))
  
  setcolorder(df_wide, c("año", "mes", "cod_pais", "nombre_pais",
                         setdiff(names(df_wide), c("año", "mes", "cod_pais", "nombre_pais"))))
  
  # --- Métricas ---
  df_wide[, `:=`(
    `Grado dependencia`          = fifelse(`Volumen CM` != 0, `Volumen CM a país` / `Volumen CM`, NA_real_),
    `Asimetría regional`         = fifelse(`Volumen ESP a país` != 0, `Volumen CM a país` / `Volumen ESP a país`, NA_real_),
    `Grado dependencia nacional` = fifelse(`Volumen ESP` != 0, `Volumen ESP a país` / `Volumen ESP`, NA_real_),
    `Peso país-arancel CM`       = fifelse(`Volumen CM total` != 0, `Volumen CM a país` / `Volumen CM total`, NA_real_),
    tamano = fifelse(cod_pais == pais_analisis, 5L, fifelse(cod_pais == 1000L, 3L, 1L))
  )]
  
  return(df_wide)
}

top_tarics_mercado <- function(df,
                               filtro_nivel = 5L,
                               filtro_ano = 2024L,
                               filtro_mes = 1L:12L,
                               filtro_flujo = 1L,
                               filtro_pais = 400L,
                               filtro_porcentaje = 0.01,
                               col_var = "euros",
                               df_taric = df_tarics,
                               df_pais = df_paises) {
  
  # --- Lazy collect ---
  df_fil <- collect_as_dt(
    ds          = df,
    filtro_flujo = filtro_flujo,
    filtro_ano   = filtro_ano,
    filtro_mes   = filtro_mes,
    filtro_pais  = filtro_pais,
    col_var      = col_var,
    group_cols   = c("año", "pais", "cod_taric")
  )
  
  total_pais <- df_fil[cod_taric == 0L, sum(Volumen)]
  
  df_fil <- cruce_taric_pais(dfbase = df_fil, dftar = df_taric, dfpais = df_pais)
  df_fil <- df_fil[nivel_taric %in% filtro_nivel]
  
  top_tarics <- df_fil[Volumen >= total_pais * filtro_porcentaje][order(-Volumen), cod_taric]
  
  return(top_tarics)
}

top_paises_mercado <- function(df,
                               filtro_ano = 2024L,
                               filtro_mes = 1L:12L,
                               filtro_flujo = 1L,
                               filtro_taric = 0L,
                               filtro_porcentaje = 0.01,
                               col_var = "euros",
                               df_taric = df_tarics,
                               df_pais = df_paises) {
  
  # --- Lazy collect ---
  df_fil <- collect_as_dt(
    ds          = df,
    filtro_flujo = filtro_flujo,
    filtro_ano   = filtro_ano,
    filtro_mes   = filtro_mes,
    filtro_taric = filtro_taric,
    col_var      = col_var,
    group_cols   = c("año", "pais", "cod_taric")
  )
  
  total_taric <- df_fil[pais == 0L, sum(Volumen)]
  
  df_fil <- df_fil[pais != 0L]
  
  top_paises <- df_fil[Volumen >= total_taric * filtro_porcentaje][order(-Volumen), pais]
  
  return(top_paises)
}

#### Plots ----
plot_dispersion_conchy <- function(df,
                                   nivel,
                                   x_var = "Grado dependencia",
                                   y_var = "Peso país"){
  
  # Generar texto de nivel usando data.table
  if (nivel <= 1) {
    df_plot <- df[, nivel_text := ""]
  } else if (nivel == 2) {
    df_plot <- df[, nivel_text := paste0("Capítulo: ", Cap, "<br>")]
  } else if (nivel == 3) {
    df_plot <-df[, nivel_text := paste0("Capítulo: ", Cap, "<br>",
                                        "Partida: ", Par, "<br>")]
  } else if (nivel == 4) {
    df_plot <-df[, nivel_text := paste0("Capítulo: ", Cap, "<br>",
                                        "Partida: ", Par, "<br>",
                                        "Subpartida: ", Sub, "<br>")]
  } else {
    df_plot <-df[, nivel_text := paste0("Capítulo: ", Cap, "<br>",
                                        "Partida: ", Par, "<br>",
                                        "Subpartida: ", Sub, "<br>",
                                        "NC: ", N, "<br>")]
  }
  
  # Crear hovertext usando data.table
  df_plot[, hovertext := paste0(
    "<b>TARIC: </b>", Tar, "<br>",
    nivel_text,
    "<b>Volumen CM:</b> ", format(round(`Volumen CM`/1e6, 1), decimal.mark = ",", big.mark = ".", nsmall = 1), " M€<br>",
    "<b>Volumen CM a país:</b> ", format(round(`Volumen CM a país`/1e6, 1), decimal.mark = ",", big.mark = ".", nsmall = 1), " M€<br>",
    "<b>Volumen ESP:</b> ", format(round(`Volumen ESP`/1e6, 1), decimal.mark = ",", big.mark = ".", nsmall = 1), " M€<br>",
    "<b>Volumen ESP a país:</b> ", format(round(`Volumen ESP a país`/1e6, 1), decimal.mark = ",", big.mark = ".", nsmall = 1), " M€<br>",
    "<b>Grado dependencia:</b> ", sprintf("%.1f%%", `Grado dependencia` * 100), "<br>",
    "<b>Asimetría regional:</b> ", sprintf("%.1f%%", `Asimetría regional` * 100), "<br>",
    "<b>Peso país:</b> ", sprintf("%.1f%%", `Peso país` * 100)
  )]
  
  # Crear columnas para los ejes
  df_plot[, `:=`(
    x_plot = get(x_var) * 100,
    y_plot = get(y_var) * 100
  )]
  
  scatter <- plot_ly(
    data = df_plot,
    x = ~x_plot,
    y = ~y_plot,
    type = "scatter",
    mode = "markers",
    text = ~hovertext,
    hoverinfo = "text",
    marker = list(size = 10, color = '#526DB0', line = list(width=1, color='#526DB0'))
  ) %>%
    layout(
      xaxis = list(
        title = paste0(x_var, " (%)"),
        tickformat = ".1f",
        ticksuffix = "%",
        rangemode = "tozero"
      ),
      yaxis = list(
        title = paste0(y_var, " (%)"),
        tickformat = ".1f",
        ticksuffix = "%",
        rangemode = "tozero"
      ),
      title = paste("Scatter de", x_var, "vs", y_var)
    )
  
  scatter <- scatter %>% layout(custom_theme_plotly())
  
  return(scatter)
}

crear_grafico_lineas_dependencia_evolucion <- function(df, y_var = "Grado dependencia", ctaric, df_taric) {
  
  # Columnas auxiliares
  df_plot <- df[, cod_pais_formato := sprintf("%04d", as.integer(as.character(cod_pais)))]
  df_plot[, año_int := as.integer(año)]
  df_plot[, mes_int := as.integer(mes)]
  df_plot[, Fecha   := as.Date(sprintf("%04d-%02d-01", año_int, mes_int))]
  df_plot[, c("año_int", "mes_int") := NULL]
  
  # Obtener información del TARIC
  info_taric <- df_taric[codint_taric == ctaric]
  nivel <- info_taric$nivel_taric[1]
  tari <- info_taric[, .(Tar = Tar[1], Cap = Cap[1], Par = Par[1], 
                         Sub = Sub[1], N = N[1])]
  
  # Crear el texto de nivel basado en el nivel TARIC
  nivel_text <- switch(
    as.character(nivel),
    "1" = "",
    "2" = paste0("Capítulo: ", tari$Cap),
    "3" = paste0("Capítulo: ", tari$Cap, "<br>",
                 "Partida: ", tari$Par),
    "4" = paste0("Capítulo: ", tari$Cap, "<br>",
                 "Partida: ", tari$Par, "<br>",
                 "Subpartida: ", tari$Sub),
    paste0("Capítulo: ", tari$Cap, "<br>",
           "Partida: ", tari$Par, "<br>",
           "Subpartida: ", tari$Sub, "<br>",
           "NC: ", tari$N)
  )
  
  # Añadir información de TARIC al dataframe
  df_plot[, `:=`(
    taric_code = tari$Tar,
    nivel_info = nivel_text,
    y_var_pct = get(y_var) * 100
  )]
  
  # Crear texto para hover
  df_plot[, hover_text := paste("País:", nombre_pais,
                                "<br>Fecha:", format(Fecha, "%b.%y"),
                                "<br>TARIC:", taric_code,
                                "<br>", nivel_info,
                                "<br>", y_var, ":", round(y_var_pct, 1), "%",
                                "<br>Volumen CM:", format(round(`Volumen CM`/1e6, 1), big.mark = ".", decimal.mark = ","), "M€",
                                "<br>Volumen CM a país:", format(round(`Volumen CM a país`/1e6, 1), big.mark = ".", decimal.mark = ","), "M€",
                                "<br>Volumen ESP:", format(round(`Volumen ESP`/1e6, 1), big.mark = ".", decimal.mark = ","), "M€",
                                "<br>Volumen ESP a país:", format(round(`Volumen ESP a país`/1e6, 1), big.mark = ".", decimal.mark = ","), "M€")]
  
  # Asignar colores personalizados basados en tamano
  df_plot[, color_personalizado := ifelse(tamano == 5, "#526DB0", 
                                          ifelse(tamano == 3, "#F5C201", 
                                                 NA_character_))]
  
  # Si hay países sin color asignado, crear paleta para ellos
  paises_sin_color <- unique(df_plot[is.na(color_personalizado)]$cod_pais_formato)
  if(length(paises_sin_color) > 0) {
    paleta_auto <- scales::hue_pal()(length(paises_sin_color))
    names(paleta_auto) <- paises_sin_color
    df_plot[is.na(color_personalizado), 
            color_personalizado := paleta_auto[cod_pais_formato]]
  }
  
  # Crear vector de colores para plotly
  colores_finales <- unique(df_plot[, .(cod_pais_formato, color_personalizado)])
  colores_vector <- colores_finales$color_personalizado
  names(colores_vector) <- colores_finales$cod_pais_formato
  
  # Crear el gráfico con plotly
  grafico <- plotly::plot_ly()
  
  # Añadir cada país como una traza separada
  for(pais in unique(df_plot$cod_pais_formato)) {
    df_pais <- df_plot[cod_pais_formato == pais]
    
    grafico <- grafico %>%
      plotly::add_trace(
        data = df_pais,
        x = ~Fecha,
        y = ~y_var_pct,
        name = ~nombre_pais[1],
        type = 'scatter',
        mode = 'lines+markers',
        line = list(
          color = colores_vector[pais]
          # width = df_pais$tamano[1]
        ),
        marker = list(
          color = colores_vector[pais]
        ),
        hoverinfo = 'text',
        text = ~hover_text
      )
  }
  
  grafico <- grafico %>%
    plotly::layout(
      yaxis = list(title = paste(y_var, "(%)")),
      hovermode = "closest",
      legend = list(title = list(text = "País"))
    )
  
  grafico <- grafico %>% layout(custom_theme_plotly())
  
  return(grafico) 
}

crear_grafico_lineas_dependencia_periodo <- function(df, y_var = "Grado dependencia", ctaric, df_taric) {
  # Formatear el código de país a 4 dígitos
  df_plot <- df[, cod_pais_formato := sprintf("%04d", as.integer(as.character(cod_pais)))]
  
  # Obtener información del TARIC
  info_taric <- df_taric[codint_taric == ctaric]
  nivel <- info_taric$nivel_taric[1]
  tari <- info_taric[, .(Tar = Tar[1], Cap = Cap[1], Par = Par[1], 
                         Sub = Sub[1], N = N[1])]
  
  # Crear el texto de nivel basado en el nivel TARIC
  nivel_text <- switch(
    as.character(nivel),
    "1" = "",
    "2" = paste0("Capítulo: ", tari$Cap),
    "3" = paste0("Capítulo: ", tari$Cap, "<br>",
                 "Partida: ", tari$Par),
    "4" = paste0("Capítulo: ", tari$Cap, "<br>",
                 "Partida: ", tari$Par, "<br>",
                 "Subpartida: ", tari$Sub),
    paste0("Capítulo: ", tari$Cap, "<br>",
           "Partida: ", tari$Par, "<br>",
           "Subpartida: ", tari$Sub, "<br>",
           "NC: ", tari$N)
  )
  
  # Añadir información de TARIC al dataframe usando sintaxis data.table
  df_plot[, `:=`(
    taric_code = tari$Tar,
    nivel_info = nivel_text,
    y_var_pct = get(y_var) * 100
  )]
  
  # Asignar colores personalizados basados en tamano (sin ñ)
  df_plot[, color_personalizado := ifelse(tamano == 5, "#526DB0", 
                                          ifelse(tamano == 3, "#F5C201", 
                                                 NA_character_))]
  
  # Si hay países sin color asignado, crear paleta para ellos
  paises_sin_color <- unique(df_plot[is.na(color_personalizado)]$cod_pais_formato)
  if(length(paises_sin_color) > 0) {
    paleta_auto <- scales::hue_pal()(length(paises_sin_color))
    names(paleta_auto) <- paises_sin_color
    df_plot[is.na(color_personalizado), 
            color_personalizado := paleta_auto[cod_pais_formato]]
  }
  
  # Crear vector de colores para plotly
  colores_finales <- unique(df_plot[, .(cod_pais_formato, color_personalizado)])
  colores_vector <- colores_finales$color_personalizado
  names(colores_vector) <- colores_finales$cod_pais_formato
  
  # Crear el gráfico con plotly
  grafico <- plotly::plot_ly()
  
  # Añadir cada país como una traza separada
  for(pais in unique(df_plot$cod_pais_formato)) {
    df_pais <- df_plot[cod_pais_formato == pais]
    
    grafico <- grafico %>%
      plotly::add_trace(
        data = df_pais,
        x = ~año,
        y = ~y_var_pct,
        name = ~nombre_pais[1],
        type = 'scatter',
        mode = 'lines+markers',
        line = list(
          color = colores_vector[pais]
          # width = df_pais$tamano[1] 
        ),
        marker = list(
          color = colores_vector[pais]
        ),
        hoverinfo = 'text',
        text = ~paste("País:", nombre_pais,
                      "<br>Año:", año,
                      "<br>TARIC:", taric_code,
                      "<br>", nivel_info,
                      "<br>", y_var, ":", round(y_var_pct, 1), "%",
                      "<br>Volumen CM:", format(round(`Volumen CM`/1e6, 1), big.mark = ".", decimal.mark = ","), "M€",
                      "<br>Volumen CM a país:", format(round(`Volumen CM a país`/1e6, 1), big.mark = ".", decimal.mark = ","), "M€",
                      "<br>Volumen ESP:", format(round(`Volumen ESP`/1e6, 1), big.mark = ".", decimal.mark = ","), "M€",
                      "<br>Volumen ESP a país:", format(round(`Volumen ESP a país`/1e6, 1), big.mark = ".", decimal.mark = ","), "M€")
      )
  } 
  
  grafico <- grafico %>%
    plotly::layout(
      xaxis = list(title = "Año", tickmode = "linear"),
      yaxis = list(title = paste(y_var, "(%)")),
      hovermode = "closest",
      legend = list(title = list(text = "País"))
    )
  
  grafico <- grafico %>% layout(custom_theme_plotly())
  
  return(grafico) 
}