instala_carga_librerias <- function(librerias_necesarias) {
  # Instalación depaquetes faltantes
  for (lib in librerias_necesarias) {
    if (!requireNamespace(lib, quietly = TRUE)) {
      message("Instalando ", lib, "...")
      utils::install.packages(lib, dependencies = TRUE)
    }
  }
  
  # Cargar todos los paquetes
  invisible(lapply(librerias_necesarias, function(lib) {
    message("Cargando ", lib, "...")
    base::library(lib, character.only = TRUE)
  }))
  
  message("Carga de librerías necesarias completada.")
}

load_all_data <- function(base_path) {
  data_list <- list()
  
  tryCatch({
    # Datos mensuales
    data_list[["ccaa_amp"]] <- read_processed_data(file.path(base_path, "df_ccaa_mes_amp.csv"), "mes")
    data_list[["taric_amp"]] <- read_processed_data(file.path(base_path, "df_taric_mes_amp.csv"), "mes")
    data_list[["paises_amp"]] <- read_processed_data(file.path(base_path, "df_paises_mes_amp.csv"), "mes")
    
    # Datos trimestrales
    data_list[["ccaa_trim_amp"]] <- read_processed_data(file.path(base_path, "df_ccaa_trim_amp.csv"), "trim")
    data_list[["taric_trim_amp"]] <- read_processed_data(file.path(base_path, "df_taric_trim_amp.csv"), "trim")
    data_list[["paises_trim_amp"]] <- read_processed_data(file.path(base_path, "df_paises_trim_amp.csv"), "trim")
    
    # Datos anuales
    data_list[["ccaa_anos_amp"]] <- read_processed_data(file.path(base_path, "df_ccaa_anos_amp.csv"), "anos")
    data_list[["taric_anos_amp"]] <- read_processed_data(file.path(base_path, "df_taric_anos_amp.csv"), "anos")
    data_list[["paises_anos_amp"]] <- read_processed_data(file.path(base_path, "df_paises_anos_amp.csv"), "anos")
    
    message("Datos cargados exitosamente")
  }, error = function(e) {
    stop(paste("Error en la carga de datos:", e$message))
  })
  
  return(data_list)
}

read_processed_data <- function(file_path, data_type) {
  df <- data.table::fread(file = file_path)
  
  if (data_type == "mes") {
    df[, Fecha := as.IDate(Fecha, format = "%Y-%m-%d")] 
  } else if (data_type == "trim") {
    df[, Trimestre := as.character(Trimestre)]
  } else if (data_type == "anos") {
    df[, Año := as.integer(Año)]
  }
  
  return(df)
}

# Función para convertir nombre corto de CCAA
convertir_ccaa <- function(ccaa) {
  if (ccaa == "Madrid, Comunidad de") {
    return("Comunidad de Madrid")
  } else {
    return(ccaa)
  }
}

asigna_nombre_mes <- function(mes_num) {
  meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
             "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  return(meses[mes_num])
}

asigna_abr_mes <- function(mes_num) {
  meses <- c("ene.", "feb.", "mar.", "abr.", "may.", "jun.",
             "jul.", "ago.", "sep.", "oct.", "nov.", "dic.")
  return(meses[mes_num])
}

crear_tabla_de <- function(df, ano_filtro, mes_filtro) {
  get_value <- function(df_filtrado, region) {
    val <- df_filtrado[df_filtrado$ccaa == region, "valor"]
    if(length(val) == 0 || is.na(val)) return(NA)
    return(as.numeric(val))
  }
  
  regiones <- c("España", "Madrid, Comunidad de")
  
  # Filtros para dato mes
  df_export <- subset(df, flujo == "EXPORT" & var == "mes" & temp == "datoper" &
                        Año == ano_filtro & Mes == mes_filtro & ccaa %in% regiones)
  df_import <- subset(df, flujo == "IMPORT" & var == "mes" & temp == "datoper" &
                        Año == ano_filtro & Mes == mes_filtro & ccaa %in% regiones)
  
  # Filtros para TVA mes
  df_export_tva <- subset(df, flujo == "EXPORT" & var == "TVA" & temp == "datoper" &
                            Año == ano_filtro & Mes == mes_filtro & ccaa %in% regiones)
  df_import_tva <- subset(df, flujo == "IMPORT" & var == "TVA" & temp == "datoper" &
                            Año == ano_filtro & Mes == mes_filtro & ccaa %in% regiones)
  
  # Filtros para acumulado
  df_export_acum <- subset(df, flujo == "EXPORT" & var == "mes" & temp == "acumulado" &
                             Año == ano_filtro & Mes == mes_filtro & ccaa %in% regiones)
  df_import_acum <- subset(df, flujo == "IMPORT" & var == "mes" & temp == "acumulado" &
                             Año == ano_filtro & Mes == mes_filtro & ccaa %in% regiones)
  df_export_acum_tva <- subset(df, flujo == "EXPORT" & var == "TVA" & temp == "acumulado" &
                                 Año == ano_filtro & Mes == mes_filtro & ccaa %in% regiones)
  df_import_acum_tva <- subset(df, flujo == "IMPORT" & var == "TVA" & temp == "acumulado" &
                                 Año == ano_filtro & Mes == mes_filtro & ccaa %in% regiones)
  
  resultado <- lapply(regiones, function(region) {
    exp_mes         <- get_value(df_export, region)
    imp_mes         <- get_value(df_import, region)
    tva_exp_mes     <- get_value(df_export_tva, region)
    tva_imp_mes     <- get_value(df_import_tva, region)
    tc_mes          <- ifelse(!is.na(exp_mes) && !is.na(imp_mes) && imp_mes != 0, 
                              round((exp_mes / imp_mes) * 100, 2), NA)
    exp_acum        <- get_value(df_export_acum, region)
    imp_acum        <- get_value(df_import_acum, region)
    tva_exp_acum    <- get_value(df_export_acum_tva, region)
    tva_imp_acum    <- get_value(df_import_acum_tva, region)
    tc_acum         <- ifelse(!is.na(exp_acum) && !is.na(imp_acum) && imp_acum != 0,
                              round((exp_acum / imp_acum) * 100, 2), NA)
    c(
      Región = ifelse(region == "Madrid, Comunidad de", "Madrid", "España"),
      Exportaciones = round(exp_mes, 2),
      `TVA (Exp)` = round(tva_exp_mes, 2),
      Importaciones = round(imp_mes, 2),
      `TVA (Imp)` = round(tva_imp_mes, 2),
      TC = tc_mes,
      `Exportaciones (YTM)` = round(exp_acum, 2),
      `TVA (Exp YTM)` = round(tva_exp_acum, 2),
      `Importaciones (YTM)` = round(imp_acum, 2),
      `TVA (Imp YTM)` = round(tva_imp_acum, 2),
      `TC (YTM)` = tc_acum
    )
  })
  
  tabla_resultado <- as.data.frame(do.call(rbind, resultado), stringsAsFactors = FALSE)
  # Convertir a numéricos salvo la primera columna (Región)
  for(col in colnames(tabla_resultado)[-1]) {
    tabla_resultado[[col]] <- as.numeric(tabla_resultado[[col]])
  }
  return(tabla_resultado)
}

plot_lines_exp_imp <- function(ccaa_amp, region, fecha_ini, fecha_fin, var_value, temp_value, nombre_formal_ccaa, paleta = paleta_de) {
  # Filtrar datos
  df <- ccaa_amp %>%
    filter(
      ccaa == region,
      flujo %in% c("EXPORT", "IMPORT"),
      var == var_value,
      temp == temp_value,
      Fecha >= fecha_ini,
      Fecha <= fecha_fin
    )
  
  # Diccionario de nombres en español
  nombres <- c("EXPORT" = "Exportaciones", "IMPORT" = "Importaciones")
  
  # Colores personalizados
  colores <- c(paleta[1], paleta[3]) 
  
  # Crear figura vacía
  fig <- plot_ly()
  
  # Añadir líneas por flujo
  for (i in 1:2) {
    flujo_actual <- c("EXPORT", "IMPORT")[i]
    color_actual <- colores[i]
    
    df_flujo <- df %>% filter(flujo == flujo_actual)
    hover_texts <- format(round(df_flujo$valor, 2), big.mark = ".", decimal.mark = ",")
    
    fig <- fig %>%
      add_trace(
        x = df_flujo$Fecha,
        y = df_flujo$valor,
        type = 'scatter',
        mode = 'lines',
        name = nombres[flujo_actual],
        line = list(color = color_actual, width = 1),
        text = hover_texts,
        hovertemplate = paste0(
          "<b>", nombres[flujo_actual], "</b><br>",
          "Región: ", df$ccaa[1], "<br>",
          "Fecha: %{x}<br>",
          "Valor: %{text} M<br>",
          "<extra></extra>"
        )
      )
  }
  
  # Título específico
  fig <- fig %>%
    layout(
      # title = list(
      #   text = paste("Evolución temporal de las Exportaciones e Importaciones en", nombre_formal_ccaa),
      #   font = list(size = 16, family = "Calibri", color = "black"),
      #   x = 0.5,
      #   xanchor = "center"
      # ),
      legend = list(
        title = list(text = "Flujo"),
        orientation = "h",     
        x = 1,                  
        xanchor = "right",      
        y = 1,              
        yanchor = "bottom"    
      ),
      xaxis = list(
        title = list(text = "Fecha"),
        rangeslider = list(visible = TRUE)
      ),
      yaxis = list(
        title = list(text = "Volumen (mill. de euros)"),
        tickformat = ".0f",
        tickmode = "auto"
      )
    )
  
  # Aplicar tema personalizado
  fig <- fig %>% layout(custom_theme_plotly())
  
  return(fig)
}

grafica_flujos_ccaa <- function(df, flujo_fil, var_fil, temp_fil, ccaa_fil, ano_fil, mes_fil, paleta = paleta_de) {
  # Filtrar datos
  df_filtrado <- df %>%
    filter(
      ccaa %in% ccaa_fil,
      Mes == mes_fil,
      Año >= ano_fil,
      flujo %in% flujo_fil,
      var %in% var_fil,
      temp %in% temp_fil
    ) %>%
    mutate(
      valor = as.numeric(valor),
      valor_fmt = format(round(valor, 2), big.mark = ".", decimal.mark = ","),
      flujo_lbl = ifelse(flujo == "EXPORT", "Exportaciones", "Importaciones"),
      año = format(Fecha, "%Y"),
      text = paste0(
        "<b>Flujo:</b> ", flujo_lbl, "<br>",
        "<b>Año:</b> ", format(Fecha, "%Y"), "<br>",
        "<b>Volumen:</b> ", valor_fmt, " millones de euros"
      )
    ) %>%
    arrange(Fecha, flujo)
  
  # Convertir número de mes a nombre
  nombre_mes <- asigna_nombre_mes(mes_fil)
  
  # Títulos
  ccaa_corto <- convertir_ccaa(ccaa_fil[1])
  titulo <- paste("Evolución del comercio exterior de", ccaa_corto)
  subtitulo <- paste("Meses de", nombre_mes, "desde", ano_fil)

    
  # Cambiar solo esta parte:
  fig <- plot_ly() %>%
    add_bars(
      data = df_filtrado %>% filter(flujo == "EXPORT"),
      x = ~Fecha,
      y = ~valor,
      name = "Exportaciones",
      marker = list(color = paleta[1]),
      hovertext = ~text,
      hoverinfo = "text"
    ) %>%
    add_bars(
      data = df_filtrado %>% filter(flujo == "IMPORT"),
      x = ~Fecha,
      y = ~valor,
      name = "Importaciones",
      marker = list(color = paleta[3]),
      hovertext = ~text,
      hoverinfo = "text"
    ) %>%
    layout(
      barmode = 'group',
      # title = list(
      #   text = paste0(titulo, "<br><sup>", subtitulo, "</sup>"),
      #   x = 0,
      #   xanchor = "left"
      # ),
      xaxis = list(
        title = "Año",
        tickformat = "%Y",
        dtick = "M12",
        tickangle = 0
      ),
      yaxis = list(
        title = " Volumen (mill. de euros)",
        tickformat = ".0f",
        tickmode = "auto"
      ),
      legend = list(
        title = list(text = "Flujo"),
        # Personalizar las etiquetas de la leyenda
        orientation = "v"
      )
    ) %>%
    layout(custom_theme_plotly())
    
  # Si quieres cambiar las etiquetas de la leyenda a español:
  fig <- fig %>% 
    layout(
      legend = list(
        title = list(text = "Flujo"),
        orientation = "h",     
        x = 1,                  
        xanchor = "right",      
        y = 1,              
        yanchor = "bottom"    
      )
    ) %>%
    # Cambiar nombres en la leyenda
    plotly::style(name = "Exportaciones", traces = 1) %>%
    plotly::style(name = "Importaciones", traces = 2)
    
  return(fig)
}

grafica_flujos_ccaa_con_tva <- function(df, flujo_fil, var_fil, temp_fil, ccaa_fil, ano_fil, mes_fil, paleta = paleta_de) {
  # Filtrar datos para barras (datos mensuales)
  df_barras <- df %>%
    filter(
      ccaa %in% ccaa_fil,
      Mes == mes_fil,
      Año >= ano_fil,
      flujo %in% flujo_fil,
      var == "mes",
      temp %in% temp_fil
    ) %>%
    mutate(
      valor = as.numeric(valor),
      valor_fmt = format(round(valor, 2), big.mark = ".", decimal.mark = ","),
      flujo_lbl = ifelse(flujo == "EXPORT", "Exportaciones", "Importaciones"),
      año = format(Fecha, "%Y"),
      text_barras = paste0(
        "<b>Flujo:</b> ", flujo_lbl, "<br>",
        "<b>Año:</b> ", format(Fecha, "%Y"), "<br>",
        "<b>Volumen:</b> ", valor_fmt, " millones de euros"
      )
    ) %>%
    arrange(Fecha, flujo)
  
  # Filtrar datos para líneas (TVA)
  df_linea <- df %>%
    filter(
      ccaa %in% ccaa_fil,
      Mes == mes_fil,
      Año >= ano_fil,
      flujo %in% flujo_fil,
      var == "TVA",
      temp %in% temp_fil
    ) %>%
    mutate(
      valor = as.numeric(valor),
      flujo_lbl = ifelse(flujo == "EXPORT", "Exportaciones", "Importaciones"),
      año = format(Fecha, "%Y"),
      text_linea = paste0(
        "<b>Flujo:</b> ", flujo_lbl, "<br>",
        "<b>Año:</b> ", format(Fecha, "%Y"), "<br>",
        "<b>TVA:</b> ", round(valor, 1), "%"
      )
    ) %>%
    arrange(Fecha, flujo)
  
  # Convertir número de mes a nombre
  nombre_mes <- asigna_nombre_mes(mes_fil)
  
  # Títulos
  ccaa_corto <- convertir_ccaa(ccaa_fil[1])
  titulo <- paste("Evolución del comercio exterior de", ccaa_corto)
  subtitulo <- paste("Meses de", nombre_mes, "desde", ano_fil)
  
  # Crear gráfico
  fig <- plot_ly()
  
  # Añadir barras
  fig <- fig %>%
    add_bars(
      data = df_barras %>% filter(flujo == "EXPORT"),
      x = ~Fecha,
      y = ~valor,
      name = "Exportaciones",
      marker = list(color = paleta[1]),
      hovertext = ~text_barras,
      hoverinfo = "text"
    ) %>%
    add_bars(
      data = df_barras %>% filter(flujo == "IMPORT"),
      x = ~Fecha,
      y = ~valor,
      name = "Importaciones",
      marker = list(color = paleta[3]),
      hovertext = ~text_barras,
      hoverinfo = "text"
    )
  
  # Añadir líneas TVA
  fig <- fig %>%
    add_lines(
      data = df_linea %>% filter(flujo == "EXPORT"),
      x = ~Fecha,
      y = ~valor,
      name = "Exportaciones TVA (%)",
      line = list(color = paleta[2]),
      mode = "lines+markers",
      yaxis = "y2",
      hovertext = ~text_linea,
      hoverinfo = "text"
    ) %>%
    add_lines(
      data = df_linea %>% filter(flujo == "IMPORT"),
      x = ~Fecha,
      y = ~valor,
      name = "Importaciones TVA (%)",
      line = list(color = paleta[4]),
      mode = "lines+markers",
      yaxis = "y2",
      hovertext = ~text_linea,
      hoverinfo = "text"
    )
  
  # Configurar layout
  fig <- fig %>%
    layout(
      barmode = 'group',
      # title = list(
      #   text = paste0(titulo, "<br><sup>", subtitulo, "</sup>"),
      #   x = 0,
      #   xanchor = "left"
      # ),
      xaxis = list(
        title = "Año",
        tickformat = "%Y",
        dtick = "M24",
        tickangle = 0
      ),
      yaxis = list(
        title = " Volumen (mill. de euros)",
        tickformat = ".0f",
        tickmode = "auto",
        automargin = TRUE
      ),
      yaxis2 = list(
        title = "TVA (%)",
        overlaying = "y",
        side = "right",
        showgrid = FALSE,
        automargin = TRUE
      ),
      legend = list(
        title = list(text = "Flujo"),
        orientation = "h",
        x = 1,
        xanchor = "right",
        y = 1,
        yanchor = "bottom"
      )
    ) %>%
    layout(custom_theme_plotly())
  
  return(fig)
}

grafica_anos <- function(dataframe, ccaa_fil, flujo_fil, temp_fil, var_fil, mes_filtro, ano_filtro, paleta = paleta_de) {
  # Calcular df1
  df1 <- dataframe %>%
    filter(
      ccaa %in% ccaa_fil,
      Mes == mes_filtro,
      Año >= ano_filtro,
      flujo %in% flujo_fil,
      var %in% var_fil,
      temp %in% temp_fil
    ) %>%
    select(-Fecha, -var, -Mes, -Tri)
  
  # Calcular df2
  df2 <- dataframe %>%
    filter(
      ccaa %in% ccaa_fil,
      Mes == 12,
      Año >= ano_filtro,
      flujo %in% flujo_fil,
      var %in% var_fil,
      temp == "acumulado"
    ) %>%
    select(-Fecha, -var, -Mes, -Tri) %>%
    mutate(temp = case_when(
      temp == "acumulado" ~ "totalano",
      TRUE ~ temp
    ))
  
  # Combinar df1 y df2
  df <- bind_rows(df1, df2)
  
  # Crear df1_aux
  df1_aux <- dataframe %>%
    filter(
      ccaa %in% ccaa_fil,
      Mes == mes_filtro,
      flujo %in% flujo_fil,
      var %in% var_fil,
      temp %in% temp_fil
    ) %>%
    select(-Fecha, -var, -Mes, -Tri)
  
  # Crear df2_aux
  df2_aux <- dataframe %>%
    filter(
      ccaa %in% ccaa_fil,
      Mes == 12,
      flujo %in% flujo_fil,
      var %in% var_fil,
      temp == "acumulado"
    ) %>%
    select(-Fecha, -var, -Mes, -Tri) %>%
    mutate(temp = case_when(
      temp == "acumulado" ~ "totalano",
      TRUE ~ temp
    ))
  
  # Calcular año máximo y valor para df1_aux
  max_year <- max(df1_aux$Año)
  max_valor <- df1_aux %>%
    filter(Año == max_year) %>%
    pull(valor) %>%
    max()
  
  # Verificar si max_year falta en df2_aux
  if (!max_year %in% df2_aux$Año) {
    # Crear nueva fila con max_year y max_valor
    new_row <- data.frame(
      ccaa = ccaa_fil[1],
      Año = max_year,
      flujo = flujo_fil[1],
      temp = "totalano",
      valor = max_valor
    )
    # Añadir la nueva fila a df2_aux
    df2_aux <- bind_rows(df2_aux, new_row)
  }
  
  # Combinar df1_aux y df2_aux
  df_aux <- bind_rows(df1_aux, df2_aux)
  
  # Convertir valor a numérico
  df_aux <- df_aux %>%
    mutate(valor = as.numeric(valor))
  
  # Calcular rankings dentro de cada grupo 'temp'
  df_aux <- df_aux %>%
    group_by(temp) %>%
    mutate(rank = min_rank(desc(valor))) %>%
    ungroup()
  
  # Convertir ccaa
  ccaa_corto <- convertir_ccaa(ccaa_fil[1])
  
  # Determinar tipo de flujo para título y leyenda
  tipo_flujo <- ifelse("EXPORT" %in% flujo_fil, "exportaciones", "importaciones")
  nombre_leyenda <- ifelse("EXPORT" %in% flujo_fil, "Exportaciones", "Importaciones")
  
  # Crear etiquetas dinámicas
  mes_nombre <- asigna_nombre_mes(mes_filtro)
  titulo <- paste("Evolución de las", tipo_flujo, "de", ccaa_corto)
  
  # Establecer el orden de la categoría 'temp'
  df_aux <- df_aux %>%
    mutate(temp = factor(temp, levels = c("datoper", "acumulado", "totalano"), ordered = TRUE))
  
  # Definir colores
  colores_temp <- c(
    "datoper" = unname(paleta_de[1]),
    "acumulado" = unname(paleta_de[3]), 
    "totalano" = unname(paleta_de[5])
  )
  
  # Crear nombres para la leyenda
  df_aux <- df_aux %>%
    mutate(
      temp_label = case_when(
        temp == "datoper" ~ paste0("Mes (", mes_nombre, ")"),
        temp == "acumulado" ~ paste0("Acumulado enero-", mes_nombre),
        temp == "totalano" ~ "Total año",
        TRUE ~ as.character(temp)
      ),
      hover_text = paste0(
        "<b>Año:</b> ", Año, "<br>",
        "<b>Volumen:</b> ", format(round(valor, 1), big.mark = ".", decimal.mark = ","), " millones de euros<br>",
        "<b>Rank:</b> ", rank
      )
    )
  
  # Crear gráfico
  fig <- plot_ly()
  
  # Añadir barras para cada categoría temp
  for(temp_cat in levels(df_aux$temp)) {
    data_temp <- df_aux %>% filter(temp == temp_cat)
    if(nrow(data_temp) > 0) {
      fig <- fig %>%
        add_bars(
          data = data_temp,
          x = ~Año,
          y = ~valor,
          name = ~unique(temp_label),
          marker = list(color = colores_temp[temp_cat]),
          hovertext = ~hover_text,
          hoverinfo = "text"
        )
    }
  }
  
  # Configurar layout
  fig <- fig %>%
    layout(
      barmode = 'group',
      # title = list(
      #   text = titulo,
      #   x = 0,
      #   xanchor = "left"
      # ),
      xaxis = list(
        title = "Año",
        tickformat = "d"
      ),
      yaxis = list(
        title = "Volumen (mill. de euros)",
        tickformat = "d"
      ),
      legend = list(
        title = list(text = nombre_leyenda),
        orientation = "h",
        x = 1,
        xanchor = "right",
        y = 1,
        yanchor = "bottom"
      )
    ) %>%
    layout(custom_theme_plotly())
  
  return(fig)
}

grafica_mm <- function(df, flujo_fil, temp_fil, ccaa_fil, fecha_ini, fecha_fin, paleta = paleta_de) {
  # Filtrar datos
  df_filtrado <- df %>%
    filter(
      ccaa %in% ccaa_fil,
      var %in% c("mes", "TVA"),
      temp %in% temp_fil,
      flujo %in% flujo_fil,
      Fecha >= fecha_ini,
      Fecha <= fecha_fin
    )
  
  df1 <- df_filtrado %>% filter(var == "mes")
  df2 <- df_filtrado %>% filter(var == "TVA")
  
  ccaa_corto <- convertir_ccaa(ccaa_fil[1])
  
  # Tipo de flujo y etiquetas
  tipo_flujo <- ifelse("EXPORT" %in% flujo_fil, "exportaciones", "importaciones")
  nombre_leyenda <- tools::toTitleCase(tipo_flujo)
  
  # Título
  titulo <- paste("Evolución de las", tipo_flujo, "de la", ccaa_corto)
  
  # Crear gráfico
  fig <- plot_ly()
  
  # Área para volumen (df1)
  fig <- fig %>%
    add_trace(
      data = df1,
      x = ~Fecha,
      y = ~valor,
      type = 'scatter',
      mode = 'lines',
      fill = 'tozeroy',
      name = "Volumen (MMNC12)",
      line = list(color = paleta[3]),
      fillcolor = paleta[3],
      hovertemplate = paste0(
        "<b>Flujo:</b> ", tipo_flujo, "<br>",
        "<b>Fecha:</b> %{x|%b-%y}<br>",
        "<b>Vol (MM12):</b> %{y:.1f}<extra></extra>"
      )
    )
  
  # Línea para TVA (df2), eje secundario
  fig <- fig %>%
    add_trace(
      data = df2,
      x = ~Fecha,
      y = ~valor,
      type = 'scatter',
      mode = 'lines',
      name = "TVA",
      yaxis = "y2",
      line = list(color = paleta[1]),
      hovertemplate = paste0(
        "<b>Flujo:</b> ", tipo_flujo, "<br>",
        "<b>Fecha:</b> %{x|%b-%y}<br>",
        "<b>TVA (MMNC12):</b> %{y:.1f}%<extra></extra>"
      )
    )
  
  # Ejes y diseño
  fig <- fig %>%
    layout(
      # title = list(text = titulo, x = 0),
      xaxis = list(title = "Fecha"),
      yaxis = list(
        title = "Volumen (mill. de euros)", 
        tickformat = "d", 
        automargin = TRUE),
      yaxis2 = list(
        title = "TVA (%)",
        overlaying = "y",
        side = "right",
        automargin = TRUE
      ),
      legend = list(
        title = list(text = nombre_leyenda),
        orientation = "h",
        x = 1,
        xanchor = "right",
        y = 1,
        yanchor = "bottom"
      )
    )
  
  # Aplicar tema
  fig <- fig %>% layout(custom_theme_plotly())
  
  # Etiquetas del eje X con nombres de meses en español
  tickvals <- seq(as.Date(fecha_ini), as.Date(fecha_fin), by = "3 months")
  month_numbers <- as.integer(format(tickvals, "%m"))
  ticktext <- paste0(asigna_abr_mes(month_numbers), "-", format(tickvals, "%y"))
  
  fig <- fig %>%
    layout(
      xaxis = list(
        tickvals = tickvals,
        ticktext = ticktext,
        tickangle = 90
      )
    )
  
  return(fig)
}

grafico_barras_detalle <- function(data, start_date, end_date, group_col, flujo = 'EXPORT', top_n = 10, paleta = paleta_de) {
  # Filtrar el dataframe con las condiciones ajustadas
  df_filtrado <- data %>%
    filter(
      Fecha >= as.Date(start_date),
      Fecha <= as.Date(end_date),
      flujo == !!flujo,
      var %in% c('mes', 'peso'),
      temp %in% c('datoper', 'acumulado')
    )
  
  # Calcular nom
  nom <- ifelse(group_col == "taric", "TARIC", "paises")
  
  # Convertir 'valor' a numérico
  df_filtrado <- df_filtrado %>%
    mutate(valor = as.numeric(valor))
  
  # Seleccionar top N basado en valores acumulados para "mes"
  top_valores <- df_filtrado %>%
    filter(temp == 'acumulado', var == 'mes') %>%
    top_n(top_n, wt = valor) %>%
    pull(!!sym(group_col)) %>%
    unique()
  
  # Filtrar dataset para incluir solo los top N
  df_plot <- df_filtrado %>%
    filter(!!sym(group_col) %in% top_valores)
  
  # Filtrar df_plot para 'mes' y 'peso'
  df_plot_mes <- df_plot %>% filter(var == 'mes')
  df_plot_peso <- df_plot %>% filter(var == 'peso')
  
  # Calcular rank para 'acumulado' y 'mes'
  df_plot_mes <- df_plot_mes %>%
    group_by(temp) %>%
    mutate(
      rank_temp = rank(desc(valor))
    ) %>%
    ungroup() %>%
    mutate(
      rank_acumulado = ifelse(temp == 'acumulado', rank_temp, NA),
      rank_mes = ifelse(temp == 'datoper', rank_temp, NA)
    ) %>%
    select(-rank_temp)
  
  # Llenar los valores NA de rank con los valores correctos para cada grupo
  df_plot_mes <- df_plot_mes %>%
    group_by(!!sym(group_col)) %>%
    mutate(
      rank_acumulado = max(rank_acumulado, na.rm = TRUE),
      rank_mes = max(rank_mes, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Ordenar por rank de 'acumulado'
  df_plot_mes <- df_plot_mes %>%
    arrange(rank_acumulado)
  
  # Modificar group_col para incluir solo la primera palabra para el eje x
  df_plot_mes <- df_plot_mes %>%
    mutate(short_group_col = sapply(strsplit(as.character(!!sym(group_col)), " "), `[`, 1))
  
  # Fusionar información de peso desde df_plot_peso
  df_plot_mes <- df_plot_mes %>%
    left_join(
      df_plot_peso %>% select(valor, !!sym(group_col), temp),
      by = c(group_col, "temp"),
      suffix = c("", "_peso")
    )
  
  # Formatear la fecha con mes en español
  mes_num <- as.numeric(format(as.Date(start_date), "%m"))
  mes_abr <- asigna_abr_mes(mes_num)
  formatted_date_spanish <- paste0(mes_abr, "-", format(as.Date(start_date), "%Y"))
  
  # Título con mes en español
  titulo <- paste0("Top ", top_n, " ", nom, " ", tolower(flujo), "aciones (", formatted_date_spanish, ")")
  
  # Obtener el nombre del mes para la leyenda
  mes_nombre <- asigna_nombre_mes(as.numeric(format(as.Date(end_date), "%m")))
  
  # Preparar datos para el gráfico
  df_plot_mes_datoper <- df_plot_mes %>% 
    filter(temp == 'datoper') %>%
    arrange(factor(short_group_col, levels = unique(df_plot_mes$short_group_col)))
  
  df_plot_mes_acumulado <- df_plot_mes %>% 
    filter(temp == 'acumulado') %>%
    arrange(factor(short_group_col, levels = unique(df_plot_mes$short_group_col)))
  
  # Crear texto hover personalizado
  hover_datoper <- paste0(
    df_plot_mes_datoper[[group_col]], "<br>",
    "Vol: ", format(round(df_plot_mes_datoper$valor, 2), nsmall = 2), " Millones de euros<br>",
    "Peso: ", format(round(df_plot_mes_datoper$valor_peso, 1), nsmall = 1), "%<br>",
    "Rank: ", round(df_plot_mes_datoper$rank_mes, 0)
  )
  
  hover_acumulado <- paste0(
    df_plot_mes_acumulado[[group_col]], "<br>",
    "Vol: ", format(round(df_plot_mes_acumulado$valor, 2), nsmall = 2), " Millones de euros<br>",
    "Peso: ", format(round(df_plot_mes_acumulado$valor_peso, 1), nsmall = 1), "%<br>",
    "Rank: ", round(df_plot_mes_acumulado$rank_acumulado, 0)
  )
  
  # Crear gráfico usando plotly
  fig <- plot_ly() %>%
    # Barras para datoper
    add_bars(
      data = df_plot_mes_datoper,
      x = ~short_group_col,
      y = ~valor,
      name = paste0("Mes (", mes_nombre, ")"),
      marker = list(color = paleta[1]),
      hovertext = hover_datoper,
      hoverinfo = 'text'
    ) %>%
    # Barras para acumulado
    add_bars(
      data = df_plot_mes_acumulado,
      x = ~short_group_col,
      y = ~valor,
      name = paste0("Acumulado enero-", mes_nombre),
      marker = list(color = paleta[3]),
      hovertext = hover_acumulado,
      hoverinfo = 'text'
    )
  
  # Aplicar tema
  fig <- fig %>% layout(custom_theme_plotly())
  
  fig <- fig %>%
    layout(
      # title = list(text = titulo, x = 0),
      xaxis = list(
        title = paste0("Código (", nom, ")"),
        categoryorder = "array",
        categoryarray = unique(df_plot_mes$short_group_col)
      ),
      yaxis = list(title = "Volumen (mill. de euros)"),
      barmode = 'group',
      showlegend = TRUE,
      legend = list(
        title = list(text = "Período"),
        orientation = "h",
        x = 1,
        xanchor = "right",
        y = 1,
        yanchor = "bottom"
      )
    )
  
  return(fig)
}

grafico_contribuciones <- function(data, start_date, end_date, group_col, flujo_fil, temporal, top_n = 10, paleta = paleta_de) {
  # Filtrar datos
  df_filtrado <- data %>%
    filter(
      Fecha >= as.Date(start_date),
      Fecha <= as.Date(end_date),
      flujo == flujo_fil, 
      var %in% c("TVA", "DIFA", "con_TVA", "mes", "con_tva"),
      temp == temporal
    )
  
  # Obtener top_n mayores y menores valores de contribuciones
  con_tva_data <- df_filtrado %>% 
    filter(var == "con_tva") %>%
    mutate(valor = as.numeric(valor))
  
  top_largest_values <- con_tva_data %>% top_n(top_n, wt = valor)
  top_smallest_values <- con_tva_data %>% top_n(-top_n, wt = valor)
  selected_values <- bind_rows(top_largest_values, top_smallest_values)
  selected_group_values <- unique(selected_values[[group_col]])
  
  # Filtrar conjunto original con los grupos seleccionados
  df_filtrado <- df_filtrado %>% filter(.data[[group_col]] %in% selected_group_values)
  
  # Crear pivot para valores
  df_filtered_subset <- df_filtrado %>% 
    select(!!sym(group_col), Fecha, var, valor) %>%
    mutate(valor = as.numeric(valor))
  
  df_plot <- df_filtered_subset %>%
    tidyr::pivot_wider( 
      names_from = var,
      values_from = valor,
      values_fn = mean,
      values_fill = NA
    )
  
  # Ordenar y factorizar
  df_plot_sorted <- df_plot %>%
    mutate(nombre = .data[[group_col]]) %>%
    arrange(con_tva) %>%
    mutate(nombre = factor(nombre, levels = unique(nombre)))
  
  # Filtrar filas con datos válidos (no NA en con_tva)
  df_plot_sorted <- df_plot_sorted %>%
    filter(!is.na(con_tva))
  
  # Colores
  colors <- ifelse(df_plot_sorted$con_tva >= 0, paleta[1], paleta[3])
  
  # Texto hover - MODIFICADO para incluir el nombre
  hover_text <- paste0(
    "<b>", df_plot_sorted$nombre, "</b><br>",  # AÑADIDO: nombre del país/TARIC
    "Contribución: ", round(df_plot_sorted$con_tva, 1), " p.p.<br>",
    "Valor: ", format(round(df_plot_sorted$mes, 1), big.mark = "."), " Millones de euros<br>",
    "Valor año pasado: ", format(round(df_plot_sorted$mes - df_plot_sorted$DIFA, 1), big.mark = "."), " Millones de euros<br>",
    "Diferencia absoluta: ", format(round(df_plot_sorted$DIFA, 1), big.mark = "."), " Millones de euros<br>",
    "TVA: ", round(df_plot_sorted$TVA, 1), "%"
  )
  
  # Determinar título del eje y según group_col
  y_title <- ifelse(group_col == "pais", "País", "Capítulo TARIC")
  
  # Crear gráfico SIN texto en las barras
  fig <- plot_ly() %>%
    add_bars(
      data = df_plot_sorted,
      x = ~con_tva,
      y = ~nombre,
      orientation = 'h',
      marker = list(color = colors),
      hovertext = hover_text,
      hoverinfo = 'text'
    )
  
  # Aplicar tema
  fig <- fig %>% layout(custom_theme_plotly())
  
  # Layout sin título - MODIFICADO para separar el título del eje Y
  fig <- fig %>%
    layout(
      xaxis = list(title = "Contribución (puntos porcentuales)"),
      yaxis = list(
        title = list(
          text = y_title,
          standoff = 20  # AÑADIDO: separa el título de los tickvals
        ),
        automargin = TRUE,
        tickfont = list(size = 11)  # Opcional: ajustar tamaño de fuente
      ),
      margin = list(l = 150),  # AUMENTADO de margen por defecto a 150
      showlegend = FALSE
    )
  
  return(fig)
}