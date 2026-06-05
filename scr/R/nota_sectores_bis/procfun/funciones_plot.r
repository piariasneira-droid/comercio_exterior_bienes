# Theme ----
custom_theme <- ggplot2::theme(
  # Fondo y bordes
  panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
  plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
  
  # Títulos y texto
  plot.title = ggplot2::element_text(size = 7, face = "bold", color = "black", hjust = 0.5),
  plot.subtitle = ggplot2::element_text(size = 6, color = "black", hjust = 0.5),
  plot.title.position = "plot",
  axis.title = ggplot2::element_text(size = 7, color = "black", face = "bold"),
  axis.text = ggplot2::element_text(size = 6, color = "black"),
  axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, margin = ggplot2::margin(t = 0)),
  axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 0)),
  
  # Ejes y líneas de la cuadrícula
  axis.ticks = ggplot2::element_line(color = "black"),
  axis.line = ggplot2::element_line(color = "black"),
  panel.grid.major.x = ggplot2::element_blank(),
  panel.grid.minor.x = ggplot2::element_blank(),
  panel.grid.major.y = ggplot2::element_line(color = "black", size = 0.25, linetype = "dashed"),
  panel.grid.minor.y = ggplot2::element_line(color = "grey", size = 0.25, linetype = "dashed"),
  
  # Leyenda
  legend.box.background = ggplot2::element_rect(fill = "transparent", color = NA),
  legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
  legend.position = "bottom",
  legend.justification = "right",
  legend.text = ggplot2::element_text(size = 6, family = "Calibri"),
  legend.title = ggplot2::element_text(size = 7, family = "Calibri", face = "bold"),
  legend.margin = ggplot2::margin(0, 0, 0, 0),
  legend.spacing = ggplot2::unit(0.3, "cm"),
  legend.key.size = ggplot2::unit(0.3, "cm"),
  legend.box.spacing = ggplot2::unit(0.1, "cm"),
  
  # Otros elementos
  plot.caption.position = "plot",
  plot.tag = ggplot2::element_text(size = 6, hjust = 0),
  plot.caption = ggplot2::element_text(size = 6, hjust = 0),
  strip.background = ggplot2::element_rect(fill = NA)
)

# Aux ----
.convertir_ccaa <- function(ccaa) {
  if (ccaa == "Madrid, Comunidad de") {
    return("Comunidad de Madrid")
  }
  return(ccaa)
}

# Plots ----
.grafica_flujos_ccaa <- function(df, flujo_fil, var_fil, temp_fil, ccaa_fil, ano_fil, mes_fil, colde1, colde3) {
  # Aplicar filtros
  df_filtrado <- df %>%
    filter(
      ccaa %in% ccaa_fil,
      Mes == mes_fil,
      Año >= ano_fil,
      flujo %in% flujo_fil,
      var %in% var_fil,
      temp %in% temp_fil
    ) %>%
    mutate(valor = as.numeric(valor)) %>%
    # Asegurar que Fecha es Date y ordenar los datos
    arrange(Fecha, flujo)
  
  # Vector con los nombres de los meses
  nombres_meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
                     "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  
  # Obtener el nombre del mes correspondiente
  nombre_mes <- nombres_meses[mes_fil]
  
  # Crear el subtítulo
  subtitulo <- paste0("Meses de ", nombre_mes, " desde ", ano_fil, ". Volumen en millones de euros")
  
  df_filtrado <- df_filtrado %>%
    mutate(Año_label = year(Fecha))
  
  # Crear gráfico
  p <- ggplot(df_filtrado, aes(x = factor(Año_label), y = valor, fill = flujo)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.75) + 
    custom_theme +
    scale_fill_manual(name = "Flujo",
                      values = c("EXPORT" = colde1, "IMPORT" = colde3),
                      labels = c("EXPORT" = "Exportaciones", "IMPORT" = "Importaciones")) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.major.x = element_line(color = "grey", size = 0.25, linetype = "dashed")
    ) +
    labs(
      title = "Evolución del comercio exterior de la C. de Madrid",
      subtitle = subtitulo,
      x = NULL,
      y = NULL,
      fill = "Flujo",
      caption = "Fuente: AEAT"
    )
  
  
  # Añadir etiquetas para el último valor
  ultimo_dato <- df_filtrado %>%
    group_by(flujo) %>%
    slice_max(Fecha) %>%
    mutate(
      label = format(round(valor, 1), big.mark = ".", decimal.mark = ",")
    )
  
  # p <- p +
  #   geom_text(
  #     data = ultimo_dato,
  #     aes(label = label),
  #     position = position_dodge(width = 365),
  #     vjust = -0.5,
  #     size = 2.5,
  #     show.legend = FALSE
  #   )
  # 
  return(p)
}


.grafica_mm <- function(df, flujo_fil, temp_fil, ccaa_fil, fecha_ini, fecha_fin,colde1, colde3) {
  # Filtrar datos según los criterios proporcionados
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
  
  ccaa_corto <- .convertir_ccaa(ccaa_fil)
  
  # Determinar el tipo de flujo para el título
  tipo_flujo <- ifelse("EXPORT" %in% flujo_fil, "exportaciones", "importaciones")
  nombre_leyenda <- tools::toTitleCase(tipo_flujo)
  
  # Título y subtítulo del gráfico
  titulo <- paste0("Evolución de las ", tipo_flujo, " de la ", ccaa_corto)
  
  # Vector con los nombres de los meses
  nombres_meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
                     "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  
  # Obtener el mes y el año de fecha_ini y fecha_fin
  mes_ini <- nombres_meses[as.numeric(format(fecha_ini, "%m"))]
  ano_ini <- format(fecha_ini, "%Y")
  
  mes_fin <- nombres_meses[as.numeric(format(fecha_fin, "%m"))]
  ano_fin <- format(fecha_fin, "%Y")
  
  # Crear el subtítulo
  subtitulo <- paste0("Desde ", mes_ini, " de ", ano_ini, " hasta ", mes_fin, " de ", ano_fin)
  
  # Calcular el rango de las áreas
  max_area <- max(df1$valor, na.rm = TRUE)
  min_area <- 0
  rango_area <- max_area - min_area
  
  # Calcular el rango de las líneas (TVA)
  max_tva <- max(df2$valor, na.rm = TRUE)
  min_tva <- min(df2$valor, na.rm = TRUE)
  rango_tva <- max_tva - min_tva
  
  # Calcular la escala
  factor_escala <- ifelse(rango_tva != 0, rango_area / rango_tva, 1)
  
  # Crear gráfico
  p <- ggplot() +
    # Área para df1
    geom_area(data = df1, aes(x = Fecha, y = valor), fill = colde3, alpha = 1) +
    # Línea para df1
    geom_line(data = df1, aes(x = Fecha, y = valor, color = "Volumen (MM12)"), size = 1) +
    # Línea para df2 escalada
    geom_line(data = df2, aes(x = Fecha, y = (valor - min_tva) * factor_escala, color = "TVA"), size = 1) +
    # Escalas y etiquetas
    scale_x_date(
      name = "Fecha",
      date_breaks = "3 month",
      date_labels = "%b-%y",
      expand = c(0,0)
    ) +
    scale_y_continuous(
      name = NULL,
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      sec.axis = sec_axis(~ . / factor_escala + min_tva, name = NULL)) +
    scale_color_manual(
      name = nombre_leyenda,
      values = c("Volumen (MM12)" = colde3, "TVA" = colde1),
      breaks = c("Volumen (MM12)", "TVA"),
      labels = c("Volumen (MM12)", "TVA (MM12)")
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Fecha",
      y = "Volumen (MM12)",
      caption = "Fuente: AEAT"
    ) +
    custom_theme +
    theme(
      axis.text.x = element_text(angle = 90, hjust=0.5, vjust=0.5))
  
  return(p)
}

.grafica_anos <- function(dataframe, ccaa_fil, flujo_fil, temp_fil, var_fil, mes_filtro, ano_filtro, colde1, colde3, colde5) {
  # Calcular df1
  df1 <- dataframe %>%
    filter(ccaa %in% ccaa_fil,
           Mes == mes_filtro,
           Año >= ano_filtro,
           flujo %in% flujo_fil,
           var %in% var_fil,
           temp %in% temp_fil) %>%
    select(-Fecha, -var, -Mes, -Tri)
  
  # Calcular df2
  df2 <- dataframe %>%
    filter(ccaa %in% ccaa_fil,
           Mes == 12,
           Año >= ano_filtro,
           flujo %in% flujo_fil,
           var %in% var_fil,
           temp == "acumulado") %>% 
    select(-Fecha, -var, -Mes, -Tri) %>%
    mutate(temp = ifelse(temp == "acumulado", "totalano", temp))
  
  # Combinar df1 y df2
  df <- bind_rows(df1, df2)
  
  df1_aux <- dataframe %>%
    filter(ccaa %in% ccaa_fil,
           Mes == mes_filtro,
           flujo %in% flujo_fil,
           var %in% var_fil,
           temp %in% temp_fil) %>%
    select(-Fecha, -var, -Mes, -Tri)
  
  df2_aux <- dataframe %>%
    filter(ccaa %in% ccaa_fil,
           Mes == 12,
           flujo %in% flujo_fil,
           var %in% var_fil,
           temp == "acumulado") %>% 
    select(-Fecha, -var, -Mes, -Tri) %>%
    mutate(temp = ifelse(temp == "acumulado", "totalano", temp))
  
  df_aux <- bind_rows(df1_aux, df2_aux)
  
  # Calcular rankings y crear etiquetas
  label <- df_aux %>%
    group_by(temp) %>%
    mutate(
      rank = rank(-valor)
    ) %>%
    ungroup() %>%
    pivot_wider(
      names_from = temp,
      values_from = c(valor, rank),
      names_glue = "{.value}_{temp}"
    ) %>%
    mutate(
      etiqueta = paste0("Vol. (rank): ",
                        scales::label_number(accuracy = 0.1, big.mark = ".", decimal.mark = ",")(valor_datoper), " (", rank_datoper, "), ",
                        scales::label_number(accuracy = 0.1, big.mark = ".", decimal.mark = ",")(valor_acumulado), " (", rank_acumulado, "), ",
                        scales::label_number(accuracy = 0.1, big.mark = ".", decimal.mark = ",")(valor_totalano), " (", rank_totalano, ")"
      )
    ) %>%
    filter(Año >= ano_filtro)
  
  ccaa_corto <- .convertir_ccaa(ccaa_fil)
  
  # Determinar el tipo de flujo para el título y la leyenda
  tipo_flujo <- ifelse("EXPORT" %in% flujo_fil, "exportaciones", "importaciones")
  nombre_leyenda <- ifelse("EXPORT" %in% flujo_fil, "Exportaciones", "Importaciones")
  
  # Crear etiquetas dinámicas
  mes_nombre <- format(as.Date(paste0("2023-", mes_filtro, "-01")), "%b")
  titulo <- paste0("Evolución de las ", tipo_flujo, " de la ", ccaa_corto)
  
  # Crear gráfico
  p <- ggplot(df, aes(x = factor(Año), y = valor)) +
    geom_col(data = df %>% filter(temp == "totalano"), aes(fill = "totalano"), position = position_dodge(width = 0.7), width = 0.6) +
    geom_col(data = df %>% filter(temp == "acumulado"), aes(fill = "acumulado"), position = position_dodge(width = 0.7), width = 0.6) +
    geom_col(data = df %>% filter(temp == "datoper"), aes(fill = "datoper"), position = position_dodge(width = 0.7), width = 0.6) +
    # geom_text(data = label, aes(y = Inf, label = etiqueta), hjust = 1, vjust= 0.3, size = 2) +
    scale_fill_manual(
      name = paste0("Volumen ", tipo_flujo),
      values = c(
        "datoper" = colde1,
        "acumulado" = colde3,
        "totalano" = colde5
      ),
      breaks = c("datoper", "acumulado", "totalano"),
      labels = c(
        paste("Mes (", mes_nombre, ")", sep = ""),
        paste("Acumulado ene-", mes_nombre, sep = ""),
        "Total año"
      )
    )+
    labs(
      title = titulo,
      y = NULL,
      x = NULL,
      fill = "Tipo de Dato",
      caption = "Fuente: AEAT"
    ) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","), expand = c(0, 0)) +
    custom_theme +
    theme(
      axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    coord_flip()
  
  return(p)
}