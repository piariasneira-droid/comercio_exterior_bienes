#### Funciones auxiliares ----
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

format_fecha <- function(fecha) {
  mes <- format(fecha, "%b")
  mes <- paste0(toupper(substr(mes, 1, 1)), substr(mes, 2, nchar(mes)))  # Capitaliza primera letra
  anio <- format(fecha, "%y")
  paste0(mes, " ", anio)
}

convertir_ccaa <- function(ccaa) {
  if (ccaa == "Madrid, Comunidad de") {
    return("Comunidad de Madrid")
  }
  return(ccaa)
}

obtener_nombre_flujo <- function(flujo) {
  if (flujo == 1) {
    return("exportaciones")
  } else if (flujo == 0) {
    return("importaciones")
  }
}

obtener_paleta <- function(flujo) {
  if (flujo == 1) {
    paleta = c(colde1, colde3)
  } else if (flujo == 0) {
    paleta = c(colde8, colde9)
  } 
  return(paleta)
}


#### Plots ggplot ----
load_all_data <- function(base_path) {
  data_list <- list()
  
  tryCatch({
    # Datos mensuales
    data_list$ccaa_amp <- read_processed_data(paste0(base_path, "df_ccaa_mes_amp.csv"), "mes")
    data_list$taric_amp <- read_processed_data(paste0(base_path, "df_taric_mes_amp.csv"), "mes")
    data_list$paises_amp <- read_processed_data(paste0(base_path, "df_paises_mes_amp.csv"), "mes")
    
    # Datos trimestrales
    data_list$ccaa_trim_amp <- read_processed_data(paste0(base_path, "df_ccaa_trim_amp.csv"), "trim")
    data_list$taric_trim_amp <- read_processed_data(paste0(base_path, "df_taric_trim_amp.csv"), "trim")
    data_list$paises_trim_amp <- read_processed_data(paste0(base_path, "df_paises_trim_amp.csv"), "trim")
    
    # Datos anuales
    data_list$ccaa_anos_amp <- read_processed_data(paste0(base_path, "df_ccaa_anos_amp.csv"), "anos")
    data_list$taric_anos_amp <- read_processed_data(paste0(base_path, "df_taric_anos_amp.csv"), "anos")
    data_list$paises_anos_amp <- read_processed_data(paste0(base_path, "df_paises_anos_amp.csv"), "anos")
    
    message("Datos cargados exitosamente")
  }, error = function(e) {
    stop("Error en la carga de datos: ", e$message)
  })
  
  return(data_list)
}

read_processed_data <- function(file_path, data_type) {
  # Leer el archivo directamente como data.table
  df <- fread(file_path)
  
  if (data_type == "mes") {
    
    df[, Fecha := as.IDate(Fecha)] 
  } else if (data_type == "trim") {
    df[, Trimestre := as.character(Trimestre)]
  } else if (data_type == "anos") {
    df[, Año := as.numeric(Año)]
  }
  
  return(df)
}

# Función para verificar los datos
verify_data <- function(data_list) {
  # Verificar dimensiones
  cat("Dimensiones de los dataframes:\n")
  for (name in names(data_list)) {
    cat(sprintf("%s: %d filas, %d columnas\n", 
                name, 
                dim(data_list[[name]])[1], 
                dim(data_list[[name]])[2]))
  }
  
  # Mostrar primeras filas de cada tipo
  cat("\nPrimeras filas de datos mensuales:\n")
  print(head(data_list$ccaa_amp))
  cat("\nPrimeras filas de datos trimestrales:\n")
  print(head(data_list$ccaa_trim_amp))
  cat("\nPrimeras filas de datos anuales:\n")
  print(head(data_list$ccaa_anos_amp))
}

grafica_contribuciones <- function(df, fecha_ini, fecha_fin, flujo_fil, var_fil, col, ano_fil) {
  # Aplicar filtros y convertir valor a numérico
  df_filtrado <- df %>%
    filter(
      Fecha >= fecha_ini,
      Fecha <= fecha_fin,
      flujo %in% flujo_fil,
      var %in% var_fil,
      temp == "datoper"  # Filtrar solo por datoper
    ) %>%
    mutate(valor = as.numeric(valor))
  
  # Obtener el mes y el año de fecha_ini
  mes_ano <- format_fecha(fecha_ini)
  
  # Ordenar por valor de mayor a menor y seleccionar top 3 y bottom 3
  df_plot <- df_filtrado %>%
    arrange(desc(valor)) %>%
    slice(c(1:3, (n()-2):n()))  # Asegúrate de que n() sea mayor que 5 para evitar errores
  
  # Definir título y subtítulo
  titulo <- if(col == "taric") {
    "Capítulos TARIC con las contribuciones más destacadas"
  } else if(col == "pais") {
    "Paises con las contribuciones más destacadas"
  } else {
    "Contribuciones más relevantes"
  }
  
  subtitulo <- if(flujo_fil == "EXPORT") {
    paste0("a la tasa de variación de las exportaciones en ", mes_ano)
  } else if(flujo_fil == "IMPORT") {
    paste0("a la tasa de variación de las importaciones en ", mes_ano)
  } else {
    paste0("Contribución en ", mes_ano)
  }
  
  # Crear plot de barras verticales
  p <- ggplot(df_plot, aes(y = valor, x = reorder(.data[[col]], valor), fill = valor > 0)) +
    geom_col(position = position_dodge(width = 1), width = 0.7) +  # Ajustado width a 0.7
    coord_flip() +  # Voltear coordenadas para hacerlo horizontal
    custom_theme +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    
    scale_fill_manual(values = c(`TRUE` = colde1, `FALSE` = colde3)) +
    guides(fill = "none") +
    
    geom_text(aes(label = paste0(round(valor, 1), " p.p."),
                  hjust = ifelse(valor >= 0, 1, 0)), 
              position = position_dodge(width = 1),
              size = 2, color= "#000000") +
    
    labs(
      title = titulo,
      subtitle = subtitulo,
      y = "Contribución (p.p.)",
      x = NULL,
      fill = "Tipo",
      caption = "Fuente: AEAT"
    )
  
  return(p)
}

grafica_contribuciones_old <- function(df, fecha_ini, fecha_fin, flujo_fil, var_fil, col) {
  # Aplicar filtros y convertir valor a numérico
  df_filtrado <- df %>%
    filter(
      Fecha >= fecha_ini,
      Fecha <= fecha_fin,
      flujo %in% flujo_fil,
      var %in% var_fil,
      #temp %in% c("datoper", "acumulado")
      temp == "datoper",
    ) %>%
    mutate(valor = as.numeric(valor))
  
  # Ordenar por valor de mayor a menor y seleccionar top 3 y bottom 3 para datoper
  df_plot1 <- df_filtrado %>%
    filter(temp == "datoper") %>%
    arrange(desc(valor)) %>%
    slice(c(1:3, (n()-2):n()))
  
  # Ordenar por valor de mayor a menor y seleccionar top 3 y bottom 3 para acumulado
  df_plot2 <- df_filtrado %>%
    filter(temp == "acumulado") %>%
    arrange(desc(valor)) %>%
    slice(c(1:3, (n()-2):n()))
  
  # Obtener valores únicos combinando ambos dataframes
  valores_unicos <- unique(c(df_plot1[[col]], df_plot2[[col]]))
  
  # Calcula tasas
  tv1 <- df_filtrado %>%
    filter(temp == "datoper") %>%
    summarise(suma = sum(valor)) %>%
    pull(suma)
  
  tv2 <- df_filtrado %>%
    filter(temp == "acumulado") %>%
    summarise(suma = sum(valor)) %>%
    pull(suma)
  
  # Combinar los dos dataframes
  df_combined <- df_filtrado %>%
    filter(.data[[col]] %in% valores_unicos)
  
  # Definir título
  titulo <- if(col == "pais" && flujo_fil == "EXPORT") {
    paste0("Con. más relevantes por país a la TVA de las exp. (", format_fecha(fecha_ini), ")")
  } else if(col == "pais" && flujo_fil == "IMPORT") {
    paste0("Con. más relevantes por país a la TVA de las imp. (", format_fecha(fecha_ini), ")")
  } else if(col == "taric" && flujo_fil == "EXPORT") {
    paste0("Con. más relevantes por TARIC a la TVA de las exp. (", format_fecha(fecha_ini), ")")
  } else if(col == "taric" && flujo_fil == "IMPORT") {
    paste0("Con. más relevantes por TARIC a la TVA de las imp. (", format_fecha(fecha_ini), ")")
  } else {
    paste0("Contribución (", format_fecha(fecha_ini), ")")
  }
  # Crear plot de barras verticales
  p <- ggplot(df_combined, aes(y = valor, x = reorder(.data[[col]], valor), fill = temp)) +
    geom_col(position = position_dodge(width = 1), width = 0.7) +  # Ajustado width a 0.7
    coord_flip() +  # Voltear coordenadas para hacerlo horizontal
    custom_theme +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    
    scale_fill_manual(values = c("datoper" = colde1)) +
    guides(fill = "none")  +
    
    # scale_fill_manual(name = "Flujo",
    #                   values = c("datoper" = colde1, "acumulado" = colde3),
    #                   labels = c("datoper" = "mes", "acumulado" = "acumulado")) +
    
    geom_text(aes(label = paste0(round(valor, 1), " p.p."),
                  hjust = ifelse(valor >= 0, 1, 0)), 
              position = position_dodge(width = 1),
              size = 1) +
    
    geom_hline(yintercept = tv1, color = colde1, linetype = "solid") +
    geom_hline(yintercept = tv2, color = colde3, linetype = "solid") +
    # annotate("text", x = Inf, y = tv1, 
    #          label = paste0("TVA ", round(tv1, 1), "%"),
    #          hjust = 1, vjust = 1,
    #          color = colde1,
    #          size = 2) +
    # annotate("text", x = Inf, y = tv2, 
    #          label = paste0("TVA ", round(tv2, 1), "%"),
    #          hjust = 1, vjust = 1,
    #          color = colde2,
    #          size = 2) +
    
    labs(
      title = titulo,
      y = "Contribución (p.p.)",
      x = if(col == "pais") "País" else "TARIC",
      fill = "Tipo"
    )
  
  return(p)
}

grafica_barras_detalle <- function(df, fecha_ini, fecha_fin, flujo_fil, var_fil, col) {
  
  # Aplicar filtros y convertir valor a numérico
  df_filtrado <- df %>%
    filter(
      Fecha >= fecha_ini,
      Fecha <= fecha_fin,
      flujo %in% flujo_fil,
      var %in% var_fil,
      temp %in% c("datoper", "acumulado")
    )
  
  # Seleccionar top 10 basado en valores acumulados para "mes"
  top_valores <- df_filtrado %>%
    filter(temp == "acumulado", var == "mes") %>%
    arrange(desc(valor)) %>%
    slice_head(n = 10) %>%
    pull(!!sym(col))
  
  # Filtrar dataset para incluir solo los top 10
  df_plot <- df_filtrado %>%
    filter(!!sym(col) %in% top_valores)
  
  # Crear un dataframe para las etiquetas combinando mes y peso
  df_labels <- df_plot %>%
    select(!!sym(col), temp, var, valor) %>%
    pivot_wider(
      names_from = var,
      values_from = valor
    ) %>%
    group_by(!!sym(col), temp) %>%
    summarise(
      label = paste0(format(round(mes, 1), decimal.mark = ","), " (", round(peso, 1), "%)"),
      valor = mes,  # Añadir columna valor para el posicionamiento
      .groups = 'drop'
    )
  
  # Definir título
  titulo <- if(col == "pais" && flujo_fil == "EXPORT") {
    paste0("Top 10 países exportaciones (", format_fecha(fecha_ini), ")")
  } else if(col == "pais" && flujo_fil == "IMPORT") {
    paste0("Top 10 países importaciones (", format_fecha(fecha_ini), ")")
  } else if(col == "taric" && flujo_fil == "EXPORT") {
    paste0("Top 10 TARIC exportaciones (", format_fecha(fecha_ini), ")")
  } else if(col == "taric" && flujo_fil == "IMPORT") {
    paste0("Top 10 TARIC importaciones (", format_fecha(fecha_ini), ")")
  } else {
    paste0("Top 10 (", format_fecha(fecha_ini), ")")
  }
  
  # Ordenar los niveles del factor para el eje x
  df_plot_mes <- df_plot %>% 
    filter(var == "mes") %>%
    arrange(desc(valor))
  
  factor_levels <- df_plot_mes %>% 
    pull(!!sym(col)) %>% 
    unique()
  
  df_plot <- df_plot %>%
    mutate(!!sym(col) := factor(!!sym(col), levels = factor_levels))
  
  df_labels <- df_labels %>%
    mutate(!!sym(col) := factor(!!sym(col), levels = factor_levels))
  
  # Crear gráfico
  p <- ggplot(df_plot %>% filter(var == "mes"), 
              aes(x = !!sym(col), y = valor, fill = temp)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    custom_theme +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = if(col == "taric") 45 else 90, hjust = 1)) +
    scale_fill_manual(name = "Periodo",
                      values = c("datoper" = colde1, "acumulado" = colde3),
                      labels = c("datoper" = "mes", "acumulado" = "acumulado")) +
    geom_text(data = df_labels,
              aes(y = ifelse(temp == "datoper", 
                             valor,
                             valor), 
                  label = label,
                  hjust = ifelse(temp == "datoper", -0.1, 1.1)), 
              position = position_dodge(width = 0.7),
              size = 3,
              angle = 90) +
    labs(
      title = titulo,
      y = "Volumen",
      x = if(col == "pais") "País" else "TARIC",
      fill = "Tipo",
      caption = "Fuente: AEAT"
    )
  
  return(p)
}

grafica_barras_detalle_bis <- function(df, fecha_ini, fecha_fin, flujo_fil, var_fil, col) {
  
  # Aplicar filtros y convertir valor a numérico
  df_filtrado <- df %>%
    filter(
      Fecha >= fecha_ini,
      Fecha <= fecha_fin,
      flujo %in% flujo_fil,
      var %in% var_fil,
      temp %in% c("datoper", "acumulado")
    ) %>%
    mutate(valor = as.numeric(valor)) 
  
  # Seleccionar top 10 basado en valores acumulados para "mes"
  top_valores <- df_filtrado %>%
    filter(temp == "acumulado", var == "mes") %>%
    arrange(desc(valor)) %>%
    slice_head(n = 10) %>%
    pull(!!sym(col))
  
  # Filtrar dataset para incluir solo los top 10
  df_plot <- df_filtrado %>%
    filter(!!sym(col) %in% top_valores)
  
  # Crear un dataframe para las etiquetas combinando mes y peso
  df_labels <- df_plot %>%
    select(!!sym(col), temp, var, valor) %>%
    pivot_wider(
      names_from = var,
      values_from = valor
    ) %>%
    group_by(!!sym(col)) %>%
    summarise(
      label = paste0(
        "Mes: ", format(round(first(mes[temp == "datoper"]), 1), decimal.mark = ","),  
        " (", round(first(peso[temp == "datoper"]), 1), "%)\n",  
        "Acum.: ", format(round(first(mes[temp == "acumulado"]), 1), decimal.mark = ","),
        " (", round(first(peso[temp == "acumulado"]), 1), "%)"
      ),
      valor = max(mes),  
      .groups = 'drop'
    )
  
  # Definir título
  titulo <- if(col == "pais" && flujo_fil == "EXPORT") {
    paste0("Top 10 países exportaciones (", format_fecha(fecha_ini), ")")
  } else if(col == "pais" && flujo_fil == "IMPORT") {
    paste0("Top 10 países importaciones (", format_fecha(fecha_ini), ")")
  } else if(col == "taric" && flujo_fil == "EXPORT") {
    paste0("Top 10 TARIC exportaciones (", format_fecha(fecha_ini), ")")
  } else if(col == "taric" && flujo_fil == "IMPORT") {
    paste0("Top 10 TARIC importaciones (", format_fecha(fecha_ini), ")")
  } else {
    paste0("Top 10 (", format_fecha(fecha_ini), ")")
  }
  
  # Ordenar los niveles del factor para el eje x
  df_plot_mes <- df_plot %>% 
    filter(var == "mes") %>%
    arrange(desc(valor))
  
  factor_levels <- df_plot_mes %>% 
    pull(!!sym(col)) %>% 
    unique()
  
  df_plot <- df_plot %>%
    mutate(!!sym(col) := factor(!!sym(col), levels = factor_levels))
  
  df_labels <- df_labels %>%
    mutate(!!sym(col) := factor(!!sym(col), levels = factor_levels))
  
  # Crear gráfico
  # En la parte del gráfico, añadir geom_text después de los geom_col
  p <- ggplot(df_plot %>% filter(var == "mes"), 
              aes(x = !!sym(col), y = valor)) +
    geom_col(data = . %>% filter(temp == "acumulado"),
             aes(fill = "acumulado"),
             width = 0.6) +
    geom_col(data = . %>% filter(temp == "datoper"),
             aes(fill = "datoper"),
             width = 0.6) +
    geom_text(data = df_labels,
              aes(y = valor, label = label),
              hjust = 1,
              vjust = 0.5,
              size = 2,  
              lineheight = 0.8,
              angle = 90) +
    custom_theme +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(name = "Periodo",
                      values = c("datoper" = colde1, "acumulado" = colde3),
                      labels = c("datoper" = "mes", "acumulado" = "acumulado")) +
    labs(
      title = titulo,
      y = "Volumen",
      x = if(col == "pais") "País" else "TARIC",
      fill = "Tipo",
      caption = "Fuente: AEAT"
    )
  
  return(p)
}

grafica_flujos_ccaa <- function(df, flujo_fil, var_fil, temp_fil, ccaa_fil, ano_fil, mes_fil) {
  
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


grafica_flujos_ccaa_con_tva <- function(df, flujo_fil, temp_fil, ccaa_fil, ano_fil, mes_fil) {
  # Aplicar filtros para barras (mes)
  df_barras <- df %>%
    filter(
      ccaa %in% ccaa_fil,
      Mes == mes_fil,
      Año >= ano_fil,
      flujo %in% flujo_fil,
      var == "mes",
      temp %in% temp_fil
    ) %>%
    mutate(valor = as.numeric(valor)) %>%
    arrange(Fecha, flujo)
  
  # Aplicar filtros para línea (TVA)
  df_linea <- df %>%
    filter(
      ccaa %in% ccaa_fil,
      Mes == mes_fil,
      Año >= ano_fil,
      flujo %in% flujo_fil,
      var == "TVA",
      temp %in% temp_fil
    ) %>%
    mutate(valor = as.numeric(valor)) %>%
    arrange(Fecha, flujo)
  
  # Convertir nombre de CCAA
  ccaa_corto <- convertir_ccaa(ccaa_fil)
  
  # Convertir mes a nombre
  meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
             "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  nombre_mes <- meses[mes_fil]
  
  # Nuevo título
  titulo <- paste0("Evolución del comercio exterior de la ", ccaa_corto)
  subtitulo <- paste0("Meses de ", nombre_mes, " desde ", ano_fil)
  
  # Calcular el rango de las barras
  max_barras <- max(df_barras$valor, na.rm = TRUE)
  min_barras <- 0
  rango_barras <- max_barras - min_barras
  
  # Calcular el rango de las líneas (TVA)
  max_tva <- max(df_linea$valor, na.rm = TRUE)
  min_tva <- min(df_linea$valor, na.rm = TRUE)
  rango_tva <- max_tva - min_tva
  
  # Calcular la escala
  factor_escala <- rango_barras / rango_tva
  
  # Ajustar la posición de las líneas para que se centren alrededor de cero
  df_linea <- df_linea %>%
    mutate(valor_escalado = (valor - min_tva) * factor_escala)
  
  # Crear gráfico
  p <- ggplot() +
    # Barras
    geom_bar(data = df_barras,
             aes(x = Año, y = valor, fill = flujo),
             stat = "identity",
             position = position_dodge(width = 0.9),
             width = 0.7) +
    # Líneas
    geom_line(data = df_linea,
              aes(x = Año, y = valor_escalado, color = flujo, group = flujo),
              size = 0.7) +
    geom_point(data = df_linea,
               aes(x = Año, y = valor_escalado, color = flujo),
               size = 1.5) +
    # Línea horizontal en y = 0 para TVA
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.3) +
    # Escalas
    scale_color_manual(name = "TVA",
                       values = c("EXPORT" = colde2, "IMPORT" = colde4),
                       labels = c("EXPORT" = "Exportaciones", "IMPORT" = "Importaciones")) +
    
    scale_fill_manual(name = "Flujo",
                      values = c("EXPORT" = colde1, "IMPORT" = colde3),
                      labels = c("EXPORT" = "Exportaciones", "IMPORT" = "Importaciones")) +
    
    scale_x_continuous(
      name = "Año",
      breaks = seq(min(df$Año), max(df$Año), by = 1),
      expand = c(0, 0)
    ) +
    # Ejes secundarios
    scale_y_continuous(
      name = "Millones de euros",
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      sec.axis = sec_axis(~ . / factor_escala + min_tva, name = "TVA (%)"),
      expand=c(0,0)
    ) +
    custom_theme +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.major.x = element_line(color = "grey", size = 0.25, linetype = "dashed"),
      axis.title.y.right = element_text(size = 7, color = "black", face="bold")
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Año",
      caption = "Fuente: AEAT"
    )
  
  # Añadir etiquetas para el último valor de las barras
  ultimo_dato_barras <- df_barras %>%
    group_by(flujo) %>%
    slice_max(Fecha) %>%
    mutate(
      label = format(round(valor, 1), big.mark = ".", decimal.mark = ",")
    )
  
  # Añadir etiquetas para el último valor de las barras
  ultimo_dato_barras <- df_barras %>%
    group_by(flujo) %>%
    slice_max(Fecha) %>%
    mutate(
      label_barras = format(round(valor, 1), big.mark = ".", decimal.mark = ",")
    )
  
  # Añadir etiquetas para el último valor de las líneas
  ultimo_dato_lineas <- df_linea %>%
    group_by(flujo) %>%
    slice_max(Fecha) %>%
    mutate(
      tva_label = paste0("TVA: ", format(round(valor, 1), decimal.mark = ","), "%")
    )
  
  # Combinar datos de barras y líneas
  ultimo_dato <- ultimo_dato_barras %>%
    left_join(ultimo_dato_lineas, by = "flujo") %>%
    mutate(
      label = paste0(label_barras, " (", tva_label, ")")
    )
  
  # Añadir etiquetas con TVA
  # p <- p +
  #   geom_label_repel(
  #     data = ultimo_dato,
  #     aes(x = Inf, y = valor.x, label = label, fill = flujo),
  #     color = "white",
  #     segment.color = aes(fill = flujo),  # Color de la flecha
  #     box.padding = 0.1,
  #     point.padding = 0.1,
  #     size = 2,
  #     fontface = "bold",  # Texto en negrita
  #     show.legend = FALSE,
  #     max.overlaps = 1
  #   )
  # 
  return(p)
  
}

grafica_mm <- function(df, flujo_fil, temp_fil, ccaa_fil, fecha_ini, fecha_fin) {
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
  
  ccaa_corto <- convertir_ccaa(ccaa_fil)
  
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

grafica_flujo <- function(df, flujo_fil, temp_fil, ccaa_fil, fecha_ini, fecha_fin, mes_filtro) {
  # Filtrar datos según los criterios proporcionados
  df_filtrado <- df %>%
    filter(
      ccaa %in% ccaa_fil,
      var %in% c("mes", "TVA"),
      temp %in% temp_fil,
      flujo %in% flujo_fil,
      Fecha >= fecha_ini,
      Fecha <= fecha_fin,
      Mes == mes_filtro
    )
  
  df1 <- df_filtrado %>% filter(var == "mes")
  df2 <- df_filtrado %>% filter(var == "TVA")
  
  ccaa_corto <- convertir_ccaa(ccaa_fil)
  
  # Determinar el tipo de flujo para el título
  tipo_flujo <- ifelse("EXPORT" %in% flujo_fil, "exportaciones", "importaciones")
  nombre_leyenda <- tools::toTitleCase(tipo_flujo)
  
  # Título y subtítulo del gráfico
  titulo <- paste0("Evolución de las ", tipo_flujo, " de la ", ccaa_corto)
  subtitulo <- paste0("Desde ", format(fecha_ini, "%Y-%m-%d"), " hasta ", format(fecha_fin, "%Y-%m-%d"))
  
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
    geom_line(data = df1, aes(x = Fecha, y = valor, color = "Volumen"), size = 1) +
    # Línea para df2 escalada
    geom_line(data = df2, aes(x = Fecha, y = (valor - min_tva) * factor_escala, color = "TVA"), size = 1) +
    # Escalas y etiquetas
    scale_x_date(
      name = "Fecha",
      date_labels = "%b-%y",
      expand = c(0,0)
    ) +
    scale_y_continuous(
      name = "Volumen",
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      sec.axis = sec_axis(~ . / factor_escala + min_tva, name = "TVA (%)")
    ) +
    scale_color_manual(
      name = nombre_leyenda,
      values = c("Volumen" = colde3, "TVA" = colde1),
      breaks = c("Volumen", "TVA"),
      labels = c("Volumen", "TVA")
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Fecha",
      y = "Volumen",
      caption = "Fuente: AEAT"
    ) +
    custom_theme +
    theme(
      axis.text.x = element_text(angle = 90, hjust=0.5, vjust=0.5)
    )
  
  return(p)
}


grafica_flujo_mes <- function(df, flujo_fil, temp_fil, ccaa_fil, fecha_ini, fecha_fin) {
  # Filtrar datos según los criterios proporcionados
  df_filtrado <- df %>%
    filter(
      ccaa %in% ccaa_fil,
      var %in% c("mes", "TVM"),
      temp %in% temp_fil,
      flujo %in% flujo_fil,
      Fecha >= fecha_ini,
      Fecha <= fecha_fin
    )
  
  df1 <- df_filtrado %>% filter(var == "mes")
  df2 <- df_filtrado %>% filter(var == "TVM")
  
  ccaa_corto <- convertir_ccaa(ccaa_fil)
  
  # Determinar el tipo de flujo para el título
  tipo_flujo <- ifelse("EXPORT" %in% flujo_fil, "exportaciones", "importaciones")
  nombre_leyenda <- tools::toTitleCase(tipo_flujo)
  
  # Título y subtítulo del gráfico
  titulo <- paste0("Evolución de las ", tipo_flujo, " de la ", ccaa_corto)
  subtitulo <- paste0("Desde ", format(fecha_ini, "%Y-%m-%d"), " hasta ", format(fecha_fin, "%Y-%m-%d"))
  
  # Calcular el rango de las áreas
  max_area <- max(df1$valor, na.rm = TRUE)
  min_area <- 0
  rango_area <- max_area - min_area
  
  # Calcular el rango de las líneas (TVA)
  max_tvm <- max(df2$valor, na.rm = TRUE)
  min_tvm <- min(df2$valor, na.rm = TRUE)
  rango_tvm <- max_tvm - min_tvm
  
  # Calcular la escala
  factor_escala <- ifelse(rango_tvm != 0, rango_area / rango_tvm, 1)
  
  # Crear gráfico
  p <- ggplot() +
    # Área para df1
    geom_area(data = df1, aes(x = Fecha, y = valor), fill = colde3, alpha = 1) +
    # Línea para df1
    geom_line(data = df1, aes(x = Fecha, y = valor, color = "Volumen"), size = 1) +
    # Línea para df2 escalada
    geom_line(data = df2, aes(x = Fecha, y = (valor - min_tvm) * factor_escala, color = "TVM"), size = 1) +
    # Escalas y etiquetas
    scale_x_date(
      name = "Fecha",
      date_labels = "%b-%y",
      expand = c(0,0)
    ) +
    scale_y_continuous(
      name = "Volumen",
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      sec.axis = sec_axis(~ . / factor_escala + min_tvm, name = "TVM (%)")
    ) +
    scale_color_manual(
      name = nombre_leyenda,
      values = c("Volumen" = colde3, "TVM" = colde1),
      breaks = c("Volumen", "TVM"),
      labels = c("Volumen", "TVM")
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Fecha",
      y = "Volumen",
      caption = "Fuente: AEAT"
    ) +
    custom_theme +
    theme(
      axis.text.x = element_text(angle = 90, hjust=0.5, vjust=0.5)
    )
  
  return(p)
}

grafica_anos <- function(dataframe, ccaa_fil, flujo_fil, temp_fil, var_fil, mes_filtro, ano_filtro) {
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
  
  ccaa_corto <- convertir_ccaa(ccaa_fil)
  
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