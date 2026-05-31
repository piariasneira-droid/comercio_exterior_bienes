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

#### Tema plotly -----
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

#### Carga TARIC, paises y periodo ----
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

periodos_choices <- c(
  setNames(1:12,
           c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
             "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")),
  
  setNames(21:24,
           c("Trimestre 1","Trimestre 2","Trimestre 3","Trimestre 4")),
  
  setNames(31:32,
           c("Semestre 1","Semestre 2")),
  
  setNames(41, "Año entero"),
  
  setNames(51:58,
           c("Enero-Febrero","Enero-Abril","Enero-Mayo",
             "Enero-Julio","Enero-Agosto","Enero-Septiembre",
             "Enero-Octubre","Enero-Noviembre"))
)

# Mapeo de periodos a meses
periodos_map <- list(
  "1"  = 1L, "2"  = 2L, "3"  = 3L, "4"  = 4L, "5"  = 5L, "6"  = 6L,
  "7"  = 7L, "8"  = 8L, "9"  = 9L, "10" = 10L, "11" = 11L, "12" = 12L,
  "21" = 1L:3L, "22" = 4L:6L, "23" = 7L:9L, "24" = 10L:12L,
  "31" = 1L:6L, "32" = 7L:12L,
  "41" = 1L:12L,
  "51" = 1L:2L, "52" = 1L:4L, "53" = 1L:5L, "54" = 1L:7L, "55" = 1L:8L,   
  "56" = 1L:9L, "57" = 1L:10L, "58" = 1L:11L   
)

obtener_meses_periodo <- function(cod_periodo) {
  meses <- periodos_map[[as.character(cod_periodo)]]
  if (is.null(meses)) {
    return(NA_integer_) 
  }
  return(meses)
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

#### Plots ----
##### Treemaps -----
grafica_treemap_taric <- function(dt, vars, aux, tipo_plot = "treemap"){
  var <- aux$var
  
  # Eliminar columnas no necesarias
  dt[, c("pais", "region", "continente", "nombre") := NULL]
  
  # Crear jerarquía de padres basada en el código TARIC y nivel
  dt[nivel_taric == 1, parent := ""]
  dt[nivel_taric == 2, parent := substr(cod_taric_char, 1, 2)]
  dt[nivel_taric == 3, parent := substr(cod_taric_char, 1, 4)]
  dt[nivel_taric == 4, parent := substr(cod_taric_char, 1, 6)]
  dt[nivel_taric == 5, parent := substr(cod_taric_char, 1, 8)]

  # Calcular volumen escalado
  max_int <- .Machine$integer.max - 1
  max_vol <- max(dt[nivel_taric == 1, get(var)], na.rm = TRUE)
  scale_factor <- max_vol / max_int
  dt[, Volumen := as.integer(get(var) / scale_factor)]
  
  # Variables auxiliares en millones
  dt[, Vol := get(var) / aux$varfactor]
  dt[, Vol_ant := euros_ant / aux$varfactor]
  dt[, Dif := diferencia / aux$varfactor]
  
  # Calcular límites de la escala de colores (Contribución en porcentaje)
  contribucion_min <- dt[nivel_taric > 0, min(contribucion, na.rm = TRUE)]
  contribucion_max <- dt[nivel_taric > 0, max(contribucion, na.rm = TRUE)]
  zero_position <- (-contribucion_min) / (contribucion_max - contribucion_min)
  
  # Crear texto hover
  dt[, hover_text := paste0(
    "<b>Flujo:</b> ", aux$nombre_flujo, "<br>",
    "<b>País:</b> ", aux$nombre_pais, "<br>",
    "<b>Producto:</b> ", Tar, "<br>",
    "<b>Volumen:</b> ", format(sprintf("%.1f", Vol), big.mark = ".", decimal.mark = ",", scientific = FALSE), " ", aux$varud, "<br>",
    "Volumen año anterior: ", format(sprintf("%.1f", Vol_ant), big.mark = ".", decimal.mark = ",", scientific = FALSE), " ", aux$varud, "<br>",
    "Diferencia: ", format(sprintf("%.1f", Dif), big.mark = ".", decimal.mark = ",", scientific = FALSE), " ", aux$varud, "<br>",
    "TVA: ", ifelse(is.na(tva), "-", format(sprintf("%.2f", tva), big.mark = ".", decimal.mark = ",", scientific = FALSE)), "% <br>",
    "<b>Contribución:</b> ", format(sprintf("%.2f", contribucion), big.mark = ".", decimal.mark = ",", scientific = FALSE), " p.p.<br>",
    "Peso: ", format(sprintf("%.2f", peso), big.mark = ".", decimal.mark = ",", scientific = FALSE), "%"
  )]
  
  # Crear figura
  fig <- plot_ly(
    data = dt,
    type = tipo_plot,
    ids = ~cod_taric_char,
    labels = ~cod_taric_char,
    parents = ~parent,
    values = ~Volumen,
    text = ~hover_text,
    branchvalues = "total",
    textinfo = "none",
    hovertemplate = "%{text}<extra></extra>",
    marker = list(
      colors = ~contribucion,
      # colorscale = list(
      #   list(0, aux$paleta[2]),
      #   list(1, aux$paleta[1])
      # ),
      colorscale = list(
        list(0, "red"),
        list(zero_position, "lightgrey"),
        list(1, "green")
      ),
      colorbar = list(
        title = "Contribución (%)",
        ticksuffix = " p.p."
      ),
      cmin = contribucion_min,
      cmax = contribucion_max,
      reversescale = FALSE
    )
  ) %>%
    layout(custom_theme_plotly()) %>%
    layout(uniformtext=list(minsize=10, mode='hide')) %>%
    style(root = list(color = "lightgrey")) 
  
  return(fig)
}

grafica_treemap_paises <- function(dt, vars, aux, tipo_plot = "treemap") {
  var <- aux$var
  var_ant <- paste0(var, "_ant") 
  
  # Totales del periodo actual y anterior
  total_actual <- max(dt$total_general, na.rm = TRUE)
  total_anterior <- max(dt$total_general_ant, na.rm = TRUE)
  
  # Nivel raíz
  total_row <- data.table(
    labels = "Total",
    parents = "",
    values = total_actual,
    values_ant = total_anterior
  )
  
  # Nivel continente
  continentes <- dt[, .(
    values = sum(get(var), na.rm = TRUE),
    values_ant = sum(get(var_ant), na.rm = TRUE)
  ), by = continente]
  continentes[, parents := "Total"]
  setnames(continentes, "continente", "labels")
  continentes[labels == "África", labels := "África (cont.)"]
  continentes[labels == "No determinado**", labels := "No det (cont.)"]
  
  # Nivel región
  regiones <- dt[, .(
    values = sum(get(var), na.rm = TRUE),
    values_ant = sum(get(var_ant), na.rm = TRUE)
  ), by = .(continente, region)]
  setnames(regiones, c("region", "continente"), c("labels", "parents"))
  regiones[labels == "China", labels := "China (reg.)"]
  regiones[labels == "Japón", labels := "Japón (reg.)"]
  regiones[labels == "Australia", labels := "Australia (reg.)"]
  regiones[parents == "África", parents := "África (cont.)"]
  regiones[parents == "No determinado**", parents := "No det (cont.)"]
  
  # Nivel país
  paises <- dt[, .(
    values = sum(get(var), na.rm = TRUE),
    values_ant = sum(get(var_ant), na.rm = TRUE)
  ), by = .(nombre, region)]
  setnames(paises, c("nombre", "region"), c("labels", "parents"))
  paises[parents == "China", parents := "China (reg.)"]
  paises[parents == "Japón", parents := "Japón (reg.)"]
  paises[parents == "Australia", parents := "Australia (reg.)"]
  
  # Unir todos los niveles
  jerarquia <- rbindlist(
    list(total_row, continentes, regiones, paises),
    use.names = TRUE,
    fill = TRUE
  )
  
  # Cálculos derivados
  jerarquia[, Dif:= values - values_ant]
  jerarquia[, tva := ifelse(Dif == 0, NA, (Dif / values_ant) * 100)]
  jerarquia[, peso := values / total_actual * 100]
  jerarquia[, contribucion := Dif / total_anterior * 100]
  
  # Crear columnas en millones
  jerarquia[, `:=`(
    values_mill = values / 1e6,
    values_ant_mill = values_ant / 1e6,
    Dif_mill = Dif / 1e6
  )]
  
  # Crear texto hover
  jerarquia[, hover_text := paste0(
    "<b>Flujo:</b> ", aux$nombre_flujo, "<br>",
    "<b>Elemento:</b> ", labels, "<br>",
    "<b>Producto:</b> ", aux$nombre_taric, "<br>",
    "<b>Volumen:</b> ", format(sprintf("%.1f", values_mill), big.mark = ".", decimal.mark = ",", scientific = FALSE), " ", aux$varud, "<br>",
    "Volumen año anterior: ", format(sprintf("%.1f", values_ant_mill), big.mark = ".", decimal.mark = ",", scientific = FALSE), " ", aux$varud, "<br>",
    "Diferencia: ", format(sprintf("%.1f", Dif_mill), big.mark = ".", decimal.mark = ",", scientific = FALSE), " ", aux$varud, "<br>",
    "TVA: ", ifelse(is.na(tva), "-", format(sprintf("%.2f", tva), big.mark = ".", decimal.mark = ",", scientific = FALSE)), "% <br>",
    "<b>Contribución:</b> ", format(sprintf("%.2f", contribucion), big.mark = ".", decimal.mark = ",", scientific = FALSE), " p.p.<br>",
    "Peso: ", format(sprintf("%.2f", peso), big.mark = ".", decimal.mark = ",", scientific = FALSE), "%"
  )]
  
  # Crear texto a mostrar en el gráfico (label + valor en millones)
  jerarquia[, text_label := paste0(
    labels, "<br>",
    format(sprintf("%.1f", values_mill), big.mark = ".", decimal.mark = ",", scientific = FALSE), " ", aux$varud
  )]
  
  # Escalado para el gráfico
  max_vol <- max(continentes$values, na.rm = TRUE)
  max_int <- .Machine$integer.max
  scale_factor <- max_vol / max_int
  jerarquia[, Volumen := as.integer(values / scale_factor)]
  
  # Calcular límites de la escala de colores (Contribución en porcentaje)
  contribucion_min <- min(jerarquia$contribucion, na.rm = TRUE)
  contribucion_max <- max(jerarquia$contribucion, na.rm = TRUE)
  # contribucion_min <- min(paises$contribucion, na.rm = TRUE)
  # contribucion_max <- max(paises$contribucion, na.rm = TRUE)
  zero_position <- (-contribucion_min) / (contribucion_max - contribucion_min)
  
  # Gráfico Treemap
  fig <- plot_ly(
    data = jerarquia,
    type = "treemap",
    ids = ~labels,
    labels = ~labels,
    parents = ~parents,
    values = ~Volumen,
    branchvalues = "total",
    text = ~text_label,           
    textinfo = "text",            
    hoverinfo = "text",       
    hovertext = ~hover_text,
    marker = list(
      colors = ~contribucion,
      # colorscale = list(
      #   list(0, aux$paleta[2]),
      #   list(1, aux$paleta[1])
      # ),
      colorscale = list(
        list(0, "red"),
        list(zero_position, "lightgrey"),
        list(1, "green")
      ),
      colorbar = list(
        title = "Contribución (%)",
        ticksuffix = " p.p."
      ),
      cmin = contribucion_min,
      cmax = contribucion_max,
      reversescale = FALSE
    )
  ) %>%
    layout(uniformtext = list(minsize = 10, mode = "hide")) %>%
    style(root = list(color = "lightgrey"))
  
  return(fig)
}