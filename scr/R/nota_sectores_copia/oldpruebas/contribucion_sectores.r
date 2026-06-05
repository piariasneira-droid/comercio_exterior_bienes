.grafica_contribuciones_sectores_combis <- function(
    dt,
    tit = "Contrib",
    parametros = list(
      anho         = 2024L,
      varud        = "M€",
      dec_num      = 1L,
      dec_per      = 1L,
      font_title   = 11,
      font_axis    = 9,
      fuente_texto = "Whitney Light",
      colorbf      = "#FFFFFF"
    )
) {
  
  dec_num    <- parametros$dec_num
  dec_per    <- parametros$dec_per
  font_title <- if (!is.null(parametros$font_title)) parametros$font_title else 11
  font_axis  <- if (!is.null(parametros$font_axis)) parametros$font_axis else 9
  
  df <- data.table::copy(dt)
  df <- df[order(rep)]
  
  paises_recortados <- sapply(
    strsplit(df$paises, ",\\s*"),
    function(lista_paises) {
      paste(substr(trimws(lista_paises), 1L, 14L), collapse = ", ")
    }
  )
  
  df[, etiqueta_completa := paste0(
    nombre,
    "<br>(",
    paises_recortados,
    ")"
  )]
  
  df[, etiqueta_factor := factor(
    etiqueta_completa,
    levels = unique(etiqueta_completa)
  )]
  
  df[, color_sector := ifelse(
    rep >= 0,
    parametros$colpal1,
    parametros$colpal2
  )]
  
  df[, color_paises := ifelse(
    rep_paises >= 0,
    "#B0B0B0",
    "#E0E0E0"
  )]
  
  # Text displayed between bars
  df[, text_con := paste0(
    .fmt_num_inf(rep, dec_per),
    "<br><span style='font-size:8px;color:gray'>(",
    .fmt_num_inf(rep_paises, dec_per),
    ")</span>"
  )]
  
  # Sector bars
  df_sector <- data.table::copy(df)
  df_sector[, bar_type := "sector"]
  df_sector[, y_pos := as.numeric(etiqueta_factor) + 0.14]
  
  # Countries bars
  df_paises <- data.table::copy(df)
  df_paises[, bar_type := "paises"]
  df_paises[, y_pos := as.numeric(etiqueta_factor) - 0.14]
  
  df_combined <- data.table::rbindlist(
    list(df_sector, df_paises)
  )
  
  df_combined[, hover_label := paste0(
    "<b>", nombre, "</b><br><br>",
    "Contribución sector: ", .fmt_pp(rep, dec_per), " p.p.<br>",
    "TVA: ", .fmt_pp(tva, dec_per), "%<br>",
    "Países relevantes: ", paises, "<br>",
    "Contribución mercados a sector: ",
    .fmt_pp(rep_paises, dec_per), " p.p."
  )]
  
  fig <- plotly::plot_ly()
  
  # Sector bars
  df_sector_plot <- df_combined[bar_type == "sector"]
  
  fig <- fig |>
    plotly::add_bars(
      data          = df_sector_plot,
      y             = ~y_pos,
      x             = ~rep,
      name          = "Contribución sector",
      marker        = list(color = ~color_sector),
      hovertext     = ~hover_label,
      hovertemplate = "%{hovertext}<extra></extra>",
      orientation   = "h",
      width         = 0.66
    )
  
  # Countries bars
  df_paises_plot <- df_combined[bar_type == "paises"]
  
  fig <- fig |>
    plotly::add_bars(
      data          = df_paises_plot,
      y             = ~y_pos,
      x             = ~rep_paises,
      name          = "Contribución países",
      marker        = list(color = ~color_paises),
      hoverinfo     = "skip",
      orientation   = "h",
      width         = 0.10
    ) |>
    plotly::layout(
      
      title = list(
        text = paste0("<b>", tit, "</b>"),
        x = 0.5,
        font = list(
          size = font_title,
          color = "black",
          family = parametros$fuente_texto
        )
      ),
      
      barmode = "overlay",
      
      xaxis = list(
        title = list(
          text = "Contribución (p.p.)",
          font = list(family = parametros$fuente_texto)
        ),
        tickfont = list(
          size = font_axis,
          family = parametros$fuente_texto
        ),
        automargin = TRUE,
        showgrid = FALSE,
        zeroline = TRUE,
        zerolinecolor = "black",
        zerolinewidth = 1,
        showline = TRUE,
        linecolor = "black",
        linewidth = 1
      ),
      
      yaxis = list(
        title = "",
        tickfont = list(
          size = font_axis,
          family = parametros$fuente_texto
        ),
        tickmode = "array",
        tickvals = seq_len(nrow(df)),
        ticktext = df$etiqueta_completa,
        automargin = TRUE,
        showgrid = FALSE,
        zeroline = FALSE,
        showline = TRUE,
        linecolor = "black",
        linewidth = 1
      ),
      
      showlegend = FALSE,
      
      paper_bgcolor = parametros$colorbf,
      plot_bgcolor  = "rgba(0,0,0,0)",
      
      margin = list(
        b = 100
      ),
      
      annotations = lapply(seq_len(nrow(df)), function(i) {
        
        positivo <- df$rep[i] >= 0
        
        list(
          x         = 0,
          y         = i,
          text      = df$text_con[i],
          
          xanchor   = ifelse(
            positivo,
            "right",
            "left"
          ),
          
          yanchor   = "middle",
          
          showarrow = FALSE,
          
          xref      = "x",
          yref      = "y",
          
          xshift    = ifelse(
            positivo,
            -4,
            4
          ),
          
          align = "center",
          
          font = list(
            color  = "black",
            size   = 9,
            family = parametros$fuente_texto
          )
        )
      })
    )
  
  return(fig)
}

contribalt_exp_esp_sec <- .grafica_contribuciones_sectores_combis(
  dt = df_contrib_sec_exp_informe,
  tit = "Contribuciones más destacadas a la tasa de variación de las exportaciones madrileñas",
  parametros = paramets
)

print(contribalt_exp_esp_sec)

htmlwidgets::saveWidget(
  contribalt_exp_esp_sec,
  file = file.path(
    getwd(),
    paste0("contribuciones_sector_exp_mad", ".html")
  ),
  selfcontained = TRUE
)