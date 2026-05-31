server <- function(input, output, session) {
  
  #### Reactive: Parámetros basados en filtros ----
  parametros_react <- shiny::reactive({
    crear_listas_parametros(
      region         = input$region,
      ano            = base::as.integer(input$anio),
      per            = base::as.integer(input$periodo),
      fpais          = base::as.integer(input$pais),
      fsec           = input$sector,
      mapeo_pais     = df_pais,
      mapeo_sectores = df_sectores
    )
  })
  
  #### Reactive: Dataset Arrow ----
  dataset_react <- shiny::reactive({
    params <- parametros_react()
    shiny::req(params$archivo)
    
    # Abrir dataset con arrow
    ds <- arrow::open_dataset(params$archivo)
    
    # Excluir la columna 'estado'
    ds %>% dplyr::select(-estado)
  })
  
  #### Reactive: Totales ----
  totales_react <- shiny::reactive({
    shiny::req(dataset_react(), parametros_react())
    calculo_totales(
      df_query = dataset_react(),
      param    = parametros_react()
    )
  })
  
  #### Reactive: Tabla Sectores ----
  df_tabla_sectores_react <- shiny::reactive({
    shiny::req(dataset_react(), totales_react(), parametros_react())
    tabla_sectores_datacomex(
      datas      = dataset_react(),
      tot        = totales_react(),
      df_sec     = df_sectores,
      parametros = parametros_react()
    )
  })
  
  #### Reactive: Tabla Países ----
  df_tabla_paises_react <- shiny::reactive({
    shiny::req(dataset_react(), totales_react(), parametros_react())
    tabla_paises_datacomex(
      datas      = dataset_react(),
      tot        = totales_react(),
      df_paises  = df_pais,
      parametros = parametros_react()
    )
  })
  
  #### Reactive: Gráficas temporales sectores ----
  lista_plots_sect_temporales_react <- shiny::reactive({
    shiny::req(dataset_react(), totales_react(), parametros_react())
    graficas_temporales_sectores(
      datas = dataset_react(),
      tot   = totales_react(),
      para  = parametros_react()
    )
  })
  
  #### Reactive: Gráficas evolución sector/país ----
  lista_temporal_react <- shiny::reactive({
    shiny::req(dataset_react(), parametros_react())
    graficas_evolucion_secpais(
      datas = dataset_react(),
      para  = parametros_react()
    )
  })
  
  #### OUTPUTS: Tablas ----
  
  ##### Tabla Sectores ----
  output$tabla_sectores <- DT::renderDT({
    shiny::req(df_tabla_sectores_react(), parametros_react())
    
    render_datatable_datacomexsec_desplegable(
      df = df_tabla_sectores_react()[, .(
        orden, niv, nombre,
        exp, exp_per_reg, tva_exp, con_exp,
        imp, imp_per_reg, tva_imp, con_imp,
        saldo, saldo_prev
      )],
      param            = parametros_react(),
      cols_semaforo    = base::c("tva_exp", "tva_imp", "saldo", "saldo_prev"),
      cols_barras_cien = base::c("exp_per_reg", "imp_per_reg"),
      cols_barras_con  = base::c("con_exp", "con_imp"),
      cols_enteros     = base::c("orden")
    )
  }, server = FALSE)
  
  ##### Tabla Países ----
  output$tabla_paises <- DT::renderDT({
    shiny::req(df_tabla_paises_react(), parametros_react())
    
    render_datatable_datacomexpaises_desplegable(
      df = df_tabla_paises_react()[, .(
        orden, niv, pais,
        exp, exp_per_reg, tva_exp, con_exp,
        imp, imp_per_reg, tva_imp, con_imp,
        saldo, saldo_prev
      )],
      param            = parametros_react(),
      cols_semaforo    = base::c("tva_exp", "tva_imp", "saldo", "saldo_prev"),
      cols_barras_cien = base::c("exp_per_reg", "imp_per_reg"),
      cols_barras_con  = base::c("con_exp", "con_imp"),
      cols_enteros     = base::c("orden")
    )
  }, server = FALSE)
  
  #### OUTPUTS: Volumen Subsectores ----
  
  ##### Volumen Subsectores Exportaciones ----
  output$vol_subsectores_exp <- plotly::renderPlotly({
    shiny::req(df_tabla_sectores_react())
    
    grafica_volumen_subsectores(
      df = df_tabla_sectores_react(),
      para = parametros_react(),
      nmax = 8,
      flujo = "exp"
    )
  })
  
  ##### Volumen Subsectores Importaciones ----
  output$vol_subsectores_imp <- plotly::renderPlotly({
    shiny::req(df_tabla_sectores_react())
    
    grafica_volumen_subsectores(
      df = df_tabla_sectores_react(),
      para = parametros_react(),
      nmax = 8,
      flujo = "imp"
    )
  })
  
  #### OUTPUTS: Contribuciones Subsectores ----
  
  ##### Contribuciones Subsectores Exportaciones ----
  output$con_subsectores_exp <- plotly::renderPlotly({
    shiny::req(df_tabla_sectores_react())
    
    grafica_contribuciones_subsectores(
      df = df_tabla_sectores_react(),
      para = parametros_react(),
      nmax = 4,
      flujo = "exp"
    )
  })
  
  ##### Contribuciones Subsectores Importaciones ----
  output$con_subsectores_imp <- plotly::renderPlotly({
    shiny::req(df_tabla_sectores_react())
    
    grafica_contribuciones_subsectores(
      df = df_tabla_sectores_react(),
      para = parametros_react(),
      nmax = 4,
      flujo = "imp"
    )
  })
  
  #### OUTPUTS: Volumen Países ----
  
  ##### Volumen Países Exportaciones ----
  output$vol_paises_exp <- plotly::renderPlotly({
    shiny::req(df_tabla_paises_react())
    
    grafica_volumen_paises(
      df = df_tabla_paises_react(),
      para = parametros_react(),
      nmax = 8,
      flujo = "exp"
    )
  })
  
  ##### Volumen Países Importaciones ----
  output$vol_paises_imp <- plotly::renderPlotly({
    shiny::req(df_tabla_paises_react())
    
    grafica_volumen_paises(
      df = df_tabla_paises_react(),
      para = parametros_react(),
      nmax = 8,
      flujo = "imp"
    )
  })
  
  #### OUTPUTS: Contribuciones Países ----
  ##### Contribuciones Países Exportaciones ----
  output$con_paises_exp <- plotly::renderPlotly({
    shiny::req(df_tabla_paises_react())
    
    grafica_contribuciones_paises(
      df = df_tabla_paises_react(),
      para = parametros_react(),
      nmax = 4,
      flujo = "exp"
    )
  })
  
  ##### Contribuciones Países Importaciones ----
  output$con_paises_imp <- plotly::renderPlotly({
    shiny::req(df_tabla_paises_react())
    
    grafica_contribuciones_paises(
      df = df_tabla_paises_react(),
      para = parametros_react(),
      nmax = 4,
      flujo = "imp"
    )
  })
  
  #### OUTPUTS: Treemaps Sectores ----
  ##### Treemap Exportaciones Sectores ----
  output$treemap_sectores_exp <- plotly::renderPlotly({
    shiny::req(df_tabla_sectores_react())
    
    grafica_treemap_informe(
      dt    = df_tabla_sectores_react(),
      tipo  = "sectores",
      flujo = "exp",
      para  = parametros_react()
    )
  })
  
  ##### Treemap Importaciones Sectores ----
  output$treemap_sectores_imp <- plotly::renderPlotly({
    shiny::req(df_tabla_sectores_react())
    
    grafica_treemap_informe(
      dt    = df_tabla_sectores_react(),
      tipo  = "sectores",
      flujo = "imp",
      para  = parametros_react()
    )
  })
  
  #### OUTPUTS: Treemaps Países ----
  
  ##### Treemap Exportaciones Países ----
  output$treemap_paises_exp <- plotly::renderPlotly({
    shiny::req(df_tabla_paises_react())
    
    grafica_treemap_informe(
      dt    = df_tabla_paises_react(),
      tipo  = "paises",
      flujo = "exp",
      para  = parametros_react()
    )
  })
  
  ##### Treemap Importaciones Países ----
  output$treemap_paises_imp <- plotly::renderPlotly({
    shiny::req(df_tabla_paises_react())
    
    grafica_treemap_informe(
      dt    = df_tabla_paises_react(),
      tipo  = "paises",
      flujo = "imp",
      para  = parametros_react()
    )
  })
  
  #### OUTPUTS: Gráficas Temporales ----
  
  ##### Gráfico Animado Sectores ----
  output$plot_sec_animado <- plotly::renderPlotly({
    shiny::req(lista_plots_sect_temporales_react())
    lista_plots_sect_temporales_react()$animado
  })
  
  ##### Gráfico Facetado Sectores ----
  output$plot_sec_facetado <- plotly::renderPlotly({
    shiny::req(lista_plots_sect_temporales_react())
    lista_plots_sect_temporales_react()$spaghetti
  })
  
  ##### Evolución Anual ----
  output$plot_temporal_anual <- plotly::renderPlotly({
    shiny::req(lista_temporal_react())
    lista_temporal_react()$fig_anual
  })
  
  ##### Evolución Mensual ----
  output$plot_temporal_mensual <- plotly::renderPlotly({
    shiny::req(lista_temporal_react())
    lista_temporal_react()$fig_mensual
  })
  
  #### Textos descriptores ----
  
  output$descripcion_sectores <- shiny::renderUI({
    shiny::div(
      class = "descripcion-tab",
      shiny::h5(shiny::icon("info-circle"), " Análisis por Sectores Económicos"),
      
      shiny::p("Esta sección permite explorar la ", shiny::strong("estructura del comercio exterior"), 
               " desglosada por sectores económicos para la región y período seleccionados, filtrando ",
               "los datos en base al país seleccionado."),
      
      # Parámetros dinámicos
      shiny::div(
        style = "background-color: #e8f4f8; padding: 12px 15px; border-radius: 5px; margin: 15px 0;",
        shiny::h6(shiny::icon("filter"), " Parámetros seleccionados:", 
                  style = "margin: 0 0 10px 0; color: #2c3e50;"),
        shiny::div(
          style = "display: flex; flex-wrap: wrap; gap: 10px;",
          shiny::span(
            style = "background-color: #3498db; color: white; padding: 5px 12px; border-radius: 15px; font-size: 13px;",
            shiny::icon("map-marker-alt"), " Territorio: ", shiny::strong(parametros_react()$nombre_region)
          ),
          shiny::span(
            style = "background-color: #9b59b6; color: white; padding: 5px 12px; border-radius: 15px; font-size: 13px;",
            shiny::icon("calendar-alt"), " Período: ", shiny::strong(base::paste0(
              base::toupper(base::substr(base::tolower(parametros_react()$texto_periodo), 1, 1)),
              base::substr(base::tolower(parametros_react()$texto_periodo), 2, base::nchar(parametros_react()$texto_periodo)),
              " ", parametros_react()$ano))
          ),
          shiny::span(
            style = "background-color: #27ae60; color: white; padding: 5px 12px; border-radius: 15px; font-size: 13px;",
            shiny::icon("globe"), " País/Zona: ", shiny::strong(parametros_react()$nombre_pais)
          )
        )
      ),
      
      shiny::p("A continuación se muestra:"),
      shiny::tags$ul(
        shiny::tags$li(shiny::strong("Treemaps:"), " Visualización proporcional del peso de cada sector en las exportaciones e importaciones. ",
                       "El tamaño de cada rectángulo representa el volumen comercial. ",
                       "Se incluye una escala de color con el valor de la contribución a la tasa de variación del total de la región."),
        shiny::tags$li(shiny::strong("Tabla de datos:"), " Detalle numérico con valores absolutos, porcentajes y variaciones interanuales por sector. ",
                       "Se puede desglosar por subsectores."),
        shiny::tags$li(shiny::strong("Análisis por subsectores:"), " Desagregación adicional que permite examinar el volumen comercial ",
                       "y la contribución a la tasa de variación de los subsectores más relevantes, tanto en exportaciones como importaciones.")
      ),
      
      shiny::p(style = "margin-bottom: 0; font-style: italic; color: #666;",
               "💡 Tip: Pasa el cursor sobre los treemaps para ver información detallada de cada sector."),
      
      shiny::p(style = "margin-top: 10px; font-size: 12px; color: #888; border-top: 1px dashed #ccc; padding-top: 10px;",
               shiny::icon("check-circle"), shiny::strong(" Comprobación de datos: "), 
               "Seleccionando España, el período correspondiente y los totales para país y sector, ",
               "la tabla de datos debería coincidir con la publicada en los ",
               shiny::tags$a(href = "https://comercio.gob.es/importacionexportacion/informes_estadisticas/paginas/informes-periodicos.aspx", 
                             target = "_blank", "informes mensuales de comercio exterior"),
               " del Ministerio de Economía, Comercio y Empresa. ",
               shiny::tags$a(href = "https://comercio.gob.es/ImportacionExportacion/Informes_Estadisticas/Documents/informe-mensual/Informe-Mensual-de-Comercio-Exterior-ultimo-periodo.pdf",
                             target = "_blank", style = "color: #3c8dbc;",
                             shiny::icon("file-pdf"), " Acceso al último informe")
      )
    )
  })
  
  output$descripcion_paises <- shiny::renderUI({
    shiny::div(
      class = "descripcion-tab",
      shiny::h5(shiny::icon("info-circle"), " Análisis por Países y Regiones"),
      
      shiny::p("Esta sección muestra la ", shiny::strong("distribución geográfica del comercio exterior"), 
               " para la región y período seleccionados, filtrando por el sector especificado."),
      
      # Parámetros dinámicos
      shiny::div(
        style = "background-color: #e8f4f8; padding: 12px 15px; border-radius: 5px; margin: 15px 0;",
        shiny::h6(shiny::icon("filter"), " Parámetros seleccionados:", 
                  style = "margin: 0 0 10px 0; color: #2c3e50;"),
        shiny::div(
          style = "display: flex; flex-wrap: wrap; gap: 10px;",
          shiny::span(
            style = "background-color: #3498db; color: white; padding: 5px 12px; border-radius: 15px; font-size: 13px;",
            shiny::icon("map-marker-alt"), " Territorio: ", shiny::strong(parametros_react()$nombre_region)
          ),
          shiny::span(
            style = "background-color: #9b59b6; color: white; padding: 5px 12px; border-radius: 15px; font-size: 13px;",
            shiny::icon("calendar-alt"), " Período: ", shiny::strong(base::paste0(
              base::toupper(base::substr(base::tolower(parametros_react()$texto_periodo), 1, 1)),
              base::substr(base::tolower(parametros_react()$texto_periodo), 2, base::nchar(parametros_react()$texto_periodo)),
              " ", parametros_react()$ano))
          ),
          shiny::span(
            style = "background-color: #e67e22; color: white; padding: 5px 12px; border-radius: 15px; font-size: 13px;",
            shiny::icon("industry"), " Sector: ", shiny::strong(parametros_react()$nombre_sector)
          )
        )
      ),
      
      shiny::p("A continuación se muestra:"),
      shiny::tags$ul(
        shiny::tags$li(shiny::strong("Treemaps:"), " Representación visual de los principales socios comerciales. ",
                       "Cada rectángulo representa un país o región, y su tamaño es proporcional al volumen de intercambio. ",
                       "La escala de color indica la contribución a la tasa de variación."),
        shiny::tags$li(shiny::strong("Tabla de datos:"), " Ranking detallado con cifras de exportación, importación, ",
                       "saldo comercial y contribuciones por destino/origen. Las regiones se despliegan mostrando los principales ",
                       "socios comerciales."),
        shiny::tags$li(shiny::strong("Análisis por países:"), " Desagregación que permite examinar el volumen comercial ",
                       "y la contribución a la tasa de variación de los principales países, tanto en exportaciones como importaciones.")
      ),
      
      shiny::p(style = "margin-bottom: 0; font-style: italic; color: #666;",
               "💡 Tip: Utiliza el selector de país en el panel lateral para filtrar por un socio comercial específico o por agrupaciones regionales."),
      
      shiny::p(style = "margin-top: 10px; font-size: 12px; color: #888; border-top: 1px dashed #ccc; padding-top: 10px;",
               shiny::icon("check-circle"), shiny::strong(" Comprobación de datos: "), 
               "Seleccionando España, el período correspondiente y los totales para país y sector, ",
               "la tabla de datos debería coincidir con la publicada en los ",
               shiny::tags$a(href = "https://comercio.gob.es/importacionexportacion/informes_estadisticas/paginas/informes-periodicos.aspx", 
                             target = "_blank", "informes mensuales de comercio exterior"),
               " del Ministerio de Economía, Comercio y Empresa. ",
               shiny::tags$a(href = "https://comercio.gob.es/ImportacionExportacion/Informes_Estadisticas/Documents/informe-mensual/Informe-Mensual-de-Comercio-Exterior-ultimo-periodo.pdf",
                             target = "_blank", style = "color: #3c8dbc;",
                             shiny::icon("file-pdf"), " Acceso al último informe")
      )
    )
  })
  
  output$descripcion_evolucion <- renderUI({
    div(
      class = "descripcion-tab",
      h5(icon("info-circle"), " Análisis de Evolución Temporal"),
      
      p("Esta sección presenta la ", strong("dinámica histórica del comercio exterior"), 
        " para la combinación de región, sector y ámbito geográfico seleccionados."),
      
      p("Debido a las limitaciones de capacidad de cómputo del servidor gratuito utilizado, ",
        "se muestran únicamente dos gráficos principales: uno con evolución ", strong("mensual"), 
        " y otro con evolución ", strong("anual"), " (agregando los meses del período seleccionado). ",
        "Adicionalmente, se incluyen dos visualizaciones complementarias a modo de portfolio ",
        "sobre la evolución temporal de los principales sectores."),
      
      div(
        style = "background-color: #e8f4f8; padding: 12px 15px; border-radius: 5px; margin: 15px 0;",
        h6(icon("filter"), " Parámetros seleccionados:", style = "margin: 0 0 10px 0; color: #2c3e50;"),
        div(
          style = "display: flex; flex-wrap: wrap; gap: 10px;",
          span(style = "background-color: #3498db; color: white; padding: 5px 12px; border-radius: 15px; font-size: 13px;",
               icon("map-marker-alt"), " Territorio: ", strong(parametros_react()$nombre_region)),
          span(style = "background-color: #9b59b6; color: white; padding: 5px 12px; border-radius: 15px; font-size: 13px;",
               icon("calendar-alt"), " Meses incluidos: ", strong(parametros_react()$texto_meses)),
          span(style = "background-color: #27ae60; color: white; padding: 5px 12px; border-radius: 15px; font-size: 13px;",
               icon("globe"), " País/Zona: ", strong(parametros_react()$nombre_pais)),
          span(style = "background-color: #e67e22; color: white; padding: 5px 12px; border-radius: 15px; font-size: 13px;",
               icon("industry"), " Sector: ", strong(parametros_react()$nombre_sector))
        )
      ),
      
      p("A continuación se muestra:"),
      tags$ul(
        tags$li(strong("Gráfico animado:"), " Evolución dinámica del peso relativo de los principales sectores ",
                "sobre el total exportado, desde 1995 hasta 2025. Pulsa ", em("Play"), " para iniciar la animación ",
                "o utiliza el slider inferior para seleccionar un período específico."),
        tags$li(strong("Gráfico facetado:"), " Comparación simultánea del porcentaje sobre el total anual importado ",
                "de los principales sectores, considerando los meses seleccionados."),
        tags$li(strong("Evolución sector/país:"), " Análisis detallado de la combinación sector-país elegida ",
                "en el panel lateral, con desagregación anual y mensual.")
      ),
      
      p(style = "margin-bottom: 0; font-style: italic; color: #666;",
        "💡 Tip: La vista mensual permite detectar patrones estacionales, mientras que la anual muestra la tendencia a largo plazo.")
    )
  })
}