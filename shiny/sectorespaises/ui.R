ui <- shiny::fluidPage(
  
  # CSS personalizado para título y sidebar fijos
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      /* Título fijo en la parte superior */
      .titulo-fijo {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        background-color: #fff;
        z-index: 1000;
        padding: 15px 20px;
        border-bottom: 2px solid #3c8dbc;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      
      .titulo-fijo h2 {
        margin: 0;
        font-size: 22px;
        color: #333;
      }
      
      /* Contenedor principal con margen para compensar título fijo */
      .contenido-principal {
        margin-top: 80px;
      }
      
      /* Sidebar fijo */
      .sidebar-fixed {
        position: fixed;
        top: 90px;
        width: 16%;
        max-height: calc(100vh - 110px);
        overflow-y: auto;
        padding-right: 15px;
      }
      
      /* Ajuste del main panel para compensar el sidebar fijo */
      .main-panel-offset {
        margin-left: 18%;
      }
      
      /* Estilo para las cajas descriptivas */
      .descripcion-tab {
        background-color: #f8f9fa;
        border-left: 4px solid #3c8dbc;
        padding: 15px 20px;
        margin-bottom: 25px;
        border-radius: 0 5px 5px 0;
        font-size: 14px;
        color: #333;
        line-height: 1.6;
      }
      
      .descripcion-tab h5 {
        color: #3c8dbc;
        margin-top: 0;
        margin-bottom: 10px;
        font-weight: 600;
      }
      
      .descripcion-tab ul {
        margin-bottom: 0;
        padding-left: 20px;
      }
      
      .descripcion-tab li {
        margin-bottom: 5px;
      }
      
      /* Scrollbar personalizado para el sidebar */
      .sidebar-fixed::-webkit-scrollbar {
        width: 6px;
      }
      
      .sidebar-fixed::-webkit-scrollbar-track {
        background: #f1f1f1;
      }
      
      .sidebar-fixed::-webkit-scrollbar-thumb {
        background: #888;
        border-radius: 3px;
      }
      
      .sidebar-fixed::-webkit-scrollbar-thumb:hover {
        background: #555;
      }
    "))
  ),
  
  # Título fijo
  shiny::div(
    class = "titulo-fijo",
    shiny::h2("Análisis comercio exterior por sectores, países y regiones")
  ),
  
  # Contenedor principal
  shiny::div(
    class = "contenido-principal",
    
    shiny::fluidRow(
      
      #### Panel lateral con filtros (FIJO) ----
      shiny::column(
        width = 2,
        shiny::div(
          class = "sidebar-fixed",
          
          shiny::h3("Filtros", style = "margin-bottom: 20px;"),
          
          # Año
          shiny::sliderInput(
            inputId = "anio",
            label = "Año:",
            min = 1995L,
            max = anofin,
            value = fil_ano,
            step = 1,
            sep = "",
            width = "100%"
          ),
          
          # País
          shiny::selectizeInput(
            inputId = "pais",
            label = "País o región:",
            choices = stats::setNames(df_pais$cod, df_pais$paisconcod),
            selected = fil_pais,
            multiple = FALSE,
            options = list(
              placeholder = "Escribe país o código...",
              create = TRUE,
              persist = FALSE,
              openOnFocus = TRUE,
              selectOnTab = TRUE
            ),
            width = "100%"
          ),
          
          # Período
          shiny::selectInput(
            inputId = "periodo",
            label = "Período:",
            choices = periodos_choices,
            selected = fil_per,
            width = "100%"
          ),
          
          # Región (RadioButtons)
          shiny::radioButtons(
            inputId = "region",
            label = "Región:",
            choices = c("C. de Madrid" = "mad", "España" = "esp"),
            selected = fil_region,
            width = "100%"
          ),
          
          # Sector
          shiny::selectizeInput(
            inputId = "sector",
            label = "Sector económico o agrupación:",
            choices = stats::setNames(df_sectores$cod_sec, df_sectores$codconnombre),
            selected = fil_sectores,
            multiple = FALSE,
            options = list(
              placeholder = "Escribe sector o código...",
              create = TRUE,
              persist = FALSE,
              openOnFocus = TRUE,
              selectOnTab = TRUE
            ),
            width = "100%"
          ),
          
          # Texto explicativo
          shiny::div(
            style = "margin-top:15px; font-size:13px; color:#555; text-align: justify",
            shiny::p(
              "Selecciona el año y el período que deseas analizar ",
              "(mes, trimestre, semestre, año completo o acumulado hasta un mes determinado), ",
              "así como la región, el sector económico y el país, región o continente de interés. ",
              "En el caso de los selectores de sector, país y período puedes borrar la opción por defecto ",
              "y utilizar el buscador para buscar por nombre o código."
            )
          )
        )
      ),
      
      #### Panel principal ----
      shiny::column(
        width = 10,
        class = "main-panel-offset",
        
        shiny::tabsetPanel(
          id = "tabs_principal",
          type = "tabs",
          
          ##### Tab: Sectores ----
          shiny::tabPanel(
            title = "Sectores (estructura)",
            icon = shiny::icon("industry"),
            
            shiny::br(),
            
            # Texto descriptivo completo renderizado desde server
            shiny::uiOutput("descripcion_sectores"),
            
            # Treemaps sectores
            shiny::h3("Treemaps"),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::h4("Exportaciones"),
                plotly::plotlyOutput("treemap_sectores_exp", height = "400px")
              ),
              shiny::column(
                width = 6,
                shiny::h4("Importaciones"),
                plotly::plotlyOutput("treemap_sectores_imp", height = "400px")
              )
            ),
            
            shiny::hr(),
            
            # Tabla sectores
            shiny::h3("Tabla de datos"),
            DT::DTOutput("tabla_sectores"),
            
            shiny::hr(),
            
            # Volumen subsectores
            shiny::h3("Análisis por subsectores"),
            shiny::h4("Volumen"),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::h5("Exportaciones"),
                plotly::plotlyOutput("vol_subsectores_exp", height = "500px")
              ),
              shiny::column(
                width = 6,
                shiny::h5("Importaciones"),
                plotly::plotlyOutput("vol_subsectores_imp", height = "500px")
              )
            ),
            
            shiny::hr(),
            
            # Contribuciones subsectores
            shiny::h4("Contribución a la tasa de variación"),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::h5("Exportaciones"),
                plotly::plotlyOutput("con_subsectores_exp", height = "500px")
              ),
              shiny::column(
                width = 6,
                shiny::h5("Importaciones"),
                plotly::plotlyOutput("con_subsectores_imp", height = "500px")
              )
            )
          ),
          
          ##### Tab: Países ----
          shiny::tabPanel(
            title = "Países (estructura)",
            icon = shiny::icon("globe"),
            
            shiny::br(),
            
            # Texto descriptivo completo renderizado desde server
            shiny::uiOutput("descripcion_paises"),
            
            # Treemaps países
            shiny::h3("Treemaps"),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::h4("Exportaciones"),
                plotly::plotlyOutput("treemap_paises_exp", height = "400px")
              ),
              shiny::column(
                width = 6,
                shiny::h4("Importaciones"),
                plotly::plotlyOutput("treemap_paises_imp", height = "400px")
              )
            ),
            
            shiny::hr(),
            
            # Tabla países
            shiny::h3("Tabla de Países"),
            DT::DTOutput("tabla_paises"),
            
            shiny::hr(),
            
            # Volumen países
            shiny::h3("Análisis por países"),
            shiny::h4("Volumen"),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::h5("Exportaciones"),
                plotly::plotlyOutput("vol_paises_exp", height = "500px")
              ),
              shiny::column(
                width = 6,
                shiny::h5("Importaciones"),
                plotly::plotlyOutput("vol_paises_imp", height = "500px")
              )
            ),
            
            shiny::hr(),
            
            # Contribuciones países
            shiny::h4("Contribución a la tasa de variación"),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::h5("Exportaciones"),
                plotly::plotlyOutput("con_paises_exp", height = "500px")
              ),
              shiny::column(
                width = 6,
                shiny::h5("Importaciones"),
                plotly::plotlyOutput("con_paises_imp", height = "500px")
              )
            )
          ),
          
          ##### Tab: Evolución temporal ----
          shiny::tabPanel(
            title = "Evolución Temporal",
            icon = shiny::icon("chart-line"),
            
            shiny::br(),
            
            # Texto descriptivo completo renderizado desde server
            shiny::uiOutput("descripcion_evolucion"),
            
            # Gráficas evolución sector/país seleccionado
            shiny::h4("Evolución combinación sector país seleccionado"),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::h5("Evolución Anual"),
                plotly::plotlyOutput("plot_temporal_anual", height = "400px")
              ),
              shiny::column(
                width = 6,
                shiny::h5("Evolución Mensual"),
                plotly::plotlyOutput("plot_temporal_mensual", height = "400px")
              )
            ),
            
            shiny::hr(),
            
            # Gráficas evolución sectores
            shiny::h4("Evolución temporal por Sectores"),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::h5("Gráfico Animado: Evolucución volumen exportado"),
                plotly::plotlyOutput("plot_sec_animado", height = "400px")
              ),
              shiny::column(
                width = 6,
                shiny::h5("Gráfico Facetado: Evolución peso en el total importado por sector"),
                plotly::plotlyOutput("plot_sec_facetado", height = "400px")
              )
            ),
          )
        )
      )
    )
  )
)