#### Funciones auxiliares ----
instala_carga_librerias <- function(pkgs){
  nuevos <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
  if(length(nuevos)) install.packages(nuevos)
  lapply(pkgs, library, character.only = TRUE)
}

#### Instalación de librerías y carga funciones ----
librerias <- c(
  "shiny", "dplyr", "data.table", "DT",
  "arrow", "openxlsx", "plotly"
)

instala_carga_librerias(librerias)
rm(librerias)
source("./procfunshinysectores.R")

#### Parámetros por defecto ----
fil_region   <- "esp"
fil_ano      <- 2025L
anofin       <- 2025L
fil_per      <- 9L
fil_pais     <- 0L
fil_sectores <- "0"

#### Metadatos ----
##### Sectores y agrupaciones ----
df_sectores <- data.table::rbindlist(
  base::list(
    data.table::as.data.table(openxlsx::read.xlsx("../../data/metatratado/sectores.xlsx", sheet = "sectores")),
    data.table::as.data.table(openxlsx::read.xlsx("../../data/metatratado/sectores.xlsx", sheet = "agregaciones"))
  ),
  use.names = TRUE,
  fill = TRUE
)

##### Países y regiones ----
df_pais <- data.table::rbindlist(
  base::list(
    data.table::as.data.table(openxlsx::read.xlsx("../../data/metatratado/paises_zonas.xlsx", sheet = "paises")),
    data.table::as.data.table(openxlsx::read.xlsx("../../data/metatratado/paises_zonas.xlsx", sheet = "regiones"))
  ),
  use.names = TRUE,
  fill = TRUE
)

##### Opciones período ----
periodos_choices <- base::c(
  stats::setNames(
    1:12,
    base::c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
            "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  ),
  
  stats::setNames(
    21:24,
    base::c("Trimestre 1", "Trimestre 2", "Trimestre 3", "Trimestre 4")
  ),
  
  stats::setNames(
    31:32,
    base::c("Semestre 1", "Semestre 2")
  ),
  
  stats::setNames(
    41,
    "Año entero"
  ),
  
  stats::setNames(
    51:58,
    base::c("Enero-Febrero", "Enero-Abril", "Enero-Mayo",
            "Enero-Julio", "Enero-Agosto", "Enero-Septiembre",
            "Enero-Octubre", "Enero-Noviembre")
  )
)