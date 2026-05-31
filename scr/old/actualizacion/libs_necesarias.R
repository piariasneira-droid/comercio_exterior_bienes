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

instala_carga_librerias(c(
  "arrow", "data.table", "dplyr", "lubridate", "purrr", 
  "readr", "tidyr", "vroom", "zoo", "ggplot2", "ggrepel", 
  "scales", "DT", "htmltools", "knitr", "plotly", 
  "stringr", "tibble"
))