# install_r_packages.R
# Script para instalar automáticamente librerías R necesarias según pixi.toml

# Lista manual de librerías R que usamos (coincide con tu pixi.toml)
r_packages <- c(
  "data.table",
  "tidyverse",
  "lubridate",
  "zoo",
  "arrow",
  "plotly",
  "gganimate",
  "DT",
  "here",
  "shiny",
  "readxl",
  "haven",
  "openxlsx",
  "knitr",
  "rmarkdown",
  "IRkernel",
  "remotes",
  "writexl",
  "purrr",
  "vroom",
  "ggrepel",
  "patchwork",
  "scales",
  "htmltools"
)

# Función para instalar si no está instalada
instala_carga_librerias <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Instalando paquete: ", pkg)
      install.packages(pkg, repos = "https://cloud.r-project.org/")
    } else {
      message("Paquete ya instalado: ", pkg)
    }
  }
}

# Ejecutar la instalación
instala_carga_librerias(r_packages)

message("¡Todas las librerías R están listas!")