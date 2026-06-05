library(arrow)
library(dplyr)
library(data.table)
library(openxlsx)
library(openxlsx2)
library(officer)
library(flextable)
library(webshot2)
library(knitr)
library(tidyverse)
library(patchwork)
library(scales)
library(flexlsx)
library(quarto)
library(xml2)
library(plotly)
library(gt)
library(gtExtras)
library(magick)
library(png)
library(zip)

# # Lista de paquetes requeridos
# paquetes <- c(
#   "arrow",
#   "dplyr",
#   "data.table",
#   "openxlsx",
#   "openxlsx2",
#   "officer",
#   "flextable",
#   "webshot2",
#   "knitr",
#   "tidyverse",
#   "patchwork",
#   "scales",
#   "flexlsx",
#   "quarto",
#   "xml2",
#   "plotly",
#   "gt",
#   "gtExtras",
#   "magick",
#   "png",
#   "zip"
# )
# 
# # Identificar los que no están instalados
# faltan <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
# 
# # Instalar los faltantes junto con sus dependencias
# if (length(faltan) > 0) {
#   install.packages(
#     faltan,
#     dependencies = TRUE
#   )
# } else {
#   message("Todos los paquetes ya están instalados.")
# }
# 
# # Cargar todos los paquetes
# invisible(
#   lapply(paquetes, library, character.only = TRUE)
# )
# 
# message("Paquetes cargados correctamente.")