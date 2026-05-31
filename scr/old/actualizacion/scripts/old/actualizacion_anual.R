#### Establecimiento marco de trabajo de trabajo ----
setwd(dirname(dirname(this.path::this.dir())))
source("./actualizacion/scripts/procmicro.R")
instala_carga_librerias(c("arrow", "data.table", "dplyr", "purrr", "readr", "vroom"))

actualizacion_anual(
  year_nuevo_def = 2023,    
  year_actual = 2025,       
  mes_actual = "07",        
  fyea = 1995
)