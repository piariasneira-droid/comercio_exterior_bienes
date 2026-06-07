# Ejecucción.r
# ============================================================
# PUNTO DE ENTRADA ÚNICO
# ============================================================
# Para cambiar el periodo de análisis, edita SOLO:
#   parametros.r  →  mes = 3L         (mes suelto)
#                    mes = 4L:6L      (Q2)
#                    mes = 1L:6L      (semestre)
#
# Este fichero NO necesita tocarse nunca.
# ============================================================

source("./scr/R/nota_sectores_bis/procfun/librerias.R")
source("./scr/R/nota_sectores_bis/procfun/parametros.r")   # parámetros + sufijos + validación
source("./scr/R/nota_sectores_bis/procfun/configaux.r")    # mes_label, fecha_hoy
source("./scr/R/nota_sectores_bis/main_etl.r")             # ETL — genera todos los data.frames
source("./scr/R/nota_sectores_bis/main_tablas.R")          # sin cambios
source("./scr/R/nota_sectores_bis/main_phtmls.R")          # plots HTML + PNG
source("./scr/R/nota_sectores_bis/main_texts.R")           # listas de texto para el informe