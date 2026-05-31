# Environment ----
# source("./scr/R/nota_sectores/main_etl.r")
source("./scr/R/nota_sectores/procfun/funciones_flextable.r")

# Tabla CCAA ----
df_tp1 <- .prepare_flextable_ccaas(df_ccaas)[c1 != "ND",
                                             .(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)]

ft_ccaa <- .make_flextable_ccaa(
  df   = df_tp1,
  para = paramets
)

save_as_image(ft_ccaa, path = file.path(paramets$path_outt , "table_p1_t1.png"), zoom = 3)
wb <- wb_workbook()$add_worksheet("Resultados CCAA")
wb <- wb_add_flextable(wb, sheet = "Resultados CCAA", ft = ft_ccaa, dims = "A1")
wb_save(wb, file = file.path(paramets$path_outx, "table_p1_t1.xlsx"))

.limpiar_memoria()

