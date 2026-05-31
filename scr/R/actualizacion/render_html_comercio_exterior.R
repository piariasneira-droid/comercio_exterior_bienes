#### Render comercio exterior bienes ----
library(quarto)

# Parámetros
fecha <- "2026-03-01"
fecha_ini <- "2017-01-01"

# Año y mes
anio <- format(as.Date(fecha), "%Y")
mes  <- format(as.Date(fecha), "%m")

# Directorio destino (relativo al proyecto)
output_dir <- file.path(
  "data", "output", "html", anio, mes
)

# Crear directorio si no existe
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Nombre del archivo final
final_file <- paste0(
  "comercio_exterior_bienes_",
  anio, "_", mes, ".html"
)

# Renderizar (SIN rutas en output_file)
quarto::quarto_render(
  input = "scr/R/actualizacion/comercio_exterior_bienes.qmd",
  execute_params = list(
    fecha = fecha,
    fecha_ini = fecha_ini
  ),
  output_file = final_file
)

# Ruta donde Quarto deja el HTML por defecto
rendered_file <- file.path(
  "scr/R/actualizacion",
  final_file
)

# Mover al directorio final
file.rename(
  from = rendered_file,
  to   = file.path(output_dir, final_file)
)

message(
  "✔ Render completado: ",
  normalizePath(file.path(output_dir, final_file))
)

