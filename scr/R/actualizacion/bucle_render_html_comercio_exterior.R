# ======================================
# Render comercio exterior bienes (loop mensual)
# ======================================

library(quarto)

# ---------------------------
# Configuración de fechas
# ---------------------------

# Última fecha disponible (solo hasta aquí)
fecha_fin <- as.Date("2025-10-01")

# Fecha inicial del bucle
fecha_inicio_bucle <- as.Date("2024-01-01")

# Primer año del dataset
primer_ano_dataset <- 1995

# Diferencia de años entre fecha y fecha_ini
diferencia_anios <- 8

# Secuencia mensual
fechas <- seq(from = fecha_inicio_bucle, to = fecha_fin, by = "month")

# ---------------------------
# Bucle principal
# ---------------------------
for (fecha in fechas) {
  
  # Asegurar que es Date
  fecha <- as.Date(fecha)
  
  # Año y mes
  anio <- as.integer(format(fecha, "%Y"))
  mes  <- sprintf("%02d", as.integer(format(fecha, "%m")))
  
  # Fecha inicial manteniendo diferencia de años, siempre enero, limitada al primer año del dataset
  fecha_ini <- max(
    as.Date(paste0(anio - diferencia_anios, "-01-01")),
    as.Date(paste0(primer_ano_dataset, "-01-01"))
  )
  
  message("▶ Renderizando: ", anio, "-", mes,
          " | fecha_ini = ", fecha_ini)
  
  # ---------------------------
  # Directorio de salida
  # ---------------------------
  output_dir <- file.path(
    "data", "output", "html", anio, mes
  )
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Nombre del archivo final
  final_file <- paste0(
    "comercio_exterior_bienes_",
    anio, "_", mes, ".html"
  )
  
  # ---------------------------
  # Renderizar con control de errores
  # ---------------------------
  tryCatch({
    
    quarto::quarto_render(
      input = "scr/R/actualizacion/comercio_exterior_bienes.qmd",
      execute_params = list(
        fecha     = as.character(fecha),
        fecha_ini = as.character(fecha_ini)
      ),
      output_file = final_file
    )
    
    # Ruta donde Quarto deja el HTML por defecto
    rendered_file <- file.path(
      "scr", "R", "actualizacion",
      final_file
    )
    
    # Mover al directorio final
    file.rename(
      from = rendered_file,
      to   = file.path(output_dir, final_file)
    )
    
    message("✔ Guardado en: ", file.path(output_dir, final_file))
    
  }, error = function(e) {
    message("⚠ Error al renderizar ", anio, "-", mes, ": ", e$message)
  })
  
}

message("🏁 Renderización completa")
