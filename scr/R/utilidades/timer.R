# Captura el momento de inicio
tiempo_inicio <- proc.time()

# Captura el momento final y calcula la diferencia
tiempo_total <- proc.time() - tiempo_inicio

# Muestra el resultado por pantalla
cat("\n--- Ejecución finalizada ---\n")
cat("Tiempo total: ", tiempo_total["elapsed"], " segundos\n")