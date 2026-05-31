# Definir el nombre del archivo
archivo_salida <- "resultado.txt"

# Iniciar la redirección. Split = TRUE' hace que se muestre en ambos sitios, false que se deje de mostrar en consola
sink(archivo_salida, split = TRUE)
print("Esto se guardará en el archivo Y se verá en la consola")

# --- Detener la redirección ---
sink()