# Procedimiento: Pipeline de Datos y Renderizado de Informes

> **Autor:** Pablo Iván Arias Neira  
> **Fecha:** 10 de junio de 2026  
> **Fuente:** Grabaciones de reunión (8 y 10 min)

---

## 1. Carga de Datos (`run_data_lake.R`)

### Parámetros a configurar (líneas 17–22)
| Parámetro | Descripción |
|---|---|
| `ultimo_mes` | Último mes disponible |
| `ultimo_año_definitivo` | Año de datos definitivos (puede excluir provisionales) |
| `año_inicio_join` | Año de inicio para el join (por defecto 1995) |

### Pasos
1. **Abrir** `surce` → microdata (ignorar `microdata_polar`, no está pulido en R).
2. **Ejecutar** función `run_data_lake` — **no hacer `source` directo**, llamar línea por línea.
3. **Líneas 17–22:** ajustar parámetros.
4. **Ejecutar** bloque `promes_base_derivados`.
5. **Ejecutar** líneas 68–76 → genera ficheros de `interin/`.

> ⏱ El proceso tarda unos segundos. Incluye totales por comunidad autónoma y el fichero de España al final.

---

## 2. Generación de Plots (`actualización/plots_informes`)

Ejecutar `plots_informes` para generar los **6 gráficos** que van en las páginas 1 y 2 del informe.

- Los plots se guardan en la carpeta `plots/`.
- **Obligatorio** antes del renderizado: las páginas 1 y 2 del informe los cargan desde aquí.
- También se genera el HTML de `render_comercio_exterior` (ignorar posibles errores menores).

---

## 3. Renderizado del Informe (`nota_sectores_bis`)

**Importante:** no renderizar desde el `.qmd` directamente — falla por rutas relativas.

- Renderizar desde el script de `renderizado` con el parámetro `1/4` y el fichero de configuración de rutas provisionales.
- El renderizado con **Quarto** funciona bien para PDF, pero **no para `.docx`**.
- Para generar `.docx` usar la librería **`officer`** (ya integrada en el flujo).

> ⏱ Tiempos de referencia: ~200 s (Conchi) / ~61 s (máquina de Pablo).

---

## 4. Actualización del Excel heredado (opcional)

Si se quiere mantener el Excel antiguo actualizado:

1. Ejecutar `actualizador_exceles` → genera el parámetro de comunidades autónomas (exportación/importación) y taric.
2. Copiar y pegar en el Excel desde `data/output/comunidades_autonomas_capitulo_pais/`.

> Más cómodo que volcar datos manualmente desde Data Comex.

---

## 5. Mejoras pendientes propuestas

### 5.1 Eliminar dependencia del CSV de `output/comunidad_pais`
- **Problema:** se carga un CSV externo (`df_cca_mesamp`) en el paso de parámetros; ese CSV hay que generarlo previamente.
- **Solución:** añadir en `main_ETL` la carga directa desde `interin/` y calcular ahí el dataframe equivalente. Solo se necesita un subconjunto de variables.

### 5.2 Convertir plots a Plotly y cachearlos
- Los plots de páginas 1 y 2 se podrían exportar como objetos Plotly y cargarlos directamente, eliminando el paso intermedio de `plots_informes`.

### 5.3 Power BI sobre los CSVs de output
- La carpeta `data/output/comunidad_autonoma_capitulo_pais/` contiene CSVs ricos (desde 1995, múltiples flujos, tasas de variación, medias móviles…) listos para cargar en Power BI con mínimo procesamiento en DAX.
- **Variables disponibles:** exportaciones, importaciones, saldo, tasa de cobertura, TVA, TVM, diferencias, contribuciones, pesos · desagregado por mes/trimestre/año.
- Las tasas de variación conviene calcularlas en DAX (no son lineales).

---

## Estructura de outputs relevantes

```
data/
└── output/
    └── comunidades_autonomas_capitulo_pais/   ← CSVs para Power BI / Excel
interin/                                        ← ficheros intermedios del pipeline
plots/                                          ← gráficos generados por plots_informes
```

---

## Notas rápidas

- El código es extenso: **paciencia** antes de tocarlo.
- Todo el pipeline puede correr con **Chi** (automatización disponible).
- La lógica de R se adaptó desde Python; `microdata_polar` en R no está finalizado.
