library(officer)
library(quarto)
library(xml2)
library(png)

# Timer start
tiempo_inicio <- proc.time()

# ===========================================================
# ▼▼▼  PARÁMETROS — SOLO TOCAR AQUÍ  ▼▼▼
# ===========================================================
mis_params <- list(
  anho         = 2026L,            # Año análisis
  mes          = 4L:6L,            # mes suelto (3L), trimestre (4L:6L) o conjunto de meses sueltos c(1L, 3L, 4L, 5L)
  ano_ini      = 2017L,            # Año inicial
  anho_idx     = 2019L,            # Año de referencia para tendencias anexos
  
  # Flags — TRUE = generar / FALSE = saltar
  flagmadmes   = TRUE,             # General mes o periodos para Madrid
  flagespmes   = TRUE,             # Id España
  flagmadytm   = TRUE,             # General acumulados para Madrid. El acumulado se hace 1L: máximo meses
  flagespytm   = TRUE,             # Id España
  flagmadanop  = TRUE,             # Totales año anterior para Madrid
  flagespanop  = TRUE,             # Id Esapaña
  flag_ccaa    = TRUE              # Flag para los dataframes con los totales generales de las CCAA 
                                   # Importante: FALSE si trimestre o se incluyen varios meses, ya que los plots de la página 1 y 2 se crashean
                                   # De hecho, si se genera un trimestre o conjunto de meses lo suyo sería poner FASE a tidi menos a la flag de meses
  )
# ===========================================================
# ▲▲▲  FIN DE ZONA EDITABLE  ▲▲▲
# ===========================================================

# RUTAS ----
ruta_plantilla_original <- "./plantillas/plantilla_nota.docx"
ruta_plantilla_quarto   <- "./plantillas/plantilla_nota_sectores.docx"
ruta_quarto_qmd         <- "./scr/R/nota_sectores_bis/nota_sectores.qmd"

# ENTORNO ----
# Cargamos parametros.r y sobreescribimos los 4 campos clave.
# mes NO se colapsa con as.integer() para preservar vectores (4L:6L).
source("./scr/R/nota_sectores_bis/procfun/parametros.r")
paramets$anho        <- as.integer(mis_params$anho)
paramets$mes         <- as.integer(mis_params$mes)  
paramets$ano_ini     <- as.integer(mis_params$ano_ini)
paramets$anho_idx    <- as.integer(mis_params$anho_idx)
paramets$flagmadmes  <- isTRUE(mis_params$flagmadmes)
paramets$flagespmes  <- isTRUE(mis_params$flagespmes)
paramets$flagmadytm  <- isTRUE(mis_params$flagmadytm)
paramets$flagespytm  <- isTRUE(mis_params$flagespytm)
paramets$flagmadanop <- isTRUE(mis_params$flagmadanop)
paramets$flagespanop <- isTRUE(mis_params$flagespanop)
paramets$flag_ccaa   <- isTRUE(mis_params$flag_ccaa)

source("./scr/R/nota_sectores_bis/procfun/funciones_flextable.r")

# configaux.r usa paramets ya actualizado y genera:
#   - rutas de salida + directorios (path_out, path_outp, etc.)
#   - mes_label, fecha_hoy
#   - sufijo_mes, sufijo_ytm, sufijo_anopas  (quarter-aware)
source("./scr/R/nota_sectores_bis/procfun/configaux.r")

# ENCABEZADOS DE PLANTILLA ----
doc <- read_docx(ruta_plantilla_original)
doc <- headers_replace_all_text(doc, old_value = "[[MESNOTA]]", new_value = mes_label, fixed = TRUE)
doc <- headers_replace_all_text(doc, old_value = "[[fecha]]",   new_value = fecha_hoy,  fixed = TRUE)
print(doc, target = ruta_plantilla_quarto)

# NOMBRE ARCHIVO FINAL ----
mes_limpio <- tolower(mes_label)
mes_limpio <- gsub("[[:space:]]", "", mes_limpio)
mes_limpio <- iconv(mes_limpio, to = "ASCII//TRANSLIT")
mes_limpio <- gsub("[^a-z0-9]",  "", mes_limpio)

nombre_salida_final <- paste0("nota_sectores_", mes_limpio, ".docx")
ruta_final_completa <- file.path(paramets$path_out, nombre_salida_final)

# RENDER QUARTO ----
# Quarto serializa execute_params a YAML, que no admite vectores R.
# Si mes es un vector (trimestre), lo convertimos a string "4:6".
# El .qmd lo reconvierte con .parse_mes() en el setup chunk.
mis_params_quarto <- list(
  anho     = mis_params$anho,
  mes      = if (length(mis_params$mes) > 1L) {
    paste0(min(mis_params$mes), ":", max(mis_params$mes))
  } else {
    mis_params$mes
  },
  ano_ini     = mis_params$ano_ini,
  anho_idx    = mis_params$anho_idx,
  flagmadmes  = isTRUE(mis_params$flagmadmes),
  flagespmes  = isTRUE(mis_params$flagespmes),
  flagmadytm  = isTRUE(mis_params$flagmadytm),
  flagespytm  = isTRUE(mis_params$flagespytm),
  flagmadanop = isTRUE(mis_params$flagmadanop),
  flagespanop = isTRUE(mis_params$flagespanop),
  flag_ccaa   = isTRUE(mis_params$flag_ccaa)
)

nombre_temporal      <- "temp_render_output.docx"
ruta_temporal_creada <- file.path(dirname(ruta_quarto_qmd), nombre_temporal)

quarto_render(
  input          = ruta_quarto_qmd,
  output_file    = nombre_temporal,
  execute_params = mis_params_quarto
)

if (!file.exists(ruta_temporal_creada)) {
  stop("Error: Quarto no genero el archivo temporal.")
}

# FUNCIÓN AUTOMATIZADA DE INYECCIÓN XML ----
# (esquema idéntico al original que funciona)
inyectar_imagen_objeto <- function(tmp_dir, marcador, ruta_img, id_rel, ancho_cm, alinear_h = "right", alinear_v = "top", wrap_mode = "square") {
  if (!file.exists(ruta_img)) {
    warning("No se localizó el archivo de imagen: ", ruta_img)
    return(FALSE)
  }
  
  # Leer proporciones reales para evitar deformación
  img_meta  <- png::readPNG(ruta_img)
  alto_cm   <- ancho_cm * (dim(img_meta)[1] / dim(img_meta)[2])
  
  emu_per_cm <- 360000L
  w_emu      <- as.integer(ancho_cm * emu_per_cm)
  h_emu      <- as.integer(alto_cm * emu_per_cm)
  dist_emu   <- as.integer(0.3 * emu_per_cm)
  
  # Copiar recurso al contenedor media descompreso
  dir_media <- file.path(tmp_dir, "word", "media")
  if (!dir.exists(dir_media)) dir.create(dir_media, recursive = TRUE)
  nombre_archivo_inj <- paste0("inj_", id_rel, "_", basename(ruta_img))
  file.copy(ruta_img, file.path(dir_media, nombre_archivo_inj), overwrite = TRUE)
  
  # Registrar Relación en .rels
  ruta_rels <- file.path(tmp_dir, "word", "_rels", "document.xml.rels")
  xml_rels  <- xml2::read_xml(ruta_rels)
  nodo_rel  <- sprintf(
    '<Relationship xmlns="http://schemas.openxmlformats.org/package/2006/relationships" Id="%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="media/%s"/>',
    id_rel, nombre_archivo_inj
  )
  nodo_xml2 <- xml2::as_xml_document(xml2::read_xml(nodo_rel))
  
  xml2::xml_add_child(xml_rels, nodo_xml2)
  xml2::write_xml(xml_rels, ruta_rels)
  
  # Definición de envoltura de texto y alineación vertical
  xml_wrap <- if (wrap_mode == "square") '<wp:wrapSquare wrapText="bothSides"/>' else '<wp:wrapNone/>'
  xml_v_pos <- if (alinear_v == "bottom") {
    '<wp:positionV relativeFrom="bottomMargin"><wp:align>bottom</wp:align></wp:positionV>'
  } else if (alinear_v == "center") {
    '<wp:positionV relativeFrom="margin"><wp:align>center</wp:align></wp:positionV>'
  } else {
    '<wp:positionV relativeFrom="margin"><wp:posOffset>0</wp:posOffset></wp:positionV>'
  }
  
  # Generar estructura XML del Anchor Drawing
  xml_anchor <- sprintf(
    '<w:p xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><w:r><w:drawing><wp:anchor distT="%d" distB="%d" distL="%d" distR="%d" simplePos="0" relativeHeight="251658240" behindDoc="0" locked="0" layoutInCell="1" allowOverlap="0"><wp:simplePos x="0" y="0"/><wp:positionH relativeFrom="margin"><wp:align>%s</wp:align></wp:positionH>%s<wp:extent cx="%d" cy="%d"/><wp:effectExtent l="0" t="0" r="0" b="0"/>%s<wp:docPr id="200" name="%s"/><wp:cNvGraphicFramePr/><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture"><pic:pic><pic:nvPicPr><pic:cNvPr id="0" name="%s"/><pic:cNvPicPr/></pic:nvPicPr><pic:blipFill><a:blip r:embed="%s"/><a:stretch><a:fillRect/></a:stretch></pic:blipFill><pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="%d" cy="%d"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr></pic:pic></a:graphicData></a:graphic></wp:anchor></w:drawing></w:r></w:p>',
    dist_emu, dist_emu, dist_emu, dist_emu, alinear_h, xml_v_pos, w_emu, h_emu, xml_wrap, id_rel, id_rel, id_rel, w_emu, h_emu
  )
  
  # Buscar e intercambiar el nodo del marcador en el documento principal
  ruta_doc_xml <- file.path(tmp_dir, "word", "document.xml")
  xml_doc      <- xml2::read_xml(ruta_doc_xml)
  ns_w         <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  todos_los_p  <- xml2::xml_find_all(xml_doc, "//w:p", ns_w)
  
  remplazado <- FALSE
  for (p in todos_los_p) {
    if (grepl(marcador, xml2::xml_text(p), fixed = TRUE)) {
      xml2::xml_add_sibling(p, xml2::read_xml(xml_anchor), .where = "before")
      xml2::xml_remove(p)
      remplazado <- TRUE
      break
    }
  }
  
  if (remplazado) {
    xml2::write_xml(xml_doc, ruta_doc_xml)
    return(TRUE)
  }
}

# PROCESAMIENTO GENERAL ----
tmp_final <- tempfile()
dir.create(tmp_final)
unzip(ruta_temporal_creada, exdir = tmp_final)

# Plan de inyección
# sufijo_mes viene de configaux.r (quarter-aware) y coincide
# exactamente con los nombres de PNG generados por main_phtmls.r
plan_mapeo <- list(
  # Pagina 1
  list(m = "MARCADOR_P1_PLOT1",   f = "plot1_mad_evo_mes.png",
       id = "rIdP1Plot",  w = 8.5,  h = "right",  v = "top",    wr = "square",
       ccaa_only = TRUE),
  # list(m = "MARCADOR_P1_CCAA",  f = "table_p1_t1.png",
  #      id = "rIdP1Ccaa",  w = 18.0, h = "center", v = "bottom", wr = "none"),
  
  # Pagina 2
  list(m = "MARCADOR_P2_PLOT2",   f = "plot2_mad_mm12_anos.png",
       id = "rIdP2Plot",  w = 18.0, h = "left",   v = "bottom", wr = "none",
       ccaa_only = TRUE),
  
  # Pagina 3
  list(m = "MARCADOR_P3_CONTRIB", f = paste0("contrib_exp_mad_sec_",  sufijo_mes, ".png"),
       id = "rIdP3Cont",  w = 10,   h = "right",  v = "top",    wr = "square",
       ccaa_only = FALSE),
  list(m = "MARCADOR_P3_TREEMAP", f = paste0("treemap_exp_mad_sec_",  sufijo_mes, ".png"),
       id = "rIdP3Tree",  w = 18.0, h = "left",   v = "bottom", wr = "none",
       ccaa_only = FALSE),
  
  # Pagina 4
  list(m = "MARCADOR_P4_CONTRIB", f = paste0("contrib_imp_mad_sec_",  sufijo_mes, ".png"),
       id = "rIdP4Cont",  w = 10,   h = "right",  v = "top",    wr = "square",
       ccaa_only = FALSE),
  list(m = "MARCADOR_P4_TREEMAP", f = paste0("treemap_imp_mad_sec_",  sufijo_mes, ".png"),
       id = "rIdP4Tree",  w = 18.0, h = "left",   v = "bottom", wr = "none",
       ccaa_only = FALSE),
  
  # Pagina 5
  list(m = "MARCADOR_P5_CONTRIB", f = paste0("contrib_exp_mad_pais_", sufijo_mes, ".png"),
       id = "rIdP5Cont",  w = 10,   h = "right",  v = "top",    wr = "square",
       ccaa_only = FALSE),
  list(m = "MARCADOR_P5_TREEMAP", f = paste0("treemap_exp_mad_pais_", sufijo_mes, ".png"),
       id = "rIdP5Tree",  w = 18.0, h = "left",   v = "bottom", wr = "none",
       ccaa_only = FALSE),
  
  # Pagina 6
  list(m = "MARCADOR_P6_CONTRIB", f = paste0("contrib_imp_mad_pais_", sufijo_mes, ".png"),
       id = "rIdP6Cont",  w = 10,   h = "right",  v = "top",    wr = "square",
       ccaa_only = FALSE),
  list(m = "MARCADOR_P6_TREEMAP", f = paste0("treemap_imp_mad_pais_", sufijo_mes, ".png"),
       id = "rIdP6Tree",  w = 18.0, h = "left",   v = "bottom", wr = "none",
       ccaa_only = FALSE),
  
  # Paginas 7 y 8 (Spark Tables)
  list(m = "MARCADOR_P7_SPARK",   f = paste0("tabla_sec_spark_mad_",  sufijo_mes, ".png"),
       id = "rIdP7Sprk",  w = 18.0, h = "center", v = "center", wr = "none",
       ccaa_only = FALSE),
  list(m = "MARCADOR_P8_SPARK",   f = paste0("tabla_pais_spark_mad_", sufijo_mes, ".png"),
       id = "rIdP8Sprk",  w = 18.0, h = "center", v = "center", wr = "none",
       ccaa_only = FALSE)
)

# Ejecución secuencial controlada del plan de inyección
for (item in plan_mapeo) {
  # Saltar entradas exclusivas de CC.AA. si el flag está desactivado
  if (isTRUE(item$ccaa_only) && !isTRUE(paramets$flag_ccaa)) {
    cat("  --> Saltado (flag_ccaa = FALSE):", item$m, "\n")
    next
  }
  ruta_completa_img <- file.path(paramets$path_outp, item$f)
  exito <- inyectar_imagen_objeto(
    tmp_dir   = tmp_final,
    marcador  = item$m,
    ruta_img  = ruta_completa_img,
    id_rel    = item$id,
    ancho_cm  = item$w,
    alinear_h = item$h,
    alinear_v = item$v,
    wrap_mode = item$wr
  )
  if (isTRUE(exito)) {
    cat("  -> Insertado con éxito:", item$m, "\n")
  } else {
    cat("  --> ADVERTENCIA: No se pudo mapear el marcador:", item$m, "\n")
  }
}

# EMPAQUETADO Y RECOMPRESIÓN ----
# (esquema idéntico al original: setwd + list.files + zip::zip)
ruta_zip_out <- tempfile(fileext = ".docx")
old_wd       <- getwd()
setwd(tmp_final)

zip::zip(zipfile = ruta_zip_out, files = list.files(all.files = TRUE, recursive = TRUE, include.dirs = TRUE))

setwd(old_wd)

if (file.exists(ruta_final_completa)) file.remove(ruta_final_completa)
file.copy(ruta_zip_out, ruta_final_completa, overwrite = TRUE)

# Limpieza
file.remove(ruta_zip_out)
file.remove(ruta_temporal_creada)
unlink(tmp_final, recursive = TRUE)

cat("\n================================================================\n")
cat("¡Proceso completado!\n")
cat("Periodo analizado :", mes_label, "\n")
cat("Documento guardado:", ruta_final_completa, "\n")
cat("================================================================\n")

# Timer end
tiempo_total <- proc.time() - tiempo_inicio
cat("Tiempo total: ", tiempo_total["elapsed"], " segundos\n")