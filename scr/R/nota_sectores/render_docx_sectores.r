library(officer)
library(quarto)
library(xml2)

# PARÁMETROS ----
mis_params <- list(
  anho  = 2026,
  mes = 3
)

# RUTAS ----
ruta_plantilla_original <- "./plantillas/plantilla_nota.docx"
ruta_plantilla_quarto   <- "./plantillas/plantilla_nota_sectores.docx"
ruta_quarto_qmd         <- "./scr/R/nota_sectores/nota_sectores.qmd"

# ENTORNO ----
source("./scr/R/nota_sectores/procfun/parametros.r")

paramets$anho  <- as.integer(mis_params$anho)
paramets$mes <- as.integer(mis_params$mes)

source("./scr/R/nota_sectores/procfun/funciones_flextable.r")
source("./scr/R/nota_sectores/main_texts.r")

# ENCABEZADOS DE PLANTILLA ----
doc <- read_docx(ruta_plantilla_original)

doc <- headers_replace_all_text(doc, old_value = "[[MESNOTA]]", new_value = mes_label, fixed = TRUE)
doc <- headers_replace_all_text(doc, old_value = "[[fecha]]",   new_value = fecha_hoy,  fixed = TRUE)

print(doc, target = ruta_plantilla_quarto)

# NOMBRE ARCHIVO FINAL ----
mes_limpio <- tolower(mes_label)
mes_limpio <- gsub("[[:space:]]", "", mes_limpio)
mes_limpio <- iconv(mes_limpio, to = "ASCII//TRANSLIT")
mes_limpio <- gsub("[^a-z0-9]", "", mes_limpio)

nombre_salida_final <- paste0("nota_sectores_", mes_limpio, ".docx")
ruta_final_completa <- file.path(paramets$path_out, nombre_salida_final)

# RENDER QUARTO ----
nombre_temporal      <- "temp_render_output.docx"
ruta_temporal_creada <- file.path(dirname(ruta_quarto_qmd), nombre_temporal)

quarto_render(
  input          = ruta_quarto_qmd,
  output_file    = nombre_temporal,
  execute_params = mis_params
)

if (!file.exists(ruta_temporal_creada)) {
  stop("Error: Quarto no genero el archivo temporal.")
}

# IMAGEN FLOTANTE ESQUINA SUPERIOR DERECHA ----
ruta_plot1 <- file.path(paramets$path_outp, "plot1_mad_evo_mes.png")

if (file.exists(ruta_plot1)) {
  
  emu_per_cm <- 360000L
  img_w_emu  <- as.integer(paramets$w1 * emu_per_cm)
  img_h_emu  <- as.integer(paramets$h1 * emu_per_cm)
  dist_emu   <- as.integer(0.3 * emu_per_cm)
  img_w_in   <- paramets$w1 / 2.54
  img_h_in   <- paramets$h1 / 2.54
  
  # Usa doc de referencia vacio para registrar la imagen y obtener target + rId ----
  doc_ref  <- read_docx()
  doc_ref  <- body_add_img(doc_ref, src = ruta_plot1, width = img_w_in, height = img_h_in)
  ruta_ref <- tempfile(fileext = ".docx")
  print(doc_ref, target = ruta_ref)
  
  tmp_ref  <- tempfile()
  dir.create(tmp_ref)
  unzip(ruta_ref, exdir = tmp_ref)
  
  ns_rels    <- c(r = "http://schemas.openxmlformats.org/package/2006/relationships")
  rels_ref   <- read_xml(file.path(tmp_ref, "word", "_rels", "document.xml.rels"))
  img_nodes  <- xml_find_all(rels_ref, "//r:Relationship[@Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/image']", ns_rels)
  img_target <- xml_attr(img_nodes[[1]], "Target")   # e.g. "media/image1.png"
  
  # Descomprime el docx final y copia la imagen ----
  tmp_final <- tempfile()
  dir.create(tmp_final)
  unzip(ruta_temporal_creada, exdir = tmp_final)
  
  media_dir <- file.path(tmp_final, "word", "media")
  if (!dir.exists(media_dir)) dir.create(media_dir, recursive = TRUE)
  file.copy(
    from      = file.path(tmp_ref, "word", img_target),
    to        = file.path(tmp_final, "word", img_target),
    overwrite = TRUE
  )
  
  # Añade la relacion en document.xml.rels ----
  rels_final_path <- file.path(tmp_final, "word", "_rels", "document.xml.rels")
  rels_final      <- read_xml(rels_final_path)
  nuevo_rid       <- "rIdPlot1"
  
  new_rel_xml <- sprintf(
    '<Relationship xmlns="http://schemas.openxmlformats.org/package/2006/relationships" Id="%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="%s"/>',
    nuevo_rid, img_target
  )
  new_rel_tmp <- tempfile(fileext = ".xml")
  writeLines(new_rel_xml, new_rel_tmp)
  xml_add_child(rels_final, read_xml(new_rel_tmp))
  write_xml(rels_final, rels_final_path)
  file.remove(new_rel_tmp)
  
  # Construye el XML del anchor en una sola linea con sprintf (escalar garantizado) ----
  anchor_xml <- sprintf('<w:p xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><w:r><w:drawing><wp:anchor distT="%d" distB="%d" distL="%d" distR="%d" simplePos="0" relativeHeight="251658240" behindDoc="0" locked="0" layoutInCell="1" allowOverlap="0"><wp:simplePos x="0" y="0"/><wp:positionH relativeFrom="margin"><wp:align>right</wp:align></wp:positionH><wp:positionV relativeFrom="margin"><wp:posOffset>0</wp:posOffset></wp:positionV><wp:extent cx="%d" cy="%d"/><wp:effectExtent l="0" t="0" r="0" b="0"/><wp:wrapSquare wrapText="bothSides"/><wp:docPr id="100" name="plot1_mad_evo_mes"/><wp:cNvGraphicFramePr/><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture"><pic:pic><pic:nvPicPr><pic:cNvPr id="0" name="plot1_mad_evo_mes"/><pic:cNvPicPr/></pic:nvPicPr><pic:blipFill><a:blip r:embed="%s"/><a:stretch><a:fillRect/></a:stretch></pic:blipFill><pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="%d" cy="%d"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr></pic:pic></a:graphicData></a:graphic></wp:anchor></w:drawing></w:r></w:p>',
                        dist_emu, dist_emu, dist_emu, dist_emu,
                        img_w_emu, img_h_emu,
                        nuevo_rid,
                        img_w_emu, img_h_emu
  )
  
  # Escribe a tempfile y lee desde ruta (evita el bug de read_xml con strings) ----
  anchor_tmp <- tempfile(fileext = ".xml")
  writeLines(anchor_xml, con = anchor_tmp, useBytes = FALSE)
  
  doc_final_xml_path <- file.path(tmp_final, "word", "document.xml")
  doc_final_xml      <- read_xml(doc_final_xml_path)
  
  ns_w        <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  body_node   <- xml_find_first(doc_final_xml, "//w:body", ns_w)
  first_child <- xml_child(body_node, 1)
  
  anchor_node <- read_xml(anchor_tmp)
  xml_add_sibling(first_child, anchor_node, .where = "before")
  file.remove(anchor_tmp)
  
  write_xml(doc_final_xml, doc_final_xml_path)
  
  # Recomprime el docx ----
  ruta_zip_out <- tempfile(fileext = ".docx")
  old_wd <- getwd()
  setwd(tmp_final)
  system(paste("zip -r", shQuote(ruta_zip_out), "."))
  setwd(old_wd)
  file.copy(ruta_zip_out, ruta_temporal_creada, overwrite = TRUE)
  file.remove(ruta_zip_out)
  
  unlink(tmp_final, recursive = TRUE)
  unlink(tmp_ref,   recursive = TRUE)
  file.remove(ruta_ref)
  
  cat("  -> Imagen flotante insertada correctamente.\n")
  
} else {
  warning("No se encontro plot1_mad_evo_mes.png en: ", ruta_plot1,
          "\n  La nota se genera sin imagen flotante.")
}

# TABLA FLOTANTE ABAJO IZQUIERDA ----
ruta_tabla1 <- file.path(paramets$path_outt, "table_p1_t1.png")

if (file.exists(ruta_tabla1)) {
  
  emu_per_cm  <- 360000L
  tab_w_cm    <- 18
  tab_img     <- png::readPNG(ruta_tabla1)
  tab_ratio   <- dim(tab_img)[1] / dim(tab_img)[2]   # alto/ancho
  tab_h_cm    <- tab_w_cm * tab_ratio
  tab_w_emu   <- as.integer(tab_w_cm * emu_per_cm)
  tab_h_emu   <- as.integer(tab_h_cm * emu_per_cm)
  tab_w_in    <- tab_w_cm / 2.54
  tab_h_in    <- tab_h_cm / 2.54
  dist_emu    <- as.integer(0.3 * emu_per_cm)
  
  # Doc de referencia para registrar la imagen ----
  doc_ref2  <- read_docx()
  doc_ref2  <- body_add_img(doc_ref2, src = ruta_tabla1, width = tab_w_in, height = tab_h_in)
  ruta_ref2 <- tempfile(fileext = ".docx")
  print(doc_ref2, target = ruta_ref2)
  
  tmp_ref2  <- tempfile()
  dir.create(tmp_ref2)
  unzip(ruta_ref2, exdir = tmp_ref2)
  
  ns_rels     <- c(r = "http://schemas.openxmlformats.org/package/2006/relationships")
  rels_ref2   <- read_xml(file.path(tmp_ref2, "word", "_rels", "document.xml.rels"))
  img_nodes2  <- xml_find_all(rels_ref2, "//r:Relationship[@Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/image']", ns_rels)
  img_target2 <- xml_attr(img_nodes2[[1]], "Target")
  
  # Descomprime el docx final y copia la imagen ----
  tmp_final2 <- tempfile()
  dir.create(tmp_final2)
  unzip(ruta_temporal_creada, exdir = tmp_final2)
  
  media_dir2 <- file.path(tmp_final2, "word", "media")
  if (!dir.exists(media_dir2)) dir.create(media_dir2, recursive = TRUE)
  
  # Evita colision de nombre con plot1
  img_target2_dest <- sub("image1", "image_tabla1", img_target2)
  file.copy(
    from      = file.path(tmp_ref2, "word", img_target2),
    to        = file.path(tmp_final2, "word", img_target2_dest),
    overwrite = TRUE
  )
  
  # Añade la relacion ----
  rels_final2_path <- file.path(tmp_final2, "word", "_rels", "document.xml.rels")
  rels_final2      <- read_xml(rels_final2_path)
  nuevo_rid2       <- "rIdTabla1"
  
  new_rel2_xml <- sprintf(
    '<Relationship xmlns="http://schemas.openxmlformats.org/package/2006/relationships" Id="%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="%s"/>',
    nuevo_rid2, img_target2_dest
  )
  new_rel2_tmp <- tempfile(fileext = ".xml")
  writeLines(new_rel2_xml, new_rel2_tmp)
  xml_add_child(rels_final2, read_xml(new_rel2_tmp))
  write_xml(rels_final2, rels_final2_path)
  file.remove(new_rel2_tmp)
  
  # XML anchor: alineada a la izquierda, posicion vertical al final de pagina ----
  anchor2_xml <- sprintf('<w:p xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><w:r><w:drawing><wp:anchor distT="%d" distB="%d" distL="%d" distR="%d" simplePos="0" relativeHeight="251658241" behindDoc="0" locked="0" layoutInCell="1" allowOverlap="0"><wp:simplePos x="0" y="0"/><wp:positionH relativeFrom="margin"><wp:align>left</wp:align></wp:positionH><wp:positionV relativeFrom="bottomMargin"><wp:align>bottom</wp:align></wp:positionV><wp:extent cx="%d" cy="%d"/><wp:effectExtent l="0" t="0" r="0" b="0"/><wp:wrapNone/><wp:docPr id="101" name="table_p1_t1"/><wp:cNvGraphicFramePr/><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture"><pic:pic><pic:nvPicPr><pic:cNvPr id="0" name="table_p1_t1"/><pic:cNvPicPr/></pic:nvPicPr><pic:blipFill><a:blip r:embed="%s"/><a:stretch><a:fillRect/></a:stretch></pic:blipFill><pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="%d" cy="%d"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr></pic:pic></a:graphicData></a:graphic></wp:anchor></w:drawing></w:r></w:p>',
                         dist_emu, dist_emu, dist_emu, dist_emu,
                         tab_w_emu, tab_h_emu,
                         nuevo_rid2,
                         tab_w_emu, tab_h_emu
  )
  
  anchor2_tmp <- tempfile(fileext = ".xml")
  writeLines(anchor2_xml, con = anchor2_tmp, useBytes = FALSE)
  
  doc_final2_xml_path <- file.path(tmp_final2, "word", "document.xml")
  doc_final2_xml      <- read_xml(doc_final2_xml_path)
  
  ns_w        <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  body_node2  <- xml_find_first(doc_final2_xml, "//w:body", ns_w)
  last_child2 <- xml_child(body_node2, xml_length(body_node2))
  
  anchor2_node <- read_xml(anchor2_tmp)
  xml_add_sibling(last_child2, anchor2_node, .where = "before")
  file.remove(anchor2_tmp)
  
  write_xml(doc_final2_xml, doc_final2_xml_path)
  
  # Recomprime ----
  ruta_zip_out2 <- tempfile(fileext = ".docx")
  old_wd2 <- getwd()
  setwd(tmp_final2)
  system(paste("zip -r", shQuote(ruta_zip_out2), "."))
  setwd(old_wd2)
  file.copy(ruta_zip_out2, ruta_temporal_creada, overwrite = TRUE)
  file.remove(ruta_zip_out2)
  
  unlink(tmp_final2, recursive = TRUE)
  unlink(tmp_ref2,   recursive = TRUE)
  file.remove(ruta_ref2)
  
  cat("  -> Tabla flotante insertada correctamente.\n")
  
} else {
  warning("No se encontro table_p1_t1.png en: ", ruta_tabla1,
          "\n  La nota se genera sin tabla flotante.")
}

# DESTINO FINAL ----
if (file.exists(ruta_final_completa)) file.remove(ruta_final_completa)

file.rename(from = ruta_temporal_creada, to = ruta_final_completa)

cat("\n================================================================\n")
cat("Proceso completado!\n")
cat("Parametros -> Anno:", mis_params$anho, "| Mes:", mis_params$meses, "\n")
cat("Documento guardado en:", ruta_final_completa, "\n")
cat("================================================================\n")