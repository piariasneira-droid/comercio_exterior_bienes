#### Formatear númerico
formatear_num <- function(x) {
  format(round(x, 1), big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

#### Listas paramétros ----
crear_listas_parametros <- function(region = "mad",
                                    ano = 2025L,
                                    per = 8L,
                                    fpais = 0L,
                                    fsect = "0",
                                    mapeo_pais,
                                    mapeo_sectores) {
  
  # Constantes 
  factoreuros <- 1e6L
  uneuros <- "mill. €"
  texeuros <- "millones de euros"
  
  # Meses del periodo
  meses <- obtener_meses_periodo(per)
  
  # Región legible
  nombre_region <- switch(region,
                          "mad" = "C. de Madrid",
                          "esp" = "España",
                          stop("Región no válida: use 'mad' o 'esp'"))
  
  # Texto del periodo
  texto_periodo <- switch(as.character(per),
                          "1" = "ENERO DE",
                          "2" = "FEBRERO DE",
                          "3" = "MARZO DE",
                          "4" = "ABRIL DE",
                          "5" = "MAYO DE",
                          "6" = "JUNIO DE",
                          "7" = "JULIO DE",
                          "8" = "AGOSTO DE",
                          "9" = "SEPTIEMBRE DE",
                          "10" = "OCTUBRE DE",
                          "11" = "NOVIEMBRE DE",
                          "12" = "DICIEMBRE DE",
                          "21" = "PRIMER TRIMESTRE DE",
                          "22" = "SEGUNDO TRIMESTRE DE",
                          "23" = "TERCER TRIMESTRE DE",
                          "24" = "CUARTO TRIMESTRE DE",
                          "31" = "PRIMER SEMESTRE DE",
                          "32" = "SEGUNDO SEMESTRE DE",
                          "41" = "",
                          "51" = "ENERO-FEBRERO DE",
                          "52" = "ENERO-ABRIL DE",
                          "53" = "ENERO-MAYO DE",
                          "54" = "ENERO-JULIO DE",
                          "55" = "ENERO-AGOSTO DE",
                          "56" = "ENERO-SEPTIEMBRE DE",
                          "57" = "ENERO-OCTUBRE DE",
                          "58" = "ENERO-NOVIEMBRE DE",
                          stop("Periodo no válido")
  )
  
  texto_periodo_bis <- switch(as.character(per),
                              "1" = "en el mes de enero",
                              "2" = "en el mes de febrero",
                              "3" = "en el mes de marzo",
                              "4" = "en el mes de abril",
                              "5" = "en el mes de mayo",
                              "6" = "en el mes de junio",
                              "7" = "en el mes de julio",
                              "8" = "en el mes de agosto",
                              "9" = "en el mes de septiembre",
                              "10" = "en el mes de octubre",
                              "11" = "en el mes de noviembre",
                              "12" = "en el mes de diciembre",
                              "21" = "en el primer trimestre",
                              "22" = "en el segundo trimestre",
                              "23" = "en el tercer trimestre",
                              "24" = "en el cuarto trimestre",
                              "31" = "en el primer semestre",
                              "32" = "en el segundo semestre",
                              "41" = "en el año completo",
                              "51" = "en el acumulado enero-febrero",
                              "52" = "en el acumulado enero-abril",
                              "53" = "en el acumulado enero-mayo",
                              "54" = "en el acumulado enero-julio",
                              "55" = "en el acumulado enero-agosto",
                              "56" = "en el acumulado enero-septiembre",
                              "57" = "en el acumulado enero-octubre",
                              "58" = "en el acumulado enero-noviembre",
                              stop("Periodo no válido")
  )
  
  # Archivo
  archivo <- switch(region,
                    "mad" = "../../data/interim/madrid/madrid_euros_sectores.parquet",
                    "esp" = "../../data/interim/espana/espana_euros_sectores.parquet"
  )
  
  # Variables paises
  fila_pais <- mapeo_pais[cod == as.numeric(fpais)]
  nombre_pais <- fila_pais$pais
  lista_str <- fila_pais$listapaises
  pais_vector <- as.integer(unlist(strsplit(gsub("[c\\(\\) ]", "", lista_str), ",")))
  
  # Variables sectores
  fila_sector <- mapeo_sectores[cod_sec == fsect]
  nombre_sect <- fila_sector$nombre
  lista_str <- fila_sector$listasec
  
  if (grepl("^c\\(", lista_str)) {
    # Extraer elementos entre comillas
    sec_vector <- unlist(regmatches(lista_str, gregexpr('"([^"]+)"', lista_str)))
    sec_vector <- gsub('"', "", sec_vector)  # quitar comillas
  } else {
    # Caso simple: un único código, se devuelve como vector de longitud 1
    sec_vector <- lista_str
  }
  
  # Lista final
  list(
    ano            = ano,
    per            = per,
    region         = region,
    nombre_region  = nombre_region,
    meses          = meses,
    var            = "euros",
    varfactor      = factoreuros,
    varud          = uneuros,
    texto_periodo  = texto_periodo,
    texto_per      = texto_periodo_bis,
    texud          = texeuros,
    archivo        = archivo,
    pais           = pais_vector,
    nombre_pais    = nombre_pais,
    sector         = sec_vector,
    nombre_sector  = nombre_sect
  )
}

#### Carga metadatos ----
cargar_pais <- function(path) {
  # Leer el archivo Excel
  df_pais <- openxlsx::read.xlsx(path)
  
  # Convertir a data.table
  data.table::setDT(df_pais)
  df_pais[, cod_pais := as.integer(cod_pais)]
  
  # Añadir la fila "Total"
  df_pais <- data.table::rbindlist(
    list(
      df_pais,
      data.table::data.table(
        pais = "C Total mundial",
        region = "Total",
        continente = "Total",
        cod_pais = 0L,
        nombre = "Total mundial"
      )
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  return(df_pais)
}

cargar_sectores <- function(path){
  # Leer el fichero con vroom
  df_micro <- vroom::vroom(
    file       = path,
    delim      = "\t",
    col_names  = TRUE,
    locale     = vroom::locale(encoding = "UTF-16LE"),
    col_types  = vroom::cols(.default = vroom::col_character())
  )
  
  # Convertir a data.table
  dt_micro <- data.table::as.data.table(df_micro)

  return(dt_micro)
}



obtener_meses_periodo <- function(cod_periodo) {
  periodos_choices <- c(
    stats::setNames(1:12,
                    c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                      "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")),
    
    stats::setNames(21:24,
                    c("Trimestre 1","Trimestre 2","Trimestre 3","Trimestre 4")),
    
    stats::setNames(31:32,
                    c("Semestre 1","Semestre 2")),
    
    stats::setNames(41, "Año entero"),
    
    stats::setNames(51:58,
                    c("Enero-Febrero","Enero-Abril","Enero-Mayo",
                      "Enero-Julio","Enero-Agosto","Enero-Septiembre",
                      "Enero-Octubre","Enero-Noviembre"))
  )
  
  # Mapeo de periodos a meses
  periodos_map <- list(
    "1"  = 1L, "2"  = 2L, "3"  = 3L, "4"  = 4L, "5"  = 5L, "6"  = 6L,
    "7"  = 7L, "8"  = 8L, "9"  = 9L, "10" = 10L, "11" = 11L, "12" = 12L,
    "21" = 1L:3L, "22" = 4L:6L, "23" = 7L:9L, "24" = 10L:12L,
    "31" = 1L:6L, "32" = 7L:12L,
    "41" = 1L:12L,
    "51" = 1L:2L, "52" = 1L:4L, "53" = 1L:5L, "54" = 1L:7L, "55" = 1L:8L,   
    "56" = 1L:9L, "57" = 1L:10L, "58" = 1L:11L   
  )
  
  meses <- periodos_map[[as.character(cod_periodo)]]
  if (is.null(meses)) {
    return(NA_integer_) 
  }
  return(meses)
}

#### Data processing ----
calculo_totales <- function(df_query, param) {
  df <- df_query |>
    dplyr::filter(
      año == param$ano | año == param$ano - 1L,
      mes %in% param$meses,
      pais == 0L,
      cod_sector_economico == 0L
    ) |>
    dplyr::group_by(flujo, año) |>
    dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect()
  
  df <- data.table:: as.data.table(df)
  
  # Totales
  exp_total      <- df[flujo == 1 & año == param$ano, euros]
  exp_prev_total <- df[flujo == 1 & año == param$ano - 1, euros]
  imp_total      <- df[flujo == 0 & año == param$ano, euros]
  imp_prev_total <- df[flujo == 0 & año == param$ano - 1, euros]
  
  # Retorno
  return(list(
    exp      = exp_total,
    exp_prev = exp_prev_total,
    imp      = imp_total,
    imp_prev = imp_prev_total
  ))
}


tabla_sectores_datacomex <- function(datas, tot, df_sec, parametros) {
  # Filtrar df_sec
  lista_nivel3 <- c("431", "432", "433", "434", "435", "436", "437", "438", 
                    "441", "4421", "4422", "4423", "443", "444",
                    "511", "512", "513", "514", "515", "516",
                    "521", "522", "523", 
                    "531", "532", "533", "534", 
                    "541", "542", "543", "544", 
                    "814")
  
  df_sec <- df_sec[
    (nivel_sec %in% c(1L, 2L) | cod_sec %in% lista_nivel3)
  ]
  
  # Microdatos
  df_micro <- datas |>
    dplyr::filter(
      año == parametros$ano | año == parametros$ano - 1L,
      mes %in% parametros$meses,
      pais %in% parametros$pais 
    ) |>
    dplyr::group_by(flujo, año, cod_sector_economico) |>
    dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
    dplyr::select(flujo, año, cod_sector_economico, euros) |>
    dplyr::collect()
  
  # INNER JOIN microdatos con sectores
  df_micro_joined <- df_sec[df_micro, 
                            on = .(cod_sec = cod_sector_economico),
                            nomatch = 0]
  
  # Agrupación por sector
  df_final <- df_micro_joined[, .(
    exp      = sum(euros[flujo == 1 & año == parametros$ano], na.rm = TRUE),
    exp_prev = sum(euros[flujo == 1 & año == parametros$ano - 1L], na.rm = TRUE),
    imp      = sum(euros[flujo == 0 & año == parametros$ano], na.rm = TRUE),
    imp_prev = sum(euros[flujo == 0 & año == parametros$ano - 1L], na.rm = TRUE)
  ), by = .(cod_sec, orden, niv, nombre, nivel_sec, sec)]
  
  # Subtotales 
  exp_subtotal      <- df_final[nivel_sec == 1, sum(exp, na.rm = TRUE)]
  exp_prev_subtotal <- df_final[nivel_sec == 1, sum(exp_prev, na.rm = TRUE)]
  imp_subtotal      <- df_final[nivel_sec == 1, sum(imp, na.rm = TRUE)]
  imp_prev_subtotal <- df_final[nivel_sec == 1, sum(imp_prev, na.rm = TRUE)]
  
  # Excluir códigos no deseados
  df_final <- df_final[!is.na(orden)]
  
  df_final <- df_final[, .(
    exp      = sum(exp, na.rm = TRUE),
    exp_prev = sum(exp_prev, na.rm = TRUE),
    imp      = sum(imp, na.rm = TRUE),
    imp_prev = sum(imp_prev, na.rm = TRUE)
  ), by = .(cod_sec, orden, nombre, niv)]
  
  # CREAR FILA ORDEN 18 (suma de órdenes 19-23)
  fila_18_valores <- df_final[orden %in% c(19, 20, 21, 22, 23), .(
    exp      = sum(exp, na.rm = TRUE),
    exp_prev = sum(exp_prev, na.rm = TRUE),
    imp      = sum(imp, na.rm = TRUE),
    imp_prev = sum(imp_prev, na.rm = TRUE)
  )]
  
  fila_18 <- data.table::data.table(
    orden     = 18,
    nombre    = "Semifacturas no químicas",
    niv       = 1L,
    exp       = fila_18_valores$exp,
    exp_prev  = fila_18_valores$exp_prev,
    imp       = fila_18_valores$imp,
    imp_prev  = fila_18_valores$imp_prev,
    cod_sec   = "Z3"
  )
  
  df_final <- rbind(df_final, fila_18, use.names = TRUE)
  df_final <- data.table::setorder(df_final, orden)
  
  # Añadir columnas de diferencias y porcentajes
  df_final[, exp_dif := exp - exp_prev]
  df_final[, imp_dif := imp - imp_prev]
  df_final[, exp_per := exp / tot$exp * 100]
  df_final[, imp_per := imp / tot$imp * 100]
  df_final[, exp_per_reg := exp / exp_subtotal * 100]
  df_final[, imp_per_reg := imp / imp_subtotal * 100]
  df_final[, tva_exp := ifelse(exp_prev != 0, exp_dif / exp_prev * 100, 0)]
  df_final[, tva_imp := ifelse(imp_prev != 0, imp_dif / imp_prev * 100, 0)]
  df_final[, con_exp := exp_dif / tot$exp_prev * 100]
  df_final[, con_imp := imp_dif / tot$imp_prev * 100]
  df_final[, con_exp_reg := exp_dif / exp_prev_subtotal * 100]
  df_final[, con_imp_reg := imp_dif / imp_prev_subtotal * 100]
  
  # CALCULAR SALDOS
  df_final[, saldo := exp - imp]
  df_final[, saldo_prev := exp_prev - imp_prev]
  
  # Añadir fila subtotal
  fila_subtotal <- data.table::data.table(
    orden           = 65L,
    nombre          = paste0("Subtotal mercado: ", parametros$nombre_pais, ", Territorial: ", parametros$nombre_region),
    niv             = 0L,
    exp             = exp_subtotal,
    exp_prev        = exp_prev_subtotal,
    imp             = imp_subtotal,
    imp_prev        = imp_prev_subtotal,
    exp_dif         = exp_subtotal - exp_prev_subtotal,
    imp_dif         = imp_subtotal - imp_prev_subtotal,
    exp_per         = exp_subtotal / tot$exp * 100,
    imp_per         = imp_subtotal / tot$imp * 100,
    exp_per_reg     = 100,
    imp_per_reg     = 100,
    tva_exp         = 100 * (exp_subtotal - exp_prev_subtotal) / exp_prev_subtotal,
    tva_imp         = 100 * (imp_subtotal - imp_prev_subtotal) / imp_prev_subtotal,
    con_exp         = 100 * (exp_subtotal - exp_prev_subtotal) / tot$exp_prev,
    con_imp         = 100 * (imp_subtotal - imp_prev_subtotal) / tot$imp_prev,
    con_exp_reg     = 100 * (exp_subtotal - exp_prev_subtotal) / exp_prev_subtotal,
    con_imp_reg     = 100 * (imp_subtotal - imp_prev_subtotal) / imp_prev_subtotal,
    saldo           = exp_subtotal - imp_subtotal,
    saldo_prev      = exp_prev_subtotal - imp_prev_subtotal,
    cod_sec         = "Z0"
  )
  
  # Añadir fila total
  fila_total <- data.table::data.table(
    orden       = 66L,
    nombre      = paste0("Total territorial: ", parametros$nombre_region),
    niv         = 0L,
    exp         = tot$exp,
    exp_prev    = tot$exp_prev,
    imp         = tot$imp,
    imp_prev    = tot$imp_prev,
    exp_dif     = tot$exp - tot$exp_prev,
    imp_dif     = tot$imp - tot$imp_prev,
    exp_per     = 100,
    imp_per     = 100,
    exp_per_reg = "",
    imp_per_reg = "",
    tva_exp     = 100 * (tot$exp - tot$exp_prev) / tot$exp_prev,
    tva_imp     = 100 * (tot$imp - tot$imp_prev) / tot$imp_prev,
    con_exp     = 100 * (tot$exp - tot$exp_prev) / tot$exp_prev,
    con_imp     = 100 * (tot$imp - tot$imp_prev) / tot$imp_prev,
    con_exp_reg = 100 * (tot$exp - tot$exp_prev) / tot$exp_prev,
    con_imp_reg = 100 * (tot$imp - tot$imp_prev) / tot$imp_prev,
    saldo       = tot$exp - tot$imp,
    saldo_prev  = tot$exp_prev - tot$imp_prev,
    cod_sec     = "0"
  )
  
  df_final <- rbind(df_final, fila_subtotal, fila_total, use.names = TRUE)
  df_final <- data.table::setorder(df_final, orden)
  
  # Dividir entre varfactor
  cols_dividir <- c(
    "exp", "exp_prev", "imp", "imp_prev",
    "exp_dif", "imp_dif", "saldo", "saldo_prev"
  )
  
  df_final[, (cols_dividir) := lapply(.SD, function(x) x / parametros$varfactor), 
           .SDcols = cols_dividir]
  
  return(df_final[])
}

tabla_paises_datacomex <- function(datas, tot, df_paises, parametros) {
  # Microdatos
  df_micro <- datas |>
    dplyr::filter(
      año == parametros$ano | año == parametros$ano - 1L,
      mes %in% parametros$meses,
      cod_sector_economico %in% parametros$sector 
    ) |>
    dplyr::group_by(flujo, año, pais) |>
    dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
    dplyr::select(flujo, año, pais, euros) |>
    dplyr::collect()
  
  
  df_micro_joined <- df_paises[df_micro, on = .(cod = pais), nomatch = 0]

  df_final <- df_micro_joined[, .(
    exp      = sum(euros[flujo == 1 & año == parametros$ano], na.rm = TRUE),
    exp_prev = sum(euros[flujo == 1 & año == parametros$ano - 1L], na.rm = TRUE),
    imp      = sum(euros[flujo == 0 & año == parametros$ano], na.rm = TRUE),
    imp_prev = sum(euros[flujo == 0 & año == parametros$ano - 1L], na.rm = TRUE)
  ), by = .(cod, pais, reg)]
  
  # Subtotales 
  exp_subtotal      <- df_final[cod== 0L, sum(exp, na.rm = TRUE)]
  exp_prev_subtotal <- df_final[cod== 0L, sum(exp_prev, na.rm = TRUE)]
  imp_subtotal      <- df_final[cod== 0L, sum(imp, na.rm = TRUE)]
  imp_prev_subtotal <- df_final[cod== 0L, sum(imp_prev, na.rm = TRUE)]
  
  # Regiones
  df_regiones <- df_final[!is.na(reg), .(
    exp = sum(exp, na.rm = TRUE),
    exp_prev = sum(exp_prev, na.rm = TRUE),
    imp = sum(imp, na.rm = TRUE),
    imp_prev = sum(imp_prev, na.rm = TRUE)
  ), by = reg]
  
  # Renombrar reg → pais
  df_regiones <- data.table::setnames(
    df_regiones,
    old = "reg",
    new = "pais"
  )
  
  # Crear tabla de códigos
  df_codigos <- data.table::data.table(
    pais = c("ZONA EURO", "RESTO DE EUROPA", "RESTO UE", "ASIA (excl. Oriente Medio)",
      "ÁFRICA", "AMÉRICA DEL NORTE", "AMÉRICA LATINA", "RESTO DE AMÉRICA",
      "ORIENTE MEDIO", "OCEANÍA", "OTROS"),
    cod = c(1001, 1003, 1002, 1007, 1009, 1004, 1005, 1006, 1008, 1010, 1011)
  )

  df_regiones <- df_codigos[df_regiones, on = "pais"]
  df_final <- data.table::rbindlist(
    list(df_final, df_regiones),
    use.names = TRUE,
    fill = TRUE
  )
  
  # Añadir columnas de diferencias y porcentajes
  df_final[, exp_dif := exp - exp_prev]
  df_final[, imp_dif := imp - imp_prev]
  df_final[, exp_per := exp / tot$exp * 100]
  df_final[, imp_per := imp / tot$imp * 100]
  df_final[, exp_per_reg := exp / exp_subtotal * 100]
  df_final[, imp_per_reg := imp / imp_subtotal * 100]
  df_final[, tva_exp := ifelse(exp_prev != 0, exp_dif / exp_prev * 100, 0)]
  df_final[, tva_imp := ifelse(imp_prev != 0, imp_dif / imp_prev * 100, 0)]
  df_final[, con_exp := exp_dif / tot$exp_prev * 100]
  df_final[, con_imp := imp_dif / tot$imp_prev * 100]
  df_final[, con_exp_reg := exp_dif / exp_prev_subtotal * 100]
  df_final[, con_imp_reg := imp_dif / imp_prev_subtotal * 100]
  df_final[, saldo := exp - imp]
  df_final[, saldo_prev := exp_prev - imp_prev]
  df_final[, c("pais", "reg") := NULL]

  df_result <- df_paises[, .(cod, pais, orden, niv)][df_final, on = "cod", nomatch = 0][
    !is.na(orden)
  ][
    order(orden)
  ]
  
  # Añadir fila UE 27
  df_ue27 <- df_result[orden %in% c(3,23)]
  
  # Crear fila total UE27
  fila_ue27 <- data.table::data.table(
    cod       = 1013L,
    pais      = "UE 27",
    orden     = 2L,
    niv       = 2L,
    exp       = sum(df_ue27$exp, na.rm = TRUE),
    exp_prev  = sum(df_ue27$exp_prev, na.rm = TRUE),
    imp       = sum(df_ue27$imp, na.rm = TRUE),
    imp_prev  = sum(df_ue27$imp_prev, na.rm = TRUE)
  )
  
  # Calcular resto de métricas igual que para el resto de filas
  fila_ue27[, exp_dif      := exp - exp_prev]
  fila_ue27[, imp_dif      := imp - imp_prev]
  fila_ue27[, exp_per      := exp / tot$exp * 100]
  fila_ue27[, imp_per      := imp / tot$imp * 100]
  fila_ue27[, exp_per_reg  := exp / exp_subtotal * 100]
  fila_ue27[, imp_per_reg  := imp / imp_subtotal * 100]
  fila_ue27[, tva_exp      := ifelse(exp_prev != 0, exp_dif / exp_prev * 100, 0)]
  fila_ue27[, tva_imp      := ifelse(imp_prev != 0, imp_dif / imp_prev * 100, 0)]
  fila_ue27[, con_exp      := exp_dif / tot$exp_prev * 100]
  fila_ue27[, con_imp      := imp_dif / tot$imp_prev * 100]
  fila_ue27[, con_exp_reg  := exp_dif / exp_prev_subtotal * 100]
  fila_ue27[, con_imp_reg  := imp_dif / imp_prev_subtotal * 100]
  fila_ue27[, saldo := exp - imp]
  fila_ue27[, saldo_prev := exp_prev - imp_prev]
  
  # Añadir fila america
  df_america <- df_result[orden %in% c(38,41, 47)]
  
  # Crear fila total america
  fila_america <- data.table::data.table(
    cod       = 1014L,
    pais      = "AMERICA",
    orden     = 37L,
    niv       = 1L,
    exp       = sum(df_america$exp, na.rm = TRUE),
    exp_prev  = sum(df_america$exp_prev, na.rm = TRUE),
    imp       = sum(df_america$imp, na.rm = TRUE),
    imp_prev  = sum(df_america$imp_prev, na.rm = TRUE)
  )
  
  # Calcular resto de métricas igual que para el resto de filas
  fila_america[, exp_dif      := exp - exp_prev]
  fila_america[, imp_dif      := imp - imp_prev]
  fila_america[, exp_per      := exp / tot$exp * 100]
  fila_america[, imp_per      := imp / tot$imp * 100]
  fila_america[, exp_per_reg  := exp / exp_subtotal * 100]
  fila_america[, imp_per_reg  := imp / imp_subtotal * 100]
  fila_america[, tva_exp      := ifelse(exp_prev != 0, exp_dif / exp_prev * 100, 0)]
  fila_america[, tva_imp      := ifelse(imp_prev != 0, imp_dif / imp_prev * 100, 0)]
  fila_america[, con_exp      := exp_dif / tot$exp_prev * 100]
  fila_america[, con_imp      := imp_dif / tot$imp_prev * 100]
  fila_america[, con_exp_reg  := exp_dif / exp_prev_subtotal * 100]
  fila_america[, con_imp_reg  := imp_dif / imp_prev_subtotal * 100]
  fila_america[, saldo := exp - imp]
  fila_america[, saldo_prev := exp_prev - imp_prev]
  
  # Añadir fila asia
  df_asia <- df_result[orden %in% c(49, 59)]
  
  # Crear fila total asia
  fila_asia <- data.table::data.table(
    cod       = 1015L,
    pais      = "ASIA",
    orden     = 48L,
    niv       = 1L,
    exp       = sum(df_asia$exp, na.rm = TRUE),
    exp_prev  = sum(df_asia$exp_prev, na.rm = TRUE),
    imp       = sum(df_asia$imp, na.rm = TRUE),
    imp_prev  = sum(df_asia$imp_prev, na.rm = TRUE)
  )
  
  # Calcular resto de métricas igual que para el resto de filas
  fila_asia[, exp_dif      := exp - exp_prev]
  fila_asia[, imp_dif      := imp - imp_prev]
  fila_asia[, exp_per      := exp / tot$exp * 100]
  fila_asia[, imp_per      := imp / tot$imp * 100]
  fila_asia[, exp_per_reg  := exp / exp_subtotal * 100]
  fila_asia[, imp_per_reg  := imp / imp_subtotal * 100]
  fila_asia[, tva_exp      := ifelse(exp_prev != 0, exp_dif / exp_prev * 100, 0)]
  fila_asia[, tva_imp      := ifelse(imp_prev != 0, imp_dif / imp_prev * 100, 0)]
  fila_asia[, con_exp      := exp_dif / tot$exp_prev * 100]
  fila_asia[, con_imp      := imp_dif / tot$imp_prev * 100]
  fila_asia[, con_exp_reg  := exp_dif / exp_prev_subtotal * 100]
  fila_asia[, con_imp_reg  := imp_dif / imp_prev_subtotal * 100]
  fila_asia[, saldo := exp - imp]
  fila_asia[, saldo_prev := exp_prev - imp_prev]
  
  # Añadir filas
  df_result <- data.table::rbindlist(list(df_result, fila_ue27, fila_america, fila_asia), use.names = TRUE, fill = TRUE)[order(orden)]
  
  # Añadir fila europa
  df_europa <- df_result[orden %in% c(2,31)]
  
  # Crear fila total europa
  fila_europa <- data.table::data.table(
    cod       = 1012L,
    pais      = "EUROPA",
    orden     = 1L,
    niv       = 1L,
    exp       = sum(df_europa$exp, na.rm = TRUE),
    exp_prev  = sum(df_europa$exp_prev, na.rm = TRUE),
    imp       = sum(df_europa$imp, na.rm = TRUE),
    imp_prev  = sum(df_europa$imp_prev, na.rm = TRUE)
  )
  
  # Calcular resto de métricas igual que para el resto de filas
  fila_europa[, exp_dif      := exp - exp_prev]
  fila_europa[, imp_dif      := imp - imp_prev]
  fila_europa[, exp_per      := exp / tot$exp * 100]
  fila_europa[, imp_per      := imp / tot$imp * 100]
  fila_europa[, exp_per_reg  := exp / exp_subtotal * 100]
  fila_europa[, imp_per_reg  := imp / imp_subtotal * 100]
  fila_europa[, tva_exp      := ifelse(exp_prev != 0, exp_dif / exp_prev * 100, 0)]
  fila_europa[, tva_imp      := ifelse(imp_prev != 0, imp_dif / imp_prev * 100, 0)]
  fila_europa[, con_exp      := exp_dif / tot$exp_prev * 100]
  fila_europa[, con_imp      := imp_dif / tot$imp_prev * 100]
  fila_europa[, con_exp_reg  := exp_dif / exp_prev_subtotal * 100]
  fila_europa[, con_imp_reg  := imp_dif / imp_prev_subtotal * 100]
  fila_europa[, saldo := exp - imp]
  fila_europa[, saldo_prev := exp_prev - imp_prev]
  
  # Añadir fila subtotal
  fila_subtotal <- data.table::data.table(
    cod             = 1100L,
    pais            = paste0("Subtotal sector: ", parametros$nombre_sector, ", Territorial: ", parametros$nombre_region),
    orden           = 71L,
    niv             = 0L,
    exp             = exp_subtotal,
    exp_prev        = exp_prev_subtotal,
    imp             = imp_subtotal,
    imp_prev        = imp_prev_subtotal,
    exp_dif         = exp_subtotal - exp_prev_subtotal,
    imp_dif         = imp_subtotal - imp_prev_subtotal,
    exp_per         = exp_subtotal / tot$exp * 100,
    imp_per         = imp_subtotal / tot$imp * 100,
    exp_per_reg     = 100,
    imp_per_reg     = 100,
    tva_exp         = 100 * (exp_subtotal - exp_prev_subtotal) / exp_prev_subtotal,
    tva_imp         = 100 * (imp_subtotal - imp_prev_subtotal) / imp_prev_subtotal,
    con_exp         = 100 * (exp_subtotal - exp_prev_subtotal) / tot$exp_prev,
    con_imp         = 100 * (imp_subtotal - imp_prev_subtotal) / tot$imp_prev,
    con_exp_reg     = 100 * (exp_subtotal - exp_prev_subtotal) / exp_prev_subtotal,
    con_imp_reg     = 100 * (imp_subtotal - imp_prev_subtotal) / imp_prev_subtotal,
    saldo           = tot$exp - tot$imp,
    saldo_prev      = tot$exp_prev - tot$imp_prev
  )
  
  # Añadir fila total
  fila_total <- data.table::data.table(
    cod         = 0L,
    pais        = paste0("Total territorial: ", parametros$nombre_region),
    orden       = 72L,
    niv         = 0L,
    exp         = tot$exp,
    exp_prev    = tot$exp_prev,
    imp         = tot$imp,
    imp_prev    = tot$imp_prev,
    exp_dif     = tot$exp - tot$exp_prev,
    imp_dif     = tot$imp - tot$imp_prev,
    exp_per     = 100,
    imp_per     = 100,
    exp_per_reg = "",
    imp_per_reg = "",
    tva_exp     = 100 * (tot$exp - tot$exp_prev) / tot$exp_prev,
    tva_imp     = 100 * (tot$imp - tot$imp_prev) / tot$imp_prev,
    con_exp     = 100 * (tot$exp - tot$exp_prev) / tot$exp_prev,
    con_imp     = 100 * (tot$imp - tot$imp_prev) / tot$imp_prev,
    con_exp_reg = 100 * (tot$exp - tot$exp_prev) / tot$exp_prev,
    con_imp_reg = 100 * (tot$imp - tot$imp_prev) / tot$imp_prev,
    saldo       = tot$exp - tot$imp,
    saldo_prev  = tot$exp_prev - tot$imp_prev
  )
  
  df_result <- data.table::rbindlist(list(df_result, fila_europa, fila_subtotal, fila_total), use.names = TRUE, fill = TRUE)[order(orden)]
  
  # Dividir entre varfactor
  cols_dividir <- c(
    "exp", "exp_prev", "imp", "imp_prev",
    "exp_dif", "imp_dif", "saldo", "saldo_prev"
  )
  
  df_result[, (cols_dividir) := lapply(.SD, function(x) x / parametros$varfactor), 
           .SDcols = cols_dividir]


  return(df_result)
}


treemap_data <- function(datas, tot, df_sec, parametros) {
  # Microdatos
  df_micro <- datas |>
    dplyr::filter(
      año == parametros$ano | año == parametros$ano - 1L,
      mes %in% parametros$meses,
      pais %in% parametros$pais 
    ) |>
    dplyr::group_by(flujo, año, cod_sector_economico) |>
    dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
    dplyr::select(flujo, año, cod_sector_economico, euros) |>
    dplyr::collect()
  
  # INNER JOIN microdatos con sectores
  df_micro_joined <- df_sec[df_micro, 
                            on = .(cod_sec = cod_sector_economico),
                            nomatch = 0]
  
  # Agrupación por sector
  df_final <- df_micro_joined[, .(
    exp      = sum(euros[flujo == 1 & año == parametros$ano], na.rm = TRUE),
    exp_prev = sum(euros[flujo == 1 & año == parametros$ano - 1L], na.rm = TRUE),
    imp      = sum(euros[flujo == 0 & año == parametros$ano], na.rm = TRUE),
    imp_prev = sum(euros[flujo == 0 & año == parametros$ano - 1L], na.rm = TRUE)
  ), by = .(cod_sec, nivel_sec, sec)]
  
  # Subtotales 
  exp_subtotal      <- df_final[nivel_sec == 1, sum(exp, na.rm = TRUE)]
  exp_prev_subtotal <- df_final[nivel_sec == 1, sum(exp_prev, na.rm = TRUE)]
  imp_subtotal      <- df_final[nivel_sec == 1, sum(imp, na.rm = TRUE)]
  imp_prev_subtotal <- df_final[nivel_sec == 1, sum(imp_prev, na.rm = TRUE)]
  
  # Añadir columnas de diferencias y porcentajes
  df_final[, exp_dif := exp - exp_prev]
  df_final[, imp_dif := imp - imp_prev]
  df_final[, exp_per := exp / tot$exp * 100]
  df_final[, imp_per := imp / tot$imp * 100]
  df_final[, exp_per_reg := exp / exp_subtotal * 100]
  df_final[, imp_per_reg := imp / imp_subtotal * 100]
  df_final[, tva_exp := ifelse(exp_prev != 0, exp_dif / exp_prev * 100, 0)]
  df_final[, tva_imp := ifelse(imp_prev != 0, imp_dif / imp_prev * 100, 0)]
  df_final[, con_exp := exp_dif / tot$exp_prev * 100]
  df_final[, con_imp := imp_dif / tot$imp_prev * 100]
  df_final[, parent := data.table::fifelse(
    as.integer(nivel_sec) == 1L, 
    "0", 
    substr(cod_sec, 1, as.integer(nivel_sec) - 1L)
  )]
  
  # Añadir fila subtotal
  fila_subtotal <- data.table::data.table(
    cod_sec       = "0",
    nivel_sec     = 0L,
    sec           = "Total pais/región",
    exp           = exp_subtotal,
    exp_prev      = exp_prev_subtotal,
    imp           = imp_subtotal,
    imp_prev      = imp_prev_subtotal,
    exp_dif       = exp_subtotal - exp_prev_subtotal,
    imp_dif       = imp_subtotal - imp_prev_subtotal,
    exp_per       = exp_subtotal / tot$exp * 100,
    imp_per       = imp_subtotal / tot$imp * 100,
    exp_per_reg   = 100,
    imp_per_reg   = 100,
    tva_exp       = 100 * (exp_subtotal - exp_prev_subtotal) / exp_prev_subtotal,
    tva_imp       = 100 * (imp_subtotal - imp_prev_subtotal) / imp_prev_subtotal,
    con_exp       = 100 * (exp_subtotal - exp_prev_subtotal) / tot$exp_prev,
    con_imp       = 100 * (imp_subtotal - imp_prev_subtotal) / tot$imp_prev,
    parent        = ""
  )
  
  df_final <- rbind(df_final, fila_subtotal, use.names = TRUE)
  
  return(df_final[])
}


#### Salidas ----
##### Tablas sectores y paises ----
render_datatable_datacomexsec_desplegable <- function(df, cols_semaforo = NULL, cols_barras = NULL, 
                                                   cols_barras_con = NULL, cols_barras_cien = NULL, 
                                                   cols_fecha = NULL, param, cols_enteros = NULL, 
                                                   pageLength = 100, decimales_defecto = 1, 
                                                   nodos_con_hijos = NULL, ...) {
  
  df_prep <- as.data.frame(df)
  
  # Guardar columnas auxiliares
  orden_col <- if("orden" %in% names(df_prep)) df_prep$orden else seq_len(nrow(df_prep))
  niv_col <- if("niv" %in% names(df_prep)) df_prep$niv else rep(2, nrow(df_prep))
  
  # Convertir a character
  orden_col_char <- as.character(orden_col)
  
  # CONSTRUIR RELACIONES PADRE-HIJO AUTOMÁTICAMENTE
  parent_id <- character(nrow(df_prep))
  current_parent_niv1 <- NA
  current_parent_niv2 <- NA
  
  for (i in seq_len(nrow(df_prep))) {
    niv <- niv_col[i]
    
    if (niv == 0 || niv == 1) {
      # Nivel 0 y 1 no tienen padre
      parent_id[i] <- ""
      current_parent_niv1 <- orden_col_char[i]
      current_parent_niv2 <- NA
    } else if (niv == 2) {
      # Nivel 2 es hijo del último nivel 1
      parent_id[i] <- if (!is.na(current_parent_niv1)) current_parent_niv1 else ""
      current_parent_niv2 <- orden_col_char[i]
    } else if (niv == 3) {
      # Nivel 3 es hijo del último nivel 2
      parent_id[i] <- if (!is.na(current_parent_niv2)) current_parent_niv2 else ""
    }
  }
  
  # Determinar qué nodos tienen hijos
  if (!is.null(nodos_con_hijos)) {
    has_children <- orden_col_char %in% as.character(nodos_con_hijos)
  } else {
    # Calcular automáticamente
    has_children <- sapply(orden_col_char, function(ord) {
      any(parent_id == ord & parent_id != "", na.rm = TRUE)
    })
  }
  
  # Agregar espacios según nivel en la columna nombre
  if("nombre" %in% names(df_prep)) {
    df_prep$nombre <- ifelse(niv_col == 0, df_prep$nombre,
                             ifelse(niv_col == 1, df_prep$nombre,
                                    ifelse(niv_col == 2, paste0("\u00A0\u00A0", df_prep$nombre),
                                           paste0("\u00A0\u00A0\u00A0\u00A0", df_prep$nombre))))
  }
  
  # Columna con iconos de expansión
  expansion_col <- sapply(seq_along(niv_col), function(i) {
    if (has_children[i]) {
      sprintf('<span class="expander" data-row-id="%s" style="cursor:pointer; user-select:none; font-size:14px; font-weight:bold; color:#2e5c8a;">▶</span>', 
              orden_col_char[i])
    } else {
      '<span style="visibility:hidden; font-size:14px;">▶</span>'
    }
  })
  
  # Ocultar columnas auxiliares si existen
  cols_to_remove <- c("niv", "parent")
  df_display <- df_prep[, setdiff(names(df_prep), cols_to_remove), drop = FALSE]
  
  # Agregar columna de expansión al principio
  df_display <- cbind(` ` = expansion_col, df_display)
  
  # Agregar columna # si no existe orden
  if(!"orden" %in% names(df_display)) {
    df_display <- cbind(`#` = orden_col, df_display)
  } else {
    names(df_display)[names(df_display) == "orden"] <- "#"
  }
  
  # Agregar columnas ocultas para el control jerárquico
  df_display$`.niv` <- niv_col
  df_display$`.parent` <- parent_id
  df_display$`.orden` <- orden_col_char
  
  # Extraer variables para texto
  ano_actual <- param$ano
  ano_anterior <- param$ano - 1L
  periodo <- paste0(param$texto_periodo, " ", param$ano)
  
  # Sketch de encabezados
  sketch <- htmltools::withTags(
    table(
      class = 'display compact',
      thead(
        tr(
          th(colspan = 3, periodo,
             style='text-align:center; background-color:#2e5c8a; color:white;'),
          th(colspan = 4, 'EXPORTACIONES',
             style='text-align:center; background-color:#2e5c8a; color:white;'),
          th(colspan = 4, 'IMPORTACIONES',
             style='text-align:center; background-color:#2e5c8a; color:white;'),
          th(colspan = 2, 'SALDO',
             style='text-align:center; background-color:#2e5c8a; color:white;'),
          th('', style='display:none;'),
          th('', style='display:none;'),
          th('', style='display:none;')
        ),
        tr(
          th('', style='text-align:center; background-color:#2e5c8a; color:white; width:30px;'),
          th('#', style='text-align:center; background-color:#2e5c8a; color:white;'),
          th('Sector', style='text-align:center; background-color:#2e5c8a; color:white;'),
          lapply(
            c('Mill. €', '% Total', 'TVA (%)', 'Con. (p.p.)',
              'Mill. €', '% Total', 'TVA (%)', 'Con. (p.p.)',
              ano_actual, ano_anterior),
            function(x) {
              th(x, style='text-align:center; background-color:#2e5c8a; color:white;')
            }
          )
        )
      )
    )
  )
  
  # Índices de columnas ocultas
  niv_col_idx <- ncol(df_display) - 3
  parent_col_idx <- ncol(df_display) - 2
  orden_col_idx <- ncol(df_display) - 1
  
  # Índices de columnas semáforo
  semaforo_indices <- c()
  if (!is.null(cols_semaforo)) {
    for (col in cols_semaforo) {
      if (col %in% names(df_display)) {
        semaforo_indices <- c(semaforo_indices, which(names(df_display) == col) - 1)
      }
    }
  }
  semaforo_indices_js <- paste0("[", paste(semaforo_indices, collapse = ","), "]")
  
  # JavaScript para barras de contribución
  js_parts_con <- c()
  if (!is.null(cols_barras_con)) {
    for (col in cols_barras_con) {
      if (col %in% names(df_display)) {
        vals <- df_display[[col]]
        vals <- vals[!is.na(vals)]
        max_pos <- max(abs(vals), na.rm = TRUE)
        max_neg <- -max_pos
        col_idx <- which(names(df_display) == col) - 1
        
        js_parts_con <- c(js_parts_con, sprintf("
        (function() {
          var val = parseFloat(data[%d]);
          if (!isNaN(val) && val !== null) {
            data[%d] = val.toFixed(1).replace('.', ',');
            var maxPos = %f;
            var maxNeg = %f;
            var gradient;
            if (val >= 0) {
              var pct = 50 * val / maxPos;
              gradient = 'linear-gradient(to right, transparent 50%%, #28a745 50%%, #28a745 ' + (50 + pct) + '%%, transparent ' + (50 + pct) + '%%)';
            } else {
              var pct = 50 * Math.abs(val) / Math.abs(maxNeg);
              gradient = 'linear-gradient(to right, transparent ' + (50 - pct) + '%%, #dc3545 ' + (50 - pct) + '%%, #dc3545 50%%, transparent 50%%)';
            }
            $('td:eq(%d)', row).css('background', gradient);
          }
        })();
      ", col_idx, col_idx, max_pos, max_neg, col_idx))
      }
    }
  }
  
  # JavaScript para formato de filas y lógica de visibilidad inicial
  js_row_format <- sprintf("
    var niv = parseInt(data[%d]);
    var parent = String(data[%d] || '');
    var orden = String(data[%d] || '');
    var semaforoIndices = %s;
    
    // Centrar texto en todas las celdas
    $('td', row).css('text-align', 'center');
    
    // Marcar atributos en el row
    $(row).attr('data-nivel', niv);
    $(row).attr('data-orden', orden);
    $(row).attr('data-parent', parent);
    
    // Ocultar filas de nivel 2 y 3 inicialmente
    if (niv >= 2) {
      $(row).addClass('nivel-' + niv);
      $(row).css('display', 'none');
    }
    
    if (niv === 0) {
      $(row).css('background-color', '#2e5c8a');
      $(row).css('color', 'white');
      $(row).css('font-weight', 'bold');
      $('td', row).css('background', 'none');
      
      semaforoIndices.forEach(function(idx) {
        var val = parseFloat(data[idx]);
        if (!isNaN(val)) {
          if (val < 0) {
            $('td:eq(' + idx + ')', row).css('color', '#ffb3b3');
          } else {
            $('td:eq(' + idx + ')', row).css('color', '#28a745');
          }
        }
      });
      
    } else if (niv === 1) {
      $(row).css('background-color', '#b8d4ea');
      $(row).css('font-weight', 'bold');
      $('td', row).css('background', 'none');
      
    } else if (niv === 3) {
      $(row).css('font-style', 'italic');
    }
  ", niv_col_idx, parent_col_idx, orden_col_idx, semaforo_indices_js)
  
  # Combinar todo en rowCallback
  row_callback_js <- DT::JS(paste0(
    "function(row, data, displayNum, displayIndex, dataIndex) {",
    js_row_format,
    paste(js_parts_con, collapse = "\n"),
    "}"
  ))
  
  # JavaScript para manejar clics en los expandidores
  expand_js <- DT::JS("
    function(settings, json) {
      var api = this.api();
      
      $(api.table().container()).on('click', '.expander', function(e) {
        e.stopPropagation();
        var span = $(this);
        var parentOrden = String(span.data('row-id') || '');
        var isExpanded = span.html().indexOf('▼') !== -1;
        
        if (isExpanded) {
          // Colapsar
          span.html('▶');
          
          // Función recursiva para colapsar todos los descendientes
          function collapseDescendants(orden) {
            if (!orden) return;
            
            api.rows().every(function() {
              var row = this.node();
              var rowParent = String($(row).attr('data-parent') || '');
              
              if (rowParent === orden) {
                $(row).css('display', 'none');
                var rowOrden = String($(row).attr('data-orden') || '');
                
                // Colapsar el expander de este hijo si tiene
                $(row).find('.expander').html('▶');
                
                // Recursivamente colapsar sus descendientes
                collapseDescendants(rowOrden);
              }
            });
          }
          
          collapseDescendants(parentOrden);
          
        } else {
          // Expandir solo hijos directos
          span.html('▼');
          
          if (!parentOrden) return;
          
          api.rows().every(function() {
            var row = this.node();
            var rowParent = String($(row).attr('data-parent') || '');
            
            if (rowParent === parentOrden) {
              $(row).css('display', '');
            }
          });
        }
      });
    }
  ")
  
  # Construir opciones
  dt_options <- list(
    pageLength = pageLength,
    lengthMenu = c(10,25,50,100),
    dom = 'Bfrtip',
    buttons = list(
      list(extend='copy', text='Copiar', className='btn-sm'),
      list(extend='csv', text='CSV', className='btn-sm'),
      list(extend='excel', text='Excel', className='btn-sm')
    ),
    scrollX = TRUE,
    language = list(
      decimal = ",",
      thousands = ".",
      url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"
    ),
    columnDefs = list(
      list(visible = FALSE, targets = c(niv_col_idx, parent_col_idx, orden_col_idx)),
      list(className = 'dt-center', targets = '_all'),
      list(orderable = FALSE, targets = 0)
    ),
    rowCallback = row_callback_js,
    initComplete = expand_js
  )
  
  dt <- DT::datatable(
    df_display,
    container = sketch,
    extensions = 'Buttons',
    options = dt_options,
    class = 'display compact',
    rownames = FALSE,
    escape = FALSE
  )
  
  # SEMÁFORO
  if(!is.null(cols_semaforo)) {
    for(col in cols_semaforo) {
      if(col %in% names(df_display)) {
        dt <- dt |> DT::formatStyle(
          columns = col,
          color = DT::styleInterval(0, c("#dc3545", "#28a745")), 
          fontWeight = 'bold'
        )
      }
    }
  }
  
  # BARRAS
  if(!is.null(cols_barras)) {
    for(col in cols_barras) {
      if(col %in% names(df_display)) {
        max_val <- max(abs(df_display[[col]]), na.rm=TRUE)
        dt <- dt |> DT::formatRound(col, 1, mark=".", dec.mark=",")
        dt <- dt |> DT::formatStyle(
          columns = col,
          background = DT::styleColorBar(c(0, max_val), "lightblue"),
          backgroundSize = '98% 80%', 
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'left center'
        )
      }
    }
  }
  
  if(!is.null(cols_barras_cien)) {
    for(col in cols_barras_cien) {
      if(col %in% names(df_display)) {
        dt <- dt |> DT::formatRound(col, 1, mark=".", dec.mark=",")
        dt <- dt |> DT::formatStyle(
          columns = col,
          background = DT::styleColorBar(c(0, 100), "lightblue"),
          backgroundSize = '98% 80%', 
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'left center'
        )
      }
    }
  }
  
  # FORMATO NUMÉRICO
  numeric_cols <- names(df_display)[sapply(df_display, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, c(".niv", ".parent", ".orden", "#"))
  cols_con_barras <- c(cols_barras, cols_barras_cien)
  numeric_cols <- setdiff(numeric_cols, cols_con_barras)
  
  if(!is.null(cols_enteros)) {
    for(col in cols_enteros) {
      if(col %in% numeric_cols) {
        dt <- dt |> DT::formatRound(col, 0, mark=".", dec.mark=",")
        numeric_cols <- numeric_cols[numeric_cols != col]
      }
    }
  }
  
  for(col in numeric_cols) {
    dt <- dt |> DT::formatRound(col, decimales_defecto, mark=".", dec.mark=",")
  }
  
  return(dt)
}

render_datatable_datacomexpaises_desplegable <- function(df, cols_semaforo = NULL, cols_barras = NULL, 
                                                         cols_barras_con = NULL, cols_barras_cien = NULL, 
                                                         cols_fecha = NULL, param, cols_enteros = NULL, 
                                                         pageLength = 100, decimales_defecto = 1, 
                                                         nodos_con_hijos = NULL, ...) {
  
  df_prep <- as.data.frame(df)
  
  # Guardar columnas auxiliares
  orden_col <- if("orden" %in% names(df_prep)) df_prep$orden else seq_len(nrow(df_prep))
  niv_col <- if("niv" %in% names(df_prep)) df_prep$niv else rep(2, nrow(df_prep))
  
  # Convertir a character
  orden_col_char <- as.character(orden_col)
  
  # CONSTRUIR RELACIONES PADRE-HIJO AUTOMÁTICAMENTE
  parent_id <- character(nrow(df_prep))
  current_parent_niv1 <- NA
  current_parent_niv2 <- NA
  current_parent_niv3 <- NA
  
  for (i in seq_len(nrow(df_prep))) {
    niv <- niv_col[i]
    
    if (niv == 0 || niv == 1) {
      # Nivel 0 y 1 no tienen padre
      parent_id[i] <- ""
      current_parent_niv1 <- orden_col_char[i]
      current_parent_niv2 <- NA
      current_parent_niv3 <- NA
    } else if (niv == 2) {
      # Nivel 2 es hijo del último nivel 1
      parent_id[i] <- if (!is.na(current_parent_niv1)) current_parent_niv1 else ""
      current_parent_niv2 <- orden_col_char[i]
      current_parent_niv3 <- NA
    } else if (niv == 3) {
      # Nivel 3 es hijo del último nivel 2
      parent_id[i] <- if (!is.na(current_parent_niv2)) current_parent_niv2 else ""
      current_parent_niv3 <- orden_col_char[i]
    } else if (niv == 4) {
      # Nivel 4 es hijo del último nivel 3
      parent_id[i] <- if (!is.na(current_parent_niv3)) current_parent_niv3 else ""
    }
  }
  
  # Determinar qué nodos tienen hijos
  if (!is.null(nodos_con_hijos)) {
    has_children <- orden_col_char %in% as.character(nodos_con_hijos)
  } else {
    # Calcular automáticamente
    has_children <- sapply(orden_col_char, function(ord) {
      any(parent_id == ord & parent_id != "", na.rm = TRUE)
    })
  }
  
  # Agregar espacios según nivel en la columna pais
  if("pais" %in% names(df_prep)) {
    df_prep$pais <- ifelse(niv_col == 0, df_prep$pais,
                           ifelse(niv_col == 1, df_prep$pais,
                                  ifelse(niv_col == 2, paste0("\u00A0\u00A0", df_prep$pais),
                                         ifelse(niv_col == 3, paste0("\u00A0\u00A0\u00A0\u00A0", df_prep$pais),
                                                paste0("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", df_prep$pais)))))
  }
  
  # Columna con iconos de expansión
  expansion_col <- sapply(seq_along(niv_col), function(i) {
    if (has_children[i]) {
      sprintf('<span class="expander" data-row-id="%s" style="cursor:pointer; user-select:none; font-size:14px; font-weight:bold; color:#2e5c8a;">▶</span>', 
              orden_col_char[i])
    } else {
      '<span style="visibility:hidden; font-size:14px;">▶</span>'
    }
  })
  
  # Ocultar columnas auxiliares si existen
  cols_to_remove <- c("niv", "parent", "cod")
  df_display <- df_prep[, setdiff(names(df_prep), cols_to_remove), drop = FALSE]
  
  # Agregar columna de expansión al principio
  df_display <- cbind(` ` = expansion_col, df_display)
  
  # Agregar columna # si no existe orden
  if(!"orden" %in% names(df_display)) {
    df_display <- cbind(`#` = orden_col, df_display)
  } else {
    names(df_display)[names(df_display) == "orden"] <- "#"
  }
  
  # Agregar columnas ocultas para el control jerárquico
  df_display$`.niv` <- niv_col
  df_display$`.parent` <- parent_id
  df_display$`.orden` <- orden_col_char
  
  # Extraer variables para texto
  ano_actual <- param$ano
  ano_anterior <- param$ano - 1L
  periodo <- paste0(param$texto_periodo, " ", param$ano)
  
  # Sketch de encabezados
  sketch <- htmltools::withTags(
    table(
      class = 'display compact',
      thead(
        tr(
          th(colspan = 3, periodo,
             style='text-align:center; background-color:#2e5c8a; color:white;'),
          th(colspan = 4, 'EXPORTACIONES',
             style='text-align:center; background-color:#2e5c8a; color:white;'),
          th(colspan = 4, 'IMPORTACIONES',
             style='text-align:center; background-color:#2e5c8a; color:white;'),
          th(colspan = 2, 'SALDO',
             style='text-align:center; background-color:#2e5c8a; color:white;'),
          th('', style='display:none;'),
          th('', style='display:none;'),
          th('', style='display:none;')
        ),
        tr(
          th('', style='text-align:center; background-color:#2e5c8a; color:white; width:30px;'),
          th('#', style='text-align:center; background-color:#2e5c8a; color:white;'),
          th('País/Región', style='text-align:center; background-color:#2e5c8a; color:white;'),
          lapply(
            c('Mill. €', '% Total', 'TVA (%)', 'Con. (p.p.)',
              'Mill. €', '% Total', 'TVA (%)', 'Con. (p.p.)',
              ano_actual, ano_anterior),
            function(x) {
              th(x, style='text-align:center; background-color:#2e5c8a; color:white;')
            }
          )
        )
      )
    )
  )
  
  # Índices de columnas ocultas
  niv_col_idx <- ncol(df_display) - 3
  parent_col_idx <- ncol(df_display) - 2
  orden_col_idx <- ncol(df_display) - 1
  
  # Índices de columnas semáforo
  semaforo_indices <- c()
  if (!is.null(cols_semaforo)) {
    for (col in cols_semaforo) {
      if (col %in% names(df_display)) {
        semaforo_indices <- c(semaforo_indices, which(names(df_display) == col) - 1)
      }
    }
  }
  semaforo_indices_js <- paste0("[", paste(semaforo_indices, collapse = ","), "]")
  
  # JavaScript para barras de contribución
  js_parts_con <- c()
  if (!is.null(cols_barras_con)) {
    for (col in cols_barras_con) {
      if (col %in% names(df_display)) {
        vals <- df_display[[col]]
        vals <- vals[!is.na(vals)]
        max_pos <- max(abs(vals), na.rm = TRUE)
        max_neg <- -max_pos
        col_idx <- which(names(df_display) == col) - 1
        
        js_parts_con <- c(js_parts_con, sprintf("
        (function() {
          var val = parseFloat(data[%d]);
          if (!isNaN(val) && val !== null) {
            data[%d] = val.toFixed(1).replace('.', ',');
            var maxPos = %f;
            var maxNeg = %f;
            var gradient;
            if (val >= 0) {
              var pct = 50 * val / maxPos;
              gradient = 'linear-gradient(to right, transparent 50%%, #28a745 50%%, #28a745 ' + (50 + pct) + '%%, transparent ' + (50 + pct) + '%%)';
            } else {
              var pct = 50 * Math.abs(val) / Math.abs(maxNeg);
              gradient = 'linear-gradient(to right, transparent ' + (50 - pct) + '%%, #dc3545 ' + (50 - pct) + '%%, #dc3545 50%%, transparent 50%%)';
            }
            $('td:eq(%d)', row).css('background', gradient);
          }
        })();
      ", col_idx, col_idx, max_pos, max_neg, col_idx))
      }
    }
  }
  
  # JavaScript para formato de filas y lógica de visibilidad inicial
  js_row_format <- sprintf("
    var niv = parseInt(data[%d]);
    var parent = String(data[%d] || '');
    var orden = String(data[%d] || '');
    var semaforoIndices = %s;
    
    // Centrar texto en todas las celdas
    $('td', row).css('text-align', 'center');
    
    // Marcar atributos en el row
    $(row).attr('data-nivel', niv);
    $(row).attr('data-orden', orden);
    $(row).attr('data-parent', parent);
    
    // Ocultar filas de nivel 2, 3 y 4 inicialmente
    if (niv >= 2) {
      $(row).addClass('nivel-' + niv);
      $(row).css('display', 'none');
      $(row).attr('data-visible', 'false');
    } else {
      $(row).attr('data-visible', 'true');
    }
    
    if (niv === 0) {
      $(row).css('background-color', '#2e5c8a');
      $(row).css('color', 'white');
      $(row).css('font-weight', 'bold');
      $('td', row).css('background', 'none');
      
      semaforoIndices.forEach(function(idx) {
        var val = parseFloat(data[idx]);
        if (!isNaN(val)) {
          if (val < 0) {
            $('td:eq(' + idx + ')', row).css('color', '#ffb3b3');
          } else {
            $('td:eq(' + idx + ')', row).css('color', '#28a745');
          }
        }
      });
      
    } else if (niv === 1) {
      $(row).css('background-color', '#b8d4ea');
      $(row).css('font-weight', 'bold');
      $('td', row).css('background', 'none');
      
    } else if (niv === 2 || niv === 3) {
      // Nivel 2 y 3 tienen formato normal (sin estilo especial)
      
    } else if (niv === 4) {
      $(row).css('font-style', 'italic');
    }
  ", niv_col_idx, parent_col_idx, orden_col_idx, semaforo_indices_js)
  
  # Combinar todo en rowCallback
  row_callback_js <- DT::JS(paste0(
    "function(row, data, displayNum, displayIndex, dataIndex) {",
    js_row_format,
    paste(js_parts_con, collapse = "\n"),
    "}"
  ))
  
  # JavaScript para manejar clics en los expandidores
  expand_js <- DT::JS("
    function(settings, json) {
      var api = this.api();
      
      $(api.table().container()).on('click', '.expander', function(e) {
        e.stopPropagation();
        var span = $(this);
        var parentOrden = String(span.data('row-id') || '');
        var isExpanded = span.html().indexOf('▼') !== -1;
        
        if (isExpanded) {
          // Colapsar
          span.html('▶');
          
          // Función recursiva para colapsar todos los descendientes
          function collapseDescendants(orden) {
            if (!orden) return;
            
            api.rows().every(function() {
              var row = this.node();
              var rowParent = String($(row).attr('data-parent') || '');
              
              if (rowParent === orden) {
                $(row).css('display', 'none');
                var rowOrden = String($(row).attr('data-orden') || '');
                
                // Colapsar el expander de este hijo si tiene
                $(row).find('.expander').html('▶');
                
                // Recursivamente colapsar sus descendientes
                collapseDescendants(rowOrden);
              }
            });
          }
          
          collapseDescendants(parentOrden);
          
        } else {
          // Expandir solo hijos directos
          span.html('▼');
          
          if (!parentOrden) return;
          
          api.rows().every(function() {
            var row = this.node();
            var rowParent = String($(row).attr('data-parent') || '');
            
            if (rowParent === parentOrden) {
              $(row).css('display', '');
            }
          });
        }
      });
    }
  ")
  
  # Construir opciones
  dt_options <- list(
    pageLength = pageLength,
    lengthMenu = c(10,25,50,100),
    dom = 'Bfrtip',
    buttons = list(
      list(extend='copy', text='Copiar', className='btn-sm'),
      list(extend='csv', text='CSV', className='btn-sm'),
      list(extend='excel', text='Excel', className='btn-sm')
    ),
    scrollX = TRUE,
    language = list(
      decimal = ",",
      thousands = ".",
      url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"
    ),
    columnDefs = list(
      list(visible = FALSE, targets = c(niv_col_idx, parent_col_idx, orden_col_idx)),
      list(className = 'dt-center', targets = '_all'),
      list(orderable = FALSE, targets = 0)
    ),
    rowCallback = row_callback_js,
    initComplete = expand_js
  )
  
  dt <- DT::datatable(
    df_display,
    container = sketch,
    extensions = 'Buttons',
    options = dt_options,
    class = 'display compact',
    rownames = FALSE,
    escape = FALSE
  )
  
  # SEMÁFORO
  if(!is.null(cols_semaforo)) {
    for(col in cols_semaforo) {
      if(col %in% names(df_display)) {
        dt <- dt |> DT::formatStyle(
          columns = col,
          color = DT::styleInterval(0, c("#dc3545", "#28a745")), 
          fontWeight = 'bold'
        )
      }
    }
  }
  
  # BARRAS
  if(!is.null(cols_barras)) {
    for(col in cols_barras) {
      if(col %in% names(df_display)) {
        max_val <- max(abs(df_display[[col]]), na.rm=TRUE)
        dt <- dt |> DT::formatRound(col, 1, mark=".", dec.mark=",")
        dt <- dt |> DT::formatStyle(
          columns = col,
          background = DT::styleColorBar(c(0, max_val), "lightblue"),
          backgroundSize = '98% 80%', 
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'left center'
        )
      }
    }
  }
  
  if(!is.null(cols_barras_cien)) {
    for(col in cols_barras_cien) {
      if(col %in% names(df_display)) {
        dt <- dt |> DT::formatRound(col, 1, mark=".", dec.mark=",")
        dt <- dt |> DT::formatStyle(
          columns = col,
          background = DT::styleColorBar(c(0, 100), "lightblue"),
          backgroundSize = '98% 80%', 
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'left center'
        )
      }
    }
  }
  
  # FORMATO NUMÉRICO
  numeric_cols <- names(df_display)[sapply(df_display, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, c(".niv", ".parent", ".orden", "#"))
  cols_con_barras <- c(cols_barras, cols_barras_cien)
  numeric_cols <- setdiff(numeric_cols, cols_con_barras)
  
  if(!is.null(cols_enteros)) {
    for(col in cols_enteros) {
      if(col %in% numeric_cols) {
        dt <- dt |> DT::formatRound(col, 0, mark=".", dec.mark=",")
        numeric_cols <- numeric_cols[numeric_cols != col]
      }
    }
  }
  
  for(col in numeric_cols) {
    dt <- dt |> DT::formatRound(col, decimales_defecto, mark=".", dec.mark=",")
  }
  
  return(dt)
}

##### Plots ----
grafica_treemap_informe <- function(dt, flujo = "exp", para, tipo = "sectores", tipo_plot = "treemap") {
  # Configuración según tipo
  if (tipo == "sectores") {
    filtro_orden <- 66
    parent_raiz <- "65"
    orden_raiz <- 65
    col_codigo <- "cod_sec"
    col_label <- "nombre"
    col_label_final <- "sec"
    texto_entidad <- "Producto"
  } else {  # paises
    filtro_orden <- 72
    parent_raiz <- "71"
    orden_raiz <- 71
    col_codigo <- "cod_pais"
    col_label <- "pais"
    col_label_final <- "pais"
    texto_entidad <- "País/Región"
  }
  
  # Copiar datos originales
  dt <- data.table::copy(dt)
  dt <- dt[orden != filtro_orden]
  
  # Crear columna de código como carácter
  dt[, (col_codigo) := as.character(orden)]
  
  # Construcción del campo 'parent'
  for (i in seq_len(nrow(dt))) {
    niv_actual <- dt[i, niv]
    
    if (tipo == "sectores") {
      if (niv_actual == 0) {
        dt[i, parent := ""]
      } else if (niv_actual == 1) {
        dt[i, parent := parent_raiz]
      } else if (niv_actual %in% c(2, 3)) {
        idx_parent <- max(dt[1:(i - 1)][niv < niv_actual, orden], na.rm = TRUE)
        if (is.finite(idx_parent)) {
          dt[i, parent := as.character(idx_parent)]
        } else {
          dt[i, parent := ""]
        }
      }
    } else {  # paises
      if (niv_actual == 1) {
        dt[i, parent := parent_raiz]
      } else if (niv_actual > 1) {
        idx_parent <- max(dt[1:(i - 1)][niv < niv_actual, orden], na.rm = TRUE)
        if (is.finite(idx_parent)) {
          dt[i, parent := as.character(idx_parent)]
        } else {
          dt[i, parent := parent_raiz]
        }
      } else {
        dt[i, parent := ""]
      }
    }
  }
  
  # Configurar parent del nodo raíz
  if (orden_raiz %in% dt$orden) {
    dt[orden == orden_raiz, parent := ""]
  }
  
  # Variables dinámicas según flujo
  var_val  <- flujo
  var_prev <- paste0(flujo, "_prev")
  var_dif  <- paste0(flujo, "_dif")
  var_per  <- paste0(flujo, "_per")
  var_tva  <- paste0("tva_", flujo)
  var_con  <- paste0("con_", flujo)
  
  # Variables base y de flujo
  vars_base <- c(col_codigo, col_label, "parent")
  vars_flujo <- c(var_val, var_prev, var_dif, var_per, var_tva, var_con)
  
  df <- dt[, c(vars_base, vars_flujo), with = FALSE]
  
  # Renombrar columna label si es necesario
  if (col_label != col_label_final) {
    data.table::setnames(df, col_label, col_label_final)
  }
  
  # Escalado de valores
  if (tipo == "sectores") {
    max_val <- max(df[get(col_codigo) != "0", get(var_val)], na.rm = TRUE)
  } else {
    max_val <- max(df[[var_val]], na.rm = TRUE)
  }
  max_int <- .Machine$integer.max
  scale_factor <- max_val / max_int
  df[, Volumen := as.integer(get(var_val) / scale_factor)]
  
  # Escala de contribuciones
  contrib_raw <- df[[var_con]]
  contrib_max <- max(contrib_raw, na.rm = TRUE)
  contrib_min <- min(contrib_raw, na.rm = TRUE)
  
  # Evitar división por cero
  if (contrib_max == contrib_min) {
    zero_position <- 0.5
  } else {
    zero_position <- (-contrib_min) / (contrib_max - contrib_min)
  }
  
  # Formatear valores
  df[, vol := get(var_val)]
  df[, volprev := get(var_prev)]
  df[, voldif := get(var_dif)]
  
  df[, vol := formatear_num(vol)]
  df[, volprev := formatear_num(volprev)]
  df[, voldif := formatear_num(voldif)]
  
  # Labels y texto hover
  df[, text_label := paste0(get(col_label_final), ": ", vol)]
  
  flujo_text <- ifelse(flujo == "exp", "Exportaciones", "Importaciones")
  df[, hover_text := paste0(
    "<b>Flujo:</b> ", flujo_text, "<br>",
    "<b>", texto_entidad, ":</b> ", get(col_label_final), "<br>",
    "<b>Volumen:</b> ", vol, " ", para$varud, "<br>",
    "Volumen año anterior: ", volprev, " ", para$varud, "<br>",
    "Diferencia: ", voldif, " ", para$varud, "<br>",
    "TVA: ", ifelse(is.na(get(var_tva)), "-", 
                    paste0(formatear_num(get(var_tva)), "%")), "<br>",
    "<b>Contribución:</b> ", formatear_num(get(var_con)), " p.p.<br>",
    "<b>Peso:</b> ", formatear_num(get(var_per)), "%"
  )]
  
  # Construcción del treemap
  fig <- plotly::plot_ly(
    data = df,
    type = tipo_plot,
    ids = as.formula(paste0("~", col_codigo)),
    labels = as.formula(paste0("~", col_label_final)),
    parents = ~parent,
    values = ~Volumen,
    branchvalues = "total",
    text = ~text_label,
    textinfo = "text",
    hoverinfo = "text",
    hovertext = ~hover_text,
    marker = list(
      colors = df[[var_con]],
      colorscale = list(
        list(0, "red"),
        list(zero_position, "lightgrey"),
        list(1, "green")
      ),
      colorbar = list(
        title = "Contribución (p.p.)",
        ticksuffix = " p.p."
      ),
      cmin = contrib_min,
      cmax = contrib_max,
      reversescale = FALSE
    )
  )
  
  return(fig)
}

graficas_temporales_sectores <- function(datas, tot, para){
  columna <- "exp"
  df_micro <- datas |>
    dplyr::filter(
      mes %in% para$meses,
      cod_sector_economico %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "43"),
      pais %in% para$pais 
    ) |>
    dplyr::group_by(flujo, año, cod_sector_economico) |>
    dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
    dplyr::select(flujo, año, cod_sector_economico, euros) |>
    dplyr::collect()
  
  df_micro <- data.table::as.data.table(df_micro)
  
  # Calcular exp e imp para cada año
  df_anual <- df_micro[, .(
    exp = sum(euros[flujo == 1], na.rm = TRUE),
    imp = sum(euros[flujo == 0], na.rm = TRUE)
  ), by = .(cod_sector_economico, año)]
  
  # Crear sector semimanufacturas no químicas
  df_4  <- df_anual[cod_sector_economico == "4"]
  df_43 <- df_anual[cod_sector_economico == "43"]
  
  df_z3 <- merge(df_4, df_43, 
                 by = "año", 
                 suffixes = c("_4","_43"))
  
  df_z3 <- df_z3[, .(
    cod_sector_economico = "Z3",
    año,
    exp = exp_4 - exp_43,
    imp = imp_4 - imp_43
  )]
  
  # Añadir sector 10 al dataset anual
  df_anual <- data.table::rbindlist(list(df_anual, df_z3), use.names = TRUE)
  
  # Eliminar sector 4
  df_anual <- df_anual[cod_sector_economico != "4"]
  
  # Crear tabla con año previo
  df_prev <- data.table::copy(df_anual)
  df_prev[, año := año + 1]
  
  data.table::setnames(df_prev,
                       old = c("exp", "imp"),
                       new = c("exp_prev", "imp_prev"))
  
  # Join para obtener exp_prev e imp_prev
  df_final <- df_prev[df_anual, 
                      on = .(cod_sector_economico, año)]
  
  map_sectores <- data.table::data.table(
    cod_sector_economico = c("1","2","3","43","5","6","7","8","9","Z3"),
    sec = c(
      "Alimentación, bebidas y tabaco",
      "Productos energéticos",
      "Materias primas",
      "Productos químicos",
      "Bienes de equipo",
      "Sector automóvil",
      "Bienes de consumo duradero",
      "Manufacturas de consumo",
      "Otras mercancías",
      "Semifacturas no químicas"
    )
  )
  
  # Añadir columnas
  df_final <- map_sectores[df_final,
                           on = .(cod_sector_economico), nomatch = 0]
  
  df_final[, exp_dif := exp - exp_prev]
  df_final[, imp_dif := imp - imp_prev]
  df_final[, exp_per := exp / tot$exp * 100]
  df_final[, imp_per := imp / tot$imp * 100]
  df_final[, tva_exp := ifelse(exp_prev != 0, exp_dif / exp_prev * 100, 0)]
  df_final[, tva_imp := ifelse(imp_prev != 0, imp_dif / imp_prev * 100, 0)]
  df_final[, con_exp := exp_dif / tot$exp_prev * 100]
  df_final[, con_imp := imp_dif / tot$imp_prev * 100]
  
  
  # GRÁFICO ANIMADO (RACE CHART)
  df_final[, valor := exp / para$varfactor]  
  sectores_unicos <- unique(df_final$sec)
  colors <- RColorBrewer::brewer.pal(n = length(sectores_unicos), name = "Set3")
  color_map <- setNames(colors, sectores_unicos)
  df_final[, color := color_map[sec]]
  df_final[, rank := data.table::frank(-valor, ties.method = "first"), by = año]
  
  # Formatear texto dentro de la barra
  df_final[, label :=
             paste0(
               sec, ": ",
               formatear_num(valor), " M (",
               scales::number(get(paste0(columna, "_per")),
                              big.mark = ".", decimal.mark = ",", accuracy = 0.1),
               "%)"
             )
  ]
  
  df_final[, valor_prev := exp_prev / para$varfactor]
  df_final[, valor_dif := exp_dif / para$varfactor]
  
  df_final[, hover_text := paste0(
    "<b>Flujo:</b> Exportaciones<br>",
    "<b>Año:</b> ", año, "<br>",
    "<b>Sector:</b> ", sec, "<br>",
    "<b>Volumen:</b> ", formatear_num(valor), " ", para$varud, "<br>",
    "Volumen año anterior: ", formatear_num(valor_prev), " ", para$varud, "<br>",
    "Diferencia: ", formatear_num(valor_dif), " ", para$varud, "<br>",
    "TVA: ", ifelse(is.na(tva_exp), "-", paste0(formatear_num(tva_exp), "%")), "<br>",
    "<b>Contribución:</b> ", formatear_num(con_exp), " p.p.<br>",
    "<b>Peso:</b> ", formatear_num(exp_per), "%"
  )]
  
  epsilon <- 0.001 * max(df_final$valor, na.rm = TRUE)
  
  # Crear factor con todos los sectores para mantener consistencia
  todos_sectores <- unique(df_final$sec)
  
  # Crear plotly animado
  fig_animado <- plotly::plot_ly() |>
    plotly::add_trace(
      data = df_final,
      x = ~valor,
      y = ~factor(sec, levels = todos_sectores),
      frame = ~año,
      color = ~sec,
      type = "bar",
      orientation = "h",
      colors = colors,
      hovertext = ~hover_text,
      hoverinfo = "text",
      textposition = "none",
      showlegend = FALSE
    ) |>
    plotly::add_trace(
      data = df_final,
      x = epsilon,                
      y = ~factor(sec, levels = todos_sectores),
      frame = ~año,
      text = ~label,
      type = "scatter",
      mode = "text",
      textposition = "middle right",
      textfont = list(color = "black", size = 10),
      hoverinfo = "none",
      showlegend = FALSE,
      inherit = FALSE
    ) |>
    plotly::layout(
      yaxis = list(
        title = "Sectores económicos",
        categoryorder = "total ascending",  
        showticklabels = FALSE
      ),
      legend = list(title = list(text = "Sector")),
      margin = list(l = 50)
    ) |> 
    plotly::animation_opts(frame = 1000, transition = 500, redraw = FALSE) |> 
    plotly::animation_slider(currentvalue = list(prefix = "Año: "))
  
  # Mostrar el último frame por defecto
  fig_animado <- plotly::plotly_build(fig_animado)
  
  if (!is.null(fig_animado$x$layout$sliders) && length(fig_animado$x$layout$sliders) > 0) {
    num_frames <- length(fig_animado$x$frames)
    fig_animado$x$layout$sliders[[1]]$active <- num_frames - 1
    
    if (num_frames > 0) {
      ultimo_frame <- fig_animado$x$frames[[num_frames]]
      fig_animado$x$data <- ultimo_frame$data
    }
  }
  
  
  # GRÁFICO SPAGHETTI (LÍNEAS TEMPORALES)
  
  df_final <- df_final[cod_sector_economico != "9"]
  
  # Obtener sectores únicos y sus colores
  sectores <- unique(df_final$sec)
  colores_sector <- setNames(unique(df_final$color), unique(df_final$sec))
  
  # Crear figura base
  fig_spaghetti <- plotly::plot_ly()
  
  # Para cada sector, crear un subplot
  for (sector in sectores) {
    # Datos del sector actual
    df_sector <- df_final[sec == sector]
    
    # Añadir líneas grises de fondo (todos los otros sectores)
    for (sector_bg in sectores) {
      df_bg <- df_final[sec == sector_bg]
      
      fig_spaghetti <- fig_spaghetti |>
        plotly::add_trace(
          data = df_bg,
          x = ~año,
          y = ~imp_per,
          type = "scatter",
          mode = "lines",
          line = list(
            color = ifelse(sector_bg == sector, colores_sector[sector], "rgba(200, 200, 200, 0.4)"),
            width = ifelse(sector_bg == sector, 3, 0.5)
          ),
          name = sector_bg,
          legendgroup = sector,
          showlegend = FALSE,
          xaxis = paste0("x", match(sector, sectores)),
          yaxis = paste0("y", match(sector, sectores)),
          hovertemplate = paste0(
            "<b> Sector: ", sector_bg, "</b><br>",
            "Año: %{x}<br>",
            "Importaciones: %{y:.1f}%<br>",
            "<extra></extra>"
          )
        )
    }
  }
  
  # Configurar layout con subplots 3x3
  n_cols <- 3
  n_rows <- ceiling(length(sectores) / n_cols)
  
  # Crear anotaciones para títulos de cada faceta
  annotations <- list()
  
  for (i in seq_along(sectores)) {
    row <- ceiling(i / n_cols)
    col <- ((i - 1) %% n_cols) + 1
    
    annotations[[i]] <- list(
      x = (col - 1) / n_cols + 1 / (2 * n_cols),
      y = 1 - (row - 1) / n_rows - 0.02 / n_rows,
      text = paste0("<b>", sectores[i], "</b>"),
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 11)
    )
  }
  
  # Configurar ejes para cada subplot
  axis_list <- list()
  
  for (i in seq_along(sectores)) {
    row <- ceiling(i / n_cols)
    col <- ((i - 1) %% n_cols) + 1
    
    # Dominio X
    x_domain <- c(
      (col - 1) / n_cols + 0.02,
      col / n_cols - 0.02
    )
    
    # Dominio Y (invertido para que fila 1 esté arriba)
    y_domain <- c(
      1 - row / n_rows + 0.08,
      1 - (row - 1) / n_rows - 0.05
    )
    
    # Configuración eje X
    x_name <- if (i == 1) "xaxis" else paste0("xaxis", i)
    axis_list[[x_name]] <- list(
      domain = x_domain,
      showgrid = TRUE,
      zeroline = FALSE,
      title = "",
      side = if (row == n_rows) "bottom" else "top",
      showticklabels = if (row == n_rows) TRUE else FALSE
    )
    
    # Configuración eje Y
    y_name <- if (i == 1) "yaxis" else paste0("yaxis", i)
    axis_list[[y_name]] <- list(
      domain = y_domain,
      showgrid = TRUE,
      zeroline = FALSE,
      title = ""
    )
  }
  
  # Aplicar layout
  fig_spaghetti <- fig_spaghetti |>
    plotly::layout(
      annotations = annotations,
      showlegend = FALSE,
      margin = list(t = 80, b = 60, l = 60, r = 20)
    )
  
  # Añadir configuración de ejes
  fig_spaghetti$x$layout <- c(fig_spaghetti$x$layout, axis_list)
  
  
  # DEVOLVER LISTA CON AMBOS GRÁFICOS
  
  return(list(
    animado = fig_animado,
    spaghetti = fig_spaghetti
  ))
}

graficas_evolucion_secpais <- function(datas, para){
  calc_series <- function(df, prefix){
    df_mensual <- df[, .(
      exp = sum(euros[flujo == 1], na.rm = TRUE),
      imp = sum(euros[flujo == 0], na.rm = TRUE)
    ), by = .(año, mes)]
    
    df_previo <- data.table::copy(df_mensual)[, año := año + 1]
    
    data.table::setnames(df_mensual,
                         old = c("exp", "imp"),
                         new = paste0(prefix, c("_exp", "_imp")))
    
    data.table::setnames(df_previo,
                         old = c("exp", "imp"),
                         new = paste0(prefix, c("_exp_prev", "_imp_prev")))
    
    df_previo[df_mensual, on = .(año, mes)]
  }
  
  df_norm <- data.table::as.data.table(datas |>
                                         dplyr::filter(
                                           pais %in% para$pais,
                                           cod_sector_economico %in% para$sector
                                         ) |>
                                         dplyr::group_by(flujo, año, mes) |>
                                         dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
                                         dplyr::select(flujo, año, mes, euros) |>
                                         dplyr::collect()
  )
  
  df_final_norm <- calc_series(df_norm, "norm")
  
  df_pais <- data.table::as.data.table(datas |>
                                         dplyr::filter(
                                           pais %in% para$pais,
                                           cod_sector_economico %in% 0L
                                         ) |>
                                         dplyr::group_by(flujo, año, mes) |>
                                         dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
                                         dplyr::select(flujo, año, mes, euros) |>
                                         dplyr::collect()
  )
  
  df_final_pais <- calc_series(df_pais, "pais")
  
  df_sector <- data.table::as.data.table(datas |>
                                           dplyr::filter(
                                             pais %in% 0L,
                                             cod_sector_economico %in% para$sector
                                           ) |>
                                           dplyr::group_by(flujo, año, mes) |>
                                           dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
                                           dplyr::select(flujo, año, mes, euros) |>
                                           dplyr::collect()
  )
  
  df_final_sector <- calc_series(df_sector, "sector")
  
  df_total <- data.table::as.data.table(datas |>
                                          dplyr::filter(
                                            pais %in% 0L,
                                            cod_sector_economico %in% 0L
                                          ) |>
                                          dplyr::group_by(flujo, año, mes) |>
                                          dplyr::summarise(euros = sum(euros, na.rm = TRUE), .groups = "drop") |>
                                          dplyr::select(flujo, año, mes, euros) |>
                                          dplyr::collect()
  )
  
  df_final_total <- calc_series(df_total, "tot")
  
  df_mensual <- Reduce(function(x, y) merge(x, y, by = c("año", "mes"), all = TRUE),
                       list(df_final_norm, df_final_pais, df_final_sector, df_final_total))
  
  data.table::setorder(df_mensual, año, mes)
  
  df_mensual[, `:=`(
    dif_exp = norm_exp - norm_exp_prev,
    dif_imp = norm_imp - norm_imp_prev
  )]
  
  df_anual <- df_mensual[mes %in% para$meses, 
                         lapply(.SD, sum, na.rm = TRUE), 
                         by = año, 
                         .SDcols = setdiff(names(df_mensual), c("año", "mes"))]
  
  df_mensual[, `:=`(
    tva_exp = 100 * dif_exp / norm_exp_prev,
    per_pais_exp   = 100 * norm_exp / pais_exp,
    per_sector_exp = 100 * norm_exp / sector_exp,
    per_exp        = 100 * norm_exp / tot_exp,
    con_pais_exp   = 100 * dif_exp / pais_exp_prev,
    con_sector_exp = 100 * dif_exp / sector_exp_prev,
    con_exp        = 100 * dif_exp / tot_exp_prev,
    
    tva_imp = 100 * dif_imp / norm_imp_prev,
    per_pais_imp   = 100 * norm_imp / pais_imp,
    per_sector_imp = 100 * norm_imp / sector_imp,
    per_imp        = 100 * norm_imp / tot_imp,
    con_pais_imp   = 100 * dif_imp / pais_imp_prev,
    con_sector_imp = 100 * dif_imp / sector_imp_prev,
    con_imp        = 100 * dif_imp / tot_imp_prev
  )]
  
  df_anual[, `:=`(
    tva_exp = 100 * dif_exp / norm_exp_prev,
    per_pais_exp   = 100 * norm_exp / pais_exp,
    per_sector_exp = 100 * norm_exp / sector_exp,
    per_exp        = 100 * norm_exp / tot_exp,
    con_pais_exp   = 100 * dif_exp / pais_exp_prev,
    con_sector_exp = 100 * dif_exp / sector_exp_prev,
    con_exp        = 100 * dif_exp / tot_exp_prev,
    
    tva_imp = 100 * dif_imp / norm_imp_prev,
    per_pais_imp   = 100 * norm_imp / pais_imp,
    per_sector_imp = 100 * norm_imp / sector_imp,
    per_imp        = 100 * norm_imp / tot_imp,
    con_pais_imp   = 100 * dif_imp / pais_imp_prev,
    con_sector_imp = 100 * dif_imp / sector_imp_prev,
    con_imp        = 100 * dif_imp / tot_imp_prev
  )]
  
  df_mensual[, `:=`(
    rank_exp = data.table::frank(-norm_exp, ties.method = "min"),
    rank_imp = data.table::frank(-norm_imp, ties.method = "min")
  )]
  
  df_anual[, `:=`(
    rank_exp = data.table::frank(-norm_exp, ties.method = "min"),
    rank_imp = data.table::frank(-norm_imp, ties.method = "min")
  )]
  
  df_mensual[, fecha := as.Date(paste0(año, "-", sprintf("%02d", mes), "-01"))]
  
  df_mensual[, hover_text_exp := paste0(
    "Fecha: ", format(fecha, "%Y-%m"), "<br>",
    "<b>Exportaciones: </b>", formatear_num(norm_exp / para$varfactor), " ", para$varud, "<br>",
    "Exp. Previo: ", formatear_num(norm_exp_prev / para$varfactor), " ", para$varud, "<br>",
    "Diferencia: ", formatear_num(dif_exp / para$varfactor), " ", para$varud, "<br>",
    "Ranking: ", rank_exp, "<br>",
    "TVA: ", formatear_num(tva_exp), "%<br>",
    "Contribución país: ", formatear_num(con_pais_exp), " p.p.<br>",
    "Contribución sector: ", formatear_num(con_sector_exp), " p.p.<br>",
    "Contribución total: ", formatear_num(con_exp), " p.p.<br>",
    "Peso país: ", formatear_num(per_pais_exp), "%<br>",
    "Peso sector: ", formatear_num(per_sector_exp), "%<br>",
    "Peso total: ", formatear_num(per_exp), "%"
  )]
  
  df_mensual[, hover_text_imp := paste0(
    "Fecha: ", format(fecha, "%Y-%m"), "<br>",
    "<b>Importaciones: </b>", formatear_num(norm_imp / para$varfactor), " ", para$varud, "<br>",
    "Imp. Previo: ", formatear_num(norm_imp_prev / para$varfactor), " ", para$varud, "<br>",
    "Diferencia: ", formatear_num(dif_imp / para$varfactor), " ", para$varud, "<br>",
    "Ranking: ", rank_imp, "<br>",
    "TVA: ", formatear_num(tva_imp), "%<br>",
    "Contribución país: ", formatear_num(con_pais_imp), " p.p.<br>",
    "Contribución sector: ", formatear_num(con_sector_imp), " p.p.<br>",
    "Contribución total: ", formatear_num(con_imp), " p.p.<br>",
    "Peso país: ", formatear_num(per_pais_imp), "%<br>",
    "Peso sector: ", formatear_num(per_sector_imp), "%<br>",
    "Peso total: ", formatear_num(per_imp), "%"
  )]
  
  df_anual[, hover_text_exp := paste0(
    "Año: ", año, "<br>",
    "<b>Exportaciones: </b>", formatear_num(norm_exp / para$varfactor), " ", para$varud, "<br>",
    "Exp. Previo: ", formatear_num(norm_exp_prev / para$varfactor), " ", para$varud, "<br>",
    "Diferencia: ", formatear_num(dif_exp / para$varfactor), " ", para$varud, "<br>",
    "Ranking: ", rank_exp, "<br>",
    "TVA: ", formatear_num(tva_exp), "%<br>",
    "Contribución país: ", formatear_num(con_pais_exp), " p.p.<br>",
    "Contribución sector: ", formatear_num(con_sector_exp), " p.p.<br>",
    "Contribución total: ", formatear_num(con_exp), " p.p.<br>",
    "Peso país: ", formatear_num(per_pais_exp), "%<br>",
    "Peso sector: ", formatear_num(per_sector_exp), "%<br>",
    "Peso total: ", formatear_num(per_exp), "%"
  )]
  
  df_anual[, hover_text_imp := paste0(
    "Año: ", año, "<br>",
    "<b>Importaciones: </b>", formatear_num(norm_imp / para$varfactor), " ", para$varud, "<br>",
    "Imp. Previo: ", formatear_num(norm_imp_prev / para$varfactor), " ", para$varud, "<br>",
    "Diferencia: ", formatear_num(dif_imp / para$varfactor), " ", para$varud, "<br>",
    "Ranking: ", rank_imp, "<br>",
    "TVA: ", formatear_num(tva_imp), "%<br>",
    "Contribución país: ", formatear_num(con_pais_imp), " p.p.<br>",
    "Contribución sector: ", formatear_num(con_sector_imp), " p.p.<br>",
    "Contribución total: ", formatear_num(con_imp), " p.p.<br>",
    "Peso país: ", formatear_num(per_pais_imp), "%<br>",
    "Peso sector: ", formatear_num(per_sector_imp), "%<br>",
    "Peso total: ", formatear_num(per_imp), "%"
  )]
  
  fig_mensual <- plotly::plot_ly() |>
    plotly::add_trace(data = df_mensual,
                      x = ~fecha,
                      y = ~norm_exp / 1000000,
                      name = "Exportaciones",
                      type = 'scatter',
                      mode = 'lines+markers',
                      text = ~hover_text_exp,
                      hovertemplate = '%{text}<extra></extra>',
                      line = list(color = '#2E86AB', width = 2),
                      marker = list(color = '#2E86AB', size = 6)) |>
    plotly::add_trace(data = df_mensual,
                      x = ~fecha,
                      y = ~norm_imp / 1000000,
                      name = "Importaciones",
                      type = 'scatter',
                      mode = 'lines+markers',
                      text = ~hover_text_imp,
                      hovertemplate = '%{text}<extra></extra>',
                      line = list(color = '#E94F37', width = 2),
                      marker = list(color = '#E94F37', size = 6)) |>
    plotly::layout(
      xaxis = list(title = "Fecha", showgrid = TRUE, gridcolor = 'lightgray', tickformat = "%Y-%m"),
      yaxis = list(title = "Volumen (millones de euros)", showgrid = TRUE, gridcolor = 'lightgray'),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
      hovermode = "x unified",
      separators = ",."
    )
  
  fig_anual <- plotly::plot_ly() |>
    plotly::add_trace(data = df_anual,
                      x = ~año,
                      y = ~norm_exp / 1000000,
                      name = "Exportaciones",
                      type = 'scatter',
                      mode = 'lines+markers',
                      text = ~hover_text_exp,
                      hovertemplate = '%{text}<extra></extra>',
                      line = list(color = '#2E86AB', width = 3),
                      marker = list(color = '#2E86AB', size = 8)) |>
    plotly::add_trace(data = df_anual,
                      x = ~año,
                      y = ~norm_imp / 1000000,
                      name = "Importaciones",
                      type = 'scatter',
                      mode = 'lines+markers',
                      text = ~hover_text_imp,
                      hovertemplate = '%{text}<extra></extra>',
                      line = list(color = '#E94F37', width = 3),
                      marker = list(color = '#E94F37', size = 8)) |>
    plotly::layout(
      xaxis = list(title = "Año", showgrid = TRUE, gridcolor = 'lightgray', dtick = 1),
      yaxis = list(title = "Volumen (millones de euros)", showgrid = TRUE, gridcolor = 'lightgray'),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
      hovermode = "x unified",
      separators = ",."
    )
  
  return(list(
    fig_mensual = fig_mensual,
    fig_anual = fig_anual,
    df_mensual = df_mensual,
    df_anual = df_anual
  ))
}

##### Volumenes sub -----
grafica_volumen_subsectores <- function(df, para, nmax = 8, flujo = "exp") {
  # Definir colores según si es exportación o importación
  paleta <- if (flujo == "exp") {
    c(col1 = "#2d5532", col2 = "#b4d7b4")
  } else {
    c(col1 = "#2d3535", col2 = "#b4c7d7")
  }
  
  nombre_flujo <- ifelse(flujo == "exp", "Exportaciones", "Importaciones")
  col_actual <- flujo
  col_previo <- paste0(flujo, "_prev")
  
  # Filtrar top nmax subsectores
  df_filtrado <- df[niv >= 2][order(-get(col_actual))][1:min(nmax, .N)]
  df_filtrado <- df_filtrado[order(-get(col_actual))]
  df_filtrado[, cod_sec_factor := factor(cod_sec, levels = unique(cod_sec))]
  
  # Armar etiquetas dinámicas
  nombre_actual <- paste0(nombre_flujo, " ", para$ano, " (", para$varud, ")")
  nombre_previo <- paste0(nombre_flujo, " ", para$ano - 1, " (", para$varud, ")")
  nombre_diferencia <- paste0("Diferencia absoluta (", para$varud, ")")
  
  col_dif <- paste0(flujo, "_dif")
  col_con <- paste0("con_", flujo)
  col_tva <- paste0("tva_", flujo)
  
  # Textos para los tooltips
  df_filtrado[, hover_text_actual := paste0(
    "Subector: ", nombre, "<br>",
    "Código: ", cod_sec, "<br>",
    "Región: ", para$nombre_region, "<br>",
    nombre_actual, ": ", formatear_num(get(col_actual)), "<br>",
    nombre_diferencia, ": ", format(round(get(col_dif), 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    "Tasa de variación: ", round(get(col_tva), 1), "%<br>",
    "Contribución: ", round(get(col_con), 1), " p.p."
  )]
  
  df_filtrado[, hover_text_previo := paste0(
    nombre_previo, ": ", formatear_num(get(col_previo))
  )]
  
  # Crear gráfico de barras agrupadas
  fig <- plot_ly() %>%
    add_bars(
      data = df_filtrado,
      x = ~cod_sec_factor,
      y = ~get(col_actual),
      name = as.character(para$ano),
      marker = list(color = paleta["col1"]),
      customdata = ~hover_text_actual,
      hovertemplate = '%{customdata}<extra></extra>'
    ) %>%
    add_bars(
      data = df_filtrado,
      x = ~cod_sec_factor,
      y = ~get(col_previo),
      name = as.character(para$ano - 1L),
      marker = list(color = paleta["col2"]),
      customdata = ~hover_text_previo,
      hovertemplate = '%{customdata}<extra></extra>'
    ) %>%
    layout(
      barmode = 'group',
      xaxis = list(
        title = "Subsector",
        tickfont = list(size = 10),
        showspikes = FALSE
      ),
      yaxis = list(
        title = list(
          text = paste0(nombre_flujo, " (", para$varud, ")"),
          standoff = 20
        ),
        automargin = TRUE,
        tickformat = ",.0f"
      ),
      margin = list(l = 80, b = 100),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        yanchor = "bottom",
        y = 1.02,
        xanchor = "right",
        x = 1
      ),
      hovermode = "x unified",
      hoverdistance = 20
    )
  
  return(fig)
}

grafica_volumen_paises <- function(df, para, nmax = 8, flujo = "exp") {
  # Definir colores según si es exportacion o importacion
  paleta <- if (flujo == "exp") {
    c(col1 = "#2d5532", col2 = "#b4d7b4")
  } else {
    c(col1 = "#2d3535", col2 = "#b4c7d7")
  }
  
  nombre_flujo <- ifelse(flujo == "exp", "Exportaciones", "Importaciones")
  col_actual <- flujo
  col_previo <- paste0(flujo, "_prev")
  
  # Filtrar top nmax paises
  df_filtrado <- df[cod >= 1 & cod <= 999][order(-get(col_actual))][1:min(nmax, .N)]
  df_filtrado <- df_filtrado[order(-get(col_actual))]
  
  df_filtrado[, cod_fmt := sprintf("%03d", cod)]
  df_filtrado[, cod_factor := factor(cod_fmt, levels = unique(cod_fmt))]
  
  # Armar etiquetas dinamicas
  nombre_actual <- paste0(nombre_flujo, " ", para$ano, " (", para$varud, ")")
  nombre_previo <- paste0(nombre_flujo, " ", para$ano - 1, " (", para$varud, ")")
  nombre_diferencia <- paste0("Diferencia absoluta (", para$varud, ")")
  
  col_dif <- paste0(flujo, "_dif")
  col_con <- paste0("con_", flujo)
  col_tva <- paste0("tva_", flujo)
  
  # Textos para los tooltips
  df_filtrado[, hover_text_actual := paste0(
    "Pais: ", pais, "<br>",
    "Codigo: ", cod_fmt, "<br>",
    "Region: ", para$nombre_region, "<br>",
    nombre_actual, ": ", formatear_num(get(col_actual)), "<br>",
    nombre_diferencia, ": ", format(round(get(col_dif), 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    "Tasa de variacion: ", round(get(col_tva), 1), "%<br>",
    "Contribucion: ", formatear_num(get(col_con)), " p.p."
  )]
  
  df_filtrado[, hover_text_previo := paste0(
    nombre_previo, ": ", formatear_num(get(col_previo))
  )]
  
  # Crear grafico de barras agrupadas
  fig <- plot_ly() %>%
    add_bars(
      data = df_filtrado,
      x = ~cod_factor,
      y = ~get(col_actual),
      name = as.character(para$ano),
      marker = list(color = paleta["col1"]),
      customdata = ~hover_text_actual,
      hovertemplate = '%{customdata}<extra></extra>'
    ) %>%
    add_bars(
      data = df_filtrado,
      x = ~cod_factor,
      y = ~get(col_previo),
      name = as.character(para$ano - 1L),
      marker = list(color = paleta["col2"]),
      customdata = ~hover_text_previo,
      hovertemplate = '%{customdata}<extra></extra>'
    ) %>%
    layout(
      barmode = 'group',
      xaxis = list(
        title = "Pais",
        tickfont = list(size = 10),
        showspikes = FALSE
      ),
      yaxis = list(
        title = list(
          text = paste0(nombre_flujo, " (", para$varud, ")"),
          standoff = 20
        ),
        automargin = TRUE,
        tickformat = ",.0f"
      ),
      margin = list(l = 80, b = 100),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        yanchor = "bottom",
        y = 1.02,
        xanchor = "right",
        x = 1
      ),
      hovermode = "x unified"
    )
  
  return(fig)
}

##### Contribuciones sub -----
grafica_contribuciones_subsectores <- function(df, para, nmax = 8, flujo = "exp") {
  # Definir colores según si es exportacion o importacion
  paleta <- if (flujo == "exp") {
    c(positivo = "#2d5532", negativo = "#b4d7b4")
  } else {
    c(positivo = "#2d3535", negativo = "#b4c7d7")
  }
  
  nombre_flujo <- ifelse(flujo == "exp", "Exportaciones", "Importaciones")
  col_actual <- flujo
  col_previo <- paste0(flujo, "_prev")
  col_con <- paste0("con_", flujo)
  col_dif <- paste0(flujo, "_dif")
  col_tva <- paste0("tva_", flujo)
  
  # Etiquetas dinamicas
  nombre_actual <- paste0(nombre_flujo, " ", para$ano, " (", para$varud, ")")
  nombre_previo <- paste0(nombre_flujo, " ", para$ano - 1, " (", para$varud, ")")
  nombre_diferencia <- paste0("Diferencia absoluta (", para$varud, ")")
  
  # Filtrar subsectores
  df_filtrado <- df[niv >= 2]
  
  # Obtener top nmax positivos y nmax negativos
  df_positivos <- df_filtrado[get(col_con) > 0][order(-get(col_con))][1:min(nmax, .N)]
  df_negativos <- df_filtrado[get(col_con) < 0][order(get(col_con))][1:min(nmax, .N)]
  
  df_plot <- rbind(df_positivos, df_negativos)
  df_plot <- df_plot[order(get(col_con))]
  df_plot[, nombre_factor := factor(nombre, levels = unique(nombre))]
  
  # Color condicional
  df_plot[, color := ifelse(get(col_con) > 0, paleta["positivo"], paleta["negativo"])]
  
  # Texto con valor de contribucion redondeado a un decimal
  df_plot[, text_con := format(round(get(col_con), 1), nsmall = 1)]
  
  # Textos para los tooltips
  df_plot[, hover_text := paste0(
    "Subsector: ", nombre, "<br>",
    "Region: ", para$nombre_region, "<br>",
    nombre_actual, ": ", formatear_num(get(col_actual)), "<br>",
    nombre_previo, ": ", formatear_num(get(col_previo)), "<br>",
    nombre_diferencia, ": ", format(round(get(col_dif), 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    "Tasa de variacion: ", round(get(col_tva), 1), "%<br>",
    "Contribucion: ", round(get(col_con), 1), " p.p."
  )]
  
  # Crear grafico de barras horizontales
  fig <- plot_ly() %>%
    add_bars(
      data = df_plot,
      y = ~nombre_factor,
      x = ~get(col_con),
      orientation = 'h',
      marker = list(color = ~color),
      text = ~text_con,
      textposition = "outside",
      textfont = list(color = "black", size = 11),
      customdata = ~hover_text,
      hovertemplate = '%{customdata}<extra></extra>',
      showlegend = FALSE
    ) %>%
    layout(
      xaxis = list(
        title = paste0("Contribucion a ", nombre_flujo, " (p.p.)"),
        zeroline = TRUE,
        zerolinewidth = 2,
        zerolinecolor = "black",
        automargin = TRUE
      ),
      yaxis = list(
        title = "",
        tickfont = list(size = 10),
        automargin = TRUE
      ),
      hovermode = "closest"
    )
  
  return(fig)
}

grafica_contribuciones_paises <- function(df, para, nmax = 8, flujo = "exp") {
  # Definir colores según si es exportacion o importacion
  paleta <- if (flujo == "exp") {
    c(positivo = "#2d5532", negativo = "#b4d7b4")
  } else {
    c(positivo = "#2d3535", negativo = "#b4c7d7")
  }
  
  nombre_flujo <- ifelse(flujo == "exp", "Exportaciones", "Importaciones")
  col_actual <- flujo
  col_previo <- paste0(flujo, "_prev")
  col_con <- paste0("con_", flujo)
  col_dif <- paste0(flujo, "_dif")
  col_tva <- paste0("tva_", flujo)
  
  # Etiquetas dinamicas
  nombre_actual <- paste0(nombre_flujo, " ", para$ano, " (", para$varud, ")")
  nombre_previo <- paste0(nombre_flujo, " ", para$ano - 1, " (", para$varud, ")")
  nombre_diferencia <- paste0("Diferencia absoluta (", para$varud, ")")
  
  # Filtrar paises
  df_filtrado <- df[cod >= 1 & cod <= 999]
  
  # Obtener top nmax positivos y nmax negativos
  df_positivos <- df_filtrado[get(col_con) > 0][order(-get(col_con))][1:min(nmax, .N)]
  df_negativos <- df_filtrado[get(col_con) < 0][order(get(col_con))][1:min(nmax, .N)]
  
  df_plot <- rbind(df_positivos, df_negativos)
  df_plot <- df_plot[order(get(col_con))]
  
  # Formatear codigo con 3 digitos
  df_plot[, cod_fmt := sprintf("%03d", cod)]
  df_plot[, pais_factor := factor(pais, levels = unique(pais))]
  
  # Color condicional
  df_plot[, color := ifelse(get(col_con) > 0, paleta["positivo"], paleta["negativo"])]
  
  # Texto con valor de contribucion redondeado a un decimal
  df_plot[, text_con := format(round(get(col_con), 1), nsmall = 1)]
  
  # Textos para los tooltips
  df_plot[, hover_text := paste0(
    "Pais: ", pais, "<br>",
    "Region: ", para$nombre_region, "<br>",
    nombre_actual, ": ", formatear_num(get(col_actual)), "<br>",
    nombre_previo, ": ", formatear_num(get(col_previo)), "<br>",
    nombre_diferencia, ": ", format(round(get(col_dif), 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "<br>",
    "Tasa de variacion: ", round(get(col_tva), 1), "%<br>",
    "Contribucion: ", round(get(col_con), 1), " p.p."
  )]
  
  # Crear grafico de barras horizontales
  fig <- plot_ly() %>%
    add_bars(
      data = df_plot,
      y = ~pais_factor,
      x = ~get(col_con),
      orientation = 'h',
      marker = list(color = ~color),
      text = ~text_con,
      textposition = "outside",
      textfont = list(color = "black", size = 11),
      customdata = ~hover_text,
      hovertemplate = '%{customdata}<extra></extra>',
      showlegend = FALSE
    ) %>%
    layout(
      xaxis = list(
        title = paste0("Contribucion a ", nombre_flujo, " (p.p.)"),
        zeroline = TRUE,
        zerolinewidth = 2,
        zerolinecolor = "black",
        automargin = TRUE
      ),
      yaxis = list(
        title = "",
        tickfont = list(size = 10),
        automargin = TRUE
      ),
      hovermode = "closest"
    )
  
  return(fig)
}