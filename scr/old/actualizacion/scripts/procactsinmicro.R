#### Funciones auxiliares ----
instala_carga_librerias <- function(librerias_necesarias) {
  # Instalación depaquetes faltantes
  for (lib in librerias_necesarias) {
    if (!requireNamespace(lib, quietly = TRUE)) {
      message("Instalando ", lib, "...")
      utils::install.packages(lib, dependencies = TRUE)
    }
  }
  
  # Cargar todos los paquetes
  invisible(lapply(librerias_necesarias, function(lib) {
    message("Cargando ", lib, "...")
    base::library(lib, character.only = TRUE)
  }))
  
  message("Carga de librerías necesarias completada.")
}


leer_csv_directorio <- function(ruta) {
  # Listar ficheros CSV
  ficheros <- list.files(
    path = ruta,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  if (length(ficheros) == 0) {
    stop("No se encontraron ficheros CSV en el directorio.")
  }
  
  # Leer y combinar
  dt <- data.table::rbindlist(
    lapply(ficheros, fread),
    fill = TRUE
  )
  
  return(dt)
}

convertir_fecha <- function(dt, col_fecha, tz = "UTC") {
  # Diccionario de meses
  meses <- c(
    "Enero" = "01", "Febrero" = "02", "Marzo" = "03", "Abril" = "04",
    "Mayo" = "05", "Junio" = "06", "Julio" = "07", "Agosto" = "08",
    "Septiembre" = "09", "Octubre" = "10", "Noviembre" = "11", "Diciembre" = "12"
  )
  
  # Nombre de la columna como símbolo
  col <- as.character(substitute(col_fecha))
  dt[, (col) := {
    mes  <- sub(" de .*", "", get(col))
    anio <- sub(".* de ", "", get(col))
    as.POSIXct(
      paste0(anio, "-", meses[mes], "-01 00:00:00"),
      format = "%Y-%m-%d %H:%M:%S",
      tz = tz
    )
  }]
  
  return(dt)
}

cargar_datos_brutos <- function(archivo, hoja, var_nombre) {
  dplyr::select(
    dplyr::mutate(
      readxl::read_xlsx(archivo, sheet = hoja),
      var = "mes",
      Fecha = as.POSIXct(Fecha, tz = "UTC")
    ),
    dplyr::all_of(c(var_nombre, "Fecha", "flujo", "var", "valor"))
  )
}

agrupar_trimestre <- function(df_ccaa, df_taric, df_paises) {
  # Función auxiliar para hacer la agrupación trimestral
  agrupar_df <- function(df) {
    df %>%
      mutate(
        Fecha = as.POSIXct(Fecha, tz = "UTC"),
        Trimestre = floor_date(Fecha, "quarter")
      ) %>%
      group_by(across(-c(Fecha, valor))) %>%
      summarise(
        valor = sum(valor, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(Fecha = Trimestre)
  }
  
  # Aplicamos la agrupación a cada dataframe
  df_ccaa_trim <- agrupar_df(df_ccaa)
  df_taric_trim <- agrupar_df(df_taric)
  df_paises_trim <- agrupar_df(df_paises)
  
  # Devolvemos los tres dataframes en una lista
  return(list(
    ccaa = df_ccaa_trim,
    taric = df_taric_trim,
    paises = df_paises_trim
  ))
}

procesa_datos <- function(df_ccaa, df_taric, df_paises, var_value) {
  
  # Función interna segura para calcular variables y completar fechas
  procesa_df <- function(df, columna, var_value, periodo = "month") {
    columna_sym <- rlang::ensym(columna)  # convertir nombre de columna a símbolo
    
    # Filtrar filas relevantes
    df <- df %>% filter(var == var_value)
    
    # Si no hay filas, retornar data frame vacío
    if (nrow(df) == 0) {
      warning("No hay datos válidos para ", deparse(substitute(df)), 
              " y var_value = ", var_value)
      return(df)
    }
    
    # Eliminar fechas NA
    df <- df %>% filter(!is.na(Fecha))
    
    # Si quedan filas válidas, aplicar pipeline de cálculos
    if (nrow(df) > 0) {
      df <- df %>%
        calculo_variables_adicionales(!!columna_sym, var_value) %>%
        calculo_contribuciones_tvs(!!columna_sym, var_value) %>%
        calculo_pesos(!!columna_sym, var_value)
      
      # Completar fechas de forma segura
      if (nrow(df) > 0) {
        df <- df %>%
          tidyr::complete(
            !!columna_sym,
            Fecha = seq.POSIXt(from = min(Fecha), to = max(Fecha), by = periodo),
            flujo,
            fill = list(valor = 0)
          )
      }
    }
    
    return(df)
  }
  
  # Procesar cada data frame con el nombre de columna correcto
  df_ccaa_amp <- procesa_df(df_ccaa, "ccaa", var_value)
  df_taric_amp <- procesa_df(df_taric, "taric", var_value)
  df_paises_amp <- procesa_df(df_paises, "pais", var_value)
  
  return(list(
    ccaa = df_ccaa_amp,
    taric = df_taric_amp,
    paises = df_paises_amp
  ))
}

calculo_pesos <- function(df, variable, var_value) {
  # Convertimos el argumento variable a una expresión
  variable_quo <- enquo(variable)
  resultados <- list()
  
  # Determinamos si los datos son mensuales o trimestrales
  is_mensual <- var_value == "mes"
  
  # Definimos los temp_values según el tipo de datos
  temp_values <- if(is_mensual) {
    c("datoper", "acumulado", "MM12")
  } else {
    c("datoper", "acumulado")
  }
  
  for(temp_value in temp_values) {
    # Calculamos el total para cada fecha y flujo
    denominador_total <- df %>%
      filter(
        var == var_value,
        temp == temp_value
      ) %>%
      group_by(Fecha, flujo) %>%
      summarise(
        valor_total = sum(valor, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Filtramos los datos para el período actual
    df_temp <- df %>%
      filter(
        var == var_value,
        temp == temp_value,
        flujo %in% c("EXPORT", "IMPORT", "SALDO")
      )
    
    # Calculamos los pesos
    df_pesos <- df_temp %>%
      left_join(
        denominador_total,
        by = c("Fecha", "flujo")
      ) %>%
      mutate(
        valor = (valor / valor_total) * 100,
        var = "peso"
      ) %>%
      select(!!variable_quo, Fecha, flujo, valor, var, temp)
    
    resultados[[temp_value]] <- df_pesos
  }
  
  # Combinamos todos los resultados con el dataframe original
  df_final <- bind_rows(df, bind_rows(resultados))
  
  return(df_final)
}

formato_mes <- function(df) {
  df %>%
    mutate(
      Mes = month(Fecha),
      Tri = quarter(Fecha),
      Año = year(Fecha)
    )
}

formato_trimestre <- function(df) {
  df %>%
    mutate(
      # Creamos el formato de trimestre aaaa-Tn
      Trimestre = paste0(
        year(Fecha), 
        "-T", 
        quarter(Fecha)
      ),
      # Añadimos las columnas de año y trimestre
      Año = year(Fecha),
      Tri = quarter(Fecha)
    ) %>%
    # Eliminamos Fecha y reordenamos
    select(-Fecha) %>%
    # Reordenamos para que Trimestre aparezca en la misma posición que estaba Fecha
    select(matches("^[^Trimestre]"), Trimestre, everything())
}

calculo_contribuciones_tvs <- function(df, variable, var_value) {
  # Convertimos el argumento variable a una expresión
  variable_quo <- enquo(variable)
  resultados <- list()
  
  # Determinamos si los datos son mensuales o trimestrales
  is_mensual <- var_value == "mes"
  
  # Definimos los temp_values según el tipo de datos
  temp_values <- if(is_mensual) {
    c("datoper", "acumulado", "MM12")
  } else {
    c("datoper", "acumulado")
  }
  
  # Definimos la función para calcular el periodo anterior
  periodo_anterior <- if(is_mensual) {
    function(x) x %m+% months(1)
  } else {
    function(x) x %m+% months(3)
  }
  
  for(temp_value in temp_values) {
    # Calculamos denominador TVA
    denominador_tva <- df %>%
      filter(
        var == var_value,
        temp == temp_value
      ) %>%
      group_by(Fecha, flujo) %>%
      summarise(
        valor = sum(valor, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Fecha = Fecha %m+% years(1)) %>%
      select(Fecha, flujo, valor_anopas = valor)
    
    # Preparamos denominador para variación del periodo (mensual o trimestral)
    if(temp_value != "acumulado") {
      denominador_periodo <- df %>%
        filter(
          var == var_value,
          temp == temp_value
        ) %>%
        group_by(Fecha, flujo) %>%
        summarise(
          valor = sum(valor, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(Fecha = periodo_anterior(Fecha)) %>%
        select(Fecha, flujo, valor_perpas = valor)
    }
    
    # Filtramos las diferencias según temp
    var_diff <- if(is_mensual) "DIFM" else "DIFT"
    df_diff <- df %>%
      filter(
        var %in% c("DIFA", if(temp_value != "acumulado") var_diff),
        temp == temp_value,
        flujo %in% c("EXPORT", "IMPORT", "SALDO")
      )
    
    # Calculamos contribuciones TVA
    df_cont_tva <- df_diff %>%
      filter(var == "DIFA") %>%
      left_join(
        denominador_tva,
        by = c("Fecha", "flujo")
      ) %>%
      mutate(
        valor = (valor / valor_anopas) * 100,
        var = "con_tva"
      ) %>%
      select(!!variable_quo, Fecha, flujo, valor, var, temp)
    
    # Calculamos contribuciones del periodo si no es acumulado
    if(temp_value != "acumulado") {
      var_cont <- if(is_mensual) "con_tvm" else "con_tvt"
      df_cont_periodo <- df_diff %>%
        filter(var == var_diff) %>%
        left_join(
          denominador_periodo,
          by = c("Fecha", "flujo")
        ) %>%
        mutate(
          valor = (valor / valor_perpas) * 100,
          var = var_cont
        ) %>%
        select(!!variable_quo, Fecha, flujo, valor, var, temp)
      
      # Combinamos TVA y variación del periodo
      df_cont <- bind_rows(df_cont_tva, df_cont_periodo)
    } else {
      df_cont <- df_cont_tva
    }
    
    resultados[[temp_value]] <- df_cont
  }
  
  # Combinamos todos los resultados con el dataframe original
  df_final <- bind_rows(df, bind_rows(resultados))
  
  return(df_final)
}

calculo_pesos_ccaa <- function(df, var_value) {
  resultados <- list()
  
  # Determinamos si los datos son mensuales o trimestrales
  is_mensual <- var_value == "mes"
  
  # Definimos los temp_values según el tipo de datos
  temp_values <- if(is_mensual) {
    c("datoper", "acumulado", "MM12")
  } else {
    c("datoper", "acumulado")
  }
  
  for(temp_value in temp_values) {
    # Filtramos los datos para el período actual
    df_temp <- df %>%
      filter(
        var == var_value,
        temp == temp_value,
        flujo %in% c("EXPORT", "IMPORT", "SALDO")
      )
    
    # Obtenemos los valores de España (denominador)
    denominador_esp <- df_temp %>%
      filter(ccaa == "España") %>%
      select(Fecha, flujo, valor_esp = valor)
    
    # Calculamos los pesos
    df_pesos <- df_temp %>%
      left_join(
        denominador_esp,
        by = c("Fecha", "flujo")
      ) %>%
      mutate(
        valor = (valor / valor_esp) * 100,
        var = "peso"
      ) %>%
      select(ccaa, Fecha, flujo, valor, var, temp)
    
    resultados[[temp_value]] <- df_pesos
  }
  
  # Combinamos todos los resultados con el dataframe original
  df_final <- bind_rows(df, bind_rows(resultados))
  
  return(df_final)
}


calculo_contribuciones_tvs_ccaa <- function(df, var_value) {
  resultados <- list()
  
  # Determinamos si los datos son mensuales o trimestrales
  is_mensual <- var_value == "mes"
  
  # Definimos los temp_values según el tipo de datos
  temp_values <- if(is_mensual) {
    c("datoper", "acumulado", "MM12")
  } else {
    c("datoper", "acumulado")
  }
  
  # Definimos la función para calcular el periodo anterior
  periodo_anterior <- if(is_mensual) {
    function(x) x %m+% months(1)
  } else {
    function(x) x %m+% months(3)
  }
  
  for(temp_value in temp_values) {
    # Calculamos denominador TVA
    denominador_tva <- df %>%
      filter(
        ccaa == "España",
        var == var_value,
        temp == temp_value
      ) %>%
      mutate(Fecha = as.POSIXct(Fecha, tz = "UTC") %m+% years(1)) %>%
      select(Fecha, flujo, valor_anopas = valor)
    
    # Preparamos denominador para variación del periodo (mensual o trimestral)
    if(temp_value != "acumulado") {
      denominador_periodo <- df %>%
        filter(
          ccaa == "España",
          var == var_value,
          temp == temp_value
        ) %>%
        mutate(Fecha = periodo_anterior(Fecha)) %>%
        select(Fecha, flujo, valor_perpas = valor)
    }
    
    # Filtramos las diferencias según temp
    var_diff <- if(is_mensual) "DIFM" else "DIFT"
    df_diff <- df %>%
      filter(
        var %in% c("DIFA", if(temp_value != "acumulado") var_diff),
        temp == temp_value,
        flujo %in% c("EXPORT", "IMPORT", "SALDO")
      )
    
    # Calculamos contribuciones TVA
    df_cont_tva <- df_diff %>%
      filter(var == "DIFA") %>%
      left_join(
        denominador_tva,
        by = c("Fecha", "flujo")
      ) %>%
      mutate(
        valor = (valor / valor_anopas) * 100,
        var = "con_tva"
      ) %>%
      select(ccaa, Fecha, flujo, valor, var, temp)
    
    # Calculamos contribuciones del periodo si no es acumulado
    if(temp_value != "acumulado") {
      var_cont <- if(is_mensual) "con_tvm" else "con_tvt"
      df_cont_periodo <- df_diff %>%
        filter(var == var_diff) %>%
        left_join(
          denominador_periodo,
          by = c("Fecha", "flujo")
        ) %>%
        mutate(
          valor = (valor / valor_perpas) * 100,
          var = var_cont
        ) %>%
        select(ccaa, Fecha, flujo, valor, var, temp)
      
      # Combinamos TVA y variación del periodo
      df_cont <- bind_rows(df_cont_tva, df_cont_periodo)
    } else {
      df_cont <- df_cont_tva
    }
    
    resultados[[temp_value]] <- df_cont
  }
  
  # Combinamos todos los resultados con el dataframe original
  df_final <- bind_rows(df, bind_rows(resultados))
  
  return(df_final)
}

calculo_variables_adicionales <- function(df, variable, var_value) {
  # Convertimos el argumento variable a una expresión
  variable_quo <- enquo(variable)
  
  # Verificamos si son datos mensuales o trimestrales
  is_mensual <- var_value == "mes"
  
  # Definimos parámetros según el tipo de datos
  periodo <- if(is_mensual) "month" else "quarter"
  var_diff <- if(is_mensual) "DIFM" else "DIFT"
  
  # Definimos la función para calcular el periodo anterior
  periodo_anterior <- if(is_mensual) {
    function(x) {
      x <- as.POSIXct(x, tz = "UTC")
      x %m+% months(1)
    }
  } else {
    function(x) {
      x <- as.POSIXct(x, tz = "UTC")
      x %m+% months(3)
    }
  }
  
  # Manipulación dataframe general
  df <- df %>% select(-var) %>%
    complete(
      !!variable_quo,
      Fecha = seq.POSIXt(
        from = min(Fecha),
        to = max(Fecha),
        by = periodo
      ),
      flujo,
      fill = list(valor = 0)
    )
  
  # PARTE 1: Cálculos básicos. Saldo y Tc
  df_saldo_tc <- df %>%
    pivot_wider(names_from = flujo, values_from = valor) %>%
    mutate(
      valor_saldo = EXPORT - IMPORT,
      valor_tc = (EXPORT / IMPORT) * 100
    ) %>%
    pivot_longer(
      cols = c(valor_saldo, valor_tc),
      names_to = "temp_name",
      values_to = "valor"
    ) %>%
    mutate(
      var = var_value,
      temp = "datoper",
      flujo = case_when(
        temp_name == "valor_saldo" ~ "SALDO",
        temp_name == "valor_tc" ~ "TC"
      )
    ) %>%
    select(names(df), var, temp)
  
  # Dataframes auxiliares
  df_ano_pasado <- df %>%
    bind_rows(df_saldo_tc %>% select(-var, -temp)) %>%
    mutate(Fecha = Fecha %m+% years(1)) %>%
    rename(valor_anopas = valor)
  
  df_periodo_pasado <- df %>%
    bind_rows(df_saldo_tc %>% select(-var, -temp)) %>%
    mutate(Fecha = periodo_anterior(Fecha)) %>%
    rename(valor_perpas = valor)
  
  df_saldo_ano_pasado <- df_saldo_tc %>%
    filter(flujo == "SALDO") %>%
    mutate(Fecha = Fecha %m+% years(1)) %>%
    rename(valor_anopas = valor)
  
  df_saldo_periodo_pasado <- df_saldo_tc %>%
    filter(flujo == "SALDO") %>%
    mutate(Fecha = periodo_anterior(Fecha)) %>%
    rename(valor_perpas = valor)
  
  # Calculamos TVA
  df_tva <- df %>%
    inner_join(df_ano_pasado, 
               by = c(setdiff(names(df), c("valor", "Fecha")), "Fecha")) %>%
    mutate(
      valor = (valor / valor_anopas - 1) * 100,
      var = "TVA",
      temp = "datoper"
    ) %>%
    select(names(df), temp, var)
  
  # Calculamos variación del periodo (TVM o TVT)
  df_tvp <- df %>%
    inner_join(df_periodo_pasado,
               by = c(setdiff(names(df), c("valor", "Fecha")), "Fecha")) %>%
    mutate(
      valor = (valor / valor_perpas - 1) * 100,
      var = if(is_mensual) "TVM" else "TVT",
      temp = "datoper"
    ) %>%
    select(names(df), temp, var)
  
  # Calculamos DIFA
  df_difa <- bind_rows(
    df %>%
      inner_join(df_ano_pasado, 
                 by = c(setdiff(names(df), c("valor", "Fecha")), "Fecha")) %>%
      mutate(
        valor = valor - valor_anopas,
        var = "DIFA",
        temp = "datoper"
      ),
    df_saldo_tc %>%
      filter(flujo == "SALDO") %>%
      inner_join(df_saldo_ano_pasado,
                 by = c(setdiff(names(df), c("valor", "Fecha")), "Fecha")) %>%
      mutate(
        valor = valor - valor_anopas,
        var = "DIFA",
        temp = "datoper"
      )
  ) %>%
    select(names(df), temp, var)
  
  # Calculamos diferencia del periodo (DIFM o DIFT)
  df_difp <- bind_rows(
    df %>%
      inner_join(df_periodo_pasado,
                 by = c(setdiff(names(df), c("valor", "Fecha")), "Fecha")) %>%
      mutate(
        valor = valor - valor_perpas,
        var = var_diff,
        temp = "datoper"
      ),
    df_saldo_tc %>%
      filter(flujo == "SALDO") %>%
      inner_join(df_saldo_periodo_pasado,
                 by = c(setdiff(names(df), c("valor", "Fecha")), "Fecha")) %>%
      mutate(
        valor = valor - valor_perpas,
        var = var_diff,
        temp = "datoper"
      )
  ) %>%
    select(names(df), temp, var)
  
  # PARTE 2: Cálculos para acumulados
  df_acum <- df %>%
    mutate(año = year(Fecha)) %>%
    group_by(across(c(-Fecha, -valor))) %>%
    mutate(valor = cumsum(valor)) %>%
    ungroup() %>%
    select(-año)
  
  df_saldo_tc_acum <- df_acum %>%
    pivot_wider(names_from = flujo, values_from = valor) %>%
    mutate(
      valor_saldo = EXPORT - IMPORT,
      valor_tc = (EXPORT / IMPORT) * 100
    ) %>%
    pivot_longer(
      cols = c(valor_saldo, valor_tc),
      names_to = "temp_name",
      values_to = "valor"
    ) %>%
    mutate(
      var = var_value,
      temp = "acumulado",
      flujo = case_when(
        temp_name == "valor_saldo" ~ "SALDO",
        temp_name == "valor_tc" ~ "TC"
      )
    ) %>%
    select(names(df), var, temp)
  
  df_ano_pasado_acum <- df_acum %>%
    bind_rows(df_saldo_tc_acum %>% select(-var, -temp)) %>%
    mutate(Fecha = Fecha %m+% years(1)) %>%
    rename(valor_anopas = valor)
  
  df_saldo_ano_pasado_acum <- df_saldo_tc_acum %>%
    filter(flujo == "SALDO") %>%
    mutate(Fecha = Fecha %m+% years(1)) %>%
    rename(valor_anopas = valor)
  
  df_tva_acum <- df_acum %>%
    inner_join(df_ano_pasado_acum,
               by = c(setdiff(names(df), c("valor", "Fecha")), "Fecha")) %>%
    mutate(
      valor = (valor / valor_anopas - 1) * 100,
      var = "TVA",
      temp = "acumulado"
    ) %>%
    select(names(df), temp, var)
  
  df_difa_acum <- bind_rows(
    df_acum %>%
      inner_join(df_ano_pasado_acum,
                 by = c(setdiff(names(df), c("valor", "Fecha")), "Fecha")) %>%
      mutate(
        valor = valor - valor_anopas,
        var = "DIFA",
        temp = "acumulado"
      ),
    df_saldo_tc_acum %>%
      filter(flujo == "SALDO") %>%
      inner_join(df_saldo_ano_pasado_acum,
                 by = c(setdiff(names(df), c("valor", "Fecha")), "Fecha")) %>%
      mutate(
        valor = valor - valor_anopas,
        var = "DIFA",
        temp = "acumulado"
      )
  ) %>%
    select(names(df), temp, var)
  
  # PARTE 3: Medias móviles (solo para datos mensuales)
  if(is_mensual) {
    df_mm12 <- df %>%
      group_by(!!variable_quo, flujo) %>%
      arrange(Fecha) %>%
      mutate(
        valor = rollmean(valor, k = 12, fill = NA, align = "right"),
        var = var_value,
        temp = "MM12"
      ) %>%
      ungroup() %>%
      select(names(df), temp, var)
    
    df_saldo_tc_mm12 <- df_mm12 %>%
      pivot_wider(names_from = flujo, values_from = valor) %>%
      mutate(
        valor_saldo = EXPORT - IMPORT,
        valor_tc = (EXPORT / IMPORT) * 100
      ) %>%
      pivot_longer(
        cols = c(valor_saldo, valor_tc),
        names_to = "temp_name",
        values_to = "valor"
      ) %>%
      mutate(
        var = var_value,
        temp = "MM12",
        flujo = case_when(
          temp_name == "valor_saldo" ~ "SALDO",
          temp_name == "valor_tc" ~ "TC"
        )
      ) %>%
      select(names(df), var, temp)
    
    df_ano_pasado_mm12 <- df_mm12 %>%
      bind_rows(df_saldo_tc_mm12 %>% select(-var, -temp)) %>%
      mutate(Fecha = Fecha %m+% years(1)) %>%
      rename(valor_anopas = valor)
    
    df_periodo_pasado_mm12 <- df_mm12 %>%
      bind_rows(df_saldo_tc_mm12 %>% select(-var, -temp)) %>%
      mutate(Fecha = periodo_anterior(Fecha)) %>%
      rename(valor_perpas = valor)
    
    df_saldo_ano_pasado_mm12 <- df_saldo_tc_mm12 %>%
      filter(flujo == "SALDO") %>%
      mutate(Fecha = Fecha %m+% years(1)) %>%
      rename(valor_anopas = valor)
    
    df_saldo_periodo_pasado_mm12 <- df_saldo_tc_mm12 %>%
      filter(flujo == "SALDO") %>%
      mutate(Fecha = periodo_anterior(Fecha)) %>%
      rename(valor_perpas = valor)
    
    df_tva_mm12 <- df_mm12 %>%
      inner_join(df_ano_pasado_mm12,
                 by = c(setdiff(names(df), c("valor", "Fecha")), "Fecha")) %>%
      mutate(
        valor = (valor / valor_anopas - 1) * 100,
        var = "TVA",
        temp = "MM12"
      ) %>%
      select(names(df), temp, var)
    
    df_tvp_mm12 <- df_mm12 %>%
      inner_join(df_periodo_pasado_mm12,
                 by = c(setdiff(names(df), c("valor", "Fecha")), "Fecha")) %>%
      mutate(
        valor = (valor / valor_perpas - 1) * 100,
        var = if(is_mensual) "TVM" else "TVT",
        temp = "MM12"
      ) %>%
      select(names(df), temp, var)
    
    df_difa_mm12 <- bind_rows(
      df_mm12 %>%
        inner_join(df_ano_pasado_mm12,
                   by = c(setdiff(names(df), c("valor", "Fecha")), "Fecha")) %>%
        mutate(
          valor = valor - valor_anopas,
          var = "DIFA",
          temp = "MM12"
        ),
      df_saldo_tc_mm12 %>%
        filter(flujo == "SALDO") %>%
        inner_join(df_saldo_ano_pasado_mm12,
                   by = c(setdiff(names(df), c("valor", "Fecha")), "Fecha")) %>%
        mutate(
          valor = valor - valor_anopas,
          var = "DIFA",
          temp = "MM12"
        )
    ) %>%
      select(names(df), temp, var)
    
    df_difp_mm12 <- bind_rows(
      df_mm12 %>%
        inner_join(df_periodo_pasado_mm12,
                   by = c(setdiff(names(df), c("valor", "Fecha")), "Fecha")) %>%
        mutate(
          valor = valor - valor_perpas,
          var = var_diff,
          temp = "MM12"
        ),
      df_saldo_tc_mm12 %>%
        filter(flujo == "SALDO") %>%
        inner_join(df_saldo_periodo_pasado_mm12,
                   by = c(setdiff(names(df), c("valor", "Fecha")), "Fecha")) %>%
        mutate(
          valor = valor - valor_perpas,
          var = var_diff,
          temp = "MM12"
        )
    ) %>%
      select(names(df), temp, var)
  }
  
  # Combinamos todos los dataframes
  df_list <- list(
    df %>% mutate(var = var_value, temp = "datoper"),
    df_saldo_tc,
    df_tva,
    df_tvp,
    df_difa,
    df_difp,
    df_acum %>% mutate(var = var_value, temp = "acumulado"),
    df_saldo_tc_acum,
    df_tva_acum,
    df_difa_acum
  )
  
  if(is_mensual) {
    df_list <- c(df_list, list(
      df_mm12 %>% filter(!is.na(valor)),
      df_saldo_tc_mm12 %>% filter(!is.na(valor)),
      df_tva_mm12 %>% filter(!is.na(valor)),
      df_tvp_mm12 %>% filter(!is.na(valor)),
      df_difa_mm12 %>% filter(!is.na(valor)),
      df_difp_mm12 %>% filter(!is.na(valor))
    ))
  }
  
  df_combinado <- bind_rows(df_list)
  
  return(df_combinado)
}

crear_datos_anuales <- function(df) {
  df_anual <- df %>%
    mutate(Fecha = as.Date(Fecha)) %>%
    filter(
      month(Fecha) == 12,
      temp == "acumulado"
    ) %>%
    mutate(
      temp = "datoper",
      var = ifelse(var == "mes", "ano", var),
      Año = year(Fecha)
    ) %>%
    select(-Fecha, -Mes, -Tri) %>%  # Eliminamos Fecha, Mes y Tri
    select(matches("^[^Año]"), Año, everything())
  
  return(df_anual)
}

#### Guardado csvs ----
guardar_datos_procesados <- function(df_ccaa_amp, df_taric_amp, df_paises_amp,
                                     df_ccaa_trim_amp, df_taric_trim_amp, df_paises_trim_amp,
                                     df_ccaa_anos_amp, df_taric_anos_amp, df_paises_anos_amp,
                                     path) {
  
  # Crear directorio si no existe
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  
  # Guardar datos mensuales
  data.table::fwrite(df_ccaa_amp, file.path(path, "df_ccaa_mes_amp.csv"))
  data.table::fwrite(df_taric_amp, file.path(path, "df_taric_mes_amp.csv"))
  data.table::fwrite(df_paises_amp, file.path(path, "df_paises_mes_amp.csv"))
  
  # Guardar datos trimestrales
  data.table::fwrite(df_ccaa_trim_amp, file.path(path, "df_ccaa_trim_amp.csv"))
  data.table::fwrite(df_taric_trim_amp, file.path(path, "df_taric_trim_amp.csv"))
  data.table::fwrite(df_paises_trim_amp, file.path(path, "df_paises_trim_amp.csv"))
  
  # Guardar datos anuales
  data.table::fwrite(df_ccaa_anos_amp, file.path(path, "df_ccaa_anos_amp.csv"))
  data.table::fwrite(df_taric_anos_amp, file.path(path, "df_taric_anos_amp.csv"))
  data.table::fwrite(df_paises_anos_amp, file.path(path, "df_paises_anos_amp.csv"))
  
  # Mensaje de finalización
  message("Procesamiento completado con éxito")
  message(paste0("Archivos CSV guardados en: '", path, "'"))
  
  return(invisible(TRUE))
}