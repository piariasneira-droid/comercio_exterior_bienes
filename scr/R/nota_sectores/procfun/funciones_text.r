# Funciones limpiar memeoria ----
.limpiar_memoria <- function() {
  conservar_fns <- ls(envir = .GlobalEnv)[
    sapply(ls(envir = .GlobalEnv), function(x) is.function(get(x, envir = .GlobalEnv)))
  ]
  rm(list = setdiff(ls(envir = .GlobalEnv), c(.conservar, conservar_fns)), envir = .GlobalEnv)
  gc(verbose = FALSE)
}

# Objetos a conservar en memoria 
.conservar <- c(
  
  # Parámetros y utilidades
  "paramets",
  ".conservar",
  "dsmad",
  "dsesp",
  "meta_sec",
  "meta_pais",
  
  # Datos de main_etl 
  "df_sectores",
  "df_paises",
  "df_sec",
  "df_country",
  "df_evol_sec",
  "df_evol_pais",
  "df_evol_secfull",
  "df_evol_countryfull",
  "df_ccaas",
  "df_ccaa_amp",
  "lista_esp",
  "lista_mad",
  "totalesanho",
  
  # Procesamiento acumulado
  "df_sectores_acu",
  "df_paises_acu",
  "df_sec_acu",
  "df_country_acu",
  "df_evol_sec_acu",
  "df_evol_pais_acu",
  "df_evol_secfull_acu",
  "df_evol_countryfull_acu",
  
  # Procesamiento contribuciones datacomex
  "df_contrib_sec_exp_informe", 
  "df_contrib_sec_imp_informe", 
  "df_contrib_paises_exp_informe", 
  "df_contrib_paises_imp_informe", 
  
  # Procesamiento contribuciones datacomex - España mes
  "df_contrib_sec_exp_informe_esp",
  "df_contrib_sec_imp_informe_esp",
  "df_contrib_paises_exp_informe_esp",
  "df_contrib_paises_imp_informe_esp",
  
  # Procesamiento contribuciones datacomex - Madrid acumulado
  "df_contrib_sec_exp_informe_acu",
  "df_contrib_sec_imp_informe_acu",
  "df_contrib_paises_exp_informe_acu",
  "df_contrib_paises_imp_informe_acu",
  
  # Procesamiento contribuciones datacomex - España acumulado
  "df_contrib_sec_exp_informe_esp_acu",
  "df_contrib_sec_imp_informe_esp_acu",
  "df_contrib_paises_exp_informe_esp_acu",
  "df_contrib_paises_imp_informe_esp_acu",
  
  # Procesamiento contribuciones datacomex - Madrid año pasado
  "df_contrib_sec_exp_informe_anopas",
  "df_contrib_sec_imp_informe_anopas",
  "df_contrib_paises_exp_informe_anopas",
  "df_contrib_paises_imp_informe_anopas",
  
  # Procesamiento contribuciones datacomex - España año pasado
  "df_contrib_sec_exp_informe_esp_anopas",
  "df_contrib_sec_imp_informe_esp_anopas",
  "df_contrib_paises_exp_informe_esp_anopas",
  "df_contrib_paises_imp_informe_esp_anopas",
  
  # Procesamiento año pasado
  "df_sectores_anopas",
  "df_paises_anopas",
  "df_sec_anopas",
  "df_country_anopas",
  "df_evol_sec_anopas",
  "df_evol_pais_anopas",
  "df_evol_secfull_anopas",
  "df_evol_countryfull_anopas",
  
  # Plots treemap mes — main_phtmls 
  "treemap_exp_mad_sec",
  "treemap_exp_mad_pais",
  "treemap_imp_mad_sec",
  "treemap_imp_mad_pais",
  "treemap_exp_esp_sec",
  "treemap_exp_esp_pais",
  "treemap_imp_esp_sec",
  "treemap_imp_esp_pais",
  
  # Plots treemap acumulado — main_phtmls 
  "treemap_exp_mad_sec_acu",
  "treemap_exp_mad_pais_acu",
  "treemap_imp_mad_sec_acu",
  "treemap_imp_mad_pais_acu",
  "treemap_exp_esp_sec_acu",
  "treemap_exp_esp_pais_acu",
  "treemap_imp_esp_sec_acu",
  "treemap_imp_esp_pais_acu",
  
  # Plots treemap año pasado — main_phtmls 
  "treemap_exp_mad_sec_anopas",
  "treemap_exp_mad_pais_anopas",
  "treemap_imp_mad_sec_anopas",
  "treemap_imp_mad_pais_anopas",
  "treemap_exp_esp_sec_anopas",
  "treemap_exp_esp_pais_anopas",
  "treemap_imp_esp_sec_anopas",
  "treemap_imp_esp_pais_anopas",
  
  # Plots quarto
  "plot_mad_evo_mes",
  "plot_mad_exp_mm12",
  "plot_mad_imp_mm12",
  "plot_mad_exp_anos",
  "plot_mad_imp_anos",
  "plot_mad_mm12_anos",
  
  # Plots volumen mes — main_phtmls 
  "vol_exp_mad_sec",
  "vol_exp_mad_pais",
  "vol_imp_mad_sec",
  "vol_imp_mad_pais",
  "vol_exp_esp_sec",
  "vol_exp_esp_pais",
  "vol_imp_esp_sec",
  "vol_imp_esp_pais",
  
  # Plots volumen acumulado — main_phtmls 
  "vol_exp_mad_sec_acu",
  "vol_exp_mad_pais_acu",
  "vol_imp_mad_sec_acu",
  "vol_imp_mad_pais_acu",
  "vol_exp_esp_sec_acu",
  "vol_exp_esp_pais_acu",
  "vol_imp_esp_sec_acu",
  "vol_imp_esp_pais_acu",
  
  # Plots volumen año pasado — main_phtmls 
  "vol_exp_mad_sec_anopas",
  "vol_exp_mad_pais_anopas",
  "vol_imp_mad_sec_anopas",
  "vol_imp_mad_pais_anopas",
  "vol_exp_esp_sec_anopas",
  "vol_exp_esp_pais_anopas",
  "vol_imp_esp_sec_anopas",
  "vol_imp_esp_pais_anopas",
  
  # Plots contribuciones mes — main_phtmls 
  "contrib_exp_mad_sec",
  "contrib_exp_mad_pais",
  "contrib_imp_mad_sec",
  "contrib_imp_mad_pais",
  "contrib_exp_esp_sec",
  "contrib_exp_esp_pais",
  "contrib_imp_esp_sec",
  "contrib_imp_esp_pais",
  
  # Plots contribuciones acumulado — main_phtmls 
  "contrib_exp_mad_sec_acu",
  "contrib_exp_mad_pais_acu",
  "contrib_imp_mad_sec_acu",
  "contrib_imp_mad_pais_acu",
  "contrib_exp_esp_sec_acu",
  "contrib_exp_esp_pais_acu",
  "contrib_imp_esp_sec_acu",
  "contrib_imp_esp_pais_acu",
  
  # Plots contribuciones año pasado — main_phtmls 
  "contrib_exp_mad_sec_anopas",
  "contrib_exp_mad_pais_anopas",
  "contrib_imp_mad_sec_anopas",
  "contrib_imp_mad_pais_anopas",
  "contrib_exp_esp_sec_anopas",
  "contrib_exp_esp_pais_anopas",
  "contrib_imp_esp_sec_anopas",
  "contrib_imp_esp_pais_anopas",
  
  # Bump charts mes — main_phtmls 
  "bump_exp_mad_paises",
  "bump_exp_mad_sec",
  "bump_imp_mad_paises",
  "bump_imp_mad_sec",
  "bump_exp_esp_paises",
  "bump_exp_esp_sec",
  "bump_imp_esp_paises",
  "bump_imp_esp_sec",
  
  # Bump charts acumulado — main_phtmls 
  "bump_exp_mad_paises_acu",
  "bump_exp_mad_sec_acu",
  "bump_imp_mad_paises_acu",
  "bump_imp_mad_sec_acu",
  "bump_exp_esp_paises_acu",
  "bump_exp_esp_sec_acu",
  "bump_imp_esp_paises_acu",
  "bump_imp_esp_sec_acu",
  
  # Bump charts año pasado — main_phtmls 
  "bump_exp_mad_paises_anopas",
  "bump_exp_mad_sec_anopas",
  "bump_imp_mad_paises_anopas",
  "bump_imp_mad_sec_anopas",
  "bump_exp_esp_paises_anopas",
  "bump_exp_esp_sec_anopas",
  "bump_imp_esp_paises_anopas",
  "bump_imp_esp_sec_anopas",
  
  # Contribuciones datacomex ----
  "df_contrib_sec_exp_informe",
  "df_contrib_sec_imp_informe",
  "df_contrib_paises_exp_informe",
  "df_contrib_paises_imp_informe",
  
  # Tablas 
  "ft_ccaa",
  
  # Textos
  "mes_label",
  "fecha_hoy",
  "custom_theme",
  "sufijo_mes",
  "sufijo_ytm",
  "sufijo_anopas",
  
  # Tablas GT datacomex — main_tablas ----
  "tbl_sec_mad",
  "tbl_sec_esp",
  "tbl_pais_mad",
  "tbl_pais_esp",
  
  # Tablas GT evolución — main_tablas ----
  "tbl_evol_sec_mad_exp",
  "tbl_evol_sec_mad_imp",
  "tbl_evol_sec_esp_exp",
  "tbl_evol_sec_esp_imp",
  "tbl_evol_pais_mad_exp",
  "tbl_evol_pais_mad_imp",
  "tbl_evol_pais_esp_exp",
  "tbl_evol_pais_esp_imp",
  
  # Tablas GT evolución porcentual — main_tablas ----
  "tbl_evol_pct_sec_exp",
  "tbl_evol_pct_sec_imp",
  "tbl_evol_pct_pais_exp",
  "tbl_evol_pct_pais_imp"
)


# Formatos ----
.fmt_num_inf <- function(x, dec = 1L) {
  formatC(round(x, dec), format = "f", digits = dec,
          big.mark = ".", decimal.mark = ",")
}

.fmt_num <- function(x, varfactor = 1e6, dec = 1L) {
  formatC(round(x / varfactor, dec), format = "f", digits = dec,
          big.mark = ".", decimal.mark = ",")
}

.fmt_pct <- function(x, dec = 1L) {
  formatC(round(x, dec), format = "f", digits = dec,
          big.mark = ".", decimal.mark = ",")
}

.fmt_pp <- function(x, dec = 1L) {
  formatC(round(x, dec), format = "f", digits = dec,
          big.mark = ".", decimal.mark = ",")
}


.fmt_mna <- function(m) {
  meses_es <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
                "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  return(meses_es[as.integer(m)])
}

.fmt_per <- function(m, y) {
  m <- as.integer(m)
  if (m == 1) {
    return(paste0("enero de ", y))
  } else {
    return(paste0("enero-", .fmt_mna(m), " de ", y))
  }
}

.per_label <- function(m, y) {
  m <- as.integer(m)
  if (m == 3) {
    return(paste0("acumulado del primer trimestre de ", y))
    
  } else if (m == 6) {
    return(paste0("acumulado del primer semestre de ", y))
    
  } else if (m == 12) {
    return(paste0("acumulado del año ", y))
    
  } else {
    return(.fmt_per(m, y))
  }
}

# Labels
.build_period_labels <- function(para) {
  
  meses_es <- c(
    "ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO",
    "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE"
  )
  
  m   <- para$mes   # integer scalar or vector
  ano <- para$anho    # integer year
  
  m_min <- min(m)
  m_max <- max(m)
  
  if (m_min == m_max) {
    mes_label <- paste0(meses_es[m_min], " DE ", ano)
    acu_label <- paste0("ACUMULADO A ", meses_es[m_max], " DE ", ano)
  } else {
    mes_label <- paste0(meses_es[m_min], "-", meses_es[m_max], " DE ", ano)
    acu_label <- paste0("ACUMULADO A ", meses_es[m_max], " DE ", ano)
  }
  
  list(mes_label = mes_label, acu_label = acu_label)
}

