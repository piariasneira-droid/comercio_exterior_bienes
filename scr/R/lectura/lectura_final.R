#### Entorno ----
library(arrow)
library(dplyr) 

#### Configuración de Parámetros ----
territorio <- "madrid"  
unidad     <- "euros" 
clasif     <- "taric"   

#### Definición de Filtros ----
f_flujo <- 0          
f_anio  <- 2025
f_mes   <- 12

#### Ejecucción ----
##### Ruta ----
path <- sprintf("./data/interim/%s/%s_%s_%s.parquet", 
                territorio, territorio, unidad, clasif)

ds <- arrow::open_dataset(path)

##### Lectura ----
query <- ds %>%
  filter(
    flujo == f_flujo,
    año   == f_anio,
    mes   == f_mes
  ) 

df_final <- query %>% collect()