
# Generación de insumos para el reporteador -------------------------------

library(tidyverse)
library(gt)
library(patchwork)
library(scales)
library(pins)


# Elementos del pins ------------------------------------------------------

# carpeta <- board_folder("C:/Users/Alex/OneDrive/taller_tr_colab")


carpeta <- board_register_github(repo = "AlexB4891/taller_codam_2022",
                      branch = "main")


# Luctura de la base cruda ------------------------------------------------

base_cruda <- read_delim("EMPRESAS2020_periodo_2012_2020.csv", 
                         delim = ";", 
                         escape_double = FALSE, 
                         trim_ws = TRUE,
                         locale = locale(encoding = "UTF-8"))


# Mapas provinciales:

mapas <- read_rds("reporteador/shapefiles_simplificados_dpa.rds")

mapas[[1]] %>% 
  pin(board = carpeta,
      x = .,
      name = "shape_provincia",
      versioned = T,
      description = "Mapa del Ecuador por provincia")

# Ejemplo, corregir los problemas de encoding en tiempo real:

base_cruda %>% 
  select(codigo_provincia,provincia) %>% 
  unique %>% 
  pin(board = carpeta,x = .,
      name = "die_diccionario_provincias",
      versioned = T,
      description = "Diccionario provincias")
  

# Exportaciones netas por provincia

base_cruda %>% 
  group_by(anio,unidad_legal_tipo) %>% 
  summarise(exportaciones_netas = sum(exportaciones_netas,na.rm = T)) %>% 
  pin(board = carpeta,
      x = .,
      name = "die_exportaciones_netas",
      versioned = T,
      description = "Exportaciones netas por tipo")
  
# Empleo provisto por provincia 

base_cruda %>% 
  group_by(anio,unidad_legal_tipo) %>% 
  summarise(plazas_totales = sum(plazas,na.rm = T)) %>% 
  pin(board = carpeta,
      x = .,
      name = "die_plazas_trabajo",
      versioned = T,
      description = "Plazas de trabajo totales")


base_cruda <- base_cruda %>% 
  mutate(codigo_provincia = if_else(str_count(codigo_provincia) == 1,
                                    true = str_c("0",codigo_provincia),
                                    false = as.character(codigo_provincia))) 


# El mapa lo pasamos a tibble ---------------------------------------------



provin <- mapas[[1]] %>% 
  as_tibble()

provin %>% 
  pin(board = carpeta,
    x = .,
    name = "shape_provincia",
    versioned = T)


pin_versions(name = "shape_provincia",board = carpeta,full = T)



provincia_plazas <- base_cruda %>%
  group_by(anio,unidad_legal_tipo,codigo_provincia) %>%
  summarise(plazas_totales = sum(plazas,na.rm = T))


# Cambiar la label aqui: --------------------------------------------------

  pin(x = provincia_plazas,
      board = carpeta,
      name = "die_plazas_totales",
      description = "Valores provinciales de plazas",
      versioned = T)



  
