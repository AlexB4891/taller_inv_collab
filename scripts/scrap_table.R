
# ------------------------------------------------------------------------- #
#                  Web scrapping de una tabla de wikipedia                  #
# ------------------------------------------------------------------------- #


# Librerias ---------------------------------------------------------------

library(rvest)
library(tidyverse)
library(gt)
library(pins)


# Carpeta pins: -----------------------------------------------------------

carpeta <- board_folder("C:/Users/Alex/OneDrive/taller_tr_colab")



# Scrapping: --------------------------------------------------------------

# URL para el scrapping:
url <- 'https://es.wikipedia.org/wiki/Provincias_de_Ecuador'

# Lectura de la página web:
webpage <- read_html(url)

# Obtender el elemento con la tabla:
table <- html_nodes(webpage,'table.sortable')

# Obtener el cuerpo de la tabla:
tabla <- table[[1]] %>% 
  html_node("tbody") 

# Solo las celdas de la columna con la bandera:
banderas <- tabla %>% 
  html_nodes("td") %>% 
  html_nodes("span.flagicon")


# Obtenemos el atributo img de cada celda:
banderas <- banderas %>% 
  html_nodes("img") 

# Tabla del URL y el nombre de la provincia:
banderas <- tibble(
  nombre = banderas  %>% 
    html_attr("alt"),
  url = banderas  %>% 
    html_attr("src")
)

# Función para añadir las imagenes a gt()

add_flag <- function(gt_table){
  
  gt_table %>%
    text_transform(
      locations = cells_body(columns = url),
      fn = function(x) {
        web_image(
          url = x,
          height = as.numeric(20)
        )
      })
}

#Viste previa:



# Guardar insumos en {pins} ----------------------------------------------

# Tabla:

banderas %>% 
  pin_write(board = carpeta,
            name = "insumos_banderas",
            x = .,
            versioned = T)
  
# Función:

pin_write(board = carpeta,
          x = add_flag,
          name = "function_add_flag")



# Modificaciones ----------------------------------------------------------

# Galapagos no tenia capital lo trataremos de forma aislada
aislado <- banderas %>% 
  filter(str_detect(nombre,"Galáp"))

# Uso de pivot wider para las columnas de prov y capital
banderas <- banderas %>% 
  anti_join(aislado) %>% 
  mutate(indicador = rep(c("provincia","capital"),23)) %>% 
  pivot_wider(names_from = indicador,values_from = c("nombre","url")) %>% 
  unnest() 

# Algunos arreglos coquetos:
banderas <- banderas %>% 
  select(url = url_provincia,matches("nombre")) %>% 
  mutate(across(matches("nombre"),~str_remove_all(.x,"Bandera de "))) %>% 
  mutate(across(matches("nombre"),~str_remove_all(.x," \\(Ecua.*"))) %>% 
  rename_with(.cols = matches("nombre"),~str_remove(.x,"nombre_"))

# Ajustando a Galápagos:
aislado <- aislado %>% 
  rename(provincia = nombre)


# Reescribimos el pin -----------------------------------------------------

banderas %>% 
  bind_rows(aislado) %>% 
  pin_write(board = carpeta,
            name = "insumos_banderas",
            x = .,
            versioned = T,
            description = "Colocamos las provincias y capitales en distintas columnas")


