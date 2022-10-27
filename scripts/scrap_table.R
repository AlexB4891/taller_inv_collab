
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


