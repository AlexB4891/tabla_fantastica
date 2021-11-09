
# ------------------------------------------------------------------------- #
#                   Extacción de imagenes para los inputs                   #
# ------------------------------------------------------------------------- #


# librerias ---------------------------------------------------------------

library(tidyverse)
library(rvest)

# Leer la pagina web y usar sus elementos
pagina <- read_html("https://es.wikipedia.org/wiki/Anexo:Presidentes_del_Ecuador")

# Extraer todos los elementos de una página web que están dentro de la tag "table"
imagenes <-
  pagina %>% 
  html_nodes("table") %>%
  .[2] 

# Convertir la tabla de "xml" a data.frame
tabla <- imagenes %>% 
  html_table()


img <- imagenes %>% 
  html_nodes("tr")  %>% 
  map(~.x %>% html_node("img")) %>%
  map(~.x %>% html_attr("src")) %>% 
  unlist() %>% 
  .[2:121]



tabla <- tabla[[1]][c(-1,-2,-9)] %>% 
  mutate(img = img,
         nombre_del_presidente = str_remove(string = Presidencia,pattern = "\\(.*|[:digit:].*"))  
# %>% 
#     select(img,nombre_del_presidente)


imagenes_ecuador <- tabla %>% 
  filter(complete.cases(.))

imagenes_ecuador %>% 
  transpose() %>% 
  walk(~{
    
    ext <- str_extract(.x$img,"\\..{3}$")
    
    dest_file <- str_c("imagenes/Ecuador/",.x$nombre_del_presidente,ext)
    
    # browser()
    
    url_file <- str_c("https:",.x$img)
    
    download.file(url = url_file,destfile = dest_file,method = "curl")
    
  })


# FUNCION PARA EXTRAER IMAG -----------------------------------------------

extraer_img_presid <- function(url, posicion_tabla){
  
  paginas <- read_html(url)
  
  # Extraer todos los elementos de una página web que están dentro de la tag "table"
  imagen <- paginas %>% html_nodes("table") %>%
    .[posicion_tabla]
  
  # Convertir la tabla de "xml" a data.frame
  tabla <- imagen %>% html_table()
  
  return(tabla)
}

