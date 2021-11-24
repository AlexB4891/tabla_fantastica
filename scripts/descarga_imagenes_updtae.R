
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
  #Scrape the website for the url
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



c("Argentina", "Bolivia", "Brasil", 
  "Chile", "Colombia", "Costa Rica", 
  "Cuba", "Ecuador", "El Salvador",
  "Guatemala", "Honduras", "México", 
  "Nicaragua", "Panama", "Paraguay", 
  "Peru", "Puerto Rico", "Republica Dominicana", 
  "Uruguay", "Venezuela", "Venezuela") %>% 
  str_c("imagenes/",.) %>% walk(.f = ~dir.create(.x))

# dir.create("nueva_carpeta")

# Llamar a la base completa de gasto social
# Gasto_social <- read_rds("tablas_intermedias/indicador_gasto_social.rds")



funcion_extr_img <- function(base){
  
  imagenes_pais <- base %>%
    filter(!is.na(img))
  
  imagenes_pais %>% 
    transpose() %>% 
    walk(~{
      
      ext <- str_extract(.x$img,"\\..{3,4}$")
      
      # crear ruta: "imagenes/Pais/Presi.jpg" 
      dest_file <- str_c("imagenes/",.x$pais,"/",.x$nombre_del_presidente, ext)
      
      
      safely(download.file)(url = .x$img, destfile = dest_file, method = "curl")
      
    }) 
}

# funcion_extr_img(base = Argentina)


# list(Argentina, Bolivia, Brasil, 
#   Chile, Colombia, Costa_Rica, 
#   Cuba, Ecuador, Salvador, 
#   Guatemala, Honduras, Mexico, 
#   Nicaragua, Panama, Paraguay, 
#   Peru, Puerto_Rico, Republica_Dominicana,
#   Uruguay, Venezuela) %>% 
  
read_rds("~/tabla_fantastica/tablas_intermedias/indicador_gasto_social.rds") %>% 
  split(.$pais) %>% 
  walk(.f = ~funcion_extr_img(.x))






