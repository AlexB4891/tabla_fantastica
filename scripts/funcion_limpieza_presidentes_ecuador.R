library(tidyverse)
library(rvest)
library(lubridate)
# Lectura de la página web:

pagina_presidentes <- read_html("https://es.wikipedia.org/wiki/Anexo:Presidentes_del_Ecuador")

class(pagina_presidentes)

# Extraer todos los elementos de una página web que están dentro de la tag "table"

tablas_html <- pagina_presidentes %>% 
  html_nodes("table") 

# Convertir la tabla de "xml" a un bonito data.frame

tabla_final <- tablas_html[[2]] %>% 
  html_table()

# Si tuviera más tablas:

map(.x = tablas_html,.f = html_table)

tabla_final <- tabla_final %>% 
  select(-c(1,2,9))

# tabla_final_tidy <- 
  tabla_final %>% 
  mutate(
    temporal_1 = str_extract(string = Presidencia,pattern = "\\(.*|[:digit:].*"),
    nombre_del_presidente = str_remove(string = Presidencia,pattern = "\\(.*|[:digit:].*"),
    edad = str_extract(string = temporal_1,pattern = "[:digit:]{2}[:space:]a.os"),
    periodo_presidencia = str_split(string = Período,pattern = "-"),
    inicio = map(.x = periodo_presidencia,~.x[1]),
    fin = map(.x = periodo_presidencia,~.x[2]),
    across(.cols = c(inicio,fin),
           .fns = ~ str_remove(string = .x,pattern = "\\[.\\]")),
    across(.cols = c(inicio,fin),
           .fns = ~ str_remove_all(string = .x,pattern = "de[:space:]")),
    across(.cols = c(inicio,fin),
           .fns = ~ parse_date(.x,"%d %B %Y",locale=locale("es")))
  ) %>% 
    select()

class(tabla_final_tidy$edad)
class(tabla_final_tidy$periodo_presidencia)

tabla_final_tidy %>% 
  unnest(cols = periodo_presidencia) %>% View



# 1. Armar una lista de urls
# 2. Crear un vector con la posicion de la tabl despues de sacar los nodos

extraer_tabla_presidentes <- function(url,posicion_tabla){
  
  pagina_presidentes <- read_html(url)
  
  class(pagina_presidentes)
  
  # Extraer todos los elementos de una página web que están dentro de la tag "table"
  
  tablas_html <- pagina_presidentes %>% 
    html_nodes("table") 
  
  # Convertir la tabla de "xml" a un bonito data.frame
  
  tabla_final <- tablas_html[[posicion_tabla]] %>% 
    html_table()

  
  # Demas operaciones hasta tener la tabla tidy
    
  return(tabla_tidy)
}

urls <- c("https://es.wikipedia.org/wiki/Anexo:Gobernantes_de_Venezuela",
          "https://es.wikipedia.org/wiki/Anexo:Gobernantes_de_boliia",
          "https://es.wikipedia.org/wiki/Anexo:Ecuador")

posiciones <- c(4,3,2)
          

map2(.x = urls,.y = posiciones,.f = extraer_tabla_presidentes)

tabla_final_tidy %>% mutate(tamano = map(periodo_presidencia,length)) %>% count(tamano)
