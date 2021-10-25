library(tidyverse)
library(rvest)
library(lubridate)


# Limpiar base para Ecuador -----------------------------------------------

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
#
# map(.x = tablas_html,.f = html_table)


# Eliminar los nombres repetidos

tabla_final <- tabla_final %>% 
  select(-c(1,2,9))

 tabla_final_tidy <- tabla_final %>% 
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
  )
 

# Base Ecuador ------------------------------------------------------------
 
 Ecuador <- tabla_final_tidy %>%
    select(nombre_del_presidente, inicio, fin)
 
 Ecuador %>% filter()

 
 # Funciones auxiliares que no se usaron
# class(tabla_final_tidy$edad)
# class(tabla_final_tidy$periodo_presidencia)
# 
# tabla_final_tidy %>% 
#   unnest(cols = periodo_presidencia) %>% View
# tabla_final_tidy %>% mutate(tamano = map(periodo_presidencia,length)) %>% count(tamano)


# Funcion para sacar tablas de los paises ---------------------------------

# 1. Armar una lista de urls
# 2. Crear un vector con la posicion de la tabla despues de sacar los nodos

extraer_tabla_presidentes <- function(url, posicion_tabla){
  
  pagina_presidentes <- read_html(url)
  
  # Extraer todos los elementos de una página web que están dentro de la tag "table"
  tablas_html <- pagina_presidentes %>% html_nodes("table") 
  
  # Convertir la tabla de "xml" a un bonito data.frame
  tabla_final <- tablas_html[[posicion_tabla]] %>% html_table()
    
  return(tabla_final)
}


# a <- extraer_tabla_presidentes(url = "https://es.wikipedia.org/wiki/Anexo:Presidentes_del_Ecuador", 
#                           posicion_tabla = 2)


# Obteniendo la base bruta de cada país -----------------------------------

paises_latinoamerica <- c("Argentina", "Bolivia", "Brasil", 
                          "Chile", "Colombia", "Costa Rica", 
                          "Cuba", "Ecuador", "El Salvador",
                          "Guatemala", "Honduras", "México", 
                          "Nicaragua", "Panama", "Paraguay", 
                          "Peru", "Puerto Rico", "Republica Dominicana", 
                          "Uruguay", "Venezuela", "Venezuela")

urls <- c("https://es.wikipedia.org/wiki/Anexo:Presidentes_de_la_Nación_Argentina",
          "https://es.wikipedia.org/wiki/Anexo:Presidentes_de_Bolivia",
          "https://es.wikipedia.org/wiki/Anexo:Presidentes_de_Brasil",
          "https://es.wikipedia.org/wiki/Anexo:Presidentes_de_Chile",
          "https://es.wikipedia.org/wiki/Anexo:Presidentes_de_Colombia",
          "https://es.wikipedia.org/wiki/Anexo:Presidentes_de_Costa_Rica",
          "https://es.wikipedia.org/wiki/Anexo:Presidentes_de_Cuba",
          "https://es.wikipedia.org/wiki/Anexo:Presidentes_del_Ecuador",
          "https://es.wikipedia.org/wiki/Anexo:Presidentes_de_El_Salvador",
          "https://es.wikipedia.org/wiki/Anexo:Presidentes_de_Guatemala",
          "https://es.wikipedia.org/wiki/Anexo:Gobernantes_de_Honduras",
          "https://es.wikipedia.org/wiki/Anexo:Gobernantes_de_M%C3%A9xico",
          "https://es.wikipedia.org/wiki/Anexo:Gobernantes_de_Nicaragua",
          "https://es.wikipedia.org/wiki/Anexo:Presidentes_de_Panam%C3%A1",
          "https://es.wikipedia.org/wiki/Anexo:Presidentes_de_Paraguay",
          "https://es.wikipedia.org/wiki/Anexo:Presidentes_del_Per%C3%BA",
          "https://es.wikipedia.org/wiki/Anexo:Gobernantes_de_Puerto_Rico",
          "https://es.wikipedia.org/wiki/Anexo:Presidentes_de_la_Rep%C3%BAblica_Dominicana",
          "https://es.wikipedia.org/wiki/Anexo:Gobernantes_de_Uruguay",
          "https://es.wikipedia.org/wiki/Anexo:Gobernantes_de_Venezuela",
          "https://es.wikipedia.org/wiki/Anexo:Gobernantes_de_Venezuela")

posiciones <- c(1, 2, 2, 
                14, 6, 1, 
                3, 2, 6,
                4, 6, 22,
                16, 5, 2,
                1, 6, 6,
                6, 19, 20)

bases <- map2(.x = urls,.y = posiciones,.f = extraer_tabla_presidentes)

#View(bases[[5]])

# extraer_tabla_presidentes(url = "https://es.wikipedia.org/wiki/Anexo:Gobernadores_de_las_Bahamas", 
#                           posicion_tabla = 20) %>% View



# Base Argentina ----------------------------------------------------------

Argentina <- bases[[1]]

Argentina <- Argentina %>% select(-c(1,2,10))

Argentina <- Argentina %>% mutate(
  Presidente = str_remove(string = `Presidente de la Nación`,pattern = "\\(.*|[:digit:].*"))

operacion_fechas <- function(base,variables,formato){
  
  base <-
    base %>% mutate(
      # inicio=  parse_date(`Inicio del mandato`, "%d %B %Y",locale=locale("es")) 
      
      across(.cols = variables,
             .fns = ~ str_remove(string = .x,pattern = "\\[.+]")),
      across(.cols = variables,
             .fns = ~ str_remove_all(string = .x,pattern = "de[:space:]")),
      across(.cols = variables,
             .fns = ~ str_trim(string = .x,side = "both")),
      across(.cols = variables,
             .fns = ~ str_squish(string = .x)),
      across(.cols = variables,
             .fns = ~ str_remove_all(string = .x,pattern = "\u200b")),
      across(.cols = variables,
             .fns = ~ parse_date(.x,formato,locale=locale("es")))
      
    )  
  
  return(base)
}
 


operacion_fechas(base = Argentina,variables = c("Inicio del mandato","Fin del mandato"),formato = "%d %B %Y") %>% 
    select(matches("Inicio "),matches("Fin ")) 

    

Bolivia <- bases[[2]]

Bolivia <- Bolivia %>% 
  select(-c(1:3)) %>% 
  mutate(across(.cols = c(Inicio, Final),
                ~ str_extract_all(.x,".*[:digit:]{4}")),
         across(.cols = c(Inicio, Final),
                ~ str_remove_all(.x,".*\\,")),
         across(.cols = c(Inicio, Final),
                ~ str_remove_all(.x,"\\.")))


completar_meses <- function(variable){
  
  var_low <- str_to_title(variable) 
  
  
  variable_mod <- case_when(
      str_detect(var_low,"Ene") ~ str_replace_all(var_low,"Ene","Enero"),
      str_detect(var_low,"Feb") ~ str_replace_all(var_low,"Feb","Febrero"),
      str_detect(var_low,"Mar") ~ str_replace_all(var_low,"Mar","Marzo"),
      str_detect(var_low,"Abr") ~ str_replace_all(var_low,"Abr","Abril"),
      str_detect(var_low,"May") ~ str_replace_all(var_low,"May","Mayo"),
      str_detect(var_low,"Jun") ~ str_replace_all(var_low,"Jun","Junio"),
      str_detect(var_low,"Jul") ~ str_replace_all(var_low,"Jul","Julio"),
      str_detect(var_low,"Ago") ~ str_replace_all(var_low,"Ago","Agosto"),
      str_detect(var_low,"Sep") ~ str_replace_all(var_low,"Sep","Septiembre"),
      str_detect(var_low,"Oct") ~ str_replace_all(var_low,"Oct","Octubre"),
      str_detect(var_low,"Nov") ~ str_replace_all(var_low,"Nov","Noviembre"),
      str_detect(var_low,"Dic") ~ str_replace_all(var_low,"Dic","Diciembre")
    
  )
  
  variable_mod <- str_to_lower(variable_mod)
  
  return(variable_mod)
  
}

Bolivia <- Bolivia %>% 
  rowwise() %>% 
  mutate( across(.cols = c(Inicio, Final),
                           ~ completar_meses(.x))) %>% 
  ungroup()

Bolivia <- operacion_fechas(base = Bolivia,variables = c("Inicio", "Final"),formato = "%d %B %Y" ) 



Bolivia %>% 
  filter(year(Inicio) > 1990)

