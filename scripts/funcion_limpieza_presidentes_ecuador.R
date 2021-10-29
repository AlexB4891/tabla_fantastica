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
 
 Ecuador <- Ecuador %>%
   filter(year(inicio) >= 1998) 


 
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




# Fechas formato: "25 de agosto de 2021" -----------------------------------


operacion_fechas <- function(base,variables,formato){
  
  base <-
    base %>% mutate(
      # inicio=  parse_date(`Inicio del mandato`, "%d %B %Y",locale=locale("es")) 
      
      across(.cols = variables,
             .fns = ~ str_remove(string = .x, pattern = "\\[.+]")),
      across(.cols = variables,
             .fns = ~ str_remove_all(string = .x, pattern = "de[:space:]")),
      across(.cols = variables,
             .fns = ~ str_trim(string = .x, side = "both")),
      across(.cols = variables,
             .fns = ~ str_squish(string = .x)),
      across(.cols = variables,
             .fns = ~ str_remove_all(string = .x, pattern = "\u200b")),
      across(.cols = variables,
             .fns = ~ parse_date(.x,formato,locale=locale("es")))
      
    )  
  
  return(base)
}


 
# Funcion para completar los nombres de los meses del año -----------------

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



# Base Argentina ----------------------------------------------------------

Argentina <- bases[[1]]

Argentina <- Argentina %>% select(-c(1,2,10))

Argentina <- Argentina %>% mutate(
  nombre_del_presidente = str_remove(string = `Presidente de la Nación`,pattern = "\\(.*|[:digit:].*"))

Argentina <- operacion_fechas(base = Argentina, 
                 variables = c("Inicio del mandato","Fin del mandato"), 
                 formato = "%d %B %Y") 

Argentina <- Argentina %>% mutate(inicio = `Inicio del mandato`, fin = `Fin del mandato`) %>% 
  select(nombre_del_presidente, inicio, fin) %>%
  filter(year(inicio) >= 1998) %>% 
  mutate(pais = "Argentina")


# Base Bolivia ------------------------------------------------------------

Bolivia <- bases[[2]]

Bolivia <- Bolivia %>% 
  select(-c(1:3))

Bolivia <- Bolivia %>% mutate(
  nombre_del_presidente = str_remove(string = Presidente, 
                                     pattern = "\\(.*|[:digit:].*|\\[.*"))


Bolivia <- Bolivia  %>% 
  mutate(across(.cols = c(Inicio, Final),
                ~ str_extract_all(.x,".*[:digit:]{4}")),
         across(.cols = c(Inicio, Final),
                ~ str_remove_all(.x,".*\\,")),
         across(.cols = c(Inicio, Final),
                ~ str_remove_all(.x,"\\.")))

Bolivia <- Bolivia %>% 
  rowwise() %>% 
  mutate( across(.cols = c(Inicio, Final),
                           ~ completar_meses(.x))) %>% 
  ungroup()

Bolivia <- operacion_fechas(base = Bolivia,variables = c("Inicio", "Final"),formato = "%d %B %Y" ) 

Bolivia <- Bolivia %>% mutate(inicio = Inicio, fin = Final) %>% 
  select(nombre_del_presidente, inicio, fin) %>%
  filter(year(inicio) >= 1997) 

Bolivia <- Bolivia %>% mutate(pais = "Bolivia")



# Base Brasil -------------------------------------------------------------

Brasil <- bases[[3]]

Brasil <- Brasil %>% select(-c(3:4))

Brasil <- operacion_fechas(base = Brasil, 
                 variables = c("Inicio del mandato", "Fin del mandato"), 
                 formato = "%d %B %Y")

Brasil <- Brasil %>% mutate(nombre_del_presidente = Presidente, 
                            inicio = `Inicio del mandato`, 
                            fin = `Fin del mandato`) %>% 
  select(nombre_del_presidente, inicio, fin) %>%
  filter(year(inicio) >= 1995) 

Brasil <- Brasil %>% mutate(pais = "Brasil")


# Base Chile --------------------------------------------------------------

Chile <- bases[[4]]

names(Chile)

Chile <- Chile %>% select(-c(2,6,8))

Chile <- operacion_fechas(base = Chile, 
                 variables = c("Inicio del mandato", "Fin del mandato"), 
                 formato = "%d %B %Y")

Chile <- Chile %>% mutate(nombre_del_presidente = Presidente, 
                            inicio = `Inicio del mandato`, 
                            fin = `Fin del mandato`) %>% 
  select(nombre_del_presidente, inicio, fin) %>%
  filter(year(inicio) >= 1994) 

Chile <- Chile %>% mutate(pais = "Chile")


# Base Colombia -----------------------------------------------------------

Colombia <- bases[[5]]

names(Colombia)

Colombia <- Colombia %>% select(-c(1))

Colombia <- Colombia %>% mutate(periodo_presidencia = str_split(string = Período,pattern = "-"),
                    inicio = map(.x = periodo_presidencia,~.x[1]),
                    fin = map(.x = periodo_presidencia,~.x[2]),
                    across(.cols = c(inicio,fin),
       .fns = ~ str_remove(string = .x,pattern = "\\[.\\]")),
       across(.cols = c(inicio,fin),
       .fns = ~ str_remove_all(string = .x,pattern = "de[:space:]")),
       across(.cols = c(inicio,fin),
       .fns = ~ parse_date(.x,"%d %B %Y",locale=locale("es"))),
       nombre_del_presidente = str_remove(string = Presidente, 
                                          pattern = "\\(.*|[:digit:].*")) %>% 
  select(nombre_del_presidente, inicio, fin) %>%
  filter(year(inicio) >= 1998) 

Colombia <- Colombia %>% mutate(pais = "Colombia")


# Base Costa Rica ---------------------------------------------------------

Costa_Rica <- bases[[6]]

names(Costa_Rica)

names(Costa_Rica)[c(3,4,5,7)] <- c("nombre_del_presidente",
                                   "inicio", 
                                   "fin", "temporal")

Costa_Rica <- operacion_fechas(base = Costa_Rica, 
                               variables = c("inicio", "fin"), 
                               formato = "%d %B %Y") %>% 
  mutate(nombre_del_presidente = str_remove(string = nombre_del_presidente, 
                                                         pattern = "\\(.*")) %>% 
  select(nombre_del_presidente, inicio, fin) 

Costa_Rica <- Costa_Rica[c(96,98,100,102,104, 106),]

Costa_Rica <- Costa_Rica %>% mutate(pais = "Costa_Rica")


# Base Cuba ---------------------------------------------------------------

Cuba <- bases[[7]]

names(Cuba)

Cuba <- Cuba[-1,]

Cuba <- Cuba %>% rename(inicio = X5, fin = X6, nombre_del_presidente = X4) 

Cuba <- operacion_fechas(base = Cuba, variables = c("inicio", "fin"), formato = "%d %B %Y")

Cuba <- Cuba %>%
  select(nombre_del_presidente, inicio, fin) 

Cuba <- Cuba %>% mutate(pais = "Cuba")


# Base El Salvador --------------------------------------------------------

Salvador <- bases[[9]]

names(Salvador)

Salvador <- Salvador %>% select(-c(1,2))

Salvador <- Salvador %>% mutate(nombre_del_presidente = Nombre, 
                                periodo_presidencia = str_split(string = Período,pattern = "-"),
                                inicio = map(.x = periodo_presidencia,~.x[1]),
                                fin = map(.x = periodo_presidencia,~.x[2]),
                                across(.cols = c(inicio,fin),
                                       .fns = ~ str_remove(string = .x,pattern = "\\[.\\]")),
                                across(.cols = c(inicio,fin),
                                       .fns = ~ str_remove_all(string = .x,pattern = "de[:space:]")),
                                across(.cols = c(inicio,fin),
                                       .fns = ~ parse_date(.x,"%d %B %Y",locale=locale("es"))),
                                # nombre_del_presidente = str_remove(string = Presidente, 
                                #                                    pattern = "\\(.*|[:digit:].*")
                                ) %>% 
  select(nombre_del_presidente, inicio, fin) %>%
  filter(year(inicio) >= 1999) 

Salvador <- Salvador %>% mutate(pais= "El_Salvador")


# Guatemala ---------------------------------------------------------------

Guatemala <- bases[[10]]

names(Guatemala)

Guatemala <- Guatemala %>% select(-c(2,3))

Guatemala <- operacion_fechas(base = Guatemala, 
                 variables = c("Inicio del mandato", "Fin del mandato"), 
                 formato = "%d %B %Y")

Guatemala <- Guatemala %>% rename(nombre_del_presidente = Presidente, 
                     inicio = `Inicio del mandato`,
                     fin = `Fin del mandato`) %>% 
  select(nombre_del_presidente, inicio, fin) 

Guatemala <- Guatemala %>% filter(year(inicio) >= 1996)

Guatemala <- Guatemala %>% mutate(nombre_del_presidente = str_remove(string = nombre_del_presidente,
                                                        pattern = "\\..*|[:digit:].*"))

Guatemala <- Guatemala %>% mutate(pais = "Guatemala")


# Base Honduras -----------------------------------------------------------

# Honduras <- bases[[11]]

Honduras <- tibble(nombre_del_presidente = c("Carlos Roberto Flores", "Ricardo Maduro", "Manuel Zelaya",
                                             "Roberto Micheletti", "Porfirio Lobo", "Juan Orlando Hernández"),
                   inicio = c("27 de enero de 1998", "27 de enero de 2002", "27 de enero de 2006",
                              "29 de junio de 2009", "27 de enero de 2010", "27 de enero de 2014"),
                   fin = c("27 de enero de 2002", "27 de enero de 2006", "28 de junio de 2009",
                           "27 de enero de 2010", "27 de enero de 2014", "27 de enero de 2022"))

Honduras <- operacion_fechas(base = Honduras, 
                             variables = c("inicio", "fin"), 
                             formato = "%d %B %Y")
Honduras <- Honduras %>% mutate(pais = "Honduras")

# Base México -------------------------------------------------------------

Mexico <- bases[[12]]

names(Mexico)

Mexico <- Mexico %>% select(c(3,6))

Mexico <- Mexico %>% mutate(nombre_del_presidente = Presidente, 
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
  select(nombre_del_presidente, inicio, fin) %>%
  filter(year(inicio) >= 1994) 

Mexico <- Mexico %>% mutate(pais = "Mexico")


# Base Nicaragua ----------------------------------------------------------

Nicaragua <- bases[[13]]

names(Nicaragua)

Nicaragua <- Nicaragua %>% select(c(3,4))


Nicaragua <- Nicaragua %>% mutate(periodo_presidencia = str_split(string = Mandato,pattern = "-"),
                  inicio = map(.x = periodo_presidencia,~.x[1]),
                  fin = map(.x = periodo_presidencia,~.x[2]),
                  across(.cols = c(inicio,fin),
                         .fns = ~ str_remove(string = .x,pattern = "\\[.\\]")),
                  across(.cols = c(inicio,fin),
                         .fns = ~ str_remove_all(string = .x,pattern = "de[:space:]")),
                  across(.cols = c(inicio,fin),
                         .fns = ~ parse_date(.x,"%d %B %Y",locale=locale("es")))
                  ) %>% 
  select(Nombre, inicio, fin) %>%
  filter(year(inicio) >= 1997) 

Nicaragua <- Nicaragua %>% mutate(nombre_del_presidente = str_extract(string = Nombre,pattern = "[:alpha:].*")) %>% 
  select(nombre_del_presidente, inicio, fin)

Nicaragua <- Nicaragua %>% mutate(pais = "Nicaragua")


# Base Panama -------------------------------------------------------------

Panama <- bases[[14]]

names(Panama)[c(3,5,6)] <- c("nombre_del_presidente", "inicio", "fin") 

names(Panama)

Panama <- operacion_fechas(base = Panama, variables = c("inicio", "fin"), formato = "%d %B %Y") %>% 
  filter(year(inicio)>=1999) %>% select("nombre_del_presidente", "inicio", "fin") %>% 
  mutate(nombre_del_presidente = str_remove(string = nombre_del_presidente, pattern = "\\(.*") %>% 
  str_trim())

Panama <- Panama %>% mutate(pais = "Panama")


# Base Paraguay -----------------------------------------------------------

Paraguay <- bases[[15]]

names(Paraguay)[c(3,5,6)] <- c("nombre_del_presidente", "inicio", "fin")

Paraguay <- operacion_fechas(base = Paraguay, variables = c("inicio", "fin"), formato = "%d %B %Y") %>% 
  filter(year(inicio)>= 1999) %>% 
  select("nombre_del_presidente", "inicio", "fin")

Paraguay <- Paraguay %>% mutate(pais = "Paraguay")


# Base Peru ---------------------------------------------------------------

Peru <- bases[[16]]

names(Peru)

Peru <- Peru %>% select(c(3,5,6))

Peru <- operacion_fechas(base = Peru, 
                         variables = c("Inicio del mandato", "Fin del mandato"), 
                         formato = "%d %B %Y") 

Peru <-  Peru %>% rename(inicio = `Inicio del mandato`, 
                         fin = `Fin del mandato`)

Peru <-  Peru %>% filter(year(inicio) >= 1995) 

Peru <-  Peru %>% mutate(nombre_del_presidente = str_remove(string = Presidente,pattern = "\\[.*") %>% 
                           str_trim()) %>% 
  select(-Presidente)

Peru <- Peru %>% mutate(pais = "Peru")


# Base Puerto Rico --------------------------------------------------------

Puerto_Rico <- bases[[17]]

names(Puerto_Rico)

Puerto_Rico <- Puerto_Rico %>% select("Gobernador", "Inicio", "Fin")

Puerto_Rico <- operacion_fechas(base = Puerto_Rico, 
                                variables = c("Inicio", "Fin"), 
                                formato = "%d %B %Y") %>% 
  filter(year(Inicio) >= 1997) 

Puerto_Rico <- Puerto_Rico %>% 
  mutate(nombre_del_presidente = str_remove(string = Gobernador, pattern = "\\(.*")) %>% 
  rename(inicio = Inicio, fin = Fin) %>% 
  select(nombre_del_presidente, inicio, fin)

Puerto_Rico <- Puerto_Rico %>% mutate(pais = "Puerto_Rico")


# Base Republica Dominicana -----------------------------------------------

Republica_Dominicana <- bases[[18]]

names(Republica_Dominicana)

Republica_Dominicana <- Republica_Dominicana %>% select(-c(2,3,9))

Republica_Dominicana <- operacion_fechas(base = Republica_Dominicana, 
                 variables = c("Inicio del mandato", "Fin del mandato"), 
                 formato = "%d %B %Y") %>% 
  rename(nombre_del_presidente = Presidente,
         inicio = `Inicio del mandato`,
         fin = `Fin del mandato`) %>% 
  filter(year(inicio)>=1996) %>% select(nombre_del_presidente, inicio, fin)

Republica_Dominicana <- Republica_Dominicana %>% mutate(pais = "Republica_Dominicana")


# Base Uruguay ------------------------------------------------------------

Uruguay <- bases[[19]]

names(Uruguay)[c(3,5,6)] <- c("nombre_del_presidente", "inicio", "fin")

Uruguay <- operacion_fechas(base = Uruguay, 
                            variables = c("inicio", "fin"), 
                            formato = "%d %B %Y") %>% 
  filter(year(inicio)>=1995) %>% 
  select("nombre_del_presidente", "inicio", "fin")

Uruguay <- Uruguay %>% mutate(pais = "Uruguay")


# Base Venezuela ----------------------------------------------------------

# Primera tabla

Venezuela <- bases[[20]]

names(Venezuela)

Venezuela <- Venezuela %>% 
  select(c(4, 7,8)) %>% 
  rename(nombre_del_presidente = Presidente, 
                                           inicio = `Inicio del mandato`, 
                                           fin = `Fin del mandato`) 
Venezuela <-   operacion_fechas(base = Venezuela, 
                   variables = c("inicio", "fin"), 
                   formato = "%d %B %Y")

# Segunda tabla

Venezuela1 <- bases[[21]]

Venezuela1 <- Venezuela1 %>% 
  select(c(4, 7,8)) %>% 
  rename(nombre_del_presidente = Presidente, 
         inicio = `Inicio del mandato`, 
         fin = `Fin del mandato`) %>% 
  mutate(nombre_del_presidente = str_remove(string = nombre_del_presidente, pattern = "\\(.*"))

Venezuela1 <- operacion_fechas(base = Venezuela1, 
                 variables = c("inicio", "fin"), 
                 formato = "%d %B %Y")

# Uniendo las bases para dejar una sola

Venezuela <- rbind(Venezuela, Venezuela1) %>% mutate(pais = "Venezuela")

# Borrando la base que ya no se necesita
rm(Venezuela1)


# Uniendo a todos los países ----------------------------------------------

paises_base <- rbind(Argentina, Bolivia, Brasil, 
      Chile, Colombia, Costa_Rica, 
      Cuba, Ecuador, Salvador, 
      Guatemala, Honduras, Mexico, 
      Nicaragua, Panama, Paraguay, 
      Peru, Puerto_Rico, Republica_Dominicana,
      Uruguay, Venezuela)




# UNIR paises con gasto social --------------------------------------------


# Crear una nueva columna con el año de inicio y ponerla como character
paises_base <- paises_base %>% 
  mutate(Year = year(inicio) %>% as.character())


# Unir las dos bases por año y país, ordenar y rellenar (para abajo) con los valores faltantes
paises_base %>% 
  right_join(gasto_social, by = c("Year", "pais")) %>% 
  arrange("Year") %>% 
  fill(Indicador_valor, .direction = "down") 




## EJEMPLO DE LO QUE SE ESTÁ HACIENDO PREVIAMENTE

## 1. Sacar el año de la fecha de inicio
# 2. Hacer un join con los datos del gasto social
# 3. Pasar la funcion del grafico 
# Hay que completar las series de tiempo

data_year <- tibble(year = 1990:2021) 

tibble(year = c(2006,2010,2015,2021),
       valor = rnorm(4)) %>% 
  right_join(data_year) %>% 
  arrange(year) %>% 
  fill(valor,.direction = "down") %>% 
  ggplot(mapping = aes(x = year,y = valor)) +
  geom_line()



