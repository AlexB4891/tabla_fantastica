
# -------------------------------------------------------------------------#
# ---------------LIMPIEZA DE DATOS DE PAISES -----------------------------
# -------------------------------------------------------------------------#



# Librerias ---------------------------------------------------------------

library(tidyverse)
library(rvest)
library(lubridate)


 
 # ------------------FUNCIÓN para sacar tablas de los paises ---------------
 
 # 1. Armar una lista de urls
 # 2. Crear un vector con la posicion de la tabla despues de sacar los nodos

extraer_tabla_presidentes <- function(url, posicion_tabla){
  
# Leer la pagina web y usar sus elementos
  pagina_presidentes <- read_html(url)
  
# Extraer todos los elementos de una página web que están dentro de la tag "table"
  tablas_html <- pagina_presidentes %>% html_nodes("table") 
  
# Convertir la tabla de "xml" a un bonito data.frame
  tabla_final <- tablas_html[[posicion_tabla]] %>% html_table()
  
  return(tabla_final)
}


# ------------Obteniendo la base bruta de cada país -----------------------

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


# -------------------------------------------------------------------------#
# -----------------------------------PAISES -------------------------------#
# -------------------------------------------------------------------------#

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
   filter(year(inicio) >= 1988) 

 Ecuador <- Ecuador %>% 
   mutate(pais = "Ecuador",
          ideologia = c("Izquierda", "Centro Derecha", "Centro Derecha",
                        "Centro Izquierda", "Centro Derecha", "Centro Derecha",
                        "Centro Derecha", "Centro Derecha", "Centro Izquierda",
                        "Independiente", "Izquierda", "Izquierda",
                        "Izquierda", "Izquierda","Izquierda",
                        "Izquierda", "Izquierda", "Izquierda",
                        "Derecha"))

 Ecuador <- Ecuador[-c(3,5,12,13,15,16,17,18),]
 
 Ecuador <- Ecuador %>% mutate(img = c("https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/Rodrigo_Borja.png/200px-Rodrigo_Borja.png", 
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c1/Sixto_Durán-Ballén.png/200px-Sixto_Durán-Ballén.png",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e8/Bucaram_foto.png/200px-Bucaram_foto.png",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8a/Fabian_Alarcon.jpg/200px-Fabian_Alarcon.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a7/JamilMahuad.jpg/198px-JamilMahuad.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9e/Gustavo_Noboa.png/200px-Gustavo_Noboa.png",
                            "https://upload.wikimedia.org/wikipedia/commons/7/72/Lucio_Gutiérrez.png",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/d/da/Luis_Alfredo_Palacio.png/198px-Luis_Alfredo_Palacio.png",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3c/Presidente_Rafael_Correa.jpg/200px-Presidente_Rafael_Correa.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ac/A_Len%C3%ADn_Moreno_%28Transmisión_del_Mando_Presidencial_Ecuador_2017%29_%28cropped%29.jpg/200px-A_Len%C3%ADn_Moreno_%28Transmisión_del_Mando_Presidencial_Ecuador_2017%29_%28cropped%29.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/7/70/Guillermo_Lasso_inauguration_%286%29_%28cropped%29.jpg/200px-Guillermo_Lasso_inauguration_%286%29_%28cropped%29.jpg"))

 
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
  filter(year(inicio) >= 1989) %>% 
  mutate(pais = "Argentina", 
         ideologia = "Centro Izquierda")


Argentina <- Argentina[-c(1,4),]

Argentina <- Argentina %>% mutate(img= c("https://upload.wikimedia.org/wikipedia/commons/thumb/1/14/Menem_con_banda_presidencial.jpg/150px-Menem_con_banda_presidencial.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/1/14/Menem_con_banda_presidencial.jpg/150px-Menem_con_banda_presidencial.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b5/Fernando_de_la_Rúa_con_bastón_y_banda_de_presidente.jpg/150px-Fernando_de_la_Rúa_con_bastón_y_banda_de_presidente.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d3/Asunción_Rodr%C3%ADguez_Saá.jpg/150px-Asunción_Rodr%C3%ADguez_Saá.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/9/91/Eduardo_duhalde_presidente.jpg/150px-Eduardo_duhalde_presidente.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/d/db/NestorKirchner.jpeg/150px-NestorKirchner.jpeg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Cristina_Fernandez_de_Kirchner_-_Foto_Oficial_2.jpg/150px-Cristina_Fernandez_de_Kirchner_-_Foto_Oficial_2.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Cristina_Fernandez_de_Kirchner_-_Foto_Oficial_2.jpg/150px-Cristina_Fernandez_de_Kirchner_-_Foto_Oficial_2.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/4/44/Retrato_oficial_del_Presidente_Mauricio_Macri.jpg/200px-Retrato_oficial_del_Presidente_Mauricio_Macri.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/d/db/Alberto_fernandez_presidente_%28cropped%29.jpg/150px-Alberto_fernandez_presidente_%28cropped%29.jpg"
                            ))


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
  filter(year(inicio) >= 1989) 

Bolivia <- Bolivia %>% mutate(pais = "Bolivia", 
                              ideologia = c("Izquierda", "Centro Derecha", "Derecha", 
                                            "Derecha", "Centro Derecha", "Centro Derecha",
                                            "Independiente", "Izquierda", "Izquierda", 
                                            "Izquierda","Centro Derecha", "Izquierda"))
Bolivia <- Bolivia %>% mutate(img= c("https://upload.wikimedia.org/wikipedia/commons/3/3b/Jaime_Paz_Zamora.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c9/Gonzálo_Sánchez_de_Lozada-Agencia_BrasilAntonio_Cruz.jpg/240px-Gonzálo_Sánchez_de_Lozada-Agencia_BrasilAntonio_Cruz.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ed/Hugo_Banzer_Suarez%2C_General%2C_Presidente_da_Bol%C3%ADvia..tif/lossy-page1-239px-Hugo_Banzer_Suarez%2C_General%2C_Presidente_da_Bol%C3%ADvia..tif.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f9/Jorge_Quiroga_Inter-American_2019_cropped.jpg/240px-Jorge_Quiroga_Inter-American_2019_cropped.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c9/Gonzálo_Sánchez_de_Lozada-Agencia_BrasilAntonio_Cruz.jpg/240px-Gonzálo_Sánchez_de_Lozada-Agencia_BrasilAntonio_Cruz.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2d/Carlos_Mesa%2C_ex-President_of_Bolivia_%28cropped_2%29.jpg/240px-Carlos_Mesa%2C_ex-President_of_Bolivia_%28cropped_2%29.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/9/96/Eduardo_Rodr%C3%ADguez_Veltzé_en_la_XV_Cumbre_Iberoamericana_%28cropped%29.jpg/240px-Eduardo_Rodr%C3%ADguez_Veltzé_en_la_XV_Cumbre_Iberoamericana_%28cropped%29.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/Evo_Morales_Ayma_%28cropped%29.jpg/240px-Evo_Morales_Ayma_%28cropped%29.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/Evo_Morales_Ayma_%28cropped%29.jpg/240px-Evo_Morales_Ayma_%28cropped%29.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/Evo_Morales_Ayma_%28cropped%29.jpg/240px-Evo_Morales_Ayma_%28cropped%29.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/1/18/66_-_Jeanine_Áñez.jpg/240px-66_-_Jeanine_Áñez.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a6/Luis_Arce_%2823588020275%29_%28cropped%29.jpg/239px-Luis_Arce_%2823588020275%29_%28cropped%29.jpg"
                          ))


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
  filter(year(inicio) >= 1990) 

Brasil <- Brasil %>% mutate(pais = "Brasil",
                            ideologia = c("Centro Izquierda", "Centro Izquierda", "Centro Derecha",
                                          "Centro Derecha", "Centro Izquierda", "Centro Izquierda",
                                          "Centro Izquierda", "Centro Izquierda", "Centro Izquierda",
                                          "Independiente"))

Brasil <- Brasil[-c(3,5,7),]

Brasil <- Brasil %>% mutate(img = c("https://upload.wikimedia.org/wikipedia/commons/thumb/b/bf/Fernando_Collor_1992_B%26W.jpg/159px-Fernando_Collor_1992_B%26W.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Itamar_Franco_%28cropped%29.jpg/160px-Itamar_Franco_%28cropped%29.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/4/46/Fernando_Henrique_Cardoso_%281999%29.jpg/160px-Fernando_Henrique_Cardoso_%281999%29.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b8/Lula_-_foto_oficial_-_05_jan_2007_%28cropped_3%29.jpg/160px-Lula_-_foto_oficial_-_05_jan_2007_%28cropped_3%29.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/8/81/Dilma_Rousseff_-_foto_oficial_2011-01-09.jpg/160px-Dilma_Rousseff_-_foto_oficial_2011-01-09.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9b/Presidente_Michel_Temer_%28foto_oficial%29_-_cortada.jpg/160px-Presidente_Michel_Temer_%28foto_oficial%29_-_cortada.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8a/Jair_Bolsonaro_em_24_de_abril_de_2019_%281%29_%28cropped%29.jpg/160px-Jair_Bolsonaro_em_24_de_abril_de_2019_%281%29_%28cropped%29.jpg"
                          ))

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
  filter(year(inicio) >= 1990) 

Chile <- Chile %>% mutate(pais = "Chile",
                          ideologia = c("Centro Izquierda", "Centro Izquierda", "Centro Izquierda",
                                        "Centro Izquierda", "Centro Derecha", "Centro Izquierda",
                                        "Centro Derecha"))

Chile <- Chile %>% mutate(img = c("https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Aylwin_Banda_%282%29.jpg/200px-Aylwin_Banda_%282%29.jpg",
                         "https://upload.wikimedia.org/wikipedia/commons/thumb/3/37/Eduardo_Frei_1998.jpg/200px-Eduardo_Frei_1998.jpg",
                         "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f2/Ricardo_Lagos_despedida_%28cropped%29.jpg/200px-Ricardo_Lagos_despedida_%28cropped%29.jpg",
                         "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2e/Michelle_Bachelet_2006_%28Cropped_2%29.png/200px-Michelle_Bachelet_2006_%28Cropped_2%29.png",
                         "https://upload.wikimedia.org/wikipedia/commons/thumb/7/7a/Fotograf%C3%ADa_oficial_del_Presidente_Sebastián_Piñera_-_2.jpg/200px-Fotograf%C3%ADa_oficial_del_Presidente_Sebastián_Piñera_-_2.jpg",
                         "https://upload.wikimedia.org/wikipedia/commons/thumb/0/02/Portrait_Michelle_Bachelet.jpg/200px-Portrait_Michelle_Bachelet.jpg",
                         "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8a/Retrato_Oficial_Presidente_Piñera_2018.jpg/200px-Retrato_Oficial_Presidente_Piñera_2018.jpg"
                         ))

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
  filter(year(inicio) >= 1986) 

Colombia <- Colombia %>% mutate(pais = "Colombia",
                                ideologia = c("Centro Izquierda", "Centro Izquierda", "Centro Izquierda",
                                              "Centro Izquierda", "Centro Izquierda", "Derecha",
                                              "Derecha", "Centro Derecha", "Centro Derecha"))
Colombia <- Colombia[-1,]

Colombia <- Colombia %>% mutate(img = c("https://upload.wikimedia.org/wikipedia/commons/2/25/Virgilio_Barco.png",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/5/51/César_Gaviria%2C_World_Economic_Forum_on_Latin_America_2009_%28cropped%29.jpg/244px-César_Gaviria%2C_World_Economic_Forum_on_Latin_America_2009_%28cropped%29.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d2/Samper_cropped.jpg/201px-Samper_cropped.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/Ernesto_Samper_%28cropped%29.jpg/167px-Ernesto_Samper_%28cropped%29.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/7/78/Andres_Pastrana_Arango_%282001%29.jpg/200px-Andres_Pastrana_Arango_%282001%29.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/5/57/Álvaro_Uribe_Vélez.png/200px-Álvaro_Uribe_Vélez.png",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/9/98/Juan_Manuel_Santos_2.jpg/225px-Juan_Manuel_Santos_2.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/Secretary_Pompeo_Delivers_a_Press_Statement_with_Colombian_President_Duque_%2850369491081%29.jpg/268px-Secretary_Pompeo_Delivers_a_Press_Statement_with_Colombian_President_Duque_%2850369491081%29.jpg"
                            ))


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
                                                         pattern = "\\(.*"))%>% 
  select(nombre_del_presidente, inicio, fin) 

Costa_Rica <- Costa_Rica[c(92, 94,96,98,100,102,104, 106),]

Costa_Rica <- Costa_Rica %>% mutate(pais = "Costa_Rica",
                                    ideologia = c("Centro Derecha", "Centro Derecha", "Centro Derecha",
                                                  "Centro Derecha", "Centro Derecha", "Centro Derecha",
                                                  "Centro Derecha", "Centro Derecha"))
Costa_Rica <- Costa_Rica %>% mutate(img = c("https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/Rafael_Ángel_Calderón_Fournier.jpg/180px-Rafael_Ángel_Calderón_Fournier.jpg",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/4/43/José_Mar%C3%ADa_Figueres_Olsen.jpg/180px-José_Mar%C3%ADa_Figueres_Olsen.jpg",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dd/Miguel_Ángel_Rodr%C3%ADguez_Echeverr%C3%ADa_Retrato.jpg/180px-Miguel_Ángel_Rodr%C3%ADguez_Echeverr%C3%ADa_Retrato.jpg",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/5/58/Retrato_de_Abel_Pacheco_de_la_Espriella_%28cropped%29.png/180px-Retrato_de_Abel_Pacheco_de_la_Espriella_%28cropped%29.png",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fb/Óscar_Arias_%28cropped%29.jpg/180px-Óscar_Arias_%28cropped%29.jpg",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Laura_Chinchilla_Miranda_with_Hillary_Rodham_Clinton_%28cropped%29.jpg/180px-Laura_Chinchilla_Miranda_with_Hillary_Rodham_Clinton_%28cropped%29.jpg",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b9/President_Luis_Guillermo_Solis.jpg/180px-President_Luis_Guillermo_Solis.jpg",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Future_Affairs_Berlin_2019_-_„Digital_Revolution_Resetting_Global_Power_Politics%3F“_%2847959618541%29.jpg/173px-Future_Affairs_Berlin_2019_-_„Digital_Revolution_Resetting_Global_Power_Politics%3F“_%2847959618541%29.jpg"
                              ))

# Base Cuba ---------------------------------------------------------------

Cuba <- bases[[7]]

names(Cuba)

Cuba <- Cuba[-1,]

Cuba <- Cuba %>% rename(inicio = X5, fin = X6, nombre_del_presidente = X4) 

Cuba <- operacion_fechas(base = Cuba, variables = c("inicio", "fin"), formato = "%d %B %Y")

Cuba <- Cuba %>%
  select(nombre_del_presidente, inicio, fin) 

Cuba <- Cuba %>% mutate(pais = "Cuba",
                        ideologia = "Izquierda")

Cuba <- Cuba %>% mutate(img = c("https://upload.wikimedia.org/wikipedia/commons/thumb/6/63/Fidel_Castro2.jpg/200px-Fidel_Castro2.jpg",
                        "https://upload.wikimedia.org/wikipedia/commons/thumb/7/78/Presidente_de_Cuba%2C_Raúl_Castro%2C_visita_Salvador.jpg/200px-Presidente_de_Cuba%2C_Raúl_Castro%2C_visita_Salvador.jpg",
                        "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a1/Miguel_Diaz_Canel.jpg/199px-Miguel_Diaz_Canel.jpg"
                        ))

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
  filter(year(inicio) >= 1989) 

Salvador <- Salvador %>% mutate(pais= "El_Salvador",
                                ideologia = c("Derecha", "Derecha", "Derecha", 
                                              "Derecha", "Izquierda", "Izquierda",
                                              "Centro Derecha"))
Salvador <- Salvador %>% mutate(img = c("https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/Alfredo_Cristiani.jpg/200px-Alfredo_Cristiani.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/9/95/Former_Presidents_and_First_Ladies_of_El_Salvador.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5b/President_Francisco_Flores_El_Salvador1.jpg/200px-President_Francisco_Flores_El_Salvador1.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e0/Antonio_Saca.png/200px-Antonio_Saca.png",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3f/Mauricio_Funes_%28Brasilia%2C_May_2008%29.jpg/200px-Mauricio_Funes_%28Brasilia%2C_May_2008%29.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/6/64/Salvador_Sanchez_Ceren.jpg/200px-Salvador_Sanchez_Ceren.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/7/76/Nayib_Bukele_-_2019_%2848342383356%29_%28cropped%29.jpg/200px-Nayib_Bukele_-_2019_%2848342383356%29_%28cropped%29.jpg"
                            ))

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

Guatemala <- Guatemala %>% filter(year(inicio) >= 1986)

Guatemala <- Guatemala %>% mutate(nombre_del_presidente = str_remove(string = nombre_del_presidente,
                                                        pattern = "\\..*|[:digit:].*"))

Guatemala <- Guatemala %>% mutate(pais = "Guatemala",
                                  ideologia = c("Centro Izquierda", "Derecha", "Derecha",
                                                "Independiente", "Derecha", "Derecha",
                                                "Centro Derecha", "Centro Izquierda", "Derecha",
                                                "Derecha", "Independiente", "Derecha",
                                                "Centro Derecha"))

Guatemala <- Guatemala[-9,]

Guatemala <- Guatemala %>% mutate(img = c("https://upload.wikimedia.org/wikipedia/commons/thumb/d/d2/Marco_Vinicio_Cerezo_Arévalo_DN-SN-86-05174_%28cropped%29.JPEG/200px-Marco_Vinicio_Cerezo_Arévalo_DN-SN-86-05174_%28cropped%29.JPEG",
                             "https://upload.wikimedia.org/wikipedia/commons/thumb/0/09/Jorge_Serrano_El%C3%ADas_%28cropped%29.jpg/200px-Jorge_Serrano_El%C3%ADas_%28cropped%29.jpg",
                             "",
                             "https://upload.wikimedia.org/wikipedia/commons/thumb/9/91/León_Carpio_1993_%28cropped%29.jpg/200px-León_Carpio_1993_%28cropped%29.jpg",
                             "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e2/Álvaro_Arzú_2017_%28cropped-a%29.jpg/200px-Álvaro_Arzú_2017_%28cropped-a%29.jpg",
                             "https://upload.wikimedia.org/wikipedia/commons/thumb/8/85/Portillo.jpg/200px-Portillo.jpg",
                             "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Oscar_Berger_2005_%28cropped%29.jpg/200px-Oscar_Berger_2005_%28cropped%29.jpg",
                             "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b9/Alvaro_Colom_Caballeros_with_Obamas_%28cropped%29.jpg/200px-Alvaro_Colom_Caballeros_with_Obamas_%28cropped%29.jpg",
                             "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d1/Pérez_Molina_cropped.jpg/200px-Pérez_Molina_cropped.jpg",
                             "https://upload.wikimedia.org/wikipedia/commons/thumb/8/85/Alejandro_Maldonado_2015_%28cropped%29.jpg/200px-Alejandro_Maldonado_2015_%28cropped%29.jpg",
                             "https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/President_Trump_Meets_with_the_President_of_Guatemala_%2849235087891%29_%28cropped%29.jpg/200px-President_Trump_Meets_with_the_President_of_Guatemala_%2849235087891%29_%28cropped%29.jpg",
                             "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2d/Alejandro_Giammattei_%282019%29_%28cropped%29.jpg/200px-Alejandro_Giammattei_%282019%29_%28cropped%29.jpg"
                             ))

# Base Honduras -----------------------------------------------------------

# Honduras <- bases[[11]]

Honduras <- tibble(nombre_del_presidente = c("Rafael Leonardo Callejas", "Carlos Roberto Reina",
                                             "Carlos Roberto Flores", "Ricardo Maduro", "Manuel Zelaya",
                                             "Roberto Micheletti", "Porfirio Lobo", "Juan Orlando Hernández"),
                   inicio = c("27 de enero de 1990", "27 de enero de 1994",
                              "27 de enero de 1998", "27 de enero de 2002", "27 de enero de 2006",
                              "29 de junio de 2009", "27 de enero de 2010", "27 de enero de 2014"),
                   fin = c("27 de enero de 1994", "27 de enero de 1998",
                           "27 de enero de 2002", "27 de enero de 2006", "28 de junio de 2009",
                           "27 de enero de 2010", "27 de enero de 2014", "27 de enero de 2022"))

Honduras <- operacion_fechas(base = Honduras, 
                             variables = c("inicio", "fin"), 
                             formato = "%d %B %Y")

Honduras <- Honduras %>% mutate(pais = "Honduras", 
                                ideologia = c("Centro Derecha","Centro Derecha", 
                                              "Centro Derecha", "Derecha", "Centro Izquierda", 
                                              "Centro Derecha", "Derecha", "Derecha"))

Honduras <- Honduras %>% mutate(img = c("https://upload.wikimedia.org/wikipedia/commons/thumb/5/5f/Chema_Callejas.jpg/160px-Chema_Callejas.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f9/Carlos_Roberto_Reina.jpg/160px-Carlos_Roberto_Reina.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Carlos_Flores_Facusse.JPG/160px-Carlos_Flores_Facusse.JPG",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c4/US_Navy_030820-F-2828D-168_Honduran_President_Ricardo_Maduro_2003-08-20.jpg/160px-US_Navy_030820-F-2828D-168_Honduran_President_Ricardo_Maduro_2003-08-20.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/1/12/Zelaya_en_Brasil_Agosto_2009.jpg/160px-Zelaya_en_Brasil_Agosto_2009.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/d/d5/Roberto_micheletti_01.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Pepe_Lobo_2010-01-27.jpg/160px-Pepe_Lobo_2010-01-27.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Juan_Orlando_Hernández_Alvarado_no_Brasil.jpg/160px-Juan_Orlando_Hernández_Alvarado_no_Brasil.jpg"
                            ))

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
  filter(year(inicio) >= 1988) 

Mexico <- Mexico %>% mutate(pais = "Mexico",
                            ideologia = c("Centro Derecha", "Centro Derecha", "Derecha",
                                          "Derecha", "Centro Derecha", "Centro Izquierda")
)

Mexico <- Mexico %>% mutate(img = c("https://upload.wikimedia.org/wikipedia/commons/thumb/f/fa/Felipe_González_pasea_con_presidente_de_México._Pool_Moncloa._15_de_julio_de_1989_%28cropped%29.jpeg/300px-Felipe_González_pasea_con_presidente_de_México._Pool_Moncloa._15_de_julio_de_1989_%28cropped%29.jpeg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/0/09/Ernesto_Zedillo_Ponce_de_Leon_Official_Photo_1999.jpg/300px-Ernesto_Zedillo_Ponce_de_Leon_Official_Photo_1999.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6d/Vicente_Fox_Official_Photo_2000.jpg/300px-Vicente_Fox_Official_Photo_2000.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/1/11/Felipe_Calderon_20090130_%28cropped%29.jpg/300px-Felipe_Calderon_20090130_%28cropped%29.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/3/30/Presidente_Enrique_Peña_Nieto._Fotograf%C3%ADa_oficial.jpg/300px-Presidente_Enrique_Peña_Nieto._Fotograf%C3%ADa_oficial.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6d/Andrés_Manuel_López_Obrador_2020.jpg/300px-Andrés_Manuel_López_Obrador_2020.jpg"
                          ))


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
  filter(year(inicio) >= 1990) 

Nicaragua <- Nicaragua %>% mutate(nombre_del_presidente = str_extract(string = Nombre,pattern = "[:alpha:].*")) %>% 
  select(nombre_del_presidente, inicio, fin)

Nicaragua <- Nicaragua %>% mutate(pais = "Nicaragua",
                                  ideologia = c("Derecha", "Derecha", "Derecha",
                                                "Izquierda", "Izquierda", "Izquierda"))

Nicaragua <- Nicaragua %>% mutate(img = c("https://upload.wikimedia.org/wikipedia/commons/thumb/a/ac/Violeta_Chamorro.jpg/200px-Violeta_Chamorro.jpg",
                             "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dd/Arnoldo_Alemán_%28cropped%29.JPG/200px-Arnoldo_Alemán_%28cropped%29.JPG",
                             "https://upload.wikimedia.org/wikipedia/commons/thumb/0/04/Enrique_Bolaños_Geyer_2004_%28cropped%29.jpg/200px-Enrique_Bolaños_Geyer_2004_%28cropped%29.jpg",
                             "https://upload.wikimedia.org/wikipedia/commons/thumb/1/10/01.10_總統與尼加拉瓜總統奧德嘉%28José_Daniel_Ortega_Saavedra%29雙邊會晤_%2832074399712%29_%28cropped%29.jpg/200px-01.10_總統與尼加拉瓜總統奧德嘉%28José_Daniel_Ortega_Saavedra%29雙邊會晤_%2832074399712%29_%28cropped%29.jpg",
                             "https://upload.wikimedia.org/wikipedia/commons/thumb/1/10/01.10_總統與尼加拉瓜總統奧德嘉%28José_Daniel_Ortega_Saavedra%29雙邊會晤_%2832074399712%29_%28cropped%29.jpg/200px-01.10_總統與尼加拉瓜總統奧德嘉%28José_Daniel_Ortega_Saavedra%29雙邊會晤_%2832074399712%29_%28cropped%29.jpg",
                             "https://upload.wikimedia.org/wikipedia/commons/thumb/1/10/01.10_總統與尼加拉瓜總統奧德嘉%28José_Daniel_Ortega_Saavedra%29雙邊會晤_%2832074399712%29_%28cropped%29.jpg/200px-01.10_總統與尼加拉瓜總統奧德嘉%28José_Daniel_Ortega_Saavedra%29雙邊會晤_%2832074399712%29_%28cropped%29.jpg"
                             ))


# Base Panama -------------------------------------------------------------

Panama <- bases[[14]]

names(Panama)[c(3,5,6)] <- c("nombre_del_presidente", "inicio", "fin") 

names(Panama)

Panama <- operacion_fechas(base = Panama, variables = c("inicio", "fin"), 
                           formato = "%d %B %Y") %>% 
  filter(year(inicio)>=1989) %>% 
  select("nombre_del_presidente", "inicio", "fin") %>% 
  mutate(nombre_del_presidente = str_remove(string = nombre_del_presidente, 
                                            pattern = "\\(.*") %>% 
  str_trim())

Panama <- Panama %>% mutate(pais = "Panama",
                            ideologia = c("Centro Izquierda", "Derecha", "Centro Izquierda", 
                                          "Derecha", "Centro Izquierda", "Centro Derecha", 
                                          "Derecha", "Centro Izquierda"))
Panama <- Panama[-1,]

Panama <- Panama %>% mutate(img = c("https://upload.wikimedia.org/wikipedia/commons/thumb/8/83/Guillermo_Endara_1993.jpg/159px-Guillermo_Endara_1993.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/Mr._Ernesto_Perez_Balladares_%28cropped%29.jpg/161px-Mr._Ernesto_Perez_Balladares_%28cropped%29.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a7/Mireya_Moscoso_in_2012.jpg/160px-Mireya_Moscoso_in_2012.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a6/Panama.MartinTorrijos.01.jpg/161px-Panama.MartinTorrijos.01.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8a/Accelerating_Infrastructure_Development_Ricardo_Martinelli_%288410953465%29.jpg/159px-Accelerating_Infrastructure_Development_Ricardo_Martinelli_%288410953465%29.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/d/da/Juan_Carlos_Varela_%282014%29.jpg/161px-Juan_Carlos_Varela_%282014%29.jpg",
                          "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cf/Laurentino_Cortizo_%28cropped%29.jpg/160px-Laurentino_Cortizo_%28cropped%29.jpg"
                          ))

# Base Paraguay -----------------------------------------------------------

Paraguay <- bases[[15]]

names(Paraguay)[c(3,5,6)] <- c("nombre_del_presidente", "inicio", "fin")

Paraguay <- operacion_fechas(base = Paraguay, 
                             variables = c("inicio", "fin"), 
                             formato = "%d %B %Y") %>% 
  filter(year(inicio)>= 1989) %>% 
  select("nombre_del_presidente", "inicio", "fin")

Paraguay <- Paraguay %>% 
  mutate(pais = "Paraguay",
         ideologia = c("Derecha", "Derecha", "Derecha",
                       "Derecha", "Derecha", "Derecha",
                       "Centro Izquierda", "Centro", "Derecha",
                       "Derecha"))


Paraguay <- Paraguay[-1, ]

Paraguay <- Paraguay %>% mutate(img = c("https://upload.wikimedia.org/wikipedia/commons/thumb/7/7d/Andrés_Rodr%C3%ADguez_Pedotti.png/200px-Andrés_Rodr%C3%ADguez_Pedotti.png",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b1/Wasmosy_1990_%28cropped%29.jpg/200px-Wasmosy_1990_%28cropped%29.jpg",
                            "",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/Gonzalez_Macchi_2003.jpg/200px-Gonzalez_Macchi_2003.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b3/Duarte_Frutos.jpg/200px-Duarte_Frutos.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/8/88/Fernando_Lugo_Mendez_Py_%28Copyred%29.jpg/200px-Fernando_Lugo_Mendez_Py_%28Copyred%29.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f2/Federico_Franco.jpg/200px-Federico_Franco.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fb/Horacio_Cartes_con_banda.jpg/200px-Horacio_Cartes_con_banda.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/f/fc/Presidente_de_Paraguay.jpg"
                            ))


# Base Peru ---------------------------------------------------------------

Peru <- bases[[16]]

names(Peru)

Peru <- Peru %>% select(c(3,5,6))

Peru <- operacion_fechas(base = Peru, 
                         variables = c("Inicio del mandato", "Fin del mandato"), 
                         formato = "%d %B %Y") 

Peru <-  Peru %>% rename(inicio = `Inicio del mandato`, 
                         fin = `Fin del mandato`) 

Peru <-  Peru %>% filter(year(inicio) >= 1990) 

Peru <-  Peru %>% mutate(nombre_del_presidente = str_remove(string = Presidente,pattern = "\\[.*") %>% 
                           str_trim()) %>% 
  select(-Presidente)%>% 
  select("nombre_del_presidente", "inicio", "fin")

Peru <- Peru %>% mutate(pais = "Peru",
                        ideologia = c("Derecha", "Derecha", "Derecha",
                                      "Derecha", "Derecha", "Derecha",
                                      "Centro Izquierda", "Derecha", "Izquierda",
                                      "Centro Derecha", "Independiente", "Derecha",
                                      "Centro", "Izquierda"))

Peru <- Peru %>% mutate(img = c("https://upload.wikimedia.org/wikipedia/commons/thumb/7/76/Al_Fujimori.jpg/240px-Al_Fujimori.jpg",
                        "https://upload.wikimedia.org/wikipedia/commons/thumb/7/76/Al_Fujimori.jpg/240px-Al_Fujimori.jpg",
                        "https://upload.wikimedia.org/wikipedia/commons/thumb/7/76/Al_Fujimori.jpg/240px-Al_Fujimori.jpg",
                        "https://upload.wikimedia.org/wikipedia/commons/thumb/7/76/Al_Fujimori.jpg/240px-Al_Fujimori.jpg",
                        "https://upload.wikimedia.org/wikipedia/commons/thumb/7/76/Al_Fujimori.jpg/240px-Al_Fujimori.jpg",
                        "",
                        "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d2/Alejandro_Toledo_%288682%29.jpg/240px-Alejandro_Toledo_%288682%29.jpg",
                        "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c8/Alan_Garc%C3%ADa_presidente_del_Perú.jpg/240px-Alan_Garc%C3%ADa_presidente_del_Perú.jpg",
                        "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cc/Ollanta_Humala_2014.jpg/240px-Ollanta_Humala_2014.jpg",
                        "https://upload.wikimedia.org/wikipedia/commons/thumb/7/79/Pedro_Pablo_Kuczynski_2016_%28cropped%29.jpg/240px-Pedro_Pablo_Kuczynski_2016_%28cropped%29.jpg",
                        "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f9/Mart%C3%ADn_Vizcarra_Cornejo_%28cropped%29_%28cropped%29.png/240px-Mart%C3%ADn_Vizcarra_Cornejo_%28cropped%29_%28cropped%29.png",
                        "https://upload.wikimedia.org/wikipedia/commons/thumb/7/7e/Manuel_Merino_de_Lama_%28cropped%29.jpg/240px-Manuel_Merino_de_Lama_%28cropped%29.jpg",
                        "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c5/Francisco_Sagasti_-_50727430858_%28cropped%29.jpg/240px-Francisco_Sagasti_-_50727430858_%28cropped%29.jpg",
                        "https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/Mensaje_a_la_Nación_-_Pedro_Castillo_%28cropped%29.png/240px-Mensaje_a_la_Nación_-_Pedro_Castillo_%28cropped%29.png"))


# Base Puerto Rico --------------------------------------------------------

Puerto_Rico <- bases[[17]]

names(Puerto_Rico)

Puerto_Rico <- Puerto_Rico %>% select("Gobernador", "Inicio", "Fin")

Puerto_Rico <- operacion_fechas(base = Puerto_Rico, 
                                variables = c("Inicio", "Fin"), 
                                formato = "%d %B %Y") %>% 
  filter(year(Inicio) >= 1989) 

Puerto_Rico <- Puerto_Rico %>% 
  mutate(nombre_del_presidente = str_remove(string = Gobernador, pattern = "\\(.*")) %>% 
  rename(inicio = Inicio, fin = Fin) %>% 
  select(nombre_del_presidente, inicio, fin)

Puerto_Rico <- Puerto_Rico %>% mutate(pais = "Puerto_Rico",
                                      ideologia = c("Centro", "Centro Derecha", "Centro Derecha",
                                                    "Centro", "Centro", "Centro Derecha",
                                                    "Centro", "Centro Derecha", "Centro Derecha",
                                                    "Centro Derecha", "Centro Derecha"))
Puerto_Rico <- Puerto_Rico %>% mutate(img= c("https://upload.wikimedia.org/wikipedia/commons/thumb/4/4e/Rafael_Hernández_Colón%2C_Former_Governor_of_Puerto_Rico.jpg/200px-Rafael_Hernández_Colón%2C_Former_Governor_of_Puerto_Rico.jpg",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/PedroRossello.jpg/200px-PedroRossello.jpg",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/PedroRossello.jpg/200px-PedroRossello.jpg",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/8/87/Puerto_Rican_Governor_Sila_Calderon_at_the_Pentagon%2C_Feb_27%2C_2001.jpg/200px-Puerto_Rican_Governor_Sila_Calderon_at_the_Pentagon%2C_Feb_27%2C_2001.jpg",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1a/Anibal_Acevedo_Vila.jpg/200px-Anibal_Acevedo_Vila.jpg",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/9/93/Fortuno_main.jpg/200px-Fortuno_main.jpg",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/9/95/Alejandro_Garcia_Padilla_-cropped.jpg/200px-Alejandro_Garcia_Padilla_-cropped.jpg",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/ROSSELLO_HUD_%28cropped%29.jpg/200px-ROSSELLO_HUD_%28cropped%29.jpg",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/2/24/Pedro_R._Pierluisi.jpg/200px-Pedro_R._Pierluisi.jpg",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Wanda_Vasquez_%28cropped%29.jpg/200px-Wanda_Vasquez_%28cropped%29.jpg",
                              "https://upload.wikimedia.org/wikipedia/commons/thumb/2/24/Pedro_R._Pierluisi.jpg/200px-Pedro_R._Pierluisi.jpg"))

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
  filter(year(inicio)>=1986) %>% select(nombre_del_presidente, inicio, fin)

Republica_Dominicana <- Republica_Dominicana %>% 
  mutate(pais = "Republica_Dominicana",
         ideologia = c("Centro Derecha", "Centro Derecha", "Centro Derecha",
                       "Centro Izquierda", "Centro Izquierda", "Centro Izquierda",
                       "Centro Izquierda", "Centro Izquierda", "Centro Izquierda", "Centro"))

Republica_Dominicana <- Republica_Dominicana[-c(1,2,6,8),]

Republica_Dominicana <- Republica_Dominicana %>% mutate(img = c("https://upload.wikimedia.org/wikipedia/commons/0/0b/Joaquin_Balaguer_1977.jpg",
                                        "https://upload.wikimedia.org/wikipedia/commons/thumb/9/95/Leonel_2008.jpg/179px-Leonel_2008.jpg",
                                        "https://upload.wikimedia.org/wikipedia/commons/thumb/4/48/Hipolito_2000-2004.jpg/179px-Hipolito_2000-2004.jpg",
                                        "https://upload.wikimedia.org/wikipedia/commons/thumb/9/95/Leonel_2008.jpg/179px-Leonel_2008.jpg",
                                        "https://upload.wikimedia.org/wikipedia/commons/thumb/2/21/Danilo_M.jpg/180px-Danilo_M.jpg",
                                        "https://upload.wikimedia.org/wikipedia/commons/thumb/c/ca/Luis_2020-2024.png/181px-Luis_2020-2024.png"))

# Base Uruguay ------------------------------------------------------------

Uruguay <- bases[[19]]

names(Uruguay)[c(3,5,6)] <- c("nombre_del_presidente", "inicio", "fin")

Uruguay <- operacion_fechas(base = Uruguay, 
                            variables = c("inicio", "fin"), 
                            formato = "%d %B %Y") %>% 
  filter(year(inicio)>=1990) %>% 
  select("nombre_del_presidente", "inicio", "fin")

Uruguay <- Uruguay %>% mutate(pais = "Uruguay",
                              ideologia = c("Centro Derecha","Centro", "Centro",
                                            "Izquierda", "Izquierda", "Izquierda",
                                            "Centro Derecha"))

Uruguay <- Uruguay %>% mutate(img = c("https://upload.wikimedia.org/wikipedia/commons/thumb/6/64/Luisalbertolacalle.jpg/200px-Luisalbertolacalle.jpg",
                           "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a4/PresidenteSanguinetti.jpg/200px-PresidenteSanguinetti.jpg",
                           "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d4/Jorge_Batlle.jpg/200px-Jorge_Batlle.jpg",
                           "https://upload.wikimedia.org/wikipedia/commons/thumb/2/29/Visita_Oficial_del_Presidente_de_Uruguay_3_%28cropped%29.jpg/200px-Visita_Oficial_del_Presidente_de_Uruguay_3_%28cropped%29.jpg",
                           "https://upload.wikimedia.org/wikipedia/commons/thumb/7/79/Pepemujica2.jpg/200px-Pepemujica2.jpg",
                           "https://upload.wikimedia.org/wikipedia/commons/thumb/2/29/Visita_Oficial_del_Presidente_de_Uruguay_3_%28cropped%29.jpg/200px-Visita_Oficial_del_Presidente_de_Uruguay_3_%28cropped%29.jpg",
                           "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e9/Foto_Oficial_Presidente_Luis_Lacalle_Pou_%28cropped%29.jpg/199px-Foto_Oficial_Presidente_Luis_Lacalle_Pou_%28cropped%29.jpg"))

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

Venezuela <- rbind(Venezuela, Venezuela1) %>% 
  mutate(pais = "Venezuela",
         ideologia = c("Izquierda", "Izquierda","Independiente",
                       "Izquierda", "Izquierda","Izquierda",
                       "Izquierda", "Izquierda", "Izquierda",
                       "Izquierda", "Centro Izquierda", "Independiente"))

Venezuela <- Venezuela %>% mutate(img= c("https://upload.wikimedia.org/wikipedia/commons/thumb/1/12/Chavez141610-2.jpg/200px-Chavez141610-2.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/1/12/Chavez141610-2.jpg/200px-Chavez141610-2.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dc/Pedro_Carmona_Juramentándose.png/200px-Pedro_Carmona_Juramentándose.png",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/1/14/Diosdado_Cabello_2013_cropped.jpg/200px-Diosdado_Cabello_2013_cropped.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8a/Hugo_Rafael_Chávez_Fr%C3%ADas.jpeg/200px-Hugo_Rafael_Chávez_Fr%C3%ADas.jpeg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8a/Hugo_Rafael_Chávez_Fr%C3%ADas.jpeg/200px-Hugo_Rafael_Chávez_Fr%C3%ADas.jpeg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8a/Hugo_Rafael_Chávez_Fr%C3%ADas.jpeg/200px-Hugo_Rafael_Chávez_Fr%C3%ADas.jpeg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/4/45/Nicolás_Maduro_2015_%28cropped%29.jpeg/186px-Nicolás_Maduro_2015_%28cropped%29.jpeg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/4/45/Nicolás_Maduro_2015_%28cropped%29.jpeg/186px-Nicolás_Maduro_2015_%28cropped%29.jpeg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/4/45/Nicolás_Maduro_2015_%28cropped%29.jpeg/186px-Nicolás_Maduro_2015_%28cropped%29.jpeg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2f/Juan_Guaidó_na_embaixada_americana_no_Brasil_%28cropped%29.jpg/200px-Juan_Guaidó_na_embaixada_americana_no_Brasil_%28cropped%29.jpg",
                            "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2f/Juan_Guaidó_na_embaixada_americana_no_Brasil_%28cropped%29.jpg/200px-Juan_Guaidó_na_embaixada_americana_no_Brasil_%28cropped%29.jpg"))

# Borrando la base que ya no se necesita
rm(Venezuela1)




# Uniendo a todos los países ----------------------------------------------

paises_base <- bind_rows(Argentina, Bolivia, Brasil, 
      Chile, Colombia, Costa_Rica, 
      Cuba, Ecuador, Salvador, 
      Guatemala, Honduras, Mexico, 
      Nicaragua, Panama, Paraguay, 
      Peru, Puerto_Rico, Republica_Dominicana,
      Uruguay, Venezuela) %>% 
  rename_with(.cols = c(2,3), .fn = ~str_c(.x, "_fecha"))




# Crear una nueva columna con el año de inicio y ponerla como character
paises_base <- paises_base %>% 
  mutate(Year = year(inicio_fecha) %>% as.character())


# UNIR paises con gasto social --------------------------------------------


# crear la ruta para llamar a las bases de gasto social

archivos_gasto <- list.files(path = "tablas_intermedias",pattern = "indicador_",full.names = T) 


#leer las bases y guardar sus ubicaciones en una lista

gasto_social_1 <- archivos_gasto %>% 
  map(read_rds)



# Unir las dos bases por año y país, ordenar y rellenar (para abajo) con los valores faltantes

gasto_social_presidente <- gasto_social_1 %>% 
  map(~{

    paises_base %>% 
      right_join(.x, by = c("Year", "pais")) %>% 
      arrange(pais,Year) %>% 
      group_by(pais) %>% 
      fill(Indicador_valor, .direction = "down") %>% 
      filter(str_detect(Year,"[:alpha:]",negate = T))
    
  })


# Guardar las bases
walk2(.x = archivos_gasto,
     .y = gasto_social_presidente,~{
       
       tabla <- .y
       
       ruta <- .x
       
       write_rds(x = tabla,file = ruta)
     })



## EJEMPLO DE LO QUE SE ESTÁ HACIENDO PREVIAMENTE

# 1. Sacar el año de la fecha de inicio
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



