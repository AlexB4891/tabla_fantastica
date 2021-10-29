
# -------------------------------------------------------------------------#
# Depurar la base de datos de la CEPAL ------------------------------------#
# -------------------------------------------------------------------------#

# Librerías ---------------------------------------------------------------

library(tidyverse)
library(readxl)


# Depurando el primer país ------------------------------------------------

# Indicar el número de celda de inicio y fin de la base en excel
inicio <- seq(from = 6,to = 254,by = 10)
fin <- seq(from = 14,to = 254,by = 10)

# Pegar la letra "A" junto al primer valor del inicio para que el nombre de 
# la celda sea la correcta, lo mismo se hace con la última celda (BI254)
inicio <- str_c("A",inicio)
fin <- str_c("BI",fin)


# Ejemplo para la primera tabla, para el resto usar un map o un for 
# Instrucciones:
# 1. Construir una función con tres argumentos: 
  # path (ruta del archivo)
  # inicio
  # fin


# Se crea el rango de la tabla 
# (en excel se marca el rango de inicio y fin con dos puntos)
rango_1 <- str_c(inicio[1],":",fin[1])

# Cargar los datos
tabla_1 <- read_excel("tablas_crudas/gasto_social_cepal_18102021.xlsx",range = rango_1)



# Asi debe quedar dentro de la funcion:
#
# rango_1 <- str_c(inicio,":",fin)
# 
# tabla_1 <- read_excel(path,range = rango_1)


# Como la primera celda es el nombre del país, entonces se guarda en un objeto
pais <- names(tabla_1)[1]

# Se crea un vector cuyos elementos son los de la tibble
# slice(): Separa la base y tomar una parte de esta, en este caso se separa por cada elemento
# unlist: quitar la lista
nombres_1 <- slice(tabla_1,1) %>% unlist

# guardar el tamaño de los vacios
indicador <- which(is.na(nombres_1)) %>% length

# crear un vector con la cantidad de vacíos que existen
vacios_1 <- str_c("vacio_",1:indicador)

# En las posiciones vacías del vector de datos se sobreescriben con el vector de vacios creado
nombres_1[is.na(nombres_1)] <- vacios_1

# Guardar la cantidad de datos duplicados en el vector
indicador <-   which(duplicated(nombres_1)) %>% length

# Guardar los duplicados con ese mismo nombre con el tamaño de los duplicados
duplicados_1 <- str_c("duplicados_",1:indicador)

# Sobreescribir los duplicados con su respectivo nombre en cada respectiva posicion
nombres_1[duplicated(nombres_1)] <- duplicados_1


# separar los elementos del tibble y quitar el primer elemento
# renombrar cada elemento usando el vector creado: nombres_1
# eliminar aquellos: vacios, duplicados y con [D]
# eliminar los espacios (de existir) adelante y detrás de cada objeto
preliminar <- slice(tabla_1, -1) %>%
  rename_with(.fn = ~nombres_1) %>% 
  select(-matches("^vacio"),-matches("^duplicado"),-matches("\\[D\\]")) %>% 
  mutate(across(.cols = 1,str_trim, side = "both"))


# Crear la tabla con 4 columnas
# Renombrar las columnas
# Incluir el pais
preliminar %>% 
  pivot_longer(cols = 2:ncol(preliminar)) %>% 
  rename_with(~c("funciones_gobierno","year","indicador_valor")) %>% 
  mutate(pais = pais)


# return(preliminar)




# Ejemplo practico con iris: ----------------------------------------------


# Creando una funcion usando la base iris:
# Separar la base y tomar una parte de esta en usando un rango específico
# Crear el rango: inicio:fin e imprimir en una nueva columna de la base

secciones_de_iris <- function(tabla, inicio, fin){
  
  seccion <- tabla %>% 
    slice(inicio:fin) %>% 
    mutate(Species_2 = str_c(inicio,":",fin))
  
  return(seccion)
  
}

# Aplicar la funcion
secciones_de_iris(tabla = iris, inicio = 3, fin =16)

# Aplicar la funcion a toda la base iris
inicio_1 <- seq(6,50, by = 9)

fin_1 <- seq(61,100, by = 9)

map2(.x = inicio_1, 
     .y = fin_1, 
     .f = secciones_de_iris,tabla = iris)



# Función -----------------------------------------------------------------

secciones_cepal <- function(path, inicio, fin){
  
  inicio <- str_c("A",inicio)
  
  fin <- str_c("BI",fin)
  
  rango_1 <- str_c(inicio,":",fin)
  
  tabla_1 <- read_excel(path, range = rango_1)
  
  pais <- names(tabla_1)[1]
  
  nombres_1 <- slice(tabla_1,1) %>% unlist
  
  indicador <- which(is.na(nombres_1)) %>% length
  
  vacios_1 <- str_c("vacio_",1:indicador)
  
  nombres_1[is.na(nombres_1)] <- vacios_1
  
  indicador <- which(duplicated(nombres_1)) %>% length
  
  duplicados_1 <- str_c("duplicados_",1:indicador)
  
  nombres_1[duplicated(nombres_1)] <- duplicados_1
  
  preliminar <- slice(tabla_1, -1) %>%
    rename_with(.fn = ~nombres_1) %>% 
    select(-matches("^vacio"),-matches("^duplicado"),-matches("\\[D\\]")) %>% 
    mutate(across(.cols = 1,str_trim, side = "both"))
  
  preliminar <- preliminar %>% 
    pivot_longer(cols = 2:ncol(preliminar)) %>% 
    rename_with(~c("Funciones_gobierno","Year","Indicador_valor")) %>% 
    mutate(Pais = pais,
           Rango = str_c(inicio,":",fin))
  
  preliminar
}




# Aplicando la función ----------------------------------------------------


# ejemplo1 <- secciones_cepal(path = "tablas_crudas/gasto_social_cepal_18102021.xlsx", 
#                 inicio = 6, 
#                 fin = 254)
# ejemplo1
# 
# ejemplo2 <- secciones_cepal(path = "tablas_crudas/gasto_social_cepal_18102021.xlsx", 
#                             inicio = 16, 
#                             fin = 254)
# ejemplo2
# 
# ejemplo3 <- secciones_cepal(path = "tablas_crudas/gasto_social_cepal_18102021.xlsx", 
#                             inicio = 26, 
#                             fin = 254)
# ejemplo3
# 
# ejemplo4 <- secciones_cepal(path = "tablas_crudas/gasto_social_cepal_18102021.xlsx", 
#                             inicio = 36, 
#                             fin = 254)
# ejemplo4


# Aplicar a toda la base --------------------------------------------------

inicio_2 <- seq(from = 6,to = 254,by = 10)

fin_2 <- seq(from = 14,to = 254,by = 10)

indi_gst_social <- map2(.x = inicio_2, 
     .y = fin_2, 
     .f = secciones_cepal, path = "tablas_crudas/gasto_social_cepal_18102021.xlsx") %>% 
  reduce(bind_rows) 



# Guardado ----------------------------------------------------------------


# Guardar la base en formato comprimido y "gz" es para comprimir más pero
# manteniendo la calidad

write_rds(x = indi_gst_social, 
          file = "tablas_intermedias/gasto_social_cepal.rds", compress = "gz")



# SOLO GASTO SOCIAL -------------------------------------------------------

# Elegir solo de Gasto social y las variables de interes

gasto_social <- indi_gst_social %>% 
  
# Aquí se ve que todos tienen "GOBIERNO CENTRAL - "
  
# Dejar solo los nombres de los paises
  mutate(Pais = str_remove(string = Pais, 
                           pattern = "GOBIERNO[:space:]CENTRAL[:space:]\\-[:space:]"),
         Pais = str_remove(string = Pais, pattern = "\\(.*") %>% 
           str_trim()) %>% 
# Quitar las tildes y espacios para homologar la base
  mutate(Pais = case_when(Pais == "Costa Rica"~"Costa_Rica",
                          Pais == "El Salvador"~"Salvador",
                          Pais == "México" ~ "Mexico",
                          Pais == "Panamá" ~ "Panama",
                          Pais == "Perú" ~ "Peru",
                          Pais == "República Dominicana" ~ "Republica_Dominicana",
                          TRUE ~ Pais)) %>% 
# Cambiar de mayúsculas a tipo oracion
  mutate(Pais = stringr::str_to_title(Pais)) %>% 
# Convertir a número el indicador
  mutate(Indicador_valor = as.double(Indicador_valor)) %>% 

  split(.$Funciones_gobierno) %>% 
  map(~.x %>% 
        select(Year, Indicador_valor, Pais,Funciones_gobierno) %>% 
        rename(pais = Pais) )
  
# Guardamos los indicadores:

iwalk(.x = gasto_social,~{
  
 ruta <-  str_replace_all(.y," ","_") %>%
    str_to_lower() %>%
    str_c("tablas_intermedias/indicador_",.,".rds")
  
 write_rds(x = .x,file = ruta)
 
  })


# Verificar los nombres
table(gasto_social$pais)
summary(gasto_social)





















