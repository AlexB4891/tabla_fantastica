library(tidyverse)
library(readxl)

inicio <- seq(from = 6,to = 254,by = 10)

fin <- seq(from = 14,to = 254,by = 10)

inicio <- str_c("A",inicio)

fin <- str_c("BI",fin)


# Ejemplo para la primera tabla, para el resto usar un map o un for -------

# Instrucciones:
# 1. Construir una función con tres argumentos: 
  # path (ruta del archivo)
  # inicio
  # fin


rango_1 <- str_c(inicio[1],":",fin[1])

tabla_1 <- read_excel("tablas_crudas/gasto_social_cepal_18102021.xlsx",range = rango_1)

# Asi debe quedar dentro de la funcion:

# rango_1 <- str_c(inicio,":",fin)
# 
# tabla_1 <- read_excel(path,range = rango_1)



pais <- names(tabla_1)[1]


nombres_1 <- slice(tabla_1,1) %>% unlist


indicador <-   which(is.na(nombres_1)) %>% length

vacios_1 <- str_c("vacio_",1:indicador)

nombres_1[is.na(nombres_1)] <- vacios_1


indicador <-   which(duplicated(nombres_1)) %>% length

duplicados_1 <- str_c("dplicados_",1:indicador)

nombres_1[duplicated(nombres_1)] <- duplicados_1



preliminar <- slice(tabla_1, -1) %>%
  rename_with(.fn = ~nombres_1) %>% 
  select(-matches("^vacio"),-matches("^dplicado"),-matches("\\[D\\]")) %>% 
  mutate(across(.cols = 1,str_trim,side = "both"))

preliminar %>% 
  pivot_longer(cols = 2:ncol(preliminar)) %>% 
  rename_with(~c("funciones_gobierno","year","indicador_valor")) %>% 
  mutate(pais = pais)


# return(preliminar)


# Ejemplo practico:

secciones_de_iris <- function(tabla, inicio,fin){
  
  
  seccion <- iris %>% 
    slice(inicio:fin) %>% 
    mutate(Species_2 = str_c(inicio,":",fin))
  
  
  return(seccion)
  
}


secciones_de_iris(tabla = iris,inicio = 3,fin =16)


inicio_1 <- seq(6,50,by = 9)

fin_1 <- seq(61,100,by = 9)

map2(.x = inicio_1,.y = fin_1,.f = secciones_de_iris,tabla = iris)
