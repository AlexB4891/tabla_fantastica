dir.create("imagenes/Ecuador")

tabla_indicadores <- read_excel("tablas_intermedias/ejemplo_1.xlsx")

fotos_enlace <- tabla_indicadores %>% 
  pull(link_foto) %>% 
  na.omit() %>% 
  unclass()


download.file(url = fotos_enlace[2],destfile = "imagenes/Ecuador/JamilMahuad.jpg")
