



# Librerias ---------------------------------------------------------------

library(tidyverse)
library(janitor)
library(waffle)
library(hrbrthemes)
library(extrafont)
library(assertthat)
library(igraph)
library(ggraph)
library(ggmap)
library(raster)
library(maptools)


# Datos: ------------------------------------------------------------------

source("scripts/funcion_limpieza_presidentes_ecuador.R", encoding = "UTF-8")

Educacion <- gasto_social_presidente[[2]]

# load maps ---------------------------------------------------------------

mapa_datos <- ggplot2::map_data("world", regions = c("Argentina", "Bolivia", "Brazil", 
                                                     "Chile", "Colombia", "Costa_Rica", 
                                                     "Cuba", "Ecuador", "Salvador", 
                                                     "Guatemala", "Honduras", "Mexico", 
                                                     "Nicaragua", "Panama", "Paraguay", 
                                                     "Peru", "Puerto_Rico", "Republica_Dominicana",
                                                     "Uruguay", "Venezuela"))




# first map of Brazil -----------------------------------------------------

mapa <- borders("world", regions = c("Argentina", "Bolivia", "Brazil", 
                                     "Chile", "Colombia", "Costa_Rica", 
                                     "Cuba", "Ecuador", "Salvador", 
                                     "Guatemala", "Honduras", "Mexico", 
                                     "Nicaragua", "Panama", "Paraguay", 
                                     "Peru", "Puerto_Rico", "Republica_Dominicana",
                                     "Uruguay", "Venezuela"),
                fill = "#eceded", colour = "#828070",size = 0.3)

centroides <- mapa$data %>%
  group_by(region) %>%
  summarise_at(.vars = c("long","lat"),list(min = min,max= max),na.rm = T) %>%
  mutate(long = (long_max+long_min)/2,
         lat = (lat_max+lat_min)/2
         # ,
         # long = case_when(region == "Ecuador" ~ -78,
         #                  region == "Chile" ~ -73,
         #                  region == "Argentina" ~ -66,
         #                  TRUE ~ long)
         )


# Uniendo las bases de centroides con el gasto en Educacion

centroides <- centroides %>%
  left_join(Educacion,by = c("region" = "pais"))


mapa_datos <-
  mapa_datos %>%
  left_join(Educacion,by = c("region"="pais"))

# red <- mapa_datos %>%
#   mutate(label = round(Indicador_valor,2),
#          label = str_c(region,"\n",label))

mapa_destinos <-
  ggplot() +
  mapa +
  geom_polygon(data = mapa_datos %>%
                 filter(!is.na(Indicador_valor)),
               mapping = aes(group = group,
                             x = long, 
                             y = lat, 
                             color=Indicador_valor,
                             alpha = Indicador_valor),
               # colour ="#828070",
               fill = "#88CDD3",
               show.legend = F,
               size = .3) +
  theme_bw() +
 labs(title = "Gasto Social en Educacion en América Latina") +
  scale_size_continuous(range = c(2,10))+
  theme(panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.2,0.3),axis.ticks = element_blank())

