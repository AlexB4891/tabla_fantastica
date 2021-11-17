



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
library(ggrepel)
library(maptools)


# Datos: ------------------------------------------------------------------

# source("scripts/funcion_limpieza_presidentes_ecuador.R", encoding = "UTF-8")

Educacion <- read_rds("tablas_intermedias/indicador_educación.rds")

# Educacion <- gasto_social_presidente[[2]]

Educacion <- Educacion %>% mutate(pais = case_when(pais=="Brasil"~"Brazil",
                                      pais=="República Dominicana" ~ "Dominican Republic",
                                      pais=="Trinidad Y Tabago" ~ "Trinidad and Tobago",
                                      pais=="Haití" ~ "Haiti",
                                      pais=="Perú" ~ "Peru",
                                      pais=="México" ~ "Mexico",
                                      pais=="Panamá" ~ "Panama",
                                      TRUE~pais))

# load maps ---------------------------------------------------------------

mapa_datos <- ggplot2::map_data("world", regions = c("Argentina", "Bolivia", "Brazil", 
                                                     "Chile", "Colombia", "Costa Rica", 
                                                     "Cuba", "Ecuador", "El Salvador", 
                                                     "Guatemala", "Honduras", "Mexico", 
                                                     "Nicaragua", "Panama", "Paraguay", 
                                                     "Peru", "Puerto Rico", "Dominican Republic",
                                                     "Uruguay", "Venezuela", "Bahamas", "Barbados",
                                                     "Trinidad and Tobago", "Haiti", "Guyana",
                                                     "Jamaica"))




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
         lat = (lat_max+lat_min)/2,
         long = case_when(region == "Ecuador" ~ -78,
                          region == "Chile" ~ -73,
                          region == "Argentina" ~ -66,
                          TRUE ~ long)
         )

mapa_latinoamerica <- function(datos, year, pais_filtro){

# Uniendo las bases de centroides con el gasto en Educacion

datos <- datos %>%
  filter(!is.na(Indicador_valor), Year ==year) %>%
  mutate(Dummi = if_else(condition = pais == pais_filtro, true = 1, false = 0)
         # Dummi = factor(x = Dummi)
  )

breaks <- quantile(datos$Indicador_valor,probs = c(15,30,45,60,75,90)/100)

labels_b <- scales::dollar(round(breaks,2))

centroides <- centroides %>%
  left_join(datos,by = c("region" = "pais")) %>% 
  mutate(label_pais = str_c(region,"\n",round(Indicador_valor,2)))


mapa_datos <-
  mapa_datos %>%
  left_join(datos,by = c("region"="pais"))


# View(mapa_con_filtro)

mapa_destinos <-
  ggplot() +
  mapa +
  geom_polygon(data = mapa_datos,
               mapping = aes(group = group,
                             x = long, 
                             y = lat, 
                             fill=Indicador_valor,
                             alpha = Dummi
                             ),
               colour ="#828070",
               # fill = "#88CDD3",
               show.legend = T,
               size = .3) +
  geom_label_repel(data = centroides,mapping = aes(x = long,
                                                   y = lat,
                                                   label = label_pais),
                   alpha = 0.6, 
                   color = "#16298A",) +
  labs(title = "Gasto Social en Educacion en América Latina") +
  scale_size_continuous(range = c(2,10))+
  scale_alpha(range = c(0.6, 1),guide = 'none') +
  scale_fill_viridis_c(option = "C",
                       guide = guide_legend(nrow = 1, direction = 'horizontal',
                                            label.hjust = 0, label.position = 'bottom',
                                            keywidth = 5.51,
                                            keyheight = 0.75,
                                            title = ""),
                       breaks = as.numeric(breaks),
                       labels = labels_b)  +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",axis.ticks = element_blank())

return(mapa_destinos)

}

plotly::ggplotly(mapa_latinoamerica(datos = Educacion, year = 2014, pais = "Ecuador"))

mapa_latinoamerica(datos = Educacion, year = 2014, pais = "Ecuador")
