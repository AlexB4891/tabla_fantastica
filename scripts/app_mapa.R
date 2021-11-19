library(shiny)
library(shiny.semantic)
library(tidyverse)
library(DT)
library(janitor)
library(waffle)
library(hrbrthemes)
library(extrafont)
library(assertthat)
library(igraph)
library(ggraph)
library(ggmap)
library(ggrepel)
library(maptools)


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
    # labs(title = "Gasto Social en Educacion en América Latina") +
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




ui <- semanticPage(
  title = titlePanel(span(img(src = "hex ergos-01.png", height = 35), "Observatorio del gasto social")),
  sidebar_layout(
    sidebar_panel(
      h2("Controles:"),
      shiny.semantic::selectInput(inputId = "indicador",
                                  label = "Elige el indicador",
                                  choices = c("Gasto social",
                                              "Educación",
                                              "Protección del medio ambiente",
                                              "Vivienda y servicios comunitarios",
                                              "Salud",
                                              "Actividades recreativas, cultura y religión",
                                              "Protección social")),
      
      shiny.semantic::selectInput(inputId = "pais", 
                                  label = "Elige el pais", 
                                  choices = c("Argentina", "Bolivia", "Brasil", 
                                              "Chile", "Colombia", "Costa Rica", 
                                              "Cuba", "Ecuador", "El Salvador",
                                              "Guatemala", "Honduras", "México", 
                                              "Nicaragua", "Panama", "Paraguay", 
                                              "Peru", "Puerto Rico", "Republica Dominicana", 
                                              "Uruguay", "Venezuela", "Venezuela")),
      p("Elige el año de análisis"),
      shiny.semantic::slider_input(input_id = "year", 
                                   custom_ticks = c(1990,1995,2000,2005,2010,2015,2020) ,
                                   value = 2014, 
                                   min = 1990, 
                                   max = 2019, 
                                   step = 1)
    ),
    main_panel(
      shiny.semantic::tabset(
        tabs =
          list(
            list(menu = "Análisis geografico", content = tagList(plotOutput("mapa",width = "100%")) ,id = "geo_exp"),
            list(menu = "Tabal de datos", content = tagList(DTOutput("tabla_indicadores")), id = "tab_exp")
          ),
        active = "second_tab",
        id = "exampletabset"
      )
      
      
    )
  )
  
)

server <- function(input, output, session) {
  
  ruta_datos <- reactive({
    
    ruta <- case_when(input$indicador == "Gasto social" ~ "tablas_intermedias/indicador_gasto_social.rds",
              input$indicador == "Educación" ~ "tablas_intermedias/indicador_educación.rds" ,
              input$indicador == "Protección del medio ambiente" ~ "tablas_intermedias/indicador_protección_del_medio_ambiente.rds",
              input$indicador == "Vivienda y servicios comunitarios" ~ "tablas_intermedias/indicador_vivienda_y_servicios_comunitarios.rds",
              input$indicador == "Salud" ~ "tablas_intermedias/indicador_salud.rds" ,
              input$indicador == "Actividades recreativas, cultura y religión" ~ "tablas_intermedias/indicador_actividades_recreativas,_cultura_y_religión.rds",
              input$indicador == "Protección social" ~ "tablas_intermedias/indicador_protección_social.rds" )
    
    str_c("../",ruta)
    
  })
  
  
  datos_indicador <- reactive({
    read_rds(ruta_datos())
  })
  
  datos_para_mapa <- reactive({
    datos_indicador() %>% mutate(pais = case_when(pais=="Brasil"~"Brazil",
                                                    pais=="República Dominicana" ~ "Dominican Republic",
                                                    pais=="Trinidad Y Tabago" ~ "Trinidad and Tobago",
                                                    pais=="Haití" ~ "Haiti",
                                                    pais=="Perú" ~ "Peru",
                                                    pais=="México" ~ "Mexico",
                                                    pais=="Panamá" ~ "Panama",
                                                    TRUE~pais))
  })
  
  
  output$tabla_indicadores <- renderDT({
    
    datos_indicador() %>% filter(pais == input$pais)
    
  })
  
  
  output$mapa <- renderPlot({
    mapa_latinoamerica(datos = datos_para_mapa(), year = input$year, pais = input$pais)
  })
  
}

shinyApp(ui, server)