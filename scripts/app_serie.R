library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(

    mainPanel(
      
      slickROutput("slick_output",width='100%',height='200px')
      
    )
  )
)

server <- function(input, output, session) {
  
  paleta_colores <- eventReactive(input$numero){
    
    paleta <- colorRampPalette(colors = c("#03071e","#9d0208","#f48c06"))
    
    paleta_n <- paleta(n = numero)
    
    
    return(paleta_n)
  }
  
  
  
  serie_de_tiempo_resaltada <- eventReactive(input$datos,
                                             input$variables_resaltar,
                                             input$variable_filtro,
                                             input$variable_x,
                                             input$variable_y){
    tabla_mod <- datos %>% 
      ungroup() %>% 
      dplyr::filter(dplyr::if_any(.cols = dplyr::any_of(names(variable_filtro)), ~.x == unlist(variable_filtro))) %>% 
      dplyr::mutate(
        dplyr::across(
          .col = names(variables_resaltar),
          .fns = list(indicador = ~ dplyr::if_else(condition = .x == unlist(variables_resaltar),
                                                   true = 1,
                                                   false = 0,
                                                   missing = 0)),
          .names = "{.fn}"
        ),
        indicador = factor(indicador)
      )
    
    
    limites_ideo <- tabla_mod %>%
      mutate(Year = as.numeric(Year)) %>% 
      fill(ideologia) %>% 
      arrange(Year) %>% 
      mutate(indicador_tend = if_else(ideologia != lag(ideologia) | is.na(lag(ideologia)), 1,0)) %>% 
      filter(indicador_tend == 1,
             !is.na(ideologia))
    
    limites_ideo <- limites_ideo %>% 
      mutate(lag_year = lag(Year))
    
    conteo_ideo <- limites_ideo %>% ungroup() %>% summarise(np = n_distinct(ideologia))
    
    grafico_mod <- ggplot2::ggplot(data = tabla_mod) +
      ggplot2::geom_line(mapping = ggplot2::aes_string(x = variable_x,
                                                       y = variable_y,
                                                       group = "indicador",
                                                       color = "indicador")) +
      ggplot2::geom_point(mapping = ggplot2::aes_string(x = variable_x,
                                                        y = variable_y,
                                                        group = "indicador",
                                                        color = "indicador"),size = 3) +
      ggplot2::scale_color_manual(values = c("#FFC300",
                                             "#581845")) +
      geom_rect(data = limites_ideo,
                mapping = aes(xmin = lag_year,
                              xmax = Year,
                              group = ideologia,
                              fill = ideologia,
                              ymin = -Inf,
                              ymax = Inf),alpha = 0.4) +
      scale_fill_manual(values = paleta_colores(conteo_ideo$np))
    
    
    resultado <- list(
      tabla = tabla_mod,
      plot = grafico_mod
    )
    
    return(resultado)
  }
  
  
  
  
}

shinyApp(ui, server)