# Define server logic required to draw a histogram
server <- function(input, output) {
  
  imgs <- list.files("../imagenes/Ecuador/", full.names = TRUE)
  
  # browser()
  
  output$slick_output <- renderSlickR({
    slickR(imgs)
  })
  
  
  presidente <- reactive({
  # browser()
    str_remove(imgs[input$imageIndex],"\\..{3,4}$") %>% 
      str_extract("(?<=\\/\\/).*$")
  })
  
  tabla_serie <- reactive({
    Ecuador %>% filter(nombre_del_presidente == presidente())
  })
  
  grafico_serie <- reactive({
    serie_de_tiempo_resaltada(datos = indicador_gasto_social, 
                              variables_resaltar = list(nombre_del_presidente = presidente()),
                              variable_filtro = list(pais = "Ecuador"), 
                              variable_x = "Year", 
                              variable_y = "Indicador_valor") 
    
  })
  
  output$tabla_filtrada <- renderDT({
    
   # browser()
    
    tibble(
      País = "Ecuador",
      Presidentes = list(slickROutput("slick_output"))
      # ,
      # `Serie de tiempo` = list(ggplotly(grafico_serie()$plot))
    ) %>% 
      mutate(Presidentes = list(Presidentes %>%
                                  as.tags() %>%
                                  as.character() %>%
                                  htmltools::HTML())
             # ,
             # `Serie de tiempo` =  list(`Serie de tiempo` %>%
             #                             as.tags() %>%
             #                             as.character() %>%
             #                             htmltools::HTML())
             )
  }, escape = FALSE,options = list(
    fnDrawCallback = htmlwidgets::JS(
      '
function(){
  HTMLWidgets.staticRender();
}
'
    )))
  
  
  output$serie_de_tiempo <- renderPlotly({
    
  })
  
  output[["imgName"]] <- renderText({
    paste0("CURRENT IMAGE: ", presidente())
  })
  
}