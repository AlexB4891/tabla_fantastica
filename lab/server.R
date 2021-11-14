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
  
  tabla_seria <- reactive({
    Ecuador %>% filter(nombre_del_presidente == presidente())
  })
  
  output$tabla_filtrada <- renderDT({
    tabla_seria()
  })
  
  grafico_serie <- reactive({
    serie_de_tiempo_resaltada(datos = indicador_gasto_social, 
                              variables_resaltar = list(nombre_del_presidente = presidente()),
                              variable_filtro = list(pais = "Ecuador"), 
                              variable_x = "Year", 
                              variable_y = "Indicador_valor") 
  })
  
  output$serie_de_tiempo <- renderPlotly({
    ggplotly(grafico_serie()$plot)
  })
  
  output[["imgName"]] <- renderText({
    paste0("CURRENT IMAGE: ", presidente())
  })
  
}