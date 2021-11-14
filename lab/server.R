# Define server logic required to draw a histogram
server <- function(input, output) {
  
  imgs <- list.files("../imagenes/Ecuador/", full.names = TRUE)
  
  # browser()
  
  output$slick_output <- renderSlickR({
    slickR(imgs)
  })
  
  
  presidente <- reactive({
  # browser()
    str_remove(imgs[input$imageIndex],"\\..{3,4}$")
  })
  
  output[["imgName"]] <- renderText({
    paste0("CURRENT IMAGE: ", presidente())
  })
  
}