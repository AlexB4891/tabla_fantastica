library(shiny)
library(slickR)

js <- "
$(document).ready(function(){
  var ss = document.getElementById('slickr');
  // create an observer instance
  var observer = new MutationObserver(function(mutations) {
    var index = $(ss).find('.slick-current').data('slick-index');
    Shiny.setInputValue('imageIndex', parseInt(index)+1);
  });
  // configuration of the observer
  var config = {subtree: true, attributes: true};
  // observe 
  observer.observe(ss, config);
})
"

ui <- fluidPage(
  tags$head(
    tags$script(HTML(js))
  ),
  textOutput("imgName"),
  tags$hr(),
  tags$div(
    slickROutput("slickr", width="500px"),
    style = "margin-left:100px;"
  )
)

server <- function(input, output) {
  
  imgs <- list.files("../imagenes/Ecuador/", full.names = TRUE)
  
  # browser()
  
  output[["slickr"]] <- renderSlickR({
    slickR(imgs)
  })
  
  presidente <- reactive({
    # browser()
    # str_remove(
      imgs[input[["imageIndex"]]]
    # ,"\\..{3,4}$") %>% 
    #   str_extract("(?<=\\/\\/).*$")
  })
  
  
  output[["imgName"]] <- renderText({
    paste0("CURRENT IMAGE: ", basename(imgs[input[["imageIndex"]]]),"; ",presidente())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
