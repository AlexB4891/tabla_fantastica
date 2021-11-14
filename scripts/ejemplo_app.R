library(tidyverse)
library(slickR)
library(svglite)
library(shiny)
library(magick)
library(png)
library(jpeg)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
    #   
    #   sliderInput(inputId = "plot_num", 
    #               label = "Number of Plots:", 
    #               min = 1, max = 20, value = 5),
    #   
    #   sliderInput(inputId = "n_obs", 
    #               label = "Number of observations:", 
    #               min = 10, max = 500, value = 100),
    #   
    #   shiny::radioButtons('slick_type',
    #                       label = 'Carousel Type',
    #                       choices = c('single','stack','synch'),
    #                       selected = 'single',
    #                       inline = TRUE),
    #   
    #   shiny::verbatimTextOutput('current')
    ),
    mainPanel(
      
      slickROutput("slick_output",width='100%',height='200px')
      
    )
  )
)


server <- function(input, output) {
  
  # Create content for the carousel
  
  plots <- eventReactive(c(input$n_obs,input$plot_num),{

    map(list.files("imagenes/Ecuador/",full.names = T),~{

      # if(str_detect(.x,pattern = "jpg")){
      #
      # readJPEG(source = .x)
      # }else{
      #
      # readPNG(source = .x)
      # }

      # browser()

    OpenImageR::readImage(path = .x) %>%
        xmlSVG()

    },simplify = FALSE)
  })
  
  
  # plots <- eventReactive(c(input$n_obs,input$plot_num),{
  #   
  #   replicate(input$plot_num,{
  #     
  #     xmlSVG({hist(rnorm(input$n_obs),
  #                  col = 'darkgray',
  #                  border = 'white')},
  #            standalone=TRUE)
  #     
  #   },simplify = FALSE)
  # })
  
  # renderSlickR (We create the slickR objects here)
  
  output$slick_output <- renderSlickR({
    
    x <- slickR(plots(),
                slideId = 'myslick',
                height = 600,
                width = '50%') + 
      settings(slidesToShow=3,centerMode=TRUE)
    
    switch(input$slick_type,
           'single' = x,
           'stack'  = x %stack% x,
           'synch'  = x %synch% x
    )
    
  })
  
  # Observe the active slick
  
  # The htmlwidget is observed by shiny and information can be retrieved. 
  
  # Using the output name you set for the `renderSlick` object in this example
  # it is `output$slick_output`
  
  # Using this you can interact server-side "on click" of the active carousel
  # by accessing elements in `input$slick_output_current$`
  
  # `.clicked_slide`   : The index of the clicked element|
  # `.relative_clicked`: The relative position of the clicked element|
  # `.center_slide`    : The index of the center element|
  # `.total_slide`     : The total number of elements in the carousel|
  # `.active_slide`    : The ID of the active carousel|
  
  # We will store this information in a new reactive environment
  active_slick <- shiny::reactiveValues()
  
  shiny::observeEvent(input$slick_output_current,{
    
    clicked_slide    <- input$slick_output_current$.clicked
    relative_clicked <- input$slick_output_current$.relative_clicked
    center_slide     <- input$slick_output_current$.center
    total_slide      <- input$slick_output_current$.total
    active_slide     <- input$slick_output_current$.slide
    
    if(!is.null(clicked_slide)){
      active_slick$clicked_slide    <- clicked_slide
      active_slick$center_slide     <- center_slide
      active_slick$relative_clicked <- relative_clicked
      active_slick$total_slide      <- total_slide
      active_slick$active_slide     <- active_slide
    }
  })
  
  # Show in the UI the values in active_slick
  
  output$current <- renderText({
    l <- shiny::reactiveValuesToList(active_slick)
    paste(gsub('_',' ',names(l)), unlist(l),sep=' = ',collapse='\n')
  })
  
}
shinyApp(ui = ui, server = server)


bind_rows(Argentina, Bolivia, Brasil, 
          Chile, Colombia, Costa_Rica, 
          Cuba, Ecuador, Salvador, 
          Guatemala, Honduras, Mexico, 
          Nicaragua, Panama, Paraguay, 
          Peru, Puerto_Rico, Republica_Dominicana,
          Uruguay, Venezuela) 

imgs_bare <- lapply(imagenes_ecuador,function(x){
  x <- str_c("https:")
  
  htmltools::tags$img(src = x)
})

slickR::slickR(imgs_bare)
