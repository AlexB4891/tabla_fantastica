options(encoding = "UTF-8")
# Define UI for application that draws a histogram
ui <- semanticPage(
  tags$head(
    tags$script(HTML(js_ecu))
  ),
  # Application title
  titlePanel("Social Expense Lab"),
  
  # Sidebar with a slider input for number of bins 
  sidebar_layout(
    
    sidebar_panel(
      slickROutput("slickEcuador",width='100%',height='200px'),
      width = 2
    ),
    
    # Show a plot of the generated distribution
    main_panel(width = 3,
          fluidRow(textOutput("imgName"),
                   DTOutput("tabla_filtrada")),
          fluidRow(plotlyOutput("serie_de_tiempo", width='100%'))
    )
  )
)


