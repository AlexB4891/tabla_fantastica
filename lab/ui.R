options(encoding = "UTF-8")
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$script(HTML(js_ecu))
  ),
  # Application title
  titlePanel("Social Expense Lab"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      slickROutput("slickEcuador",width='100%',height='200px')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(width = 3,
          fluidRow(textOutput("imgName"),
                   plotOutput("serie_de_tiempo"),
                   DTOutput("tabla_filtrada")
                   )
    )
  )
)


