
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$script(HTML(js))
  ),
  # Application title
  titlePanel("Social Expense Lab"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      slickROutput("slick_output",width='100%',height='200px'),width = 2
    ),
    
    # Show a plot of the generated distribution
    mainPanel(width = 3,
          textOutput("imgName")
    )
  )
)


