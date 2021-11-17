library(shiny)
library(shiny.semantic)

ui <- semanticPage(
  sidebar_layout(
    sidebar_panel(
      shiny.semantic::selectInput(inputId = "indicador",
                                  label = "Elige el indicador",
                                  choices = c("Gasto social",
                                              "Educación",
                                              "Protección del medio ambiente",
                                              "Vivienda y servicios comunitarios",
                                              "Salud",
                                              "Actividades recreativas, cultura y religión",
                                              "Protección social"))
    ),
    main_panel(
      DTOutput("tabla_indicadores")
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
  
  output$tabla_indicadores <- renderDT({
    
    datos_indicador()
    
  })
}

shinyApp(ui, server)