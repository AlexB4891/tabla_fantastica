#' seleccion_indicador UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_seleccion_indicador_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::selectInput(inputId =  ns("tipo_indicador"),
                       label = "Ruta para el indicador",
                       choices = c(
                         "../tablas_intermedias/indicador_actividades_recreativas,_cultura_y_religión.rds",
                         "../tablas_intermedias/indicador_educación.rds",                              
                         "../tablas_intermedias/indicador_gasto_social.rds",             
                         "../tablas_intermedias/indicador_protección_del_medio_ambiente.rds",
                         "../tablas_intermedias/indicador_protección_social.rds",  
                         "../tablas_intermedias/indicador_salud.rds",
                         "../tablas_intermedias/indicador_vivienda_y_servicios_comunitarios.rds" 
                         
                       ),
                       selected =  "../tablas_intermedias/indicador_salud.rds")
  )
}
    
#' seleccion_indicador Server Functions
#'
#' @noRd 
mod_seleccion_indicador_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    return(
      list(
        indicador = shiny::reactive(input$tipo_indicador)
      )
    )
  })
}
    
## To be copied in the UI
# mod_seleccion_indicador_ui("seleccion_indicador_ui_1")
    
## To be copied in the server
# mod_seleccion_indicador_server("seleccion_indicador_ui_1")
