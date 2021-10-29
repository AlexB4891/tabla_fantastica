#' tabla_central UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tabla_central_ui <- function(id){
  ns <- NS(id)
  tagList(
   DT::DTOutput(ns("tabla"))
  )
}
    
#' tabla_central Server Functions
#'
#' @noRd 
mod_tabla_central_server <- function(id,
                                     tipo_indicador){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    output$tabla <- DT::renderDT({
      
      readr::read_rds(tipo_indicador())
      
    })
    
  })
}
    
## To be copied in the UI
# mod_tabla_central_ui("tabla_central_ui_1")
    
## To be copied in the server
# mod_tabla_central_server("tabla_central_ui_1")
