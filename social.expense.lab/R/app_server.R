#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
 
  indicadores <- mod_seleccion_indicador_server("seleccion_indicador_ui_1")
  
  
  mod_tabla_central_server("tabla_central_ui_1",
                           tipo_indicador =   indicadores$indicador)
  
}
