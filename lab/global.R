

# ------------------------------------------------------------------------- #
#                     GLOBAL: Determina el environment                      #
# ------------------------------------------------------------------------- #


# Librerias ---------------------------------------------------------------

# Desarrollo de la app:
require(shiny)
require(shiny.semantic)
require(shinycssloaders)
require(shinyjs)
require(shinydashboard)
require(slickR)

# Manejo de datos:
require(tidyverse)
require(lubridate)

# Formatos:
require(scales)
require(dichromat)
require(DT)



# Insumos -----------------------------------------------------------------


paises <- c("Argentina", "Bolivia", "Brasil", 
            "Chile", "Colombia", "Costa Rica", 
            "Cuba", "Ecuador", "El Salvador",
            "Guatemala", "Honduras", "México", 
            "Nicaragua", "Panama", "Paraguay", 
            "Peru", "Puerto Rico", "Republica Dominicana", 
            "Uruguay", "Venezuela") %>% 
  set_names()


# Otras fuentes: ----------------------------------------------------------

js <- "
$(document).ready(function(){
  var ss = document.getElementById('slick_output');
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

# Lectura de los datos ----------------------------------------------------


indicador_gasto_social <- read_rds("../tablas_intermedias/indicador_gasto_social.rds")

Ecuador <- indicador_gasto_social %>% filter(pais == "Ecuador")

# Funciones creadas -------------------------------------------------------


paleta_colores <- function(numero){
  
  paleta <- colorRampPalette(colors = c("#03071e","#9d0208","#f48c06"))
  
  paleta_n <- paleta(n = numero)
  
  
  return(paleta_n)
}

#' Resalta una porción de la serie de tiempo de acuerdo a una variable de filtro
#'
#' @param datos Un objeto de tipo`tibble` al cual se va a filtrar y con el cual se genera la serie de tiempo
#' @param variables_filtro Una lista nombrada. El nombre del elemento de cada lista debe ser una variable en `datos` y 
#' el contenido del elemento de la lista debe ser un vector con las categorias para el filtro.
#' @param variable_x Cadena con el nombre de la variable del eje x
#' @param varaible_y Cadena con el nombre de la variable del eje y
#'
#' @return
#' 
#' Una lista con:
#' * La tabla modificada
#' * Un gráfico de series de tiempo `ggplot`
#' @export
#'
#' @examples
#' serie_de_tiempo_resaltada(datos = tibble(iris),
#'                           variables_filtro = list(Species = "setosa"),
#'                           variable_x = "Sepal.Length",
#'                           variable_y = "Sepal.Width")
#'                           
serie_de_tiempo_resaltada <- function(datos,
                                      variables_resaltar,
                                      variable_filtro,
                                      variable_x,
                                      variable_y){
  tabla_mod <- datos %>% 
    ungroup() %>% 
    dplyr::filter(dplyr::if_any(.cols = dplyr::any_of(names(variable_filtro)), ~.x == unlist(variable_filtro))) %>% 
    dplyr::mutate(
      dplyr::across(
        .col = names(variables_resaltar),
        .fns = list(indicador = ~ dplyr::if_else(condition = .x == unlist(variables_resaltar),
                                                 true = 1,
                                                 false = 0,
                                                 missing = 0)),
        .names = "{.fn}"
      ),
      indicador = factor(indicador)
    )
  
  
  limites_ideo <- tabla_mod %>%
    mutate(Year = as.numeric(Year)) %>% 
    fill(ideologia) %>% 
    arrange(Year) %>% 
    mutate(indicador_tend = if_else(ideologia != lag(ideologia) | is.na(lag(ideologia)), 1,0)) %>% 
    filter(indicador_tend == 1,
           !is.na(ideologia))
  
  limites_ideo <- limites_ideo %>% 
    mutate(lag_year = lag(Year))
  
  conteo_ideo <- limites_ideo %>% ungroup() %>% summarise(np = n_distinct(ideologia))
  
  grafico_mod <- ggplot2::ggplot(data = tabla_mod) +
    ggplot2::geom_line(mapping = ggplot2::aes_string(x = variable_x,
                                                     y = variable_y,
                                                     group = "indicador",
                                                     color = "indicador")) +
    ggplot2::geom_point(mapping = ggplot2::aes_string(x = variable_x,
                                                      y = variable_y,
                                                      group = "indicador",
                                                      color = "indicador"),size = 3) +
    ggplot2::scale_color_manual(values = c("#FFC300",
                                           "#581845")) +
    geom_rect(data = limites_ideo,
              mapping = aes(xmin = lag_year,
                            xmax = Year,
                            group = ideologia,
                            fill = ideologia,
                            ymin = -Inf,
                            ymax = Inf),alpha = 0.4) +
    scale_fill_manual(values = paleta_colores(conteo_ideo$np))
  
  resultado <- list(
    tabla = tabla_mod,
    plot = grafico_mod
  )
  
  return(resultado)
}

