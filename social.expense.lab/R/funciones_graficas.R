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
                                      variables_filtro,
                                      variable_x,
                                      variable_y){
  tabla_mod <- datos %>% 
    dplyr::mutate(
      dplyr::across(
      .col = names(variables_filtro),
      .fns = list(indicador = ~ dplyr::if_else(condition = .x == unlist(variables_filtro),
                                        true = 1,
                                        false = 0,
                                        missing = 0)),
      .names = "{.fn}"
    ),
    indicador = factor(indicador)
    )
  
  grafico_mod <- ggplot2::ggplot(data = tabla_mod) +
    ggplot2::geom_point(mapping = ggplot2::aes_string(x = variable_x,
                                                      y = variable_y,
                                                      group = "indicador",
                                                      color = "indicador")) +
    ggplot2::scale_color_manual(values = c("#FFC300",
                                           "#581845"))
  
  resultado <- list(
    tabla = tabla_mod,
    plot = grafico_mod
  )
  
  return(resultado)
}



