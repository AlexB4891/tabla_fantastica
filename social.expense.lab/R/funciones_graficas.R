
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
                                           "#581845"))
  
  resultado <- list(
    tabla = tabla_mod,
    plot = grafico_mod
  )
  
  return(resultado)
}



# INTENTO FALLIDO ---------------------------------------------------------


Educacion <- gasto_social_presidente[[2]]


Educacion <- Educacion %>% mutate(Year = as.numeric(Year))


argentina_ejemplo <- serie_de_tiempo_resaltada(datos = Educacion, 
                          variables_resaltar = list(nombre_del_presidente = "Fernando de la Rúa"),
                          variable_filtro = list(pais = "Argentina"), 
                          variable_x = "Year", 
                          variable_y = "Indicador_valor") 


edu_boli <- Educacion %>% 
  filter(pais == "Bolivia" )

limites_ideo <- edu_boli %>%
  mutate(Year = as.numeric(Year)) %>% 
  fill(ideologia) %>% 
  arrange(Year) %>% 
  mutate(indicador_tend = if_else(ideologia != lag(ideologia) | is.na(lag(ideologia)), 1,0)) %>% 
  filter(indicador_tend == 1,
         !is.na(ideologia))



limites_ideo <- limites_ideo %>% 
  mutate(lag_year = lag(Year))


argentina_ejemplo$plot +
  geom_rect(data = limites_ideo,
            mapping = aes(xmin = lag_year,
                          xmax = Year,
                          group = ideologia,
                          fill = ideologia,
                          ymin = -Inf,
                          ymax = Inf),alpha = 0.4) +
  scale_fill_manual(values = paleta_colores(4))


library(gganimate)

ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point(aes(colour = Species)) + 
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)

# Declarar como serie de tiempo
# Educacion_1 <- ts(Educacion, start = c(1990, 1), frequency = 1)
# plot(Educacion_1)


# EJEMPLO PRÁCTICO DE LA FUNCIÓN ------------------------------------------


serie_de_tiempo_resaltada(datos = tibble(iris),
                         variables_filtro = list(Species = "virginica"),
                         variables_resaltar = list(Species = "virginica"),
                         variable_x = "Sepal.Length",
                         variable_y = "Sepal.Width")

iris2 <- iris %>% 
  mutate(ind = sample(c("a","b"), size = nrow(.),replace = T))

iris_limites <- tibble(x = 4:8) %>% 
  mutate(x_1 = lag(x, n = 1),
         y = c("a","b","a","b","a"))

 
ggplot() + 
  geom_point(data = iris, aes(Sepal.Length,Sepal.Width)) + 
  geom_rect(data = iris_limites,
            aes(xmin = x_1, xmax=x, ymin = -Inf, ymax = Inf, fill = y, color = y),
                           alpha = 0.1)

