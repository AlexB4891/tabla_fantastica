# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Creación de una aplicación brillante, robusta y lista para el producto.
#
# README: cada paso de los archivos dev es opcional y no es necesario
# Complete todos los scripts de desarrollo antes de comenzar.
# 01_start.R debe llenarse al inicio.
# 02_dev.R debe usarse para realizar un seguimiento de su desarrollo durante el proyecto.
# 03_deploy.R debe usarse una vez que necesite implementar su aplicación.
#
#
##################################
#### ARCHIVO ACTUAL: DEV SCRIPT #####
##################################

# Engineering
# Ingenieria

## Dependencies ----
## Add one line by package you want to add as dependency
## Dependencias ----
## Agregue una línea por paquete que desea agregar como dependencia
usethis::use_package( "shiny" )
usethis::use_package( "shiny.semantic" )
usethis::use_package( "leaflet" )
usethis::use_package( "dplyr" )
usethis::use_package( "purrr" )
usethis::use_package( "ggplot2" )
usethis::use_package( "tibble" )
usethis::use_package( "tidyr" )
usethis::use_package( "readr" )
usethis::use_package( "forcats" )
usethis::use_package( "stringr" )
usethis::use_package( "lubridate" )
usethis::use_package( "scales" )
usethis::use_package( "shinycssloaders" )
usethis::use_package( "shinyjs" )
usethis::use_package( "shinydashboard" )
usethis::use_package( "dichromat" )

# Add the pipe:
# Añadir la pipe:

usethis::use_pipe()

# Create modules:
# Crear modulos:

golem::add_module( name = "tabla_central" ) # Name of the module
golem::add_module( name = "seleccion_indicador")
# Auxiliary functions:
# Funciones auxiliares:
usethis::use_r("funciones_graficas")

## External resources
## Creates .js and .css files at inst/app/www
## Recursos externos
## Crea archivos .js y .css en inst / app / www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
## Agregar conjuntos de datos internos ----
## Si tienes datos en tu paquete
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
## Pruebas ----
## Agrega una línea por prueba que quieras crear
usethis::use_test( "app" )

# Documentation
## Vignette ----
# Documentación
## Viñeta ----
usethis::use_vignette("social.expense.lab")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
## Cobertura de código----
## Establecer el servicio de cobertura de código ("codecov" o "overoles")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
# Cree un archivo "readme" de resumen para el subdirectorio testthat
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
## 
## (You'll need GitHub there)
## CI ----
## Utilice esta parte del script si necesita configurar un CI
## servicio para su aplicación
##
## (Necesitarás GitHub allí)
usethis::use_github()

# GitHub Actions
# Acciones de GitHub
usethis::use_github_action() 
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release() 
usethis::use_github_action_check_standard() 
usethis::use_github_action_check_full() 
# Add action for PR
# Agregar acción para relaciones públicas
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis() 
usethis::use_travis_badge() 

# AppVeyor 
usethis::use_appveyor() 
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
# ¡Ya estás listo! ----
# vaya a dev / 03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

