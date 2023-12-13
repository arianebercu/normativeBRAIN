#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#' Launch Shiny App
#'
#' @param name The name of the app to run
#' @param ... arguments to pass to shiny::runApp
#' @export
#'
app <- function(name = "normativeBRAIN", ...) {
  
  shiny::runApp(paste0("inst/apps/", name,".R"), ...)
  }