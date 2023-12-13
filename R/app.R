#' Launch Shiny App
#'
#' @param name The name of the app to run
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'

app <- function(name = "normativeBRAIN", ...) {
  shiny::runApp("inst/apps/normativeBRAIN.R", ...)
}