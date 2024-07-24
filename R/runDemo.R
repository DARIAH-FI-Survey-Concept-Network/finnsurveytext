#' Run Shiny App Demo
#'
#' @return launches the RShiny demo
#' @export
#'
#' @examples
#' runDemo()
runDemo <- function() {
  appDir <- system.file("shiny-examples", "app", package = "finnsurveytext")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `finnsurveytext`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
