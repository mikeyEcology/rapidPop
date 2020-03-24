#' A wrapper function to run Shiny Apps from \code{rapidPop}.
#' 
#' @param app The name of the app you want to run. The options is `occMod`
#' 
#' @export
runShiny <- function(app="occMod") {
  # locate all the shiny app apps that exist
  validExamples <- list.files(system.file("shiny-apps", package = "rapidPop"))
  
  validExamplesMsg <-
    paste0(
      "Valid shiny apps for rapidPop are: '",
      paste(validExamples, collapse = "', '"),
      "'")
  
  # if an invalid app is given, throw an error
  if (missing(app) || !nzchar(app) ||
      !app %in% validExamples) {
    stop(
      'Please run `runShiny()` with a valid app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }
  
  # find and launch the app
  appDir <- system.file("shiny-apps", app, package = "rapidPop")
  shiny::runApp(appDir, display.mode = "normal")
}
