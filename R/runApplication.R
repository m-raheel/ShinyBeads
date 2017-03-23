#' Launch RnShinyBeads app
#'
#' Run the RnShinyBeads application
#' @export

runApplication <- function()
{
     appDir <- system.file("RnShinyBeads", package = "RnShinyBeads")
     if (appDir == "") {
       stop("Could not find app directory. Try re-installing RnShinyBeads", call. = FALSE)
     }

     shiny::runApp(appDir, display.mode = "normal")
}
