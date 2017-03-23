#' Launch RnBeadsInterface app
#'
#' Run the RnBeadsInterface application
#' @export

runApplication <- function()
{
     appDir <- system.file("RnShinyBeads", package = "RnShinyBeads")
     if (appDir == "") {
       stop("Could not find app directory. Try re-installing RnShinyBeads", call. = FALSE)
     }

     shiny::runApp(appDir, display.mode = "normal")
}
