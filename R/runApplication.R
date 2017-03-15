#' Launch RnBeadsInterface app
#'
#' Run the RnBeadsInterface application
#' @export

runApplication <- function()
{
     appDir <- system.file("RnShinyBeads", package = "RnBeadsInterface")
     if (appDir == "") {
       stop("Could not find app directory. Try re-installing RnBeadsInterface.", call. = FALSE)
     }

     shiny::runApp(appDir, display.mode = "normal")
}
