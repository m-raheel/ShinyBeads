#' Launch RnShinyBeads app
#'
#' Run the RnShinyBeads application
#' @export

runApplication <- function()
{

    library(tcltk2)
    filename <- tclvalue(tkchooseDirectory()) # Very simple, isn't it?
    if (!nchar(filename)) {
      tkmessageBox(message = "No file was selected!")
    } else {
      tkmessageBox(message = paste("The file selected was", filename))
    }


    .GlobalEnv$.global.filename <- filename
    on.exit(rm(.global.filename, envir=.GlobalEnv))

     appDir <- system.file("RnShinyBeads", package = "RnShinyBeads")
     if (appDir == "") {
       stop("Could not find app directory. Try re-installing RnShinyBeads", call. = FALSE)
     }

     shiny::runApp(appDir, display.mode = "normal")
}
