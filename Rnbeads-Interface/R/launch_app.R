#' Launch RnBeadsInterface app
#'
#' Run the RnBeadsInterface application
#' @export


launch_app <- function()
{


  app.path <- normalizePath(paste(getwd(),sep = "/"), winslash = "\\", mustWork = NA)

  print(app.path)
  shiny::runApp(appDir = app.path)
}
