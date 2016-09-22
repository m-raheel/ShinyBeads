#' Launch RnBeadsInterface app
#'
#' Run the RnBeadsInterface application
#' @export


launch_app <- function()
{
  print (getwd())
  app.dir = file.path(getwd())
  print (app.dir)
  shiny::runApp(appDir = app.dir)
}
