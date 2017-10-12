#' Launch ShinyBeads app
#'
#' Takes in a results directory and Run the ShinyBeads application
#' @param wd A path to the rnbeads analysis folder
#' @export

runApplication <- function(wd)
{
    analysisDir <- wd
    if (!nchar(analysisDir)) {
      print("No directory was selected!")
    } else {

      choices <- list.files(path = analysisDir)

      total.analysis.list <- list()
      counter <- 1
      check = FALSE
      for (i in 1:length(choices)) {

        # if index.html file exist in the directory than it is considered as an RnBeads analysis
        if ( file.exists( paste(analysisDir,choices[i],'index.html',sep="/") ) )
        {
          total.analysis.list[counter] <- choices[i]
          counter <- counter + 1
          check = TRUE

        }
      }
      if (check == TRUE){
        .GlobalEnv$.global.analysisDir <- analysisDir
        on.exit(rm(.global.analysisDir, envir=.GlobalEnv))

        appDir <- system.file("ShinyBeads", package = "ShinyBeads")
        if (appDir == "") {
          stop("Could not find app directory. Try re-installing ShinyBeads", call. = FALSE)
        }

        shiny::runApp(appDir, display.mode = "normal")

      }
      else{
        print(paste("Not a valid RnBeads repository -> ", analysisDir))

      }
    }
}
