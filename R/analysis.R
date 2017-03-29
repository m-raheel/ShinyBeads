#' RnBeads Analysis found
#'
#' Takes in a results directory and returns a list of analysis found in that directories
#' @param wd A path to the rnbeads analysis folder
#' @return list of analysis found
#' @export


########################################################################################################################
#' rnbi.total.analysis
#'
#' return the list of RnBeads analysis found in the given path
#'
rnbi.total.analysis <- function(wd) {

  choices <- list.files(path = wd)


  total.analysis.list <- list()
  counter <- 1

  for (i in 1:length(choices)) {

    # if index.html file exist in the directory than it is considered as an RnBeads analysis
    if ( file.exists( isolate({ paste(wd,choices[i],'index.html',sep="/") }) ) )
    {
      total.analysis.list[counter] <- choices[i]
      counter <- counter + 1

    }
  }
  return(total.analysis.list)

}
