#' RnBeads analysis type (IDAT, RRBS etc..)
#'
#' Takes in a working directory and analysis name and return the analysis data type of a specified analysis from the table in data_import.html file
#' @param wd path to the rnbeads results directory and analysis.name is the selected analysis
#' @return text containg idat identifier to check and declare idat analysis or not
#' @export

########################################################################################################################

#' rnbi.analysis.datatype
#'
#' return the analysis data type of a specified analysis from the table in data_import.html file
#'
rnbi.analysis.datatype <- function(wd , analysis.name) {

  analysis.dir <- file.path(wd, analysis.name)

  filename <- file.path(analysis.dir,'data_import.html')

  differential.methylation.path <- filename

  webpage <- readLines(tc <- textConnection(differential.methylation.path)); close(tc)
  pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)

  query = "//*/div[@id='section2']/div/table"
  query
  datatype = xpathSApply(pagetree, query, xmlValue)

  return(datatype)

}
