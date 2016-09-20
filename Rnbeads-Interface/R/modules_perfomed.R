#' RnBeads modules performed
#'
#' Takes in a working directory and returns a list of rnbeads modules performed
#' @param wd path to the rnbeads results directory where analysis.log file is located
#' @return list of rnbeads modules performed
#' @export


modules_performed <- function(wd) {

  resulting.modules.list <- list()
  module.counter <- 1

  check_vectors <- c('COMPLETED Loading Data', 'COMPLETED Quality Control', 'COMPLETED Preprocessing', 'COMPLETED Tracks and Tables','COMPLETED Covariate Inference','COMPLETED Exploratory Analysis','COMPLETED Differential Methylation')
  display_vectors <- c('Data Import', 'Quality Control', 'Preprocessing', 'Tracks and Tables','Covariate Inference','Exploratory Analysis','Differential Methylation')

  dat<- c('')
  logfile.path = reactive({file.path(wd, 'analysis.log')})
  A <- read.table(logfile.path(), sep="\t", fill=FALSE, strip.white=TRUE)
  dat <- data.frame(v=c(A))
  performed_modules <- 0
  count <- 0
  for (i in 1:length(check_vectors)) {

    result2 = with(dat, grepl(check_vectors[i], V1))

    for (j in 1:length(result2)) {

      if (result2[j] == TRUE){

        count <- count + 1

      }

    }

    if (count == 0) {
      break
    }
    else{
      performed_modules <- i
      resulting.modules.list[module.counter] <- display_vectors[i]

      module.counter <- module.counter + 1

    }

    count <- 0
  }

  #print (resulting.modules.list)

  #session$sendCustomMessage(type = "myperformedmodulesno", performed_modules)

  return(resulting.modules.list)

}
