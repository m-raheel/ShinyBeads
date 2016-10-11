#' RnBeads modules performed
#'
#' Takes in a working directory and csv file name and returns a list of differential methylation p values from the comparisons csv files located in differential_methylation_data dir of Rnbeads Analysis
#' @param wd path to the rnbeads results directory where csv files are located and name of csv file
#' @return list of p values
#' @export


comparison_plot <- function(wd) {

  if ( file.exists( isolate({ paste(wd,'differential_methylation_data','diffMethTable_site_cmp1.csv',sep="/") }) ) ){
    filename <- file.path(wd, 'differential_methylation_data','diffMethTable_site_cmp1.csv')


    filename= as.character(filename)

    # fread function from the library data.table
    list.diff.p.values <- fread(filename,sep = ",", select = c("diffmeth.p.val"))

    # converting the data into list so that it can be plotted

    list.diff.p.values <- as.data.frame(list.diff.p.values)

    list.diff.p.values <- as.matrix(list.diff.p.values)

    list.diff.p.values <- lapply(seq_len(ncol(list.diff.p.values)), function(col) list.diff.p.values[,col])

    list.diff.p.values <- unlist(list.diff.p.values)

    return(list.diff.p.values)
  }

  else{

    empty_list <- list()

    return(empty_list)
  }


}
