#' Get Comparison file data
#'
#' Takes in any numeric value and square it
#' @param x A numeric value to be squared
#' @return Squared value of x
#' @export



readComparisonData <- function(qq.value, qq.dir , comp.index , topRows) {

  if (qq.value == "" || qq.value == "NA"){
    dataset <- data.table( data = "No data available.")
  }
  else{



    #index_list() contains the index of the selected file ffrom the dropdown
    f = paste("diffMethTable_site_cmp",comp.index, ".csv",sep = '')

    if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){


      filename <- file.path(qq.dir, 'differential_methylation_data',f)


      filename= as.character(filename)

      nrows.value <- as.character(topRows)
      if (nrows.value == 'ALL'){
        nrows.value = -1
      }

      # fread function from the library data.table
      comp.file <- fread(filename,sep = ",", nrows = nrows.value)
      #comp.file <- fread(filename,sep = ",")

      comp.file <- as.data.frame(comp.file)




      dataset <- data.table( comp.file)



      dataset
    }
    else{
      dataset <- data.table( data = "No data available.")
    }
  }

  return(dataset)

}

