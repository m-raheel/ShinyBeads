#' Get differential methylation data comparison file data
#'
#' rnbi.read.comparisondata
#' Reads the comparison csv file under differential_methylation_data folder of the selected analysis and returns the results.
#' @param qq.value The analysis name
#' @param qq.dir analysis path
#' @param comp.index the comparison file index (e.g. 1,2,3..)
#' @param columnSelected which column to read the data
#' @param topRows  reading top n rows
#' @return dataframe of the data read from csv file
#' @export




rnbi.read.comparisondata <- function(qq.value, qq.dir , comp.index , topRows, columnSelected) {

  if (qq.value == "" || qq.value == "NA"){
    dataset <- data.table( data = "No data available.")
  }
  else{

    #comp.index is the file number to open and read
    f = paste("diffMethTable_site_cmp",comp.index, ".csv",sep = '')

    if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){


      filename <- file.path(qq.dir, 'differential_methylation_data',f)
      filename= as.character(filename)

      nrows.value <- as.character(topRows)
      if (nrows.value == 'ALL'){
        nrows.value = -1
      }

      # fread function from the library data.table
      comp.file <- fread(filename,sep = ",", nrows = nrows.value , select = c('cgid','Chromosome','Start','Strand',columnSelected))
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

#' rnbi.read.comparisondata.rrbs
#' Reads the rrbs comparison csv file under differential_methylation_data folder of the selected analysis and returns the results.
#' @param qq.value The analysis name
#' @param qq.dir analysis path
#' @param comp.index the comparison file index (e.g. 1,2,3..)
#' @param columnSelected which column to read the data
#' @param topRows  reading top n rows
#' @param results.dir absolute path to the analysis folder
#' @return dataframe of the data read from csv file
#' @export



rnbi.read.comparisondata.rrbs <- function(qq.value, qq.dir , comp.index , topRows, columnSelected , results.dir) {

  if (qq.value == "" || qq.value == "NA"){
    dataset <- data.table( data = "No data available.")
  }
  else{

    data_type = rnbi.analysis.datatype(results.dir , qq.value)

    if (grepl('idat files' , data_type )) # true if idat files is the data type of the analysis in the string data_type
    {
      rrbs_analysis = FALSE
    }
    else{
      rrbs_analysis = TRUE
    }

    if (rrbs_analysis == TRUE)
    {

      f = paste("diffMethTable_site_cmp",comp.index, ".csv.gz",sep = '')
    }
    else{
      f = paste("diffMethTable_site_cmp",comp.index, ".csv",sep = '')
    }



    if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){


      filename <- file.path(qq.dir, 'differential_methylation_data',f)
      filename= as.character(filename)

      nrows.value <- as.character(topRows)
      if (nrows.value == 'ALL'){
        nrows.value = -1
      }


      if (rrbs_analysis == TRUE)
      {
        # fread function from the library data.table
        comp.file <- fread(input = paste('zcat < ',filename,sep = ''),sep = ",", nrows = nrows.value, select = c("Chromosome",'Start',columnSelected))
      }
      else{
        # fread function from the library data.table
        comp.file <- fread(filename,sep = ",", nrows = nrows.value , select = c('cgid','Chromosome','Start','Strand',columnSelected))

      }
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

#' rnbi.read.comparisondatalogical
#'
#' Reads the comparison csv file under differential_methylation_data folder of the selected analysis and returns the Logical (True, False) results used for Venn Diagram purpose.
#' @param dataset dataframe of differential methylation comparison data,
#' @param column column selected from the interface to read the data
#' @param equality the assignment operator selected form the interface
#' @param range range of value selected from the interface
#' @return dataframe of the data read from csv file
#' @export



rnbi.read.comparisondatalogical <- function(dataset, column ,equality,range ) {

  print("inside the get logical data colums")
  print(equality)
  print(column)
  print(range)

  if(equality == ">="){ d <- (subset(dataset, select = c(column) ) >= as.numeric( range))}
  else if(equality == ">"){ d <- (subset(dataset, select = c(column) ) > as.numeric( range))}
  else if(equality == "<="){d <- (subset(dataset, select = c(column) ) <= as.numeric( range))}
  else if(equality == "<"){ d <- (subset(dataset, select = c(column) ) < as.numeric( range))}
  else if(equality == "="){ d <- (subset(dataset, select = c(column) ) == as.numeric( range))}
  else {d <- (subset(dataset, select = c(column) ) > as.numeric( range))}

  print("inside the get logical data colums")
  print(d)
  return(d)
}




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

