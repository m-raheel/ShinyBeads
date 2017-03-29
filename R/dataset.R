#' All Datasets List used in RnBeads Analysis
#'
#' rnbi.dataset
#'
#' return the list of datasets/sample sheet used in the RnBeads analysis found in the given path
#'
#' Takes in a results directory and returns the list of total common and uncommon datasets/sample sheet used in the RnBeads analysis found in the given path
#' @param rd A path to the rnbeads analysis folder
#' @return list of common and uncommon datasets index
#' @export
#'


########################################################################################################################



rnbi.dataset <- function(rd) {

  folders <- rnbi.total.analysis(rd)


  #final list of common dataset analysis
  common.list <- list()
  counter = 1

  #vector of all the indexes of the analysis that have been found common overall
  commmon.index <- c()


  analysis.list <- c()



  if ( length(folders) != 0 ){

    for (i in 1:length(folders)) {

      if (i %in% commmon.index){

      }
      else{

        if ( file.exists( isolate({ paste(rd, folders[i], 'data_import_data','annotation.csv',sep="/") }) ) )
        {
          tmp = toString(paste(rd, folders[i], 'data_import_data','annotation.csv',sep="/"))
          filepath <- tmp

          if (file.exists( isolate({ paste(filepath) }) ) ){
            filename <- as.character(filepath)

            A <- try(read.csv((toString(filename))))

            if(inherits(A, "try-error")){
              print ('error occured in try block of A')
            }

            else
            {
              check.common = FALSE

              inner.common.list <- c()

              for (j in i:length(folders)) {



                if ( file.exists( isolate({ paste(rd, folders[j], 'data_import_data','annotation.csv',sep="/") }) ) ){


                  tmp = toString(paste(rd, folders[j], 'data_import_data','annotation.csv',sep="/"))

                  B <- try(read.csv((toString(tmp))))

                  if(inherits(B, "try-error")){
                    print ('error occured in try block of B')
                  }

                  else
                  {

                    if ( j != i){



                      comparison <- identical(A,B)

                      if (comparison == TRUE){

                        check.common = TRUE

                        commmon.index <- c(commmon.index, j)
                        inner.common.list <- c(inner.common.list, j)

                      }




                    }
                  }#end else try-error of B
                }#end of if for checking file
              }# eend for loop j

              if (check.common == TRUE  ){

                commmon.index <- c(commmon.index, i)
                inner.common.list <- c(inner.common.list, i)

                common.list[counter] <- list(inner.common.list)
                counter <- counter + 1



              }
              else if ( i == length(folders)){

                analysis.list <- c(analysis.list, i)

              }
              else if (check.common == FALSE && i != length(folders)){

                analysis.list <- c(analysis.list, i)

              }

            }

          }
        }
        else{

        }

      }# end of if for common.index

    }# end for loop i

    newList <- list("common_list" = common.list, "analysis_list" = analysis.list,"total_common_index" = commmon.index)
    return(newList)
  }
  else{

    newList <- list("common_list" = common.list, "analysis_list" = analysis.list, "total_common_index" = commmon.index)
    return(newList)
  }
}
