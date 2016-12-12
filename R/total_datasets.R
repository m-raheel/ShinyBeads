#' All Datasets List used in RnBeads Analysis
#'
#' Takes in a results directory and returns a list of grouped directories that used the same set of datasets
#' @param rd A path to the rnbeads results repository
#' @return list of directories
#' @export


datasets_total <- function(rd) {


  folders <- list.files(rd,full.names = FALSE)

  path.lists <- list()
  final.list <- list()
  analysis.list <- list()
  counter <- 1
  analysis.counter <- 1

  if ( length(folders) != 0 ){

    for (i in 1:length(folders)) {


      if ( file.exists( isolate({ paste(rd, folders[i], 'data_import_data','annotation.csv',sep="/") }) ) )
      {
        tmp = toString(paste(rd, folders[i], 'data_import_data','annotation.csv',sep="/"))
        filepath <- tmp

        if (file.exists( isolate({ paste(filepath) }) ) ){
          filename <- as.character(filepath)

          path.lists[i] <- filename

          A <- try(read.csv((toString(path.lists[i]))))

          if(inherits(A, "try-error")){
            print ('error occured in try block of A')
          }

          else
          {



            check.common = FALSE

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

                    }


                  }
                }#end else try-error of B
              }#end of if for checking file
            }# eend for loop j

            if (check.common == FALSE ){


              analysis.list[analysis.counter] <- folders[i]
              final.list[counter] <- path.lists[i]

              counter = counter+1
              analysis.counter = analysis.counter + 1

            }

          }

        }
      }
      else{

      }

    }# end for loop i

    newList <- list("path_list" = final.list, "analysis_list" = analysis.list)
    return(newList)
  }
  else{

    newList <- list("path_list" = final.list, "analysis_list" = analysis.list)
    return(newList)


  }

}
