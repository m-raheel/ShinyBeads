#' Datasets List used in RnBeads Analysis
#'
#' Takes in a results directory and returns a list of grouped directories that used the same set of datasets
#' @param rd A path to the rnbeads results repository
#' @return list of directories
#' @export


datasets_groups <- function(rd) {


  folders <- list.files(rd,full.names = FALSE)
  folders

  if ( length(folders) != 0 ){

    path.lists <- list()
    for (i in 1:length(folders)) {


      if ( file.exists( isolate({ paste(rd, folders[i], 'data_import_data','annotation.csv',sep="/") }) ) ){

        filepath <- file.path(rd, folders[i], 'data_import_data','annotation.csv')



        # removing the last character from the path '/' cause error in the server

        tmp = toString(filepath)
        len = nchar(tmp)
        last_character = substr(tmp,len,len)

        if (last_character == '/'){

          print ('inside')
          print (filepath)
          tmp = substr(tmp,len,len - 1)
          filepath = tmp
        }
        print ('outside')
        print (filepath)
        if (file.exists( isolate({ paste(filepath) }) ) ){
          filename <- as.character(filepath)

          path.lists[i] <- filename

        }
      }

    }

    if (length(path.lists) != 0){


      # this for loops store the combination of results folder that uses the same  dataset
      same.sample.list <- list()
      same.sample.list2 <- list()

      temp.list <- list()
      temp.counter <- 1

      counter <- 1
      for (i in 1:length(folders)) {


        # # removing the last character from the path '/' cause error in the server
        #
        # tmp = toString(path.lists[i])
        # len = nchar(tmp)
        # last_character = substr(tmp,len,len)
        #
        # if (last_character == '/'){
        #
        #   tmp = substr(tmp,len,len - 1)
        #   path.lists[i] = tmp
        # }

        A <- read.csv((toString(path.lists[i])))[ ,2:3]

        for (j in i:length(folders)) {


          # # removing the last character from the path '/' cause error in the server
          # tmp = toString(path.lists[j])
          # len = nchar(tmp)
          # last_character = substr(tmp,len,len)
          #
          # if (last_character == '/'){
          #
          #   tmp = substr(tmp,len,len - 1)
          #   path.lists[j] = tmp
          # }


          B <- read.csv((toString(path.lists[j])))[ ,2:3]

          if (length(A) == length(B) && j != i){
            comparison <- identical(A,B)
            comparison
            if (comparison == TRUE){

              temp.list[temp.counter] <- folders[j]
              temp.counter = temp.counter+1

              if ((folders[i] %in% temp.list) & (folders[j] %in% temp.list) ){
              }
              else{
                same.sample.list[counter] <- list(c(folders[i],folders[j]))
                same.sample.list2[counter] <- folders[j]
                counter = counter+1

              }
            }

          }


        }


      }


      # datastructure logic of storing the list of folders as a list that share the common folders
      temp.list <- list()
      actual.list <- list()
      actual.counter <- 1
      length(same.sample.list)

      increament <- 1
      k <- 1
      actual.list


      apath.lists <- list()
      apath.counter <- 1

      while (k <= length(same.sample.list)) {

        a <- unlist(same.sample.list[k], use.names = FALSE)
        temp.variable <- a[1]
        temp.list <- append(temp.list, temp.variable)

        # bn <- basename(file.path(rd, temp.variable,'data_import_data', c("annotation.csv")))
        # dn <- dirname(file.path(rd, temp.variable,'data_import_data/annotation.csv'))
        # full.path <- file.path(dn,'annotation.csv')


        filepath <- file.path(rd, folders[i], 'data_import_data','annotation.csv')

        filepath= as.character(filepath)

        #filename <- paste(filepath, 'annotation.csv', sep="/")

        if (file.exists( isolate({ paste(filepath) }) ) ){

          filename <- filepath
          #removing space


          #tmp <- file.path(rd, paste(temp.variable,'/data_import_data/annotation.csv'),sep='')
          #removing space
          #tmp <- gsub(" /", "/", tmp)
          apath.lists[apath.counter] <- filename
          apath.counter <- apath.counter + 1


          for (l in k:length(same.sample.list)) {
            b <- unlist(same.sample.list[l], use.names = FALSE)
            b1 <- b[1]
            if (b1 == temp.variable){

              temp.list <- append(temp.list, b[2])

            }
            else{
              increament = l -1

              break
            }

          }

          #message(paste0("Value of K =  ", k))

          k <- k + increament

          temp.variable <- b

          actual.list[actual.counter] <- list(temp.list)
          temp.list <- list()


          actual.counter <- actual.counter + 1
        }


      }

      return(actual.list)

    }
    else{
      common.datasets <- list()

      return(common.datasets)
    }
  }
  else{

    common.datasets <- list()

    return(common.datasets)


  }


}
