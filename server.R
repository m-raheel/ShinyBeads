
########################################################################################################################
## server.R
## created: 2016-09-01
## creator: Muhammad Raheel
## ---------------------------------------------------------------------------------------------------------------------
## Main workflow of the RnBeads Interface tool.
########################################################################################################################


# libraries to run on the shiny server ( uncomment it on the server)
######################################################################
library(DT)
library(shiny)

#library(RnBeads)
library(XML)
library(compare)
# libFolders <- .libPaths()
# .libPaths(.libPaths()[-1])

# .libPaths(libFolders)
library(data.table) # using the function fread for reading large csv files
library(qqman)
library(tcltk)# OS independent file dir selection
library(lattice)# using qqunif.plot
library(plotly , lib.loc = '/opt/Rlib/3.4') #interactive graphics with D3
library(RnBeadsInterface, lib.loc = '/home/users/mraheel/R/x86_64-pc-linux-gnu-library/3.4')
library(manhattanly , lib.loc = '/home/users/mraheel/R/x86_64-pc-linux-gnu-library/3.4')
library(VennDiagram, lib.loc = '/home/users/mraheel/R/x86_64-pc-linux-gnu-library/3.4')
library(plyr)
library(shinydashboard, lib.loc = '/home/users/mraheel/R/x86_64-pc-linux-gnu-library/3.4')
#####################################################################


# local (comment while on the server)
#####################################################################
# library(shiny)
# library(RnBeadsInterface)
# #library(RnBeads)
# library(XML)
# library(compare)
# library(DT)
# library(data.table) # using the function fread for reading large csv files
# library(qqman)
# library(tcltk)# OS independent file dir selection
# library(lattice)# using qqunif.plot
# library(plotly) #interactive graphics with D3
# library(manhattanly)
# library(VennDiagram)
library(limma)


qqman.qq <- qqman::qq    #EDIT

#library(shinyFiles)

# createLink <- function(val) {
#   sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">Info</a>',val)
# }


## F U N C T I O N S ###################################################################################################



########################################################################################################################

#' rnbi.read.comparisondata
#'
#' Reads the comparison csv file under differential_methylation_data folder of the selected analysis and returns the results.
#'
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


########################################################################################################################
# For Top scorer tab

#' rnbi.read.comparisondatalogical
#'
#' Reads the comparison csv file under differential_methylation_data folder of the selected analysis and returns the Logical (True, False) results.
#'
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


plotVennDiagram <- function(a, a1,a2,a3, a12, a23, a13, a123) {

  if (length(a) == 1) {
    out <- draw.single.venn(area = a1, category = a[1],fill = "skyblue")
  }
  if (length(a) == 2) {
    out <- draw.pairwise.venn(area1 = a1 , area2 = a2 , cross.area = a3,
                              category = c(a[1], a[2]), lty = rep("blank",2),
                              fill = c("light blue", "pink"), alpha = rep(0.5, 2), cat.pos = c(0,0),
                              cat.dist = rep(0.025, 2), scaled = FALSE)
  }
  if (length(a) == 3) {
    out <- draw.triple.venn(area1 = a1, area2 = a2, area3 = a3,
                       n12 = a12, n23 = a23, n13 = a13,
                       n123 = a123, category = c(a[1], a[2], a[3]), lty = "blank",
                       fill = c("skyblue", "pink1", "mediumorchid"))
  }
  if (length(a) == 4) {
    out <- draw.quad.venn(likes(a[1]), likes(a[2]), likes(a[3]), likes(a[4]),
                          likes(a[1:2]), likes(a[c(1, 3)]), likes(a[c(1, 4)]), likes(a[2:3]),
                          likes(a[c(2, 4)]), likes(a[3:4]), likes(a[1:3]), likes(a[c(1, 2,
                                                                                     4)]), likes(a[c(1, 3, 4)]), likes(a[2:4]), likes(a), ...)
  }
  if (!exists("out"))
    out <- "Oops"
  return(out)
}

########################################################################################################################

## S H I N Y S E R V E R ###################################################################################################

options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output, session) {

  # update timestamp when update is clicked
  shinyjs::onclick("update", shinyjs::html("time", date()))

  # logo of the app

  output$preImage <- renderImage({

    filename <- normalizePath(file.path(getwd(),'includes','images',
                                        paste('Rnbeads', '.png', sep='')))

    # Return a list containing the filename and alt text
    list(src = filename,
         contentType = 'image/png',
         width = 100,
         height = 50,
         alt = paste("Image number", 'logo'))

  }, deleteFile = FALSE)


  # oberve to change the tabs when button or table clicked!

  observeEvent(input$action, {
    newtab <- switch(input$tabs,
                     "about" = "home",
    )
    updateTabItems(session, "tabs", newtab)
  })

  observe({
    if(input$view_datasets > 0){

      session$sendCustomMessage("myCallbackHandler", "4")
    }
  })

  #Remove working directory
  observeEvent(input$clearDirButton,{
    session$reload()

  })

  #updatedDir <- normalizePath("/projects/factorization/raw_data/Demo Repository", winslash = "\\", mustWork = NA)

  updatedDir <- normalizePath("/var/www/html/data", winslash = "\\", mustWork = NA)



  selectedDir <-  as.character(updatedDir)

  # updating all the selectInput dropdowns of the app

  updateSelectInput(session, "input_type",
                    label = paste("Select analysis folder"),
                    choices = list.files(path = selectedDir))

  updateSelectInput(session, "select_ia",
                    label = paste("Select analysis folder"),
                    choices = list.files(path = selectedDir))



  updateSelectInput(session, "input_dmcomp_choices",
                    label = paste("Select analysis folder"),
                    choices = list.files(path = selectedDir))

  updateSelectInput(session, "input_dmcomp_choices_1",
                    label = paste("Analysis 1"),
                    choices = list.files(path = selectedDir))

  updateSelectInput(session, "input_dmcomp_choices_2",
                    label = paste("Analysis 2"),
                    choices = list.files(path = selectedDir))

  updateSelectInput(session, "input_tablebrowser_choices",
                    label = paste("Select analysis folder"),
                    choices = list.files(path = selectedDir))

  updateSelectInput(session, "input_topscorer_choices_1",
                    label = paste("Analysis 1"),
                    choices = list.files(path = selectedDir))

  updateSelectInput(session, "input_topscorer_choices_2",
                    label = paste("Analysis 2"),
                    choices = list.files(path = selectedDir))


  # Can also set the label and select items
  updateCheckboxGroupInput(session, "cb_ts_comp_venn",
                           label = paste("Select analysis"),
                           choices = list.files(path = selectedDir),
                           selected = 1
  )



  dirfolder = list.files(path = selectedDir)

  if ( file.exists( isolate({ paste(selectedDir,dirfolder[1],'index.html',sep="/") }) ) ){
    output$ErrorText1 <- renderText({ paste("You are working with the RnBeads analysis repository:",sep="") })
    output$ErrorText2 <- renderText({ paste(selectedDir,sep="") })
  }
  else{
    output$ErrorText1 <- renderText({ paste("Not a Valid RnBeads Repository:",sep="") })
    output$ErrorText2 <- renderText({ paste(selectedDir,sep="") })
    observe({

      check.repo = 'FALSE'
      session$sendCustomMessage(type = "t", check.repo)
    })

  }

  results.dir = reactive({file.path(path = selectedDir)})

  # selected RnBeads repository folder
  value <- reactive({as.character(input$input_type) })
  rwaDir <- reactive({file.path(results.dir(), value()) })

  # output logo

  output$logo <- renderText({

    file.path(results.dir(), 'images/RnBeads.png')
  })


  ####################################################################################

  # displaying folders of repository selected
  ##################################################################################
  observe({


    choices <- list.files(path = results.dir())

    output$count_rfolders <- renderText({
      paste("Total RnBeads reports in this repository =", length(choices), sep = " ")

    })

    if ( length(choices) != 0 ){

      output$list_folders <- renderDataTable({
        DT <- data.table( RnBeads_Analysis = choices)
        return(DT)

      },selection = 'single', escape = FALSE)

    }

    else{

      output$list_folders <- renderDataTable({

        DT <- data.table( RnBeads_Analysis = 'No directory in this repository.')
        return(DT)

      },selection = 'single', escape = FALSE)

    }
  })

  ############################################################################################

  # check and return the results folder that have the same sample annotation file.
  ############################################################################################

  observe({

    cd_list <- list()
    cd_list_counter <- 1


    common.datasets = datasets_total(results.dir())
    common.datasets = common.datasets$path_list


    if (length(common.datasets) != 0){

      cd_list <- lapply(1:length(common.datasets), function(i) {
        cd_list[cd_list_counter] <- paste("Dataset",i,sep = "_")
        cd_list_counter = cd_list_counter + 1


        cd_list

      })


      dataset_choices <- unlist(cd_list)
      # update the datalist dropdown in the individual data sets tab
      updateSelectInput(session, "dd_ids_datasets",
                        label = "Datasets",
                        choices = dataset_choices)

      output$total_datasets <- renderText({
        paste("Total datasets used in this repository =", length(cd_list), sep = " ")

      })

      output$list_datasets <- renderDataTable({
        cd_list <- unlist(cd_list)
        DT <- data.table( Datasets_Used = cd_list)

        DT

      },selection = 'single', escape = FALSE)


    }


    else if ( file.exists( isolate({ normalizePath(paste(results.dir(),input$select_ia,'data_import_data','annotation.csv',sep="/"), winslash = "\\", mustWork = NA) }) ) )
    {
      # update the datalist dropdown in the individual data sets tab
      updateSelectInput(session, "dd_ids_datasets",
                        label = "Datasets",
                        choices = 'Dataset_1')

      output$total_datasets <- renderText({
        paste("Total datasets used in this repository = 1", sep = " ")

      })

      output$list_datasets <- renderDataTable({

        DT <- data.table( Datasets_Used = 'Dataset_1')

        DT

      },selection = 'single', escape = FALSE)

    }

    else{


      output$total_datasets <- renderText({
        paste("Total datasets used in this repository =", length(cd_list), sep = " ")

      })

      output$list_datasets <- renderDataTable({

        DT <- data.table( Datasets_Used = 'No information available.')

        DT

      },selection = 'single', escape = FALSE)

    }


  })


  ############################################################################################

  # if any of the datasetslist rows is clicked then it will redirects to individual dataset
  #tab to display the annotaion.csv file contents
  ############################################################################################


  observeEvent(input$list_datasets_rows_selected, {
    row <- input$list_datasets_rows_selected

    row <- as.integer(row)

    total.datasets = datasets_total(results.dir())

    path_list = total.datasets$path_list

    # if no datasets returned means that we have only one analysis so in else showing it
    if (length(path_list) != 0){
      a.file <- reactive({read.csv(as.character(path_list[row]))})

      # Generate a summary of the dataset
      output[[paste0('annotation')]] <- renderDataTable({

        dataset <- a.file()
        dataset
      },selection = 'single', escape = TRUE)

      output$h1_datasettab <- renderText({
        paste('Dataset_',row,sep = '')

      })
      updateSelectInput(session, "dd_ids_datasets",
                         selected = paste('Dataset_',row,sep = ''))

    }
    else{

      if ( file.exists( isolate({ paste(results.dir(),input$select_ia,'data_import_data','annotation.csv',sep="/") }) ) )
      {

        a.file <- reactive({read.csv(normalizePath(paste(results.dir(),input$select_ia,'data_import_data','annotation.csv',sep="/"), winslash = "\\", mustWork = NA))})

        # Generate a summary of the dataset
        output[[paste0('annotation')]] <- renderDataTable({

          dataset <- a.file()
          dataset

        },selection = 'single', escape = TRUE)

        output$h1_datasettab <- renderText({
          paste("Dataset_",row)

        })
      }
      else{

        # Generate a summary of the dataset
        output[[paste0('annotation')]] <- renderDataTable({

          dataset <- data.table( data = "No infomation available.")
          dataset
        },selection = 'single', escape = TRUE)

        output$h1_datasettab <- renderText({
          paste("No data available")

        })

      }
    }


    # how many analysis is the selected dataset is used in common in the analysis
    ########################################################################################
    analysis_list = unlist(total.datasets$analysis_list)

    common.datasets = datasets_common(results.dir())

    check.common = FALSE

    common.index = 1
    matrix.list = as.matrix(common.datasets)
    for (i in 1:length(matrix.list)) {

      matrix.unlist = unlist(matrix.list[i])

      if (analysis_list[row]  %in% matrix.unlist){
        #print(paste('matrix unlist inside if ',i,matrix.unlist))
        check.common = TRUE
        common.index = i
      }
    }

    #print((matrix.list[1]))


    # if no datasets returned means that we have only one analysis so in else showing it

    if (check.common == TRUE){



      matrix.unlist = unlist(matrix.list[common.index])
      # Generate a summary of the dataset
      output[[paste0('annotation1')]] <- renderDataTable({

        DT <- data.table( Analysis_Dir = matrix.unlist)

        DT


      },selection = 'single', escape = TRUE)

    }

    else if (length(analysis_list) != 0){
      al <- reactive({analysis_list[row]})

      # Generate a summary of the dataset
      output[[paste0('annotation1')]] <- renderDataTable({

        DT <- data.table( Analysis_Dir = al())

        DT


      },selection = 'single', escape = TRUE)

    }
    else{




        # Generate a summary of the dataset
        output[[paste0('annotation1')]] <- renderDataTable({

          dataset <- data.table( data = "No infomation available.")
          dataset




        },selection = 'single', escape = TRUE)




    }

    ###################################################################################
    #print(row)
    session$sendCustomMessage("myCallbackHandler", "2")
    #updateTabsetPanel(session, "Individual dataset", selected = "DatasetTab")
  })


  ########################################################################################################################
  ##
  ## Nav Bar Tab : Individual Analaysis
  ## ---------------------------------------------------------------------------------------------------------------------
  ## Main sub tabs under Individual Analaysis top nav bar.
  ########################################################################################################################


  ############################################################################################

  # displaying RnBeads options
  ############################################################################################


  observeEvent(input$select_ia,{

    value.options <- reactive({as.character(input$select_ia) })

    wd_options <- reactive({file.path(results.dir(), value.options()) })


    if ( file.exists( isolate({ paste(wd_options(),'analysis_options.RData',sep="/") }) ) ){

      rwaDirUpdated <- reactive({file.path(wd_options(), "analysis_options.RData")})

      # function to read analysis_options.RData file

      LoadToEnvironment <- function(rwaDir, env = new.env()){
        load(rwaDir, env)
        return(env)
      }

      output$list_options <- renderDataTable({

        rdata.env <- LoadToEnvironment(rwaDirUpdated())
        rdata.fit <- rdata.env$analysis.options

        options_values <- list()

        for (i in 1:length(rdata.fit)) {

          options_values[i] <- toString(rdata.fit[i])

        }

        options_values <- unlist(options_values)

        names.rdata.fit <- names(rdata.fit)

        DT = data.table( Analysis_Options = names.rdata.fit, Values = options_values)

        DT

      })
    }
    else{


      output$list_options <- renderDataTable({

        DT = data.table( data = "No infomation available.")

        DT

      })
    }

  })

  ############################################################################################

  # displaying list of RnBeads modules performed
  ############################################################################################

  observeEvent(input$select_ia,{

    value.modules <- reactive({as.character(input$select_ia) })
    wd_modules <- reactive({file.path(results.dir(), value.modules()) })

    if ( file.exists( isolate({ paste(wd_modules(),'analysis.log',sep="/") }) ) ){
      #fucntion from the RnBeadsInterface package


      Performed_Modules <-  modules_performed(wd_modules())

      modules <- unlist(Performed_Modules)

      output$list_module <- renderTable({
        DT <- data.table( Performed_Modules = modules)
        DT

      })

    }
    else{

      output$list_module <- renderTable({
        DT <- data.table(Performed_Modules = 'No file exist or no data available.')
        DT

      })
    }

  })

  ############################################################################################

  # displaying rnbeads reports index.html file content
  ############################################################################################
  observeEvent(input$select_ia,{
    output$rnbeadsReports <- renderUI({

      value.modules <- reactive({as.character(input$select_ia) })
      wd_modules <- reactive({file.path(results.dir(), value.modules()) })

      if ( file.exists( isolate({ paste(wd_modules(),'index.html',sep="/") }) ) ){

          #browseURL(paste('http://internal.genetik.uni-sb.de/dataT7600','as.character(input$select_ia)','index.html',sep="/"))
          HTML(paste('<a class = "btn btn-primary" target = "_blank" href = "http://internal.genetik.uni-sb.de/dataT7600/',paste(as.character(input$select_ia),'index.html"',sep="/"),'>View Reports','</a>',sep=""))


      }
      else{
          HTML('<p>No reports exist!</p>')

      }

    })
  })


  ########################################################################################################################
  ##
  ## Nav Bar Tab : Individual Datset
  ## ---------------------------------------------------------------------------------------------------------------------
  ## Main sub tabs under Individual Dataset top nav bar.
  ########################################################################################################################


  ############################################################################################

  # Displaying annotation file contents as well as in how many of the RnBeads analysis
  # annotation file is used
  ############################################################################################

  observeEvent(input$dd_ids_datasets,{

    dd_datasets <- as.character(input$dd_ids_datasets)

    tmp = toString(dd_datasets)
    len = nchar(tmp)
    last_character = substr(tmp,len,len)




    if (dd_datasets != "NA"){

      last_character = as.integer(last_character)

      datasets_files = datasets_total(results.dir())

      path_list = datasets_files$path_list
      # if no datasets returned means that we have only one analysis so in else showing it
      if (length(path_list) != 0){
        a.file <- reactive({read.csv(as.character(path_list[last_character]))})


        # Generate a summary of the dataset
        output[[paste0('annotation')]] <- renderDataTable({

          dataset <- a.file()
          dataset

        },selection = 'single', filter = 'top',

        extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL),
        options = list(
          scrollX = TRUE,
          scrollY = TRUE,
          dom = 'Blfrtip',
          buttons = list(
            'copy',
            'print',
            list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download'
            ),
            I('colvis')

          ),
          br(),
          keys = TRUE

        ), escape = TRUE)

        output$h1_datasettab <- renderText({
          paste("Dataset_",last_character)

        })
      }
      else{

        if ( file.exists( isolate({ paste(results.dir(),input$select_ia,'data_import_data','annotation.csv',sep="/") }) ) )
        {
          a.file <- reactive({read.csv(normalizePath(paste(results.dir(),input$select_ia,'data_import_data','annotation.csv',sep="/"), winslash = "\\", mustWork = NA))})

          # Generate a summary of the dataset
          output[[paste0('annotation')]] <- renderDataTable({

            dataset <- a.file()
            dataset

          },selection = 'single', filter = 'top',

          extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL),
          options = list(
            scrollX = TRUE,
            scrollY = TRUE,
            dom = 'Blfrtip',
            buttons = list(
              'copy',
              'print',
              list(
                extend = 'collection',
                buttons = c('csv', 'excel', 'pdf'),
                text = 'Download'
              ),
              I('colvis')

            ),
            br(),
            keys = TRUE

          ), escape = TRUE)

          output$h1_datasettab <- renderText({
            paste("Dataset_",last_character)

          })


        }
        else{
          # Generate a summary of the dataset
          output[[paste0('annotation')]] <- renderDataTable({

            dataset <- data.table( data = "No infomation available.")
            dataset

          })

          output$h1_datasettab <- renderText({
            paste("No Dataset Available")

          })


        }

      }


      # how many analysis is the selected dataset is used in common in the analysis
      ########################################################################################
      analysis_list = unlist(datasets_files$analysis_list)
      common.datasets = datasets_common(results.dir())


      # checking if the selected datasets is used in more than one analysis then saving the index of the matrix
      check.common = FALSE

      common.index = 1
      matrix.list = as.matrix(common.datasets)
      for (i in 1:length(matrix.list)) {

        matrix.unlist = unlist(matrix.list[i])

        if (analysis_list[last_character]  %in% matrix.unlist){
          #print(paste('matrix unlist inside if ',i,matrix.unlist))
          check.common = TRUE
          common.index = i
        }
      }

      # if no datasets returned means that we have only one analysis so in else showing it

      if (check.common == TRUE){



        matrix.unlist = unlist(matrix.list[common.index])

        # Generate a summary of the dataset
        output[[paste0('annotation1')]] <- renderDataTable({

          DT <- data.table( Analysis_Dir = matrix.unlist)

          DT


        },selection = 'single', escape = TRUE)

        output$common_dataset_pie <- renderPlotly({

          v2 <- vector(mode="character", length=length(matrix.unlist))
          for (i in 1:length(matrix.unlist)) {
            v2[i] <- '10'
          }

          print (v2)
          matrix.unlist <- data.frame("Categorie"=matrix.unlist, "values"= v2)

          data <- matrix.unlist[,c('Categorie', 'values')]
          print (data)

          p <- plot_ly(data, labels = ~Categorie, values = ~values, type = 'pie') %>%
            layout(title = paste('RnBeads Analysis using',dd_datasets ),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



        })

      }

      else if (length(analysis_list) != 0){
        al <- reactive({analysis_list[last_character]})

        # Generate a summary of the dataset
        output[[paste0('annotation1')]] <- renderDataTable({

          DT <- data.table( Analysis_Dir = al())

          DT


        },selection = 'single', escape = TRUE)


        output$common_dataset_pie <- renderPlotly({
            v2 <- c('10')


            print (v2)
            matrix.unlist <- data.frame("Categorie"=al(), "values"= v2)

            data <- matrix.unlist[,c('Categorie', 'values')]
            print (data)

            p <- plot_ly(data, labels = ~Categorie, values = ~values, type = 'pie') %>%
                layout(title = paste('RnBeads Analysis using',dd_datasets ),
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



            })

      }
      else{




        # Generate a summary of the dataset
        output[[paste0('annotation1')]] <- renderDataTable({

          dataset <- data.table( data = "No infomation available.")
          dataset




        },selection = 'single', escape = TRUE)

      }

      ###################################################################################
    }

  })

  ########################################################################################################################
  ##
  ## Top Nav Bar Tab : Integrative Visualization
  ## ---------------------------------------------------------------------------------------------------------------------
  ## Main sub tabs under Integrative Visualization top nav bar.
  ########################################################################################################################



  ########################################################################################################################
  ##
  ## Nav Bar Tab : QQ-plots
  ##
  ########################################################################################################################


  ############################################################################################

  # Differential Methylation options getting it from the HTML file
  ############################################################################################

  observe({

    value.options <- reactive({as.character(input$input_dmcomp_choices) })

    if (value.options() != "NA"){


      wd_options <- reactive({file.path(results.dir(), value.options()) })


      if ( file.exists( isolate({ paste(wd_options(),'differential_methylation.html',sep="/") }) ) ){

        filename <- reactive({ normalizePath(file.path(wd_options(),'differential_methylation.html'), winslash = "\\", mustWork = NA) })

        #filename= as.character(filename)

        differential.methylation.path <- filename()

        webpage <- readLines(tc <- textConnection(differential.methylation.path)); close(tc)
        pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)

        # Extract table header and contents of the analysis option table of differential methylation
        tablehead <- xpathSApply(pagetree, "//*/table[@class='tindex']/thead/tr/th", xmlValue)
        results <- xpathSApply(pagetree, "//*/table[@class='tindex']/tbody/tr/td", xmlValue)


        # Convert character vector to dataframe
        content <- as.data.frame(matrix(results, ncol =2, byrow = TRUE))

        # Clean up the results
        content[,1] <- gsub("Â ", "", content[,1])
        tablehead <- gsub("Â ", "", tablehead)
        names(content) <- tablehead

        output$htmlTable = renderTable({
          content
        })

        output$htmlcomparisonTable = renderTable({
          "comparisonTable No data avaialbe"
        })

      }

      else{

        output$htmlTable = renderTable({
          paste("No data available")
        })

        output$htmlcomparisonTable = renderTable({
          paste("No data available")
        })

      }



    }


  })

  ############################################################################################

  # showing comparisons performed from the HTML file and displaying in the dropdown for qqplot 1
  ############################################################################################

  observeEvent(input$input_dmcomp_choices,{




    shinyjs::hide(id = "id_qqplot")

    input_choices <- as.character(input$input_dmcomp_choices)

    qq.dir <- file.path(results.dir(), input_choices)

    # Extracting the values from the table from differential methylation html file and displaying the values of comparisons in the dropdown

    if (input_choices != "NA"){


      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation.html',sep="/") }) ) ){

        filename <- file.path(qq.dir,'differential_methylation.html')

        differential.methylation.path <- filename


        webpage <- readLines(tc <- textConnection(differential.methylation.path)); close(tc)
        pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)

        query = "//*/div[@id='section3']/ul/li"
        dates = xpathSApply(pagetree, query, xmlValue)
        dates
        comp_names <- list()
        comp_names_counter <- 1
        for (i in 1:length(dates)) {

          comp_names[comp_names_counter] <- dates[i]
          comp_names_counter = comp_names_counter + 1

        }


        choices.list <- comp_names


      }
      else{
        choices.list <- 'NA'
      }
    }
    else{
      choices.list <- 'NA'

    }



    updateSelectInput(session, "input_dmcomp_files",
                      label = paste("Comparison", ""),
                      choices = choices.list)

    if (length(choices.list) > 0){

      updateCheckboxGroupInput(session, "check_comp",

                               label = paste("Select comparison", ""),
                               choices = choices.list
      )



    }
    else{


      updateCheckboxGroupInput(session, "check_comp",

                               label = paste("Select comparison", ""),
                               choices = ""
      )

    }




  })


  ############################################################################################

  # qqplots 1 of diff methylation p- values
  ############################################################################################

  # returns the index of selected comparison file in QQplot 1
  index_list <- eventReactive(input$input_dmcomp_files, {


    input_choices <- as.character(input$input_dmcomp_choices)

    qq.dir <- file.path(results.dir(), input_choices)


    # Extracting the values from the table from differential methylation html file and displaying the values of comparisons in the dropdown

    choice.index <- '1'

    if (input_choices != "NA"){


      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation.html',sep="/") }) ) ){

        filename <- file.path(qq.dir,'differential_methylation.html')

        differential.methylation.path <- filename


        webpage <- readLines(tc <- textConnection(differential.methylation.path)); close(tc)
        pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)

        query = "//*/div[@id='section3']/ul/li"
        dates = xpathSApply(pagetree, query, xmlValue)


        for (i in 1:length(dates)) {

          if (identical(input$input_dmcomp_files, dates[i])){

            choice.index <- as.character(i)

            break
          }
        }

      }
      else{
        choice.index <- '1'

      }
    }
    else{
      choice.index <- '1'


    }

    return(choice.index)
  })



  observeEvent(input$displayQQPlotBtn,{

    shinyjs::show(id = "id_qqplot")

    plotInput <- function(){

      qq.value <- as.character(input$input_dmcomp_choices)

      qq.dir <- file.path(results.dir(), qq.value)

      f = paste("diffMethTable_site_cmp",index_list(), ".csv",sep = '')

      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) )
      {

        # Create a Progress object
        progress <- shiny::Progress$new()

        progress$set(message = "Making QQ Plot", value = 50)

        #y <- dist(ppoints(length(list.pvalues())))
        #qqline(y,list.pvalues())
        #qq(gwasResults$P, main = "Q-Q plot of GWAS p-values")

        #qqman.qq(list.pvalues(),main="Q-Q plot of p-values")

        #qqplot(y,list.pvalues(),main=input$dist,xlab="Theoretical Quantile", ylab="diffmeth.p.val")

        ##from package lattice

        #qqunif.plot(list.pvalues())

        filename <- file.path(qq.dir, 'differential_methylation_data',f)


        filename= as.character(filename)

        nrows.value <- as.integer(input$input_qqplot_readtop)




        # fread function from the library data.table
        comp.file <- fread(filename,sep = ",", select = c("cgid","Chromosome","diffmeth.p.val","diffmeth.p.adj.fdr"), nrows = nrows.value)


        if (nrows.value == -1){
          nrows.value = 100
        }
        else if (nrows.value > 1000){
          nrows.value = 1000
        }

        setnames(comp.file, "diffmeth.p.val", "P")
        comp.file <- data.frame(comp.file[1:nrows.value,])
        qqrObject <- qqr(comp.file)

#         qqrObject <- qqr(HapMap[1:100,])
#         qqly(qqrObject)
        #       qqrObject <- qqr(comp.file , p = "diffmeth.p.val" , snp = "cgid" )
        #
        p <- qqly(qqrObject, col = "#6087ea", size = 1, type = 20, abline_col = "pink",
                  abline_size = 0.5, abline_type = 1, highlight = NULL,
                  highlight_color = "#00FF00", xlab = "Expected -log10(p)",
                  ylab = "Observed -log10(p)", title = "")

        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())

        p

      }

      else {

        # print error/ warning message
        # qqplot(1,1,main="Normal Q-Q Plot", ylab="diffmeth.p.val")
        # text(1,1,"No data available or no comparison file exist")

        Primates <- c('No Data Avaiable')
        Bodywt <- c(0.5 )
        Brainwt <- c(0.5)

        data <- data.frame(Primates, Bodywt, Brainwt)


        p <- plot_ly(data,x = ~Bodywt, y = ~Brainwt, type = 'scatter',
                     mode = 'text', text = ~Primates, textposition = 'middle center',
                     textfont = list(color = '#000000', size = 16))%>%
          layout(title = 'Q-Q Plot',
                 xaxis = list(title = 'Expected -log10(p)',
                              zeroline = TRUE,
                              range = c(0, 1)),
                 yaxis = list(title = 'Observed -log10(p)',
                              range = c(0,1)))


      }


      return(p)
    }

    output$compqqplotly <- renderPlotly({
      pdf(NULL)
      p <- plotInput()
      dev.off()
      p
    })
  })


  ############################################################################################

  # qqplots 2 of diff methylation p- values in which two comarprisons qqplots is displayed
  ############################################################################################

  v <- reactiveValues(data = TRUE)

  # for Repository 1
  observeEvent(input$input_dmcomp_choices_1,{

    v$data <- FALSE

    input_choices <- as.character(input$input_dmcomp_choices_1)

    qq.dir <- file.path(results.dir(), input_choices)

    # qq.dmd.dir <- file.path(qq.dir, 'differential_methylation_data')
    #
    # input_c = list.files(path = qq.dmd.dir)

    # Extracting the values from the table from differential methylation html file and displaying the values of comparisons in the dropdown

    if (input_choices != "NA"){



      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation.html',sep="/") }) ) ){

        filename <- file.path(qq.dir,'differential_methylation.html')

        differential.methylation.path <- filename


        webpage <- readLines(tc <- textConnection(differential.methylation.path)); close(tc)
        pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)


        query = "//*/div[@id='section3']/ul/li"
        dates = xpathSApply(pagetree, query, xmlValue)
        dates
        comp_names <- list()
        comp_names_counter <- 1
        for (i in 1:length(dates)) {

          comp_names[comp_names_counter] <- dates[i]
          comp_names_counter = comp_names_counter + 1


        }

        choices.list <- comp_names
      }
      else{
        choices.list <- 'NA'
      }
    }
    else{
      choices.list <- 'NA'

    }


    updateSelectInput(session, "input_dmcomp_files_1",
                      label = paste("Comparison 1", ""),
                      choices = choices.list)




  })


  # returns the index of selected comparison file in QQplot 1
  index_list_1 <- eventReactive(input$input_dmcomp_files_1, {



    input_choices <- as.character(input$input_dmcomp_choices_1)

    qq.dir <- file.path(results.dir(), input_choices)


    # Extracting the values from the table from differential methylation html file and displaying the values of comparisons in the dropdown

    choice.index <- '1'

    if (input_choices != "NA"){


      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation.html',sep="/") }) ) ){

        filename <- file.path(qq.dir,'differential_methylation.html')

        differential.methylation.path <- filename


        webpage <- readLines(tc <- textConnection(differential.methylation.path)); close(tc)
        pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)

        query = "//*/div[@id='section3']/ul/li"
        dates = xpathSApply(pagetree, query, xmlValue)


        for (i in 1:length(dates)) {
          if (identical(input$input_dmcomp_files_1, dates[i])){

            choice.index <- as.character(i)
            break


          }


        }

      }
      else{
        choice.index <- '1'

      }
    }
    else{
      choice.index <- '1'


    }

    return(choice.index)
  })



  list.pvalues_1 <- reactive({



    qq.value <- as.character(input$input_dmcomp_choices_1)

    qq.dir <- file.path(results.dir(), qq.value)

    #qq.value <- as.character(input$input_dmcomp_files_1)


    if (qq.value == "" || qq.value == "NA"){
      x <- list()
      x
    }
    else{
      #fucntion from the RnBeadsInterface package

      #index_list() contains the index of the selected file ffrom the dropdown
      f = paste("diffMethTable_site_cmp",index_list_1(), ".csv",sep = '')

      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){

        # Create a Progress object
#         progress <- shiny::Progress$new()
#
#         progress$set(message = "Making QQ Plot", value = 50)

        #x <- comparison_plot(qq.dir , f)

        filename <- file.path(qq.dir, 'differential_methylation_data',f)
        filename= as.character(filename)
        nrows.value <- as.integer(input$input_multiqqplot_readtop)


        comp.file <- fread(filename,sep = ",", select = c("diffmeth.p.val"), nrows = nrows.value)


        if (nrows.value == -1){
          nrows.value = 100
        }
        else if (nrows.value > 1000){
          nrows.value = 1000
        }


        setnames(comp.file, "diffmeth.p.val", "P")
        comp.file <- data.frame(comp.file[1:nrows.value,])
        x <- qqr(comp.file)


        # Make sure it closes when we exit this reactive, even if there's an error
        #on.exit(progress$close())

        x
      }
      else{
        x <- list()
        x
      }
    }


  })

  list.pvalues_old_1 <- reactive({



    qq.value <- as.character(input$input_dmcomp_choices_1)

    qq.dir <- file.path(results.dir(), qq.value)

    #qq.value <- as.character(input$input_dmcomp_files_1)


    if (qq.value == "" || qq.value == "NA"){
      x <- list()
      x
    }
    else{
      #fucntion from the RnBeadsInterface package

      #index_list() contains the index of the selected file ffrom the dropdown
      f = paste("diffMethTable_site_cmp",index_list_1(), ".csv",sep = '')

      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){

#         # Create a Progress object
#         progress <- shiny::Progress$new()
#
#         progress$set(message = "Making QQ Plot", value = 50)

        #x <- comparison_plot(qq.dir , f)

        filename <- file.path(qq.dir, 'differential_methylation_data',f)
        filename= as.character(filename)
        nrows.value <- as.integer(input$input_multiqqplot_readtop)


        comp.file <- fread(filename,sep = ",", select = c("diffmeth.p.val"), nrows = nrows.value)


        if (nrows.value == -1){
          nrows.value = 100
        }
        else if (nrows.value > 1000){
          nrows.value = 1000
        }



        list.diff.p.values <- as.data.frame(comp.file)

        list.diff.p.values <- as.matrix(list.diff.p.values)

        list.diff.p.values <- lapply(seq_len(ncol(list.diff.p.values)), function(col) list.diff.p.values[,col])

        x <- unlist(list.diff.p.values)

        # Make sure it closes when we exit this reactive, even if there's an error
        #on.exit(progress$close())

        x
      }
      else{
        x <- list()
        x
      }
    }


  })
  ################################################

  # for Repository 2
  observeEvent(input$input_dmcomp_choices_2,{
    v$data <- FALSE

    input_choices <- as.character(input$input_dmcomp_choices_2)

    qq.dir <- file.path(results.dir(), input_choices)

    # qq.dmd.dir <- file.path(qq.dir, 'differential_methylation_data')
    #
    # input_c = list.files(path = qq.dmd.dir)

    # Extracting the values from the table from differential methylation html file and displaying the values of comparisons in the dropdown

    if (input_choices != "NA"){



      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation.html',sep="/") }) ) ){

        filename <- file.path(qq.dir,'differential_methylation.html')

        differential.methylation.path <- filename


        webpage <- readLines(tc <- textConnection(differential.methylation.path)); close(tc)
        pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)


        query = "//*/div[@id='section3']/ul/li"
        dates = xpathSApply(pagetree, query, xmlValue)
        dates
        comp_names <- list()
        comp_names_counter <- 1
        for (i in 1:length(dates)) {

          comp_names[comp_names_counter] <- dates[i]
          comp_names_counter = comp_names_counter + 1


        }

        choices.list <- comp_names
      }
      else{
        choices.list <- 'NA'
      }
    }
    else{
      choices.list <- 'NA'

    }


    updateSelectInput(session, "input_dmcomp_files_2",
                      label = paste("Comparison 2", ""),
                      choices = choices.list)




  })


  # returns the index of selected comparison file in QQplot 1
  index_list_2 <- eventReactive(input$input_dmcomp_files_2, {



    input_choices <- as.character(input$input_dmcomp_choices_2)

    qq.dir <- file.path(results.dir(), input_choices)


    # Extracting the values from the table from differential methylation html file and displaying the values of comparisons in the dropdown

    choice.index <- '1'

    if (input_choices != "NA"){


      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation.html',sep="/") }) ) ){

        filename <- file.path(qq.dir,'differential_methylation.html')

        differential.methylation.path <- filename


        webpage <- readLines(tc <- textConnection(differential.methylation.path)); close(tc)
        pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)

        query = "//*/div[@id='section3']/ul/li"
        dates = xpathSApply(pagetree, query, xmlValue)


        for (i in 1:length(dates)) {

          #if statement is not vectorized. For vectorized if statements you should use ifelse
          #ifelse(length(comp_names)>0,choices.list <- comp_names, choices.list <- 'NA')

          if (identical(input$input_dmcomp_files_2, dates[i])){


            choice.index <- as.character(i)




            break


          }


        }

      }
      else{
        choice.index <- '1'

      }
    }
    else{
      choice.index <- '1'


    }

    return(choice.index)
  })

  list.pvalues_2 <- reactive({



    qq.value <- as.character(input$input_dmcomp_choices_2)

    qq.dir <- file.path(results.dir(), qq.value)

    #qq.value <- as.character(input$input_dmcomp_files_2)


    if (qq.value == "" || qq.value == "NA"){
      x <- list()
      x
    }
    else{
      #fucntion from the RnBeadsInterface package

      #index_list() contains the index of the selected file ffrom the dropdown
      f = paste("diffMethTable_site_cmp",index_list_2(), ".csv",sep = '')

      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){

        # Create a Progress object
        #progress <- shiny::Progress$new()

        #progress$set(message = "Making QQ Plot", value = 50)

        #x <- comparison_plot(qq.dir , f)

        filename <- file.path(qq.dir, 'differential_methylation_data',f)
        filename= as.character(filename)
        nrows.value <- as.integer(input$input_multiqqplot_readtop)

        comp.file <- fread(filename,sep = ",", select = c("diffmeth.p.val") , nrows = nrows.value )

        if (nrows.value == -1){
          nrows.value = 100
        }
        else if (nrows.value > 1000){
          nrows.value = 1000
        }

        setnames(comp.file, "diffmeth.p.val", "P")
        comp.file <- data.frame(comp.file[1:nrows.value,])
        y <- qqr(comp.file)



        # Make sure it closes when we exit this reactive, even if there's an error
        #on.exit(progress$close())

        y
      }
      else{
        y <- list()
        y
      }
    }


  })


  list.pvalues_old_2 <- reactive({



    qq.value <- as.character(input$input_dmcomp_choices_2)

    qq.dir <- file.path(results.dir(), qq.value)

    #qq.value <- as.character(input$input_dmcomp_files_2)


    if (qq.value == "" || qq.value == "NA"){
      x <- list()
      x
    }
    else{
      #fucntion from the RnBeadsInterface package

      #index_list() contains the index of the selected file ffrom the dropdown
      f = paste("diffMethTable_site_cmp",index_list_2(), ".csv",sep = '')

      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){

        # Create a Progress object
        #progress <- shiny::Progress$new()

        #progress$set(message = "Making QQ Plot", value = 50)

        #x <- comparison_plot(qq.dir , f)

        filename <- file.path(qq.dir, 'differential_methylation_data',f)
        filename= as.character(filename)
        nrows.value <- as.integer(input$input_multiqqplot_readtop)

        comp.file <- fread(filename,sep = ",", select = c("diffmeth.p.val") , nrows = nrows.value )

        if (nrows.value == -1){
          nrows.value = 100
        }
        else if (nrows.value > 1000){
          nrows.value = 1000
        }


        list.diff.p.values <- as.data.frame(comp.file)

        list.diff.p.values <- as.matrix(list.diff.p.values)

        list.diff.p.values <- lapply(seq_len(ncol(list.diff.p.values)), function(col) list.diff.p.values[,col])

        y <- unlist(list.diff.p.values)



        # Make sure it closes when we exit this reactive, even if there's an error
        #on.exit(progress$close())

        y
      }
      else{
        y <- list()
        y
      }
    }


  })




  observeEvent(input$displayBtn, {

    v$data <- TRUE

    print (v$data)




    output$multicompqqplot1 <- renderPlotly({

      # dist <- switch(input$dist,
      #                unif = runif,
      #                norm = rnorm,
      #
      #                # lnorm = rlnorm,
      #                # exp = rexp,
      #                rnorm)

      if (is.null(v$data)) {
        print (v$data)
        return()
      }

      else if (identical(v$data, FALSE)) {
        print (v$data)
        return()
      }

      else {




        if(length(list.pvalues_1()) == 0) {

          # print error/ warning message


#           qqplot(1,1,main="Normal Q-Q Plot", ylab="diffmeth.p.val")
#           text(1,1,"No data available or no comparison file exist from repository 1")

          Primates <- c('No Data Avaiable for analysis 1')
          Bodywt <- c(0.5 )
          Brainwt <- c(0.5)

          data <- data.frame(Primates, Bodywt, Brainwt)

          pdf(NULL)
          q <- plot_ly(data,x = ~Bodywt, y = ~Brainwt, type = 'scatter',
                       mode = 'text', text = ~Primates, textposition = 'middle center',
                       textfont = list(color = '#000000', size = 16))%>%
            layout(title = 'Q-Q Plot',
                   xaxis = list(title = 'Expected -log10(p)',
                                zeroline = TRUE,
                                range = c(0, 1)),
                   yaxis = list(title = 'Observed -log10(p)',
                                range = c(0,1)))


          dev.off()

          q


        }

        else{

          # Create a Progress object
          progress <- shiny::Progress$new()

          progress$set(message = "Making analysis 1 QQ Plot", value = 50)

          x<- list.pvalues_1()




          #my.pvalue.list<-list("Analysis 1"=x, "Analysis 2"=y)
          #q <- qqunif.plot(my.pvalue.list, auto.key=list(corner=c(.95,.05)))
          pdf(NULL)
          q <- qqly(x, col = "#6087ea", size = 1, type = 20, abline_col = "pink",
                    abline_size = 0.5, abline_type = 1, highlight = NULL,
                    highlight_color = "#00FF00", xlab = "Expected -log10(p)",
                    ylab = "Observed -log10(p)", title = "")

          dev.off()
          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())

          q

        }


      }

    })


    # plotly ouput for analysis 2 qq plot

      output$multicompqqplot2 <- renderPlotly({

        if (is.null(v$data)) {
          print (v$data)
          return()
        }

        else if (identical(v$data, FALSE)) {
          print (v$data)
          return()
        }

        else {


          if (length(list.pvalues_2()) == 0){
            # print error/ warning message
            #           qqplot(1,1,main="Normal Q-Q Plot", ylab="diffmeth.p.val")
            #           text(1,1,"No data available or no comparison file exist from repository 2")

            Primates <- c('No Data Avaiable from analysis 2')
            Bodywt <- c(0.5 )
            Brainwt <- c(0.5)

            data <- data.frame(Primates, Bodywt, Brainwt)

            pdf(NULL)
            q <- plot_ly(data,x = ~Bodywt, y = ~Brainwt, type = 'scatter',
                         mode = 'text', text = ~Primates, textposition = 'middle center',
                         textfont = list(color = '#000000', size = 16))%>%
              layout(title = 'Q-Q Plot',
                     xaxis = list(title = 'Expected -log10(p)',
                                  zeroline = TRUE,
                                  range = c(0, 1)),
                     yaxis = list(title = 'Observed -log10(p)',
                                  range = c(0,1)))

            dev.off()

            q

          }
          else{

            # Create a Progress object
            progress <- shiny::Progress$new()

            progress$set(message = "Making analysis 2 QQ Plot", value = 50)


            y<- list.pvalues_2()



            #my.pvalue.list<-list("Analysis 1"=x, "Analysis 2"=y)
            #q <- qqunif.plot(my.pvalue.list, auto.key=list(corner=c(.95,.05)))
            pdf(NULL)
            q <- qqly(y, col = "red", size = 1, type = 20, abline_col = "pink",
                      abline_size = 0.5, abline_type = 1, highlight = NULL,
                      highlight_color = "green", xlab = "Expected -log10(p)",
                      ylab = "Observed -log10(p)", title = "")

            dev.off()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())

            q

          }


        }

      })


      #old
      output$multicompqqplot <- renderPlot({

        # dist <- switch(input$dist,
        #                unif = runif,
        #                norm = rnorm,
        #
        #                # lnorm = rlnorm,
        #                # exp = rexp,
        #                rnorm)

        if (is.null(v$data)) {
          print (v$data)
          return()
        }

        else if (identical(v$data, FALSE)) {
          print (v$data)
          return()
        }

        else {




          if(length(list.pvalues_1()) == 0) {

            # print error/ warning message


            qqplot(1,1,main="Normal Q-Q Plot", ylab="diffmeth.p.val")
            text(1,1,"No data available or no comparison file exist from repository 1")

          }
          else if (length(list.pvalues_2()) == 0){
            # print error/ warning message
            qqplot(1,1,main="Normal Q-Q Plot", ylab="diffmeth.p.val")
            text(1,1,"No data available or no comparison file exist from repository 2")
          }
          else{

            # Create a Progress object
            progress <- shiny::Progress$new()

            progress$set(message = "Makind old QQ Plot", value = 50)

            x<- list.pvalues_old_1()
            y<- list.pvalues_old_2()

            #qqplot(x,y,main="Normal Q-Q Plot", xlab="diffmeth.p.val 1", ylab="diffmeth.p.val 2")
            my.pvalue.list<-list("Analysis 1"=x, "Analysis 2"=y)
            q <- qqunif.plot(my.pvalue.list)

            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())

            q

          }


        }

      }, height = 400, width = 600)


  })


  ########################################################################################################################
  ##
  ## Nav Bar Tab : Table Browser
  ##
  ########################################################################################################################


  ############################################################################################

  # Table browser filling up the comparisons in the dropdown
  ############################################################################################

  checkDisplay <- reactiveValues(data = FALSE , data2 = FALSE)

  observeEvent(input$input_tablebrowser_choices,{

    checkDisplay$data <- FALSE
    checkDisplay$data2 <- FALSE

    shinyjs::hide(id = "id_tb_filterPlot_Btn")
    shinyjs::hide(id = "id_tb_filterPlot")

    input_choices <- as.character(input$input_tablebrowser_choices)

    qq.dir <- file.path(results.dir(), input_choices)

    # Extracting the values from the table from differential methylation html file and displaying the values of comparisons in the dropdown

    if (input_choices != "NA"){


      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation.html',sep="/") }) ) ){

        filename <- file.path(qq.dir,'differential_methylation.html')

        differential.methylation.path <- filename


        webpage <- readLines(tc <- textConnection(differential.methylation.path)); close(tc)
        pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)

        query = "//*/div[@id='section3']/ul/li"
        dates = xpathSApply(pagetree, query, xmlValue)
        dates
        comp_names <- list()
        comp_names_counter <- 1
        for (i in 1:length(dates)) {

          comp_names[comp_names_counter] <- dates[i]
          comp_names_counter = comp_names_counter + 1

        }


        choices.list <- comp_names


      }
      else{
        choices.list <- 'NA'
      }
    }
    else{
      choices.list <- 'NA'

    }



    updateSelectInput(session, "input_tablebrowser_files",
                      label = paste("Comparison", ""),
                      choices = choices.list)


  })


  # returns the index of selected comparison file in table browser section
  comp.file.index <- eventReactive(input$input_tablebrowser_files, {

    #checkDisplay$data <- FALSE
    shinyjs::hide(id = "id_tb_filterPlot")

    input_choices <- as.character(input$input_tablebrowser_choices)

    qq.dir <- file.path(results.dir(), input_choices)


    # Extracting the values from the table from differential methylation html file and displaying the values of comparisons in the dropdown

    choice.index <- '1'

    if (input_choices != "NA"){


      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation.html',sep="/") }) ) ){

        filename <- file.path(qq.dir,'differential_methylation.html')

        differential.methylation.path <- filename


        webpage <- readLines(tc <- textConnection(differential.methylation.path)); close(tc)
        pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)

        query = "//*/div[@id='section3']/ul/li"
        dates = xpathSApply(pagetree, query, xmlValue)


        for (i in 1:length(dates)) {

          if (identical(input$input_tablebrowser_files, dates[i])){

            choice.index <- as.character(i)

            break
          }
        }

      }
      else{
        choice.index <- '1'

      }
    }
    else{
      choice.index <- '1'


    }

    return(choice.index)
  })

  ############################################################################################

  # showing comparisons in the Table Browser tab
  ############################################################################################

  observeEvent(input$displayTableBrowserBtn,{


    checkDisplay$data <- TRUE
    checkDisplay$data2 <- FALSE


    shinyjs::show(id = "id_tb_filterPlot_Btn")

    shinyjs::hide(id = "id_tb_filterPlot")

      output$output.comparison.file <- renderDataTable({


        if (is.null(checkDisplay$data)) {
          print (checkDisplay$data)
          return()
        }

        else if (identical(checkDisplay$data, FALSE)) {
          print (checkDisplay$data)
          return()
        }

        else {


            checkDisplay$data2 <- FALSE

            qq.value <- as.character(input$input_tablebrowser_choices)

            qq.dir <- file.path(results.dir(), qq.value)


            if (qq.value == "" || qq.value == "NA"){
              dataset <- data.table( data = "No data available.")
            }
            else{



              #index_list() contains the index of the selected file ffrom the dropdown
              f = paste("diffMethTable_site_cmp",comp.file.index(), ".csv",sep = '')

              if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){

                # Create a Progress object
                progress <- shiny::Progress$new()

                progress$set(message = "Reading data! please wait...", value = 50)


                filename <- file.path(qq.dir, 'differential_methylation_data',f)


                filename= as.character(filename)

                nrows.value <- as.character(input$input_tablebrowser_readtop)

                if (nrows.value == 'ALL'){
                  nrows.value = -1
                }

                # fread function from the library data.table
                comp.file <- fread(filename,sep = ",", nrows = nrows.value )
                #comp.file <- fread(filename,sep = ",")

                comp.file <- as.data.frame(comp.file)



                updateSelectInput(session, "input_tablebrowser_x_axis",
                                  label = paste("Select x-axis:"),
                                  choices = as.character(as.vector(names(comp.file))))

                updateSelectInput(session, "input_tablebrowser_y_axis",
                                  label = paste("Select y-axis:"),
                                  choices = as.character(as.vector(names(comp.file))))

                dataset <- data.table( comp.file)


                inFile <- input$file1

                if (!is.null(inFile)){
                  input.file <- fread(inFile$datapath, header = input$header,
                                      sep = input$sep)

                  dataset <- merge(dataset, input.file, by.x="cgid", by.y="cgid", all.x="TRUE")

                }





                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())

                dataset
              }
              else{
                dataset <- data.table( data = "No data available.")
              }
            }





            dataset

        }




      },selection = 'single', filter = 'top',

      extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL),
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,
        dom = 'Blfrtip',
        buttons = list(
          'copy',
          'print',
          list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          ),
          I('colvis')

        ),
        br(),
        keys = TRUE

      ), escape = TRUE)
  })


  observeEvent(input$displayPlotBtn,{


    #checkDisplay$data <- FALSE
    checkDisplay$data2 <- TRUE
    shinyjs::show(id = "id_tb_filterPlot")

    output$x5 <- renderPlotly({



      if (is.null(checkDisplay$data2)) {



        return()


      }

      else if (identical(checkDisplay$data2, FALSE)) {



        return()

      }

      else {

            # use the key aesthetic/argument to help uniquely identify selected observations
        filtered_data <- input$output.comparison.file_rows_all
        print(length(filtered_data))

        if (length(filtered_data) <  2)
        {
          Primates <- c('No Data Avaiable')
          Bodywt <- c(0.5 )
          Brainwt <- c(0.5)

          data <- data.frame(Primates, Bodywt, Brainwt)


          pdf(NULL)
          p <- plot_ly(data,x = ~Bodywt, y = ~Brainwt, type = 'scatter',
                       mode = 'text', text = ~Primates, textposition = 'middle center',
                       textfont = list(color = '#000000', size = 16))%>%
            layout(                        # all of layout's properties: /r/reference/#layout
              title = "Plot", # layout's title: /r/reference/#layout-title
              xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                title = "mean.diff",      # xaxis's title: /r/reference/#layout-xaxis-title
                showgrid = F),       # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
              yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                title = "diffmeth.p.val")     # yaxis's title: /r/reference/#layout-yaxis-title
            )
          dev.off()
          p
        }
        else if (length(filtered_data) > 50000)
        {
          Primates <- c('Data too big to display in plot! please filter some data')
          Bodywt <- c(0.5 )
          Brainwt <- c(0.5)

          data <- data.frame(Primates, Bodywt, Brainwt)

          pdf(NULL)
          p <- plot_ly(data,x = ~Bodywt, y = ~Brainwt, type = 'scatter',
                       mode = 'text', text = ~Primates, textposition = 'middle center',
                       textfont = list(color = '#000000', size = 14))%>%
            layout(                        # all of layout's properties: /r/reference/#layout
              title = "Plot", # layout's title: /r/reference/#layout-title
              xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                title = "mean.diff",      # xaxis's title: /r/reference/#layout-xaxis-title
                showgrid = F),       # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
              yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                title = "diffmeth.p.val")     # yaxis's title: /r/reference/#layout-yaxis-title
            )
          dev.off()
          p

        }
        else{

            qq.value <- as.character(input$input_tablebrowser_choices)

            qq.dir <- file.path(results.dir(), qq.value)


            if (qq.value == "" || qq.value == "NA"){
              dataset <- data.table( data = "No data available.")
            }
            else{



              #index_list() contains the index of the selected file ffrom the dropdown
              f = paste("diffMethTable_site_cmp",comp.file.index(), ".csv",sep = '')

              if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){

                # Create a Progress object
                progress <- shiny::Progress$new()

                progress$set(message = "Making Plot! please wait...", value = 50)


                filename <- file.path(qq.dir, 'differential_methylation_data',f)


                filename= as.character(filename)

                # fread function from the library data.table
                comp.file <- fread(filename,sep = ",")
                #comp.file <- fread(filename,sep = ",")

                comp.file <- as.data.frame(comp.file)


                filtered <- comp.file[filtered_data, , drop = FALSE]
                key <- colnames(comp.file) <- names(comp.file)
                print(key)

                pdf(NULL)
                p <- plot_ly(filtered,
                             type = "scatter",        # all "scatter" attributes: https://plot.ly/r/reference/#scatter
                             x = ~filtered[,c(input$input_tablebrowser_x_axis)],               # more about scatter's "x": /r/reference/#scatter-x
                             y = ~filtered[,c(input$input_tablebrowser_y_axis)],            # more about scatter's "y": /r/reference/#scatter-y
                             name = "Plot",   # more about scatter's "name": /r/reference/#scatter-name
                             marker = list(           # marker is a named list, valid keys: /r/reference/#scatter-marker
                               color="#264E86"        # more about marker's "color" attribute: /r/reference/#scatter-marker-color
                             )) %>%

                  # add_trace(x = ~mean.diff,                                         # scatter's "x": /r/reference/#scatter-x
                  #           y = ~diffmeth.p.val,  # scatter's "y": /r/reference/#scatter-y
                  #           mode = 'lines',                                    # scatter's "y": /r/reference/#scatter-mode
                  #           line = list(                                       # line is a named list, valid keys: /r/reference/#scatter-line
                  #             color = "#5E88FC",                               # line's "color": /r/reference/#scatter-line-color
                  #             dash = "dashed"                                  # line's "dash" property: /r/reference/#scatter-line-dash
                  #           )
                  # ) %>%

                  layout(                        # all of layout's properties: /r/reference/#layout
                    title = "Plot", # layout's title: /r/reference/#layout-title
                    xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                      title = input$input_tablebrowser_x_axis,      # xaxis's title: /r/reference/#layout-xaxis-title
                      showgrid = F),       # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                    yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                      title = input$input_tablebrowser_y_axis)     # yaxis's title: /r/reference/#layout-yaxis-title
                  )

                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                dev.off()

                p
              }

            }

        }
      }
    })

  })



  observeEvent(input$displaySimplePlotBtn,{



    shinyjs::show(id = "id_tb_filterPlot")

    output$simpleTBPlot <- renderPlot({


        # use the key aesthetic/argument to help uniquely identify selected observations
        filtered_data <- input$output.comparison.file_rows_all
        print(length(filtered_data))

        if (length(filtered_data) <  2)
        {
          p <- plot('no data available')

          # qqplot(1,1,main="Normal Q-Q Plot", ylab="diffmeth.p.val")
          # text(1,1,"No data available or no comparison file exist")

        }
        else{

          qq.value <- as.character(input$input_tablebrowser_choices)

          qq.dir <- file.path(results.dir(), qq.value)


          if (qq.value == "" || qq.value == "NA"){
            dataset <- data.table( data = "No data available.")
          }
          else{



            #index_list() contains the index of the selected file ffrom the dropdown
            f = paste("diffMethTable_site_cmp",comp.file.index(), ".csv",sep = '')

            if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){

              # Create a Progress object
              progress <- shiny::Progress$new()

              progress$set(message = "Making Plot! please wait...", value = 50)


              filename <- file.path(qq.dir, 'differential_methylation_data',f)


              filename= as.character(filename)

              # fread function from the library data.table
              comp.file <- fread(filename,sep = ",", select = c( "id","mean.diff"))
              #comp.file <- fread(filename,sep = ",")

              comp.file <- as.data.frame(comp.file)


              key <- colnames(comp.file) <- c("id","mean.diff")
              print(key)

              p <- qqnorm(comp.file[filtered_data, , drop = FALSE]$mean.diff   )

              # Make sure it closes when we exit this reactive, even if there's an error
              on.exit(progress$close())


              p

            }
          }
        }


    })



  })


  output$contents <- renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    fread(inFile$datapath, header = input$header,
             sep = input$sep)


  },selection = 'single', filter = 'top',

  extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL),
  options = list(
    dom = 'Blfrtip',
    buttons = list(
      'copy',
      'print',
      list(
        extend = 'collection',
        buttons = c('csv', 'excel', 'pdf'),
        text = 'Download'
      ),
      I('colvis')

    ),
    br(),
    keys = TRUE

  ), escape = TRUE)


  output$mergedData <- renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    qq.value <- as.character(input$input_tablebrowser_choices)

    qq.dir <- file.path(results.dir(), qq.value)


    if (qq.value == "" || qq.value == "NA"){
      dataset <- data.table( data = "Some information is missing to do the merging.")
    }
    else{



      #index_list() contains the index of the selected file ffrom the dropdown
      f = paste("diffMethTable_site_cmp",index_list(), ".csv",sep = '')

      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){

        # Create a Progress object
        progress <- shiny::Progress$new()

        progress$set(message = "Reading data! please wait...", value = 50)


        filename <- file.path(qq.dir, 'differential_methylation_data',f)


        filename= as.character(filename)

        # fread function from the library data.table
        comp.file <- fread(filename,sep = ",", select = c("cgid","Chromosome","diffmeth.p.val","diffmeth.p.adj.fdr","mean.covg.Normal"))

        comp.file <- as.data.frame(comp.file)




        dataset <- data.table( comp.file)


        dataset

        inFile <- input$file1

        if (is.null(inFile))
          return(NULL)

        input.file <- fread(inFile$datapath, header = input$header,
                            sep = input$sep)

        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())


      }
      else{
        dataset <- data.table( data = "No data available.")
      }
    }





  },selection = 'single', filter = 'top',

  extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL),
  options = list(
    dom = 'Blfrtip',
    buttons = list(
      'copy',
      'print',
      list(
        extend = 'collection',
        buttons = c('csv', 'excel', 'pdf'),
        text = 'Download'
      ),
      I('colvis')

    ),
    br(),
    keys = TRUE

  ), escape = TRUE)



  ########################################################################################################################
  ##
  ## Nav Bar Tab : Top Scorer
  ##
  ########################################################################################################################

  ############################################################################################

  # Top scorer implementation of the input select comparisons
  ############################################################################################

  output$cb <- renderUI({

    choices = list.files(path = selectedDir)


#     choices_names <- list()
#     choices_names_counter <- 1
#
#     lapply(1:length(choices), function(i) {
#
#
#       input_choices <- as.character(choices[i])
#       qq.dir <- file.path(results.dir(), input_choices)
#
#
#       if (input_choices != "NA"){
#         if ( file.exists( isolate({ paste(qq.dir,'differential_methylation.html',sep="/") }) ) ){
#
#             choices_names[choices_names_counter] <- input_choices
#             choices_names_counter = choices_names_counter + 1
#
#           }
#       }
#     })


    checkboxGroupInput(paste0("cb_ts_comp_venn"), "", choices)



  })

  output$ts.columns <- renderUI({
    selectInput("input_topscorer_columns",
                label = paste(""),
                choices = c("mean.diff","diffmeth.p.val","diffmeth.p.adj.fdr","mean.quot.log2"))
  })

  output$ts.columns.equality <- renderUI({
    selectInput("input_topscorer_columns_equality",
                label = paste(""),
                choices = c(">","<", ">=","<=","="))
  })

  output$ts.columns.range <- renderUI({
    selectInput("input_topscorer_columns_range",
                label = paste(""),
                choices = c("0.01","0.1", "0.05","0.5","0","1"))
  })



# returns the index of selected comparison file in table browser section
get.choice.index <- reactive({

  # preparing data to display in Venn diagram and in data table
  cb.checked <- c(input$cb_ts_comp_venn)

  choice.index <- list()

  choices = cb.checked
  for (i in 1:length(choices)) {


    input_choices <- as.character(choices[i])
    qq.dir <- file.path(results.dir(), input_choices)


    if (input_choices != "NA"){
      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation.html',sep="/") }) ) ){

        filename <- file.path(qq.dir,'differential_methylation.html')

        differential.methylation.path <- filename


        webpage <- readLines(tc <- textConnection(differential.methylation.path)); close(tc)
        pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)


        query = "//*/div[@id='section3']/ul/li"
        dates = xpathSApply(pagetree, query, xmlValue)
        dates

        for (j in 1:length(dates)) {

          if ( i == 1 ){

            if (identical(input$si1, dates[j])){
              choice.index[i] <- as.character(j)
              break
            }

          }
          else if( i == 2 ){

            if (identical(input$si2, dates[j])){
              choice.index[i] <- as.character(j)
              break
            }

          }
          else if( i == 3 ){

            if (identical(input$si3, dates[j])){
              choice.index[i] <- as.character(j)
              break
            }

          }
          else if( i == 4 ){

            if (identical(input$si4, dates[j])){
              choice.index[i] <- as.character(j)
              break
            }

          }
          else if( i == 5 ){

            if (identical(input$si5, dates[j])){
              choice.index[i] <- as.character(j)
              break
            }

          }
          else if( i == 6 ){

            if (identical(input$si6, dates[j])){
              choice.index[i] <- as.character(j)
              break
            }

          }

          else{
            if (identical(input$si1, dates[j])){
              choice.index[i] <- as.character(j)
              break
            }

          }


        }
      }
    }


  }# end of outer for loop



  return(choice.index)

})


# returns the index of selected comparison file in QQplot 1
observeEvent(input$cb_ts_comp_venn, {
  # preparing data to display in Venn diagram and in data table
  cb.checked <- c(input$cb_ts_comp_venn)


  output$si <- renderUI({


    choices = cb.checked
    lapply(1:length(choices), function(i) {


      input_choices <- as.character(choices[i])
      qq.dir <- file.path(results.dir(), input_choices)


      if (input_choices != "NA"){
        if ( file.exists( isolate({ paste(qq.dir,'differential_methylation.html',sep="/") }) ) ){

          filename <- file.path(qq.dir,'differential_methylation.html')

          differential.methylation.path <- filename


          webpage <- readLines(tc <- textConnection(differential.methylation.path)); close(tc)
          pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)


          query = "//*/div[@id='section3']/ul/li"
          dates = xpathSApply(pagetree, query, xmlValue)
          dates
          comp_names <- list()
          comp_names_counter <- 1
          for (j in 1:length(dates)) {

            comp_names[comp_names_counter] <- dates[j]
            comp_names_counter = comp_names_counter + 1

          }

          choices.list <- comp_names
        }
        else{
          choices.list <- 'NA'

        }
      }
      else{
        choices.list <- 'NA'


      }
      selectInput(paste0("si",i), paste(input_choices,""), choices.list)
    })

  })

})




  # display venn diagram

  observeEvent(input$btnMultipleShowVenn ,{





    output$output.ts.multivenn.plot <- renderPlot({

      output$ts.venn.overlapping.error.value <- renderText({
        paste("")
      })

      output$comparison.check <- renderText({
        paste("Cho",get.choice.index())
      })

      cb.checked <- c(input$cb_ts_comp_venn)

      column_selected = as.character(input$input_topscorer_columns)
      equality = as.character(input$input_topscorer_columns_equality)
      range_selected = as.numeric(input$input_topscorer_columns_range)


      original.dataset.list <- list()
      filtered.dataset.list <- list()
      filtered.logical.dataset.list <- list()
      dataset.venn.count.list <- list()

      for (i in 1:length(cb.checked)) {

        analysis.selected <- as.character(cb.checked[i])
        analysis.path <- file.path(results.dir(), analysis.selected)
        nrows.value <- as.character(100)

        if (length(get.choice.index()[i]) < 1){

          get.choice.index()[i] <- 1
        }
        dataset <- rnbi.read.comparisondata(analysis.selected,analysis.path,as.integer(unlist(get.choice.index()[i])), nrows.value,column_selected)

        if(column_selected == "diffmeth.p.val"){
          colselected <- dataset$diffmeth.p.val
        }

        else if(column_selected == "diffmeth.p.adj.fdr"){
          colselected <- dataset$diffmeth.p.adj.fdr
        }
        else if(column_selected == "mean.quot.log2"){
          colselected <- dataset$mean.quot.log2
        }
        else{
          colselected <- dataset$mean.diff
        }

        # filtering the dataframe and also using the logical column dataset to use in venn diagram counts
        # and finally display the cg values based on the numbers generated by venn counts using apended
        # logical column in orginal dataset

        if(equality == ">="){
          original.dataset.list[[i]] <- dataset
          filtered.dataset.list[[i]] <- dataset[colselected > range_selected,]
          filtered.logical.dataset.list[[i]] <- (subset(dataset, select = c(column_selected) ) >= as.numeric( range_selected))
          original.dataset.list[[i]][,"logical"] <- filtered.logical.dataset.list[[i]]

        }
        else if(equality == ">"){
          original.dataset.list[[i]] <- dataset
          filtered.dataset.list[[i]] <- dataset[colselected > range_selected,]
          filtered.logical.dataset.list[[i]] <- (subset(dataset, select = c(column_selected) ) > as.numeric( range_selected))
          original.dataset.list[[i]][,"logical"] <- filtered.logical.dataset.list[[i]]

        }
        else if(equality == "<="){

          original.dataset.list[[i]] <- dataset
          filtered.dataset.list[[i]] <- dataset[colselected > range_selected,]
          filtered.logical.dataset.list[[i]] <- (subset(dataset, select = c(column_selected) ) <= as.numeric( range_selected))
          original.dataset.list[[i]][,"logical"] <- filtered.logical.dataset.list[[i]]

        }
        else if(equality == "<"){
          original.dataset.list[[i]] <- dataset
          filtered.dataset.list[[i]] <- dataset[colselected > range_selected,]
          filtered.logical.dataset.list[[i]] <- (subset(dataset, select = c(column_selected) ) < as.numeric( range_selected))
          original.dataset.list[[i]][,"logical"] <- filtered.logical.dataset.list[[i]]

        }
        else if(equality == "="){

          original.dataset.list[[i]] <- dataset
          filtered.dataset.list[[i]] <- dataset[colselected > range_selected,]
          filtered.logical.dataset.list[[i]] <- (subset(dataset, select = c(column_selected) ) == as.numeric( range_selected))
          original.dataset.list[[i]][,"logical"] <- filtered.logical.dataset.list[[i]]

        }
        else {
          original.dataset.list[[i]] <- dataset
          filtered.dataset.list[[i]] <- dataset[colselected > range_selected,]
          filtered.logical.dataset.list[[i]] <- (subset(dataset, select = c(column_selected) ) > as.numeric( range_selected))
          original.dataset.list[[i]][,"logical"] <- filtered.logical.dataset.list[[i]]

        }

        if(column_selected == "diffmeth.p.val"){
          original.dataset.list[[i]]$diffmeth.p.val <- sprintf("%.3f", original.dataset.list[[i]]$diffmeth.p.val)

        }

        else if(column_selected == "diffmeth.p.adj.fdr"){
          original.dataset.list[[i]]$diffmeth.p.adj.fdr <- sprintf("%.3f", original.dataset.list[[i]]$diffmeth.p.adj.fdr)

        }
        else if(column_selected == "mean.quot.log2"){
          original.dataset.list[[i]]$mean.quot.log2 <- sprintf("%.3f", original.dataset.list[[i]]$mean.quot.log2)

        }
        else{
          original.dataset.list[[i]]$mean.diff <- sprintf("%.3f", original.dataset.list[[i]]$mean.diff)

        }

        if (i == 1){
          clogicaldf <- cbind(filtered.logical.dataset.list[[1]])

        }
        else if (i == 2){
          clogicaldf <- cbind(filtered.logical.dataset.list[[1]], filtered.logical.dataset.list[[2]])

        }
        else if (i == 3){
          clogicaldf <- cbind(filtered.logical.dataset.list[[1]], filtered.logical.dataset.list[[2]], filtered.logical.dataset.list[[3]])

        }

        else if (i == 4){
          clogicaldf <- cbind(filtered.logical.dataset.list[[1]], filtered.logical.dataset.list[[2]], filtered.logical.dataset.list[[3]], filtered.logical.dataset.list[[4]])

        }
        else if (i == 5){
          clogicaldf <- cbind(filtered.logical.dataset.list[[1]], filtered.logical.dataset.list[[2]], filtered.logical.dataset.list[[3]], filtered.logical.dataset.list[[4]], filtered.logical.dataset.list[[5]])

        }
        else if (i == 6){
          clogicaldf <- cbind(filtered.logical.dataset.list[[1]], filtered.logical.dataset.list[[2]], filtered.logical.dataset.list[[3]], filtered.logical.dataset.list[[4]], filtered.logical.dataset.list[[5]], filtered.logical.dataset.list[[6]])

        }
        else {
          clogicaldf <- cbind(filtered.logical.dataset.list[[1]])
        }

        dataset.venn.count.list[[i]] <- vennCounts(clogicaldf)
      }#end of forloop


      if (length(cb.checked) == 1){

        output$output.ts.table.multivenn.plot.labels <- renderTable({
          Selected_analysis= c(paste("A = ",cb.checked[1]))
          result <- data.frame(Selected_analysis)
          data.table(result)
        })

        p <- vennDiagram(dataset.venn.count.list[[1]], include = "both",
                         names = paste (paste("A = ",cb.checked[1])),
                         cex = 1, counts.col = "red", circle.col = c("green"))




        output$ts.selector.overlapping.value <- renderUI({
          selectInput("input_ts_selector_overlapping_value",
                      label = paste("Venn Diagram numbers."),
                      choices = c(paste("!A", " = " , dataset.venn.count.list[[1]][1,2]),paste("A", " = " , dataset.venn.count.list[[1]][2,2])) , selected = -1 , width = validateCssUnit("100%"))
        })

        # updating the table and showing content

        # output the datatable according to the venn diagram count number selected
        output$output.topscorer.overlappingComparison <- renderDataTable({

          # Create a Progress object
          progress <- shiny::Progress$new()

          progress$set(message = "Reading data! please wait...", value = 50)


          selected_value = input$input_ts_selector_overlapping_value

          if(identical(as.character(selected_value) , as.character(paste("A", " = " , dataset.venn.count.list[[1]][2,2])))){
            c1 = original.dataset.list[[1]] [(original.dataset.list[[1]]$logical == "TRUE" ) ]
          }
          else{
            c1 = original.dataset.list[[1]] [(original.dataset.list[[1]]$logical == "FALSE" ) ]
          }
          c1$logical      <- NULL

          outputTableData <- c1

          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())

          outputTableData


        },selection = 'single', filter = 'top',
        extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL),
        options = list( scrollX = TRUE, scrollY = TRUE, dom = 'Blfrtip',buttons = list( 'copy', 'print',list( extend = 'collection',buttons = c('csv', 'excel', 'pdf'), text = 'Download'), I('colvis')),br(), keys = TRUE
        ), escape = TRUE)

      }
      else if(length(cb.checked) == 2){

        output$output.ts.table.multivenn.plot.labels <- renderTable({
          Selected_analysis= c(paste("A = ",cb.checked[1]), paste("B = ",cb.checked[2]))
          result <- data.frame(Selected_analysis)
          data.table(result)
        })

        # drawing venn diagram
        p <- vennDiagram(dataset.venn.count.list[[2]], include = "both",
                         names = c(paste("A = ",cb.checked[1]), paste("B = ",cb.checked[2])),
                         cex = 1, counts.col = "red", circle.col = c("green","blue"))




        # updating  the count numbers of the venn diagram under venn count selector option

        output$ts.selector.overlapping.value <- renderUI({
          selectInput("input_ts_selector_overlapping_value",
                      label = paste("Select CpGs contents based on Venn Diagram numbers."),
                      choices = c(paste("!AB", " = " , dataset.venn.count.list[[2]][1,3]), paste("B", " = " , dataset.venn.count.list[[2]][2,3]), paste("A", " = " , dataset.venn.count.list[[2]][3,3]) , paste("AB", " = " , dataset.venn.count.list[[2]][4,3])) , selected = -1 , width = validateCssUnit("100%"))
        })



        # updating the table and showing content

        # output the datatable according to the venn diagram count number selected
        output$output.topscorer.overlappingComparison <- renderDataTable({

          # Create a Progress object
          progress <- shiny::Progress$new()

          progress$set(message = "Reading data! please wait...", value = 50)


          selected_value = input$input_ts_selector_overlapping_value

          if(identical(as.character(selected_value) , as.character(paste("!AB", " = " , dataset.venn.count.list[[2]][1,3])))){
            c1 = original.dataset.list[[1]] [(original.dataset.list[[1]]$logical == "FALSE" ) ]

            c2 = original.dataset.list[[2]] [ (original.dataset.list[[2]]$logical == "FALSE" )]

          }
          else if(identical(as.character(selected_value) , as.character(paste("B", " = " , dataset.venn.count.list[[2]][2,3])))){
            c1 = original.dataset.list[[1]] [(original.dataset.list[[1]]$logical == "FALSE" ) ]

            c2 = original.dataset.list[[2]] [ (original.dataset.list[[2]]$logical == "TRUE" )]

          }
          else if(identical(as.character(selected_value) , as.character(paste("A", " = " , dataset.venn.count.list[[2]][3,3])))){
            c1 = original.dataset.list[[1]] [(original.dataset.list[[1]]$logical == "TRUE" ) ]

            c2 = original.dataset.list[[2]] [ (original.dataset.list[[2]]$logical == "FALSE" )]

          }
          else if(identical(as.character(selected_value) , as.character(paste("AB", " = " , dataset.venn.count.list[[2]][4,3])))){
            c1 = original.dataset.list[[1]] [(original.dataset.list[[1]]$logical == "TRUE" ) ]

            c2 = original.dataset.list[[2]] [ (original.dataset.list[[2]]$logical == "TRUE" )]

          }
          else{
            c1 = original.dataset.list[[1]] [(original.dataset.list[[1]]$logical == "FALSE" ) ]

            c2 = original.dataset.list[[2]] [ (original.dataset.list[[2]]$logical == "FALSE" )]

          }

          c1$logical      <- NULL
          c2$logical      <- NULL

          outputTableData <- merge(c1, c2, by = c("cgid","Chromosome","Start","Strand") , all = F)

          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())

          outputTableData


        },selection = 'single', filter = 'top',
        extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL),
        options = list( scrollX = TRUE, scrollY = TRUE, dom = 'Blfrtip',buttons = list( 'copy', 'print',list( extend = 'collection',buttons = c('csv', 'excel', 'pdf'), text = 'Download'), I('colvis')),br(), keys = TRUE
        ), escape = TRUE)

      }
      else if(length(cb.checked) == 3){


        output$output.ts.table.multivenn.plot.labels <- renderTable({
          Selected_analysis= c(paste("A = ",cb.checked[1]), paste("B = ",cb.checked[2]), paste("C = ",cb.checked[3]))
          result <- data.frame(Selected_analysis)
          data.table(result)
        })

        p <- vennDiagram(dataset.venn.count.list[[3]], include = "both",
                         names = c(paste ("A = ",cb.checked[1]), paste ("B = ",cb.checked[2]),paste ("C = ",cb.checked[3])),
                         cex = 1, counts.col = "red", circle.col = c("green","blue", "orange"))




        # updating  the count numbers of the venn diagram under venn count selector option
        choices.vector <- list()
        combination <- c("!ABC = ","C = ","B = ","BC = ","A = ","AC = ","AB = ","ABC = ")
        combinationLogicalA <- c("FALSE","FALSE","FALSE","FALSE","TRUE","TRUE","TRUE","TRUE")
        combinationLogicalB <- c("FALSE","FALSE","TRUE","TRUE","FALSE","FALSE","TRUE","TRUE")
        combinationLogicalC <- c("FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE")
        for (i in 1:8) {
          choices.vector[i] = paste(combination[i] , dataset.venn.count.list[[3]][i,4])

        }
        output$ts.selector.overlapping.value <- renderUI({
          selectInput("input_ts_selector_overlapping_value",
                      label = paste("Select CpGs contents based on Venn Diagram numbers."),
                      choices = choices.vector , selected = -1 , width = validateCssUnit("100%"))
          })

        output$ts.table.overlapping.value3 <- renderText({
          dataset.venn.count.list[[3]]
        })

        # updating the table and showing content

        # output the datatable according to the venn diagram count number selected
        output$output.topscorer.overlappingComparison <- renderDataTable({

          # Create a Progress object
          progress <- shiny::Progress$new()

          progress$set(message = "Reading data! please wait...", value = 50)


          selected_value = input$input_ts_selector_overlapping_value


          for (i in 1:8) {
            if(identical(as.character(selected_value) , as.character(paste(combination[i] , dataset.venn.count.list[[3]][i,4]))))
            {
              c1 = original.dataset.list[[1]] [(original.dataset.list[[1]]$logical == as.character(combinationLogicalA[i]) ) ]
              c2 = original.dataset.list[[2]] [(original.dataset.list[[2]]$logical == as.character(combinationLogicalB[i]) ) ]
              c3 = original.dataset.list[[3]] [(original.dataset.list[[3]]$logical == as.character(combinationLogicalC[i]) ) ]

              c1$logical      <- NULL
              c2$logical      <- NULL
              c3$logical      <- NULL
              #outputTableData <- Reduce(function(x, y) merge(x, y, by = c("cgid","Chromosome","Start","Strand"), all=F), list(c1, c2))
              c <- merge(c1, c2, by = c("cgid","Chromosome","Start","Strand") , all = F)

              outputTableData <- merge(c, c3, by = c("cgid","Chromosome","Start","Strand") , all = F)

              break
            }
          }




          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())

          outputTableData


        },selection = 'single', filter = 'top',
        extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL),
        options = list( scrollX = TRUE, scrollY = TRUE, dom = 'Blfrtip',buttons = list( 'copy', 'print',list( extend = 'collection',buttons = c('csv', 'excel', 'pdf'), text = 'Download'), I('colvis')),br(), keys = TRUE
        ), escape = TRUE)


      }

      else if(length(cb.checked) == 4){


        output$output.ts.table.multivenn.plot.labels <- renderTable({
          Selected_analysis= c(paste("A = ",cb.checked[1]), paste("B = ",cb.checked[2]), paste("C = ",cb.checked[3]), paste("D = ",cb.checked[4]))
          result <- data.frame(Selected_analysis)
          data.table(result)
        })

        p <- vennDiagram(dataset.venn.count.list[[4]], include = "both",
                         names = c(paste ("A = ",cb.checked[1]), paste ("B = ",cb.checked[2]),paste ("C = ",cb.checked[3]),paste ("D = ",cb.checked[4])),
                         cex = 1, counts.col = "red", circle.col = c("green","blue", "orange","yellow"))




        # updating  the count numbers of the venn diagram under venn count selector option
        choices.vector <- list()
        combination <- c("!ABCD = ","D = ","C = ","DC = ","B = ","BD = ","BC = ","BCD = ","A = ","AD = ","AC = ","ACD = ","AB = ","ABD = ","ABC = ","ABCD = ")
        combinationLogicalA <- c("FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE", "TRUE","TRUE","TRUE","TRUE" ,"TRUE","TRUE","TRUE","TRUE")
        combinationLogicalB <- c("FALSE","FALSE","FALSE","FALSE","TRUE","TRUE","TRUE","TRUE","FALSE","FALSE","FALSE","FALSE","TRUE","TRUE","TRUE","TRUE")
        combinationLogicalC <- c("FALSE","FALSE","TRUE","TRUE","FALSE","FALSE","TRUE","TRUE","FALSE","FALSE","TRUE","TRUE","FALSE","FALSE","TRUE","TRUE")
        combinationLogicalD <- c("FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE")
        for (i in 1:16) {
          choices.vector[i] = paste(combination[i] , dataset.venn.count.list[[4]][i,5])

        }
        output$ts.selector.overlapping.value <- renderUI({
          selectInput("input_ts_selector_overlapping_value",
                      label = paste("Select CpGs contents based on Venn Diagram numbers."),
                      choices = choices.vector , selected = -1 , width = validateCssUnit("100%"))
        })

        output$ts.table.overlapping.value3 <- renderText({
          dataset.venn.count.list[[4]]
        })

        # updating the table and showing content

        # output the datatable according to the venn diagram count number selected
        output$output.topscorer.overlappingComparison <- renderDataTable({

          # Create a Progress object
          progress <- shiny::Progress$new()

          progress$set(message = "Reading data! please wait...", value = 50)


          selected_value = input$input_ts_selector_overlapping_value


          for (i in 1:16) {
            if(identical(as.character(selected_value) , as.character(paste(combination[i] , dataset.venn.count.list[[4]][i,5]))))
            {
              c1 = original.dataset.list[[1]] [(original.dataset.list[[1]]$logical == as.character(combinationLogicalA[i]) ) ]
              c2 = original.dataset.list[[2]] [(original.dataset.list[[2]]$logical == as.character(combinationLogicalB[i]) ) ]
              c3 = original.dataset.list[[3]] [(original.dataset.list[[3]]$logical == as.character(combinationLogicalC[i]) ) ]
              c4 = original.dataset.list[[4]] [(original.dataset.list[[4]]$logical == as.character(combinationLogicalD[i]) ) ]

              c1$logical      <- NULL
              c2$logical      <- NULL
              c3$logical      <- NULL
              c4$logical      <- NULL
              #outputTableData <- Reduce(function(x, y) merge(x, y, by = c("cgid","Chromosome","Start","Strand"), all=F), list(c1, c2))
              c12 <- merge(c1, c2, by = c("cgid","Chromosome","Start","Strand") , all = F)
              c34 <- merge(c3, c4, by = c("cgid","Chromosome","Start","Strand") , all = F)
              c1234 <- merge(c12, c34, by = c("cgid","Chromosome","Start","Strand") , all = F)

              outputTableData <- c1234

              break
            }
          }




          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())

          outputTableData


        },selection = 'single', filter = 'top',
        extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL),
        options = list( scrollX = TRUE, scrollY = TRUE, dom = 'Blfrtip',buttons = list( 'copy', 'print',list( extend = 'collection',buttons = c('csv', 'excel', 'pdf'), text = 'Download'), I('colvis')),br(), keys = TRUE
        ), escape = TRUE)

      }

      else if(length(cb.checked) == 5){

        output$output.ts.table.multivenn.plot.labels <- renderTable({
          Selected_analysis= c(paste("A = ",cb.checked[1]), paste("B = ",cb.checked[2]), paste("C = ",cb.checked[3]), paste("D = ",cb.checked[4]), paste("E = ",cb.checked[5]))
          result <- data.frame(Selected_analysis)
          data.table(result)
        })

        p <- vennDiagram(dataset.venn.count.list[[5]], include = "both",
                         names = c(paste ("A = ",cb.checked[1]), paste ("B = ",cb.checked[2]),paste ("C = ",cb.checked[3]),paste ("D = ",cb.checked[4]),paste ("E = ",cb.checked[5])),
                         cex = 1, counts.col = "red", circle.col = c("green","blue", "orange","yellow"))




        # updating  the count numbers of the venn diagram under venn count selector option
        choices.vector <- list()
        combination <- c("!ABCDE = ","E = ","D = ","ED = ","C = ","CE = ","CD = ","CDE = ","B = ","BE = ","BD = ","BDE = ","BC = ","BCE = ","BCD = ","BCDE = ","A = ","AE = ","AD = ","ADE = ","AC = ","ACE = ","ACD = ","ACDE = ","AB = ","ABE = ","ABD = ","ABDE = ","ABC = ","ABCE = ","ABCD = ","ABCDE = ")
        combinationLogicalA <- c("FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE", "FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE", "TRUE","TRUE","TRUE","TRUE" ,"TRUE","TRUE","TRUE","TRUE" , "TRUE","TRUE","TRUE","TRUE" ,"TRUE","TRUE","TRUE","TRUE")
        combinationLogicalB <- c("FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE", "TRUE","TRUE","TRUE","TRUE" ,"TRUE","TRUE","TRUE","TRUE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE", "TRUE","TRUE","TRUE","TRUE" ,"TRUE","TRUE","TRUE","TRUE")
        combinationLogicalC <- c("FALSE","FALSE","FALSE","FALSE","TRUE","TRUE","TRUE","TRUE","FALSE","FALSE","FALSE","FALSE","TRUE","TRUE","TRUE","TRUE","FALSE","FALSE","FALSE","FALSE","TRUE","TRUE","TRUE","TRUE","FALSE","FALSE","FALSE","FALSE","TRUE","TRUE","TRUE","TRUE")
        combinationLogicalD <- c("FALSE","FALSE","TRUE","TRUE","FALSE","FALSE","TRUE","TRUE","FALSE","FALSE","TRUE","TRUE","FALSE","FALSE","TRUE","TRUE","FALSE","FALSE","TRUE","TRUE","FALSE","FALSE","TRUE","TRUE","FALSE","FALSE","TRUE","TRUE","FALSE","FALSE","TRUE","TRUE")
        combinationLogicalE <- c("FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE","FALSE","TRUE")

        for (i in 1:32) {
          choices.vector[i] = paste(combination[i] , dataset.venn.count.list[[5]][i,6])

        }
        output$ts.selector.overlapping.value <- renderUI({
          selectInput("input_ts_selector_overlapping_value",
                      label = paste("Select CpGs contents based on Venn Diagram numbers."),
                      choices = choices.vector , selected = -1 , width = validateCssUnit("100%"))
        })

        output$ts.table.overlapping.value3 <- renderText({
          dataset.venn.count.list[[5]]
        })

        # updating the table and showing content

        # output the datatable according to the venn diagram count number selected
        output$output.topscorer.overlappingComparison <- renderDataTable({

          # Create a Progress object
          progress <- shiny::Progress$new()

          progress$set(message = "Reading data! please wait...", value = 50)


          selected_value = input$input_ts_selector_overlapping_value


          for (i in 1:32) {
            if(identical(as.character(selected_value) , as.character(paste(combination[i] , dataset.venn.count.list[[5]][i,6]))))
            {
              c1 = original.dataset.list[[1]] [(original.dataset.list[[1]]$logical == as.character(combinationLogicalA[i]) ) ]
              c2 = original.dataset.list[[2]] [(original.dataset.list[[2]]$logical == as.character(combinationLogicalB[i]) ) ]
              c3 = original.dataset.list[[3]] [(original.dataset.list[[3]]$logical == as.character(combinationLogicalC[i]) ) ]
              c4 = original.dataset.list[[4]] [(original.dataset.list[[4]]$logical == as.character(combinationLogicalD[i]) ) ]
              c5 = original.dataset.list[[5]] [(original.dataset.list[[5]]$logical == as.character(combinationLogicalE[i]) ) ]

              c1$logical      <- NULL
              c2$logical      <- NULL
              c3$logical      <- NULL
              c4$logical      <- NULL
              c5$logical      <- NULL

              #outputTableData <- Reduce(function(x, y) merge(x, y, by = c("cgid","Chromosome","Start","Strand"), all=F), list(c1, c2))
              c12 <- merge(c1, c2, by = c("cgid","Chromosome","Start","Strand") , all = F)
              c34 <- merge(c3, c4, by = c("cgid","Chromosome","Start","Strand") , all = F)
              c1234 <- merge(c12, c34, by = c("cgid","Chromosome","Start","Strand") , all = F)
              c12345 <- merge(c1234, c5, by = c("cgid","Chromosome","Start","Strand") , all = F)

              outputTableData <- c12345

              break
            }
          }




          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())

          outputTableData


        },selection = 'single', filter = 'top',
        extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL),
        options = list( scrollX = TRUE, scrollY = TRUE, dom = 'Blfrtip',buttons = list( 'copy', 'print',list( extend = 'collection',buttons = c('csv', 'excel', 'pdf'), text = 'Download'), I('colvis')),br(), keys = TRUE
        ), escape = TRUE)

      }

      else if(length(cb.checked) == 6){



        p <- vennDiagram(dataset.venn.count.list[[6]] , include = "both",
                         names = c(cb.checked[1], cb.checked[2], cb.checked[3], cb.checked[4], cb.checked[5], cb.checked[6]),
                         cex = 1, counts.col = "red", circle.col = c("green","blue", "orange","yellow"))


        output$output.ts.table.multivenn.plot.labels <- renderTable({
          Selected_analysis= c(paste("A = ",cb.checked[1]), paste("B = ",cb.checked[2]), paste("C = ",cb.checked[3]), paste("D = ",cb.checked[4]), paste("E = ",cb.checked[5]), paste("F = ",cb.checked[6]))
          result <- data.frame(Selected_analysis)
          data.table(result)
        })

        output$ts.selector.overlapping.value <- renderUI({
          selectInput("input_ts_selector_overlapping_value",
                      label = paste("Sorry can not display table"),
                      choices = c("") , selected = -1 , width = validateCssUnit("100%"))
        })

      }

      else{
        output$ts.venn.overlapping.error.value <- renderText({
          paste("Please select atleast 6 analysis to draw venn diagram!")
        })

      }

      p

    })

  })



  #end of top scorer tab
  ####################################################################################


  output$runR <- renderText({
      paste("Please wait! RnBeads library is loading")
      library(RnBeads)
      x <- matrix(1:10, ncol = 5)
#       write(x, file = paste(getwd(),"data.txt",sep ="/"),
#             ncolumns = if(is.character(x)) 1 else 5,
#             append = TRUE, sep = " ")

      #mydata = read.table(paste(getwd(),"data.txt",sep ="/"))
      paste(rnb.getOption("analyze.sites"))
    })

})

.LOG.INFO <- new.env()

.LOG.INDENT <- "    "
.LOGGER <- "RnBeadsInterface"
.LOG.STAT <- c("INFO" = "INFO", "STATUS" = "STATUS", "WARNING" = "WARNING", "ERROR" = "ERROR")

logger.transform <- function(txt, indent = TRUE) {
  if (!(is.character(txt) && is.vector(txt))) {
    stop("invalid value for parameter txt")
  }
  if (length(txt) > 1) {
    txt <- paste(txt, collapse = " ")
  }
  if (indent && length(.LOG.INFO[["titles"]]) != 0) {
    txt <- paste(paste(rep(.LOG.INDENT, length(.LOG.INFO[["titles"]])), collapse = ""), txt, sep = "")
  }
  return(txt)
}

logger.paste <- function(status.word, txt) {
  record <- list(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S ")
  )
  txt <- paste(record, "\n", sep = "")
  cat(txt, append = TRUE)

}


# http://stats.stackexchange.com/questions/110755/how-calculate-inflation-observed-and-expected-p-values-from-uniform-distribution
# refrerence : http://genome.sph.umich.edu/wiki/Code_Sample:_Generating_QQ_Plots_in_R

#Quantile-quantile plots (qq-plots) can be useful for verifying that a set of values
#come from a certain distribution. For example in a genome-wide association study,
#we expect that most of the SNPs we are testing not to be associated with the disease.
#Under the null, this means that the p-values we get from tests where no true
#association exists should follow a uniform(0,1) distribution. Since we're usually most
#interested in really small p-values, we generally transform the p-values by -log10 so
#that the smallest values near zero become the larger values and are thus easier to see.

qqunif.plot<-function(pvalues,
                      should.thin=T, thin.obs.places=2, thin.exp.places=2,
                      xlab=expression(paste("Expected (",-log[10], " p-value)")),
                      ylab=expression(paste("Observed (",-log[10], " p-value)")),
                      draw.conf=TRUE, conf.points=1000, conf.col="lightgray", conf.alpha=.05,
                      already.transformed=FALSE, pch=20, aspect="fill", prepanel=prepanel.qqunif,
                      par.settings=list(superpose.symbol=list(pch=pch)), ...) {


  #error checking
  if (length(pvalues)==0) stop("pvalue vector is empty, can't draw plot")
  if(!(class(pvalues)=="numeric" ||
       (class(pvalues)=="list" && all(sapply(pvalues, class)=="numeric"))))
    stop("pvalue vector is not numeric, can't draw plot")
  if (any(is.na(unlist(pvalues)))) stop("pvalue vector contains NA values, can't draw plot")
  if (already.transformed==FALSE) {
    if (any(unlist(pvalues)==0)) stop("pvalue vector contains zeros, can't draw plot")
  } else {
    if (any(unlist(pvalues)<0)) stop("-log10 pvalue vector contains negative values, can't draw plot")
  }


  grp<-NULL
  n<-1
  exp.x<-c()
  if(is.list(pvalues)) {
    nn<-sapply(pvalues, length)
    rs<-cumsum(nn)
    re<-rs-nn+1
    n<-min(nn)
    if (!is.null(names(pvalues))) {
      grp=factor(rep(names(pvalues), nn), levels=names(pvalues))
      names(pvalues)<-NULL
    } else {
      grp=factor(rep(1:length(pvalues), nn))
    }
    pvo<-pvalues
    pvalues<-numeric(sum(nn))
    exp.x<-numeric(sum(nn))
    for(i in 1:length(pvo)) {
      if (!already.transformed) {
        pvalues[rs[i]:re[i]] <- -log10(pvo[[i]])
        exp.x[rs[i]:re[i]] <- -log10((rank(pvo[[i]], ties.method="first")-.5)/nn[i])
      } else {
        pvalues[rs[i]:re[i]] <- pvo[[i]]
        exp.x[rs[i]:re[i]] <- -log10((nn[i]+1-rank(pvo[[i]], ties.method="first")-.5)/(nn[i]+1))
      }
    }
  } else {
    n <- length(pvalues)+1
    if (!already.transformed) {
      exp.x <- -log10((rank(pvalues, ties.method="first")-.5)/n)
      pvalues <- -log10(pvalues)
    } else {
      exp.x <- -log10((n-rank(pvalues, ties.method="first")-.5)/n)
    }
  }


  #this is a helper function to draw the confidence interval
  panel.qqconf<-function(n, conf.points=1000, conf.col="gray", conf.alpha=.05, ...) {
    require(grid)
    conf.points = min(conf.points, n-1);
    mpts<-matrix(nrow=conf.points*2, ncol=2)
    for(i in seq(from=1, to=conf.points)) {
      mpts[i,1]<- -log10((i-.5)/n)
      mpts[i,2]<- -log10(qbeta(1-conf.alpha/2, i, n-i))
      mpts[conf.points*2+1-i,1]<- -log10((i-.5)/n)
      mpts[conf.points*2+1-i,2]<- -log10(qbeta(conf.alpha/2, i, n-i))
    }
    grid.polygon(x=mpts[,1],y=mpts[,2], gp=gpar(fill=conf.col, lty=0), default.units="native")
  }

  #reduce number of points to plot
  if (should.thin==T) {
    if (!is.null(grp)) {
      thin <- unique(data.frame(pvalues = round(pvalues, thin.obs.places),
                                exp.x = round(exp.x, thin.exp.places),
                                grp=grp))
      grp = thin$grp
    } else {
      thin <- unique(data.frame(pvalues = round(pvalues, thin.obs.places),
                                exp.x = round(exp.x, thin.exp.places)))
    }
    pvalues <- thin$pvalues
    exp.x <- thin$exp.x
  }
  gc()

  prepanel.qqunif= function(x,y,...) {
    A = list()
    A$xlim = range(x, y)*1.02
    A$xlim[1]=0
    A$ylim = A$xlim
    return(A)
  }

  #draw the plot
  xyplot(pvalues~exp.x, groups=grp, xlab=xlab, ylab=ylab, aspect=aspect,
         prepanel=prepanel, scales=list(axs="i"), pch=pch,
         panel = function(x, y, ...) {
           if (draw.conf) {
             panel.qqconf(n, conf.points=conf.points,
                          conf.col=conf.col, conf.alpha=conf.alpha)
           };
           panel.xyplot(x,y, ...);
           panel.abline(0,1);
         }, par.settings=par.settings, ...
  )
}

