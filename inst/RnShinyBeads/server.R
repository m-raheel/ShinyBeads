
########################################################################################################################
## server.R
## created: 2016-09-01
## creator: Muhammad Raheel
## ---------------------------------------------------------------------------------------------------------------------
## Main workflow of the RnShinyBeads tool.
########################################################################################################################


# libraries
######################################################################
library(DT)
library(shiny)
library(XML)
library(compare)
library(data.table) # using the function fread for reading large csv files
library(qqman)
library(tcltk)# OS independent file dir selection
library(lattice)# using qqunif.plot
library(plotly) #interactive graphics with D3
library(RnShinyBeads)
library(VennDiagram)
library(plyr)
library(shinydashboard)
library(limma)

#####################################################################


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

  observeEvent(input$dataset, {
    newtab <- switch(input$tabs,
                     "home" = "dataset",
    )
    updateTabItems(session, "tabs", newtab)
  })


  observe({
    if(input$view_datasets > 0){

      session$sendCustomMessage("myCallbackHandler", "4")
    }
  })

  #updatedDir <- normalizePath("/projects/factorization/raw_data/Demo_Repository", winslash = "\\", mustWork = NA)


#   volumes <- getVolumes() #c('R Installation'=R.home())
#
#   shinyDirChoose(input, 'folder', roots=volumes)
#
#   output$directorypath <- renderPrint({parseDirPath(roots=volumes, input$folder)})


  # .global.analysisDir is the path to the RnBeads repository selected before running shiny app
  updatedDir <- normalizePath(.global.analysisDir, winslash = "\\", mustWork = NA)

  #updatedDir <- normalizePath("/var/www/html/data", winslash = "\\", mustWork = NA)

  selectedDir <-  as.character(updatedDir)

  analysis <- rnbi.total.analysis(selectedDir)

  # updating all the selectInput dropdowns of the app

  updateSelectInput(session, "input_type",
                    label = paste("Select analysis folder"),
                    choices = analysis)

  updateSelectInput(session, "select_ia",
                    label = paste("Select analysis folder"),
                    choices = analysis)



  updateSelectInput(session, "input_dmcomp_choices",
                    label = paste("Select analysis folder"),
                    choices = analysis)

  updateSelectInput(session, "input_dmcomp_choices_1",
                    label = paste("Analysis 1"),
                    choices = analysis)

  updateSelectInput(session, "input_dmcomp_choices_2",
                    label = paste("Analysis 2"),
                    choices = analysis)

  updateSelectInput(session, "input_tablebrowser_choices",
                    label = paste("Select analysis folder"),
                    choices = analysis)

  updateSelectInput(session, "input_topscorer_choices_1",
                    label = paste("Analysis 1"),
                    choices = analysis)

  updateSelectInput(session, "input_topscorer_choices_2",
                    label = paste("Analysis 2"),
                    choices = analysis)


  # Can also set the label and select items
  updateCheckboxGroupInput(session, "cb_ts_comp_venn",
                           label = paste("Select analysis"),
                           choices = analysis,
                           selected = 1
  )



  dirfolder = analysis

  if ( file.exists( isolate({ paste(selectedDir,dirfolder[1],'index.html',sep="/") }) ) ){
    output$ErrorText1 <- renderText({ paste("You are working with the following RnBeads analysis repository:",sep="") })
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

    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Reading analysis, please wait..", value = 50)

    choices <- rnbi.total.analysis(results.dir())

    output$count_rfolders <- renderText({
      paste("Total RnBeads reports in this repository = ", length(choices), sep = " ")

    })

    if ( length(choices) != 0 ){

      output$list_folders <- renderDataTable({
        choices <- unlist(choices)
        DT <- data.table( RnBeads_Reports = choices)

        DT

      },selection = 'single', escape = FALSE)

    }

    else{

      output$list_folders <- renderDataTable({

        DT <- data.table( RnBeads_Analysis = 'No directory in this repository.')
        return(DT)

      },selection = 'single', escape = FALSE)

    }

    #closing the progress bar
    on.exit(progress$close())

  })

  ############################################################################################

  # check and return the results folder that have the same sample annotation file.
  ############################################################################################

  observe({

    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Collecting information, please wait..", value = 50)


    cd_list <- list()
    cd_list_counter <- 1

    commond_list <- list()
    commond_list_counter <- 1

#     common.datasets = rnbi.total.dataset(results.dir())
#     common.datasets.path = common.datasets$path_list
#     common.datasets.analysis = common.datasets$analysis_list
#
#     total.common.datasets = rnbi.common.dataset(results.dir())


    analysis <- rnbi.total.analysis(results.dir())

    common.datasets = rnbi.dataset(results.dir())
    datasets.unique.analysis.index = common.datasets$analysis_list
    datasets.common.analysis.index = common.datasets$common_list

    if (length(datasets.unique.analysis.index) != 0 || length(datasets.common.analysis.index) != 0 ){


      # unique analysis
      cd_list <- lapply(1:(length(datasets.unique.analysis.index)), function(i) {
        cd_list[cd_list_counter] <- paste("Dataset",analysis[datasets.unique.analysis.index[i]],datasets.unique.analysis.index[i],sep = "_")
        cd_list_counter = cd_list_counter + 1


        cd_list

      })


      # common analysis
      commond_list <- lapply(1:(length(datasets.common.analysis.index)), function(i) {
        list_index <- unlist(datasets.common.analysis.index[i])
        commond_list[commond_list_counter] <- paste("Dataset",analysis[list_index[1]],list_index[1],sep = "_")
        commond_list_counter = commond_list_counter + 1


        commond_list

      })

      # combining unique and common analysis into one

      total_dataset <-  c(cd_list, commond_list)

      dataset_choices <- unlist(total_dataset)
      # update the datalist dropdown in the individual data sets tab
      updateSelectInput(session, "dd_ids_datasets",
                        label = "Datasets",
                        choices = dataset_choices)

      output$total_datasets <- renderText({
        paste("Total datasets used in this repository =", length(cd_list) + length(commond_list), sep = " ")

      })


      output$list_datasets <- renderDataTable({
        cd_list <- unlist(cd_list)
        DT <- data.table( Datasets_Used = cd_list)

        DT

      },selection = 'single', escape = FALSE)


      output$common_datasets_used <- renderDataTable({
        commond_list <- unlist(commond_list)
        DT <- data.table( Datasets_Used = commond_list)

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

    #closing the progress bar
    on.exit(progress$close())

  })


  ############################################################################################

  # if any of the datasetslist rows is clicked then it will redirects to individual dataset
  #tab to display the annotaion.csv file contents
  ############################################################################################


  observeEvent(input$list_datasets_rows_selected, {
    row <- input$list_datasets_rows_selected

    row <- as.integer(row)


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

      },selection = 'single',
      extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL),

      options = list(autoWidth = FALSE,columnDefs = list(list(className = 'dt-center', targets = 2)),
                     pageLength = 100,
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#368BC1', 'color': '#000'});",
                       "}"),
                     #lengthMenu = c(5, 10, 15, 20)
                     scrollX = TRUE, scrollY = TRUE, dom = 'Blfrtip',buttons = list( 'copy', 'print',list( extend = 'collection',buttons = c('csv', 'excel', 'pdf'), text = 'Download'), I('colvis')),br(), keys = TRUE
      ), escape = TRUE)
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

    if (as.character(input$select_ia) == 'NA')
    {
      output$list_module <- renderTable({
        DT <- data.table(Performed_Modules = 'No file exist or no data available.')
        DT

      })
    }
    else{
      wd_modules <- reactive({file.path(results.dir(), value.modules()) })

      data_type = rnbi.analysis.datatype(results.dir() , value.modules())

      if (grepl('idat files' , data_type )) # true if idat files is the data type of the analysis in the string data_type
      {
        rrbs_analysis = FALSE
      }
      else{
        rrbs_analysis = TRUE
      }

      if (rrbs_analysis == TRUE){

        Performed_Modules <- rnbi.analysis.rrbs.modules.performed(wd_modules())


        if ( length(Performed_Modules) > 0 ){
          #fucntion from the RnShinyBeads package

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
      }
      else{

        if ( file.exists( isolate({ paste(wd_modules(),'analysis.log',sep="/") }) ) ){
          #fucntion from the RnShinyBeads package


          #Performed_Modules <-  modules_performed(wd_modules())
          Performed_Modules <- rnbi.analysis.modules.performed(wd_modules())

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
      }
    }# end of else for checking NA
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
          #HTML(paste('<a class = "btn btn-primary" target = "_blank" href = "http://internal.genetik.uni-sb.de/dataT7600/',paste(as.character(input$select_ia),'index.html"',sep="/"),'>View Reports','</a>',sep=""))
          HTML(paste('<a class = "btn btn-primary" target = "_blank" href = "file:///',paste(as.character(results.dir()),as.character(input$select_ia),'index.html"',sep="/"),'>View Reports','</a>',sep=""))


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
    tmp = unlist(strsplit(tmp, "_"))
    len = length(tmp)
    #last_character = substr(tmp,len,len)
    last_character = tmp[len]



#     # Create a Progress object
#     progress <- shiny::Progress$new()
#     progress$set(message = "please wait..", value = 50)


    analysis <- rnbi.total.analysis(results.dir())

    if (dd_datasets != "NA"){

      last_character = as.integer(last_character)

      path_ = paste(results.dir(), analysis[last_character], 'data_import_data','annotation.csv',sep="/")

      # if no datasets returned means that we have only one analysis so in else showing it
      if (length(path_) != 0){
        a.file <- reactive({read.csv(as.character(path_))})


        # Generate a summary of the dataset
        output[[paste0('annotation')]] <- renderDataTable({

          dataset <- a.file()
          dataset

        },selection = 'single',

        extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL),
        options = list(columnDefs = list(list(className = 'dt-center', targets = 2)),
                       pageLength = 100,
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#368BC1', 'color': '#000'});",
                         "}"),
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


      }
      else{

        if ( file.exists( isolate({ paste(results.dir(),input$select_ia,'data_import_data','annotation.csv',sep="/") }) ) )
        {
          a.file <- reactive({read.csv(normalizePath(paste(results.dir(),input$select_ia,'data_import_data','annotation.csv',sep="/"), winslash = "\\", mustWork = NA))})

          # Generate a summary of the dataset
          output[[paste0('annotation')]] <- renderDataTable({

            dataset <- a.file()
            dataset

          },selection = 'single',

          extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL),
          options = list(
                         columnDefs = list(list(className = 'dt-center', targets = c(1,2))),
                         #pageLength = 100,
                         initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#368BC1', 'color': '#000'});",
                           "}"),
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


      common.datasets = rnbi.dataset(results.dir())
      datasets.unique.analysis.index = common.datasets$analysis_list
      datasets.common.analysis.index = common.datasets$common_list

      datasets.total.common.analysis.index = common.datasets$total_common_index


      # if dataset is used in more than one analysis or not
      if (last_character %in% datasets.total.common.analysis.index ){

        common <- c()
        common_index <- 1

        # iterate over the common dataset analysis
        for (i in 1:length(datasets.common.analysis.index)) {

          list_index <- unlist(datasets.common.analysis.index[i])

          if(last_character %in% list_index)
          {

            # store the different analysis sharing common dataset
            for (j in 1:length(list_index)){
              common <- c(common, analysis[list_index[j]] )
            }
            common_index <- i
            break

          }

        }

        output[[paste0('annotation1')]] <- renderDataTable({
          common <- unlist(common)
          DT <- data.table( Analysis_Dir = common)

          DT


        },selection = 'single', escape = TRUE)

      }

      else{

        # if no dataset is shared among other analysis

        output[[paste0('annotation1')]] <- renderDataTable({

          DT <- data.table( Analysis_Dir = analysis[last_character])

          DT


        },selection = 'single', escape = TRUE)

      }

      ###################################################################################
    }

#     #closing the progress bar
#     on.exit(progress$close())

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

  output$qq.columns <- renderUI({
    selectInput("input_qq_columns",
                label = paste(""),
                choices = c("diffmeth.p.val"))
  })

  output$qq.columns.equality <- renderUI({
    selectInput("input_qq_columns_equality",
                label = paste(""),
                choices = c(">","<", ">=","<=","all"))
  })

  output$qq.columns.range <- renderUI({
    selectInput("input_qq_columns_range",
                label = paste(""),
                choices = c("0.01","0.1", "0.05","0.5","0","1"))
  })


  output$qq.multi.columns <- renderUI({
    selectInput("input_qq_multi_columns",
                label = paste(""),
                choices = c("diffmeth.p.val"))
  })

  output$qq.multi.columns.equality <- renderUI({
    selectInput("input_qq_multi_columns_equality",
                label = paste(""),
                choices = c(">","<", ">=","<=","all"))
  })

  output$qq.multi.columns.range <- renderUI({
    selectInput("input_qq_multi_columns_range",
                label = paste(""),
                choices = c("0.01","0.1", "0.05","0.5","0","1"))
  })


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


      data_type = rnbi.analysis.datatype(results.dir() , qq.value)

      if (grepl('idat files' , data_type )) # true if idat files is the data type of the analysis in the string data_type
      {
        rrbs_analysis = FALSE
      }
      else{
        rrbs_analysis = TRUE
      }

      if (rrbs_analysis == TRUE)
      {

        f = paste("diffMethTable_site_cmp",index_list(), ".csv.gz",sep = '')
      }
      else{
        f = paste("diffMethTable_site_cmp",index_list(), ".csv",sep = '')
      }

      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) )
      {

        # Create a Progress object
        progress <- shiny::Progress$new()

        progress$set(message = "Making QQ Plot", value = 50)

        filename <- file.path(qq.dir, 'differential_methylation_data',f)


        filename= as.character(filename)

        nrows.value <- as.integer(input$input_qqplot_readtop)

        column_selected = as.character(input$input_qq_columns)
        equality = as.character(input$input_qq_columns_equality)
        range_selected = as.numeric(input$input_qq_columns_range)


        if (rrbs_analysis == TRUE)
        {
          # fread function from the library data.table
          comp.file <- fread(input = paste('zcat < ',filename,sep = ''),sep = ",", select = c("Chromosome","diffmeth.p.val"))

          if(column_selected == "diffmeth.p.val"){
            colselected <- comp.file$diffmeth.p.val
          }
          else{
            colselected <- comp.file$diffmeth.p.val
          }

          # filtering the dataframe

          if(equality == ">="){
            original.dataset <- comp.file
            filtered.dataset <- comp.file[colselected > range_selected,]

          }
          else if(equality == ">"){
            original.dataset <- comp.file
            filtered.dataset <- comp.file[colselected > range_selected,]

          }
          else if(equality == "<="){

            original.dataset <- comp.file
            filtered.dataset <- comp.file[colselected <= range_selected,]

          }
          else if(equality == "<"){
            original.dataset <- comp.file
            filtered.dataset <- comp.file[colselected < range_selected,]

          }

          else {
            original.dataset <- comp.file
            filtered.dataset <- comp.file

          }

          comp.file <- filtered.dataset
        }
        else{
          # fread function from the library data.table
          comp.file <- fread(filename,sep = ",", select = c("cgid","Chromosome","diffmeth.p.val"))

          if(column_selected == "diffmeth.p.val"){
            colselected <- comp.file$diffmeth.p.val
          }
          else{
            colselected <- comp.file$diffmeth.p.val
          }

          # filtering the dataframe

          if(equality == ">="){
            original.dataset <- comp.file
            filtered.dataset <- comp.file[colselected >= range_selected,]

          }
          else if(equality == ">"){
            original.dataset <- comp.file
            filtered.dataset <- comp.file[colselected > range_selected,]

          }
          else if(equality == "<="){

            original.dataset <- comp.file
            filtered.dataset <- comp.file[colselected <= range_selected,]

          }
          else if(equality == "<"){
            original.dataset <- comp.file
            filtered.dataset <- comp.file[colselected < range_selected,]

          }

          else {
            original.dataset <- comp.file
            filtered.dataset <- comp.file

          }

          comp.file <- filtered.dataset
        }


        if (length(comp.file$diffmeth.p.val) < nrows.value){
          nrows.value = length(comp.file$diffmeth.p.val)
        }




        comp.file <- data.frame(comp.file[1:nrows.value,])

        if (rrbs_analysis == TRUE)
        {

          #remove NA columns rows

          completeFun <- function(data, desiredCols) {
            completeVec <- complete.cases(data[, desiredCols])
            return(data[completeVec, ])
          }

          filtered.comp.file <- completeFun(comp.file, "diffmeth.p.val")

          q <- rnbi.qqplot.single.rrbs(filtered.comp.file)
        }
        else{
          q <- rnbi.qqplot.single(comp.file)
        }


        p <- plotly::ggplotly(q)


        output$info.qqplot <- renderUI({

          HTML(paste("'<p>QQplot is generated from the diffmeth.p.values of the comparison selected above.</p> <br/ > <p><b>diffmeth.p.val:</b> p-value obtained from a two-sided Welch t-test or alternatively from linear models employed in the limma package (which type of p-value is computed is specified in the differential.site.test.method option). In case of paired analysis, the paired Student's t-test is applied.",'</p>',sep=""))

        })


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
                 xaxis = list(title = 'Expected -log10(p) (uniform distribution)',
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

# tetsing code####
output$testingcompqqplotly <- renderPlotly({
  pdf(NULL)




  f = paste("diffMethTable_site_cmp",1, ".csv",sep = '')
  filename <- file.path("/var","www","html","data","20150212_NeuronPD_SBvsBN_reports", 'differential_methylation_data',f)


  filename= as.character(filename)

  nrows.value <- as.integer(100)

  # fread function from the library data.table
  comp.file <- fread(filename,sep = ",", select = c("cgid","Chromosome","diffmeth.p.val","diffmeth.p.adj.fdr"), nrows = nrows.value)


  f = paste("diffMethTable_site_cmp",2, ".csv",sep = '')
  filename <- file.path("/var","www","html","data","20150212_NeuronPD_SBvsBN_WithBlacklist", 'differential_methylation_data',f)


  filename= as.character(filename)

  nrows.value <- as.integer(100)

  # fread function from the library data.table
  comp.file2 <- fread(filename,sep = ",", select = c("cgid","Chromosome","diffmeth.p.val","diffmeth.p.adj.fdr"), nrows = nrows.value)


  p <- rnbi.qqplot.single (comp.file)

  p <- plotly::ggplotly(p)

  dev.off()



  p
})

output$testingcompqqplot <- renderPlot({
  #FAKE SAMPLE DATA
  my.pvalues<-runif(10000)


  p <-  qqmath(~-log10(my.pvalues),
               distribution=function(x){-log10(qunif(1-x))}
  )


  p
})



############
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
      #fucntion from the RnShinyBeads package

      data_type = rnbi.analysis.datatype(results.dir() , qq.value)

      if (grepl('idat files' , data_type )) # true if idat files is the data type of the analysis in the string data_type
      {
        rrbs_analysis = FALSE
      }
      else{
        rrbs_analysis = TRUE
      }

      if (rrbs_analysis == TRUE)
      {

        f = paste("diffMethTable_site_cmp",index_list_1(), ".csv.gz",sep = '')
      }
      else{
        f = paste("diffMethTable_site_cmp",index_list_1(), ".csv",sep = '')
      }


      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){


        filename <- file.path(qq.dir, 'differential_methylation_data',f)
        filename= as.character(filename)
        nrows.value <- as.integer(input$input_multiqqplot_readtop)



        column_selected = as.character(input$input_qq_multi_columns)
        equality = as.character(input$input_qq_multi_columns_equality)
        range_selected = as.numeric(input$input_qq_multi_columns_range)

        if (rrbs_analysis == TRUE)
        {
          # fread function from the library data.table
          comp.file <- fread(input = paste('zcat < ',filename,sep = ''),sep = ",", select = c("Chromosome","diffmeth.p.val"))
          original.dataset <- comp.file

          if(column_selected == "diffmeth.p.val"){
            colselected <- comp.file$diffmeth.p.val
          }
          else{
            colselected <- comp.file$diffmeth.p.val
          }

          # filtering the dataframe

          if(equality == ">="){

            filtered.dataset <- comp.file[colselected >= range_selected,]

          }
          else if(equality == ">"){

            filtered.dataset <- comp.file[colselected > range_selected,]

          }
          else if(equality == "<="){


            filtered.dataset <- comp.file[colselected <= range_selected,]

          }
          else if(equality == "<"){

            filtered.dataset <- comp.file[colselected < range_selected,]

          }

          else {

            filtered.dataset <- comp.file

          }

          comp.file <- filtered.dataset
        }
        else{
          # fread function from the library data.table
          comp.file <- fread(filename,sep = ",", select = c("cgid","Chromosome","diffmeth.p.val"))
          original.dataset <- comp.file

          if(column_selected == "diffmeth.p.val"){
            colselected <- comp.file$diffmeth.p.val
          }
          else{
            colselected <- comp.file$diffmeth.p.val
          }

          # filtering the dataframe

          if(equality == ">="){

            filtered.dataset <- comp.file[colselected >= range_selected,]

          }
          else if(equality == ">"){

            filtered.dataset <- comp.file[colselected > range_selected,]

          }
          else if(equality == "<="){


            filtered.dataset <- comp.file[colselected <= range_selected,]

          }
          else if(equality == "<"){

            filtered.dataset <- comp.file[colselected < range_selected,]

          }

          else {

            filtered.dataset <- comp.file

          }

          comp.file <- filtered.dataset
        }


        if (length(comp.file$diffmeth.p.val) < nrows.value){
          nrows.value = length(comp.file$diffmeth.p.val)
        }


        comp.file <- data.frame(comp.file[1:nrows.value,])

        if (rrbs_analysis == TRUE)
        {

          #remove NA columns rows

          completeFun <- function(data, desiredCols) {
            completeVec <- complete.cases(data[, desiredCols])
            return(data[completeVec, ])
          }

          filtered.comp.file <- completeFun(comp.file, "diffmeth.p.val")

          x <- filtered.comp.file
        }
        else{

          x <- comp.file
        }

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
      #fucntion from the RnShinyBeads package
      data_type = rnbi.analysis.datatype(results.dir() , qq.value)

      if (grepl('idat files' , data_type )) # true if idat files is the data type of the analysis in the string data_type
      {
        rrbs_analysis = FALSE
      }
      else{
        rrbs_analysis = TRUE
      }

      if (rrbs_analysis == TRUE)
      {

        f = paste("diffMethTable_site_cmp",index_list_2(), ".csv.gz",sep = '')
      }
      else{
        f = paste("diffMethTable_site_cmp",index_list_2(), ".csv",sep = '')
      }


      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){


        filename <- file.path(qq.dir, 'differential_methylation_data',f)
        filename= as.character(filename)
        nrows.value <- as.integer(input$input_multiqqplot_readtop)


        column_selected = as.character(input$input_qq_multi_columns)
        equality = as.character(input$input_qq_multi_columns_equality)
        range_selected = as.numeric(input$input_qq_multi_columns_range)


        if (rrbs_analysis == TRUE)
        {
          # fread function from the library data.table
          comp.file <- fread(input = paste('zcat < ',filename,sep = ''),sep = ",", select = c("Chromosome","diffmeth.p.val"))
          original.dataset <- comp.file

          if(column_selected == "diffmeth.p.val"){
            colselected <- comp.file$diffmeth.p.val
          }
          else{
            colselected <- comp.file$diffmeth.p.val
          }

          # filtering the dataframe

          if(equality == ">="){

            filtered.dataset <- comp.file[colselected >= range_selected,]

          }
          else if(equality == ">"){

            filtered.dataset <- comp.file[colselected > range_selected,]

          }
          else if(equality == "<="){


            filtered.dataset <- comp.file[colselected <= range_selected,]

          }
          else if(equality == "<"){

            filtered.dataset <- comp.file[colselected < range_selected,]

          }

          else {

            filtered.dataset <- comp.file

          }

          comp.file <- filtered.dataset
        }
        else{
          # fread function from the library data.table
          comp.file <- fread(filename,sep = ",", select = c("cgid","Chromosome","diffmeth.p.val"))
          original.dataset <- comp.file

          if(column_selected == "diffmeth.p.val"){
            colselected <- comp.file$diffmeth.p.val
          }
          else{
            colselected <- comp.file$diffmeth.p.val
          }

          # filtering the dataframe

          if(equality == ">="){

            filtered.dataset <- comp.file[colselected >= range_selected,]

          }
          else if(equality == ">"){

            filtered.dataset <- comp.file[colselected > range_selected,]

          }
          else if(equality == "<="){


            filtered.dataset <- comp.file[colselected <= range_selected,]

          }
          else if(equality == "<"){

            filtered.dataset <- comp.file[colselected < range_selected,]

          }

          else {

            filtered.dataset <- comp.file

          }

          comp.file <- filtered.dataset
        }


        if (length(comp.file$diffmeth.p.val) < nrows.value){
          nrows.value = length(comp.file$diffmeth.p.val)
        }


        comp.file <- data.frame(comp.file[1:nrows.value,])

        if (rrbs_analysis == TRUE)
        {

          #remove NA columns rows

          completeFun <- function(data, desiredCols) {
            completeVec <- complete.cases(data[, desiredCols])
            return(data[completeVec, ])
          }

          filtered.comp.file <- completeFun(comp.file, "diffmeth.p.val")


          y <- filtered.comp.file
        }
        else{


          y <- comp.file
        }

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
                   xaxis = list(title = 'Expected -log10(p) (uniform distribution)',
                                zeroline = TRUE,
                                range = c(0, 1)),
                   yaxis = list(title = 'Observed -log10(p)',
                                range = c(0,1)))


          dev.off()

          q


        }

        else if(length(list.pvalues_2()) == 0) {

          # print error/ warning message


          #           qqplot(1,1,main="Normal Q-Q Plot", ylab="diffmeth.p.val")
          #           text(1,1,"No data available or no comparison file exist from repository 1")

          Primates <- c('No Data Avaiable for analysis 2')
          Bodywt <- c(0.5 )
          Brainwt <- c(0.5)

          data <- data.frame(Primates, Bodywt, Brainwt)

          pdf(NULL)
          q <- plot_ly(data,x = ~Bodywt, y = ~Brainwt, type = 'scatter',
                       mode = 'text', text = ~Primates, textposition = 'middle center',
                       textfont = list(color = '#000000', size = 16))%>%
            layout(title = 'Q-Q Plot',
                   xaxis = list(title = 'Expected -log10(p) (uniform distribution)',
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

          progress$set(message = "Making selected analysis QQ Plot", value = 50)


          pdf(NULL)

          x <- list.pvalues_1()
          y <- list.pvalues_2()

          a1.value <- as.character(input$input_dmcomp_choices_1)
          a2.value <- as.character(input$input_dmcomp_choices_2)

          #fucntion from the RnShinyBeads package
          data_type1 = rnbi.analysis.datatype(results.dir() , a1.value)

          if (grepl('idat files' , data_type1 )) # true if idat files is the data type of the analysis in the string data_type
          {
            rrbs_analysis1 = FALSE
          }
          else{
            rrbs_analysis1 = TRUE
          }

          data_type2 = rnbi.analysis.datatype(results.dir() , a2.value)

          if (grepl('idat files' , data_type2 )) # true if idat files is the data type of the analysis in the string data_type
          {
            rrbs_analysis2 = FALSE
          }
          else{
            rrbs_analysis2 = TRUE
          }


          if (rrbs_analysis1 == TRUE && rrbs_analysis2 == TRUE)
          {

            if(length(list.pvalues_1()) != length(list.pvalues_2())){
              Primates <- c('Could not draw qqplot! Please try again')
              Bodywt <- c(0.5 )
              Brainwt <- c(0.5)

              data <- data.frame(Primates, Bodywt, Brainwt)

              pdf(NULL)
              q <- plot_ly(data,x = ~Bodywt, y = ~Brainwt, type = 'scatter',
                           mode = 'text', text = ~Primates, textposition = 'middle center',
                           textfont = list(color = '#000000', size = 16))%>%
                layout(title = 'Q-Q Plot',
                       xaxis = list(title = ')',
                                    zeroline = TRUE,
                                    range = c(0, 1)),
                       yaxis = list(title = '',
                                    range = c(0,1)))


              dev.off()

              q
            }
            else{
              p <- rnbi.qqplot.double.rrbs(x,y)
              q <- plotly::ggplotly(p)
            }

          }
          else if (rrbs_analysis1 == FALSE && rrbs_analysis2 == FALSE){

            p <- rnbi.qqplot.double(x,y)
            q <- plotly::ggplotly(p)
          }
          else{

            Primates <- c('Please select the same analysis i.e. both either IDAT analysis or RRBS analysis')
            Bodywt <- c(0.5 )
            Brainwt <- c(0.5)

            data <- data.frame(Primates, Bodywt, Brainwt)

            pdf(NULL)
            q <- plot_ly(data,x = ~Bodywt, y = ~Brainwt, type = 'scatter',
                         mode = 'text', text = ~Primates, textposition = 'middle center',
                         textfont = list(color = '#000000', size = 16))%>%
              layout(title = 'Q-Q Plot',
                     xaxis = list(title = ')',
                                  zeroline = TRUE,
                                  range = c(0, 1)),
                     yaxis = list(title = '',
                                  range = c(0,1)))


            dev.off()

            q

          }





          output$info.qqplot2 <- renderUI({

            HTML(paste("'<p>QQplot is generated from the diffmeth.p.values of the comparisons selected above for the two analysis.</p> <br/ > <p><b>diffmeth.p.val:</b> p-value obtained from a two-sided Welch t-test or alternatively from linear models employed in the limma package (which type of p-value is computed is specified in the differential.site.test.method option). In case of paired analysis, the paired Student's t-test is applied.",'</p>',sep=""))

          })


          dev.off()
          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())

          q

        }


      }

    })
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

        else
        {


            checkDisplay$data2 <- FALSE

            qq.value <- as.character(input$input_tablebrowser_choices)

            qq.dir <- file.path(results.dir(), qq.value)


            if (qq.value == "" || qq.value == "NA"){
              dataset <- data.table( data = "No data available.")
            }
            else
            {


              data_type = rnbi.analysis.datatype(results.dir() , qq.value)

              if (grepl('idat files' , data_type )) # true if idat files is the data type of the analysis in the string data_type
              {
                rrbs_analysis = FALSE
              }
              else{
                rrbs_analysis = TRUE
              }

              if (rrbs_analysis == TRUE)
              {


                #index() contains the index of the selected file ffrom the dropdown
                f = paste("diffMethTable_site_cmp",comp.file.index(), ".csv.gz",sep = '')

                if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){

                  # Create a Progress object
                  progress <- shiny::Progress$new()

                  progress$set(message = "Reading data! please wait...", value = 50)


                  filename=file.path(qq.dir, 'differential_methylation_data',f)

                  filename= as.character(filename)

                  nrows.value <- as.character(input$input_tablebrowser_readtop)

                  if (nrows.value == 'ALL'){
                    nrows.value = -1
                  }

                  # fread function from the library data.table
                  comp.file <- fread(input = paste('zcat < ',filename,sep = ''), nrows = nrows.value )

                  comp.file <- as.data.frame(comp.file)

                  updateSelectInput(session, "input_tablebrowser_x_axis",
                                    label = paste("Select x-axis:"),
                                    choices = as.character(as.vector(names(comp.file[,6:ncol(comp.file)]))))

                  updateSelectInput(session, "input_tablebrowser_y_axis",
                                    label = paste("Select y-axis:"),
                                    choices = as.character(as.vector(names(comp.file[,6:ncol(comp.file)]))))

                  #comp.file <- sprintf("%.3f", names(comp.file[,6:ncol(comp.file)]))
                  comp.file <- rapply(object = comp.file, f = round, classes = "numeric", how = "replace", digits = 3)
                  dataset <- data.table( comp.file)
                  #%>% formatRound(columns =c('mean.diff'), digits= 3)


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
              else{

                #index() contains the index of the selected file ffrom the dropdown
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
                                    choices = as.character(as.vector(names(comp.file[,6:ncol(comp.file)]))))

                  updateSelectInput(session, "input_tablebrowser_y_axis",
                                    label = paste("Select y-axis:"),
                                    choices = as.character(as.vector(names(comp.file[,6:ncol(comp.file)]))))

                  #comp.file <- sprintf("%.3f", names(comp.file[,6:ncol(comp.file)]))
                  comp.file <- rapply(object = comp.file, f = round, classes = "numeric", how = "replace", digits = 3)
                  dataset <- data.table( comp.file)
                  #%>% formatRound(columns =c('mean.diff'), digits= 3)


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


          }
          dataset

        }




      },selection = 'single', filter = 'top',

      extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL,'Scroller'= NULL),
      options = list(deferRender = TRUE,
                     scrollY = 400,
                     scroller = TRUE,
                     #fixedHeader = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = c(1,2, 3,4,5))),
                     #pageLength = 100,
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#368BC1', 'color': '#000'});",
                       "}"),
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


    output$rnbeadsDiffMethReport <- renderUI({

      value.modules <- reactive({as.character(input$input_tablebrowser_choices) })
      wd_modules <- reactive({file.path(results.dir(), value.modules()) })

      if ( file.exists( isolate({ paste(wd_modules(),'differential_methylation.html',sep="/") }) ) ){

        #browseURL(paste('http://internal.genetik.uni-sb.de/dataT7600','as.character(input$select_ia)','index.html',sep="/"))
        HTML(paste('<b><p>Comprehensive details of differential methylation comparisons generated by RnBeads can be found ','<a target = "_blank" href = "http://internal.genetik.uni-sb.de/dataT7600/',paste(as.character(input$input_tablebrowser_choices),'differential_methylation.html"',sep="/"),'>here','</a></p></b>',sep=""))


      }
      else{
        HTML('<p>No reports exist!</p>')

      }

    })

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
        else if (length(filtered_data) > 20000)
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


              data_type = rnbi.analysis.datatype(results.dir() , qq.value)

              if (grepl('idat files' , data_type )) # true if idat files is the data type of the analysis in the string data_type
              {
                rrbs_analysis = FALSE
              }
              else{
                rrbs_analysis = TRUE
              }

              if (rrbs_analysis == TRUE){
                f = paste("diffMethTable_site_cmp",comp.file.index(), ".csv.gz",sep = '')
              }
              else{
                f = paste("diffMethTable_site_cmp",comp.file.index(), ".csv",sep = '')

              }

              if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){

                # Create a Progress object
                progress <- shiny::Progress$new()

                progress$set(message = "Making Plot! please wait...", value = 50)


                filename <- file.path(qq.dir, 'differential_methylation_data',f)


                filename= as.character(filename)

                nrows.value <- as.character(input$input_tablebrowser_readtop)

                if (nrows.value == 'ALL'){
                  nrows.value = -1
                }


                if (rrbs_analysis == TRUE){

                  comp.file <- fread(input = paste('zcat < ',filename,sep = ''), nrows = nrows.value )


                }
                else{
                  # fread function from the library data.table
                  comp.file <- fread(filename, nrows = nrows.value)
                  #comp.file <- fread(filename,sep = ",")

                }




                comp.file <- as.data.frame(comp.file)


                filtered <- comp.file[filtered_data, , drop = FALSE]

#               remove NA columns rows

#                 completeFun <- function(data, desiredCols) {
#                   completeVec <- complete.cases(data[, desiredCols])
#                   return(data[completeVec, ])
#                 }
#
#                 filtered <- completeFun(filtered, c(input$input_tablebrowser_x_axis, input$input_tablebrowser_y_axis))
#

                key <- colnames(comp.file) <- names(comp.file)
                print(key)

                pdf(NULL)

                if (rrbs_analysis == TRUE){

                  p <- plot_ly(filtered,
                               type = "scatter",        # all "scatter" attributes: https://plot.ly/r/reference/#scatter
                               x = ~filtered[,c(input$input_tablebrowser_x_axis)],               # more about scatter's "x": /r/reference/#scatter-x
                               y = ~filtered[,c(input$input_tablebrowser_y_axis)],            # more about scatter's "y": /r/reference/#scatter-y
                               #name = "Plot",   # more about scatter's "name": /r/reference/#scatter-name
                               marker = list(           # marker is a named list, valid keys: /r/reference/#scatter-marker
                                 color="#264E86"        # more about marker's "color" attribute: /r/reference/#scatter-marker-color
                               )) %>%

                    add_trace(filtered,
                              x = ~filtered[,c(input$input_tablebrowser_x_axis)],               # more about scatter's "x": /r/reference/#scatter-x
                              y = ~filtered[,c(input$input_tablebrowser_y_axis)],
                              text = ~filtered[["Chromosome"]]
                              #                             mode = 'lines',                                    # scatter's "y": /r/reference/#scatter-mode
                              #                             line = list(                                       # line is a named list, valid keys: /r/reference/#scatter-line
                              #                               color = "#5E88FC",                               # line's "color": /r/reference/#scatter-line-color
                              #                               dash = "dashed"                                  # line's "dash" property: /r/reference/#scatter-line-dash
                              #                             ),

                    ) %>%

                    layout(                        # all of layout's properties: /r/reference/#layout
                      title = "Plot", # layout's title: /r/reference/#layout-title
                      xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                        title = input$input_tablebrowser_x_axis,      # xaxis's title: /r/reference/#layout-xaxis-title
                        showgrid = F),       # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                      yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                        title = input$input_tablebrowser_y_axis)     # yaxis's title: /r/reference/#layout-yaxis-title
                    )



                }
                else{
                  p <- plot_ly(filtered,
                               type = "scatter",        # all "scatter" attributes: https://plot.ly/r/reference/#scatter
                               x = ~filtered[,c(input$input_tablebrowser_x_axis)],               # more about scatter's "x": /r/reference/#scatter-x
                               y = ~filtered[,c(input$input_tablebrowser_y_axis)],            # more about scatter's "y": /r/reference/#scatter-y
                               #name = "Plot",   # more about scatter's "name": /r/reference/#scatter-name
                               marker = list(           # marker is a named list, valid keys: /r/reference/#scatter-marker
                                 color="#264E86"        # more about marker's "color" attribute: /r/reference/#scatter-marker-color
                               )) %>%

                    add_trace(filtered,
                              x = ~filtered[,c(input$input_tablebrowser_x_axis)],               # more about scatter's "x": /r/reference/#scatter-x
                              y = ~filtered[,c(input$input_tablebrowser_y_axis)],
                              text = ~filtered[["cgid"]]
                              #                             mode = 'lines',                                    # scatter's "y": /r/reference/#scatter-mode
                              #                             line = list(                                       # line is a named list, valid keys: /r/reference/#scatter-line
                              #                               color = "#5E88FC",                               # line's "color": /r/reference/#scatter-line-color
                              #                               dash = "dashed"                                  # line's "dash" property: /r/reference/#scatter-line-dash
                              #                             ),

                    ) %>%

                    layout(                        # all of layout's properties: /r/reference/#layout
                      title = "Plot", # layout's title: /r/reference/#layout-title
                      xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                        title = input$input_tablebrowser_x_axis,      # xaxis's title: /r/reference/#layout-xaxis-title
                        showgrid = F),       # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                      yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                        title = input$input_tablebrowser_y_axis)     # yaxis's title: /r/reference/#layout-yaxis-title
                    )


                }

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

  extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL,'Scroller'= NULL),
  options = list(deferRender = TRUE,
                 scrollY = 400,
                 scroller = TRUE,
                 #fixedHeader = TRUE,
                 columnDefs = list(list(className = 'dt-center', targets = c(1,2, 3,4,5))),
                 #pageLength = 100,
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': '#368BC1', 'color': '#000'});",
                   "}"),
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

  extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL,'Scroller'= NULL),
  options = list(deferRender = TRUE,
                 scrollY = 400,
                 scroller = TRUE,
                 #fixedHeader = TRUE,
                 columnDefs = list(list(className = 'dt-center', targets = c(1,2, 3,4,5))),
                 #pageLength = 100,
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': '#368BC1', 'color': '#000'});",
                   "}"),
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

    choices = rnbi.total.analysis(selectedDir)


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

        nrows.value <- as.character(input$input_topscorer_readtop)

        if (nrows.value == 'ALL'){
          nrows.value = -1
        }

        if (length(get.choice.index()[i]) < 1){

          get.choice.index()[i] <- 1
        }
        #dataset <- rnbi.read.comparisondata.rrbs(analysis.selected,analysis.path,as.integer(unlist(get.choice.index()[i])), nrows.value,column_selected , results.dir())
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

        # rounding off the columns values
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

#           colnames(outputTableData) = head(LETTERS, ncol(outputTableData))
#           head(outputTableData)

          outputTableData


        },selection = 'single', filter = 'top',
        extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL),
        options = list(
          #fixedHeader = TRUE,
          columnDefs = list(list(className = 'dt-center', targets = c(1,2, 3,4,5))),
          pageLength = 100,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#368BC1', 'color': '#000'});",
            "}"),
                       scrollX = TRUE, scrollY = TRUE, dom = 'Blfrtip',buttons = list( 'copy', 'print',list( extend = 'collection',buttons = c('csv', 'excel', 'pdf'), text = 'Download'), I('colvis')),br(), keys = TRUE
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
                         names = c(paste ("A"), paste ("B")),
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
        extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL,'Scroller'= NULL),
        options = list(deferRender = TRUE,
                       scrollY = 400,
                       scroller = TRUE,
                       #fixedHeader = TRUE,
                       columnDefs = list(list(className = 'dt-center', targets = c(1,2, 3,4,5,6))),
                       #pageLength = 100,
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#368BC1', 'color': '#000'});",
                         "}"),
                       scrollX = TRUE, scrollY = TRUE, dom = 'Blfrtip',buttons = list( 'copy', 'print',list( extend = 'collection',buttons = c('csv', 'excel', 'pdf'), text = 'Download'), I('colvis')),br(), keys = TRUE
        ), escape = TRUE)

      }
      else if(length(cb.checked) == 3){


        output$output.ts.table.multivenn.plot.labels <- renderTable({
          Selected_analysis= c(paste("A = ",cb.checked[1]), paste("B = ",cb.checked[2]), paste("C = ",cb.checked[3]))
          result <- data.frame(Selected_analysis)
          data.table(result)
        })

        p <- vennDiagram(dataset.venn.count.list[[3]], include = "both",
                         names = c(paste ("A"), paste ("B"),paste ("C")),
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
        extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL,'Scroller'= NULL),
        options = list(deferRender = TRUE,
                       scrollY = 400,
                       scroller = TRUE,
                       #fixedHeader = TRUE,
                       columnDefs = list(list(className = 'dt-center', targets = c(1,2, 3,4,5,6,7))),
                       #pageLength = 100,
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#368BC1', 'color': '#000'});",
                         "}"),
                       scrollX = TRUE, scrollY = TRUE, dom = 'Blfrtip',buttons = list( 'copy', 'print',list( extend = 'collection',buttons = c('csv', 'excel', 'pdf'), text = 'Download'), I('colvis')),br(), keys = TRUE
        ), escape = TRUE)


      }

      else if(length(cb.checked) == 4){


        output$output.ts.table.multivenn.plot.labels <- renderTable({
          Selected_analysis= c(paste("A = ",cb.checked[1]), paste("B = ",cb.checked[2]), paste("C = ",cb.checked[3]), paste("D = ",cb.checked[4]))
          result <- data.frame(Selected_analysis)
          data.table(result)
        })

        p <- vennDiagram(dataset.venn.count.list[[4]], include = "both",
                         names = c(paste ("A"), paste ("B"),paste ("C"),paste ("D")),
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
        extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL,'Scroller'= NULL),
        options = list(deferRender = TRUE,
                       scrollY = 400,
                       scroller = TRUE,
                       #fixedHeader = TRUE,
                       columnDefs = list(list(className = 'dt-center', targets = c(1,2, 3,4,5,6,7,8))),
                       #pageLength = 100,
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#368BC1', 'color': '#000'});",
                         "}"),
                       scrollX = TRUE, scrollY = TRUE, dom = 'Blfrtip',buttons = list( 'copy', 'print',list( extend = 'collection',buttons = c('csv', 'excel', 'pdf'), text = 'Download'), I('colvis')),br(), keys = TRUE
        ), escape = TRUE)

      }

      else if(length(cb.checked) == 5){

        output$output.ts.table.multivenn.plot.labels <- renderTable({
          Selected_analysis= c(paste("A = ",cb.checked[1]), paste("B = ",cb.checked[2]), paste("C = ",cb.checked[3]), paste("D = ",cb.checked[4]), paste("E = ",cb.checked[5]))
          result <- data.frame(Selected_analysis)
          data.table(result)
        })

        p <- vennDiagram(dataset.venn.count.list[[5]], include = "both",
                         names = c(paste ("A"), paste ("B"),paste ("C"),paste ("D"),paste ("E")),
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
        extensions = list("ColReorder" = NULL,"Buttons" = NULL,"KeyTable" = NULL,'Scroller'= NULL),
        options = list(deferRender = TRUE,
                       scrollY = 400,
                       scroller = TRUE,
                       #fixedHeader = TRUE,
                       columnDefs = list(list(className = 'dt-center', targets = c(1,2, 3,4,5,6,7,8,9))),
                       #pageLength = 100,
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#368BC1', 'color': '#000'});",
                         "}"),
                       scrollX = TRUE, scrollY = TRUE, dom = 'Blfrtip',buttons = list( 'copy', 'print',list( extend = 'collection',buttons = c('csv', 'excel', 'pdf'), text = 'Download'), I('colvis')),br(), keys = TRUE
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
.LOGGER <- "RnShinyBeads"
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


