library(shiny)
library(RnBeadsInterface)
library(RnBeads)
library(XML)
library(compare)
library(data.table) # using the function fread for reading large csv files
library(qqman)
library(tcltk)# OS independent file dir selection
library(lattice)# using qqunif.plot
#library(plotly) #interactive graphics with D3

qqman.qq <- qqman::qq    #EDIT

#library(shinyFiles)

# createLink <- function(val) {
#   sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">Info</a>',val)
# }




shinyServer(function(input, output, session) {

  # update timestamp when update is clicked
  shinyjs::onclick("update", shinyjs::html("time", date()))

  # x contains all the observations of the x variable selected by the user. X is a reactive function
  x <- reactive({
    iris[,as.numeric(input$var1)]
  })
  # x contains all the observations of the y variable selected by the user. Y is a reactive function
  y <- reactive({
    iris[,as.numeric(input$var2)]

  })
  # xl contains the x variable or column name of the iris dataset selected by the user
  xl <- reactive({
    names(iris[as.numeric(input$var1)])
  })
  # yl contains the y variable or column name of the iris dataset selected by the user
  yl <- reactive({
    names(iris[as.numeric(input$var2)])
  })

  # render the plot so could be used to display the plot in the mainPanel
  output$plot <- renderPlot({
    plot(x=x(), y=y(), main = "iris dataset plot", xlab = xl(), ylab = yl())

  })

  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$down <- downloadHandler(
    filename =  function() {
      paste("iris", input$var3, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$var3 == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      plot(x=x(), y=y(), main = "iris dataset plot", xlab = xl(), ylab = yl()) # draw the plot
      dev.off()  # turn the device off

    }
  )



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


  # commented is the script to change the tab

  observe({
    if(input$action > 0){

      session$sendCustomMessage("myCallbackHandler", "1")
    }
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

  #select working directory
  selectedRepository <- eventReactive(input$workingDirButton,{

    updatedDir <- normalizePath("/projects/factorization/raw_data/Demo Repository", winslash = "\\", mustWork = NA)



    selectedDir <-  as.character(updatedDir)

    # return the RnBeads directories
    #setwd(selectedDir)

    #shinyjs::js$workingDirButton()

    # updating all the selectInput dropdowns

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

    selectedDir

  })


  # displaying all session values store in the clientData

  # Store in a convenience variable
  cdata <- session$clientData

  # Values from cdata returned as text
  output$clientdataText <- renderText({
    cnames <- names(cdata)

    allvalues <- lapply(cnames, function(name) {
      paste(name, cdata[[name]], sep=" = ")
    })
    paste(allvalues, collapse = "\n")
  })


  dirfolder = reactive({list.files(path = getwd())})
  results.dir = reactive({file.path(path = selectedRepository())})

  # selected RnBeads repository folder
  value <- reactive({as.character(input$input_type) })
  rwaDir <- reactive({file.path(results.dir(), value()) })



  # Check if file exists and working directory is correct


  output$logo <- renderText({

    file.path(results.dir(), 'images/RnBeads.png')
  })


  ####################################################################################

  # displaying folders of repository selected
  ##################################################################################
  observe({


    choices <- list.files(path = results.dir())

    output$count_rfolders <- renderText({
      paste("Total directories in this repository =", length(choices), sep = " ")

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

        })

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

          })

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

      }

      else if (length(analysis_list) != 0){
        al <- reactive({analysis_list[last_character]})

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





    }
  })

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

  # showing comparisons performed from the HTML file
  ############################################################################################

  observeEvent(input$input_dmcomp_choices,{

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


  list.pvalues <- reactive({

    qq.value <- as.character(input$input_dmcomp_choices)

    qq.dir <- file.path(results.dir(), qq.value)

    #qq.value <- as.character(input$input_dmcomp_files)



    if (qq.value == "" || qq.value == "NA"){
      x <- list()
      x
    }
    else{

      #fucntion from the RnBeadsInterface package

      #index_list() contains the index of the selected file ffrom the dropdown
      f = paste("diffMethTable_site_cmp",index_list(), ".csv",sep = '')

      if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){



        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())

        progress$set(message = "Making plot", value = 0)


        comparison_plot(qq.dir , f)

        # # Increment the progress bar, and update the detail text.
        # progress$inc(detail = paste("Please wait..."))
        #
        # # Pause for 0.1 seconds to simulate a long computation.
        # Sys.sleep(0.1)


      }
      else{
        x <- list()
        x
      }
    }


  })

  plotInput <- reactive({

    if(length(list.pvalues()) == 0) {

      # print error/ warning message
      qqplot(1,1,main="Normal Q-Q Plot", ylab="diffmeth.p.val")
      text(1,1,"No data available or no comparison file exist")

    }
    else{
      #y <- dist(ppoints(length(list.pvalues())))
      #qqline(y,list.pvalues())
      #qq(gwasResults$P, main = "Q-Q plot of GWAS p-values")

      #qqman.qq(list.pvalues(),main="Q-Q plot of p-values")

      #qqplot(y,list.pvalues(),main=input$dist,xlab="Theoretical Quantile", ylab="diffmeth.p.val")

      ##from package lattice
      qqunif.plot(list.pvalues())



    }

  })

  output$compqqplot <- renderPlot({

    plotInput()



  }, height = 400, width = 500)

  output$downloadData <- downloadHandler(



    # filename = function(file) {
    #   paste("test",".csv",sep = ".")
    # },
    # content = function(file) {
    #   write.csv('', file)
    # }

    filename = function() { paste('testing', 'png', sep='.') },
    content = function(file) {
      png(file)
      plotInput()
      dev.off()
    }

  )



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

        comparison_plot(qq.dir , f)
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

        comparison_plot(qq.dir , f)
      }
      else{
        x <- list()
        x
      }
    }


  })





  observeEvent(input$displayBtn, {

    v$data <- TRUE

    print (v$data)

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
          x<- list.pvalues_1()
          y<- list.pvalues_2()

          #qqplot(x,y,main="Normal Q-Q Plot", xlab="diffmeth.p.val 1", ylab="diffmeth.p.val 2")
          my.pvalue.list<-list("Analysis 1"=x, "Analysis 2"=y)
          qqunif.plot(my.pvalue.list, auto.key=list(corner=c(.95,.05)))
        }
      }

    }, height = 400, width = 500)


  })

  ###################################################################################################

  # QQ Plot 3  comparison among different files of same RnBeads Analysis
  ###################################################################################################

  observeEvent(input$insertBtn, {

    qq.value <- as.character(input$input_dmcomp_choices)

    qq.dir <- file.path(results.dir(), qq.value)


    vec <- as.list(input$check_comp)

    check.choices.list <- list()

    if (length(vec) == 0){
      x <- list()
      output$compqqplot3 <- renderPlot({



        if(length(check.choices.list) == 0) {

          # print error/ warning message
          qqplot(1,1,main="Normal Q-Q Plot", ylab="diffmeth.p.val")
          text(1,1,"No data available or no comparison file exist")

        }
        else{
          qqnorm(x)
        }




      }, height = 400, width = 500)
    }
    else{



      for (i in 1:length(vec)) {

        #fucntion from the RnBeadsInterface package

        f = "diffMethTable_site_cmp1.csv"

        if ( file.exists( isolate({ paste(qq.dir,'differential_methylation_data',f,sep="/") }) ) ){

          check.choices.list[i] <- list(comparison_plot(qq.dir , f))
        }
      }



      output$compqqplot3 <- renderPlot({



        print(as.numeric(length(check.choices.list)) == 1)

        if(length(check.choices.list) == 0) {

          # print error/ warning message
          qqplot(1,1,main="Normal Q-Q Plot", ylab="diffmeth.p.val")
          text(1,1,"No data available or no comparison file exist")

        }
        else if(as.numeric(length(check.choices.list)) == 1) {

          y<- unlist(check.choices.list[1])


          qqnorm(y,main="Normal Q-Q plot", xlab="Theoretical Distribution", ylab="diffmeth.p.val")



        }
        else{
          x<- unlist(check.choices.list[1])
          y<- unlist(check.choices.list[2])

          qqplot(x,y,main="Normal Q-Q Plot", xlab="diffmeth.p.val", ylab="diffmeth.p.val")

        }




      }, height = 400, width = 500)


    }


  })

  ####################################################################################
  # output for the about section

  output$text <- renderText({
    paste("You have selected:",results.dir())
  })

  output$results.directory <- renderText({
    rwaDir()
  })

  output$results = renderPrint({
    input$mydata
  })

  observe({
    input$mydata
    color = rgb(runif(1), runif(1), runif(1))
    session$sendCustomMessage(type = "myCallbackHandler", color)
  })

  output$workingDirText <- renderText({getwd() })


  ####################################################################################


  output$table1 <- renderDataTable({

    # my_table <- cbind(rownames(mtcars), mtcars)
    # colnames(my_table)[1] <- 'car'
    # my_table$link <- createLink(my_table$car)
    # return(my_table)

  }, escape = FALSE)


  # displaying plots in plots tab
  ###################################################################################3

  qqplot1.path <- reactive({file.path(rwaDir(), "preprocessing_pdfs/summary1_betas_qq.pdf")})
  qqplot2.path <- reactive({file.path(rwaDir(), "preprocessing_pdfs/summary2_betas_qq.pdf")})

  output$qqplot1 <- renderText({
    return(paste('<iframe style="height:600px; width:100%" src="', qqplot1.path , '"></iframe>', sep = ""))
  })

  # Send a qq1 image, and don't delete the image after sending it
  output$qq1plot1 <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path(rwaDir(),
                                        paste('preprocessing_images/summary1_betas_qq', '.png', sep='')), winslash = "\\", mustWork = NA)

    # Return a list containing the filename and alt text
    list(src = filename,alt = paste("No record found."))

  }, deleteFile = FALSE)

  # Send a qq2 image, and don't delete the image after sending it
  output$qq1plot2 <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path(rwaDir(),
                                        paste('preprocessing_images/summary2_betas_qq', '.png', sep='')), winslash = "\\", mustWork = NA)

    # Return a list containing the filename and alt text
    list(src = filename,alt = paste("No record found."))

  }, deleteFile = FALSE)

  #  sends pre-rendered images according to the radio button selection
  output$qqimage <- renderImage({
    if (is.null(input$qqplots))
      return(NULL)

    if (input$qqplots == "summary1_betas_qq") {

      filename <- normalizePath(file.path(rwaDir(),
                                          paste('preprocessing_images/summary1_betas_qq', '.png', sep='')), winslash = "\\", mustWork = NA)

      return(list(
        src = filename,
        contentType = "image/png",
        alt = "No record found."
      ))
    } else if (input$qqplots == "summary2_betas_qq") {

      filename <- normalizePath(file.path(rwaDir(),
                                          paste('preprocessing_images/summary2_betas_qq', '.png', sep='')), winslash = "\\", mustWork = NA)

      return(list(
        src = filename,
        filetype = "image/png",
        alt = "No record found."
      ))
    }

  }, deleteFile = FALSE)
  #######################################################################


})

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

