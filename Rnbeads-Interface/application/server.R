library(shiny)
library(RnBeadsInterface)
library(RnBeads)
library(XML)
library(compare)
library(data.table) # using the function fread for reading large csv files

library(tcltk)# OS independent file dir selection


#library(shinyFiles)

# createLink <- function(val) {
#   sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">Info</a>',val)
# }




shinyServer(function(input, output, session) {


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


  #select working directory
  selectedRepository <- eventReactive(input$workingDirButton,{



    updatedDir <- tk_choose.dir(getwd(), "Choose an Rnbeads repository")

    #updatedDir <- choose.dir(getwd(), "Choose an Rnbeads repository")

    #workDir = gsub("\\\\", "/", updatedDir)

    selectedDir <-  as.character(updatedDir)

    # return the Rnbeads directories
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
      output$ErrorText1 <- renderText({ paste("You are working with the Rnbeads analysis repository:",sep="") })
      output$ErrorText2 <- renderText({ paste(selectedDir,sep="") })
    }
    else{
      output$ErrorText1 <- renderText({ paste("Not a Valid Rnbeads Repository:",sep="") })
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


        #DT <- data.table(ID = 1:length(choices) , Rnbeads_Analysis = choices)
        # DT



        DT <- data.table( Rnbeads_Analysis = choices)
        # DT$Rnbeads_Analysis <- sapply(DT$Rnbeads_Analysis, function(x)
        #   toString(tags$a(href=paste0("#Individual analysis", x), x)))

        #DT$link <- createLink(DT$Rnbeads_Analysis)


        return(DT)

      },selection = 'single', escape = FALSE)

    }

    else{

      output$list_folders <- renderDataTable({

        DT <- data.table( Rnbeads_Analysis = 'No directory in this repository.')
        return(DT)

      },selection = 'single', escape = FALSE)

    }
  })

  # if the list folder row is selected
  observeEvent(input$list_folders_rows_selected, {
    row <- input$list_folders_rows_selected

    updateTabsetPanel(session, "repository", selected = "DatasetList")
  })



  ###############################################################

  # check and return the results folder that have the same sample annotation file.###############
  ############################################################################################

  observe({

    cd_list <- list()
    cd_list_counter <- 1


    common.datasets = datasets_common(results.dir())
    #common.datasets <- list()

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

  # if the dataset row is selected
  observeEvent(input$list_datasets_rows_selected, {
    row <- input$list_datasets_rows_selected

    #value_selected <- DT[row, "Datasets_Used"]


    datasets_files = datasets_list(results.dir())

    print(datasets_files)

    row <- as.integer(row)

    print(row)

    a.file <- reactive({read.csv(as.character(datasets_files[row]))[ ,1:6]})

    # Generate a summary of the dataset
    output[[paste0('annotation')]] <- renderDataTable({

      dataset <- a.file()
      dataset




    },selection = 'single', escape = TRUE)

    output$h1_datasettab <- renderText({
      paste("Dataset_",row)

    })

    #print(row)
    session$sendCustomMessage("myCallbackHandler", "2")
    #updateTabsetPanel(session, "Individual dataset", selected = "DatasetTab")
  })



  ############################################################################################


  # analysis options
  ##################################################################################


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

  ################################################################

  # list of modules performed
  ############################################################################################

  observeEvent(input$select_ia,{

    value.modules <- reactive({as.character(input$select_ia) })


    wd_modules <- reactive({file.path(results.dir(), value.modules()) })


    if ( file.exists( isolate({ paste(wd_modules(),'analysis.log',sep="/") }) ) ){
      #fucntion from the RnBeadsInterface package


      Performed_Modules <-  modules_performed(wd_modules())

      modules <- unlist(Performed_Modules)

      output$list_module <- renderTable({
        DT <- data.table(ID = 1:length(modules) , Performed_Modules = modules)
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



  # individual data sets tab
  ############################################################################################

  observeEvent(input$dd_ids_datasets,{

    dd_datasets <- as.character(input$dd_ids_datasets)

    tmp = toString(dd_datasets)
    len = nchar(tmp)
    last_character = substr(tmp,len,len)




    if (dd_datasets != "NA"){

      last_character = as.integer(last_character)

      datasets_files = datasets_list(results.dir())


      a.file <- reactive({read.csv(as.character(datasets_files[last_character]))[ ,1:6]})

      # Generate a summary of the dataset
      output[[paste0('annotation')]] <- renderDataTable({
        #paste0("Annotation.csv")
        dataset <- a.file()
        dataset

      })

      output$h1_datasettab <- renderText({
        paste("Dataset_",last_character)

      })

    }
  })
  ###############################################################################################
  # Comparisons Table from HTML files

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


  # qqplots 1 of diff methylation p- values
  ####################################################################################

  observeEvent(input$input_dmcomp_choices,{

    input_choices <- as.character(input$input_dmcomp_choices)

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

          #if statement is not vectorized. For vectorized if statements you should use ifelse
          #ifelse(length(comp_names)>0,choices.list <- comp_names, choices.list <- 'NA')

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

        comparison_plot(qq.dir , f)
      }
      else{
        x <- list()
        x
      }
    }


  })



  output$compqqplot <- renderPlot({

    dist <- switch(input$dist,
                   unif = runif,
                   norm = rnorm,

                   # lnorm = rlnorm,
                   # exp = rexp,
                   rnorm)

    if(length(list.pvalues()) == 0) {

      # print error/ warning message
      qqplot(1,1,main="Normal Q-Q Plot", ylab="diffmeth.p.val")
      text(1,1,"No data available or no comparison file exist")

    }
    else{
      y <- dist(ppoints(length(list.pvalues())))
      #qqline(y,list.pvalues())
      qqplot(y,list.pvalues(),main=input$dist,xlab="Theoretical Quantile", ylab="diffmeth.p.val")

    }

  }, height = 400, width = 500)

  #######################################################################



  # qqplots 2 of diff methylation p- values in which two comarprisons qqplots is displayed
  ####################################################################################


  # for Repository 1
  observeEvent(input$input_dmcomp_choices_1,{

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

          #if statement is not vectorized. For vectorized if statements you should use ifelse
          #ifelse(length(comp_names)>0,choices.list <- comp_names, choices.list <- 'NA')

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




  output$multicompqqplot <- renderPlot({

    dist <- switch(input$dist,
                   unif = runif,
                   norm = rnorm,

                   # lnorm = rlnorm,
                   # exp = rexp,
                   rnorm)

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

      qqplot(x,y,main="Normal Q-Q Plot", xlab="diffmeth.p.val 1", ylab="diffmeth.p.val 2")

    }

  }, height = 400, width = 500)

  #######################################################################


  # QQ Plot 3  comparison among different files of same Rnbeads Analysis
  ####################################################################################

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


        #qq.value <- as.character(unlist(vec[i][1]) )
        #f = "diffMethTable_site_cmp1.csv"
        #check.choices.list[i] <- list(comparison_plot(qq.dir , f))


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
