library(shiny)
library(RnBeadsInterface)
library(shiny)
library(RnBeads)
library(XML)
library(compare)
library(data.table) # using the function fread for reading large csv files


#library(shinyFiles)

shinyServer(function(input, output, session) {

  # return the Rnbeads directories
  results.dir = file.path(getwd(), 'results')

  print (results.dir)

  choices <- list.files(path = results.dir)

  lapply(1:length(choices), function(i) {
    output[[paste0('choices', i)]] <- renderUI({


      paste0(choices[i])
    })
  })



  output$logo <- renderText({
    file.path(getwd(), 'images/RnBeads.png')
  })

  value <- reactive({as.character(input$input_type) })
  rwaDir <- reactive({file.path(results.dir, value()) })

  ####################################################################################

  # extrating the modules performed information##################

  o <- reactive({

    value.modules <- reactive({as.character(input$input_modules) })

    wd_modules <- reactive({file.path(results.dir, value.modules()) })

    #fucntion from the RnBeadsInterface package
    modules_performed(wd_modules())

  })

  observe(

    lapply(1:length(o()), function(i) {

      output[[paste0('output_modules', i)]] <- renderUI({
        paste0(o()[i])
      })
    })
  )


  ###############################################################

  # analysis options

  value.options <- reactive({as.character(input$input_options) })

  wd_options <- reactive({file.path(results.dir, value.options()) })

  rwaDirUpdated <- reactive({file.path(wd_options(), "analysis_options.RData")})

  # function to read analysis_options.RData file

  LoadToEnvironment <- function(rwaDir, env = new.env()){
    load(rwaDir, env)
    return(env)
  }



  lapply(1:100, function(i) {
    output[[paste0('b', i)]] <- renderUI({
      rdata.env <- LoadToEnvironment(rwaDirUpdated())

      rdata.fit <- rdata.env$analysis.options

      names.rdata.fit <- names(rdata.fit)

      paste0(names.rdata.fit[i]," = ", rdata.fit[i])
    })
  })
  ################################################################


  # displaying plots in plots tab

  qqplot1.path <- reactive({file.path(rwaDir(), "preprocessing_pdfs/summary1_betas_qq.pdf")})
  qqplot2.path <- reactive({file.path(rwaDir(), "preprocessing_pdfs/summary2_betas_qq.pdf")})

  output$qqplot1 <- renderText({
    return(paste('<iframe style="height:600px; width:100%" src="', qqplot1.path , '"></iframe>', sep = ""))
  })

  # Send a qq1 image, and don't delete the image after sending it
  output$qq1plot1 <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path(rwaDir(),
                                        paste('preprocessing_images/summary1_betas_qq', '.png', sep='')))

    # Return a list containing the filename and alt text
    list(src = filename,alt = paste("No record found."))

  }, deleteFile = FALSE)

  # Send a qq2 image, and don't delete the image after sending it
  output$qq1plot2 <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path(rwaDir(),
                                        paste('preprocessing_images/summary2_betas_qq', '.png', sep='')))

    # Return a list containing the filename and alt text
    list(src = filename,alt = paste("No record found."))

  }, deleteFile = FALSE)

  #  sends pre-rendered images according to the radio button selection
  output$qqimage <- renderImage({
    if (is.null(input$qqplots))
      return(NULL)

    if (input$qqplots == "summary1_betas_qq") {

      filename <- normalizePath(file.path(rwaDir(),
                                          paste('preprocessing_images/summary1_betas_qq', '.png', sep='')))

      return(list(
        src = filename,
        contentType = "image/png",
        alt = "No record found."
      ))
    } else if (input$qqplots == "summary2_betas_qq") {

      filename <- normalizePath(file.path(rwaDir(),
                                          paste('preprocessing_images/summary2_betas_qq', '.png', sep='')))

      return(list(
        src = filename,
        filetype = "image/png",
        alt = "No record found."
      ))
    }

  }, deleteFile = FALSE)
  #######################################################################

  # check and return the results folder that have the same sample annotation file.###############
  # uses the functions from the rnbeadsinterface package

  # common.datasets <- reactive({
  #
  #
  #   datasets_groups(results.dir)
  #
  # })
  #
  # observe(
  #
  #   lapply(1:length(common.datasets()), function(i) {
  #
  #     lapply(1:length(common.datasets()[i]), function(j) {
  #       output[[paste0('c',i)]] <- renderUI({
  #
  #
  #         paste0(common.datasets()[i][j])
  #       })
  #     })
  #   })
  # )

  #common.datasets = datasets_groups(results.dir)
  #datasets_files = datasets_list(results.dir)



  # lapply(1:length(common.datasets), function(i) {
  #
  #   lapply(1:length(common.datasets[i]), function(j) {
  #     output[[paste0('c',i)]] <- renderUI({
  #
  #
  #       paste0(common.datasets[i][j])
  #     })
  #   })
  # })

  # lapply(1:length(datasets_files), function(i) {
  #
  #   a.file <- reactive({read.csv(as.character(datasets_files[i]))[ ,1:6]})
  #
  #   # Generate a summary of the dataset
  #   output[[paste0('annotation',i)]] <- renderTable({
  #     #paste0("Annotation.csv")
  #     dataset <- a.file()
  #     dataset
  #
  #
  #   })
  #
  # })


  ############################################################################################

  # Comparisons Table from HTML files

  filename <- normalizePath(file.path(results.dir, 'mesangial cell','differential_methylation.html'))

  #filename= as.character(filename)

  # tmp <- file.path(results.dir, paste('mesangial cell','/differential_methylation.html'),sep='')
  # #removing space
  # tmp <- gsub(" /", "/", tmp)
  differential.methylation.path <- filename

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



  # Extract table header and contents of the comparison table of differential methylation

  head <- xpathSApply(pagetree, "//*/table[@class='tabdata']/tr/td[@class='header']", xmlValue)
  results <- xpathSApply(pagetree, "//*/table[@class='tabdata']/tr/td", xmlValue)

  # Convert character vector to dataframe
  comparisonTable <- as.data.frame(matrix(results, ncol = 4, byrow = TRUE))


  tablehead <- c('Nr.','comparison','ajustment','covariateTable')
  # Clean up the results
  comparisonTable[,1] <- gsub("Â ", "", comparisonTable[,1])
  tablehead <- gsub("Â ", "", tablehead)
  names(comparisonTable) <- tablehead

  output$htmlcomparisonTable = renderTable({
    comparisonTable
  })


  ############################################################################################

  # comparison tab code


  comp.csv.path <- reactive({file.path(rwaDir(), "differential_methylation_data/diffMethTable_site_cmp1.csv")})



  output$compcsv1 <- renderText({
    return(paste('<iframe style="height:600px; width:100%" src="', comp.csv.path , '"></iframe>', sep = ""))
  })


  # Generate a summary of the dataset
  output[[paste0('diffMethTable')]] <- renderTable({

    filename <- normalizePath(file.path(rwaDir(),
                                        paste('differential_methylation_data/diffMethTable_site_cmp1', '.csv', sep='')))

    filename= as.character(filename)
    # fread function from the library data.table
    list.diff.p.values <- fread(filename,sep = ",", select = c("diffmeth.p.val"), nrows = 10)

    dataset <- list.diff.p.values
    dataset



  })

  ####################################################################################

  # qqplots of diff methylation p- values

  #selected index change event of comparison tab selection panel



  observeEvent(input$input_dmcomp_choices,{

    input_choices <- as.character(input$input_dmcomp_choices)

    qq.dir <- file.path(results.dir, input_choices)

    qq.dmd.dir <- file.path(qq.dir, 'differential_methylation_data')

    input_c = list.files(path = qq.dmd.dir)

    choices.list <- list()
    choices.list.counter <- 1

    for (i in 1:length(input_c)) {

      if(length(grep("diffMethTable_site_cmp",input_c[i]))>0){

        choices.list[choices.list.counter] <- input_c[i]
        choices.list.counter = choices.list.counter+1
      }


    }


    updateSelectInput(session, "input_dmcomp_files",
                      label = paste("differential_methylation_data folder", ""),
                      choices = choices.list)

    if (length(choices.list) > 0){

      updateCheckboxGroupInput(session, "check_comp",

                         label = paste("Select csv files", ""),
                         choices = choices.list
      )

    }
    else{


      updateCheckboxGroupInput(session, "check_comp",

                               label = paste("Select csv files", ""),
                               choices = ""
      )

    }


  })




  list.pvalues <- reactive({

    qq.value <- as.character(input$input_modules)

    qq.dir <- file.path(results.dir, qq.value)

    qq.value <- as.character(input$input_dmcomp_files)


    if (qq.value == ""){
      x <- list()
      x
    }
    else{
      #fucntion from the RnBeadsInterface package
      comparison_plot(qq.dir , qq.value)
    }


  })

  output$compqqplot <- renderPlot({

    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)

    if(length(list.pvalues()) == 0) {

      # print error/ warning message
      qqplot(1,1,main="Normal Q-Q Plot", ylab="diffmeth.p.val")
      text(1,1,"No data available or no comparison file exist")

    }
    else{
      y <- dist(ppoints(length(list.pvalues())))
      qqplot(y,list.pvalues(),main=input$dist,xlab="Theoretical Quantile", ylab="diffmeth.p.val")

    }

  }, height = 400, width = 500)

  #######################################################################

  # comparison among different files of same Rnbeads Analysis
  # observer event for checkbox input to display checked plots

  observeEvent(input$insertBtn, {

    qq.value <- as.character(input$input_modules)

    qq.dir <- file.path(results.dir, qq.value)


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

        qq.value <- as.character(unlist(vec[i][1]) )
        check.choices.list[i] <- list(comparison_plot(qq.dir , qq.value))


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
    paste("You have selected:",rwaDir())
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

  #select working directory
  observeEvent(input$workingDirButton,{
    updatedDir =  choose.dir(getwd(), "Choose a suitable folder")

    workDir = gsub("\\\\", "/", updatedDir)

    output$workingDirText <-  renderText({workDir})



    updateSelectInput(session, "input_type",
                      label = paste("Select RnBeads Results Folder", "---"),
                      choices = list.files(path = workDir))

  })
  ####################################################################################


})
