library(shiny)
library(DT)
#library(shinyFiles)


results.dir = file.path(getwd(), 'results')
image.dir = file.path(getwd(), 'images')

choices = list.files(path = results.dir)


check_vectors <- c('COMPLETED Loading Data', 'COMPLETED Quality Control', 'COMPLETED Preprocessing', 'COMPLETED Tracks and Tables','COMPLETED Covariate Inference','COMPLETED Exploratory Analysis','COMPLETED Differential Methylation')



shinyUI(navbarPage(

  #title=div(img(src=textOutput("logo")), "RnBeads"),
  title= "RnBeads"  ,

  # Repository nav menu

  tabPanel("Repository",


           tabsetPanel("Repository 1",
                       tabPanel("List of analysis",
                                br(),
                                tags$strong("Below are the list of analysis directories created by RnBeads"),

                                tags$p(""),

                                # UI output
                                lapply(1:10, function(i) {
                                  uiOutput(paste0('choices', i))
                                })

                       ),

                       tabPanel("List of data sets",
                                br(),
                                tags$strong("List of RnBeads analysis performed on the same data sets:"),
                                tags$p(""),



                                ## UI output
                                # lapply(1:10, function(i) {
                                #   uiOutput(paste0('c',i))
                                # }),

                                br()



                                # tags$strong("List of different data sets used in RnBeads analysis:"),
                                # tags$p(""),
                                #
                                # lapply(1:10, function(i) {
                                #   tableOutput(paste0('annotation',i))
                                #
                                # })



                       ),

                       tabPanel(".......")
           )
  ),


  # individual analysis nav menu

  tabPanel("Individual analysis",

           tabsetPanel("Analysis options",
                       tabPanel("Analysis options",

                                br(),

                                sidebarPanel(
                                  selectInput("input_options", "Select individual analysis directory:", choices)


                                ),

                                mainPanel(

                                  tags$strong("Displaying First 100 options from the analysis_options.RData file:"),

                                  tags$p(""),
                                  # UI output
                                  lapply(1:100, function(i) {
                                    uiOutput(paste0('b', i))
                                  })
                                )

                       ),

                       tabPanel("Modules performed",
                                br(),
                                sidebarPanel(
                                  selectInput("input_modules", "Select individual analysis directory:", choices)


                                ),

                                mainPanel(

                                  tags$strong("List of modules performed are:"),

                                  tags$p(""),
                                  # UI output
                                  tags$script('
                                              Shiny.addCustomMessageHandler("myperformedmodulesno",
                                              function(color) {
                                              return color
                                              });
                                              '),

                                  lapply(1:7, function(i) {

                                    uiOutput(paste0("output_modules",i))

                                  })



                                  )# end of main panel


                       ),
                       tabPanel("Link to report"),
                       tabPanel(".......")
  )
),


# individual analysis nav menu

tabPanel("Individual data set",

         tabsetPanel("Analysis options",
                     tabPanel("Analysis options",

                              tags$strong("Displaying:"),

                              tags$p("")

                     ),

                     tabPanel("Modules performed"),

                     tabPanel(".......")
         )
),

# individual analysis nav menu

tabPanel("Integrative Visualization",

         tabsetPanel("visualization",
                     tabPanel("QQ-Plots",

                              br(),
                              sidebarPanel(
                                selectInput("input_dmcomp_choices", "Repository:", choices)

                              ),

                              mainPanel(



                                tabsetPanel(
                                  tabPanel("Single File Comparison",
                                           br(), br(),

                                           selectInput("input_dmcomp_files", "differential_methylation_data folder:", ""),



                                           tags$p("The qqplot of diffmethy p values from the above selected csv is shown below:"),
                                           radioButtons("dist", "Distribution type:",
                                                        c("Normal" = "norm",
                                                          "Uniform" = "unif",
                                                          "Log-normal" = "lnorm",
                                                          "Exponential" = "exp")),
                                           plotOutput('compqqplot')


                                  ),# tab panel end

                                  tabPanel("Multiple File Comparison",
                                           br(), br(),
                                           # radioButtons("radio_comp", label = h3("Select comparison file"),
                                           #              choices = list("",1),
                                           #              selected = 1),
                                           #
                                           #
                                           # plotOutput('compqqplot2'),


                                           checkboxGroupInput("check_comp", label = h3("Select comparison file"),
                                                              choices = list("",1),
                                                              selected = 1),

                                           actionButton('insertBtn', 'Show'),


                                           plotOutput('compqqplot3')



                                  )

                                )# end tabset panel

                              )# end  of  main panel

                     ),

                     tabPanel("Table Browser",
                              br(), br(),

                              tabsetPanel("Browser",
                                          tabPanel("Parameter Overview",

                                                   br(), br(),

                                                   tags$p("The table below lists the options of the executed module."),

                                                   tableOutput("htmlTable")

                                          ),

                                          tabPanel("Comparisons",
                                                   br(), br(),

                                                   tags$p("The following comparisons were made:"),

                                                   tableOutput("htmlcomparisonTable")



                                          ),
                                          tabPanel("Comparisons CSV"



                                          ),
                                          tabPanel(".......")
                              )


                      ),

                     tabPanel("Top-scorer list stability")
         )
),

tabPanel("About",


         # Application title
         #headerPanel("RnBeads Results"),



         # Sidebar with controls to select the variable to plot against xyz
         # and to specify whether outliers should be included
         sidebarPanel(
           selectInput("input_type", "Select RnBeads Results Folder:", choices)

           #checkboxInput("outliers", "Show outliers", FALSE)


         ),

         mainPanel(

           tags$strong("Choose RnBeads Repository:"),
           actionButton("workingDirButton", "Choose"),
           br(), br(),


           tabsetPanel(
             tabPanel("Summary",
                      br(), br(),
                      tags$strong("Working Directory:"),
                      verbatimTextOutput("workingDirText"),

                      tags$strong("Selected RnBeads reuslts folder path:"),
                      verbatimTextOutput("text"),


                      tags$strong("Dataset path used for the analysis of RnBeads:"),
                      verbatimTextOutput("data_path"),


                      # a div named mydiv

                      #tags$div("click to change color" , id="mydiv", style="width: 50px; height :60px;
                      #left: 550px; top: 000px;
                      #background-color: gray; position: absolute"),

                      # javascript code to send data to shiny server

                      tags$script('
                                  document.getElementById("mydiv").onclick = function() {
                                  var number = Math.random();
                                  Shiny.onInputChange("mydata", number);
                                  };
                                  '),

                      # handler to receive data from server


                      tags$input( id="myinput", style="width: 100px; height :60px;
                                  left: 150px; top: 100px;
                                  position: absolute"),

                      tags$script('
                                  Shiny.addCustomMessageHandler("myperformedmodulesno",
                                  function(color) {
                                  document.getElementById("myinput").value = color;
                                  });
                                  ')

                      ),# tab panel end

             tabPanel("Analysis Options",
                      br(), br()
                      #
                      #
                      # tags$strong("Displaying First 10 options from the analysis_options.RData file:"),
                      #
                      # tags$p(""),
                      # # UI output
                      # lapply(1:10, function(i) {
                      #   uiOutput(paste0('b', i))
                      # })
                      #
             ),# tab panel end

             tabPanel("Annotation.csv",
                      br(), br()
                      # tags$strong("These are the list of RnBeads Results folder that was performed on the same samples:"),
                      # tags$p(""),
                      # #verbatimTextOutput("same_sample"),
                      #
                      # # UI output
                      # lapply(1:10, function(i) {
                      #      uiOutput(paste0('c',i))
                      # }),
                      #
                      #
                      # lapply(1:10, function(i) {
                      #   tableOutput(paste0('annotation',i))
                      #
                      # })


                      #tags$strong("Below are the samples information used to generate above mentioned analysis folder(s):"),
                      #tags$p(""),
                      #tableOutput("summary")


             ),# tab panel end




             tabPanel("qq-Plots",
                   br(), br(),
                   radioButtons("qqplots", "QQ-plots:",
                                c("summary1_betas_qq", "summary2_betas_qq")),

                   imageOutput("qqimage", width="300px",height="600px")

             ),

             tabPanel("All qq-Plots",
                      #fluidRow(
                      #splitLayout(cellWidths = c("60%", "60%"), imageOutput("qq1plot1"), imageOutput("qq1plot2")),
                      #column(6,imageOutput("qq1plot1", width="300px",height="300px")),
                      #column(6,imageOutput("qq1plot2", width="300px",height="300px"))
                      #)
                      imageOutput("qq1plot1", width="300px",height="600px"),
                      imageOutput("qq1plot2", width="300px",height="600px")

             )# tab panel end
                      )# end tabset panel

                      )
         )# end of tab panel nav bar
))
