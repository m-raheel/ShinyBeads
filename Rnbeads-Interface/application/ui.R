library(shiny)
library(RnBeadsInterface)
library(DT)
library(shinyjs)
library(shinythemes)
#library(V8) # package for extended shinyJS
#library(shinyFiles)




choices = "NA"


check_vectors <- c('COMPLETED Loading Data', 'COMPLETED Quality Control', 'COMPLETED Preprocessing', 'COMPLETED Tracks and Tables','COMPLETED Covariate Inference','COMPLETED Exploratory Analysis','COMPLETED Differential Methylation')



shinyUI(



  navbarobject <- navbarPage('idnavBarTop', theme = shinytheme("cerulean"),



  #title= div(id= "id_div_title" ,tags$a(href = '.', tags$img(src = 'RnBeads.png', width = 100, height = 20)),"Rnbeads Interface"),
  title= "Rnbeads Interface",


  # Home nav menu
  tabPanel("Home",


           #tags$style(type="text/css", "body {padding-top: 70px;}"),


           fluidRow(
             column(width = 1,
                    tags$a(href = '.', tags$img(src = 'RnBeads.png'))

             ),
             column(width = 11,
                    headerPanel("Interface")
             )
           ),

           includeCSS("includes/styles.css"),


           #shinythemes::themeSelector(),


           br(),

           fluidRow(
             column(width = 12,
                    shinyjs::useShinyjs(),
                    #shinyjs::extendShinyjs(text = "shinyjs.workingDirButton = function() { location.reload(); }"),


                    # Use imageOutput to place the image on the page
                    #imageOutput("preImage"),


                    tags$strong("Choose RnBeads analysis repository:"),
                    br(),br(),
                    actionButton("workingDirButton",label= "Choose",class="btn btn-primary"),
                    br(),br(),

                    textOutput("ErrorText1"),
                    br(),
                    verbatimTextOutput ("ErrorText2")
             )

           ),

           # commented is the script to change the tab

           tags$head(tags$script('
                                 Shiny.addCustomMessageHandler("myCallbackHandler",
                                 function(typeMessage) {
                                     console.log(typeMessage)
                                     if(typeMessage == 1){

                                        $("a:contains(Repository)").click();
                                     }
                                     if(typeMessage == 2){
                                        $("a:contains(Individual data set)").click();
                                     }
                                     if(typeMessage == 3){
                                        $("a:contains(Integrative Visualization)").click();
                                     }
                                     if(typeMessage == 4){
                                        $("a:contains(DatasetList)").click();
                                     }
                                 });
                                 ')
           ),

           br(),
           actionButton("action", label = "Continue", class="btn btn-primary")

  ),



  # Repository nav menu
  tabPanel("Repository",

           tabsetPanel(id = "repository",


                       tabPanel("AnalysisList",
                                br(),
                                verbatimTextOutput("count_rfolders"),

                                tags$strong("Below are the list of analysis directories created by RnBeads"),

                                br(),
                                br(),
                                dataTableOutput("list_folders"),

                                # # UI output
                                # lapply(1:10, function(i) {
                                #   uiOutput(paste0('choices', i))
                                # })

                                dataTableOutput('table1'),
                                br(),
                                actionButton("view_datasets", label = "View Datasets", class="btn btn-primary")



                       ),

                       tabPanel("DatasetList",
                                br(),
                                tags$strong("List of different data sets used in the analysis:"),
                                tags$p(""),

                                verbatimTextOutput("total_datasets"),
                                br(),
                                dataTableOutput("list_datasets")

                                # UI output
                                # lapply(1:10, function(i) {
                                #   uiOutput(paste0('c',i))
                                # }),

                                # lapply(1:5, function(i) {
                                #   tableOutput(paste ("list_common_dataset",i, sep = "_"))
                                # })



                       ),

                       tabPanel("Others")
           )

  ),


  # individual analysis nav menu

  tabPanel("Individual analysis",

           br(),

           sidebarPanel(
             selectInput("select_ia", "Select analysis folder:", choices)


           ),

           mainPanel(


             tabsetPanel(id = "analysis_option",
                         tabPanel("Analysis options",

                            tags$strong("Lisitng all the options of Rnbeads from the analysis_options.RData file:"),

                            tags$p(""),
                            # UI output
                            # lapply(1:114, function(i) {
                            #   uiOutput(paste0('b', i))
                            # })

                            dataTableOutput("list_options")


                         ),

                         tabPanel("Modules performed",

                              #tags$strong("List of modules performed are:"),

                              tags$p(""),


                              tableOutput("list_module")

                         ),
                         tabPanel("Link to report"),
                         tabPanel(".......")
              )#end of tabsetpanel

           )#end of mainpanel

),#end of individual analysis nav menu


# individual analysis nav menu

tabPanel("Individual data set",

         tabsetPanel(id= "DatasetTab",
                     tabPanel("Dataset",

                              br(),

                              # tags$strong("List of different data sets used in RnBeads analysis:"),
                              # tags$p(""),

                              # lapply(1:10, function(i) {
                              #   tableOutput(paste0('annotation',i))
                              #
                              # }),

                              selectInput("dd_ids_datasets", "Datasets:", choices),
                              h3(span( "Selected Dataset", class="label label-default"), class= "text-info"),
                              h3(verbatimTextOutput("h1_datasettab"), class= "text-info"),
                              br(),br(),
                              dataTableOutput(paste0('annotation'))
                     ),

                     tabPanel("..."),

                     tabPanel(".......")
         )

),# end of individual dataset nav menu

# individual analysis nav menu

tabPanel("Integrative Visualization",

         br(),
         sidebarPanel(
           selectInput("input_dmcomp_choices", "Select analysis folder:", choices)

         ),


         mainPanel(

           tabsetPanel("visualization",
                       tabPanel("QQ-Plots",
                                br(),
                                tabsetPanel("sub_visuallization",

                                    tabPanel("QQ-Plots 1",

                                           br(),

                                           selectInput("input_dmcomp_files", "comparisons:", ""),


                                           # shinyjs::hidden(
                                           #   div(id = "hide_input",
                                           #       selectInput("input_dmcomp_files_index", "comparisons:", "")
                                           #
                                           #   )
                                           # ),


                                           tags$p("The qqplot of diffmethy p values from the above selected comparison is shown below:"),
                                           radioButtons("dist", "Distribution type:",
                                                        c(
                                                          "Uniform" = "unif",
                                                          "Normal" = "norm")
                                                          # "Log-normal" = "lnorm",
                                                          # "Exponential" = "exp")
                                                          ),
                                           plotOutput('compqqplot')


                                    ),

                                   tabPanel("QQ-Plots 2",

                                            br(),
                                            h3("Select comparison among two rnbeads analysis"),
                                            fluidRow(
                                              column(width = 6,
                                                     selectInput("input_dmcomp_choices_1", "Analysis 1:", choices),
                                                     br(),

                                                     selectInput("input_dmcomp_files_1", "Comaprisons 1:", "")

                                              ),
                                              column(width = 6,
                                                     selectInput("input_dmcomp_choices_2", "Analysis 2:", choices),
                                                     br(),
                                                     selectInput("input_dmcomp_files_2", "Comparisons 2:", "")

                                              )
                                            ),



                                            plotOutput('multicompqqplot')


                                  ),

                                  tabPanel("QQ-Plots 3",

                                           br(), br(),

                                           checkboxGroupInput("check_comp", label = h3("Select comparison file"),
                                                              choices = list("",1),
                                                              selected = 1),

                                           actionButton('insertBtn', 'Show'),


                                           plotOutput('compqqplot3')

                                  )#tab panel
                                )# tab set panel

                       ),

                       tabPanel("Table Browser",
                                br(), br(),

                                tabsetPanel("Browser",
                                            tabPanel("Parameter Overview",

                                                     br(), br(),

                                                     tags$p("The table below lists the options of the executed module.")

                                                     ,

                                                     tableOutput("htmlTable")

                                            ),

                                            tabPanel("Comparisons",
                                                     br(), br(),

                                                     tags$p("The following comparisons were made:")

                                                     ,

                                                     tableOutput("htmlcomparisonTable")



                                            ),
                                            tabPanel("Comparisons CSV"



                                            ),
                                            tabPanel(".......")
                                )


                        ),

                       tabPanel("Top-scorer list stability")

           )# end of tabsetpanel("visualization")

         )# end  of  main panel

),#end of integrative visualization nav menu

tabPanel("About",


         # Application title
         #headerPanel("RnBeads Results"),



         # Sidebar with controls to select the variable to plot against xyz
         # and to specify whether outliers should be included
         sidebarPanel(
           selectInput("input_type", "Select analysis folder:", choices)

           #checkboxInput("outliers", "Show outliers", FALSE)


         ),

         mainPanel(





           tabsetPanel(
             tabPanel("Summary",
                      br(), br(),

                      h3("clientData values"),
                      verbatimTextOutput("clientdataText"),

                      br(), br(),
                      tags$strong("Working Directory:"),
                      verbatimTextOutput("workingDirText"),

                      tags$strong("Selected RnBeads reuslts folder path:"),
                      verbatimTextOutput("text"),


                      tags$strong("Dataset path used for the analysis of RnBeads:"),
                      verbatimTextOutput("data_path"),


                      # a div named mydiv

                      tags$div("click to change color" , id="mydiv", style="width: 50px; height :60px;
                      left: 550px; top: 000px;
                      background-color: gray; position: absolute"),

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

             ),# tab panel end

             tabPanel("Annotation.csv",
                      br(), br()



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

        )#end of mainbar


  )# end of about tab  menu



  )# end of nav bar page



)
