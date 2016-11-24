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


  # class="navbar navbar-fixed-bottom"


  footer = tags$div(class="", checked=NA,
                    #tags$p("&copy; 2016 - RnBeads-Interface"),

                    htmlTemplate("footer.html")
  ),


  #title= div(id= "id_div_title" ,tags$a(href = '.', tags$img(src = 'RnBeads.png', width = 100, height = 20)),"RnBeads Interface"),
  title= "RnBeads-Interface",
  #title= tags$a(href = '.', tags$p('RnBeads-Interface', class = 'text-danger')),


  # Home nav menu
  tabPanel("Home",

           shinyjs::useShinyjs(),
           #shinyjs::extendShinyjs(text = "shinyjs.clearDirButton = function() { location.reload(); }"),



           #tags$style(type="text/css", "body {padding-top: 70px;}"),




           includeCSS("includes/styles.css"),


           #shinythemes::themeSelector(),

           # fluidRow(
           #   column(width = 1,
           #          tags$a(href = '.', tags$img(src = 'RnBeads.png'))
           #
           #   ),
           #   column(width = 11,
           #          h2("Interface")
           #   )
           # ),

           HTML(paste('<div class="jumbotron">',
                        '<div class="container">',

                            '<h2>Rnbeads-Interface!</h2>',
                            '<p>It is a tool to provide user friendly interactive interface for RnBeads generated reports. It allows to keep track of the analysis performed and prevent performing same analysis again and again. It makes it interactive and easier to compare same or different RnBeads analysis. Target users are the ones who uses RnBeads for analyzing DNA methylation data either individually or as a group.</p>',

                        '</div>',
                      '</div>',

                      '<div class="container">',
                          '<!-- Example row of columns -->',
                          '<div class="row">',
                              '<div class="col-md-6">',
                                '<h2>Working Repository</h2>',
                                  tags$p("Click the select button to read the RnBeads analysis repository:"),

                                  actionButton("workingDirButton",label= "Select",class="btn btn-primary"),


                                  paste('<p>',textOutput("ErrorText1"),'</p>'),

                                  paste('<h5>',textOutput("ErrorText2"),'</h5>'),

                                  actionButton("action", label = "Continue", class="btn btn-primary"),

                                  actionButton("clearDirButton",label= "Clear",class="btn btn-primary"),

                                  br(),
                              '</div>',
                              '<div class="col-md-3">',

                              '<h2>RnBeads</h2>',

                              '</div>',
                              '<div class="col-md-3">',
                                br(),
                                tags$a(class='pull-right', href = '.', tags$img(src = 'RnBeads.png')),

                              '</div>',
                              '<div class="col-md-6">',



                                    '<p>RnBeads is an R package for comprehensive analysis of DNA methylation data obtained with any experimental protocol that provides single-CpG resolution. </p>',
                                    '<p><a class="btn btn-primary btn-md" href="http://rnbeads.mpi-inf.mpg.de/" target = "blank" role="button">Learn more &raquo;</a></p>',






                              '</div>',


                          '</div>',
                      '</div>',

                      '<br/>')),


           #includeHTML("index.html"),




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
           )

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



                       )


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

                            tags$strong("Lisitng all the options of RnBeads from the analysis_options.RData file:"),

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
                         tabPanel("reports")

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



                              wellPanel(
                                tags$strong("Selected dataset is used in following analysis.")
                              ),

                              dataTableOutput(paste0('annotation1')),


                              #h3(span( "Selected Dataset", class="label label-default"), class= "text-info"),
                              #h3(verbatimTextOutput("h1_datasettab"), class= "text-info"),
                              #absolutePanel('absolute panel'),

                              br(),

                              wellPanel(
                                tags$strong("Selected dataset file content.")
                              ),

                              dataTableOutput(paste0('annotation')),
                              br()




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
                                           # radioButtons("dist", "Distribution type:",
                                           #              c(
                                           #                "Uniform" = "unif",
                                           #                "Normal" = "norm")
                                           #                # "Log-normal" = "lnorm",
                                           #                # "Exponential" = "exp")
                                           #                ),
                                           plotOutput('compqqplot'),
                                           downloadButton('downloadData', 'Download')


                                    ),

                                   tabPanel("QQ-Plots 2",

                                            br(),
                                            h3("Select comparison among two RnBeads analysis"),
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



                                            actionButton('displayBtn', 'Display'),
                                            plotOutput('multicompqqplot')


                                  )

                                #   tabPanel("QQ-Plots 3",
                                #
                                #            br(), br(),
                                #
                                #            checkboxGroupInput("check_comp", label = h3("Select comparison file"),
                                #                               choices = list("",1),
                                #                               selected = 1),
                                #
                                #            actionButton('insertBtn', 'Show'),
                                #
                                #
                                #            plotOutput('compqqplot3')
                                #
                                #   )#tab panel
                                )# tab set panel

                       ),

                       tabPanel("Table Browser",
                                br(),



                                tabsetPanel("Browser",
                                            tabPanel("p-values",

                                                     br(), br(),

                                                     tags$p("The table below lists the p values from selected analysis.")

                                                     ,

                                                     tableOutput("htmlTable")

                                            ),

                                            # tabPanel("Comparisons",
                                            #          br(), br(),
                                            #
                                            #          tags$p("The following comparisons were made:")
                                            #
                                            #          ,
                                            #
                                            #          tableOutput("htmlcomparisonTable")
                                            #
                                            #
                                            #
                                            # ),
                                            tabPanel("Comparisons p-values"



                                            )
                                            # tabPanel(".......")
                                )


                        ),

                       tabPanel("Top-scorer list stability")

           )# end of tabsetpanel("visualization")

         )# end  of  main panel

  )#end of integrative visualization nav menu

 # tabPanel("About",
 #
 #
 #          titlePanel("Download base plot in Shiny - an example"),
 #          sidebarLayout(
 #            sidebarPanel(
 #              selectInput(inputId = "var1", label = "Select the X variable", choices = c("Sepal.Length" = 1, "Sepal.Width" = 2, "Petal.Length" = 3, "Petal.Width" = 4)),
 #              selectInput(inputId = "var2", label = "Select the Y variable", choices = c("Sepal.Length" = 1, "Sepal.Width" = 2, "Petal.Length" = 3, "Petal.Width" = 4), selected = 2),
 #              radioButtons(inputId = "var3", label = "Select the file type", choices = list("png", "pdf"))
 #            ),
 #            mainPanel(
 #              plotOutput("plot"),
 #              downloadButton(outputId = "down", label = "Download the plot")
 #            )
 #          ),
 #
 #          # Adding the 'a' tag to the sidebar linking external file
 #          tags$p("'a' tag linking external file"),
 #          tags$a(href='https://designer.genomecompiler.com/plasmid_iframe?file_url=http://s3.amazonaws.com/gcc_production/plasmid_viewer/OG34_OG34_pSF-OXB19.gb', target='blank', 'plasmid1_URLfile'),
 #
 #          # Line spacing
 #          hr(),
 #
 #          # Adding the 'a' tag to the sidebar linking local file
 #          tags$p("'a' tag linking local file"),
 #          tags$a(href='data/plasmid1.txt', target='blank', 'plasmid1_localfile', download = 'plasmid1.txt')
 # )
#
#          # Application title
#          #headerPanel("RnBeads Results"),
#
#
#
#          # Sidebar with controls to select the variable to plot against xyz
#          # and to specify whether outliers should be included
#          sidebarPanel(
#            selectInput("input_type", "Select analysis folder:", choices)
#
#            #checkboxInput("outliers", "Show outliers", FALSE)
#
#
#          ),
#
#          mainPanel(
#
#
#
#
#
#            tabsetPanel(
#              tabPanel("Summary",
#                       br(), br(),
#
#                       h3("clientData values"),
#                       verbatimTextOutput("clientdataText"),
#
#                       br(), br(),
#                       tags$strong("Working Directory:"),
#                       verbatimTextOutput("workingDirText"),
#
#                       tags$strong("Selected RnBeads reuslts folder path:"),
#                       verbatimTextOutput("text"),
#
#
#                       tags$strong("Dataset path used for the analysis of RnBeads:"),
#                       verbatimTextOutput("data_path"),
#
#
#                       # a div named mydiv
#
#                       tags$div("click to change color" , id="mydiv", style="width: 50px; height :60px;
#                       left: 550px; top: 000px;
#                       background-color: gray; position: absolute"),
#
#                       # javascript code to send data to shiny server
#
#                       tags$script('
#                                   document.getElementById("mydiv").onclick = function() {
#                                   var number = Math.random();
#                                   Shiny.onInputChange("mydata", number);
#                                   };
#                                   '),
#
#                       # handler to receive data from server
#
#
#                       tags$input( id="myinput", style="width: 100px; height :60px;
#                                   left: 150px; top: 100px;
#                                   position: absolute"),
#
#                       tags$script('
#                                   Shiny.addCustomMessageHandler("myperformedmodulesno",
#                                   function(color) {
#                                   document.getElementById("myinput").value = color;
#                                   });
#                                   ')
#
#                       ),# tab panel end
#
#              tabPanel("Analysis Options",
#                       br(), br()
#
#              ),# tab panel end
#
#              tabPanel("Annotation.csv",
#                       br(), br()
#
#
#
#              ),# tab panel end
#
#
#
#
#              tabPanel("qq-Plots",
#                    br(), br(),
#                    radioButtons("qqplots", "QQ-plots:",
#                                 c("summary1_betas_qq", "summary2_betas_qq")),
#
#                    imageOutput("qqimage", width="300px",height="600px")
#
#              ),
#
#              tabPanel("All qq-Plots",
#                       #fluidRow(
#                       #splitLayout(cellWidths = c("60%", "60%"), imageOutput("qq1plot1"), imageOutput("qq1plot2")),
#                       #column(6,imageOutput("qq1plot1", width="300px",height="300px")),
#                       #column(6,imageOutput("qq1plot2", width="300px",height="300px"))
#                       #)
#                       imageOutput("qq1plot1", width="300px",height="600px"),
#                       imageOutput("qq1plot2", width="300px",height="600px")
#
#              )# tab panel end
#                       )# end tabset panel
#
#         )#end of mainbar
#
#
#   )# end of about tab  menu



  )# end of nav bar page



)
