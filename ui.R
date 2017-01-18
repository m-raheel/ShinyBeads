
# libraries to run on the shiny server ( uncomment it on the server)
######################################################################
#library(RnBeadsInterface, lib.loc = '/home/users/mraheel/R/x86_64-pc-linux-gnu-library/3.4')
#.libPaths(.libPaths()[-1])
library(DT)
library(shiny)

library(shinyjs)
library(shinythemes)
library(plyr , lib.loc = '/opt/Rlib/3.4')
library(ggplot2 , lib.loc = '/opt/Rlib/3.4')
library(plotly , lib.loc = '/opt/Rlib/3.4') #interactive graphics with D3
#####################################################################


# local (comment while on the server)
#####################################################################

# library(shiny)
# library(RnBeadsInterface)
# library(DT)
# library(shinyjs)
# library(shinythemes)
# library(plotly) #interactive graphics with D3
# #library(V8) # package for extended shinyJS
# #library(shinyFiles)




choices = "NA"
topRowsChoices = c('100', '500' , '1000', '10000', '50000' , 'ALL')


check_vectors <- c('COMPLETED Loading Data', 'COMPLETED Quality Control', 'COMPLETED Preprocessing', 'COMPLETED Tracks and Tables','COMPLETED Covariate Inference','COMPLETED Exploratory Analysis','COMPLETED Differential Methylation')


shinyUI(



  navbarobject <- navbarPage('idnavBarTop', theme = shinytheme("cerulean"),


  # class="navbar navbar-fixed-bottom"


  footer = tags$div(class="", checked=NA,
                    #tags$p("&copy; 2016 - RnBeads-Interface"),

                    br(),
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

           tags$p(class="pull-right","Timestamp: ", span(id = "time", date()), a(id = "update", "Update")),



           includeCSS("includes/styles.css"),



           #shinythemes::themeSelector(),



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

                                  #actionButton("workingDirButton",label= "Select",class="btn btn-primary"),


                                  paste('<p>',textOutput("ErrorText1"),'</p>'),

                                  paste('<h5>',textOutput("ErrorText2"),'</h5>'),

                                  actionButton("action", label = "Continue", class="btn btn-primary"),

                                  #actionButton("clearDirButton",label= "Clear",class="btn btn-primary"),

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

                                dataTableOutput('table1'),
                                br(),
                                actionButton("view_datasets", label = "View Datasets", class="btn btn-primary"),
                                br()



                       ),

                       tabPanel("DatasetList",
                                br(),
                                tags$strong("List of different data sets used in the analysis:"),
                                tags$p(""),

                                verbatimTextOutput("total_datasets"),
                                br(),
                                dataTableOutput("list_datasets"),
                                br()
                       )


           ),
           br()

  ),


  # individual analysis nav menu

  tabPanel("Individual analysis",

           br(),

           fluidRow(
            column(width = 12,

               sidebarPanel(
                 selectInput("select_ia", "Select analysis folder:", choices)


               ),

               mainPanel(


                 tabsetPanel(id = "analysis_option",

                             tabPanel("RnBeads Reports",

                                      HTML(paste('<div class="">',
                                                 '<div class="container">',

                                                 '<h2>Rnbeads Reports!</h2>',

                                                 '<p>Select from the left menu and click View Reports!</p>',


                                                 #                                                  actionButton("view_rnbeads_reports", label = "Reports", class=""),
                                                 #                                                  br(),
                                                 uiOutput('rnbeadsReports'),


                                                 '</div>',
                                                 '</div>'




                                      )
                                      ),

                                      br()

                             ),
                             tabPanel("Analysis options",

                                      br(),
                                tags$strong("Lisitng all the options of RnBeads from the analysis_options.RData file:"),

                                dataTableOutput("list_options"),
                                br()


                             ),

                             tabPanel("Modules performed",

                                  #tags$strong("List of modules performed are:"),

                                  tableOutput("list_module"),
                                  br()

                             )


                  )#end of tabsetpanel

               )#end of mainpanel
          )),#end of fluid row
          br()

),#end of individual analysis nav menu


# individual analysis nav menu

tabPanel("Individual data set",


                     tabPanel("Dataset",

                              br(),

                              fluidRow(
                                column(width = 12,
                                       selectInput("dd_ids_datasets", "Datasets:", choices),


                                       wellPanel(
                                         tags$strong("Sample Annotations.")
                                       ),

                                       dataTableOutput(paste0('annotation')),
                                       br()
                                )
                              ),


                              fluidRow(

                                column(width = 7,



                                         wellPanel(
                                           tags$strong("Selected dataset is used in following analysis.")
                                         ),

                                         dataTableOutput(paste0('annotation1')),


                                         #h3(span( "Selected Dataset", class="label label-default"), class= "text-info"),
                                         #h3(verbatimTextOutput("h1_datasettab"), class= "text-info"),
                                         #absolutePanel('absolute panel'),
                                         br()

                                       ),

                                column(width = 5,
                                       br(),
                                       plotlyOutput('common_dataset_pie')

                                  )#end of column


                            ),#end of fluid row



                              br()



                     )



),# end of individual dataset nav menu

# individual analysis nav menu

tabPanel("Integrative Visualization",


         tabsetPanel("visualization",
                     tabPanel("QQ-Plots",
                              br(),

                              fluidRow(
                                column(width = 12,
                                       sidebarPanel(
                                         selectInput("input_dmcomp_choices", "Select analysis folder:", choices),
                                         br(),
                                         tags$h3(style="color:black;","Quantile-quantile plots (qq-plots)"),
                                         tags$div(id = "iv_div_info", class="text text-info", checked=NA,

                                                  tags$p(paste("Quantile-quantile plots (qq-plots) can be useful for verifying that a set of values",
                                                               "come from a certain distribution.")
                                                  ),

                                                  br(),
                                                  tags$p(paste("For example in a genome-wide association study,",
                                                               "we expect that most of the SNPs we are testing not to be associated with the disease.",
                                                               "Under the null, this means that the p-values we get from tests where no true",
                                                               "association exists should follow a uniform(0,1) distribution. Since we're usually most",
                                                               "interested in really small p-values, we generally transform the p-values by -log10 so",
                                                               "that the smallest values near zero become the larger values and are thus easier to see.")
                                                  ),
                                                  br()
                                         )

                                       ),


                                       mainPanel(
                                         tabsetPanel("sub_visuallization",

                                                     tabPanel("QQ-Plot",

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

                                                              actionButton('displayQQPlotBtn', 'Display',class="btn btn-primary btn-md"),



                                                              div(id="id_qqplot",
                                                                  fluidRow(
                                                                    column(width = 8,
                                                                           plotlyOutput('compqqplotly')
                                                                           #plotOutput('compqqplot')
                                                                    ))# end  of  fluid row

                                                              ),

                                                              br()


                                                     ),

                                                     tabPanel("QQ-Plot Comparison",

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



                                                              actionButton('displayBtn', 'Display',class="btn btn-primary btn-md"),
                                                              plotOutput('multicompqqplot'),
                                                              br()


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


                                       )# end  of  main panel

                                ))# end  of  fluid row



                     ),

                     tabPanel("Table Browser",

                              tags$h2(style="color:black;","Table Browser"),
                              tags$div(id = "tb_div_info", checked=NA,

                                       tags$p(paste("Table Browser is useful for filtering and sorting of the differential methylation comparison data, Select the RnBeads analysis and then you can filter the table with all the columns and you can download the results.",
                                                    "Also you can upload external files having at least a target column whoes values are like cgxxxxxxx and the table will get filtered."
                                                    )
                                              )# end p tag

                              ),





                              fluidRow(
                                column(width = 4, offset = 0, style='padding-top:0px;',

                                       div(class="well",
                                         selectInput("input_tablebrowser_choices", "Select analysis folder:", choices),
                                         selectInput("input_tablebrowser_files", "Select comparison:", ""),
                                         selectInput("input_tablebrowser_readtop", "Read top n rows:", topRowsChoices)

                                       ),

                                       div(class="well",

                                        tags$h3(style="color:black;",paste("Filter with external files",
                                                                          "e.g 450K annotation etc")),




                                           fileInput('file1', 'Choose file to filter the table on the right',
                                                     accept = c(
                                                       'text/csv',
                                                       'text/comma-separated-values',
                                                       'text/tab-separated-values',
                                                       'text/plain',
                                                       '.csv',
                                                       '.tsv'
                                                     )
                                           ),
                                           tags$hr(),
                                           checkboxInput('header', 'Header', TRUE),
                                           radioButtons('sep', 'Separator',
                                                        c(Comma=',',
                                                          Semicolon=';',
                                                          Tab='\t'),
                                                        ','),

                                           tags$hr()

                                           #dataTableOutput('p_values'),



                                      )


                                      # div(class="well",
                                      #

                                      #
                                      # ),
                                      # br()



                                ), #end of column


                                column(width = 8,

                                       div(class="",

                                         tags$p("The table below lists the data from the selected analysis."),


                                         actionButton('displayTableBrowserBtn', 'Display',class="btn btn-primary btn-md"),


                                         br(),
                                         br(),

                                         dataTableOutput('output.comparison.file'),
                                         br(),
                                         br(),

                                         div(id="id_tb_filterPlot_Btn",
                                             tags$h2(style="color:black;","Filtered Plot"),
                                             tags$p("Customize the plot by selecting x-axis and y-axis from the options. The plot will contains data from the above table."),

                                             fluidRow(
                                               column(width = 4,
                                                       selectInput("input_tablebrowser_x_axis", "Select x-axis:", '')
                                               ),
                                               column(width = 4,
                                                       selectInput("input_tablebrowser_y_axis", "Select y-axis:", '')
                                               )
                                             ),

                                           actionButton('displayPlotBtn', 'Display Plot', class="btn btn-primary btn-md"),
                                           div(id="id_tb_filterPlot",

                                               br(),
                                               br(),
                                               plotlyOutput('x5')
                                           )
                                           # actionButton('displaySimplePlotBtn', 'Simple Plot'),
                                           # div(id="id_tb_simplefilterPlot",
                                           #
                                           #     br(),
                                           #     br(),
                                           #     plotOutput('simpleTBPlot')
                                           # )
                                         )




                                       ),
                                       br()


                                )# end of column


                              )# end  of  fluid row




                     ), # end of tab set panel of table browser

                     tabPanel("Top-scorer E.A.",

                              tags$h2(style="color:black;","Top-scorer"),
                              tags$div(id = "ts_div_info", checked=NA,

                                       tags$p(paste("User can select multiple RnBeads analysis and filter the data based on top scores and see the overlappings in your seleted analysis in the form of Venn Diagram and also view the overlapping CpGs in table form.",
                                                    ""
                                       )
                                       )# end p tag

                              ),


                              fluidRow(

                                column(width = 4,


                                       tags$div(id = "ts_div_info", class="well",
                                                tags$h4(style="color:black;","Analysis"),

                                                uiOutput("cb"),
                                                tags$h4(style="color:black;","Comparisons"),

                                                uiOutput("si")



                                       )




                                ),
                                column(width = 8,

                                       tags$h4(style="color:black;","Multiple analysis Venn Diagram"),
                                       tags$p(paste("Check the analysis on the left for which you want to see the overlapping. (Top 100 rows)")),
                                       tags$p(paste("Note: Please select atleast 1 and atmost 6 analysis to draw Venn Diagram!")),


                                       #checkboxGroupInput("cb_ts_comp_venn", label = h3("Select analysis"),
                                       #                   choices = list("")),


                                       fluidRow(
                                         column(width = 5,

                                                uiOutput("ts.columns")

                                                ),
                                         column(width = 2,

                                                uiOutput("ts.columns.equality")



                                         ),

                                         column(width = 5,

                                                uiOutput("ts.columns.range")

                                                )

                                       ),# end  of  fluid row





                                       actionButton('btnMultipleShowVenn', 'Display',class="btn btn-primary btn-md"),
                                       br(),


                                       div(class="",



                                           plotOutput('output.ts.multivenn.plot'),
                                           br()

                                       ),



                                       tags$h4(style="color:black;","Overlapping CpGs"),
                                       tags$p(paste("")),


                                       actionButton('displayTopScorerOverlappingBtn', 'Display',class="btn btn-primary btn-md"),
                                       br(),
                                       br(),

                                       div(class="",


                                           dataTableOutput('output.topscorer.overlappingComparison'),
                                           br()

                                       ),
                                       br()


                                )),# end  of  fluid row



                              # showing the overlapping data Cpgs
                              fluidRow(

                                column(width = 12




                                )# end of column



                              ),# end  of  fluid row


                              # showing merge tabe after merge button is pressed
                              fluidRow(

                                column(width = 12

#                                        tags$h4(style="color:black;","Merge analysis data"),
#                                        tags$p(paste("Merge the above selected analysis data.")),
#
#
#                                        actionButton('displayTopScorerMergeBtn', 'Merge',class="btn btn-primary btn-md"),
#                                        br(),
#                                        br(),
#
#                                        div(class="well",
#
#
#                                            dataTableOutput('output.topscorer.mergedComparison'),
#                                            br()
#
#                                        ),
#                                        br()


                                )# end of column



                              ),# end  of  fluid row




                              fluidRow(
                                column(width = 12



                                         # #plotlyOutput("p_plotly"),
                                         #
                                         # radioButtons("plotType", "Plot Type:", choices = c("ggplotly", "plotly")),
                                         # plotlyOutput("plot"),
                                         # verbatimTextOutput("hover"),
                                         # verbatimTextOutput("click"),
                                         # verbatimTextOutput("brush"),
                                         # verbatimTextOutput("zoom"),
                                         #
                                         # br(),
                                         #
                                         #
                                         # title = 'Select Table Rows',
                                         #
                                         # h1('A Client-side Table'),
                                         #
                                         # fluidRow(
                                         #   column(6, DT::dataTableOutput('x1')),
                                         #   column(6, plotlyOutput('x2', height = 500))
                                         # ),
                                         #
                                         # hr(),
                                         #
                                         # h1('A Server-side Table'),
                                         #
                                         # fluidRow(
                                         #   column(9, DT::dataTableOutput('x3')),
                                         #   column(3, verbatimTextOutput('x4'))
                                         # )






                                ))# end  of  fluid row

                        )# end of tabpanel of top scorer


         ),# end of tabsetpanel("visualization")

         br()

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
