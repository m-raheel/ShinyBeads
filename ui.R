
########################################################################################################################
## ui.R
## created: 2016-09-01
## creator: Muhammad Raheel
## ---------------------------------------------------------------------------------------------------------------------
## Main User Interface of the RnBeadsInterface tool.
########################################################################################################################


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
library(shinydashboard, lib.loc = '/home/users/mraheel/R/x86_64-pc-linux-gnu-library/3.4')
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
topRowsChoices = c('100', '500' , '1000', '10000', '50000' , '-1')


check_vectors <- c('COMPLETED Loading Data', 'COMPLETED Quality Control', 'COMPLETED Preprocessing', 'COMPLETED Tracks and Tables','COMPLETED Covariate Inference','COMPLETED Exploratory Analysis','COMPLETED Differential Methylation')


# shinyUI(
#
#
#
#   navbarobject <- navbarPage('idnavBarTop', theme = shinytheme("cerulean"),
#
#   footer = tags$div(class="", checked=NA,
#                     #tags$p("&copy; 2016 - RnBeads-Interface"),
#
#                     br(),
#                     htmlTemplate("footer.html")
#   ),
#
#
#   #title= div(id= "id_div_title" ,tags$a(href = '.', tags$img(src = 'RnBeads.png', width = 100, height = 20)),"RnBeads Interface"),
#   title= "RnBeads-Interface",
#   #title= tags$a(href = '.', tags$p('RnBeads-Interface', class = 'text-danger')),
#
#
#
#   # Home nav menu
#   tabPanel("Home",
#
#            shinyjs::useShinyjs(),
#
#            #tags$style(type="text/css", "body {padding-top: 70px;}"),
#
#            tags$p(class="pull-right","Timestamp: ", span(id = "time", date()), a(id = "update", "Update")),
#
#            includeCSS("includes/styles.css"),
#
#            #shinythemes::themeSelector(),
#
#            HTML(paste('<div class="jumbotron">',
#                         '<div class="container">',
#
#                             '<h2>Rnbeads-Interface!</h2>',
#                             '<p>It is a tool to provide user friendly interactive interface for RnBeads generated reports. It allows to keep track of the analysis performed and prevent performing same analysis again and again. It makes it interactive and easier to compare same or different RnBeads analysis. Target users are the ones who uses RnBeads for analyzing DNA methylation data either individually or as a group.</p>',
#
#                         '</div>',
#                       '</div>',
#
#                       '<div class="container">',
#                           '<!-- Example row of columns -->',
#                           '<div class="row">',
#                               '<div class="col-md-6">',
#                                 '<h2>Working Repository</h2>',
#
#                                   paste('<p>',textOutput("ErrorText1"),'</p>'),
#
#                                   paste('<h5>',textOutput("ErrorText2"),'</h5>'),
#
#                                   actionButton("action", label = "Continue", class="btn btn-primary"),
#
#                                   #actionButton("clearDirButton",label= "Clear",class="btn btn-primary"),
#
#                                   br(),
#                               '</div>',
#                               '<div class="col-md-3">',
#
#                               '<h2>RnBeads</h2>',
#
#                               '</div>',
#                               '<div class="col-md-3">',
#                                 br(),
#                                 tags$a(class='pull-right', href = '.', tags$img(src = 'RnBeads.png')),
#
#                               '</div>',
#                               '<div class="col-md-6">',
#
#
#
#                                     '<p>RnBeads is an R package for comprehensive analysis of DNA methylation data obtained with any experimental protocol that provides single-CpG resolution. </p>',
#                                     '<p><a class="btn btn-primary btn-md" href="http://rnbeads.mpi-inf.mpg.de/" target = "blank" role="button">Learn more &raquo;</a></p>',
#
#
#
#
#
#
#                               '</div>',
#
#
#                           '</div>',
#                       '</div>',
#
#                       '<br/>')),
#
#
#            #includeHTML("index.html"),
#
#
#
#
#            # commented is the script to change the tab
#
#            tags$head(tags$script('
#                                  Shiny.addCustomMessageHandler("myCallbackHandler",
#                                  function(typeMessage) {
#                                      console.log(typeMessage)
#                                      if(typeMessage == 1){
#
#                                         $("a:contains(Repository)").click();
#                                      }
#                                      if(typeMessage == 2){
#                                         $("a:contains(Individual data set)").click();
#                                      }
#                                      if(typeMessage == 3){
#                                         $("a:contains(Integrative Visualization)").click();
#                                      }
#                                      if(typeMessage == 4){
#                                         $("a:contains(DatasetList)").click();
#                                      }
#                                  });
#                                  ')
#            )
#
#   ),
#
#
#   ########################################################################################################################
#   ##
#   ## Top Nav Bar Tab : Repository
#   ## ---------------------------------------------------------------------------------------------------------------------
#   ## User Interface components of Repository tab
#   ########################################################################################################################
#
#
#   # Repository nav menu
#   tabPanel("Repository",
#
#            tabsetPanel(id = "repository",
#
#
#                        tabPanel("AnalysisList",
#                                 br(),
#                                 verbatimTextOutput("count_rfolders"),
#
#                                 tags$strong("Below are the list of analysis directories created by RnBeads"),
#
#                                 br(),
#                                 br(),
#                                 dataTableOutput("list_folders"),
#
#                                 dataTableOutput('table1'),
#                                 br(),
#                                 actionButton("view_datasets", label = "View Datasets", class="btn btn-primary"),
#                                 br()
#
#
#
#                        ),
#
#                        tabPanel("DatasetList",
#                                 br(),
#                                 tags$strong("List of different data sets used in the analysis:"),
#                                 tags$p(""),
#
#                                 verbatimTextOutput("total_datasets"),
#                                 br(),
#                                 dataTableOutput("list_datasets"),
#                                 br()
#                        )
#
#
#            ),
#            br()
#
#   ),
#
#
#   ########################################################################################################################
#   ##
#   ## Top Nav Bar Tab : Individual Analysis
#   ## ---------------------------------------------------------------------------------------------------------------------
#   ## User Interface components of Individual Analysis tab
#   ########################################################################################################################
#
#
#   tabPanel("Individual analysis",
#
#            br(),
#
#            fluidRow(
#             column(width = 12,
#
#                sidebarPanel(
#                  selectInput("select_ia", "Select analysis folder:", choices)
#
#
#                ),
#
#                mainPanel(
#
#
#                  tabsetPanel(id = "analysis_option",
#
#                              tabPanel("RnBeads Reports",
#
#                                       HTML(paste('<div class="">',
#                                                  '<div class="container">',
#
#                                                  '<h2>Rnbeads Reports!</h2>',
#
#                                                  '<p>Select from the left menu and click View Reports!</p>',
#
#
#                                                  #                                                  actionButton("view_rnbeads_reports", label = "Reports", class=""),
#                                                  #                                                  br(),
#                                                  uiOutput('rnbeadsReports'),
#
#
#                                                  '</div>',
#                                                  '</div>'
#
#
#
#
#                                       )
#                                       ),
#
#                                       br()
#
#                              ),
#                              tabPanel("Analysis options",
#
#                                       br(),
#                                 tags$strong("Lisitng all the options of RnBeads from the analysis_options.RData file:"),
#
#                                 dataTableOutput("list_options"),
#                                 br()
#
#
#                              ),
#
#                              tabPanel("Modules performed",
#
#                                   #tags$strong("List of modules performed are:"),
#
#                                   tableOutput("list_module"),
#                                   br()
#
#                              )
#
#
#                   )#end of tabsetpanel
#
#                )#end of mainpanel
#           )),#end of fluid row
#           br()
#
#   ),#end of individual analysis nav menu
#
#
#   ########################################################################################################################
#   ##
#   ## Top Nav Bar Tab : Individual data set
#   ## ---------------------------------------------------------------------------------------------------------------------
#   ## User Interface components of Individual data set tab
#   ########################################################################################################################
#
#
#   tabPanel("Individual data set",
#
#
#                      tabPanel("Dataset",
#
#                               br(),
#
#                               fluidRow(
#                                 column(width = 12,
#                                        selectInput("dd_ids_datasets", "Datasets:", choices),
#
#
#                                        wellPanel(
#                                          tags$strong("Sample Annotations.")
#                                        ),
#
#                                        dataTableOutput(paste0('annotation')),
#                                        br()
#                                 )
#                               ),
#
#
#                               fluidRow(
#
#                                 column(width = 7,
#
#
#
#                                          wellPanel(
#                                            tags$strong("Selected dataset is used in following analysis.")
#                                          ),
#
#                                          dataTableOutput(paste0('annotation1')),
#
#
#                                          #h3(span( "Selected Dataset", class="label label-default"), class= "text-info"),
#                                          #h3(verbatimTextOutput("h1_datasettab"), class= "text-info"),
#                                          #absolutePanel('absolute panel'),
#                                          br()
#
#                                        ),
#
#                                 column(width = 5,
#                                        br(),
#                                        plotlyOutput('common_dataset_pie')
#
#                                   )#end of column
#
#
#                             ),#end of fluid row
#
#
#
#                               br()
#
#
#
#                      )
#
#
#
#   ),# end of individual dataset nav menu
#
#   ########################################################################################################################
#   ##
#   ## Top Nav Bar Tab : Integrative Visualization
#   ## ---------------------------------------------------------------------------------------------------------------------
#   ## User Interface components of Integrative Visualization tab
#   ########################################################################################################################
#
#
#   tabPanel("Integrative Visualization",
#
#
#          tabsetPanel("visualization",
#
#
#                      tabPanel("QQ-Plots",
#
#                               tags$h2(style="color:black;","QQ-Plots"),
#                                tags$div(id = "qq_div_info", class="", checked=NA,
#
#                                         tags$p(paste("Quantile-quantile plots (qq-plots) can be useful for verifying that a set of values",
#                                                      "come from a certain distribution.")
#                                         ),
#
#
#                                         tags$p(paste("For example in a genome-wide association study,",
#                                                      "we expect that most of the SNPs we are testing not to be associated with the disease.",
#                                                      "Under the null, this means that the p-values we get from tests where no true",
#                                                      "association exists should follow a uniform(0,1) distribution. Since we're usually most",
#                                                      "interested in really small p-values, we generally transform the p-values by -log10 so",
#                                                      "that the smallest values near zero become the larger values and are thus easier to see.")
#                                         )
#
#                                ),
#
#                               fluidRow(
#                                 column(width = 12,
#
#                                          tabsetPanel("sub_visuallization",
#
#
#                                                      tabPanel("QQ-Plot",
#
#                                                               fluidRow(
#                                                                 column(width = 4,
#                                                                        br(),
#                                                                        div(class = "well",
#                                                                            selectInput("input_dmcomp_choices", "Select analysis folder:", choices),
#                                                                            selectInput("input_dmcomp_files", "comparisons:", ""),
#                                                                            selectInput("input_qqplot_readtop", "Read top n rows:", topRowsChoices),
#
#                                                                            br()
#                                                                            #tags$h3(style="color:black;","Quantile-quantile plots (qq-plots)")
#                                                                         )
#
#                                                                 ),
#
#                                                                 column(width = 8,
#                                                                        br(),
#
#                                                                        # shinyjs::hidden(
#                                                                        #   div(id = "hide_input",
#                                                                        #       selectInput("input_dmcomp_files_index", "comparisons:", "")
#                                                                        #
#                                                                        #   )
#                                                                        # ),
#
#
#                                                                        tags$p("Select the analysis and the comparison from the left menu and click display:"),
#                                                                        # radioButtons("dist", "Distribution type:",
#                                                                        #              c(
#                                                                        #                "Uniform" = "unif",
#                                                                        #                "Normal" = "norm")
#                                                                        #                # "Log-normal" = "lnorm",
#                                                                        #                # "Exponential" = "exp")
#                                                                        #                ),
#
#                                                                        actionButton('displayQQPlotBtn', 'Display',class="btn btn-primary btn-md"),
#
#
#
#                                                                        div(id="id_qqplot",
#                                                                            fluidRow(
#                                                                              column(width = 8,
#                                                                                     plotlyOutput('compqqplotly')
#                                                                                     #plotOutput('compqqplot')
#                                                                              ))# end  of  fluid row
#
#                                                                        ),
#
#                                                                        br()
#
#                                                                 )
#                                                               )
#                                                      ),
#
#                                                      tabPanel("Multi-analysis QQ-Plot ",
#
#                                                               fluidRow(
#                                                                 column(width = 4,
#                                                                        br(),
#                                                                        div(class = "well",
#                                                                            h5("Select comparison among two RnBeads analysis"),
#
#                                                                            selectInput("input_dmcomp_choices_1", "Analysis 1:", choices),
#                                                                            selectInput("input_dmcomp_files_1", "Comaprisons 1:", ""),
#                                                                            br(),
#
#                                                                            selectInput("input_dmcomp_choices_2", "Analysis 2:", choices),
#                                                                            selectInput("input_dmcomp_files_2", "Comparisons 2:", ""),
#                                                                            br()
#
#
#                                                                        )
#                                                                 ),
#                                                                 column(width = 8,
#                                                                        br(),
#                                                                        selectInput("input_multiqqplot_readtop", "Read top n rows:", topRowsChoices),
#
#                                                                        actionButton('displayBtn', 'Display',class="btn btn-primary btn-md"),
#                                                                        br(),
#                                                                        fluidRow(
#                                                                          column(width = 6,
#                                                                                 tags$h4(style="color:black;","Analysis 1"),
#                                                                                 plotlyOutput('multicompqqplot1')
#
#                                                                          ),
#                                                                          column(width = 6,
#                                                                                 tags$h4(style="color:black;","Analysis 2"),
#                                                                                 plotlyOutput('multicompqqplot2')
#
#                                                                          )
#                                                                         ),
#                                                                        br(),
#                                                                        tags$h4(style="color:black;","Combined QQ plot"),
#                                                                        plotOutput('multicompqqplot'),
#                                                                        br()
#
#                                                                 )
#                                                               )# end of fluid row
#
#
#
#                                                      )# end of tab panel
#
#
#                                          )# tab set panel
#
#
#
#
#                                 ))# end  of  fluid row
#
#
#
#                      ),
#
#                      tabPanel("Table Browser",
#
#                               tags$h2(style="color:black;","Table Browser"),
#                               tags$div(id = "tb_div_info", checked=NA,
#
#                                        tags$p(paste("Table Browser is useful for filtering and sorting of the differential methylation comparison data, Select the RnBeads analysis and then you can filter the table with all the columns and you can download the results.",
#                                                     "Also you can upload external files having at least a target column whoes values are like cgxxxxxxx and the table will get filtered."
#                                                     )
#                                               )# end p tag
#
#                               ),
#
#
#
#
#
#                               fluidRow(
#                                 column(width = 4, offset = 0, style='padding-top:0px;',
#
#                                        div(class="well",
#                                          selectInput("input_tablebrowser_choices", "Select analysis folder:", choices),
#                                          selectInput("input_tablebrowser_files", "Select comparison:", ""),
#                                          selectInput("input_tablebrowser_readtop", "Read top n rows:", topRowsChoices)
#
#                                        ),
#
#                                        div(class="well",
#
#                                         tags$h3(style="color:black;",paste("Filter with external files",
#                                                                           "e.g 450K annotation etc")),
#
#
#
#
#                                            fileInput('file1', 'Choose file to filter the table on the right',
#                                                      accept = c(
#                                                        'text/csv',
#                                                        'text/comma-separated-values',
#                                                        'text/tab-separated-values',
#                                                        'text/plain',
#                                                        '.csv',
#                                                        '.tsv'
#                                                      )
#                                            ),
#                                            tags$hr(),
#                                            checkboxInput('header', 'Header', TRUE),
#                                            radioButtons('sep', 'Separator',
#                                                         c(Comma=',',
#                                                           Semicolon=';',
#                                                           Tab='\t'),
#                                                         ','),
#
#                                            tags$hr()
#
#                                            #dataTableOutput('p_values'),
#
#
#
#                                       )
#
#                                 ), #end of column
#
#
#                                 column(width = 8,
#
#                                        div(class="",
#
#                                          tags$p("The table below lists the data from the selected analysis."),
#
#
#                                          actionButton('displayTableBrowserBtn', 'Display',class="btn btn-primary btn-md"),
#
#
#                                          br(),
#                                          br(),
#
#                                          dataTableOutput('output.comparison.file'),
#                                          br(),
#                                          br(),
#
#                                          div(id="id_tb_filterPlot_Btn",
#                                              tags$h2(style="color:black;","Filtered Plot"),
#                                              tags$p("Customize the plot by selecting x-axis and y-axis from the options. The plot will contains data from the above table."),
#
#                                              fluidRow(
#                                                column(width = 4,
#                                                        selectInput("input_tablebrowser_x_axis", "Select x-axis:", '')
#                                                ),
#                                                column(width = 4,
#                                                        selectInput("input_tablebrowser_y_axis", "Select y-axis:", '')
#                                                )
#                                              ),
#
#                                            actionButton('displayPlotBtn', 'Display Plot', class="btn btn-primary btn-md"),
#                                            div(id="id_tb_filterPlot",
#
#                                                br(),
#                                                br(),
#                                                plotlyOutput('x5')
#                                            )
#
#                                          )
#                                        ),
#                                        br()
#
#
#                                 )# end of column
#
#
#                               )# end  of  fluid row
#
#
#
#
#                      ), # end of tab set panel of table browser
#
#                      tabPanel("Top-scorer E.A.",
#
#                               tags$h2(style="color:black;","Top-scorer"),
#                               tags$div(id = "ts_div_info", checked=NA,
#
#                                        tags$p(paste("User can select multiple RnBeads analysis and filter the data based on top scores and see the overlappings in your seleted analysis in the form of Venn Diagram and also view the overlapping CpGs in table form.",
#                                                     ""
#                                        )
#                                        )# end p tag
#
#                               ),
#
#
#                               fluidRow(
#
#                                 column(width = 4,
#
#
#                                        tags$div(id = "ts_div_info", class="well",
#                                                 tags$h4(style="color:black;","Analysis"),
#
#                                                 uiOutput("cb"),
#                                                 tags$h4(style="color:black;","Comparisons"),
#
#                                                 uiOutput("si")
#
#
#
#                                        )
#
#
#
#
#                                 ),
#                                 column(width = 8,
#
#                                        tags$h4(style="color:black;","Multiple analysis Venn Diagram"),
#                                        tags$p(paste("Check the analysis on the left for which you want to see the overlapping. (Top 100 rows)")),
#                                        tags$p(paste("Note: Please select atleast 1 and atmost 6 analysis to draw Venn Diagram!")),
#
#
#                                        #checkboxGroupInput("cb_ts_comp_venn", label = h3("Select analysis"),
#                                        #                   choices = list("")),
#
#
#                                        fluidRow(
#                                          column(width = 5,
#
#                                                 uiOutput("ts.columns")
#
#                                                 ),
#                                          column(width = 2,
#
#                                                 uiOutput("ts.columns.equality")
#
#
#
#                                          ),
#
#                                          column(width = 5,
#
#                                                 uiOutput("ts.columns.range")
#
#                                                 )
#
#                                        ),# end  of  fluid row
#
#
#
#
#
#                                        actionButton('btnMultipleShowVenn', 'Display',class="btn btn-primary btn-md"),
#                                        br(),
#
#
#                                        div(class="",
#
#                                            #tags$p(textOutput("comparison.check")),
#                                            tags$p(textOutput("ts.venn.overlapping.error.value"  )),
#                                            plotOutput('output.ts.multivenn.plot'),
#                                            tableOutput('output.ts.table.multivenn.plot.labels'),
#                                            br()
#
#                                        ),
#
#
#
#                                        tags$h4(style="color:black;","Overlapping CpGs"),
#                                        tags$p(paste("")),
#
#                                        div(class="",
#
#
#                                            uiOutput("ts.selector.overlapping.value" ),
#                                            br()
#
#                                        ),
#
#                                        div(class="",
#
#
#                                            dataTableOutput('output.topscorer.overlappingComparison'),
#                                            br()
#
#                                        ),
#                                        br()
#
#
#                                 ))# end  of  fluid row
#
#
#
#
#                         )# end of tabpanel of top scorer
#
#
#          ),# end of tabsetpanel("visualization")
#
#          br()
#
#   )#end of integrative visualization nav menu
#
#   )# end of nav bar page
#
#
#
# )



########################################################################################################################
##
## Dashboard header
## ---------------------------------------------------------------------------------------------------------------------
## User Interface components of dashboard header
########################################################################################################################

header <- dashboardHeader(

  #title = tags$a(class='pull-left', href = '.', tags$img(src = 'RnBeads.png'), style = "background-color: 'white';")
  title = "RnShinyBeads"



)

########################################################################################################################
##
## Dashboard sidebar
## ---------------------------------------------------------------------------------------------------------------------
## User Interface components of dashboard sidebar
########################################################################################################################

sidebar <- dashboardSidebar(
  hr(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$style(HTML("
                      .sidebar { position: fixed;}
                      " )
    )

  ),
  sidebarMenu(id="tabs",
              menuItem("Home", tabName="home", icon=icon("home"), selected=TRUE),
              menuItem("Analysis", tabName="analysis", icon=icon("file-text-o")),
              menuItem("Dataset", tabName="dataset", icon=icon("database")),
              menuItem("QQplots", tabName="qqplots", icon=icon("line-chart")),
              menuItem("Table Browser", tabName="tablebrowser", icon=icon("table")),
              menuItem("Top Scorer", tabName="topscorer", icon=icon("table")),


              menuItem("About", tabName = "about", icon = icon("question"))
  ),
  hr(),
  br(),
  br(),
  conditionalPanel("input.tabs=='plot'",
                   fluidRow(
                     column(1),
                     column(10

                     )
                   )
  )
)

########################################################################################################################
##
## Dashboard Body
## ---------------------------------------------------------------------------------------------------------------------
## User Interface components of dashboard body
########################################################################################################################



body <- dashboardBody(

  tags$head(
#     tags$style(HTML("
#                       .sidebar { height: 90vh; overflow-y: auto; }
#                       " )
#     ),
    tags$style(HTML("
                      .customtabbox { height: 30vh; overflow-y: auto; }
                      " )
    ),
    tags$style(HTML("
                      .customtabboxcomparison { height: 65vh; overflow-y: auto; }
                      " )
    )
  ),

  tabItems(
    tabItem(tabName = "readme"

            #includeMarkdown("readMe.Rmd")
    ),

    ########################################################################################################################
    ##
    ## Tab Item : home
    ##
    ########################################################################################################################

    tabItem(tabName = "home",
            fluidRow(

              column(width = 12,
                     box(  width = NULL, tabBox( width = NULL,
                                                 tabPanel("AnalysisList",
                                                          br(),
                                                          verbatimTextOutput("count_rfolders"),

                                                          tags$strong("Below are the list of analysis generated by RnBeads"),

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
                     ), collapsible = TRUE,
                           title = "Overview", status = "primary", solidHeader = TRUE)
              ))
    ),

    ########################################################################################################################
    ##
    ## Tab Item : analysis
    ##
    ########################################################################################################################

    tabItem(tabName = "analysis",
            fluidRow(

              column(width = 12,

                     box(  width = NULL, selectInput("select_ia", "Select analysis folder:", choices),

                                          tabBox( width = NULL,
                                                 tabPanel("RnBeads Reports",

                                                  HTML(paste('<div class="">',
                                                             '<div class="container">',

                                                             '<h2>Rnbeads Reports!</h2>',

                                                             '<p>Select analysis from the above menu and click View Reports!</p>',


                                                             #                                                  actionButton("view_rnbeads_reports", label = "Reports", class=""),
                                                             #                                                  br(),
                                                             uiOutput('rnbeadsReports'),


                                                             '</div>',
                                                             '</div>'




                                                  )),

                                                  HTML(paste(

                                                             '<div class="container">',
                                                             '<!-- Example row of columns -->',
                                                             '<div class="row">',

                                                             br(),
                                                             br(),
                                                             '<div class="col-md-12">',
                                                             br(),
                                                             tags$a(class='pull-left', href = '.', tags$img(src = 'RnBeads.png')),

                                                             '</div>',
                                                             '<div class="col-md-12">',



                                                             '<p>RnBeads is an R package for comprehensive analysis of DNA methylation data obtained with any experimental protocol that provides single-CpG resolution. </p>',
                                                             '<p><a class="btn btn-primary btn-md" href="http://rnbeads.mpi-inf.mpg.de/" target = "blank" role="button">Learn more &raquo;</a></p>',






                                                             '</div>',


                                                             '</div>',
                                                             '</div>',

                                                             '<br/>')),


                                                  br()

                         ),
                         tabPanel("Analysis options",

                                  br(),
                                  tags$strong("RnBeads options"),
                                  br(),
                                  br(),
                                  dataTableOutput("list_options"),
                                  br()


                         ),

                         tabPanel("Modules performed",

                                  #tags$strong("List of modules performed are:"),

                                  tableOutput("list_module"),
                                  br()

                         )
                     ),
                     collapsible = TRUE,
                           title = "Analysis", status = "primary", solidHeader = TRUE)
              ))
    ),

    ########################################################################################################################
    ##
    ## Tab Item : dataset
    ##
    ########################################################################################################################

    tabItem(tabName = "dataset",
            fluidRow(

              column(width = 12,

                     box(  width = NULL,
                           selectInput("dd_ids_datasets", "Datasets:", choices),
                           tabBox( width = NULL,

                                    tabPanel("dataset",

                                          tags$strong("Selected dataset is used in following analysis."),
                                          br(),
                                          br(),
                                          dataTableOutput(paste0('annotation1')),

                                          br(),
                                          plotlyOutput('common_dataset_pie'),
                                          br()
                                  ),
                                  tabPanel("annotation file",
                                           br(),
                                           tags$strong("Sample Annotations."),


                                           dataTableOutput(paste0('annotation')),
                                           br()
                                  )


                           ),
                           collapsible = TRUE,
                           title = "Dataset Information", status = "primary", solidHeader = TRUE)
              ))
    ),

    ########################################################################################################################
    ##
    ## Tab Item : QQ Plots
    ##
    ########################################################################################################################

    tabItem(tabName = "qqplots",
            fluidRow(

              column(width = 12,


                     box(  width = NULL,
                           tags$h2(style="color:black;","QQ-Plots"),

                           tags$p(paste("Quantile-quantile plots (qq-plots) can be useful for verifying that a set of values",
                                        "come from a certain distribution.")
                           ),


                           tags$p(paste("For example in a genome-wide association study,",
                                        "we expect that most of the SNPs we are testing not to be associated with the disease.",
                                        "Under the null, this means that the p-values we get from tests where no true",
                                        "association exists should follow a uniform(0,1) distribution. Since we're usually most",
                                        "interested in really small p-values, we generally transform the p-values by -log10 so",
                                        "that the smallest values near zero become the larger values and are thus easier to see.")
                           ),



                           tags$p("Select the analysis and the differential methylation comparison generated by Rnbeads from the menu below and click display:"),
                           tabBox(  width = NULL,
                                    tabPanel("QQ-Plot",
                                             br(),

                                             selectInput("input_dmcomp_choices", "Select analysis folder:", choices),
                                             selectInput("input_dmcomp_files", "comparisons:", ""),
                                             selectInput("input_qqplot_readtop", "Read top n rows:", topRowsChoices),



                                             actionButton('displayQQPlotBtn', 'Display',class="btn btn-primary btn-md"),
                                             br(),
                                             br(),


                                             div(id="id_qqplot",
                                                 fluidRow(
                                                   column(width = 8,
                                                          plotlyOutput('compqqplotly')
                                                          #plotOutput('compqqplot')
                                                   ))# end  of  fluid row

                                             ),

                                             br()
                                    ),
                                    tabPanel("Multi-analysis QQ-Plot",


                                             br(),

                                             h5("Select comparison among two RnBeads analysis"),

                                             selectInput("input_dmcomp_choices_1", "Analysis 1:", choices),
                                             selectInput("input_dmcomp_files_1", "Comaprisons 1:", ""),
                                             br(),

                                             selectInput("input_dmcomp_choices_2", "Analysis 2:", choices),
                                             selectInput("input_dmcomp_files_2", "Comparisons 2:", ""),


                                             br(),
                                             selectInput("input_multiqqplot_readtop", "Read top n rows:", topRowsChoices),




                                             actionButton('displayBtn', 'Display',class="btn btn-primary btn-md"),
                                             br(),
                                             br(),

                                             fluidRow(
                                               column(width = 6,
                                                      tags$h4(style="color:black;","Analysis 1"),
                                                      plotlyOutput('multicompqqplot1')

                                               ),
                                               column(width = 6,
                                                      tags$h4(style="color:black;","Analysis 2"),
                                                      plotlyOutput('multicompqqplot2')

                                               )
                                             ),
                                             br(),
                                             tags$h4(style="color:black;","Combined QQ plot"),
                                             plotOutput('multicompqqplot'),
                                             br()


                                    )# end of tab panel



                           ),# end of tab box


                           collapsible = TRUE,
                           title = "QQPlots", status = "primary", solidHeader = TRUE)
              )
             )
    ),

    ########################################################################################################################
    ##
    ## Tab Item : Table Browser
    ##
    ########################################################################################################################


    tabItem(tabName = "tablebrowser",
            fluidRow(

              column(width = 12,
                     box(  width = NULL,

                           tags$h2(style="color:black;","Table Browser"),
                           tags$div(id = "tb_div_info", checked=NA,

                                    tags$p(paste("Table Browser is useful for filtering and sorting of the differential methylation comparison data, Select the RnBeads analysis and then you can filter the table with all the columns and you can download the results.",
                                                 "Also you can upload external files having at least a target column whoes values are like cgxxxxxxx and the table will get filtered."
                                    )
                                    )# end p tag

                           ),

                           tabBox(  width = NULL,
                                    tabPanel("analysis",
                                             br(),
                                             selectInput("input_tablebrowser_choices", "Select analysis folder:", choices),
                                             selectInput("input_tablebrowser_files", "Select comparison:", ""),
                                             selectInput("input_tablebrowser_readtop", "Read top n rows:", topRowsChoices),

                                             br()
                                    ),
                                    tabPanel("file upload",


                                             br(),

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

                                             br()


                                    )# end of tab panel



                           ),# end of tab box


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

                               )
                           ),





                           collapsible = TRUE,
                           title = "Table Browser", status = "primary", solidHeader = TRUE)
              )
            )
    ),


    ########################################################################################################################
    ##
    ## Tab Item : Top Scorer, Venn Diagram, Overlapping CpGs
    ##
    ########################################################################################################################

    tabItem(tabName = "topscorer",
            fluidRow(
              column(width = 4,
                     tabBox(  width = NULL, height = "250px",
                              tabPanel("analysis",
                                       br(),
                                       tags$div(id = "div_analysis_checkboxes", class="customtabbox",
                                                uiOutput("cb")




                                       )

                              )


                     ),
                     tabBox(  width = NULL, height = "250px",
                              tabPanel("comparisons",

                                     br(),
                                     tags$div(id = "", class="customtabboxcomparison",
                                              uiOutput("si")
                                     )
                              )
                     )


              ),
              column(width = 8,
                     box(  width = NULL,

                           tags$h2(style="color:black;","Top-scorer"),
                           tags$div(id = "ts_div_info", checked=NA,

                                    tags$p(paste("User can select multiple RnBeads analysis and filter the data based on top scores and see the overlappings in your seleted analysis in the form of Venn Diagram and also view the overlapping CpGs in table form.",
                                                 ""
                                    )
                                    )# end p tag

                           ),

                           tags$h4(style="color:black;","Multiple analysis Venn Diagram"),
                           tags$p(paste("Check the analysis on the left for which you want to see the overlapping. (Top 100 rows)")),
                           tags$p(paste("Note: Please select atleast 1 and atmost 6 analysis to draw Venn Diagram!")),


                           #checkboxGroupInput("cb_ts_comp_venn", label = h3("Select analysis"),
                           #                   choices = list("")),

                           tags$div(id = "", class="",
                                    fluidRow(
                                      column(width = 5,

                                             uiOutput("ts.columns")

                                      ),
                                      column(width = 3,

                                             uiOutput("ts.columns.equality")



                                      ),

                                      column(width = 4,

                                             uiOutput("ts.columns.range")

                                      )

                                    )# end  of  fluid row
                           ),

                           actionButton('btnMultipleShowVenn', 'Display',class="btn btn-primary btn-md"),
                           br(),

                           div(class="",

                               #tags$p(textOutput("comparison.check")),
                               tags$p(textOutput("ts.venn.overlapping.error.value"  )),
                               plotOutput('output.ts.multivenn.plot'),
                               tableOutput('output.ts.table.multivenn.plot.labels'),
                               br()

                           ),


                           collapsible = TRUE,
                           title = "Venn Diagram", status = "primary", solidHeader = TRUE)
              )
            ),

            fluidRow(

              column(width = 12,
                     box(  width = NULL,

                           tags$h4(style="color:black;","Overlapping CpGs"),
                           tags$p(paste("")),

                           div(class="",


                               uiOutput("ts.selector.overlapping.value" ),
                               br()

                           ),

                           div(class="",


                               dataTableOutput('output.topscorer.overlappingComparison'),
                               br()

                           ),
                           br(),


                           collapsible = TRUE,
                           title = "Comparison details", status = "primary", solidHeader = TRUE)
              )
            )
    ),

    ########################################################################################################################
    ##
    ## Tab Item : About
    ##
    ########################################################################################################################

    tabItem(tabName = "about",
            #includeMarkdown("../../about/about.Rmd")



              # Home nav menu
              tabPanel("Home",

                       shinyjs::useShinyjs(),

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

                                              paste('<p>',textOutput("ErrorText1"),'</p>'),

                                              paste('<h5>',textOutput("ErrorText2"),'</h5>'),

                                              actionButton("action", label = "Continue", class="btn btn-primary"),

                                              #actionButton("clearDirButton",label= "Clear",class="btn btn-primary"),

                                              br(),
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

                                                 if(typeMessage == 4){
                                                    $("a:contains(DatasetList)").click();
                                                 }
                                             });
                                             ')
                       )

              )




    )# end of tabitem
  )# end of tab items
)# end of dashboard body


########################################################################################################################
##
## Dashboard Page
##
########################################################################################################################

dashboardPage(

  header,
  sidebar,
  body

)
