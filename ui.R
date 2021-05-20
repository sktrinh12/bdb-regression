library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(plotly)
library(knitr)
library(rmarkdown)
library(readxl)
library(officer)
library(flextable)

source('global.R')
example_file <- read.csv('stability_stats.csv')
# source('omiq_regression.R')

ui = tagList(
    # shinythemes::themeSelector(),
    navbarPage(
        theme = shinytheme("journal"),  # <--- To use a theme, uncomment this
        "Regression for Stability",
        # tabPanel('Marker Information',
        #          fluidRow(column(8,wellPanel(
        #              fluidRow(
        #                  column(4,textInput('specificity', "Specificity")),
        #                  column(4,textInput('clone', 'Clone')),
        #                  column(4,selectInput('fluorochrome', 'Fluorochrome', choices=c('BUV495', 'BUV789', 'UV455')))
        #                  ),
        #              fluidRow(
        #                  column(4,selectInput('pop1', "Cell Population 1", choices=c('Lymph', 'Mono', 'Gran'))),
        #                  column(4,selectInput('pop2', 'Cell Population 2', choices=c('Lymph', 'Mono', 'Gran'))),
        #                  column(4,selectInput('pop3', 'Cell Population 3', choices=c('Lymph', 'Mono', 'Gran')))
        #                  ),
        #              fluidRow(
        #                  column(4,textInput('employee_name', 'Name of Experimenter')),
        #                  column(4, dateInput('analysis_date', 'Date of Analysis'))
        #              )
        #              )
        #          ))),
        tabPanel("",
                 sidebarPanel(
                     includeHTML("analytics.html"),
                     # textInput('author', "Author"),
                     # fluidRow(
                     #     column(4,textInput('specificity', "Specificity")),
                     #     column(4,textInput('clone', 'Clone')),
                     #     column(4,selectInput('fluorochrome', 'Fluorochrome', choices=c('BUV495', 'BUV789', 'UV455')))),
                     fluidRow(column(7,selectizeInput('select_marker', 'Select Marker to Analyze', choices=NULL)),
                              column(5,div(uiOutput('marker_optimal'), style='padding-top:25px;'))),
                     downloadButton("downloadData", "Download Stats Template"),
                     br(),
                     br(),
                     downloadButton("downloadExample", "Download Stats Example"),
                     br(),
                     br(),
                     fileInput("raw_upload", "Choose raw stats file to upload",
                               accept = c(
                                   'text/csv',
                                   'text/comma-separated-values',
                                   '.csv'
                               )),
                     fileInput('target_upload', 'Choose file to upload',
                               accept = c(
                                   'text/csv',
                                   'text/comma-separated-values',
                                   '.csv'
                               )),
                     radioButtons('polynomial_order','Order of Polynomial', choices=c("Linear","2nd Order", "3rd Order"), selected="Linear", inline=TRUE),
                     uiOutput('warning_ui_polynomial_choice'),
                     br(),
                     selectInput('CI', 'Confidence Interval', choices=c(0.85, 0.90, 0.95, 0.99), selected = 0.95),
                     textInput('threshold', '% of 4C Reference MFI Threshold', value=75),
                     uiOutput('concentration_checkGroupInput'),
                     # checkboxGroupInput('conc_avgs', 'Regression Concentrations', choices = c(
                     #     '15 ng/test' = 2,
                     #     '30 ng/test' = 3,
                     #     '60 ng/test' = 4,
                     #     '125 ng/test' = 5,
                     #     '250 ng/test' = 6,
                     #     '500 ng/test' = 7,
                     #     '1000 ng/test' = 8,
                     #     '2000 ng/test' = 9
                     # ),
                     #     selected = c(
                     #     '15 ng/test' = 2,
                     #     '30 ng/test' = 3,
                     #     '60 ng/test' = 4,
                     #     '125 ng/test' = 5,
                     #     '250 ng/test' = 6,
                     #     '500 ng/test' = 7,
                     #     '1000 ng/test' = 8,
                     #     '2000 ng/test' = 9
                     # )),
                     radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'), inline = TRUE),
                     actionButton("write_results", "Save Summary Results to CSV", class = "btn-secondary"),
                     downloadButton("report", "Generate report", class = "btn-primary"),
                     downloadButton("pptx_id", "Download PPT", class = "btn-primary")
                     # downloadButton("download_powerpoint", "Generate PPT", class = 'btn-secondary')
                     
                 ),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Stats Table",
                                  br(),
                                  DT::dataTableOutput("sample_table"),
                                  br()

                                  
                         ),tabPanel("Plots",
                                    # br(),
                                    # DT::dataTableOutput("sample_table"),
                                    br(),
                                    fluidRow(column(6,plotOutput('mfi_vs_concentration')),
                                             column(6,plotOutput('mfi_vs_time'))
                                    ),
                                    br(),
                                    fluidRow(column(6,plotOutput('stain_index')),
                                             column(6,plotOutput('signal_to_noise'))
                                    ),
                                    br(),
                                    fluidRow(column(6,plotOutput('percent_positive')),
                                             # column(6,plotOutput('signal_to_noise'))
                                    )
                         ),
                         tabPanel("Regression & Shelf-Life", 
                                  wellPanel(h4(p(strong("Regression Analysis for Stability"))), 
                                            # plotlyOutput('plot_output'),
                                            plotOutput("plot1", height = 350,
                                                       click = "plot1_click",
                                                       brush = brushOpts(
                                                           id = "plot1_brush"
                                                       )
                                            ),
                                            actionButton("exclude_reset", "Reset")
                                  ),

                                  fluidRow(column(
                                      6,wellPanel(
                                      
                                          
                                              h4(p(strong(
                                                  "Predicted Shelf-Life"
                                              ))),
                                              uiOutput('check_shelf_life'),
                                              h4(p(
                                                  strong("Predicted Shelf-Life w/ 95% Confidence")
                                              )),
                                              uiOutput('check_lower_shelf_life'),
                                              br(),
                                              
                                              uiOutput('warning_ui_rsq'),
                                              uiOutput('warning_ui_slope')
                                          )),
                                          column(
                                              6,wellPanel(
                                              textAreaInput("notes", "Notes", placeholder = "Add any notes regarding your experiment or decisions to exclude data points.", height ="120px")
                                          )
                                      )) # end of well panel

                         ), #end of Regression & Shelf-Life tab panel
                         tabPanel("Residuals & Model Check",
                                  br(),
                                  fluidRow(column(6,plotlyOutput('residual_plot')),
                                           column(6,plotlyOutput('residual_histogram'))
                                           ),
                                  # tableOutput('second_order_results_output'),
                                  # tableOutput('optimal_linear_results_output'),
                                  # tableOutput('optimal_second_order_results_output'),
                                  br(),
                                  wellPanel(h3('Summary of Results'),
                                            br(),
                                            tableOutput('results_table'))
                                  
                         ) #end of Residuals tab panel
                     )
                 )
        )
        # tabPanel("Navbar 2", "This panel is intentionally left blank"),
        # tabPanel("Navbar 3", "This panel is intentionally left blank")
    )
)