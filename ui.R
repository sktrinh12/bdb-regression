library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(plotly)
library(knitr)
library(rmarkdown)

source('global.R')
example_file <- read.csv('stability_stats.csv')

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
                     downloadButton("downloadData", "Download Stats Template"),
                     br(),
                     br(),
                     downloadButton("downloadExample", "Download Stats Example"),
                     br(),
                     br(),
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
                     checkboxGroupInput('conc_avgs', 'Regression Concentrations', choices = c(
                         '30 ng/test' = 2, 
                         '60 ng/test' = 3,
                         '125 ng/test' = 4,
                         '250 ng/test' = 5,
                         '500 ng/test' = 6,
                         '1000 ng/test' = 7,
                         '2000 ng/test' = 8
                     ),
                     selected = c(
                         '30 ng/test' = 2, 
                         '60 ng/test' = 3,
                         '125 ng/test' = 4,
                         '250 ng/test' = 5,
                         '500 ng/test' = 6,
                         '1000 ng/test' = 7,
                         '2000 ng/test' = 8
                     )),
                     radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'), inline = TRUE),
                     downloadButton("report", "Generate report", class = "btn-primary"),
                     downloadButton("rds_download", "Save Data for Report", class = "btn-secondary")
                 ),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Stats Table",
                                  br(),
                                  DT::dataTableOutput("sample_table")
                                  
                                  
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

                         )
                         
                     )
                 )
        )
        # tabPanel("Navbar 2", "This panel is intentionally left blank"),
        # tabPanel("Navbar 3", "This panel is intentionally left blank")
    )
)