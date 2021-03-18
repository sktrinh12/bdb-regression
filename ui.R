library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(plotly)
library(knitr)
library(rmarkdown)

source('linearRegression.R')
example_file <- read.csv('stability_stats.csv')

ui = tagList(
    # shinythemes::themeSelector(),
    navbarPage(
        theme = shinytheme("journal"),  # <--- To use a theme, uncomment this
        "Regression for Stability",
        tabPanel("",
                 sidebarPanel(
                     includeHTML("analytics.html"),
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
                     downloadButton("report", "Generate report", class = "btn-primary")
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
                                  wellPanel(h4(p(strong("Predicted Shelf-Life"))), textOutput('shelf_life_output'),
                                            h4(p(strong("Predicted Shelf-Life w/ 95% Confidence"))), textOutput('shelf_life_lower_output')
                                            # verbatimTextOutput('conc_avgs_list_output'),
                                            # verbatimTextOutput('melt_out'),
                                            # 
                                            # verbatimTextOutput('text1'),
                                  )
                         )
                         
                     )
                 )
        )
        # tabPanel("Navbar 2", "This panel is intentionally left blank"),
        # tabPanel("Navbar 3", "This panel is intentionally left blank")
    )
)