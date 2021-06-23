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
library(qqplotr)
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(reshape2)
library(nortest)
library(ggpubr)
library(shinyjs)

source('global.R')

ui = tagList(
    navbarPage(
        theme = shinytheme("journal"),  # <--- To use a theme, uncomment this
        "Regression for Stability",
        tabPanel("",
                 sidebarPanel(
                     includeHTML("analytics.html"),
                     div(style = " overflow-x: scroll;",
                         radioButtons("analysis_type","Select Manual or OMIQ Analysis", choices = c("Manual", "OMIQ"), inline=TRUE),
                         uiOutput('manual_or_omiq'),
                         
                     
                         fluidRow(column(12,
                                         radioButtons('polynomial_order','Order of Polynomial', 
                                                      choices=c("Linear","2nd Order", "3rd Order"), 
                                                      selected="Linear", inline=TRUE))),
                         fluidRow(column(12,uiOutput('warning_ui_polynomial_choice'))),
                         br(),
                         selectInput('CI', 'Confidence Interval', choices=c(0.85, 0.90, 0.95, 0.99), selected = 0.95),
                         textInput('threshold', '% of 4C Reference MFI Threshold', value=75),
                         uiOutput('concentration_checkGroupInput'),
                         br(),
                         br(),
                         uiOutput("omiq_report_bundle_ui")
                     )
                 ),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Stats Table",
                                  br(),
                                  DT::dataTableOutput("reference_mfi_data_table"),
                                  br()
                                  # DT::dataTableOutput("kept_excluded_table")      
                         ),tabPanel("Plots",
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
                                             column(6,plotOutput('percent_of_4C_MFI'))
                                    )
                         ),
                         tabPanel("Regression & Shelf-Life", 
                                  wellPanel(h4(p(strong("Regression Analysis for Stability"))), 
                                            plotOutput("regression_plot_output", height = 350,
                                                       click = "regression_plot_output_click",
                                                       dblclick = "regression_plot_zoom_dblclick",
                                                       brush = brushOpts(
                                                           id = "regression_plot_output_brush",
                                                           resetOnNew = TRUE
                                                       )
                                            ),
                                            fluidRow(column(2,actionButton("exclude_reset", "Reset")),
                                                     column(10,h6(p(strong(em("To zoom in, highlight box around area of interest and double click inside box. To reset zoom, double click any blank space inside plot area."))))
                                                            ))

                                  ),

                                  fluidRow(column(
                                      6,wellPanel(
                                              h4(p(strong(
                                                  "Predicted Shelf-Life (Raw)"
                                              ))),
                                              uiOutput('check_lower_shelf_life'),
                                              h4(p(
                                                  strong("Predicted Shelf-Life (Rounded)")
                                              )),
                                              uiOutput('check_rounded_shelf_life'),
                                              h4(p(
                                                  strong("Model Coefficient P-value")
                                              )),
                                              uiOutput('model_coeff_pvalue'),
                                              br(),
                                              uiOutput('warning_ui_model_coeff_pvalue'),
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
                                  fluidRow(column(6,plotlyOutput('residual_fit_plot')),
                                           column(6,plotlyOutput('residual_histogram'))
                                           ),
                                  br(),
                                  br(),
                                  fluidRow(column(6,plotlyOutput('normal_prob_plot_output')),
                                           column(6,wellPanel(
                                               h4(p(strong(
                                               "Anderson-Darling Normality Test p-value"
                                           ))),
                                           uiOutput('anderson_darling_pvalue_output'),
                                           br(),
                                           uiOutput('warning_normality_pvalue')
                                           )))
                                  
                         ) #end of Residuals tab panel
                     )
                 )
        )
    )
)