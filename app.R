library(shiny)
library(shinythemes)
library(DT)

source('linearRegression.R')

ui = tagList(
    # shinythemes::themeSelector(),
    navbarPage(
        theme = shinytheme("journal"),  # <--- To use a theme, uncomment this
        "Regression for Stability",
        tabPanel("",
                 sidebarPanel(
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
                                  DT::dataTableOutput("sample_table"),
                                  
                         ),
                         tabPanel("Regression & Shelf-Life", 
                                  wellPanel(h4(p(strong("Regression Analysis for Stability"))), plotlyOutput('plot_output')),
                                  wellPanel(h4(p(strong("Predicted Shelf-Life"))), textOutput('shelf_life_output'),
                                            h4(p(strong("Predicted Shelf-Life w/ 95% Confidence"))), textOutput('shelf_life_lower_output')))
                     )
                 )
        )
        # tabPanel("Navbar 2", "This panel is intentionally left blank"),
        # tabPanel("Navbar 3", "This panel is intentionally left blank")
    )
)
server = function(input, output) {
    df_products_upload <- reactive({
        inFile <- input$target_upload
        if (is.null(inFile))
            return(NULL)
        df_unrounded <- read.csv(inFile$datapath, header = TRUE, sep = ',')
        df <- cbind('Time'=df_unrounded[[1]], round(df_unrounded[2:ncol(df_unrounded)]))
        return(df)
    })
    
    # Reactive User Inputs
    confidenceInterval <- reactive({input$CI})
    threshold_y <- reactive({input$threshold})
    
    # Concentrations to average
    conc_avgs_list <- reactive({ unlist(strsplit(input$conc_avgs, " ")) })
    output$conc_avgs_list_output <- renderText({ length(conc_avgs_list()) })
    
    # Run linearRegression.R file
    full_data_table <- reactive({fullDataTable(df_products_upload(), conc_avgs_list())})
    melted_data_table <- reactive({meltedDataTable(full_data_table())})
    regression_data_table <- reactive({regressionDataTable(full_data_table())})
    confidence_intervals <- reactive({ reg_conf_intervals(regression_data_table()$Time, regression_data_table()$Average, as.numeric(confidenceInterval()), as.numeric(threshold_y())) })
    
    # When file uploaded, create plot
    df_full <- eventReactive(input$target_upload,{
        # full_data_table <- fullDataTable(df_products_upload())
        # melted_data_table <- meltedDataTable(full_data_table)
        # regression_data_table <- regressionDataTable(full_data_table)
        plot <- createPlot(melted_data_table(), regression_data_table(), as.numeric(confidenceInterval()))
        return (full_data_table)
    })
    
    
    
    
    
    
    # When file uploaded, output shelf-life from linearRegression.R file
    output$shelf_life_output <- renderText({
        if (is.null(input$target_upload)) {
            return (NULL)
        }
        SL <- paste(summarizeData(regression_data_table(), as.numeric(threshold_y())), ' years')
    })
    
    output$shelf_life_lower_output <- renderText({
        if (is.null(input$target_upload)) {
            return (NULL)
        }
        SL <- paste(confidence_intervals(), ' years')
    })
    
    output$sample_table <- DT::renderDataTable({
        df <- df_products_upload()
        if (is.null(input$target_upload)) {
            return (NULL)
        }
        DT::datatable(df) %>%
            formatRound(columns=c(1), 1) %>%
            formatRound(columns=c(2:ncol(df)), 0)
    })
    
    output$plot_output <- renderPlotly({
        createPlot(melted_data_table(), regression_data_table(), as.numeric(confidenceInterval()))
    })
    
    output$averages <- renderText({
        as.list(input$conc_avgs)
        typeof(input$conc_avgs)
    })
    output$CI_output <- renderText({ as.character(paste0("Predicted Shelf Life with ", input$CI, "% Confidence")) })
    
    ################## R Markdown Report ######################
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.pdf",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(param2 = input$CI,
                           param3 = df_products_upload(),
                           param4 = full_data_table(),
                           param5 = threshold_y(),
                           param6 = melted_data_table(),
                           param7 = regression_data_table(),
                           param8 = confidence_intervals(),
                           param9 = createPlot(melted_data_table(), regression_data_table(), as.numeric(confidenceInterval())))
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
}
shinyApp(ui=ui, server=server)