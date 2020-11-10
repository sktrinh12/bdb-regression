library(shiny)
library(DT)

source('linearRegression.R')

# Define UI
ui <- shinyUI(fluidPage(
    
    ## USER INPUTS ##
    headerPanel(
        'Regression for Stability'
    ),
    
    sidebarPanel(
    # Upload csv file
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
            ))
    ),
    
    mainPanel(
        ## OUTPUTS ##
        DT::dataTableOutput("sample_table"),
        DT::dataTableOutput("full_table_output"),
        DT::dataTableOutput("regression_table_output"),
        DT::dataTableOutput("melted_table_output"),
        wellPanel(h4(p(strong("Regression Analysis for Stability"))), plotOutput('plot_output')),
        wellPanel(h5(p(strong("Predicted Shelf-Life w/ 95% Confidence"))), textOutput('shelf_life_lower_output'))
        # wellPanel(h5(p(strong("Predicted Shelf-Life w/ 95% Confidence"))), textOutput('shelf_life_lower_output'))
        
    )
    
)
)

# Define server logic
server <- shinyServer(function(input, output) {
    
    df_products_upload <- reactive({
        inFile <- input$target_upload
        if (is.null(inFile))
            return(NULL)
        df <- read.csv(inFile$datapath, header = TRUE, sep = ',')
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
    
    output$plot_output <- renderPlot({
        createPlot(melted_data_table(), regression_data_table(), as.numeric(confidenceInterval()))
    })
    
    output$averages <- renderText({
        as.list(input$conc_avgs)
        typeof(input$conc_avgs)
    })
    output$CI_output <- renderText({ as.character(paste0("Predicted Shelf Life with ", input$CI, "% Confidence")) })
    
}
)

# Run the application 
shinyApp(ui = ui, server = server)