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
        
        selectInput('CI', 'Confidence Interval', choices=c(0.85, 0.90, 0.95, 0.99)),
        textInput('threshold', '% of 4C Reference MFI Threshold', value=75),
        checkboxGroupInput('conc_avgs', 'Regression Concentrations', choices=c(
            '30 ng/test', 
            '60 ng/test',
            '125 ng/test',
            '250 ng/test',
            '500 ng/test',
            '1000 ng/test',
            '2000 ng/test'
            ))
    ),
    
    mainPanel(
        ## OUTPUTS ##
        DT::dataTableOutput("sample_table"),
        DT::dataTableOutput("full_table_output"),
        DT::dataTableOutput("regression_table_output"),
        DT::dataTableOutput("melted_table_output"),
        wellPanel(plotOutput('plot_output')),
        wellPanel('Predicted Shelf-Life',
                  textOutput('shelf_life_output'))
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
    
    full_data_table <- reactive({fullDataTable(df_products_upload())})
    melted_data_table <- reactive({meltedDataTable(full_data_table())})
    regression_data_table <- reactive({regressionDataTable(full_data_table())})
    
    df_full <- eventReactive(input$target_upload,{
        # full_data_table <- fullDataTable(df_products_upload())
        # melted_data_table <- meltedDataTable(full_data_table)
        # regression_data_table <- regressionDataTable(full_data_table)
        plot <- createPlot(melted_data_table(), regression_data_table(), as.numeric(confidenceInterval()))
        return (full_data_table)
        })
    
    # stability_plot <- eventReactive(input$target_upload, {
    #     # full_data_table <- fullDataTable(df_products_upload())
    #     # melted_data_table <- meltedDataTable(full_data_table)
    #     # regression_data_table <- regressionDataTable(full_data_table)
    #     plot <- createPlot(melted_data_table(), regression_data_table(), as.numeric(confidenceInterval()))
    #     return (plot)
    # })
    
    # shelf_life <- eventReactive(input$target_upload, {
    #     SL <- summarizeData(regression_data_table(), as.numeric(threshold_y()))
    #     return (SL)
    # })
    
    output$shelf_life_output <- renderText({
        if (is.null(input$target_upload)) {
            return (NULL)
        }
        SL <- summarizeData(regression_data_table(), as.numeric(threshold_y()))
    })
    output$sample_table <- DT::renderDataTable({
        df <- df_products_upload()
        DT::datatable(df) %>%
            formatRound(columns=c(1), 1) %>%
            formatRound(columns=c(2:ncol(df)), 0)
    })
    
    output$plot_output <- renderPlot({
        createPlot(melted_data_table(), regression_data_table(), as.numeric(confidenceInterval()))
    })
    
}
)

# Run the application 
shinyApp(ui = ui, server = server)