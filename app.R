library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(plotly)

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
                                            h4(p(strong("Predicted Shelf-Life w/ 95% Confidence"))), textOutput('shelf_life_lower_output'),
                                            # verbatimTextOutput('text1'),
                                            # verbatimTextOutput('text2'),
                                            # verbatimTextOutput('text3')
                                            )
                                  )
                         
                     )
                 )
        )
        # tabPanel("Navbar 2", "This panel is intentionally left blank"),
        # tabPanel("Navbar 3", "This panel is intentionally left blank")
    )
)
server = function(input, output) {
    
    template_data <- data.frame('Time'=c(0,0.5,1,1.5,2,3,4,5), 
                                'Conc_30_ng'=rep(NA, 8), 
                                'Conc_60_ng'=rep(NA, 8),
                                'Conc_125_ng'=rep(NA, 8),
                                'Conc_250_ng'=rep(NA, 8),
                                'Conc_500_ng'=rep(NA, 8),
                                'Conc_1000_ng'=rep(NA, 8),
                                'Conc_2000_ng'=rep(NA, 8),
                                row.names=NULL
                                )
    
    # Download Template Stats File
    output$downloadData <- downloadHandler(
        filename = 'stability_stats_template.csv',
        content = function(file) {
            write.csv(template_data, file, row.names=FALSE)
        }
    )
    
    # Download Example Stats File
    output$downloadExample <- downloadHandler(
        filename = 'stability_stats_example.csv',
        content = function(file) {
            write.csv(example_file, file, row.names=FALSE)
        }
    )
    
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
    # full_data_table <- reactive({fullDataTable(df_products_upload(), conc_avgs_list())})
    
    full_data_table <- reactive({
        if (is.null(input$target_upload)){
            return(template_data)
        }
        
        full_data_table_expr <- reactive({conc_to_exclude(df_products_upload(), conc_avgs_list())})
        return(full_data_table_expr())
    
    })
    

    melted_data_table <- reactive({meltedDataTable(full_data_table())})
    regression_data_table <- reactive({regressionDataTable(full_data_table())})
    # confidence_intervals <- reactive({ reg_conf_intervals(melted_data_table()$Time, melted_data_table()$value, as.numeric(confidenceInterval()), as.numeric(threshold_y())) })
    confidence_intervals <- reactive({ reg_conf_intervals(keep()$Time, keep()$value, as.numeric(confidenceInterval()), as.numeric(threshold_y())) })
    
    labels <- reactive({ labels_column(input$target_upload) })
    
    ###############################################################################
    # For storing which rows have been excluded
    vals <- reactiveValues(
        keeprows = rep(TRUE, 64)
        
    )
    observeEvent(input$target_upload, {
        vals$keeprows <- rep(TRUE, nrow(melted_data_table()))
        
    })
    
    # output$text1 <- renderPrint(vals$keeprows)
    output$plot1 <- renderPlot({
        # Plot the kept and excluded points as two separate data sets
        # vals$keeprows <- rep(TRUE, nrow(melted_data_table()))
        keep    <- melted_data_table()[ vals$keeprows, , drop = FALSE] # subsets the mtcars dataset to pull only values where the keeprows = TRUE
        exclude <- melted_data_table()[!vals$keeprows, , drop = FALSE] # subsets the mtcars dataset to pull only values where the keeprows = FALSE
        # output$text1 <- renderPrint(keep)
        # keep <- keep[complete.cases(keep),]
        # print(keep)
        ggplot(keep, aes(x=Time, y=value, color=Concentrations)) + 
            geom_smooth(data=keep, aes(x=Time, y=value), formula = y ~ x, method="lm", col = "red", level=as.numeric(confidenceInterval())) +
            geom_point(size=10) + # plot with keep dataset points filled in only
            geom_point(data = exclude, size = 10, shape = 21, fill = NA, color = 'black') +
            labs(x = "Time (years)",
                          y = "% of 4C Reference MFI") +
                     theme_minimal() +
                     scale_color_brewer(palette = 'Reds', na.translate = F,
                                        labels = unique(keep$Labels)
                                        # labels = c("30 ng/test", '60 ng/test', '125 ng/test', '250 ng/test', '500 ng/test', '1000 ng/test', '2000 ng/test')
                     )
        
        
        # ggplot(dataMelt, aes(x=Time, y=value, color=Concentrations)) +
        #     # Line plot based on average of certain columns
        #     geom_smooth(data=df_regression, aes(x=Time, y=Average), formula = y ~ x, method="lm", col="red", level=CI_level) +
        #     geom_point(size=4) +
        #     labs(x = "Time (y)",
        #          y = "% of 4C Reference MFI") +
        #     theme_minimal() +
        #     scale_color_brewer(palette = 'Reds', labels = c(
        #         "30 ng/test", '60 ng/test', '125 ng/test', '250 ng/test', '500 ng/test', '1000 ng/test', '2000 ng/test', 'Average'
        #     )) 
        # p <- ggplotly(outputPlot)
        
    })
    keep    <- reactive({ melted_data_table()[ vals$keeprows, , drop = FALSE] })
    # output$text1 <- renderPrint(keep())
    
    # Toggle points that are clicked
    observeEvent(input$plot1_click, {
        res <- nearPoints(melted_data_table(), input$plot1_click, allRows = TRUE) # plot1_click from plotOutput on UI sends coordinates to server, allRows adds new column to dataframe called "selected_" indicating whether the row was selected or not
        # output$text1 <- renderPrint(res$selected_)
        output$text2 <- renderPrint(vals$keeprows)
        vals$keeprows <- xor(vals$keeprows, res$selected_) # keeprows array is updated 
        output$text3 <- renderPrint(xor(vals$keeprows, res$selected_))
    })
    
    # Toggle points that are brushed, when button is clicked
    observeEvent(input$exclude_toggle, {
        res <- brushedPoints(melted_data_table(), input$plot1_brush, allRows = TRUE)
        
        vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
    
    # Reset all points
    observeEvent(input$exclude_reset, {
        vals$keeprows <- rep(TRUE, 64)
    })
    
    
    
    # # ##########################################################################
    # # For storing which rows have been excluded
    # vals <- reactiveValues(
    #     keeprows = rep(TRUE, nrow(mtcars))
    # )
    # output$text1 <- renderPrint(vals$keeprows)
    # output$plot1 <- renderPlot({
    #     # Plot the kept and excluded points as two separate data sets
    #     keep    <- mtcars[ vals$keeprows, , drop = FALSE] # subsets the mtcars dataset to pull only values where the keeprows = TRUE
    #     exclude <- mtcars[!vals$keeprows, , drop = FALSE] # subsets the mtcars dataset to pull only values where the keeprows = FALSE
    #     
    #     ggplot(keep, aes(wt, mpg)) + geom_point() + # plot with keep dataset points filled in only
    #         geom_smooth(method = lm, fullrange = TRUE, color = "black") +
    #         geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) + # add exclude dataset points with black outline, no fill
    #         coord_cartesian(xlim = c(1.5, 5.5), ylim = c(5,35))
    # })
    # 
    # # Toggle points that are clicked
    # observeEvent(input$plot1_click, {
    #     res <- nearPoints(mtcars, input$plot1_click, allRows = TRUE) # plot1_click from plotOutput on UI sends coordinates to server, allRows adds new column to dataframe called "selected_" indicating whether the row was selected or not
    #     output$text1 <- renderPrint(res$selected_)
    #     output$text2 <- renderPrint(vals$keeprows)
    #     vals$keeprows <- xor(vals$keeprows, res$selected_) # keeprows array is updated 
    #     output$text3 <- renderPrint(xor(vals$keeprows, res$selected_))
    # })
    # 
    # # Toggle points that are brushed, when button is clicked
    # observeEvent(input$exclude_toggle, {
    #     res <- brushedPoints(mtcars, input$plot1_brush, allRows = TRUE)
    #     
    #     vals$keeprows <- xor(vals$keeprows, res$selected_)
    # })
    # 
    # # Reset all points
    # observeEvent(input$exclude_reset, {
    #     vals$keeprows <- rep(TRUE, nrow(mtcars))
    # })
    # 
    # ###############################################################################
    
    
    # When file uploaded, create plot
    df_full <- eventReactive(input$target_upload,{
        # full_data_table <- fullDataTable(df_products_upload())
        # melted_data_table <- meltedDataTable(full_data_table)
        # regression_data_table <- regressionDataTable(full_data_table)
        plot <- createPlot(melted_data_table(), regression_data_table(), as.numeric(confidenceInterval()))
        if (is.null(input$target_upload)) {
            return (NULL)
        }
        return (full_data_table)
    })
    
    
    # When file uploaded, output shelf-life from linearRegression.R file
    output$shelf_life_output <- renderText({
        if (is.null(input$target_upload)) {
            return (NULL)
        }
        SL <- paste(summarizeData(keep(), as.numeric(threshold_y())), ' years')
    })
    
    output$shelf_life_lower_output <- renderText({
        if (is.null(input$target_upload)) {
            return (NULL)
        }
        SL <- paste(confidence_intervals(), ' years')
    })
    
    output$sample_table <- DT::renderDataTable({
        # df <- df_products_upload()
        if (is.null(input$target_upload)) {
            return (NULL)
        }
        df <- full_data_table()
        
        DT::datatable(df) %>%
            formatRound(columns=c(1), 1) %>%
            formatRound(columns=c(2:ncol(df)), 0)
    })
    
    # output$plot_output <- renderPlotly({
    #     if (is.null(input$target_upload)) {
    #         return (NULL)
    #     }
    #     createPlot(melted_data_table(), regression_data_table(), as.numeric(confidenceInterval()))
    # })

    # output$averages <- renderText({
    #     as.list(input$conc_avgs)
    #     typeof(input$conc_avgs)
    # })
    # output$CI_output <- renderText({ as.character(paste0("Predicted Shelf Life with ", input$CI, "% Confidence")) })
    # 
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