source('global.R')
example_file <- read.csv('stability_stats.csv')
wave_summary_file <- "wave6_summary.xlsx"

server = function(input, output, session) {
    

    ########################## TEMPLATE DATA ##########################
    template_data <- data.frame('Time'=c(0,0.5,1,1.5,2,3,4,5), 
                                'Conc_15_ng'=rep(NA, 8),
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

    ########################## EXAMPLE DATA ###########################
    # Download Example Stats File
    output$downloadExample <- downloadHandler(
        filename = 'stability_stats_example.csv',
        content = function(file) {
            write.csv(example_file, file, row.names=FALSE)
        }
    )
    
    ########################## RAW DATA ###########################
    df_products_upload <- reactive({
        inFile <- input$target_upload
        if (is.null(inFile)) 
            return(NULL)
        df_unrounded <- read_csv(inFile$datapath)
        if(is.na(df_unrounded$Conc_15_ng)) {
            print('its NA')
        }
        # df <- cbind('Time'=df_unrounded[[1]], round(df_unrounded[2:ncol(df_unrounded)]))
        df <- df_unrounded
        return(df)
    })
    
    output$concentration_checkGroupInput <-
        renderUI({
            if ( all(is.na(df_products_upload()$Conc_15_ng)) ) {
                checkboxGroupInput(
                    'conc_avgs',
                    'Regression Concentrations',
                    choices = c(
                        '15 ng/test' = 2,
                        '30 ng/test' = 3,
                        '60 ng/test' = 4,
                        '125 ng/test' = 5,
                        '250 ng/test' = 6,
                        '500 ng/test' = 7,
                        '1000 ng/test' = 8,
                        '2000 ng/test' = 9
                    ),
                    selected = c(
                        # '15 ng/test' = 2,
                        '30 ng/test' = 3,
                        '60 ng/test' = 4,
                        '125 ng/test' = 5,
                        '250 ng/test' = 6,
                        '500 ng/test' = 7,
                        '1000 ng/test' = 8,
                        '2000 ng/test' = 9
                    )
                )
            }
            else{
                checkboxGroupInput(
                    'conc_avgs',
                    'Regression Concentrations',
                    choices = c(
                        '15 ng/test' = 2,
                        '30 ng/test' = 3,
                        '60 ng/test' = 4,
                        '125 ng/test' = 5,
                        '250 ng/test' = 6,
                        '500 ng/test' = 7,
                        '1000 ng/test' = 8,
                        '2000 ng/test' = 9
                    ),
                    selected = c(
                        '15 ng/test' = 2,
                        '30 ng/test' = 3,
                        '60 ng/test' = 4,
                        '125 ng/test' = 5,
                        '250 ng/test' = 6,
                        '500 ng/test' = 7,
                        '1000 ng/test' = 8,
                        '2000 ng/test' = 9
                    )
                )
            }
            
        })
    
    raw_upload_data <- reactive({
        inFile <- input$raw_upload
        if(is.null(inFile))
            return(NULL)
        df <- read_xlsx(inFile$datapath)
        return(df)
    })
    
    raw_melted_data <- reactive({ meltedDataTable(df_products_upload()) })
    raw_shelf_life <- reactive({ solve_for_shelf_life(raw_melted_data(), as.numeric(threshold_y()), poly_order()) })
    raw_lower_shelf_life <- reactive({ solve_for_lower_shelf_life(raw_melted_data(), poly_order(), as.numeric(confidenceInterval()), as.numeric(threshold_y() )) })
    raw_confidence_bands <- reactive({ find_confidence_bands(raw_melted_data(), poly_order(), as.numeric(confidenceInterval()), as.numeric(threshold_y())) })
    
    # melted_data_table <- reactive({meltedDataTable(full_data_table())})
    # regression_data_table <- reactive({regressionDataTable(full_data_table())})
    # 
    # slope <- reactive({ best_fit_equation(keep(), poly_order())$coefficients[[2]] })
    # 
    ################################### PLOTS TAB ##################################################
    output$mfi_vs_concentration <- renderPlot({
        req(input$raw_upload)
        mfi_vs_concentration_plot(raw_upload_data())
    })
    output$mfi_vs_time <- renderPlot({
        req(input$raw_upload)
        mfi_vs_time_plot(raw_upload_data())
    })
    output$stain_index <- renderPlot({
        req(input$raw_upload)
        stain_index(raw_upload_data())
    })
    output$signal_to_noise <- renderPlot({
        req(input$raw_upload)
        signal_to_noise(raw_upload_data())
    })
    output$percent_positive <- renderPlot({
        req(input$raw_upload)
        percent_positive(raw_upload_data())
    })
    #############################################
    optimal <- reactive({ wave_df()$`Optimal (ng/test)`[wave_df()$`Marker Description` == input$select_marker] })
    wave_df <- reactive({ read_marker_data(wave_summary_file) })
    output$marker_optimal <- renderUI({
        tags$i(paste0('Optimal: ',optimal(), ' ng/test'),
               style="color: #eb6864; font-size: 18px; font-style: normal; font-weight: bold; padding-top: 20px;"
        )
        
    })
    # renderText({ if(as.character(paste0(optimal(), ' ng/test')) == ) })
    output$concentrations <- renderText({ input$conc_avgs })
    updateSelectizeInput(session, 'select_marker', choices = c(read_marker_data(wave_summary_file)$`Marker Description`), server = TRUE)
    
    output$tab_panel_title <- renderText({
        input$pop1
    })
    # Reactive User Inputs
    confidenceInterval <- reactive({input$CI})
    threshold_y <- reactive({input$threshold})
    
    # Concentrations to average
    conc_avgs_list <- reactive({ unlist(strsplit(input$conc_avgs, " ")) })
    output$conc_avgs_list_output <- renderText({ length(conc_avgs_list()) })
    
    # Run linearRegression.R file
    full_data_table <- reactive({
        if(is.null(input$target_upload)){
            return(template_data)
        }
        
        return(conc_to_exclude(df_products_upload(), conc_avgs_list()))
    })

    poly_order <- reactive({ order_of_polynomial(input$polynomial_order) })

    # best_fit_eqn <- reactive({ find_best_fit_equation(keep(), poly_order()) })
    # r_sq <- reactive({ r_sq(best_fit_eqn()) })
    # adj_r_sq <- reactive({ adj_r_sq(best_fit_eqn()) })
    # coeff <- reactive({ coeff(best_fit_eqn(), poly_order()) })
    # output$coeffs <- renderTable({ coeff() })
    # output$best_fit <- renderUI({ best_fit_eqn() })
    
    shelf_life <- reactive({ solve_for_shelf_life(keep(), as.numeric(threshold_y()), poly_order()) })
    output$polyroot <- renderText({ shelf_life() })
    melted_data_table <- reactive({meltedDataTable(full_data_table())})
    output$melt_out <- renderPrint({melted_data_table()})
    regression_data_table <- reactive({regressionDataTable(full_data_table())})
    confidence_bands <- reactive({ find_confidence_bands(keep(), poly_order(), as.numeric(confidenceInterval()), as.numeric(threshold_y())) })
    lower_shelf_life <- reactive({ solve_for_lower_shelf_life(keep(), poly_order(), as.numeric(confidenceInterval()), as.numeric(threshold_y() )) })
    
    slope <- reactive({ best_fit_equation(keep(), poly_order())$coefficients[[2]] })
    
    R_sq_val <- reactive({ R_sq(keep(), poly_order()) })

    r_val <- reactive({ ifelse(R_sq_val() < 0.80, "UH OH", "All good :)") })

    
    # output$polynomial_eval_of_linearity <- renderText({ polynomial_evaluation_of_linearity(keep(), poly_order()) })
    poly_eval <- reactive({ polynomial_evaluation_of_linearity(keep(), poly_order()) })
    output$rval <- renderText({ r_val() })

    output$check_shelf_life <- renderUI({
        if(is.null(shelf_life())){
            'Shelf Life is NA'
        }
        else{
            'Shelf Life works!'
        }
    })
    
    labels <- reactive({ labels_column(input$target_upload) })
    
    output$warning_ui_polynomial_choice <- renderUI({
        req(input$target_upload)

        if(poly_order() == 2 && poly_eval()$c_pvalue >= 0.05){
            tags$i(" Warning: p-value for 2nd order not statistically significant. Consider using linear model instead.",
                   class = "fa fa-exclamation-triangle", 
                   style = "color: #fcba03; font-size: 20px"
            )
        }
        else if(poly_order() == 3 && poly_eval()$d_pvalue >= 0.05){
            tags$i(" Warning: p-value for 3rd order not statistically significant. Consider using 2nd order or linear model instead.",
                   class = "fa fa-exclamation-triangle", 
                   style = "color: #fcba03; font-size: 20px"
            )
        }
    })
    
    output$warning_ui_rsq <- renderUI({
        req(input$target_upload)
        if(R_sq_val() < 0.80){
                tags$i(" Warning: R-squared value is below 0.80",
                class = "fa fa-exclamation-triangle", 
                style = "color: #fcba03; font-size: 20px"
                )
        }
    })
    output$warning_ui_slope <- renderUI({
        req(input$target_upload)
        if(slope() > 0){
            tags$i(" Warning: Regression slope is positive",
                   class = "fa fa-exclamation-triangle", 
                   style = "color: #fcba03; font-size: 20px"
            )
        }
    })
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
        req(input$target_upload)
        # Plot the kept and excluded points as two separate data sets
        # vals$keeprows <- rep(TRUE, nrow(melted_data_table()))
        keep    <- melted_data_table()[ vals$keeprows, , drop = FALSE] 
        exclude <- melted_data_table()[!vals$keeprows, , drop = FALSE] 
        output$text1 <- renderPrint(keep)
        keep <- keep[complete.cases(keep),]
        # print(keep)
        
        ggplot(keep, aes(x=Time, y=value, color=Concentrations)) + 
            # geom_smooth(data=keep, aes(x=Time), formula = y ~ poly(x,poly_order(), raw=TRUE), method="lm", col = "red", level=as.numeric(confidenceInterval())) +
            geom_ribbon(data=keep, aes(x=Time, y=value, ymin=confidence_bands()[[2]], ymax=confidence_bands()[[4]]), formula = y ~ poly(x,poly_order(), raw=TRUE), method="lm", col = "red", level=as.numeric(confidenceInterval()), alpha=0.2) +
            geom_line(data=confidence_bands(), aes(x=confidence_bands()[[1]], y=confidence_bands()[[3]]), formula = y ~ poly(x,poly_order(), raw=TRUE), method="lm", col = "red") +
            geom_point(size=7) + # plot with keep dataset points filled in only
            geom_point(data = exclude, size = 7, shape = 21, fill = NA, color = 'black') +
            labs(x = "Time (years)",
                 y = "% of 4C Reference MFI") +
            theme_minimal() +
            scale_color_brewer(palette = 'Reds', na.translate = F,
                               labels = unique(keep$Labels)
                               # labels = c("30 ng/test", '60 ng/test', '125 ng/test', '250 ng/test', '500 ng/test', '1000 ng/test', '2000 ng/test')
            ) +
            stat_regline_equation(data=keep, aes(x=Time, y=value,label=paste(..eq.label.., ..rr.label..,..adj.rr.label.., sep = "~~~~~~")), formula = y ~ poly(x,poly_order(),raw=TRUE), method="lm", col="red",
                                  label.x.npc="center",
                                  label.y.npc="top",
                                  
                                  size=5) +
            theme(text=element_text(size = 20)) 
            # geom_point(data=data.frame(confidence_bands()), aes(x=confidence_bands()[[1]], y=confidence_bands()[[2]]), formula = y ~ x, size=4, color="green") + 
            # geom_line(data=confidence_bands(), aes(x=confidence_bands()[[1]], y=confidence_bands()[[2]]), formula = y ~ x,  color="green") +
            # geom_point(data=data.frame(confidence_bands()), aes(x=confidence_bands()[[1]], y=confidence_bands()[[4]]), formula = y ~ x, size=4, color="green") + 
            # geom_line(data=confidence_bands(), aes(x=confidence_bands()[[1]], y=confidence_bands()[[4]]), formula = y ~ x,  color="green")
            # # geom_ribbon(data=keep(), aes(ymin=confidence_bands()[[2]], ymax=confidence_bands()[[4]]), linetype=2, alpha=0.1)
            
        
        
    })
    keep    <- reactive({ melted_data_table()[ vals$keeprows, , drop = FALSE] })
    exclude <- reactive({ melted_data_table()[!vals$keeprows, , drop = FALSE] })
    output$text1 <- renderPrint(keep())
    
    # Toggle points that are clicked
    observeEvent(input$plot1_click, {
        res <- nearPoints(melted_data_table(), input$plot1_click, allRows = TRUE) 
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
    
    output$residual_plot <- renderPlotly({ 
        req(input$target_upload)
        residual_vs_fit_plot(keep(), poly_order()) 
        })
    
    resid_plot <- reactive({ residual_vs_fit_plot(keep(), poly_order())  })
    output$residual_histogram <- renderPlotly({ 
        req(input$target_upload)
        residual_histogram(keep(), poly_order()) 
        })
    # # When file uploaded, create plot
    # df_full <- eventReactive(input$target_upload,{
    #     
    #     # plot <- createPlot(melted_data_table(), regression_data_table(), as.numeric(confidenceInterval()))
    #     if (is.null(input$target_upload)) {
    #         return (NULL)
    #     }
    #     return (full_data_table())
    # })
    
    ########################## SHELF LIFE ##########################
    output$shelf_life_output <- renderText({
        if (is.null(input$target_upload)) {
            return (NULL)
        }
        SL <- paste0(shelf_life(), ' years   ', '(', round(as.numeric(shelf_life())*365,0), ' days)')
    })
    
    output$shelf_life_lower_output <- renderText({
        if (is.null(input$target_upload)) {
            return (NULL)
        }
        SL <- paste0(lower_shelf_life(), ' years   ', '(', round(as.numeric(lower_shelf_life())*365,0), ' days)')
    })
    
    output$check_shelf_life <- renderUI({
        req(input$target_upload)
        if(is.null(shelf_life())){
            tags$i(" Does not intersect with MFI Threshold - No Shelf-life can be found",
                   class = "fa fa-times-circle", 
                   style = "color: red; font-size: 20px;"
            )
        }
        else{
            tags$i(paste0(round(shelf_life(),2), ' years   ', '(', round(as.numeric(shelf_life())*365,0), ' days)'),
                   style="color: #eb6864; font-size: 20px; font-style: normal; font-weight: bold;"
            )
        }
    })
    output$check_lower_shelf_life <- renderUI({
        req(input$target_upload)
        if(is.null(lower_shelf_life())){
            tags$i(" Does not intersect with MFI Threshold - No Shelf-life can be found",
                   class = "fa fa-times-circle", 
                   style = "color: red; font-size: 20px;"
            )
        }
        else{
            tags$i(paste0(round(lower_shelf_life(),2), ' years   ', '(', round(as.numeric(lower_shelf_life())*365,0), ' days)'),
                   style="color: #eb6864; font-size: 20px; font-style: normal; font-weight: bold;"
            )
        }
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
    
    raw_linear_results <- reactive({ results_summary(raw_melted_data(), 1, as.numeric(input$CI)) })
    output$linear_results_output <- renderTable({ raw_linear_results() })

    optimal_linear_results <- reactive({ results_summary(optimal_data(), 1, as.numeric(input$CI)) })
    output$optimal_linear_results_output <- renderTable({ optimal_linear_results() })
    
    raw_second_order_results <- reactive({ results_summary(raw_melted_data(), 2, as.numeric(input$CI)) })
    output$second_order_results_output <- renderTable({ raw_second_order_results() })
    
    optimal_second_order_results <- reactive({ results_summary(optimal_data(), 2, as.numeric(input$CI)) })
    output$optimal_second_order_results_output <- renderTable({ optimal_second_order_results() })
    
    optimal_data <- reactive({ meltedDataTable(conc_to_exclude(df_products_upload(), concentrations_around_optimal(optimal()))) })
    
    summary_table <- reactive({ req(input$target_upload)
        tibble(rbind(cbind('Concentrations Included'='Raw','Model Order'='Linear',raw_linear_results()),
                                             cbind('Concentrations Included'='Optimal +1/-2','Model Order'='Linear',optimal_linear_results()),
                                             cbind('Concentrations Included'='Raw','Model Order'='2nd Order',raw_second_order_results()),
                                             cbind('Concentrations Included'='Optimal +1/-2','Model Order'='2nd Order',optimal_second_order_results()))) })
    
    output$results_table <- renderTable({ summary_table() })
    
    
    observeEvent(input$write_results, {
        write_csv(summary_table(), 'summary_results_wave5.csv', append=TRUE) })
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
            params <- list(CI = input$CI,
                           threshold = input$threshold,
                           order = input$polynomial_order,
                           fullDT = full_data_table(),
                           raw_full_data = df_products_upload(),
                           meltedDT = melted_data_table(),
                           raw_melted_data = raw_melted_data(),
                           shelf_life = paste0(
                               round(shelf_life(),2),
                               ' years (', 
                               round(shelf_life()*365,0), 
                               ' days)' ),
                           shelf_life_lower = paste0(
                               round(lower_shelf_life(),2),
                               ' years (',
                               round(lower_shelf_life()*365,0),
                               ' days)'),
                           notes = input$notes,
                           author = input$author,
                           raw_data = req(input$target_upload),
                           confidence_bands = confidence_bands(),
                           raw_confidence_bands = raw_confidence_bands(),
                           poly_order = poly_order(),
                           raw_shelf_life = paste0(
                                                   round(raw_shelf_life(),2),
                                                   ' years (', 
                                                   round(raw_shelf_life()*365,0), 
                                                   ' days)' ),
                           raw_lower_shelf_life = paste0(
                               round(raw_lower_shelf_life(),2),
                               ' years (',
                               round(raw_lower_shelf_life()*365,0),
                               ' days)'),
                           keep = keep(),
                           exclude = exclude()
            )
            
            rmarkdown::render(tempReport, output_format = "pdf_document", output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
            
        }
    )
}