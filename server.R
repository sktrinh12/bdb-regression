source('global.R')
wave_summary_file <- "wave5_summary.xlsx"
mypptx <- read_pptx("bd_template_homemade.pptx")

server = function(input, output, session) {
    
    ########################## TEMPLATE DATA ##########################
    template_data <- tibble('Time'=c(0,0.5,1,1.5,2,3,4,5), 
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
    
    output$download_template_file <- downloadHandler(
        filename <- function() {
            paste("stats_template", "xlsx", sep=".")
        },
        content = function(file) {
            file.copy('stability_stats_template.xlsx', file)
        },
        contentType = "text/xlsx"
    )
    ########################## EXAMPLE DATA ###########################
    # Download Example Stats File
    output$downloadExample <- downloadHandler(
        filename <- function() {
            paste("stats_example", "xlsx", sep=".")
        },
        content = function(file) {
            file.copy('stability_stats_example.xlsx', file)
        },
        contentType = "text/xlsx"
    )
    
    ############################### GUI ONLY ########################################
    ## Model Criteria User Inputs from GUI
    confidenceInterval <- reactive({ input$CI })
    threshold_y <- reactive({ input$threshold })
    poly_order <- reactive({ order_of_polynomial(input$polynomial_order) })

    ############################### RAW DATA ONLY ###################################
    ## STEP 1: Upload Raw Stats (without % 4C Reference MFI Data)
    raw_upload_data <- reactive({
        inFile <- input$raw_upload
        if(is.null(inFile))
            return(NULL)
        df <- readxl::read_xlsx(inFile$datapath)
        return(df)
    })
    
    ## Step 2: Calculate % of 4C Reference MFI Data
    raw_upload_data_with_perct_MFI <- reactive({
        req(input$raw_upload)
        calculate_perct_4C_MFI(raw_upload_data())
    })
    
    
    ## Step 3: Create plots for all stats
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
    output$percent_of_4C_MFI <- renderPlot({
        req(input$raw_upload)
        percent_of_4C_MFI(raw_upload_data())
    })
    ## Step 4a: Create wide table of % 4C Reference MFI Data for UI ONLY
    raw_reference_MFI_data_wide_UI_only <- reactive({
        req(input$raw_upload)
        create_reference_MFI_table_wide_UI_only(raw_upload_data_with_perct_MFI())
    })
    
    ## Step 4b: Create wide table of % 4C Reference MFI Data for analysis use
    raw_reference_MFI_data_wide <- reactive({ 
        req(input$raw_upload)
        create_raw_reference_MFI_table_wide(raw_upload_data_with_perct_MFI()) 
    })
    
    ## Step 5: 'Melt' table to 3 to 1 column for % of 4C MFI data
    raw_melted_data <- reactive({ na.omit(melt_reference_mfi_table(raw_reference_MFI_data_wide_UI_only())) })
    
    ## Step 6: Calculate confidence intervals
    raw_confidence_bands <-
        reactive({
            find_confidence_bands(
                raw_melted_data(),
                poly_order(),
                as.numeric(confidenceInterval()),
                as.numeric(threshold_y())
            )
        })
    
    ## Step 7: Create regression plot
    raw_regr_plot <- reactive({
        
        req(input$raw_upload)
        
        ggplot(raw_melted_data(), aes(x=Time, y=value, color=Concentrations)) + 
            geom_ribbon(data=raw_melted_data(), 
                        aes(x=Time, y=value, 
                            ymin=raw_confidence_bands()[[2]], 
                            ymax=raw_confidence_bands()[[4]]), 
                        formula = y ~ poly(x,poly_order(), raw=TRUE), method="lm",col = "red", 
                        level=as.numeric(confidenceInterval()), alpha=0.2) +
            geom_line(data=raw_confidence_bands(), 
                      aes(x=raw_confidence_bands()[[1]], y=raw_confidence_bands()[[3]]), 
                      formula = y ~ poly(x,poly_order(), raw=TRUE), method="lm", col = "red") +
            geom_point(size=7) + 
            labs(x = "Time (years)",
                 y = "% of 4C Reference MFI") +
            theme_minimal() +
            scale_color_brewer(palette = 'Dark2', na.translate = F,
                               labels = unique(raw_melted_data()$Labels)) +
            stat_regline_equation(data=raw_melted_data(), 
                                  aes(x=Time, y=value,
                                      label=paste(..eq.label.., ..rr.label.., sep = "~~~~~~")), 
                                  formula = y ~ poly(x,poly_order(),raw=TRUE), method="lm", col="red",
                                  label.x.npc="center",label.y.npc="top",size=5) +
            theme(text=element_text(size = 14),
                  legend.position = "bottom")
        
    })
    
    ## Step 8: Calculate shelf life with and without confidence interval
    raw_shelf_life <-
        reactive({
            solve_for_shelf_life(raw_melted_data(),
                                 as.numeric(threshold_y()),
                                 poly_order())
        })
    
    raw_lower_shelf_life <-
        reactive({
            solve_for_lower_shelf_life(
                raw_melted_data(),
                poly_order(),
                as.numeric(confidenceInterval()),
                as.numeric(threshold_y())
            )
        })

    ## Step 8: Quality checks: model coefficient p-value & R-squared value
    
    # All model coefficient p-values
    raw_poly_eval <- reactive({ polynomial_evaluation_of_linearity(raw_melted_data(), poly_order()) })
    
    # R-squared value
    raw_R_sq_val <- reactive({ R_sq(raw_melted_data(), poly_order()) })
    
    ## Step 9: Calculate residuals and add residual plots for additional quality checks
    raw_residuals_expr <- reactive({ find_residuals(raw_melted_data(), poly_order()) })
    raw_residual_vs_fit_plot_expr <- reactive({ residual_vs_fit_plot(raw_melted_data(), poly_order())  })
    raw_residual_histogram_plot_expr <- reactive({ residual_histogram(raw_melted_data(), poly_order()) })
    
    ## Step 10: Check for normal distribution of residuals
    raw_normal_probability_plot_expr <- reactive({ normal_probability_plot(raw_melted_data(), poly_order(), raw_residuals_expr()) })
    raw_anderson_darling_normality_test_pvalue <- reactive({ anderson_darling_normality_test(raw_residuals_expr()) })
    
    ############################### MODIFIED DATA - SHOWN ON GUI ###################################
    
    ## STEP 1: Upload Raw Stats (without % 4C Reference MFI Data)
    ## already done - input$raw_upload
    
    ## Step 2: Calculate % of 4C Reference MFI Data
    ## already done - calculate_perct_4C_MFI() function
    
    ## Step 3: Gather concentrations to analyze
    
    ## Step 3a: Create list of selected concentrations to include in analysis
    output$concentration_checkGroupInput <-
        renderUI({
            # if there is no 15 ng/test concentration is found in stats, default to toggle off
            if ( all(is.na(raw_reference_MFI_data_wide_UI_only()$Conc_15_ng)) ) {
                checkboxGroupInput(
                    'concentrations_to_include',
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
            # else, default to toggle 15 ng/test on
            else{
                checkboxGroupInput(
                    'concentrations_to_include',
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
    
    concentrations_to_include_list <- reactive({ 
        req(input$concentrations_to_include)
        unlist(strsplit(input$concentrations_to_include, " ")) })
    output$concentrations_to_include_list_output <- renderTable({ concentrations_to_include_list() })
    
    ## Step 3b: Selected % of 4C Reference MFI Data (based on concentrations selected in UI)
    reference_MFI_data_to_include <- reactive({
        if(is.null(input$raw_upload)){
            return(template_data)
        }
        return(concentrations_to_keep(raw_reference_MFI_data_wide_UI_only(), concentrations_to_include_list()))
    })
    
    ## Step 3c: Output % of 4C Reference MFI Data Table to UI
    output$reference_mfi_data_table <- DT::renderDataTable({
        if (is.null(input$raw_upload)) {
            return (NULL)
        }
        df <- reference_MFI_data_to_include()
        
        DT::datatable(df) %>%
            formatRound(columns=c(1), 1) %>%
            formatRound(columns=c(2:ncol(df)), 0)
    })
    
    ## Step 4: 'Melt' % of 4C Reference MFI Data w/ selected concentrations
    selected_melted_data <- reactive({ melt_reference_mfi_table(reference_MFI_data_to_include()) })
    
    ## Step 5: Set up variables for excluding data points on regression plot via GUI

    # For storing which rows have been excluded
    vals <- reactiveValues(
        keeprows = rep(TRUE, 64)
    )
    # Update values of included rows whenever new data is uploaded
    observeEvent(input$raw_upload, {
        vals$keeprows <- rep(TRUE, nrow(selected_melted_data()))
    })
    
    keep    <- reactive({ selected_melted_data()[ vals$keeprows, , drop = FALSE] })
    exclude <- reactive({ selected_melted_data()[!vals$keeprows, , drop = FALSE] })
    
    ## Step 5: Calculate confidence intervals
    confidence_bands <-
        reactive({
            find_confidence_bands(
                keep(),
                poly_order(),
                as.numeric(confidenceInterval()),
                as.numeric(threshold_y())
            )
        })
    
    ## Step 6: Create regression plot
    regr_plot <- reactive({
        req(input$raw_upload)
        # Plot the kept and excluded points as two separate data sets
        keep    <- selected_melted_data()[ vals$keeprows, , drop = FALSE]
        exclude <- selected_melted_data()[!vals$keeprows, , drop = FALSE]
        keep <- keep[complete.cases(keep),]
        
        ggplot(keep, aes(x=Time, y=value, color=Concentrations)) + 
            geom_ribbon(data=keep, 
                        aes(x=Time, y=value, 
                            ymin=confidence_bands()[[2]], 
                            ymax=confidence_bands()[[4]]), 
                        formula = y ~ poly(x,poly_order(), raw=TRUE), 
                        method="lm", col = "red", level=as.numeric(confidenceInterval()), alpha=0.2) +
            geom_line(data=confidence_bands(), 
                      aes(x=confidence_bands()[[1]], 
                          y=confidence_bands()[[3]]), 
                      formula = y ~ poly(x,poly_order(), raw=TRUE), method="lm", col = "red") +
            geom_point(size=7) + # plot with keep dataset points filled in only
            geom_point(data = exclude, size = 7, shape = 21, fill = NA, color = 'black') +
            labs(x = "Time (years)",
                 y = "% of 4C Reference MFI") +
            theme_minimal() +
            scale_color_brewer(palette = 'Dark2', na.translate = F,
                               labels = unique(keep$Labels)) +
            stat_regline_equation(data=keep, 
                                  aes(x=Time, y=value,label=paste(..eq.label.., ..rr.label.., sep = "~~~~~~")), 
                                  formula = y ~ poly(x,poly_order(),raw=TRUE), method="lm", col="red",
                                  label.x.npc="center",label.y.npc="top",size=5) +
            theme(text=element_text(size = 14),
                  legend.position = "bottom")
    })
    
    output$regression_plot_output <- renderPlot({ regr_plot() })
    
    # Toggle points that are clicked
    observeEvent(input$regression_plot_output_click, {
        res <- nearPoints(selected_melted_data(), input$regression_plot_output_click, allRows = TRUE) 
        vals$keeprows <- xor(vals$keeprows, res$selected_) # keeprows array is updated 
    })
    
    # Toggle points that are brushed, when button is clicked
    observeEvent(input$exclude_toggle, {
        res <- brushedPoints(selected_melted_data(), input$regression_plot_output_brush, allRows = TRUE)
        vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
    
    # Reset all points
    observeEvent(input$exclude_reset, {
        vals$keeprows <- rep(TRUE, 64)
    })
    
    ## Step 7a: Calculate shelf life with and without confidence interval
    shelf_life <-
        reactive({
            solve_for_shelf_life(keep(), as.numeric(threshold_y()), poly_order())
        })
    
    lower_shelf_life <-
        reactive({
            solve_for_lower_shelf_life(
                keep(),
                poly_order(),
                as.numeric(confidenceInterval()),
                as.numeric(threshold_y())
            )
        })
    
    ## Step 7b: Output shelf-life to UI
    output$check_shelf_life <- renderUI({
        req(input$raw_upload)
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
        req(input$raw_upload)
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
    
    ## Step 8: Quality checks: model coefficient p-value & R-squared value
    
    # All model coefficient p-values
    poly_eval <- reactive({ polynomial_evaluation_of_linearity(keep(), poly_order()) })
    
    output$model_coeff_pvalue <- renderUI({ 
        if( poly_eval()$b_pvalue >= 0.05 ){ ## Model coeff. not statistically significant
            div(style = "color: red; font-size: 20px;",
                strong(format(round(poly_eval()$b_pvalue,3),nsmall=3)))
        }
        else if( poly_eval()$b_pvalue < 0.05 ){ # Model coeff. is statistically significant
            div(style = "color: #2DC62D; font-size: 20px",
                strong(format(round(poly_eval()$b_pvalue,3),nsmall=3)))
        }
        # format(round(poly_eval()$b_pvalue,2), nsmall = 3) 
    })
    
    output$warning_ui_model_coeff_pvalue <- renderUI({
        req(input$raw_upload)
        
        if(poly_order() == 1 && poly_eval()$b_pvalue >= 0.05){
            tags$i(" Warning: Model coefficient p-value not statistically significant.",
                   class = "fas fa-times-circle", 
                   style = "color: red; font-size: 20px"
            )
        }
    })
    
    # R-squared value
    R_sq_val <- reactive({ R_sq(keep(), poly_order()) })
    
    ## Step 9: Calculate residuals and add residual plots for additional quality checks
    residuals_expr <- reactive({ find_residuals(keep(), poly_order()) })
    residual_vs_fit_plot_expr <- reactive({ residual_vs_fit_plot(keep(), poly_order())  })
    residual_histogram_plot_expr <- reactive({ residual_histogram(keep(), poly_order()) })
    
    output$residual_fit_plot <- renderPlotly({ 
        req(input$raw_upload)
        residual_vs_fit_plot_expr()
    })
    
    output$residual_histogram <- renderPlotly({ 
        req(input$raw_upload)
        residual_histogram_plot_expr() 
    })
    
    ## Step 10: Check for normal distribution of residuals
    normal_probability_plot_expr <- reactive({ normal_probability_plot(keep(), poly_order(), residuals_expr()) })
    anderson_darling_normality_test_pvalue <- reactive({ anderson_darling_normality_test(residuals_expr()) })

    output$normal_prob_plot_output <- renderPlotly({ normal_probability_plot_expr() })
    output$anderson_darling_pvalue_table_output <- renderTable({ modified_anderson_darling_flextable() })
    
    output$anderson_darling_pvalue_output <- renderUI({
        
        if( anderson_darling_normality_test_pvalue() < 0.05 ){ ## Not Normal Distribution
            div(style = "color: red; font-size: 20px;",
                strong(format(round(anderson_darling_normality_test_pvalue(),3),nsmall=3)))
        }
        else if( anderson_darling_normality_test_pvalue() >= 0.05 ){ # Normal Distribution
            div(style = "color: #2DC62D; font-size: 20px",
                strong(format(round(anderson_darling_normality_test_pvalue(),3),nsmall=3)))
        }
    })
    
    output$warning_normality_pvalue <- renderUI({
        req(input$raw_upload)
        if(anderson_darling_normality_test_pvalue() < 0.05){
            tags$i(" Warning: Residuals do not follow normal distribution",
                   class = "fa fa-exclamation-triangle", 
                   style = "color: #fcba03; font-size: 20px"
            )
        }
    })
    ###################################### OUTPUT WARNINGS ############################################
    slope <- reactive({ best_fit_equation(keep(), poly_order())$coefficients[[2]] })
    
    output$warning_ui_polynomial_choice <- renderUI({
        req(input$raw_upload)
        
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
        req(input$raw_upload)
        if(R_sq_val() < 0.80){
            tags$i(" Warning: R-squared value is below 0.80",
                   class = "fa fa-exclamation-triangle", 
                   style = "color: #fcba03; font-size: 20px"
            )
        }
    })
    output$warning_ui_slope <- renderUI({
        req(input$raw_upload)
        if(slope() > 0){
            tags$i(" Warning: Regression slope is positive",
                   class = "fa fa-exclamation-triangle", 
                   style = "color: #fcba03; font-size: 20px"
            )
        }
    })
    

    ############################################ SUMMARY DATA FOR BULK ANALYSIS #############################################
    wave_df <- reactive({ read_marker_data(wave_summary_file, sheet="Sheet1") })
    
    updateSelectizeInput(
        session,
        'select_marker',
        choices = c(
            read_marker_data(wave_summary_file, sheet = "Sheet1")$`Marker Description`
        ),
        server = TRUE
    )
    # updateSelectizeInput(
    #     session,
    #     'population_name',
    #     choices = c(
    #         "Lymph", "Mono", "Gran"
    #     ),
    #     server = TRUE, 
    #     options = list(create=TRUE)
    # )
    optimal <- reactive({ wave_df()$`Optimal (ng/test)`[wave_df()$`Marker Description` == input$select_marker] })
    output$marker_optimal <- renderUI({
        tags$i(paste0('Optimal: ',optimal(), ' ng/test'),
               style="color: #eb6864; font-size: 18px; font-style: normal; font-weight: bold; padding-top: 20px;"
        )
        
    })
    
    raw_linear_results <- reactive({ results_summary(raw_melted_data(), 1, as.numeric(input$CI)) })
    output$linear_results_output <- renderTable({ raw_linear_results() })
    
    optimal_linear_results <- reactive({ results_summary(optimal_data(), 1, as.numeric(input$CI)) })
    output$optimal_linear_results_output <- renderTable({ optimal_linear_results() })
    
    raw_second_order_results <- reactive({ results_summary(raw_melted_data(), 2, as.numeric(input$CI)) })
    output$second_order_results_output <- renderTable({ raw_second_order_results() })
    
    optimal_second_order_results <- reactive({ results_summary(optimal_data(), 2, as.numeric(input$CI)) })
    output$optimal_second_order_results_output <- renderTable({ optimal_second_order_results() })
    
    optimal_data <- reactive({ melt_reference_mfi_table(concentrations_to_keep(raw_reference_MFI_data_wide_UI_only(), concentrations_around_optimal(optimal()))) })
    
    summary_table <- reactive({ req(input$raw_upload)
        tibble(rbind(cbind('Concentrations Included'='Raw','Model Order'='Linear',raw_linear_results()),
                     cbind('Concentrations Included'='Optimal +1/-2','Model Order'='Linear',optimal_linear_results()),
                     cbind('Concentrations Included'='Raw','Model Order'='2nd Order',raw_second_order_results()),
                     cbind('Concentrations Included'='Optimal +1/-2','Model Order'='2nd Order',optimal_second_order_results()))) })
    
    output$results_table <- renderTable({ summary_table() })
    
    
    observeEvent(input$write_results, {
        write_csv(summary_table(), 'summary_results_wave5.csv', append=TRUE) })
    
    

    
    ########################## SHELF LIFE ##########################
    
    
    
    ############################################ FLEXTABLES FOR REPORTS #############################################
    
    raw_shelf_life_summary_flextable <- reactive({ 
        df <- tibble("Shelf-Life (days)" = c(round(solve_for_lower_shelf_life(raw_melted_data(), poly_order(), 0.95, 75),1)*365),
                     "Shelf-Life (years)" = c(round(solve_for_lower_shelf_life(raw_melted_data(), poly_order(), 0.95, 75),1)),
                     "R-squared" = c(raw_R_sq_val()),
                     "Model p-value"=c(format(round(raw_poly_eval()$b_pvalue,3), nsmall = 3))
        )
    })
    
    modified_shelf_life_summary_flextable <- reactive({ 
        df <- tibble("Shelf-Life (days)" = c(round(solve_for_lower_shelf_life(keep(), poly_order(), 0.95, 75),1)*365),
                     "Shelf-Life (years)" = c(round(solve_for_lower_shelf_life(keep(), poly_order(), 0.95, 75),1)),
                     "R-squared" = c(format(round(R_sq_val(),2),nsmall=2)),
                     "Model p-value"=c(format(round(poly_eval()$b_pvalue,3), nsmall = 3))
        )
    })

    raw_anderson_darling_flextable <- reactive({
        df <-
            tibble("Anderson-Darling Normality Test p-value" = c(
                format(round(
                    anderson_darling_normality_test(find_residuals(raw_melted_data(),poly_order())), 
                    3), nsmall = 3))
            )
    })
    
    modified_anderson_darling_flextable <- reactive({
        df <- 
            tibble("Anderson-Darling Normality Test p-value" = c(
                format(round(
                    anderson_darling_normality_test(find_residuals(keep(),poly_order())), 
                    3), nsmall = 3))
            )
    })
    
    raw_model_pvalue_color <- reactive({
        if( raw_poly_eval()$b_pvalue >= 0.05 ){ ## Model coeff. not statistically significant
           return("red")
        }
        else if( raw_poly_eval()$b_pvalue < 0.05 ){ # Model coeff. is statistically significant
            return("#2DC62D")
        }
    })
    
    modified_model_pvalue_color <- reactive({
        if( poly_eval()$b_pvalue >= 0.05 ){ ## Model coeff. not statistically significant
            return("red")
        }
        else if( poly_eval()$b_pvalue < 0.05 ){ # Model coeff. is statistically significant
            return("#2DC62D")
        }
    })
    
    raw_ad_pvalue_color <- reactive({
        if( raw_anderson_darling_normality_test_pvalue() >= 0.05 ){ ## Normal distribution
            return("#2DC62D")
        }
        else if( raw_anderson_darling_normality_test_pvalue() < 0.05 ){ # Not normal distribution
            return("red")
        }
    })
    
    modified_ad_pvalue_color <- reactive({
        if( anderson_darling_normality_test_pvalue() >= 0.05 ){ ## Normal distribution
            return("#2DC62D")
        }
        else if( anderson_darling_normality_test_pvalue() < 0.05 ){ # Not normal distribution
            return("red")
        }
    })
    
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
                           fullDT = reference_MFI_data_to_include(),
                           raw_upload_data = raw_upload_data(),
                           raw_full_data = raw_reference_MFI_data_wide_UI_only(),
                           meltedDT = selected_melted_data(),
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
                           raw_data = req(input$raw_upload),
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

    label_fp <- fp_text(bold = TRUE, color = "black", font.size = 16)
    note_fp <- fp_text(bold = FALSE, color = "black", font.size = 16)
    notes_reactive <- reactive({

        bl2 <- block_list(fpar(ftext("Notes: ", label_fp), ftext(input$notes, note_fp)))
    })
    
    cell_pops <- reactive({
        bl1 <- block_list(fpar(
            ftext(input$model_system_data, fp_text(
                bold=FALSE, color = "black", font.size = 12
            ))
        ))
    })
    
    ######################## RESIDUAL PLOTS ###################################
    ########################## Powerpoint Output ##############################
    output$pptx_id <- downloadHandler(
        filename = function(){
            "test_pptx2.pptx"
        },
        content = function(file){
            
            mypptx  %>% 
                ################### PROTOCOL SLIDE ##################
                add_slide(layout = "Title and Content", master = "Office Theme") %>%
                ph_with(
                    value = "Reagents Protocol", 
                    location = ph_location_type(type = "title")
                ) %>%
                ################### HISTOGRAM PLOTS SLIDE ##################
                add_slide(layout = "Title and Content", master = "Office Theme") %>%
                    ph_with(
                        value = paste0(input$pop_number, ":  Histogram Plots"),
                        location = ph_location_type(type = "title")
                ) %>%
                ################### SSC PLOTS SLIDE ##################
                add_slide(layout = "Title and Content", master = "Office Theme") %>%
                    ph_with(
                        value = "SSC Plots", 
                        location = ph_location_type(type = "title")
                ) %>%
                ################### MFI PLOTS SLIDE ##################
                add_slide(layout = "Two Content", master = "Office Theme") %>%
                ph_with(
                    value = paste0(input$pop_number, ": MFI Data"), 
                    location = ph_location_type(type = "title")
                ) %>%
                ph_with(
                    value = mfi_vs_concentration_plot(raw_upload_data()), 
                    location = ph_location_type(type = "body", index = 4, position_right = F)
                ) %>%
                ph_with(
                    value = mfi_vs_time_plot(raw_upload_data()), 
                    location = ph_location_type(type = "body", index = 3, position_right = T)
                ) %>%
                ph_with(
                    value = cell_pops(),
                    location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
                ) %>%
                ################### CONCENTRATION VS TIME PLOTS SLIDE 1 ##################
                add_slide(layout = "Two Content", master = "Office Theme") %>%
                ph_with(
                    value = paste0(input$pop_number, ":  Conc. VS Time"),
                    location = ph_location_type(type = "title")
                ) %>%
                ph_with(
                    value = stain_index(raw_upload_data()), 
                    location = ph_location_type(type = "body", index = 4, position_right = F)
                ) %>%
                ph_with(
                    value = signal_to_noise(raw_upload_data()), 
                    location = ph_location_type(type = "body", index = 3, position_right = T)
                ) %>%
                ph_with(
                    value = cell_pops(),
                    location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
                ) %>%
                ################## CONCENTRATION VS TIME PLOTS SLIDE 2 ##################
                add_slide(layout = "Two Content", master = "Office Theme") %>%
                ph_with(
                    value = paste0(input$pop_number, ":  Conc. VS Time"), 
                    location = ph_location_type(type = "title")
                ) %>%
                ph_with(
                    value = percent_positive(raw_upload_data()), 
                    location = ph_location_type(type = "body", index = 4, position_right = F)
                ) %>%
                ph_with(
                    value = percent_of_4C_MFI(raw_upload_data()), 
                    location = ph_location_type(type = "body", index = 3, position_right = T)
                ) %>%
                ph_with(
                    value = cell_pops(),
                    location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
                ) %>%
                ################### REGRESSION SLIDE - RAW ##################
                add_slide(layout = "Picture with Caption", master = "Office Theme") %>%
                ph_with(
                    value = paste0(input$pop_number, ":  Regression - All Data"),
                    location = ph_location_type(type = "title")
                ) %>%
                ph_with(
                    value = flextable(raw_reference_MFI_data_wide(), cwidth=0.65) %>%
                        add_header_lines("% of 4C Reference MFI") %>%
                        bold(i = 1:2, bold = TRUE, part = "header") %>%
                        align(align="center", part="all")  %>%
                        # fontsize(size = 14, part = "body") %>%
                        fontsize(size = 11, part = "all") %>%
                        hline_top(part = "all", border = fp_border(color="black", width=2)),
                    location = ph_location_type(type = "body", position_right = F, position_top = T)
                ) %>%
                ph_with(
                    value = raw_regr_plot(), 
                    location = ph_location_type(type = "pic")
                ) %>%
                ph_with(
                    value = flextable(raw_shelf_life_summary_flextable(), cwidth=1.2) %>%
                        bold(i = 1, bold = TRUE, part = "header") %>%
                        bg(bg = "#FBF59E", part = "header") %>%
                        align(align="center", part="all") %>%
                        fontsize(size = 14, part = "body") %>%
                        color(i = 1, j = 4, color = raw_model_pvalue_color()) %>%
                        hline_top(part = "all", border = fp_border(color="black", width=2)),
                    location = ph_location_type(type = "body", position_right = F, position_top = F)
                ) %>%
                ph_with(
                    value = cell_pops(),
                    location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
                ) %>%
                ################## RESIDUAL VS FIT AND HISTOGRAM - RAW ##################
            add_slide(layout = "Two Content", master = "Office Theme") %>%
                ph_with(
                    value = paste0(input$pop_number, ":  Residuals"),
                    location = ph_location_type(type = "title")
                ) %>%
                ph_with(
                    value = residual_vs_fit_plot(raw_melted_data(), poly_order()), 
                    location = ph_location_type(type = "body", index = 4, position_right = F)
                ) %>%
                ph_with(
                    value = residual_histogram(raw_melted_data(), poly_order()), 
                    location = ph_location_type(type = "body", index = 3, position_right = T)
                ) %>%
                ph_with(
                    value = cell_pops(),
                    location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
                ) %>%
                ################## NORMALITY OF RESIDUALS - RAW ##################
            add_slide(layout = "Two Content", master = "Office Theme") %>%
                ph_with(
                    value = paste0(input$pop_number, ":  Normality of Residuals"),
                    location = ph_location_type(type = "title")
                ) %>%
                ph_with(
                    value = normal_probability_plot(raw_melted_data(), poly_order(), find_residuals(raw_melted_data(), poly_order())), 
                    location = ph_location_type(type = "body", index = 4, position_right = F)
                ) %>%
                ph_with(
                    value = flextable(raw_anderson_darling_flextable(), cwidth=3) %>% 
                        bg(bg = "#FBF59E", part = "header") %>%
                        align(align="center", part="all") %>%
                        fontsize(size = 12, part = "header") %>%
                        color(i = 1, j = 1, color = raw_ad_pvalue_color()) %>%
                        fontsize(size = 16, part = "body"),
                    location = ph_location_type(type = "body", left = 7, index = 3, position_right = T)
                ) %>%
                ph_with(
                    value = cell_pops(),
                    location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
                ) %>%
                ################### REGRESSION SLIDE - UPDATED ##################
            add_slide(layout = "Picture with Caption", master = "Office Theme") %>%
                ph_with(
                    value = paste0(input$pop_number, ":  Regression"),
                    location = ph_location_type(type = "title")
                ) %>%
                ph_with(
                    value = flextable(reference_MFI_data_to_include(), cwidth=0.65) %>%
                        add_header_lines("% of 4C Reference MFI") %>%
                        bold(i = 1:2, bold = TRUE, part = "header") %>%
                        align(align="center", part="all")  %>%
                        fontsize(size = 11, part = "all") %>%
                        hline_top(part = "all", border = fp_border(color="black", width=2)),
                    location = ph_location_type(type = "body", position_right = F, position_top = T)
                ) %>%
                ph_with(
                    value = regr_plot(),
                    location = ph_location_type(type = "pic")
                ) %>%
                ph_with(
                    value = flextable(modified_shelf_life_summary_flextable(), cwidth=1.2) %>%
                        bold(i = 1, bold = TRUE, part = "header") %>%
                        bg(bg = "#FBF59E", part = "header") %>%
                        align(align="center", part="all") %>%
                        fontsize(size = 14, part = "body") %>%
                        color(i = 1, j = 4, color = modified_model_pvalue_color()) %>%
                        hline_top(part = "all", border = fp_border(color="black", width=2)),
                    location = ph_location_type(type = "body", position_right = F, position_top = F)
                ) %>%
                ph_with(
                    value = notes_reactive(),
                    location = ph_location_template(left=8, top=6, width=5, newlabel="new", id=6)
                ) %>%
                ph_with(
                    value = cell_pops(),
                    location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
                ) %>%
                ################## RESIDUAL VS FIT AND HISTOGRAM - MODIFIED ##################
            add_slide(layout = "Two Content", master = "Office Theme") %>%
                ph_with(
                    value = paste0(input$pop_number, ":  Residuals"),
                    location = ph_location_type(type = "title")
                ) %>%
                ph_with(
                    value = residual_vs_fit_plot(keep(), poly_order()), 
                    location = ph_location_type(type = "body", index = 4, position_right = F)
                ) %>%
                ph_with(
                    value = residual_histogram(keep(), poly_order()), 
                    location = ph_location_type(type = "body", index = 3, position_right = T)
                ) %>%
                ph_with(
                    value = cell_pops(),
                    location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
                ) %>%
                ################## NORMALITY OF RESIDUALS - RAW ##################
            add_slide(layout = "Two Content", master = "Office Theme") %>%
                ph_with(
                    value = paste0(input$pop_number, ":  Normality of Residuals"),
                    location = ph_location_type(type = "title")
                ) %>%
                ph_with(
                    value = normal_probability_plot(keep(), poly_order(), find_residuals(keep(), poly_order())), 
                    location = ph_location_type(type = "body", index = 4, position_right = F)
                ) %>%
                ph_with(
                    value = flextable(modified_anderson_darling_flextable(), cwidth=3) %>% 
                        bg(bg = "#FBF59E", part = "header") %>%
                        align(align="center", part="all") %>%
                        fontsize(size = 12, part = "header") %>%
                        color(i = 1, j = 1, color = modified_ad_pvalue_color()) %>%
                        fontsize(size = 16, part = "body"),
                    location = ph_location_type(type = "body", left = 7, index = 3, position_right = T)
                ) %>%
                ph_with(
                    value = cell_pops(),
                    location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
                ) %>%
            
                print(target = file)
        }
    )
}