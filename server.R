source('global.R')
example_file <- read.csv('stability_stats.csv')
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

    ########################## EXAMPLE DATA ###########################
    # Download Example Stats File
    output$downloadExample <- downloadHandler(
        filename = 'stability_stats_example.csv',
        content = function(file) {
            write.csv(example_file, file, row.names=FALSE)
        }
    )
    
    ########################## RAW DATA ###########################
    # df_products_upload <- reactive({
    #     inFile <- input$raw_upload
    #     if (is.null(inFile)) 
    #         return(NULL)
    #     df_unrounded <- read_csv(inFile$datapath)
    #     if(is.na(df_unrounded$Conc_15_ng)) {
    #         print('its NA')
    #     }
    #     # df <- cbind('Time'=df_unrounded[[1]], round(df_unrounded[2:ncol(df_unrounded)]))
    #     df <- df_unrounded
    #     return(df)
    # })
    
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
        df <- readxl::read_xlsx(inFile$datapath)
        return(df)
    })
    
    df_with_perct_MFI <- reactive({
        req(input$raw_upload)
        calculate_perct_4C_MFI(raw_upload_data())
    })
    df_products_upload <- reactive({
        req(input$raw_upload)
        create_reference_MFI_table_wide(df_with_perct_MFI())
    })
    raw_mfi_table_wide <- reactive({ 
        req(input$raw_upload)
        create_raw_reference_MFI_table_wide(df_with_perct_MFI()) 
    })
    
    raw_melted_data <- reactive({ na.omit(meltedDataTable(df_products_upload())) })
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
    output$percent_of_4C_MFI <- renderPlot({
        req(input$raw_upload)
        percent_of_4C_MFI(raw_upload_data())
    })
    #############################################
    optimal <- reactive({ wave_df()$`Optimal (ng/test)`[wave_df()$`Marker Description` == input$select_marker] })
    wave_df <- reactive({ read_marker_data(wave_summary_file, sheet="Sheet1") })
    output$marker_optimal <- renderUI({
        tags$i(paste0('Optimal: ',optimal(), ' ng/test'),
               style="color: #eb6864; font-size: 18px; font-style: normal; font-weight: bold; padding-top: 20px;"
        )
        
    })
    # renderText({ if(as.character(paste0(optimal(), ' ng/test')) == ) })
    output$concentrations <- renderText({ input$conc_avgs })
    updateSelectizeInput(session, 'select_marker', choices = c(read_marker_data(wave_summary_file, sheet="Sheet1")$`Marker Description`), server = TRUE)
    
    output$tab_panel_title <- renderText({
        input$pop1
    })
    # Reactive User Inputs
    confidenceInterval <- reactive({input$CI})
    threshold_y <- reactive({input$threshold})
    
    # Concentrations to average
    conc_avgs_list <- reactive({ 
        req(input$conc_avgs)
        unlist(strsplit(input$conc_avgs, " ")) })
    output$conc_avgs_list_output <- renderTable({ conc_avgs_list() })
    
    # Run linearRegression.R file
    full_data_table <- reactive({
        if(is.null(input$raw_upload)){
            return(template_data)
        }
        print(df_products_upload())
        print(conc_avgs_list())
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
    
    labels <- reactive({ labels_column(input$raw_upload) })
    
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
    ###############################################################################
    # For storing which rows have been excluded
    vals <- reactiveValues(
        keeprows = rep(TRUE, 64)
        
    )
    observeEvent(input$raw_upload, {
        vals$keeprows <- rep(TRUE, nrow(melted_data_table()))
        print(nrow(melted_data_table()))
    })
    
    # output$text1 <- renderPrint(vals$keeprows)
    output$plot1 <- renderPlot({
        req(input$raw_upload)
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
    
    regr_plot <- reactive({
        req(input$raw_upload)
        # Plot the kept and excluded points as two separate data sets
        # vals$keeprows <- rep(TRUE, nrow(melted_data_table()))
        keep    <- melted_data_table()[ vals$keeprows, , drop = FALSE] 
        exclude <- melted_data_table()[!vals$keeprows, , drop = FALSE] 
        output$text1 <- renderPrint(keep)
        keep <- keep[complete.cases(keep),]
        print(keep)
        
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
            stat_regline_equation(data=keep, aes(x=Time, y=value,label=paste(..eq.label.., ..rr.label.., sep = "~~~~~~")), formula = y ~ poly(x,poly_order(),raw=TRUE), method="lm", col="red",
                                  label.x.npc="center",
                                  label.y.npc="top",
                                  
                                  size=5) +
            theme(text=element_text(size = 14),
                  legend.position = "bottom")
        # geom_point(data=data.frame(confidence_bands()), aes(x=confidence_bands()[[1]], y=confidence_bands()[[2]]), formula = y ~ x, size=4, color="green") + 
        # geom_line(data=confidence_bands(), aes(x=confidence_bands()[[1]], y=confidence_bands()[[2]]), formula = y ~ x,  color="green") +
        # geom_point(data=data.frame(confidence_bands()), aes(x=confidence_bands()[[1]], y=confidence_bands()[[4]]), formula = y ~ x, size=4, color="green") + 
        # geom_line(data=confidence_bands(), aes(x=confidence_bands()[[1]], y=confidence_bands()[[4]]), formula = y ~ x,  color="green")
        # # geom_ribbon(data=keep(), aes(ymin=confidence_bands()[[2]], ymax=confidence_bands()[[4]]), linetype=2, alpha=0.1)
        
        
        
    })
    
    raw_regr_plot <- reactive({
        
        req(input$raw_upload)
        
        ggplot(raw_melted_data(), aes(x=Time, y=value, color=Concentrations)) + 
            geom_ribbon(data=raw_melted_data(), aes(x=Time, y=value, ymin=raw_confidence_bands()[[2]], ymax=raw_confidence_bands()[[4]]), formula = y ~ poly(x,poly_order(), raw=TRUE), method="lm", col = "red", level=as.numeric(confidenceInterval()), alpha=0.2) +
            geom_line(data=raw_confidence_bands(), aes(x=raw_confidence_bands()[[1]], y=raw_confidence_bands()[[3]]), formula = y ~ poly(x,poly_order(), raw=TRUE), method="lm", col = "red") +
            geom_point(size=7) + 
            labs(x = "Time (years)",
                 y = "% of 4C Reference MFI") +
            theme_minimal() +
            scale_color_brewer(palette = 'Reds', na.translate = F,
                               labels = unique(raw_melted_data()$Labels)
                               # labels = c("30 ng/test", '60 ng/test', '125 ng/test', '250 ng/test', '500 ng/test', '1000 ng/test', '2000 ng/test')
            ) +
            stat_regline_equation(data=raw_melted_data(), aes(x=Time, y=value,label=paste(..eq.label.., ..rr.label.., sep = "~~~~~~")), formula = y ~ poly(x,poly_order(),raw=TRUE), method="lm", col="red",
                                  label.x.npc="center",
                                  label.y.npc="top",
                                  
                                  size=5) +
            theme(text=element_text(size = 14),
                  legend.position = "bottom")
        
    })
    
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
        req(input$raw_upload)
        residual_vs_fit_plot(keep(), poly_order()) 
        })
    
    resid_plot <- reactive({ residual_vs_fit_plot(keep(), poly_order())  })
    output$residual_histogram <- renderPlotly({ 
        req(input$raw_upload)
        residual_histogram(keep(), poly_order()) 
        })
    # # When file uploaded, create plot
    # df_full <- eventReactive(input$raw_upload,{
    #     
    #     # plot <- createPlot(melted_data_table(), regression_data_table(), as.numeric(confidenceInterval()))
    #     if (is.null(input$raw_upload)) {
    #         return (NULL)
    #     }
    #     return (full_data_table())
    # })
    
    ########################## SHELF LIFE ##########################
    output$shelf_life_output <- renderText({
        if (is.null(input$raw_upload)) {
            return (NULL)
        }
        SL <- paste0(shelf_life(), ' years   ', '(', round(as.numeric(shelf_life())*365,0), ' days)')
    })
    
    output$shelf_life_lower_output <- renderText({
        if (is.null(input$raw_upload)) {
            return (NULL)
        }
        SL <- paste0(lower_shelf_life(), ' years   ', '(', round(as.numeric(lower_shelf_life())*365,0), ' days)')
    })
    
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
    
    output$sample_table <- DT::renderDataTable({
        # df <- df_products_upload()
        if (is.null(input$raw_upload)) {
            return (NULL)
        }
        df <- full_data_table()
        
        DT::datatable(df) %>%
            formatRound(columns=c(1), 1) %>%
            formatRound(columns=c(2:ncol(df)), 0)
    })
    
    mfi_flextable <- reactive({

        conc_to_exclude(df_products_upload(), conc_avgs_list())
        
# 
#         DT::datatable(df) %>%
#             formatRound(columns=c(1), 1) %>%
#             formatRound(columns=c(2:ncol(df)), 0)
    })
    
    shelf_life_flextable <- reactive({
        df <- tibble("MFI Threshold" = c("75%", "80%"),
                     "No CI" = c(round(solve_for_shelf_life(keep(), 75, poly_order()),1),
                                 round(solve_for_shelf_life(keep(), 80, poly_order()),1)),
                     "Lower 95% CI" = c(round(solve_for_lower_shelf_life(keep(), poly_order(), 0.95, 75),1),
                                        round(solve_for_lower_shelf_life(keep(), poly_order(), 0.95, 80),1))
        )
    })
    
    raw_shelf_life_flextable <- reactive({ 
        df <- tibble("MFI Threshold" = c("75%", "80%"),
                     "No CI" = c(round(solve_for_shelf_life(raw_melted_data(), 75, poly_order()),1),
                                 round(solve_for_shelf_life(raw_melted_data(), 80, poly_order()),1)),
                     "Lower 95% CI" = c(round(solve_for_lower_shelf_life(raw_melted_data(), poly_order(), 0.95, 75),1),
                                        round(solve_for_lower_shelf_life(raw_melted_data(), poly_order(), 0.95, 80),1))
        )})
    summary_flextable <- reactive({
        
        df <- tibble("Model Parameter"=c("R-squared", "p-value"),
                     "Value"=c(R_sq_val(), format(round(poly_eval()$b_pvalue,2), nsmall = 2))
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
    
    optimal_data <- reactive({ meltedDataTable(conc_to_exclude(df_products_upload(), concentrations_around_optimal(optimal()))) })
    
    summary_table <- reactive({ req(input$raw_upload)
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
                           raw_upload_data = raw_upload_data(),
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

    dummy_text <- readLines(system.file(package = "officer",
                                        "doc_examples/text.txt"))
    fp_1 <- fp_text(bold = TRUE, color = "pink", font.size = 0)
    fp_2 <- fp_text(bold = TRUE, font.size = 0)
    fp_3 <- fp_text(italic = TRUE, color="red", font.size = 0)
    bl <- block_list(
        fpar(ftext("hello world", fp_1)),
        fpar(
            ftext("hello", fp_2),
            ftext("hello", fp_3)
        ),
        dummy_text
    )
    notes_reactive <- reactive({
        bl2 <- block_list(fpar(ftext(input$notes)))
    })
    
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
                ################### MFI PLOTS SLIDE ##################
                add_slide(layout = "Two Content", master = "Office Theme") %>%
                ph_with(
                    value = "P1: MFI Data", 
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
                ################### CONCENTRATION VS TIME PLOTS SLIDE 1 ##################
                add_slide(layout = "Two Content", master = "Office Theme") %>%
                ph_with(
                    value = "P1: Conc. VS Time", 
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
                ################## CONCENTRATION VS TIME PLOTS SLIDE 2 ##################
                add_slide(layout = "Two Content", master = "Office Theme") %>%
                ph_with(
                    value = "P1: Conc. VS Time", 
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
                ################### REGRESSION SLIDE - RAW ##################
                add_slide(layout = "Picture with Caption", master = "Office Theme") %>%
                ph_with(
                    value = "P1: Regression - All Data", 
                    location = ph_location_type(type = "title")
                ) %>%
                ph_with(
                    value = flextable(raw_mfi_table_wide(), cwidth=0.65) %>%
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
                    value = flextable(raw_shelf_life_flextable(), cwidth=1.5) %>% 
                        # add_header('MFI Threshold' =  "Shelf-Life Prediction (years)",
                        #            `No CI` =  "Shelf-Life Prediction (years)", `Lower 95% CI` =  "Shelf-Life Prediction (years)",
                        #            top = TRUE ) %>%
                        # add_header_row(values = "Shelf-Life Prediction (years)", colwidths = c(3)) %>%
                        # merge_at(i=1, j=1:3, part="header") %>%
                        merge_v(j = "MFI Threshold") %>%
                        add_header_lines("Shelf-Life Prediction (years)") %>%
                        # add_header_lines(values = c("Shelf-Life Prediction (years)"), top = TRUE) %>%
                        bold(i = 1:2, bold = TRUE, part = "header") %>%
                        padding(padding.top = 20, padding.bottom = 10, part="all") %>%
                        
                        bg(bg = "#FBF59E", part = "header") %>%
                        align(align="center", part="all") %>%
                        fontsize(size = 12, part = "header") %>%
                        fontsize(size = 16, part = "body") %>%
                        hline_top(part = "all", border = fp_border(color="black", width=2)),
                    location = ph_location_type(type = "body", position_right = F, position_top = F)
                ) %>%
                ph_with(
                    value = flextable(summary_flextable(), cwidth=1.5) %>% 
                        bg(bg = "#FBF59E", part = "header") %>%
                        align(align="center", part="all") %>%
                        fontsize(size = 12, part = "header") %>%
                        fontsize(size = 16, part = "body"),
                    location = ph_location_type(type = "body", position_right = F, index=9)
                ) %>%
                ################### REGRESSION SUMMARY SLIDE - RAW ##################
                add_slide(layout = "Content with Caption", master = "Office Theme") %>%
                ph_with(
                    value = "P1: Regression Summary - All Data", 
                    location = ph_location_type(type = "title")
                ) %>%
                ph_with(
                    value = residual_vs_fit_plot(keep(), poly_order()),
                    location = ph_location_type(type = "body", position_right = T)
                ) %>%
                ph_with(
                    value = flextable(summary_flextable(), cwidth=1.5) %>% 
                        bg(bg = "#FBF59E", part = "header") %>%
                        align(align="center", part="all") %>%
                        fontsize(size = 12, part = "header") %>%
                        fontsize(size = 16, part = "body"),
                    location = ph_location_type(type = "body", position_right = F)   
                ) %>%
                ################### REGRESSION SLIDE - UPDATED ##################
            add_slide(layout = "Picture with Caption", master = "Office Theme") %>%
                ph_with(
                    value = "P1: Regression", 
                    location = ph_location_type(type = "title")
                ) %>%
                ph_with(
                    value = flextable(df_products_upload(), cwidth=0.65) %>%
                        add_header_lines("% of 4C Reference MFI") %>%
                        bold(i = 1:2, bold = TRUE, part = "header") %>%
                        align(align="center", part="all")  %>%
                        # fontsize(size = 14, part = "body") %>%
                        fontsize(size = 11, part = "all") %>%
                        hline_top(part = "all", border = fp_border(color="black", width=2)),
                    location = ph_location_type(type = "body", position_right = F, position_top = T)
                ) %>%
                ph_with(
                    value = regr_plot(), 
                    location = ph_location_type(type = "pic")
                ) %>%
                ph_with(
                    value = flextable(shelf_life_flextable(), cwidth=1.5) %>% 
                        # add_header('MFI Threshold' =  "Shelf-Life Prediction (years)",
                        #            `No CI` =  "Shelf-Life Prediction (years)", `Lower 95% CI` =  "Shelf-Life Prediction (years)",
                        #            top = TRUE ) %>%
                        # add_header_row(values = "Shelf-Life Prediction (years)", colwidths = c(3)) %>%
                        # merge_at(i=1, j=1:3, part="header") %>%
                        merge_v(j = "MFI Threshold") %>%
                        add_header_lines("Shelf-Life Prediction (years)") %>%
                        # add_header_lines(values = c("Shelf-Life Prediction (years)"), top = TRUE) %>%
                        bold(i = 1:2, bold = TRUE, part = "header") %>%
                        padding(padding.top = 20, padding.bottom = 10, part="all") %>%
                        
                        bg(bg = "#FBF59E", part = "header") %>%
                        align(align="center", part="all") %>%
                        fontsize(size = 12, part = "header") %>%
                        fontsize(size = 16, part = "body") %>%
                        hline_top(part = "all", border = fp_border(color="black", width=2)),
                    location = ph_location_type(type = "body", position_right = F, position_top = F)
                ) %>%
                ph_with(
                    value = flextable(summary_flextable(), cwidth=1.5) %>% 
                        bg(bg = "#FBF59E", part = "header") %>%
                        align(align="center", part="all") %>%
                        fontsize(size = 12, part = "header") %>%
                        fontsize(size = 16, part = "body"),
                    location = ph_location_type(type = "body", position_right = F, index=9)
                ) %>%
                ph_with(
                    value = notes_reactive(),
                    location = ph_location_template(left=9, top=4, width=4, newlabel="new", id=6)
                ) %>%
                ################### REGRESSION SUMMARY SLIDE - UPDATED ##################
                add_slide(layout = "Content with Caption", master = "Office Theme") %>%
                ph_with(
                    value = "P1: Regression Summary", 
                    location = ph_location_type(type = "title")
                ) %>%
                ph_with(
                    value = residual_vs_fit_plot(keep(), poly_order()),
                    location = ph_location_type(type = "body", position_right = T)
                ) %>%
                ph_with(
                    value = flextable(summary_flextable(), cwidth=1.5) %>% 
                        bg(bg = "#FBF59E", part = "header") %>%
                        align(align="center", part="all") %>%
                        fontsize(size = 12, part = "header") %>%
                        fontsize(size = 16, part = "body"),
                    location = ph_location_type(type = "body", position_right = F)   
                ) %>%
                ################### HISTOGRAM PLOTS SLIDE ##################
                add_slide(layout = "Title and Content", master = "Office Theme") %>%
                ph_with(
                    value = "P1: Histogram Plots", 
                    location = ph_location_type(type = "title")
                ) %>%
                ################### SSC PLOTS SLIDE ##################
                add_slide(layout = "Title and Content", master = "Office Theme") %>%
                ph_with(
                    value = "SSC Plots", 
                    location = ph_location_type(type = "title")
                ) %>%
                print(target = file)
        }
    )
}